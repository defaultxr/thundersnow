(in-package #:thundersnow/piano-roll)

;;;; piano-roll
;;; a piano roll, obviously
;; TODO:
;; - allow notes to be added to the left of the start of the eseq (in which case it becomes the new 0 and the other notes are moved earlier in time)
;; FIX: check out: with-temporary-margins, with-translation
;; NOTES:
;; - define-application-command arguments can accept defaults: (arg-name arg-type :default default-value)

;;; util

(defparameter tmp nil)

;;; eseq / event stuff

(defun event-presentation-equal (event1 event2)
  "False if EVENT1 and EVENT2 differ in their CLIM presentation representation. This is used in the `updating-output' for `draw-piano-roll'."
  (and (eql (sustain event1) (sustain event2))
       (eql (beat event1) (beat event2))
       (eql (event-value event1 :midinote) (event-value event2 :midinote))))

;;; gui stuff

(defun x-pixel-to-beat (x frame)
  "Convert an x pixel in FRAME to a beat number."
  (with-slots (beat-size) frame
    (/ x beat-size)))

(defun x-pixel-to-beat-quantized (x frame)
  "Convert an x pixel in FRAME to a beat number, quantizing to the grid."
  (with-slots (grid-size) frame
    (round-by (x-pixel-to-beat x frame) grid-size)))

(defun x-pixel-to-beat-floored (x frame)
  "Convert an x pixel in FRAME to a beat number, quantizing to the grid."
  (with-slots (grid-size) frame
    (floor-by (x-pixel-to-beat x frame) grid-size)))

(defun beat-to-x-pixel (beat frame)
  "Convert a beat in FRAME to the relevant x pixel."
  (with-slots (beat-size) frame
    (* beat-size beat)))

(defun y-pixel-to-pitch (y frame)
  "Convert a y pixel in FRAME to the frame's pitch type."
  (with-slots (y-size) frame
    (/ (- (pane-real-height frame) y) y-size)))

(defun y-pixel-to-pitch-quantized (y frame)
  "Convert a y pixel in FRAME to the frame's pitch type, quantizing."
  (with-slots (y-size) frame
    (floor (y-pixel-to-pitch y frame))))

(defun pitch-to-y-pixel (pitch frame)
  "Convert a pitch value to a y pixel in FRAME."
  (with-slots (y-size) frame
    (- (* y-size 128) (* y-size pitch))))

(defun pane-real-width (frame)
  "Get the \"real\" width of the piano-roll gadget in FRAME, in pixels."
  (let ((frame (if (typep frame 'pane)
                   (pane-frame frame)
                   frame)))
    (with-slots (beat-size) frame
      (* beat-size (+ 8 (dur frame)))))) ;; FIX: ensure that this is at least the width of the pane

(defun pane-real-height (frame)
  "Get the \"real\" height of the piano-roll gadget in FRAME, in pixels."
  (pitch-to-y-pixel 0 (if (typep frame 'pane)
                          (pane-frame frame)
                          frame)))

(defun hovering-for-resize-p (x presentation)
  "True if the mouse (whose x position is provided as the X argument) is hovering over PRESENTATION's right side."
  (>= (- x (output-record-position presentation)) (- (rectangle-width presentation) 8)))

(defclass graphical-view (view)
  ())

(defconstant +graphical-view+ (make-instance 'graphical-view))

(defclass %background ()
  ())

(define-presentation-type event ())

(defclass piano-roll-pane (application-pane)
  ((%saved-extent :initform nil :documentation "The scroll position to return to when redisplaying."))
  (:default-initargs
   :name 'piano-roll
   :display-function 'draw-piano-roll
   :display-time :command-loop
   :default-view +graphical-view+
   :foreground +white+
   :background (get-theme-color :background)))

(defmethod compose-space ((pane piano-roll-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width (pane-real-width pane)
                          :height (pane-real-height pane)))

(defmethod handle-repaint :before ((pane piano-roll-pane) region)
  ;; save the scroll position
  (with-slots (%saved-extent) pane
    (setf %saved-extent (pane-viewport-region pane))))

;; (defmethod handle-event ((pane piano-roll-pane) (event pointer-motion-event))

(define-presentation-method present (background (type %background) stream (view graphical-view) &key)
  (draw-rectangle* stream
                   0 0 (pane-real-width stream) (pane-real-height stream)
                   :filled nil))

(define-presentation-method present (event (type event) stream (view graphical-view) &key)
  (let* ((frame (pane-frame stream))
         (beat (beat event))
         (x-start (beat-to-x-pixel beat frame))
         (sustain (sustain event))
         (x-end (beat-to-x-pixel (+ beat sustain) frame))
         (midinote (event-value event :midinote))
         (y-bottom (pitch-to-y-pixel midinote frame))
         (y-top (pitch-to-y-pixel (1+ midinote) frame))
         (note-color (get-theme-color :note-fill))
         (grid-color (get-theme-color :grid)))
    (surrounding-output-with-border (stream :padding 0 :thickness 1 :ink (mix-colors note-color grid-color 1/2))
      (draw-rectangle* stream
                       x-start y-top x-end y-bottom
                       :filled t
                       :ink note-color))
    (draw-text* stream
                (note-text midinote)
                x-start y-top
                :align-y :top)
    (draw-text* stream ;; FIX: hide this if the note isn't big enough to show it
                (sustain-text event)
                x-end y-bottom
                :align-x :right
                :align-y :bottom)
    (draw-text* stream
                (beat-text event)
                x-start y-bottom
                :align-y :bottom)))

(define-application-frame piano-roll ()
  ((eseq :initarg :eseq :initform (eseq) :type eseq :documentation "The `eseq' instance.")
   (beat-size :initarg :beat-size :initform 200 :documentation "The width of one beat, in pixels.")
   (grid-size :initarg :beat-size :initform 1/4 :documentation "The number of beats between each grid line.")
   (y-size :initarg :y-size :initform 40 :documentation "The height of one pitch value (i.e. midinote), in pixels."))
  (:command-table (piano-roll
		   :inherit-from (piano-roll-file-command-table
                                  piano-roll-edit-command-table
                                  piano-roll-view-command-table
                                  piano-roll-tools-command-table
                                  piano-roll-help-command-table)
		   :menu (("File" :menu piano-roll-file-command-table)
                          ("Edit" :menu piano-roll-edit-command-table)
                          ("View" :menu piano-roll-view-command-table)
                          ("Tools" :menu piano-roll-tools-command-table)
			  ("Help" :menu piano-roll-help-command-table))))
  (:panes
   (piano-roll-pane (make-pane 'piano-roll-pane))
   (interactor-pane (make-clim-interactor-pane
                     :name 'interactor
                     :scroll-bar :vertical))
   (pointer-documentation-pane :pointer-documentation
                               :name 'doc
                               :display-time :command-loop
                               :scroll-bars nil))
  (:layouts
   (default
    (vertically ()
      (5/6 (scrolling () piano-roll-pane))
      (1/6 interactor-pane)
      pointer-documentation-pane)))
  (:menu-bar t))

(defmethod dur ((piano-roll piano-roll))
  (dur (slot-value piano-roll 'eseq)))

(defun draw-piano-roll (frame stream)
  (let* ((stream-width (pane-real-width stream))
         (stream-height (pane-real-height stream))
         (grid-size (slot-value frame 'grid-size))
         (beat-size (slot-value frame 'beat-size))
         (y-size (slot-value frame 'y-size))
         (eseq (slot-value frame 'eseq))
         (events (etypecase eseq
                   (list eseq)
                   (eseq (eseq-events eseq))
                   (pattern (next-upto-n eseq))))
         (background-color (slot-value stream 'background))
         (grid-color (get-theme-color :grid))
         (bg-grid-mixed (mix-colors background-color grid-color 1/6)))
    (present (make-instance '%background) '%background :stream stream)
    ;; draw the vertical grid lines and beat numbers at the bottom
    (loop :for beat :from 0 :by grid-size :upto (max (+ (dur frame) 32) (/ stream-width (* beat-size grid-size)))
          :for xpos = (beat-to-x-pixel beat frame)
          :do (draw-line* stream xpos 0 xpos stream-height :ink (if (= (round beat) beat) grid-color bg-grid-mixed))
              (draw-text* stream (write-to-string beat) (1+ xpos) (1- stream-height) :ink grid-color))
    ;; draw the horizontal grid lines and note names on the side
    (loop :for y :from 0 :upto 127
          :for ypos := (- stream-height (* y y-size))
          :for top-ypos := (- stream-height (* (1+ y) y-size))
          :do
             (draw-line* stream 0 ypos stream-width ypos :ink grid-color)
             (draw-text* stream (note-text y) 1 top-ypos :align-y :top :ink grid-color))
    ;; draw the notes (events)
    (dolist (event events)
      (updating-output (stream :unique-id event :cache-value event :cache-test #'event-presentation-equal)
        (present event 'event :stream stream))))
  ;; make sure we don't lose our current scroll location after we redraw
  (with-slots (%saved-extent) stream
    (apply #'scroll-extent stream
           (if %saved-extent
               (list (rectangle-min-x %saved-extent) (rectangle-min-y %saved-extent))
               (list 0 0)))))

(define-command-table piano-roll-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command-table piano-roll-edit-command-table)

;; FIX: default to mouse's current position
(define-command (com-add :name t :menu t
                         :command-table piano-roll-edit-command-table
                         :keystroke (#\a :meta))
    ((event '(or event number) :prompt "Event or event start beat"))
  (eseq-add (slot-value *application-frame* 'eseq)
            (typecase event
              (event event)
              (number (event :beat event)))))

(define-command (com-erase :name t :menu t
                           :command-table piano-roll-edit-command-table)
    ((event '(or event integer)))
  (eseq-remove (slot-value *application-frame* 'eseq) event))

(define-command (com-move :name t :menu t
                          :command-table piano-roll-edit-command-table)
    ((event 'event)
     (beat 'number)
     (y 'number))
  (with-swank-output
    (print 'com-move)
    (print event)
    (print 'new-beat-number)
    (print beat)
    (print 'new-y-number)
    (print y)))

(define-command (com-edit-event :name t :menu t
                                :command-table piano-roll-edit-command-table)
    ((event 'event))
  (let* ((*standard-input* (frame-standard-input *application-frame*))
         (plist (event-plist event))
         (pitch-type (find-any (list :midinote :freq :degree) plist)) ;; FIX: this should be standard functionality in cl-patterns
         )
    (accepting-values ()
      (fresh-line)
      (accept 'string :prompt (string pitch-type) :default (event-value event pitch-type))
      (fresh-line)
      (accept 'string :default (write-to-string (random 10)))
      (fresh-line)
      (accept 'string :default (write-to-string (random 10))))))

(define-piano-roll-command (com-move-event) ((record event) (offset-x real :default 0) (offset-y real :default 0))
  ;; offset-x and offset-y are the offset that the presentation was clicked.
  (declare (ignore offset-y))
  (drag-output-record (find-pane-named *application-frame* 'piano-roll-pane) record
                      :multiple-window nil
                      :finish-on-release t
                      :feedback
                      (lambda (presentation stream old-x old-y x y mode)
                        (declare (ignore old-x old-y))
                        (ecase mode
                          (:erase
                           (erase-output-record presentation stream))
                          (:draw
                           (with-slots (beat-size y-size) *application-frame*
                             (let* ((new-y (floor (/ (- (rectangle-max-y (sheet-region stream)) y) y-size)))
                                    (new-beat (x-pixel-to-beat-quantized (- x offset-x) *application-frame*))
                                    (act-x (* new-beat beat-size))
                                    (act-y (ceiling-by y (- y-size))))
                               (setf (output-record-position presentation) (values act-x act-y))
                               (let* ((event (presentation-object presentation))
                                      (sustain (event-value event :sustain)))
                                 (setf (beat event) new-beat
                                       (event-value event :midinote) new-y)
                                 (stream-add-output-record stream presentation)
                                 (repaint-sheet stream (make-rectangle* act-x act-y (+ act-x (* sustain beat-size)) (+ act-y y-size)))))))))))

(define-presentation-to-command-translator event-dragging-translator
    (event com-move-event piano-roll
           :tester ((object presentation x)
                    (not (hovering-for-resize-p x presentation)))
           :pointer-documentation "Move event"
           :menu nil)
    (object presentation x y)
  (multiple-value-bind (old-x old-y) (output-record-position presentation)
    (list presentation (- x old-x) (- y old-y))))

(define-piano-roll-command (com-resize-event) ((record event) (offset-x real :default 0) (offset-y real :default 0))
  (declare (ignorable record offset-x offset-y))
  (let* ((stream (find-pane-named *application-frame* 'piano-roll-pane))
         (damaged-region +nowhere+)
         (event (presentation-object record))
         (event-beat (beat event))
         (old-sustain (event-value event :sustain)))
    (erase-output-record record stream nil)
    (block tracking-pointer
      (tracking-pointer (stream)
        (:pointer-motion (x)
                         (let* ((x-beat (x-pixel-to-beat-quantized x *application-frame*))
                                (new-sustain (max (slot-value *application-frame* 'grid-size) (- x-beat event-beat)))
                                (rect (multiple-value-list (bounding-rectangle* record))))
                           (when (/= old-sustain new-sustain)
                             (handle-repaint stream damaged-region)
                             (setf (elt rect 2) (* (slot-value *application-frame* 'beat-size)
                                                   (+ new-sustain event-beat))
                                   damaged-region (apply 'make-rectangle* rect)
                                   (event-value event :sustain) new-sustain
                                   old-sustain new-sustain)
                             (with-output-recording-options (stream :record nil :draw t)
                               (present event 'event :stream stream :view +graphical-view+)))))
        (:pointer-button-release ()
                                 (return-from tracking-pointer nil))))
    (stream-add-output-record stream record)))

(define-presentation-to-command-translator event-resizing-translator
    (event com-resize-event piano-roll
           :tester ((object presentation x)
                    (when (hovering-for-resize-p x presentation)
                      (climi::set-sheet-pointer-cursor ; (setf pointer-cursor) doesn't seem to work, so we do this
                       (find-port)
                       (find-pane-named *application-frame* 'piano-roll-pane)
                       :horizontal-scroll)
                      t))
           :pointer-documentation "Resize event"
           :menu nil)
    (object presentation window x y)
  (list presentation x y))

(define-command-table piano-roll-view-command-table)

(define-command (com-beat-size :name t :menu t
                               :command-table piano-roll-view-command-table)
    ((size 'real :default 50))
  "Set the length of one beat in pixels."
  (setf (slot-value *application-frame* 'beat-size) size))

(define-command (com-increase-beat-size :name t :menu t
                                        :command-table piano-roll-view-command-table
                                        :keystroke (#\= :control))
    (&key
     (increase 'real :default 10))
  "Increase the length of one beat by INCREASE pixels."
  (incf (slot-value *application-frame* 'beat-size) increase))

(define-command (com-decrease-beat-size :name t :menu t
                                        :command-table piano-roll-view-command-table
                                        :keystroke (#\- :control))
    (&key
     (decrease 'real :default 10))
  "Decrease the length of one beat by DECREASE pixels."
  (decf (slot-value *application-frame* 'beat-size) decrease))

(define-command-table piano-roll-tools-command-table
  :inherit-from (thundersnow-common-tools-command-table)
  :inherit-menu t)

(define-command-table piano-roll-help-command-table
  :inherit-from (thundersnow-common-help-command-table)
  :inherit-menu t)


(define-gesture-name :erase :pointer-button (:middle))

(define-presentation-action erase (event nil piano-roll :gesture :erase :pointer-documentation "Erase event")
    (event)
  (com-erase event)
  (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'piano-roll-pane) :force-p t))

;; FIX:
;; (define-gesture-name :motion :pointer-motion (:left))

(defun piano-roll ()
  "Open a piano-roll."
  (find-application-frame 'piano-roll))
