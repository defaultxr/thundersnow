(in-package #:thundersnow/piano-roll)

;;;; piano-roll
;;; a piano roll, obviously
;; TODO:
;; - allow notes to be added to the left of the start of the eseq (in which case it becomes the new 0 and the other notes are moved later in time)
;; FIX: check out: with-temporary-margins, with-translation

;;; util

(defvar *tmp* nil
  "Temporary variable for convenience during development.")

;;; gui stuff

(defun find-piano-roll-pane (pane &optional (errorp t))
  "If PANE is a `piano-roll-pane', return it. If it's a frame, try to locate a piano-roll-pane in it. Otherwise, signal an error if ERRORP, or just return nil.

See also: `find-piano-roll-frame'"
  (or (typecase pane
        (piano-roll-pane pane)
        (application-frame (find-pane-named pane 'piano-roll-pane)))
      (when errorp
        (error "Could not find a piano-roll-pane in ~s." pane))))

(defun find-piano-roll-frame (frame &optional (errorp t))
  "If FRAME is a `piano-roll', return it. If it's a pane and its frame is a piano-roll, return the frame. Otherwise, signal an error if ERRORP, or just return nil.

See also: `find-piano-roll-pane'"
  (or (when (typep frame 'piano-roll)
        frame)
      (when (and (typep frame 'application-pane)
                 (typep (pane-frame frame) 'piano-roll))
        (pane-frame frame))
      (when errorp
        (error "Could not find a piano-roll in ~s." frame))))

(defun piano-roll-width (&optional (pane (piano-roll-pane)))
  "Get the width in pixels of the sequence in the piano-roll. This will always be at least the width of the pane itself.

See also: `piano-roll-height'"
  (let* ((pane (etypecase pane
                 (pane pane)
                 (application-frame (find-pane-named pane 'piano-roll-pane))))
         (frame (pane-frame pane)))
    (with-slots (beat-size) frame
      (max (* beat-size (+ 8 (dur frame)))
           (rectangle-width (pane-viewport-region pane))))))

(defun piano-roll-height (&optional (pane (piano-roll-pane)))
  "Get the height in pixels of the piano-roll, i.e. the number of pixels that the full pitch range takes up.

See also: `piano-roll-width'"
  (pitch-to-y-pixel 0 pane))

(defun pane-left-beat (pane)
  "Get the leftmost visible beat in PANE.

See also: `pane-right-beat'"
  (x-pixel-to-beat (rectangle-min-x (pane-viewport-region pane)) (pane-frame pane)))

(defun pane-right-beat (pane)
  "Get the rightmost visible beat in PANE.

See also: `pane-left-beat'"
  (x-pixel-to-beat (rectangle-max-x (pane-viewport-region pane)) (pane-frame pane)))

(defun event-vertically-visible-p (event &optional pane)
  "True if EVENT is fully within the currently-visible pitch range of PANE.

See also: `event-horizontally-visible-p', `event-visible-p'"
  (let* ((pane (or pane (piano-roll-pane)))
         (region (pane-viewport-region pane))
         (frame (pane-frame pane))
         (top-pitch (1- (y-pixel-to-pitch (rectangle-min-y region) frame)))
         (bottom-pitch (y-pixel-to-pitch (rectangle-max-y region) frame)))
    (> top-pitch (event-value event :midinote) bottom-pitch)))

(defun event-horizontally-visible-p (event &optional pane)
  "True if EVENT is fully within the currently-visible beat range of PANE.

See also: `event-vertically-visible-p', `event-visible-p'"
  (let* ((pane (or pane (piano-roll-pane)))
         (left-beat (pane-left-beat pane))
         (right-beat (pane-right-beat pane))
         (beat (beat event)))
    (and (>= beat left-beat)
         (>= right-beat (+ beat (event-value event :sustain))))))

(defun event-visible-p (event &optional pane)
  "True if EVENT is fully visible in PANE.

See also: `event-vertically-visible-p', `event-horizontally-visible-p'"
  (and (event-vertically-visible-p event pane)
       (event-horizontally-visible-p event pane)))

(defun hovering-for-resize-p (x presentation)
  "True if the mouse (whose x position is provided as the X argument) is hovering over PRESENTATION's right side."
  (>= (- x (output-record-position presentation)) (- (rectangle-width presentation) 8)))

(defun x-pixel-to-beat (x &optional (frame (or *application-frame* (piano-roll))))
  "Convert an x pixel in FRAME to a beat number."
  (with-slots (beat-size) frame
    (/ x beat-size)))

(defun x-pixel-to-beat-quantized (x &optional (frame (or *application-frame* (piano-roll))))
  "Convert an x pixel in FRAME to a beat number, quantizing to the grid."
  (round-by (x-pixel-to-beat x frame) (grid-size frame)))

(defun x-pixel-to-beat-floored (x &optional (frame (or *application-frame* (piano-roll))))
  "Convert an x pixel in FRAME to a beat number, quantizing to the grid."
  (floor-by (x-pixel-to-beat x frame) (grid-size frame)))

(defun beat-to-x-pixel (beat &optional (frame (or *application-frame* (piano-roll))))
  "Convert a beat in FRAME to the relevant x pixel."
  (with-slots (beat-size) frame
    (* beat-size beat)))

(defun y-pixel-to-pitch (y &optional (frame (or *application-frame* (piano-roll))))
  "Convert a y pixel in FRAME to the frame's pitch type."
  (with-slots (y-size) frame
    (/ (- (piano-roll-height frame) y) y-size)))

(defun y-pixel-to-pitch-quantized (y &optional (frame (or *application-frame* (piano-roll))))
  "Convert a y pixel in FRAME to the frame's pitch type, quantizing."
  (with-slots (y-size) frame
    (floor (y-pixel-to-pitch y frame))))

(defun pitch-to-y-pixel (pitch &optional (frame (or *application-frame* (piano-roll))))
  "Convert a pitch value to a y pixel in FRAME."
  (with-slots (y-size) (typecase frame
                         (pane (pane-frame frame))
                         (application-frame frame))
    (- (* y-size 128) (* y-size pitch))))

(defun scroll-top-to (pane pixel)
  "Scroll PANE such that PIXEL is at the top of the view.

See also: `scroll-center-to', `scroll-bottom-to', `scroll-focus-pitch'"
  (scroll-extent pane 0 pixel))

(defun scroll-center-to (pane pixel)
  "Scroll PANE such that PIXEL is in the vertical center of the view.

See also: `scroll-top-to', `scroll-bottom-to', `scroll-focus-pitch'"
  (scroll-top-to pane (- pixel (/ (truncate (rectangle-height (pane-viewport-region pane))) 2))))

(defun scroll-bottom-to (pane pixel)
  "Scroll PANE such that PIXEL is at the bottom of the view.

See also: `scroll-top-to', `scroll-center-to', `scroll-focus-pitch'"
  (scroll-top-to pane (- pixel (truncate (rectangle-height (pane-viewport-region pane))))))

(defun scroll-focus-pitch (pane pitch)
  "Scroll PANE such that PITCH is in the center of the view.

See also: `scroll-top-to', `scroll-center-to', `scroll-bottom-to'"
  (scroll-center-to pane (pitch-to-y-pixel pitch pane)))

(defclass %background ()
  ())

(define-presentation-type note ())

(define-presentation-method accept
    ((type note) stream view &key))

;; (define-presentation-type weekday ())

;; (define-presentation-method accept
;;     ((type weekday) stream (view textual-view) &key)
;;   (values (completing-from-suggestions (stream)
;;             (dotimes (i 7)
;;               (suggest (aref *days* i) i)))))

;; (define-presentation-method present
;;     (daynumber (type weekday) stream (view textual-view) &key)
;;   (write-string (aref *days* daynumber) stream))

(define-presentation-type event (&optional selectedp)
  ;; :inherit-from
  )

(defclass piano-roll-pane (scroll-position-preserving-mixin application-pane)
  ((%last-click-timestamp :initform -1000 :documentation "The timestamp of the last click the pane received."))
  (:default-initargs
   :name 'piano-roll
   :display-function 'draw-piano-roll
   :display-time :command-loop
   :default-view +graphical-view+
   :foreground +white+
   :background (theme-color :background)))

(defmethod compose-space ((pane piano-roll-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width (piano-roll-width pane)
                          :height (piano-roll-height pane)))

;; (defmethod handle-event ((pane piano-roll-pane) (event pointer-motion-event))
;;   ;; (print 'heyo *debug-io*)
;;   ;; (when-let ((doc-pane (find-pane-named *application-frame* 'pointer-documentation-pane)))
;;   ;;   ;; (redisplay-frame-pane *application-frame* doc-pane :force-p t)
;;   ;;   (setf (pane-needs-redisplay doc-pane) t)
;;   ;;   )
;;   )

(defmethod handle-event ((pane piano-roll-pane) (event pointer-button-press-event))
  ;; CLIM ports are not required to generate double click events: http://bauhh.dyndns.org:8000/clim-spec/8-2.html#_357
  ;; McCLIM doesn't generate these events, so we do it for it.
  (with-slots (%last-click-timestamp) pane
    (let ((ts (slot-value event 'climi::timestamp)))
      (if (<= (/ (- ts %last-click-timestamp) 1000) climi::*double-click-delay*)
          (queue-event pane (make-instance 'pointer-double-click-event
                                           :button (slot-value event 'climi::button)
                                           :pointer (slot-value event 'climi::pointer)
                                           :graft-y (slot-value event 'climi::graft-y)
                                           :graft-x (slot-value event 'climi::graft-x)
                                           :y (slot-value event 'climi::y)
                                           :x (slot-value event 'climi::x)
                                           :modifier-state (slot-value event 'climi::modifier-state)
                                           :sheet pane
                                           :timestamp (slot-value event 'climi::timestamp)))
          (call-next-method))
      (setf %last-click-timestamp ts))))

(defmethod handle-event ((pane piano-roll-pane) (event pointer-double-click-event))
  (let* ((frame (pane-frame pane))
         (beat (x-pixel-to-beat-floored (slot-value event 'climi::sheet-x) frame))
         (pitch (y-pixel-to-pitch-quantized (slot-value event 'climi::sheet-y) frame)))
    (com-add (event :beat beat :midinote pitch))))

(define-presentation-method present (background (type %background) stream (view graphical-view) &key)
  (draw-rectangle* stream
                   0 0 (piano-roll-width stream) (piano-roll-height stream)
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
         (note-color (theme-color :note-fill))
         (grid-color (theme-color :grid)))
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
   (grid-size :initarg :beat-size :initform nil :documentation "The number of beats between each grid line. If nil, it is calculated from the current beat-size.")
   (y-size :initarg :y-size :initform 40 :documentation "The height of one pitch value (i.e. midinote), in pixels.")
   (scale :initarg :scale :initform :major :reader scale :documentation "The scale whose notes should be highlighted."))
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
   (beat-size-slider (make-pane :slider
                                :name 'beat-size-slider
                                :orientation :horizontal
                                :min-value 1 :max-value 40
                                :value 20
                                :drag-callback
                                (lambda (slider value)
                                  (declare (ignore slider))
                                  (with-slots (beat-size) *application-frame*
                                    (let ((new-size (* 10 value)))
                                      (unless (= beat-size new-size)
                                        (setf beat-size new-size)
                                        (redisplay-frame-pane *application-frame* (piano-roll-pane) :force-p t)))))))
   (interactor-pane (make-pane 'piano-roll-interactor-pane
                               :name 'interactor-pane))
   (pointer-documentation-pane :pointer-documentation
                               :name 'doc
                               :display-time :command-loop
                               :scroll-bars nil))
  (:layouts
   (default
    (vertically ()
      (5/6 (scrolling () piano-roll-pane))
      beat-size-slider
      (1/6 (scrolling (:scroll-bar :vertical) interactor-pane))
      pointer-documentation-pane))
   (no-zoom-bar
    (vertically ()
      (5/6 (scrolling () piano-roll-pane))
      (1/6 (scrolling (:scroll-bar :vertical) interactor-pane))
      pointer-documentation-pane)))
  (:menu-bar t))

(defmethod frame-standard-output ((frame piano-roll))
  (find-pane-named frame 'interactor-pane))

(defmethod eseq-of ((piano-roll piano-roll))
  (slot-value piano-roll 'eseq))

(defmethod eseq-of ((piano-roll-pane piano-roll-pane))
  (eseq-of (pane-frame piano-roll-pane)))

(defmethod (setf eseq-of) ((eseq eseq) (piano-roll piano-roll))
  (setf (slot-value piano-roll 'eseq) eseq)
  (let ((pane (find-pane-named piano-roll 'piano-roll-pane)))
    (scroll-focus-pitch pane (print (midinote (car (eseq-events eseq)))))
    (redisplay-frame-pane piano-roll pane :force-p t)))

(defmethod (setf eseq-of) (value (piano-roll piano-roll))
  (setf (eseq-of piano-roll) (as-eseq value)))

(defmethod (setf eseq-of) (value (piano-roll-pane piano-roll-pane))
  (setf (eseq-of (pane-frame piano-roll-pane)) value))

(defmethod dur ((piano-roll piano-roll))
  (dur (eseq-of piano-roll)))

(defmethod eseq-events ((piano-roll piano-roll))
  (eseq-events (eseq-of piano-roll)))

(defmethod eseq-events ((piano-roll-pane piano-roll-pane))
  (eseq-events (eseq-of piano-roll-pane)))

(defmethod grid-size ((piano-roll piano-roll))
  (or (slot-value piano-roll 'grid-size)
      (let ((div (/ (slot-value piano-roll 'beat-size) 40)))
        (/ 1 (nth (index-before-greater-than div +powers-of-two+)
                  +powers-of-two+)))))

(defmethod (setf grid-size) (value (piano-roll piano-roll))
  (check-type value (or null (real 0)))
  (setf (slot-value piano-roll 'grid-size) value))

(defmethod (setf scale) (scale (piano-roll piano-roll))
  (setf (slot-value piano-roll 'scale) scale)
  ;; FIX: this resets the scrolling...
  (redisplay-frame-pane piano-roll (find-pane-named piano-roll 'piano-roll-pane) :force-p t))

(defun draw-piano-roll (frame stream)
  (let* ((stream-width (piano-roll-width stream))
         (stream-height (piano-roll-height stream))
         (grid-size (grid-size frame))
         (beat-size (slot-value frame 'beat-size))
         (y-size (slot-value frame 'y-size))
         (eseq (slot-value frame 'eseq))
         (events (etypecase eseq
                   (list eseq)
                   (eseq (eseq-events eseq))
                   (pattern (next-upto-n eseq))))
         (background-color (slot-value stream 'background))
         (grid-color (theme-color :grid))
         (bg-grid-mixed (mix-colors background-color grid-color 1/6))
         (grid-accent-color (make-rgb-color 1 1 1))
         (scale (slot-value frame 'scale))
         (scale-midinotes (when scale (scale-midinotes scale :octave :all))))
    (present (make-instance '%background) '%background :stream stream)
    ;; draw the vertical grid lines and beat numbers at the bottom
    (loop :for beat :from 0 :by grid-size :upto (max (+ (dur frame) 32) (/ stream-width (* beat-size grid-size)))
          :for xpos := (beat-to-x-pixel beat frame)
          :do (draw-line* stream xpos 0 xpos stream-height :ink (if (= (round beat) beat) grid-color bg-grid-mixed))
              (draw-text* stream (write-to-string beat) (1+ xpos) (1- stream-height) :ink grid-color))
    ;; draw the horizontal grid lines and note names on the side
    (loop :for y :from 0 :upto 127
          :for ypos := (- stream-height (* y y-size))
          :for top-ypos := (- stream-height (* (1+ y) y-size))
          :do (draw-line* stream 0 ypos stream-width ypos :ink grid-color)
              (draw-text* stream (note-text y) 1 top-ypos :align-y :top :ink (if (member y scale-midinotes)
                                                                                 grid-accent-color
                                                                                 grid-color)))
    ;; draw the notes (events)
    (dolist (event events)
      (updating-output (stream :unique-id event :cache-value event :cache-test #'event-presentation-equal)
        (present event 'event :stream stream)))))

(define-gesture-name :play :keyboard (#\space))

(defclass piano-roll-interactor-pane (interactor-pane)
  ())

(define-command-table piano-roll-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command (com-open :name t :menu t
                          :command-table piano-roll-file-command-table
                          :keystroke (#\o :control))
    ((pattern '(or pattern symbol) :prompt "Pattern or pdef name"))
  (let ((pattern (typecase pattern
                   (pattern pattern)
                   (symbol (find-pdef pattern t)))))
    (setf (slot-value *application-frame* 'eseq) (as-eseq pattern))))

(define-command (com-play :name t :menu t
                          :command-table piano-roll-file-command-table
                          ;; :keystroke (#\space) ;; FIX: this conflicts with command entry in the interactor
                          )
    ()
  (play (slot-value *application-frame* 'eseq)))

(define-command-table piano-roll-edit-command-table)

;; FIX: default to mouse's current position
(define-command (com-add :name t :menu t
                         :command-table piano-roll-edit-command-table
                         :keystroke (#\a :meta))
    ((event '(or event number) :prompt "Event or event start beat"))
  (let ((event (typecase event
                 (event event)
                 (number (event :beat event)))))
    (eseq-add (eseq-of *application-frame*) event)
    (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'piano-roll-pane) :force-p t)
    ;; if the new event is not visible, scroll so it is
    (let ((pane (find-pane-named *application-frame* 'piano-roll-pane)))
      (unless (event-vertically-visible-p event pane)
        (scroll-focus-pitch pane (event-value event :midinote))))))

(define-command (com-erase :name t :menu t
                           :command-table piano-roll-edit-command-table)
    ((event '(or event integer)))
  (eseq-remove (slot-value *application-frame* 'eseq) event)
  (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'piano-roll-pane) :force-p t))

(define-command (com-move :name t :menu t
                          :command-table piano-roll-edit-command-table)
    ((event 'event)
     (beat 'number)
     (y 'number))
  (let ((*standard-output* *debug-io*))
    (dprint 'com-move event beat y)))

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
  ;; offset-x and offset-y are where in the presentation that the click occurred.
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
     :menu nil
     :echo nil)
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
                                (new-sustain (max (grid-size *application-frame*) (- x-beat event-beat)))
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
     :menu nil
     :echo nil)
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

;; FIX:
;; (define-gesture-name :add :pointer-double-click-event (:left))
;; (define-gesture-name :add :pointer-button (:left :pointer-button-double-click))

;; - destination-object context-type frame presentation destination-presentation event window x y

(define-presentation-action add
    (%background nil piano-roll
     :gesture :add
     :pointer-documentation
     ((%background x y stream frame)
      (with-slots (beat-size) frame
        (format stream "Add event at beat ~$, midinote ~a" (x-pixel-to-beat-floored x frame) y)))
     ;; :tester ((object event)
     ;;          ;; (setf krovo::tmp (list object event))
     ;;          t
     ;;          )
     )
    (%background x y frame)
  (let ((beat (x-pixel-to-beat-floored x frame))
        (y-val (y-pixel-to-pitch-quantized y frame)))
    (com-add (event :beat beat :midinote y-val))
    (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'piano-roll-pane) :force-p t)))

(define-gesture-name :erase :pointer-button (:middle))

(define-presentation-action erase (event nil piano-roll :gesture :erase :pointer-documentation "Erase event")
                            (event)
  (com-erase event))

;; FIX:
;; (define-gesture-name :motion :pointer-motion (:left))

;; mode allowing use of a computer keyboard as a piano keyboard

(define-command-table music-keys :inherit-from nil)

(define-command (music-note :name t :menu t
                            :command-table music-keys)
    ((note 'integer :default 0)
     (octave 'integer :default 5))
  "Play a musical note, recording it if recording is active."
  (play (event :type :note ;; FIX: this should just be :play event type. :end should be implemented when it's available in cl-patterns.
               ;; :instrument :spt
               ;; :buffer (bdef-buffer :classic)
               :midinote (+ note (* octave 12))
               :quant 0
               :latency 0)))

(dolist* (index key (coerce "zsxdcvgbhnjm,l.;/" 'list))
  (let ((key key) (index index))
    (add-keystroke-to-command-table 'music-keys (list key)
                                    :function (lambda (gesture arg)
                                                (dprint 'hi-there index gesture arg)
                                                (list 'music-note index 5))
                                    :errorp nil)))

;; (add-keystroke-to-command-table 'music-keys (list #\escape)
;;                                 :function (lambda (gesture arg)
;;                                             (sprint 'xxxxxxxx)
;;                                             (list 'music-note 0 5))
;;                                 :errorp nil)

(define-command (com-test :name t :menu t
                          :command-table piano-roll-file-command-table)
    ()
  (with-command-table-keystrokes (keys 'music-keys)
    ;; (let (()))
    (loop :while *true* :do
      (let ((command (read-command-using-keystrokes 'music-keys
                                                    (concatenate 'list
                                                                 keys
                                                                 (list (list :keyboard #\escape 0)
                                                                       (list :keyboard #\g :control))))))
        (if (characterp command)
            (sprint command)
            (execute-frame-command *application-frame* command))))))

(defun piano-roll-pane (&optional frame)
  "Get the piano-roll-pane of FRAME.

See also: `piano-roll'"
  (find-pane-named (or frame (piano-roll)) 'piano-roll-pane))

(defun piano-roll-interactor-pane (&optional frame)
  "Get the piano-roll-pane of FRAME.

See also: `piano-roll'"
  (find-pane-named (or frame (piano-roll)) 'interactor-pane))

(defun piano-roll ()
  "Open a piano-roll or get one of the instances already open.

See also: `piano-roll-pane'"
  (make-or-find-application-frame 'piano-roll))
