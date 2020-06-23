(in-package #:thundersnow/piano-roll)

;;;; piano-roll
;;; a piano roll, obviously
;; TODO:
;; - allow notes to be added to the left of the start of the eseq (in which case it becomes the new 0 and the other notes are moved later in time)
;; FIX: check out: with-temporary-margins, with-translation
;; NOTES:
;; - define-application-command arguments can accept defaults: (arg-name arg-type :default default-value)

;;; util

(defparameter tmp nil)

;;; eseq / event stuff

(defun event-presentation-equal (event1 event2)
  "False if EVENT1 and EVENT2 differ in their CLIM presentation representation. This is used in the `updating-output' for `draw-piano-roll'."
  (and (eql (dur event1) (dur event2))
       (eql (beat event1) (beat event2))
       (eql (event-value event1 :midinote) (event-value event2 :midinote))))

;;; gui stuff

(defclass graphical-view (view)
  ())

(defconstant +graphical-view+ (make-instance 'graphical-view))

(defclass %background ()
  ())

(define-presentation-type event ())

(defclass piano-roll-pane (application-pane)
  ((%saved-extent :initform nil :documentation "The saved scroll position to return to when redisplaying."))
  (:default-initargs
   :name 'piano-roll
   :display-function 'draw-piano-roll
   :display-time :command-loop
   :default-view +graphical-view+
   :foreground +white+
   :background (make-rgb-color 0.3 0.3 0.4)))

(defmethod compose-space ((pane piano-roll-pane) &key width height)
  (make-space-requirement :width 500
                          :height (* 128 (slot-value (pane-frame pane) 'y-size))))

(defmethod handle-repaint :before ((pane piano-roll-pane) region)
  ;; save the scroll position
  (with-slots (%saved-extent) pane
    (setf %saved-extent (pane-viewport-region pane))))

;; (defmethod handle-repaint ((pane piano-roll-pane) region)
;;   (draw-piano-roll *application-frame* pane))

;; (defmethod handle-event ((pane piano-roll-pane) (event pointer-motion-event))
;;   (when-let ((doc-pane (find-pane-named *application-frame* 'pointer-documentation-pane)))
;;     ;; (redisplay-frame-pane *application-frame* doc-pane :force-p t)
;;     (setf (pane-needs-redisplay doc-pane) t)
;;     ))

(define-presentation-method present (background (type %background) stream (view graphical-view) &key)
  (let* ((region (sheet-region stream))
         (width (rectangle-width region))
         (height (rectangle-height region)))
    (draw-rectangle* stream
                     0 0 width height
                     :filled nil)))

(define-presentation-method present (event (type event) stream (view graphical-view) &key)
  (let* ((frame (pane-frame stream))
         (beat-size (slot-value frame 'beat-size))
         (x-start (* beat-size (beat event)))
         (y-size (slot-value frame 'y-size))
         (midinote (event-value event :midinote))
         (y-top (* y-size midinote)))
    (draw-rectangle* stream
                     x-start
                     y-top
                     (+ x-start (* (event-value event :dur) beat-size))
                     (+ y-top y-size)
                     :ink +red+)
    (draw-text* stream
                (note-text midinote)
                x-start (* y-size (1+ midinote))
                :align-y :top)))

(defmethod frame-drag-and-drop-feedback
    ((frame standard-application-frame) from-presentation (stream output-recording-stream)
     initial-x initial-y x y state)
  (with-bounding-rectangle* (fp-x1 fp-y1 fp-x2 fp-y2)
      from-presentation
    ;; Offset from origin of presentation is preserved throughout
    (with-identity-transformation (stream)
      (ecase state
        (:highlight
         (with-output-recording-options (stream :record nil)
           (draw-rectangle* stream highlite-x1 highlite-y1 highlite-x2 highlite-y2
                            :filled nil :line-dashes #(4 4))))
        (:unhighlight
         (with-output-recording-options (stream :record nil)
           (draw-rectangle* stream
                            highlite-x1 highlite-y1
                            (1+ highlite-x2) (1+ highlite-y2)
                            :ink (medium-background (sheet-medium stream))))
         (stream-replay stream (make-rectangle* highlite-x1 highlite-y1
                                                (1+ highlite-x2) (1+ highlite-y2))))))))

(define-application-frame piano-roll ()
  ((eseq :initarg :eseq :initform (eseq) :type eseq :documentation "The `eseq' instance.")
   (beat-size :initarg :beat-size :initform 200 :documentation "The size of one beat, in pixels.") ;; FIX: make the initform change based on the beats-per-bar
   (grid-size :initarg :beat-size :initform 1/4 :documentation "The grid size in beats.")
   (y-size :initarg :y-size :initform 40))
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
   ;; (piano-roll-pane (make-clim-application-pane
   ;;                   :name 'piano-roll
   ;;                   :scroll-bars t
   ;;                   :incremental-redisplay t
   ;;                   :display-function 'draw-piano-roll
   ;;                   :default-view +graphical-view+
   ;;                   :foreground +white+
   ;;                   :background (make-rgb-color 0.3 0.3 0.4)))
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
      (5/6 (scrolling ()
             (make-pane 'piano-roll-pane)))
      (1/6 interactor-pane)
      pointer-documentation-pane)))
  (:menu-bar t))

;; - define-presentation-translator
;; - define-presentation-to-command-translator
;; - define-presentation-action
;; - define-drag-and-drop-translator
;; - menu-choose
;; - menu-choose-from-drawer

(defun draw-piano-roll (frame stream)
  (with-slots (eseq) frame
    (let* ((region (sheet-region stream))
           (beat-size (slot-value frame 'beat-size))
           (stream-width (rectangle-width region))
           (y-size (slot-value frame 'y-size))
           (stream-height (* y-size 128))
           (events (etypecase eseq
                     (list eseq)
                     (eseq (eseq-events eseq))
                     (pattern (next-upto-n eseq)))))
      (with-room-for-graphics (stream :first-quadrant t)
        (present (make-instance '%background) '%background :stream stream)
        (loop :for x :from 0 :upto (max (+ (dur eseq) 32) (/ stream-width beat-size))
              :for xpos = (* x beat-size)
              :do
                 (draw-line* stream xpos 0 xpos stream-height :ink +gray+)
                 (draw-text* stream (write-to-string x) (1+ xpos) 1 :ink +gray+))
        (loop :for y :from 0 :upto 127
              :for ypos := (* y y-size)
              :for top-ypos := (* (1+ y) y-size)
              :do
                 (draw-line* stream 0 ypos stream-width ypos :ink +gray+)
                 (draw-text* stream (note-text y) 1 top-ypos :align-y :top :ink +gray+))
        (dolist (event events)
          (updating-output (stream :unique-id event :cache-value event :cache-test #'event-presentation-equal)
            (present event 'event :stream stream))))))
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

(define-presentation-to-command-translator erase-event (event erase-event piano-roll :gesture :erase :pointer-documentation "Erase this event") (event)
  (with-swank-output
    (print 'command-translator))
  (list event))

(define-piano-roll-command (com-drag-event) ((record event) (offset-x real :default 0) (offset-y real :default 0))
  ;; offset-x and offset-y are the offset that the presentation was clicked.
  ;; for some reason (with-room-for-graphics (stream :first-quadrant t) ...) doesn't seem to work here?
  (declare (ignorable offset-y))
  (let ((stream (find-pane-named (find-application-frame 'piano-roll) 'piano-roll))
        (event (presentation-object record))
        (record-width (rectangle-width record)))
    (drag-output-record stream record
                        :feedback (lambda (record stream old-x old-y x y mode)
                                    (declare (ignorable record stream old-x old-y x y mode))
                                    (ecase mode
                                      (:erase
                                       (erase-output-record record stream))
                                      (:draw
                                       (with-slots (beat-size y-size) (pane-frame stream)
                                         (let* ((max-y (rectangle-max-y (sheet-region stream)))
                                                (inv-y (- max-y y))
                                                (new-beat (floor (/ x beat-size)))
                                                (new-y (floor (/ inv-y y-size)))
                                                (act-x (round-by-direction x (- beat-size)))
                                                (act-y (round-by-direction y (- y-size))))
                                           (setf (output-record-position record) (values act-x act-y))
                                           (let* ((event (presentation-object record))
                                                  (dur (event-value event :dur)))
                                             (setf (beat event) new-beat
                                                   (event-value event :midinote) new-y)
                                             (stream-add-output-record stream record)
                                             (repaint-sheet stream (make-rectangle* act-x act-y (+ act-x (* dur beat-size)) (+ act-y y-size)))))))))
                        :finish-on-release t :multiple-window nil)))

(define-piano-roll-command (com-resize-event) ((record event) (offset-x real :default 0) (offset-y real :default 0))
  ;; offset-x and offset-y are the offset that the presentation was clicked.
  ;; for some reason (with-room-for-graphics (stream :first-quadrant t) ...) doesn't seem to work here?
  (let ((stream (find-pane-named (find-application-frame 'piano-roll) 'piano-roll))
        (event (presentation-object record))
        (record-width (rectangle-width record)))
    (setf tmp record)
    (let ((old-sustain (event-value event :sustain)))
      (multiple-value-bind (old-x old-y) (output-record-position record)
        (drag-output-record stream record
                            :feedback (lambda (record stream prev-x prev-y x y mode)
                                        (declare (ignorable record stream prev-x prev-y x y mode))
                                        (ecase mode
                                          (:erase
                                           (erase-output-record record stream))
                                          (:draw
                                           (with-slots (beat-size y-size) (pane-frame stream)
                                             (let* ((sustain (+ old-sustain (- x offset-x)))
                                                    (new-end (+ old-x (* sustain beat-size))))
                                               ;; (with-swank-output
                                               ;;   (print ))
                                               (setf (output-record-hit-detection-rectangle* record) new-end)
                                               ;; (setf (event-value event :sustain) sustain)
                                               (stream-add-output-record stream record)
                                               (repaint-sheet stream (make-rectangle* old-x old-y (+ old-x (* sustain beat-size)) (+ old-y y-size))))))))
                            :finish-on-release t :multiple-window nil)))))

(defun hovering-for-resize-p (x presentation)
  "True if the mouse (whose x position is provided as the X argument) is hovering over PRESENTATION's right side."
  (>= (- x (output-record-position presentation)) (- (rectangle-width presentation) 8)))

(define-presentation-action move (event nil piano-roll
                                        :tester ((object presentation x)
                                                 (not (hovering-for-resize-p x presentation)))
                                        :pointer-documentation
                                        "Move event")
    (event)
  nil)

(define-presentation-to-command-translator event-dragging-translator
    (event com-drag-event piano-roll
           :tester ((object presentation x)
                    (not (hovering-for-resize-p x presentation)))
           :pointer-documentation "Move event"
           :menu nil)
    (object presentation x y)
  (multiple-value-bind (old-x old-y) (output-record-position presentation)
    (list presentation (- x old-x) (- y old-y))))

(define-presentation-action resize (event nil piano-roll
                                          :tester ((object presentation x)
                                                   (and (hovering-for-resize-p x presentation)
                                                        (clim-internals::set-sheet-pointer-cursor ; (setf pointer-cursor) doesn't seem to work, so we do this
                                                         (find-port)
                                                         (find-pane-named (find-application-frame 'piano-roll) 'piano-roll)
                                                         :horizontal-scroll)
                                                        t))
                                          :pointer-documentation
                                          "Resize event")
    (event)
  nil)

(define-presentation-to-command-translator event-resizing-translator
    (event com-resize-event piano-roll
           :tester ((object presentation x)
                    (and (hovering-for-resize-p x presentation)
                         (clim-internals::set-sheet-pointer-cursor ; (setf pointer-cursor) doesn't seem to work, so we do this
                          (find-port)
                          (find-pane-named (find-application-frame 'piano-roll) 'piano-roll)
                          :horizontal-scroll)
                         t))
           :pointer-documentation "Resize this event"
           :menu nil)
    (object presentation x y)
  (multiple-value-bind (old-x old-y) (output-record-position presentation)
    (list presentation (- x old-x) (- y old-y))))

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

(define-gesture-name :add :pointer-button (:left))

(define-presentation-action add (%background nil piano-roll :gesture :add :pointer-documentation
                                             ((%background x y stream frame)
                                              (with-slots (beat-size grid-size) frame
                                                (format stream "Add event at beat ~$, midinote ~a" (/ x beat-size) y))))
    (%background)
  nil)

(define-gesture-name :erase :pointer-button (:middle))

(define-presentation-action erase (event nil piano-roll :gesture :erase :pointer-documentation "Erase event")
    (event)
  (com-erase event)
  (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'piano-roll) :force-p t))

;; FIX:
;; (define-gesture-name :motion :pointer-motion (:left))

(defun piano-roll ()
  "Open a piano-roll."
  (find-application-frame 'piano-roll))
