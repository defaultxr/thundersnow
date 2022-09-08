;;;; thundersnow.lisp
;;; this file mostly contains the code for the main thundersnow gui; common gui functionality is in common.lisp
;; other files:
;; - browser.lisp
;; - keyboard-gui.lisp
;; - piano-roll.lisp
;; - stepseq.lisp
;; - tracker.lisp
;; NOTES:
;; - https://common-lisp.net/project/mcclim/static/manual/mcclim.html
;; - https://github.com/McCLIM/McCLIM/wiki/Default-Keyboard-and-Mouse-Gestures
;; (setf (frame-current-layout (thundersnow)) 'default)
;; (ql:quickload :clim-listener)
;; (clim-listener:run-listener)
;; arguments for define-drag-and-drop-translator can include:
;; - destination-object context-type frame presentation destination-presentation event window x y
;; zoom should be proportional; zoom series is: 1, 2, 3, 4, 6, 8, 12, 16, 24, 32
;; to get the clim debugger on errors, wrap your code in (clim-debugger:with-debugger () ...)
;; ... or add (:debugger t) to the define-application-frame when supported? (see https://www.youtube.com/watch?v=kfBmRsPRdGg&start=17m0s )

(in-package #:thundersnow/thundersnow)

(defvar *tmp* nil
  "Temporary variable for convenience during development.")

;;; thundersnow frame

(define-application-frame thundersnow (thundersnow-application-frame)
  ()
  (:command-table (thundersnow :inherit-from (thundersnow-common-command-table
                                              thundersnow-file-command-table
                                              thundersnow-edit-command-table
                                              thundersnow-view-command-table
                                              thundersnow-tools-command-table
                                              thundersnow-help-command-table)
                               :menu (("File" :menu thundersnow-file-command-table)
                                      ("Edit" :menu thundersnow-edit-command-table)
                                      ("View" :menu thundersnow-view-command-table)
                                      ("Tools" :menu thundersnow-tools-command-table)
                                      ("Help" :menu thundersnow-help-command-table))))
  (:default-initargs :pretty-name "thundersnow")
  (:panes
   (logo :application :scroll-bars nil)
   (tempo (make-pane 'tempo-pane))
   (scope (make-pane 'scope :name 'scope))
   (patterns-pane (make-pane 'patterns-pane))
   (pattern-pane (make-pane 'pattern-pane))
   (interactor :interactor)
   (pointer-documentation-pane (make-pane 'pointer-documentation-pane)))
  (:layouts
   (default
    (vertically ()
      (1/10 (horizontally () ;; toolbar
              (8/10 logo)
              ;; FIX: add server status pane with cpu load, number of active ugens, active synths, active groups, and number of synthdefs
              (1/10 tempo)
              (1/10 scope)))
      (make-pane 'clime:box-adjuster-gadget)
      (7/10 (horizontally ()
              (1/2 patterns-pane)
              (make-pane 'clime:box-adjuster-gadget)
              (1/2 pattern-pane)))
      (make-pane 'clime:box-adjuster-gadget)
      (2/10 interactor)
      (1/200 (horizontally ()
               pointer-documentation-pane))))
   (test-interactor
    (vertically ()
      interactor))))

(defmethod initialize-instance :before ((thundersnow thundersnow) &key &allow-other-keys)
  (thundersnow-ensure-initialized))

(defmethod note-frame-enabled :after (frame-manager (frame thundersnow))
  (declare (ignore frame-manager))
  (let ((tempo-pane (find-pane-named frame 'tempo)))
    (clime:schedule-event tempo-pane (make-instance 'timer-event :sheet tempo-pane) 0.1)))

(defmethod pane-pattern ((frame standard-application-frame))
  (pane-pattern (pattern-pane frame)))

(defmethod (setf pane-pattern) (pattern (frame standard-application-frame))
  (when-let ((pane (pattern-pane frame)))
    (setf (pane-pattern pane) pattern)))

(defmethod backend-task-added ((frame thundersnow) task)
  (thundersnow-backend-update frame task))

(defmethod backend-task-removed ((frame thundersnow) task)
  (thundersnow-backend-update frame task))

(defmethod backend-task-play-event ((frame thundersnow) task event)
  (thundersnow-backend-update frame task event))

(defun thundersnow-backend-update (frame object &optional extra-object)
  (etypecase object
    (cl-patterns::task
     (unless extra-object
       (redisplay-frame-pane frame (patterns-pane frame) :force-p t)
       (return-from thundersnow-backend-update))
     (typecase extra-object
       (event
        (case (event-value extra-object :type)
          (:tempo
           (update-tempo-information (event-value object :tempo)))
          (:note
           (let ((pattern (task-pattern object)))
             (dolist (pane (list (patterns-pane frame)
                                 (pattern-pane frame)))
               (when-let ((record (find-record pane pattern)))
                 (redisplay-output-record record pane))))
           (redisplay-frame-pane frame (patterns-pane frame) :force-p t)
           ;; i'm not sure why redisplay-output-record isn't working here..
           ;; (let ((pattern (task-pattern object)))
           ;;   (dolist (pane (list (patterns-pane frame)
           ;;                       ;; (pattern-pane frame)
           ;;                       ))
           ;;     (when-let ((record (find-presentation pane pattern)))
           ;;       (redisplay-output-record record pane))
           ;;     ;; (format *debug-io* "~&~S ~S~%"  (type-of (find-presentation pane pattern)))
           ;;     ))
           )))))))

;;; commands

(define-command-table thundersnow-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command-table thundersnow-edit-command-table
  :inherit-from (thundersnow-common-edit-command-table)
  :inherit-menu t)

(define-command-table thundersnow-view-command-table
  :inherit-from (thundersnow-common-view-command-table)
  :inherit-menu t)

(define-command-table thundersnow-tools-command-table
  :inherit-from (thundersnow-common-tools-command-table)
  :inherit-menu t)

(define-command-table thundersnow-help-command-table
  :inherit-from (thundersnow-common-help-command-table)
  :inherit-menu t)

(define-presentation-action select (pattern nil thundersnow
                                    :gesture :select
                                    :pointer-documentation "Select pattern")
                            (pattern)
  (with-room-for-graphics ()
    (format t "~&Select pattern: ~s~%" pattern))
  (let ((pattern-pane (pattern-pane *application-frame*)))
    (setf (pane-pattern pattern-pane) pattern)
    (redisplay-frame-pane *application-frame* pattern-pane :force-p t))
  nil)

(define-presentation-action play (pattern nil thundersnow
                                  :gesture nil
                                  :tester ((object)
                                           (and (pattern-p object)
                                                (eql (clp::pattern-status object) :stopped)))
                                  :pointer-documentation "Play pattern")
                            (pattern)
  (play pattern)
  (redisplay-frame-pane *application-frame* (patterns-pane *application-frame*) :force-p t)
  nil)

(define-presentation-action end (pattern nil thundersnow
                                 :gesture nil
                                 :tester ((object)
                                          (and (pattern-p object)
                                               (eql (clp::pattern-status object) :playing)))
                                 :pointer-documentation "End pattern")
                            (pattern)
  (end pattern)
  (redisplay-frame-pane *application-frame* (patterns-pane *application-frame*) :force-p t)
  nil)

(define-presentation-action stop (pattern nil thundersnow
                                  :gesture nil
                                  :tester ((object)
                                           (and (pattern-p object)
                                                (eql (clp::pattern-status object) :playing)))
                                  :pointer-documentation "Stop pattern")
                            (pattern)
  (stop pattern)
  (redisplay-frame-pane *application-frame* (patterns-pane *application-frame*) :force-p t)
  nil)

(defmethod frame-standard-output ((frame thundersnow))
  (find-pane-named frame 'interactor))

;;; patterns pane

(defclass patterns-pane (application-pane)
  ((dictionary :initarg :dictionary :initform cl-patterns::*pdef-dictionary* :accessor pane-dictionary))
  (:default-initargs :name 'patterns-pane
                     :display-function 'display-patterns
                     :foreground (theme-color :foreground)
                     :background (theme-color :background)))

(defun patterns-pane (&optional frame)
  "Get the patterns-pane of FRAME.

See also: `pattern-pane', `thundersnow'"
  (find-pane-named (or frame (thundersnow)) 'patterns-pane))

(defun display-patterns (frame stream)
  "Display all defined patterns in STREAM."
  (declare (ignorable frame))
  (let ((*package* (find-package 'thundersnow/thundersnow))) ;; FIX: there is probably a better way to change the default *package* of a frame/stream
    (updating-output (stream)
      (dolist (pdef (all-pdefs))
        (present pdef 'pattern :stream stream)))))

(define-presentation-method present (pattern (type pattern) (stream patterns-pane) (view textual-view) &key)
  (let ((status (clp::pattern-status pattern)))
    (updating-output (stream :uniqume-id pdef :cache-value status)
      (with-output-as-presentation (stream pattern 'pattern)
        (with-room-for-graphics (stream)
          (draw-rectangle* stream 0 0 16 16 :ink (case status
                                                   (:starting +green-yellow+)
                                                   (:playing +green+)
                                                   (:ending +orange+)
                                                   (:stopped +red+)))
          (draw-text* stream (format nil "~S" pattern) 16 0 :align-y :bottom))))))

;;; pattern pane

(defclass pattern-pane (application-pane)
  ((pattern :initarg :pattern :initform nil :accessor pane-pattern))
  (:default-initargs :name 'pattern-pane
                     :display-function 'display-pattern
                     :default-view +graphical-view+
                     :foreground (theme-color :foreground)
                     :background (theme-color :background)))

(defun pattern-pane (&optional frame)
  "Get the pattern-pane of FRAME.

See also: `patterns-pane', `thundersnow'"
  (find-pane-named (or frame (thundersnow)) 'pattern-pane))

(define-presentation-type pattern ()
  :inherit-from t)

(define-presentation-type slot (object accessor)
  :inherit-from 'pattern)

(define-presentation-method present (object (type number) stream (view graphical-view) &key)
  (prin1 object stream))

(define-presentation-method present (object (type symbol) stream (view graphical-view) &key)
  (prin1 object stream))

(define-presentation-method present (object (type pattern) stream (view graphical-view) &key)
  (prin1 object stream))

(define-presentation-method present ((object pattern) (type pattern) stream (view graphical-view) &key)
  (let ((class (class-of object)))
    (format stream "(~S" (class-name class))
    (closer-mop:ensure-finalized class)
    (multiple-value-bind (req opt) (parse-ordinary-lambda-list (function-arglist (class-name class)))
      (dolist (slot (append req (mapcar #'car opt)))
        (format stream " ")
        (present (slot-value object slot) `(slot ,object ,slot) :stream stream)))
    (format stream ")")))

(define-presentation-action change-value (slot nil thundersnow
                                          :gesture :select
                                          :pointer-documentation
                                          ((object stream presentation)
                                           (let ((type (presentation-type presentation)))
                                             (format stream "Change ~S ~S value" (class-name (class-of (second type))) (third type)))))
                            (object presentation)
  (setf *tmp* presentation)
  (format t "~&Change-value: slot: ~S ~S~%" object presentation)
  (destructuring-bind (type pattern slot) (presentation-type presentation)
    (declare (ignore type))
    (setf (pattern-value pattern slot)
          (accepting-values ()
            (accept t :prompt (format nil "Change ~S ~S value" (class-name (class-of pattern)) slot)
                      :default object)))))

(define-presentation-method present ((pbind pbind) (type pattern) stream (view graphical-view) &key)
  (with-room-for-graphics (stream)
    (format stream "(~S " 'pbind)
    (with-room-for-graphics (stream)
      (formatting-table (stream)
        (doplist (key val (slot-value pbind 'cl-patterns::pairs))
          (formatting-row (stream)
            (formatting-cell (stream)
              (present key 'symbol :stream stream))
            (formatting-cell (stream)
              (present val `(slot ,pbind ,key) :stream stream))))))
    (format stream ")")))

(define-presentation-method present ((pdef pdef) (type pattern) stream (view graphical-view) &key)
  (with-room-for-graphics (stream)
    (format stream "(~S " 'pdef)
    (let ((name (pdef-name pdef)))
      (present name (presentation-type-of name) :stream stream))
    (format stream "~%")
    (indenting-output (stream "    ")
      (with-room-for-graphics (stream)
        (let ((val (pdef-pattern pdef)))
          (present val (presentation-type-of val) :stream stream))
        (format stream ")")))))

(define-gesture-name :select :pointer-button (:left))

(defun display-pattern (frame stream &optional pattern)
  "Display a pattern in STREAM."
  (declare (ignorable frame))
  (let ((*package* (find-package 'thundersnow))
        (pattern (or pattern (pane-pattern stream))))
    (with-output-as-presentation (stream pattern 'pattern)
      (updating-output (stream :unique-id pattern)
        (present pattern 'pattern :stream stream)))))

;;; main

(defun thundersnow (&rest args &key pattern)
  "Start thundersnow."
  (let ((ts (apply #'make-or-find-application-frame 'thundersnow args)))
    (when pattern
      (setf (pane-pattern ts) pattern))
    ts))
