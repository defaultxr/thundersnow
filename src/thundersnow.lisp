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
;; zoom factor = pow(2, steps/100). pure data keeps steps within the -400 to 400 range - https://github.com/pure-data/pure-data/pull/1659#issuecomment-1259266894
;; to get the clim debugger on errors, wrap your code in (clim-debugger:with-debugger () ...)
;; ... or add (:debugger t) to the define-application-frame when supported? (see https://www.youtube.com/watch?v=kfBmRsPRdGg&start=17m0s )
;; - add a scrolling event view a la the screenshot here: https://twitter.com/defaultxr/status/621486028751290368

(in-package #:thundersnow/thundersnow)

(defvar *tmp* nil
  "Temporary variable for convenience during development.")

;;; presentation types

(define-presentation-type pattern ()
  :inherit-from t)

(define-presentation-type slot (object accessor)
  :inherit-from 'pattern)

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
  (:menu-bar nil)
  (:default-initargs :pretty-name "thundersnow")
  (:panes
   (menu-bar (climi::make-menu-bar (frame-command-table *application-frame*) *application-frame* 'climi::hmenu-pane))
   (logo :application :scroll-bars nil :background (theme-color :background))
   (toolbar :application :scroll-bars nil :background (theme-color :background))
   (tempo (make-pane 'tempo-pane))
   (scope (make-pane 'scope :name 'scope))
   (patterns-pane (make-pane 'patterns-pane))
   (pattern-pane (make-pane 'pattern-pane))
   (documentation-pane (make-pane 'documentation-pane))
   (interactor :interactor)
   (pointer-documentation-pane (make-pane 'pointer-documentation-pane))
   (status-pane (make-pane 'status-pane :name 'status-pane)))
  (:layouts
   (default
    (vertically ()
      (1/25 (horizontally () ; menu bar and standard gadgets
              (2/10 menu-bar)
              (7/10 logo)
              (1/10 tempo)))
      (1/10 (horizontally () ; toolbar
              (9/10 toolbar)
              ;; (make-pane 'clime:box-adjuster-gadget)
              (1/10 scope)))
      (make-pane 'clime:box-adjuster-gadget)
      (7/10 (horizontally ()
              (1/2 (scrolling (:scroll-bar :vertical) patterns-pane))
              (make-pane 'clime:box-adjuster-gadget)
              (1/2 (vertically ()
                     (2/3 (scrolling (:scroll-bar :vertical) pattern-pane))
                     (make-pane 'clime:box-adjuster-gadget)
                     (1/3 (scrolling (:scroll-bar :vertical) documentation-pane))))))
      (make-pane 'clime:box-adjuster-gadget)
      (2/10 interactor)
      (1/200 (horizontally ()
               (9/10 pointer-documentation-pane)
               (1/10 status-pane)))))
   (test-interactor
    (vertically ()
      interactor)))
  (:reinitialize-frames t))

(defmethod initialize-instance :before ((thundersnow thundersnow) &key &allow-other-keys)
  (thundersnow-ensure-initialized))

(defmethod note-frame-enabled :after (frame-manager (frame thundersnow))
  (declare (ignore frame-manager))
  (let ((tempo-pane (find-pane-named frame 'tempo)))
    (clime:schedule-event tempo-pane (make-instance 'timer-event :sheet tempo-pane) 0.1)))

(defmethod frame-standard-output ((frame thundersnow))
  (find-pane-named frame 'interactor))

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

(define-command (com-select :name t :menu t :command-table thundersnow-edit-command-table)
    ((pattern 'pattern))
  (let ((pattern-pane (pattern-pane *application-frame*))
        (documentation-pane (documentation-pane *application-frame*)))
    (setf (pane-pattern pattern-pane) pattern
          (pane-object documentation-pane) (class-of pattern))))

(define-presentation-to-command-translator select (pattern com-select thundersnow) (pattern)
  (list pattern))

(define-command (com-play :name t :menu t :command-table thundersnow-edit-command-table)
    ((pattern 'pattern))
  (play pattern))

(define-presentation-to-command-translator play (pattern com-play thundersnow
                                                 :gesture nil
                                                 :tester ((object)
                                                          (and (pattern-p object)
                                                               (eql (clp::pattern-status object) :stopped)))
                                                 :pointer-documentation "Play pattern")
                                           (pattern)
  (list pattern))


(define-command (com-end :name t :menu t :command-table thundersnow-edit-command-table)
    ((pattern 'pattern))
  (end pattern))

(define-presentation-to-command-translator end (pattern com-end thundersnow
                                                :gesture nil
                                                :tester ((object)
                                                         (and (pattern-p object)
                                                              (eql (clp::pattern-status object) :playing)))
                                                :pointer-documentation "End pattern")
                                           (pattern)
  (list pattern))

(define-command (com-stop :name t :menu t :command-table thundersnow-edit-command-table)
    ((pattern 'pattern))
  (stop pattern))

(define-presentation-to-command-translator stop (pattern com-stop thundersnow
                                                 :gesture nil
                                                 :tester ((object)
                                                          (and (pattern-p object)
                                                               (eql (clp::pattern-status object) :playing)))
                                                 :pointer-documentation "Stop pattern")
                                           (pattern)
  (list pattern))

(define-command (com-change-value :name t :menu t :command-table thundersnow-edit-command-table)
    ((pattern 'pattern :prompt "Pattern")
     (slot 'slot :prompt "Slot")
     (value t :prompt "Value"))
  (setf (pattern-value pattern slot) value))

(define-presentation-to-command-translator change-value (slot com-change-value thundersnow
                                                         :gesture :select)
                                           (object presentation)
  (let ((type (presentation-type presentation)))
    (list (second type) (third type))))

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
  (let ((*package* (find-package 'thundersnow/thundersnow))) ; FIX: there is probably a better way to change the default *package* of a frame/stream
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

(define-presentation-method present (object (type number) stream (view graphical-view) &key)
  (prin1 object stream))

(define-presentation-method present (object (type symbol) stream (view graphical-view) &key)
  (prin1 object stream))

(define-presentation-method present (object (type pattern) stream (view graphical-view) &key)
  (prin1 object stream))

(defun format-pattern-name (stream name)
  (with-drawing-options (stream :ink (theme-color :pattern-name))
    (format stream "~S" name)))

(defmethod format-pattern (stream (pattern pattern) &key print-values)
  (format stream "(")
  (with-drawing-options (stream :ink (theme-color :pattern-name))
    (format stream "~S" (class-name (class-of pattern))))
  (let ((start (multiple-value-list (stream-cursor-position stream))))
    (format stream " ")
    (let ((space-start (multiple-value-list (stream-cursor-position stream))))
      (format-pattern-parameters stream pattern :print-values print-values)
      (when (equalp space-start (multiple-value-list (stream-cursor-position stream)))
        (setf (stream-cursor-position stream) (values-list start)))))
  (format stream ")"))

(defmethod format-pattern-parameters (stream (pattern pattern) &key print-values)
  (let ((class (class-of pattern)))
    (closer-mop:ensure-finalized class)
    (multiple-value-bind (req opt) (parse-ordinary-lambda-list (function-arglist (class-name class)))
      (dolist (slot (append req (mapcar #'car opt)))
        (format stream " ")
        (present (slot-value pattern slot) `(slot ,pattern ,slot) :stream stream)))))

(define-presentation-method present ((object pattern) (type pattern) stream (view graphical-view) &key)
  (format-pattern stream object))

(define-presentation-method present ((pbind pbind) (type pattern) stream (view graphical-view) &key)
  (with-room-for-graphics (stream)
    (format stream "(")
    (format-pattern-name stream 'pbind)
    (format stream " ")
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
    (format stream "(")
    (format-pattern-name stream 'pdef)
    (format stream " ")
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

;;; documentation pane

(defclass documentation-pane (application-pane)
  ((object :initarg :object :initform nil :accessor pane-object))
  (:default-initargs :name 'documentation-pane
                     :display-function 'display-documentation
                     :default-view +textual-view+
                     :foreground (theme-color :foreground)
                     :background (theme-color :background)))

(defun documentation-pane (&optional frame)
  "Get the documentation-pane of FRAME.

See also: `patterns-pane', `pattern-pane', `thundersnow'"
  (find-pane-named (or frame (thundersnow)) 'documentation-pane))

(defun display-documentation (frame stream &optional object)
  "Display documentation in STREAM."
  (declare (ignorable frame))
  (let ((*package* (find-package 'thundersnow))
        (object (or object (pane-object stream))))
    (describe object stream)))

;;; main

(defun thundersnow (&rest args &key pattern)
  "Start thundersnow."
  (let ((ts (apply #'make-or-find-application-frame 'thundersnow args)))
    (when pattern
      (setf (pane-pattern ts) pattern))
    ts))
