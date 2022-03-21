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

;;; patterns pane

(defclass patterns-pane (application-pane)
  ((dictionary :initarg :dictionary :initform cl-patterns::*pdef-dictionary* :accessor pane-dictionary))
  (:default-initargs
   :name 'pattern
   :display-function 'display-patterns
   ;; :default-view +textual-view+
   ))

(define-presentation-method present (pattern (type pattern) (stream patterns-pane) (view textual-view) &key)
  (with-room-for-graphics (stream)
    (draw-rectangle* stream 0 0 16 16 :ink (if (playing-p pattern)
                                               +green+
                                               +red+))
    (draw-text* stream (format nil "~S" pattern) 16 0 )))

(defun display-patterns (frame stream)
  "Display all defined patterns in STREAM."
  (declare (ignorable frame))
  (bind (((:accessors dictionary) stream)
         (*package* (find-package 'thundersnow/thundersnow))) ;; FIX: there is probably a better way to change the *package* of a frame or stream
    ;; (format-textual-list (all-pdefs) (lambda (p str)
    ;;                                    (present p 'pattern :stream stream)))
    (dolist (pdef (all-pdefs))
      (with-output-as-presentation (stream pdef 'pattern)
        (updating-output (stream :unique-id pdef)
          (present pdef 'pattern :stream stream))))))

;;; pattern pane

(defclass pattern-pane (application-pane)
  ((pattern :initarg :pattern :initform nil :accessor pane-pattern))
  (:default-initargs
   :name 'pattern
   :display-function 'display-pattern
   :default-view +textual-view+))

(define-presentation-type pattern ())

(define-presentation-method present (pattern (type pattern) stream (view textual-view) &key)
  (print pattern stream))

(define-gesture-name :select :pointer-button (:left))

(define-gesture-name :erase :pointer-button (:middle))

;; (define-presentation-method present ((pattern pattern) (type pattern) stream (view textual-view) &key)
;;   (print 'gen-pat-present *debug-io*)
;;   ;; (closer-mop:class-direct-slots (find-class 'pbind))
;;   (format stream "~s" pattern))

;; (define-presentation-method present ((pbind pbind) (type pattern) stream (view textual-view) &key)
;;   ;; (closer-mop:class-direct-slots (find-class 'pbind))
;;   (format stream "(~s " 'cl-patterns::pbind)
;;   (doplist (key value (slot-value pbind 'cl-patterns::pairs))
;;       (present key 'pattern)
;;     (present value 'pattern)
;;     (format stream "~%"))
;;   (format stream ")"))

;; (define-presentation-method present ((pseq pseq) (type pattern) stream (view textual-view) &key)
;;   ;; (closer-mop:class-direct-slots (find-class 'pbind))
;;   (with-output-as-presentation (stream pseq 'pattern)
;;     (with-slots ((list cl-patterns::list) (repeats cl-patterns::repeats)) pseq
;;       (format stream "(~s ~s ~s)" 'cl-patterns::pseq list repeats))))

(defun display-pattern (frame stream &optional pattern)
  "Display a pattern in STREAM."
  (declare (ignorable frame))
  (let ((pattern (or pattern (pane-pattern stream))))
    (with-output-as-presentation (stream pattern 'pattern)
      (updating-output (stream :unique-id pattern)
        (present pattern 'pattern :stream stream)))))

;;; thundersnow frame

(define-application-frame thundersnow ()
  ((inspect :initarg :inspect :initform nil :documentation "The item to inspect in the inspector pane."))
  (:command-table (thundersnow
                   :inherit-from (thundersnow-file-command-table
                                  thundersnow-edit-command-table
                                  thundersnow-view-command-table
                                  thundersnow-tools-command-table
                                  thundersnow-help-command-table)
                   :menu (("File" :menu thundersnow-file-command-table)
                          ("Edit" :menu thundersnow-edit-command-table)
                          ("View" :menu thundersnow-view-command-table)
                          ("Tools" :menu thundersnow-tools-command-table)
                          ("Help" :menu thundersnow-help-command-table))))
  (:default-initargs
   :pretty-name "thundersnow")
  (:panes
   ;; (raised (raising (:border-width 3 :background +Gray83+)
   ;;           (make-pane 'check-box :choices '("First" "Second" "Third"))))
   (logo :application
         :scroll-bars nil
         ;; :display-function
         )
   (tempo (make-pane 'tempo-pane :name 'tempo-pane))
   (scope (make-pane 'scope :name 'scope))
   (patterns-pane (make-pane 'patterns-pane :name 'patterns-pane))
   (pattern-pane (make-pane 'pattern-pane :name 'pattern-pane))
   ;; (input :text-editor)
   ;; (pattern-view :application
   ;;               :scroll-bars t
   ;;               :display-function 'display-pattern)
   (interactor :interactor))
  (:layouts
   (default
    (raising (:border-width 0 :background +black+)
      (vertically ()
        (1/10 (horizontally () ;; toolbar
                (8/10 logo)
                ;; FIX: add server status pane with cpu load, number of active ugens, active synths, active groups, and number of synthdefs
                (1/10 tempo)
                (1/10 scope)))
        (7/10 (horizontally ()
                (1/2 patterns-pane)
                (1/2 pattern-pane))
              ;; (labelling (:label "Options")
              ;;   input)
              )
        (make-pane 'clime:box-adjuster-gadget)
        (2/10 interactor))))
   (test-interactor
    (vertically ()
      interactor)))
  (:pointer-documentation t)
  ;; (:top-level (thundersnow-frame-top-level . nil))
  )

(defmethod enable-frame :after ((frame thundersnow))
  (let ((tempo-pane (find-pane-named frame 'tempo)))
    (clime:schedule-event tempo-pane (make-instance 'timer-event :sheet tempo-pane) 0.1))
  (if-let ((pane (pattern-pane frame)))
    (setf (pane-pattern pane) (slot-value frame 'inspect))
    (sprint 'pane-is-nil)))

(defmethod pattern-pane ((frame standard-application-frame))
  (find-pane-named frame 'pattern-pane))

(defmethod pane-pattern ((frame standard-application-frame))
  (pane-pattern (find-pane-named frame 'pattern-pane)))

(defmethod (setf pane-pattern) (pattern (frame standard-application-frame))
  (when-let ((pane (pattern-pane frame)))
    (setf (pane-pattern pane) pattern))
  (setf (slot-value frame 'inspect) pattern))

;;; commands

(define-command-table thundersnow-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command (com-toggle-metronome :name t :menu t
                                      :command-table thundersnow-file-command-table
                                      :keystroke (#\m :meta))
    ()
  (format t "~a metronome." (if (play-or-stop :-metronome)
                                "Started"
                                "Stopped")))

(define-command-table thundersnow-edit-command-table)

(define-command-table thundersnow-view-command-table)

(define-command-table thundersnow-tools-command-table
  :inherit-from (thundersnow-common-tools-command-table)
  :inherit-menu t)

;; (add-menu-item-to-command-table 'thundersnow-tools-command-table "GUIs" :divider nil)

(define-command (com-test :name t :menu t
                          :command-table thundersnow-tools-command-table)
    ()
  (format t "Hello!~%"))

;; (define-command (com-piano-roll :name t :menu t
;;                                 :command-table thundersnow-tools-command-table)
;;     ()
;;   (piano-roll))

;; (define-command (com-tracker :name t :menu t
;;                              :command-table thundersnow-tools-command-table)
;;     ()
;;   (tracker))

(define-command-table thundersnow-help-command-table
  :inherit-from (thundersnow-common-help-command-table)
  :inherit-menu t)

;; (define-command (com-about :name t :menu t
;;                            :command-table thundersnow-help-command-table)
;;     ()
;;   (thundersnow-about))

;; (define-command (com-readme :name "README" :menu t
;;                             :command-table thundersnow-help-command-table)
;;     ()
;;   (thundersnow-readme))

;; (define-command (com-repo :name t :menu t
;;                           :command-table thundersnow-help-command-table)
;;     ()
;;   (thundersnow-repo))

(define-presentation-action change (pattern nil thundersnow :gesture :select :pointer-documentation "Change pattern")
                            (pattern presentation)
  (sprint 'hi)
  (sprint pattern)
  (sprint presentation)
  (redisplay-frame-pane *application-frame* (find-pane 'patterns-pane) :force-p t)
  nil)

(define-presentation-action play-or-end (pattern nil thundersnow :gesture :select :pointer-documentation "Play or end")
                            (pattern presentation)
  (play-or-end pattern)
  (redisplay-frame-pane *application-frame* (find-pane 'patterns-pane) :force-p t)
  nil)

(define-presentation-to-command-translator change-pattern
    (pattern com-change-pattern thundersnow
     :gesture :select
     :pointer-documentation "Change pattern"
     :menu nil
     :echo t)
    (object presentation)
  (list presentation *unsupplied-argument-marker*))

(define-command (com-change-pattern)
    ((pattern 'presentation :prompt "Old pattern")
     (new-pattern '(or pattern symbol string) :prompt "New pattern"))
  (sprint 'new-com-change-pattern)
  (sprint pattern)
  (setf *tmp* (typecase new-pattern
                (pattern new-pattern)
                (symbol (find-pdef new-pattern t))
                (string (eval new-pattern)))))

(defmethod frame-standard-output ((frame thundersnow))
  (find-pane-named frame 'interactor))

;; (defmethod run-frame-top-level ((frame thundersnow) &key)
;;   (when-let ((tempo-pane (find-pane-named frame 'tempo-box)))
;;     (bt:make-thread (lambda ()
;;                       (update-tempo frame tempo-pane))
;;                     :name "tempo-box update thread"))
;;   (call-next-method))

#+nil
(accepting-values (*query-io* :own-window t)
  (accept '(member 1 2 3 4) :view +radio-box-view+ :stream *query-io*))

;;; main

(defun thundersnow (&rest args &key pattern)
  "Start thundersnow."
  (unless *initialized*
    (thundersnow-initialize))
  (let ((ts (apply #'make-or-find-application-frame 'thundersnow args)))
    (when pattern
      (setf (pane-pattern ts) pattern))
    ts))
