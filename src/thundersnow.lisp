;;;; thundersnow.lisp
;;; this file mostly contains the code for the main thundersnow gui; common gui functionality is in common.lisp
;; NOTES:
;; - https://common-lisp.net/project/mcclim/static/manual/mcclim.html
;; - https://github.com/McCLIM/McCLIM/wiki/Default-Keyboard-and-Mouse-Gestures
;; (setf (frame-current-layout (thundersnow)) 'default)
;; (ql:quickload :clim-listener)
;; (clim-listener:run-listener)
;; arguments for define-drag-and-drop-translator can include:
;; - destination-object context-type frame presentation destination-presentation event window x y
;; zoom should be proportional; zoom series is: 1, 2, 3, 4, 6, 8, 12, 16, 24, 32

(in-package #:thundersnow/thundersnow)

;;; tempo pane

(defclass tempo-pane (basic-gadget)
  ())

(defmethod handle-event ((pane tempo-pane) (event timer-event))
  (let* ((rect (bounding-rectangle (sheet-region pane)))
         (width (rectangle-width rect))
         (height (rectangle-height rect))
         (clock cl-patterns:*clock*))
    (draw-rectangle* pane 0 0 width height
                     :filled t
                     :ink (if clock
                              (let ((c (expt (- 1 (mod (- (beat *clock*) (time-dur cl-patterns::*latency*)) 1.0)) 3)))
                                (make-rgb-color (* c 0.5) (+ 0.5 (* 0.5 c)) (* c 0.5)))
                              (make-gray-color 0.5)))
    (draw-text* pane
                (if clock
                    (format nil "BPM: ~f" (* 60 (cl-patterns:tempo clock)))
                    (format nil "null *clock*~%(click to create)"))
                (/ width 2) (/ height 2)
                :align-x :center
                :align-y :center))
  (clim-internals::schedule-timer-event pane 'tick 0.01))

(defmethod handle-event ((pane tempo-pane) (event climi::pointer-button-press-event))
  (if *clock*
      (com-set-tempo)
      (start-clock-loop :tempo 110/60)))

(defmethod handle-event ((pane tempo-pane) (event climi::pointer-scroll-event))
  (when *clock*
    (incf (tempo *clock*) (* 1/600 (slot-value event 'climi::delta-y)))))

;;; scope pane

(defparameter *scope-wave* (make-array 200 :element-type 'double-float :initial-element 0d0))

(defclass scope-pane (basic-gadget)
  ())

(defmethod handle-event ((pane scope-pane) (event timer-event))
  (let* ((rect (bounding-rectangle (sheet-region pane)))
         (width (rectangle-width rect))
         (height (rectangle-height rect))
         (hd2 (/ height 2)))
    (draw-rectangle* pane 0 0 width height :filled t :ink (make-gray-color 0.2))
    (draw-line* pane 0 hd2 width hd2 :ink +white+))
  (climi::schedule-timer-event pane 'tick 0.01))

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

;; (define-presentation-method present ((pattern pattern) (type pattern) stream (view textual-view) &key)
;;   (with-swank-output
;;     (print 'gen-pat-present))
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
  ()
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
   (scope (make-pane 'scope-pane :name 'scope-pane))
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
        (7/10 pattern-pane
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
  (clim-internals::schedule-timer-event
   (find-pane-named frame 'tempo) 'tick 0.1)
  (clim-internals::schedule-timer-event
   (find-pane-named frame 'scope) 'tick 0.1))

(defmethod pane-pattern ((frame standard-application-frame))
  (with-swank-output
    (print 'pane-pattern)
    (print frame))
  (pane-pattern (find-pane-named frame 'pattern-pane)))

(defmethod (setf pane-pattern) (pattern (frame standard-application-frame))
  (setf (pane-pattern (find-pane-named frame 'pattern-pane)) pattern))

;;; commands

(define-command-table thundersnow-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command (com-set-tempo :name t :menu t
                               :command-table thundersnow-file-command-table)
    ((tempo 'string :default (write-to-string (tempo *clock*)) :prompt "Tempo (default unit: bps)"))
  (destructuring-bind (value &optional (unit "bps")) (split-string tempo)
    (let ((value (read-from-string value)))
      (assert (numberp value) (value) "The provided tempo must be a number; got ~s instead. If you want to specify a unit it must come after the number." value)
      (let ((unit (make-keyword (string-upcase unit))))
        (assert (member unit (list :bps :bpm)) (unit) "UNIT must be either bps or bpm; got ~s instead." unit)
        (when (and (eql unit :bps)
                   (>= value 10))
          (restart-case
              (error "The provided beats per second value was abnormally high (~s beats per second = ~s beats per minute)!~%Are you sure you want to proceed?" value (* value 60))
            (continue ()
              :report "Use this value for the beats per second anyway."
              nil)
            (use-as-bpm ()
              :report "Use this value as beats per minute instead."
              (setf unit :bpm))
            ;; FIX these:
            ;; (specify-another-tempo ()
            ;;   :report "Specify another value for the beats per second instead."
            ;;   (com-set-tempo)
            ;;   (abort))
            ;; (abort ()
            ;;   :report (lambda ()
            ;;             (format nil "Cancel changing the tempo, leaving it at ~s (~s bpm)." (tempo *clock*) (* (tempo *clock*) 60)))
            ;;   (invoke-restart 'abort))
            ))
        (setf (tempo *clock*) (if (eql unit :bps)
                                  value
                                  (/ value 60)))))))

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

(define-thundersnow-command (com-change-pattern :name t)
    ((pattern 'pattern :gesture :select))
  (with-swank-output
    (print 'changing-pattern))
  (let ((frame-input (frame-standard-input *application-frame*)))
    (setf tmp pattern)
    ;; (with-swank-output
    ;;   (print
    ;;    (accepting-values (frame-input)
    ;;      (accept 'string :stream frame-input
    ;;              :view +cell-unparsed-text-view+
    ;;              :default (string cell)))))
    )
  nil)

(defmethod frame-standard-output ((frame thundersnow))
  (find-pane-named frame 'interactor))

;; (defmethod run-frame-top-level ((frame thundersnow) &key)
;;   (when-let ((tempo-pane (find-pane-named frame 'tempo-box)))
;;     (bt:make-thread (lambda ()
;;                       (update-tempo frame tempo-pane))
;;                     :name "tempo-box update thread"))
;;   (call-next-method))

(when nil
  (accepting-values (*query-io* :own-window t)
    (accept '(member 1 2 3 4) :view +radio-box-view+ :stream *query-io*)))

;;; main

(defun thundersnow (&rest args)
  "Start thundersnow."
  (unless *initialized*
    (thundersnow-initialize))
  (apply 'find-application-frame 'thundersnow args))
