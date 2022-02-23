(in-package #:thundersnow/stepseq)

(defclass stepseq-pane (application-pane)
  ((pattern :initarg :pattern :initform nil :accessor pane-pattern))
  (:default-initargs
   :name 'stepseq
   :display-function 'display-stepseq))

(defun display-stepseq (frame stream)
  nil)

(define-command-table stepseq-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command-table stepseq-edit-command-table
  :inherit-from (thundersnow-common-edit-command-table)
  :inherit-menu t)

(define-command-table stepseq-view-command-table
  :inherit-from (thundersnow-common-view-command-table)
  :inherit-menu t)

(define-command-table stepseq-tools-command-table
  :inherit-from (thundersnow-common-tools-command-table)
  :inherit-menu t)

(define-command-table stepseq-help-command-table
  :inherit-from (thundersnow-common-help-command-table)
  :inherit-menu t)

(define-application-frame stepseq ()
  ()
  (:command-table (stepseq
                   :inherit-from (stepseq-file-command-table
                                  stepseq-edit-command-table
                                  stepseq-view-command-table
                                  stepseq-tools-command-table
                                  stepseq-help-command-table)
                   :menu (("File" :menu stepseq-file-command-table)
                          ("Edit" :menu stepseq-edit-command-table)
                          ("View" :menu stepseq-view-command-table)
                          ("Tools" :menu stepseq-tools-command-table)
                          ("Help" :menu stepseq-help-command-table))))
  (:default-initargs
   :pretty-name "stepseq")
  (:panes
   ;; (raised (raising (:border-width 3 :background +Gray83+)
   ;;           (make-pane 'check-box :choices '("First" "Second" "Third"))))
   (logo :application
         :scroll-bars nil
         ;; :display-function
         )
   (tempo (make-pane 'tempo-pane :name 'tempo-pane))
   (scope (make-pane 'scope-pane :name 'scope-pane))
   (stepseq-pane (make-pane 'stepseq-pane :name 'stepseq-pane))
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
        (7/10 stepseq-pane
              ;; (labelling (:label "Options")
              ;;   input)
              )
        (make-pane 'clime:box-adjuster-gadget)
        (2/10 interactor))))
   (test-interactor
    (vertically ()
      interactor)))
  (:pointer-documentation t)
  ;; (:top-level (stepseq-frame-top-level . nil))
  )

(defun stepseq ()
  "Open a stepseq or get one of the instances already open.

See also: `stepseq-pane'"
  (make-or-find-application-frame 'stepseq))

