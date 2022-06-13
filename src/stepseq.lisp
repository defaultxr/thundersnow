(in-package #:thundersnow/stepseq)

(defvar *tmp* nil
  "Temporary variable for convenience during development.")

(defclass stepseq-pane (application-pane)
  ((pattern :initarg :pattern :initform nil :accessor pane-pattern))
  (:default-initargs :name 'stepseq
                     :display-function 'display-stepseq))

(defun display-stepseq (frame stream)
  (declare (ignore frame))
  (with-slots (pattern) stream
    (unless pattern
      (draw-text stream "nil" (bounding-rectangle-center stream))
      (return-from display-stepseq))))

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
                   :inherit-from (thundersnow-common-command-table
                                  stepseq-file-command-table
                                  stepseq-edit-command-table
                                  stepseq-view-command-table
                                  stepseq-tools-command-table
                                  stepseq-help-command-table)
                   :menu (("File" :menu stepseq-file-command-table)
                          ("Edit" :menu stepseq-edit-command-table)
                          ("View" :menu stepseq-view-command-table)
                          ("Tools" :menu stepseq-tools-command-table)
                          ("Help" :menu stepseq-help-command-table))))
  (:default-initargs :pretty-name "Stepseq")
  (:panes
   (stepseq-pane (make-pane 'stepseq-pane :name 'stepseq-pane))
   (interactor :interactor))
  (:layouts
   (default
    (raising (:border-width 0 :background +black+)
      (vertically ()
        (4/5 stepseq-pane)
        (make-pane 'clime:box-adjuster-gadget)
        (1/5 interactor))))
   (test-interactor
    (vertically ()
      interactor)))
  (:pointer-documentation t))

(defmethod initialize-instance :before ((stepseq stepseq) &key &allow-other-keys)
  (thundersnow-ensure-initialized))

(defun stepseq ()
  "Open a stepseq or get one of the instances already open.

See also: `stepseq-pane'"
  (make-or-find-application-frame 'stepseq))

