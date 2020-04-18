(in-package #:thundersnow)

(defun sound-file (file) ;; only supports 16-bit sounds
  (sndfile:with-open-sound-file (sf-file (truename file))
    (sndfile::read-short-samples-into-array sf-file)))

;;; gui stuff

(define-application-frame wave-editor ()
  ((sndfile :initarg :sndfile :initform nil :documentation "The sndfile instance as loaded by `sound-file'."))
  (:command-table (wave-editor
		   :inherit-from (wave-editor-file-command-table
                                  wave-editor-edit-command-table
                                  wave-editor-view-command-table
                                  wave-editor-tools-command-table
                                  wave-editor-help-command-table)
		   :menu (("File" :menu wave-editor-file-command-table)
                          ("Edit" :menu wave-editor-edit-command-table)
                          ("View" :menu wave-editor-view-command-table)
                          ("Tools" :menu wave-editor-tools-command-table)
			  ("Help" :menu wave-editor-help-command-table))))
  (:panes
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
             (make-pane 'wave-editor-pane)))
      (1/6 interactor-pane)
      pointer-documentation-pane)))
  (:menu-bar t))

(defun wave-editor (&optional file-or-buffer)
  "Open a wave-editor."
  (find-application-frame 'wave-editor))

;; cl-wav-synth

#|
(ql:quickload '(clim-listener cl-wav-synth cl-wav-synth-clim))
|#

