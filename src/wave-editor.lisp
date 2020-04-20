(in-package #:thundersnow)

;;; wave-editor
;; Notes:
;; Prior art:
;; - cl-wav-synth; https://common-lisp.net/project/cl-wav-synth/
;;
;; (defun sound-file (file) ;; only supports 16-bit sounds
;;   (sndfile:with-open-sound-file (sf-file (truename file))
;;     (sndfile::read-short-samples-into-array sf-file)))

;;; gui stuff

(defclass wave-editor-pane (application-pane)
  ((%saved-extent :initform nil))
  (:default-initargs
   :name 'wave-editor
   :display-function 'draw-wave-editor
   :display-time :command-loop
   :default-view +graphical-view+
   :foreground +white+
   :background (get-theme-color :background)))

;; (defmethod compose-space ((pane wave-editor-pane) &key width height)
;;   (make-space-requirement :width 500
;;                           :height 500))

;; (defmethod handle-repaint :before ((pane wave-editor-pane) region)
;;   nil)

(defun draw-wave-editor (frame stream)
  (with-slots (sound) frame
    (unless sound
      (return-from draw-wave-editor nil))
    (with-slots (sound %cached-frames) frame
      (unless %cached-frames
        (setf %cached-frames (bdef-subseq sound 0 (frames sound))))
      (let* ((x-margin 10)
             (y-margin 40)
             (second-px 500)
             (region (sheet-region stream))
             ;; (width (rectangle-width region))
             (height (rectangle-height region))
             (hd2 (- (/ height 2) y-margin))
             (line-color (get-theme-color :foreground))
             ;; (viewport-region (pane-viewport-region stream))
             (frames %cached-frames)
             (num-frames (length frames)) ;; FIX: use a more generic function to get bdef length
             )
        ;; (with-swank-output (print 'hi))
        ;; (setf tmp viewport-region)
        (draw-rectangle* stream x-margin y-margin (+ x-margin num-frames) (- height y-margin) :filled nil :ink (make-rgb-color 1 0 0))
        (let ((end-line-x (+ (* 2 x-margin) num-frames)))
          (draw-line* stream end-line-x 0 end-line-x height))
        (dotimes (i num-frames)
          (let ((val (elt frames i))
                (x (+ x-margin i)))
            (draw-line* stream x hd2 x (+ hd2 (* val height)) :ink line-color)))))))

(define-application-frame wave-editor ()
  ((sound :initarg :sound :initform nil :documentation "The sound instance as loaded by `sound-file'.")
   (%cached-frames :initform nil :documentation "Cached frames from the sound. Frames are cached since getting a buffer's contents from the server may take a long time."))
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
   (wave-editor-pane (make-pane 'wave-editor-pane))
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
      (5/6 (scrolling (:scroll-bar :horizontal)
             wave-editor-pane))
      (1/6 interactor-pane)
      pointer-documentation-pane)))
  (:menu-bar t))

(define-command-table wave-editor-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command-table wave-editor-edit-command-table
  :inherit-from (thundersnow-common-edit-command-table)
  :inherit-menu t)

(define-command-table wave-editor-view-command-table
  :inherit-from (thundersnow-common-view-command-table)
  :inherit-menu t)

(define-command-table wave-editor-tools-command-table
  :inherit-from (thundersnow-common-tools-command-table)
  :inherit-menu t)

(define-command-table wave-editor-help-command-table
  :inherit-from (thundersnow-common-help-command-table)
  :inherit-menu t)

(defun wave-editor (&optional wave)
  "Open a wave-editor. WAVE is the wave to edit; it can either be a bdef, or a filename, in which case the file is loaded as a bdef."
  (let ((sound (etypecase wave
                 (string (bdef wave))
                 (bdef wave)
                 (null nil))))
    (find-application-frame 'wave-editor :sound sound)))

;; cl-wav-synth

#|
(ql:quickload '(clim-listener cl-wav-synth cl-wav-synth-clim))
|#

