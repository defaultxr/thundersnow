(in-package #:thundersnow)

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

(defmethod handle-repaint :before ((pane wave-editor-pane) region)
  (with-slots (%saved-extent) pane
    (setf %saved-extent (pane-viewport-region pane))))

(defun draw-wave-editor (frame stream)
  (with-slots (sndfile) frame
    (unless sndfile
      (return-from draw-wave-editor nil))
    (let* ((region (sheet-region stream))
           (width (rectangle-width region))
           (height (rectangle-height region))
           (hd2 (/ height 2))
           (line-color (get-theme-color :foreground))
           (frames (bdef-range sndfile 0 width)))
      (dotimes (i width)
        (let ((val (elt frames i)))
          (draw-line* stream i hd2 i (+ hd2 (* val height 10)) :ink line-color)))
      ;; (with-room-for-graphics (stream :first-quadrant t)
      
      ;;   (present (make-instance '%background) '%background :stream stream)
      ;;   (loop :for x :from 0 :upto (max (+ (dur eseq) 32) (/ stream-width beat-size))
      ;;         :for xpos = (* x beat-size)
      ;;         :do
      ;;            (draw-line* stream xpos 0 xpos stream-height :ink +gray+)
      ;;            (draw-text* stream (write-to-string x) (1+ xpos) 1 :ink +gray+))
      ;;   (loop :for y :from 0 :upto 127
      ;;         :for ypos = (* y y-size)
      ;;         :do
      ;;            (draw-line* stream 0 ypos stream-width ypos :ink +gray+)
      ;;            (draw-text* stream (write-to-string y) 1 (1+ ypos) :ink +gray+))
      ;;   (dolist (event events)
      ;;     (updating-output (stream :unique-id event :cache-value event :cache-test #'event-presentation-equal)
      ;;       (present event 'event :stream stream))))
      )
    )
  ;; (with-slots (%saved-extent) stream
  ;;   (apply #'scroll-extent stream
  ;;          (if %saved-extent
  ;;              (list (rectangle-min-x %saved-extent) (rectangle-min-y %saved-extent))
  ;;              (list 0 0))))
  )

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
  (let ((sndfile (etypecase wave
                   (string (bdef wave))
                   (bdef wave)
                   (null nil))))
    (find-application-frame 'wave-editor :sndfile sndfile)))

;; cl-wav-synth

#|
(ql:quickload '(clim-listener cl-wav-synth cl-wav-synth-clim))
|#

