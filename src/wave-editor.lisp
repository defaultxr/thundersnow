(in-package #:thundersnow/wave-editor)

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
  ((sound :initarg :sound :initform nil :type (or null bdef) :documentation "The sound instance as a `bdef'.")
   (second-px :initarg :second-px :initform 1000 :type number :documentation "The number of horizontal pixels per second (i.e. the \"zoom\" level of the pane).")
   (%cached-frames :initform nil :documentation "Cached frames from the sound. Frames are cached since getting a buffer's contents from the server may take a long time.")
   (%cached-second-px :initform nil :documentation "The value of second-px when the %cached-scaled-frames were calculated.")
   (%cached-scaled-frames :initform nil :documentation "The calculated frames generated for the current zoom level.")
   (%saved-extent :initform nil) ;; FIX: implement?
   )
  (:default-initargs
   :name 'wave-editor
   :display-function 'draw-wave-editor
   :display-time :command-loop
   ;; :default-view +graphical-view+
   :foreground (get-theme-color :foreground)
   :background (get-theme-color :background)))

;; (defmethod compose-space ((pane wave-editor-pane) &key width height)
;;   (make-space-requirement :width 500
;;                           :height 500))

;; (defmethod handle-repaint :before ((pane wave-editor-pane) region)
;;   nil)

(defmethod sound ((this wave-editor-pane))
  (slot-value this 'sound))

(defmethod (setf sound) (sound (this wave-editor-pane))
  (setf (slot-value this 'sound) sound))

(defmethod (setf sound) ((sound string) (this wave-editor-pane))
  (setf (sound this) (bdef sound)))

(defun cached-frames-for (stream)
  "Get and cache sound frame data for STREAM.

See also: `scaled-frames-for'"
  (with-slots (sound %cached-frames) stream
    (or %cached-frames
        (let ((frames (bdef-subseq sound 0 (bdef-length sound))))
          (setf %cached-frames frames)
          frames))))

(defun scaled-frames-for (stream)
  "Generate and store scaled frames for STREAM.

See also: `cached-frames-for'"
  ;; FIX(?): this function doesn't yet handle second-px being greater than or equal to buffer's sample rate
  (with-slots (sound second-px %cached-second-px %cached-scaled-frames) stream
    (when (eql second-px %cached-second-px)
      (return-from scaled-frames-for %cached-scaled-frames))
    (let* ((cached-frames (cached-frames-for stream))
           (sound-duration (bdef-duration sound))
           (array-length (ceiling (* sound-duration second-px)))
           (array (make-array array-length))
           (sound-length (bdef-length sound))
           (frames-per-scaled-frame (floor (/ sound-length array-length))))
      (dotimes (n array-length)
        (setf (aref array n)
              (mean (subseq cached-frames (* n frames-per-scaled-frame)
                            (* (1+ n) frames-per-scaled-frame)))))
      (setf %cached-scaled-frames array
            %cached-second-px second-px)
      array)))

(defun draw-wave-editor (frame stream)
  (declare (ignore frame))
  (with-slots (sound) stream
    (unless sound
      (return-from draw-wave-editor nil))
    (let* ((x-margin 10)
           (y-margin 40)
           (region (sheet-region stream))
           ;; (width (rectangle-width region))
           (height (rectangle-height region))
           (hd2 (- (/ height 2) y-margin))
           (line-color (get-theme-color :foreground))
           ;; (viewport-region (pane-viewport-region stream))
           (frames (scaled-frames-for stream))
           (num-frames (length frames)))
      (draw-rectangle* stream x-margin y-margin (+ x-margin num-frames) (- height y-margin) :filled nil :ink (get-theme-color :grid))
      (let ((end-line-x (+ (* 2 x-margin) num-frames)))
        (draw-line* stream end-line-x 0 end-line-x height))
      (dotimes (i num-frames)
        (let ((val (aref frames i))
              (x (+ x-margin i)))
          (draw-line* stream x hd2 x (+ hd2 (* val height)) :ink line-color))))))

(define-application-frame wave-editor ()
  ((second-px :initarg :second-px :initform 1000))
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

(defmethod frame-standard-output ((frame wave-editor))
  (find-pane-named frame 'interactor))

(defmethod second-px ((this wave-editor))
  (slot-value this 'second-px))

(defmethod (setf second-px) (value (this wave-editor))
  (let ((pane (find-pane-named this 'wave-editor-pane)))
    (setf (slot-value this 'second-px) value
          (slot-value pane 'second-px) value
          (pane-needs-redisplay pane) t)))

(defmethod sound ((this wave-editor))
  (sound (find-pane-named this 'wave-editor-pane)))

(defmethod (setf sound) (sound (this wave-editor))
  (setf (sound (find-pane-named this 'wave-editor-pane)) sound))

(define-command-table wave-editor-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command (com-open-file :name t :menu t
                               :command-table wave-editor-file-command-table
                               :keystroke (#\o :control))
    ((file 'string :prompt "File"))
  (setf (sound *application-frame*) file))

(define-command-table wave-editor-edit-command-table
  :inherit-from (thundersnow-common-edit-command-table)
  :inherit-menu t)

(define-command (com-play :name t :menu t
                          :command-table wave-editor-edit-command-table
                          ;; :keystroke (#\ :) ;; FIX
                          )
    ()
  (play (sound *application-frame*)))

(define-command-table wave-editor-view-command-table
  :inherit-from (thundersnow-common-view-command-table)
  :inherit-menu t)

(define-command (com-zoom-in :name t :menu t
                             :command-table wave-editor-view-command-table
                             :keystroke (#\= :control))
    ()
  (setf (second-px *application-frame*) (* 2 (second-px *application-frame*))))

(define-command (com-zoom-out :name t :menu t
                              :command-table wave-editor-view-command-table
                              :keystroke (#\- :control))
    ()
  (setf (second-px *application-frame*) (* 0.5 (second-px *application-frame*))))

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
    ;; FIX: find-application-frame sometimes returns nil, which makes the `wave-editor' function's WAVE argument not work. maybe a mcclim bug?
    (when-let* ((frame (find-application-frame 'wave-editor))
                (pane (find-pane-named frame 'wave-editor-pane)))
      (setf (sound pane) sound))
    frame))


;; cl-wav-synth

#|
(ql:quickload '(clim-listener cl-wav-synth cl-wav-synth-clim))
|#
