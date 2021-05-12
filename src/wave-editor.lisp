(in-package #:thundersnow/wave-editor)

;;; wave-editor
;; Notes:
;; Prior art:
;; - cl-wav-synth; https://common-lisp.net/project/cl-wav-synth/
;; https://github.com/McCLIM/McCLIM/issues/927 - "proposal: display function performs no actual output"; possible performance gain for this, as it would avoid drawing items that are not on screen

(defvar tmp nil)

;;; gui stuff

(defclass wave-editor-pane (application-pane)
  ((sound :initarg :sound :initform nil :type (or null bdef) :documentation "The sound instance as a `bdef'.")
   (point :initarg :point :initform 0 :type (or integer list) :documentation "The frame that the point is to the left of, or a list consisting of the start and end points of the region if active.")
   (second-px :initarg :second-px :initform 1000 :type number :documentation "The number of horizontal pixels per second (i.e. the \"zoom\" level of the pane).")
   (horizontal-margin :initarg :horizontal-margin :initform 10 :type (real 0) :documentation "The margin between the left/right edges of the pane and the start/end of the waveform.")
   (vertical-margin :initarg :y-margin :initform 40 :type (real 0) :documentation "The margin between the top/bottom of the pane and 1 and -1 of the waveform.")
   (%cached-frames :initform nil :documentation "Cached frames from the sound. Frames are cached since getting a buffer's contents from the server may take a long time.")
   (%saved-extent :initform nil :documentation "The scroll position to return to when redisplaying."))
  (:default-initargs
   :name 'wave-editor
   :display-function 'draw-wave-editor
   :display-time :command-loop
   ;; :default-view +graphical-view+
   :foreground (get-theme-color :foreground)
   :background (get-theme-color :background)))

(defmethod handle-event :after ((stream wave-editor-pane) (event climi::pointer-scroll-event))
  (with-slots (%saved-extent) stream
    (let* ((new-region (pane-viewport-region stream))
           (should-redraw (or (/= (rectangle-min-x new-region) (rectangle-min-x %saved-extent))
                              (/= (rectangle-max-x new-region) (rectangle-max-x %saved-extent)))))
      (setf %saved-extent new-region)
      (when should-redraw
        (redisplay-frame-pane (pane-frame stream) stream :force-p t)))))

(define-presentation-type bdef ())

(define-presentation-type wave-editor-point ())

(define-presentation-type sound-frame ())

(defmethod handle-repaint :before ((stream wave-editor-pane) region)
  ;; save the scroll position
  (with-slots (%saved-extent) stream
    (setf %saved-extent (pane-viewport-region stream))))

(defmethod sound ((this wave-editor-pane))
  (slot-value this 'sound))

(defmethod (setf sound) ((bdef bdef) (this wave-editor-pane))
  (setf (slot-value this 'sound) bdef
        (pane-needs-redisplay this) t)
  (redisplay-frame-pane (pane-frame this) this :force-p t))

(defmethod (setf sound) ((sound string) (this wave-editor-pane))
  (setf (sound this) (bdef sound)))

(defmethod (setf sound) ((sound symbol) (this wave-editor-pane))
  (setf (sound this) (bdef sound)))

(defmethod (setf sound) ((sound pathname) (this wave-editor-pane))
  (setf (sound this) (namestring sound)))

(defun cached-frames-for (stream)
  "Get and cache sound frame data for STREAM.

See also: `scaled-frames-for'"
  (etypecase stream
    (wave-editor
     (cached-frames-for (find-pane-named stream 'wave-editor-pane)))
    (wave-editor-pane
     (with-slots (sound %cached-frames) stream
       (or %cached-frames
           (let ((frames (bdef-frames sound)))
             (setf %cached-frames frames)
             frames))))))

(defun sound-frame-pixel (wave-editor-pane sound-frame)
  "Get the pixel position of a WAVE-EDITOR-PANE's sound frame.

See also: `pixel-sound-frame'"
  (with-slots (sound second-px horizontal-margin) wave-editor-pane
    (+ horizontal-margin (* (/ second-px (bdef-sample-rate sound)) sound-frame))))

(defun pixel-sound-frame (wave-editor-pane pixel)
  "Get the sound frame at PIXEL.

See also: `sound-frame-pixel'"
  (with-slots (sound second-px horizontal-margin) wave-editor-pane
    (* (/ (- pixel horizontal-margin) second-px) (bdef-sample-rate sound))))

(defun draw-wave-editor (frame stream) ;; visible frames only (fastest)
  (declare (ignore frame))
  (with-slots (sound point second-px horizontal-margin vertical-margin %saved-extent) stream
    (unless sound
      (return-from draw-wave-editor nil))
    (let* ((region (sheet-region stream))
           (height (rectangle-height region))
           (waveform-height (/ (- height (* 2 vertical-margin)) 2))
           (center-y (/ height 2))
           (line-color (get-theme-color :foreground))
           (frames (cached-frames-for stream))
           (num-frames (length frames))
           (visible-region %saved-extent)
           (left-frame (max 0 (truncate (pixel-sound-frame stream (rectangle-min-x visible-region)))))
           (right-frame (max 0 (truncate (pixel-sound-frame stream (rectangle-max-x visible-region)))))
           (point (ensure-list point))
           (point-start (car point))
           (point-end (or (cadr point) (car point)))
           (point-start-x (sound-frame-pixel stream point-start))
           (point-end-x (sound-frame-pixel stream point-end)))
      ;; selection rectangle background
      (draw-rectangle* stream
                       point-start-x vertical-margin
                       point-end-x (- height vertical-margin)
                       :ink +yellow-green+)
      ;; line drawn at the end for the right margin
      (let ((end-line-x (+ (* 2 horizontal-margin) (sound-frame-pixel stream num-frames))))
        (draw-line* stream end-line-x 0 end-line-x height))
      (let ((sfm (/ second-px (bdef-sample-rate sound))))
        ;; zero line
        (draw-line* stream horizontal-margin center-y (* sfm (1- num-frames)) center-y :ink +red+)
        ;; frames
        (if (>= sfm 1)
            (loop :for i :from left-frame :below right-frame
                  :do (draw-line* stream
                                  (+ horizontal-margin (* sfm i))
                                  (- center-y (* (aref frames i) waveform-height))
                                  (+ horizontal-margin (* sfm (1+ i)))
                                  (- center-y (* (aref frames (1+ i)) waveform-height))
                                  :ink line-color))
            (let ((sfmr (/ sfm))
                  (min-x (rectangle-min-x visible-region))
                  (max-x (rectangle-max-x visible-region)))
              (loop :for x :from min-x :below max-x :do
                (let ((sf (pixel-sound-frame stream x))
                      (x-px (+ horizontal-margin x)))
                  (unless (or (minusp sf) (>= sf num-frames))
                    (let (min max)
                      (dotimes (j (truncate sfmr))
                        (let* ((idx (truncate (+ j sf)))
                               (val (unless (>= idx num-frames)
                                      (aref frames idx))))
                          (when val
                            (setf max (if max (max max val) val)
                                  min (if min (min min val) val)))))
                      (draw-line* stream
                                  x-px (- center-y (* waveform-height max))
                                  x-px (- center-y (* waveform-height min))
                                  :ink line-color))))))))
      ;; border rectangle
      (draw-rectangle* stream
                       horizontal-margin
                       vertical-margin
                       (sound-frame-pixel stream num-frames)
                       (- height vertical-margin)
                       :filled nil
                       :ink (get-theme-color :grid))
      ;; selection box border
      (draw-rectangle* stream
                       point-start-x vertical-margin
                       point-end-x (- height vertical-margin)
                       :filled nil
                       :ink +yellow+))
    (apply #'scroll-extent stream
           (if %saved-extent
               (list (rectangle-min-x %saved-extent) (rectangle-min-y %saved-extent))
               (list 0 0)))))

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

(define-presentation-action select (sound-frame nil wave-editor
                                    :gesture :select
                                    :pointer-documentation
                                    ((sound-frame stream)
                                     (format stream "Select frame ~s" sound-frame)))
                            (sound-frame stream)
  (print sound-frame *debug-io*)
  (setf (slot-value stream 'point) sound-frame))

(defmethod frame-standard-output ((frame wave-editor))
  (find-pane-named frame 'interactor))

(defmethod second-px ((this wave-editor))
  (slot-value this 'second-px))

(defmethod (setf second-px) (value (this wave-editor))
  (let* ((stream (find-pane-named this 'wave-editor-pane))
         (max-second-px (if-let ((bdef (sound stream)))
                          (* 16 (bdef-sample-rate bdef))
                          192000))
         (value (clamp value 1 max-second-px)))
    (setf (slot-value this 'second-px) value
          (slot-value stream 'second-px) value
          (pane-needs-redisplay stream) t)
    (with-slots (horizontal-margin) stream
      (change-space-requirements stream
                                 :min-width (+ (* 2 horizontal-margin)
                                               (* value (bdef-duration (sound stream))))))))

(defmethod sound ((this wave-editor))
  (sound (find-pane-named this 'wave-editor-pane)))

(defmethod (setf sound) ((bdef bdef) (this wave-editor))
  (setf (sound (find-pane-named this 'wave-editor-pane)) bdef)
  (let* ((bdef (sound this))
         (key (bdef-key bdef))
         (name (etypecase key
                 (symbol key)
                 (string (concat (pathname-name key) "." (pathname-type key))))))
    (setf (frame-pretty-name *application-frame*) (concat "Wave-Editor: " name))))

(defmethod (setf sound) ((sound string) (this wave-editor))
  (setf (sound this) (bdef sound)))

(defmethod (setf sound) ((sound symbol) (this wave-editor))
  (setf (sound this) (bdef sound)))

(defmethod (setf sound) ((sound pathname) (this wave-editor))
  (setf (sound this) (namestring sound)))

(define-command-table wave-editor-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command (com-open-file :name t :menu t
                               :command-table wave-editor-file-command-table
                               :keystroke (#\o :control))
    ((file '(or pathname bdef) :prompt "File"))
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

(define-command (com-zoom-to-fit :name t :menu t
                                 :command-table wave-editor-view-command-table
                                 ;; :keystroke (#\- :control)
                                 )
    ()
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
                 (null nil)))
        (frame (find-application-frame 'wave-editor)))
    ;; FIX: find-application-frame sometimes returns nil, which makes the `wave-editor' function's WAVE argument not work. maybe a mcclim bug?
    (when (and wave frame)
      (when-let ((stream (find-pane-named frame 'wave-editor-pane)))
        (setf (sound stream) sound)))
    frame))

;;; misc / testing

(defun wave-editor-pane (&optional frame)
  (find-pane-named (or frame (wave-editor)) 'wave-editor-pane))

(defun test-wave-editor (&optional wave)
  (let ((frame (find-application-frame 'wave-editor)))
    (setf (sound frame) (bdef :cat32 "~/num.wav"))))

(define-command (com-test :name t :menu t
                          :command-table wave-editor-file-command-table)
    ()
  (when-let ((bdefs (all-bdefs)))
    (present (bdef (random-elt bdefs)) 'bdef)))
