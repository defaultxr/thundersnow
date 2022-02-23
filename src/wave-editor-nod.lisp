(in-package #:thundersnow/wave-editor-nod)

;;;; wave-editor-nod.lisp - wave editor implemented in nodgui

;;; utility

(defparameter *tmp* nil)

(defparameter *tmp2* nil)

(defparameter *wave-editor* nil)

(defmacro with-we (&body body)
  `(let* ((we *wave-editor*)
          (*wish* (wave-editor-wish we)))
     ,@body))

;;; wave-editor

(defclass wave-editor ()
  ((bdef :initarg :bdef :accessor wave-editor-bdef)
   (zoom :initarg :zoom :initform 1 :reader wave-editor-zoom :documentation "The current zoom level.")
   (selection-start :initarg :selection-start :initform 0 :type integer :documentation "The start frame of the selection.")
   (selection-end :initarg :selection-end :initform nil :type (or null integer) :documentation "The end frame of the selection.")
   (wish :accessor wave-editor-wish :documentation "The wish stream.")
   (window :accessor wave-editor-window :documentation "The window that the wave-editor is running in.")
   (scrolled-canvas :initarg :scrolled-canvas :accessor wave-editor-scrolled-canvas)
   (canvas :initarg :canvas :initform nil :accessor wave-editor-canvas)
   (status :initarg :status :initform nil :accessor wave-editor-status)
   (cached-buffer-data :initform nil :accessor wave-editor-cached-buffer-data)
   (rendered-canvas-pixels :initform nil :accessor render-projected-canvas-pixels)
   (render-timer :initform nil :accessor render-project-timer)))

(defun cache-buffer-data (wave-editor)
  (with-slots (bdef cached-buffer-data) wave-editor
    (setf cached-buffer-data (bdef-frames bdef :channels 0))
    (let ((*wish* (wave-editor-wish wave-editor)))
      (send-wish-line "event generate . <<data-cached>>"))))

(defun window-x-canvas-x (x &optional (wave-editor *wave-editor*))
  "Translate a window x coordinate to a canvas x coordinate."
  (let ((canvas (wave-editor-canvas wave-editor))
        (scroll (car (scrollbar-get (hscroll (wave-editor-scrolled-canvas wave-editor))))))
    (+ x (* scroll (nodgui::scrollregion-x1 canvas)))))

(defun canvas-x-frame (x &optional (wave-editor *wave-editor*))
  "Translate a canvas x coordinate to a frame number."
  (round (/ x (wave-editor-zoom wave-editor))))

(defun window-x-frame (x &optional (wave-editor *wave-editor*))
  "Translate a window x coordinate to a frame number."
  (canvas-x-frame (window-x-canvas-x x wave-editor) wave-editor))

(defun frame-canvas-x (frame &optional (wave-editor *wave-editor*))
  "Translate a frame number to a canvas x coordinate."
  (* frame (wave-editor-zoom wave-editor)))

(defun update-sel-rect (&optional (wave-editor *wave-editor*))
  (with-slots (canvas) wave-editor
    (set-coords* canvas "sel-rect"
                 (frame-canvas-x (selection-start wave-editor) wave-editor)
                 0
                 (frame-canvas-x (wave-editor-selection-end wave-editor) wave-editor)
                 (window-height canvas)))
  ;; (tag-configure (wave-editor-canvas wave-editor) "sel-rect" :)
  )

(defmethod draw-canvas ((wave-editor wave-editor))
  (with-slots (bdef zoom window canvas cached-buffer-data render-timer) wave-editor
    (clear canvas)
    (after-cancel render-timer)
    (when-let* ((ch (window-height canvas))
                (hh (/ ch 2))
                (canvas-length (ceiling (* zoom (bdef-length bdef)))))
      (scrollregion canvas 0 0 canvas-length ch)
      (let ((sel-rect (create-rectangle canvas
                                        (frame-canvas-x (selection-start wave-editor) wave-editor)
                                        0
                                        (frame-canvas-x (wave-editor-selection-end wave-editor) wave-editor)
                                        ch)))
        (item-configure canvas sel-rect :fill "#ffaaaa")
        (item-configure canvas sel-rect :tag "sel-rect"))
      (setf (render-projected-canvas-pixels wave-editor) (make-array (list canvas-length) :initial-element nil))
      (draw-wave wave-editor))))

(defun draw-wave (wave-editor)
  "Draw one screen worth of canvas pixels, starting from any currently visible. Afterwards, set a timer to continue drawing more pixels outside the screen."
  (format t "dw ")
  (with-slots (bdef zoom window canvas cached-buffer-data rendered-canvas-pixels render-timer) wave-editor
    (let* ((hh (/ (window-height canvas) 2))
           (_ (format t "o "))
           (ww (window-width window))
           (_ (format t "d "))
           (hw (ceiling (/ ww 2)))
           (canvas-length (ceiling (* zoom (bdef-length bdef))))
           (canvas-left (truncate (window-x-canvas-x 0)))
           (canvas-right (truncate (window-x-canvas-x ww)))
           (canvas-center (truncate (/ (+ canvas-left canvas-right) 2)))
           (_ (format t "x "))
           (forward-from (when (< canvas-center canvas-length)
                           (position nil rendered-canvas-pixels :start canvas-center)))
           (backward-from (when (>= canvas-center 0)
                            (position nil rendered-canvas-pixels :from-end t :end (min (1- canvas-center) canvas-length)))))
      (format t "ff: ~a bf: ~a zoom: ~a" forward-from backward-from zoom)
      (flet ((draw-pixel (x)
               (let ((i (canvas-x-frame x wave-editor)))
                 (unless (or (minusp x)
                             (>= x canvas-length)
                             (elt rendered-canvas-pixels x))
                   (let ((rect (create-rectangle canvas 
                                                 x hh
                                                 (+ x (truncate zoom)) (+ hh (* -1 hh (elt cached-buffer-data i))))))
                     (item-configure canvas rect :fill "#000000")
                     (item-configure canvas rect :tag "frame")
                     (setf (elt rendered-canvas-pixels x) t))))))
        (when forward-from
          (dotimes (n hw)
            (draw-pixel (+ forward-from (* n (truncate zoom))))))
        (when backward-from
          (dotimes (n hw)
            (draw-pixel (- backward-from (* n (truncate zoom)))))))
      (when (or forward-from backward-from)
        (setf render-timer (after-idle (lambda ()
                                         (draw-wave wave-editor)))))
      (format t " we end dw~%"))))

(defun status-text (&optional (wave-editor *wave-editor*))
  (when-let* ((bdef (wave-editor-bdef wave-editor))
              (canvas (wave-editor-canvas wave-editor))
              (mouse-x (screen-mouse-x canvas))
              (mouse-frame (window-x-frame mouse-x wave-editor))
              (duration (bdef-duration bdef))
              (length (bdef-length bdef))
              (start (selection-start wave-editor))
              (end (wave-editor-selection-end wave-editor)))
    (format nil "(bdef ~s ~s); D: ~a/~a; F: ~s/~s-~s/~s; ~s metadata; C: ~s; SR: ~s; Zoom: ~s"
            (car (remove (bdef-file bdef) (bdef-names bdef) :test #'string=))
            (or (bdef-metadata bdef :original-file) (bdef-file bdef))
            (friendly-duration-string (* (/ mouse-frame length) duration) :include-ms t)
            (friendly-duration-string duration :include-ms t)
            mouse-frame
            start
            end
            length
            (hash-table-count (bdef-metadata bdef))
            (bdef-channels bdef)
            (bdef-sample-rate bdef)
            (wave-editor-zoom wave-editor))))

(defun update-status-text (wave-editor)
  ;; (format t "ust-1 ")
  (let ((status (wave-editor-status wave-editor))
        ;; (_ (format t "ust-2 "))
        (txt (status-text wave-editor)))
    ;; (format t "ust-3~%")
    (setf (text status) txt)))

(defun selection-start (&optional (wave-editor *wave-editor*))
  (slot-value wave-editor 'selection-start))

(defun (setf selection-start) (frame &optional (wave-editor *wave-editor*))
  (format t "we-setf-ss~%")
  (let ((length (bdef-length (wave-editor-bdef wave-editor))))
    (assert (typep frame (list 'integer 0 length))
            (frame)
            "~s is out of bounds of the buffer (~s frames)."
            frame length)
    (prog1
        (setf (slot-value wave-editor 'selection-start) frame)
      (print 'hi-1)
      (update-sel-rect wave-editor)
      (print 'hi-2)
      ;; (update-status-text wave-editor)
      (print 'hi-3))))

(defun wave-editor-selection-end (&optional (wave-editor *wave-editor*))
  (or (slot-value wave-editor 'selection-end)
      (bdef-length (wave-editor-bdef wave-editor))))

(defun (setf wave-editor-selection-end) (frame &optional (wave-editor *wave-editor*))
  (let ((length (bdef-length (wave-editor-bdef wave-editor))))
    (assert (typep frame (list 'integer 0 length))
            (frame)
            "~s is out of bounds of the buffer (~s frames)."
            frame length)
    (prog1
        (setf (slot-value wave-editor 'selection-end) frame)
      (update-sel-rect wave-editor)
      (update-status-text wave-editor))))

(defun (setf wave-editor-zoom) (new-zoom &optional (wave-editor *wave-editor*))
  (with-slots (bdef zoom window) wave-editor
    (prog1
        (setf zoom new-zoom)
      (draw-canvas wave-editor)
      (update-status-text wave-editor))))

(defun zoom-in (wave-editor)
  (setf (wave-editor-zoom wave-editor) (* 2 (wave-editor-zoom wave-editor))))

(defun zoom-out (wave-editor)
  (setf (wave-editor-zoom wave-editor) (/ (wave-editor-zoom wave-editor) 2)))

(defun play-sound (&optional (wave-editor *wave-editor*) &key loop-p)
  (let* ((bdef (wave-editor-bdef wave-editor))
         (length (bdef-length bdef))
         (start (/ (selection-start wave-editor) length))
         (end (/ (wave-editor-selection-end wave-editor) length)))
    (play (cl-patterns:event :instrument :sp :buffer bdef :start start :end end :quant 0 :latency 0.01 :beat 1/10))))

(defun save-project (&optional (wave-editor *wave-editor*))
  (format t "Save pressed (but not done yet).~%"))

(defun save-project-as (&optional (wave-editor *wave-editor*))
  (format t "Save As pressed (but not done yet).~%")
  (format t "Selected ~s.~%" (get-save-file)))

(defun load-object (&optional (wave-editor *wave-editor*))
  (format t "Load pressed (but not done yet).~%")
  (format t "Selected ~s.~%" (get-open-file)))

(defun render-project (&optional (wave-editor *wave-editor*))
  (format t "Render pressed (but not done yet).~%")
  (format t "Selected ~s.~%" (get-save-file)))

(defun render-project-selection (&optional (wave-editor *wave-editor*))
  (format t "Render Selection pressed (but not done yet).~%")
  (format t "Selected ~s.~%" (get-save-file)))

(defun exit-wave-editor (&optional (wave-editor *wave-editor*))
  (case (message-box "Save changes before exiting?" "Save?" :yesnocancel :warning)
    (:yes (do-msg "Saving changes is not yet implemented :("))
    (:no (do-msg "Not saving changes!"))
    (:cancel (do-msg "Canceling."))))

(defun select-all (&optional (wave-editor *wave-editor*))
  (setf (selection-start wave-editor) 0
        (wave-editor-selection-end wave-editor) (bdef-length (wave-editor-bdef wave-editor))))

(defun click (x &optional (wave-editor *wave-editor*))
  (format t "we-click~%")
  (setf (selection-start wave-editor) (window-x-frame x)))

(defmethod make-window ((wave-editor wave-editor))
  (with-slots (bdef window (thundersnow/wave-editor-nod::scrolled-canvas scrolled-canvas) canvas status) wave-editor
    (with-nodgui (:title "wave-editor" :debug 0)
      (let* ((frame (make-instance 'frame :pack (list :fill :both :expand t)))
             (mb (make-menubar))
             (mfile (make-menu mb "File" :underline 0))
             (mf-play (make-menubutton mfile "Play" 'play-sound :underline 0 :accelerator "SPC"))
             (mf-save (make-menubutton mfile "Save" 'save-project :underline 0 :accelerator "C-s"))
             (mf-save-as (make-menubutton mfile "Save As..." 'save-project-as :underline 1 :accelerator "C-S"))
             (mf-load (make-menubutton mfile "Load..." 'load-object :underline 0 :accelerator "C-o"))
             (mf-sep1 (add-separator mfile))
             (mf-render (make-menubutton mfile "Render..." 'render-project :underline 0 :accelerator "C-M-r"))
             (mf-render-selection (make-menubutton mfile "Render Selection..." 'render-project-selection :underline 2 :accelerator "C-M-R"))
             (mf-sep2 (add-separator mfile))
             (mf-exit (make-menubutton mfile "Exit..." 'exit-wave-editor :underline 0 :accelerator "C-q"))
             (medit (make-menu mb "Edit" :underline 0))
             (me-select-all (make-menubutton medit "Select All" 'select-all :underline 0 :accelerator "C-A"))
             (mview (make-menu mb "View" :underline 0))
             (mv-zoom-in (make-menubutton mview "Zoom In" (fn (zoom-in wave-editor)) :accelerator "C-="))
             (mv-zoom-out (make-menubutton mview "Zoom Out" (fn (zoom-out wave-editor)) :accelerator "C--"))
             (mtools (make-menu mb "Tools" :underline 0))
             (mhelp (make-menu mb "Help" :underline 0))
             (mh-about (make-menubutton mhelp "About..." 'wave-editor-about :underline 0))
             (sc (make-instance 'scrolled-canvas :master frame :pack (list :fill :both :expand t)))
             (c (canvas sc))
             (st (make-instance 'label :text (status-text wave-editor) :pack (list :fill :x))))
        (declare (ignore mf-play mf-save mf-save-as mf-load mf-sep1 mf-render mf-render-selection mf-sep2 mf-exit
                         me-select-all
                         mv-zoom-in mv-zoom-out
                         mtools
                         mh-about))
        (setf (wave-editor-wish wave-editor) *wish*
              window *tk*
              scrolled-canvas sc
              canvas c
              status st)
        (configure c :cursor :cross)
        (configure c :background "#aaffaa")
        (create-text c 0 0 "Loading...")
        (bind *tk* "<space>" (fn _ (play-sound wave-editor)))
        (bind *tk* "<Control-A>" (fn _ (select-all wave-editor)))
        (bind *tk* "<Control-equal>" (fn _ (zoom-in wave-editor)))
        (bind *tk* "<Control-minus>" (fn _ (zoom-out wave-editor)))
        (bind *tk* "<Control-q>" (fn _ (exit-wave-editor wave-editor)))
        (bind *tk* "<<data-cached>>" (fn _ (draw-canvas wave-editor)))
        (bind c "<ButtonPress-1>" (fn (click (event-x _) wave-editor)))
        (bind c "<Motion>" (fn _ (update-status-text wave-editor)))
        ;; (bind *tk* "<Configure>" (fn (setf *tmp* _)))
        ;; (bind *tk* "<<Scroll>>" (fn (print 'scroll) (setf *tmp* _)))
        (bt:make-thread (lambda () (cache-buffer-data wave-editor)))))))

(defun wave-editor (&optional bdef &key)
  (setf *wave-editor* (make-instance 'wave-editor :bdef (bdef bdef)))
  (make-window *wave-editor*)
  *wave-editor*)

(defun launch-wave-editor (&rest args)
  (bt:make-thread (lambda ()
                    (apply #'wave-editor args))
                  :name "wave-editor (nod)"))

;; (bdef :chirp "~/chirp.wav")

;; (bdef :ot-drumloop "/home/modula/ot-drumloop.wav")

;; (bdef :hahn-warn-beep "/home/modula/lp-hahn-12-warn-beep.wav")
