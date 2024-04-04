;;;; keyboard-gui.lisp - a simple interface to trigger synths from your keyboard or the gui.

(in-package #:thundersnow/keyboard-gui)

(defclass keyboard-gui-pane (application-pane)
  ()
  (:default-initargs
   :name 'keyboard-gui
   :display-function 'draw-keyboard-gui
   :display-time :command-loop
   :default-view +graphical-view+
   :foreground +white+
   :background (theme-color :background)))

(defun draw-keyboard-gui (frame stream)
  (let* ((stream-width (pane-real-width stream))
         (stream-height (pane-real-height stream))
         (grid-size (slot-value frame 'grid-size))
         (beat-size (slot-value frame 'beat-size))
         (y-size (slot-value frame 'y-size))
         (eseq (slot-value frame 'eseq))
         (events (etypecase eseq
                   (list eseq)
                   (eseq (eseq-events eseq))
                   (pattern (next-upto-n eseq))))
         (background-color (slot-value stream 'background))
         (grid-color (theme-color :grid))
         (bg-grid-mixed (mix-colors background-color grid-color 1/6)))
    (present (make-instance '%background) '%background :stream stream)
    ;; draw the vertical grid lines and beat numbers at the bottom
    (loop :for beat :from 0 :by grid-size :upto (max (+ (dur frame) 32) (/ stream-width (* beat-size grid-size)))
          :for xpos = (beat-to-x-pixel beat frame)
          :do (draw-line* stream xpos 0 xpos stream-height :ink (if (= (round beat) beat) grid-color bg-grid-mixed))
              (draw-text* stream (write-to-string beat) (1+ xpos) (1- stream-height) :ink grid-color))
    ;; draw the horizontal grid lines and note names on the side
    (loop :for y :from 0 :upto 127
          :for ypos := (- stream-height (* y y-size))
          :for top-ypos := (- stream-height (* (1+ y) y-size))
          :do
             (draw-line* stream 0 ypos stream-width ypos :ink grid-color)
             (draw-text* stream (note-text y) 1 top-ypos :align-y :top :ink grid-color))
    ;; draw the notes (events)
    (dolist (event events)
      (updating-output (stream :unique-id event :cache-value event :cache-test #'event-presentation-equal)
        (present event 'event :stream stream))))
  ;; make sure we don't lose our current scroll location after we redraw
  (with-slots (%saved-extent) stream
    (apply #'scroll-extent stream
           (if %saved-extent
               (list (rectangle-min-x %saved-extent) (rectangle-min-y %saved-extent))
               (list 0 0)))))

(define-application-frame keyboard-gui (thundersnow-application-frame)
  ()
  (:command-table (keyboard-gui
                   :inherit-from (keyboard-gui-file-command-table
                                  keyboard-gui-edit-command-table
                                  keyboard-gui-view-command-table
                                  keyboard-gui-tools-command-table
                                  keyboard-gui-help-command-table)
                   :menu (("File" :menu keyboard-gui-file-command-table)
                          ("Edit" :menu keyboard-gui-edit-command-table)
                          ("View" :menu keyboard-gui-view-command-table)
                          ("Tools" :menu keyboard-gui-tools-command-table)
                          ("Help" :menu keyboard-gui-help-command-table))))
  (:panes
   (keyboard-gui-pane (make-pane 'keyboard-gui-pane))
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
      (5/6 keyboard-gui-pane)
      (1/6 interactor-pane)
      pointer-documentation-pane)))
  (:menu-bar t))

(define-command-table keyboard-gui-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

(define-command-table keyboard-gui-edit-command-table)

(define-keyboard-gui-command (com-resize-event) ((record 'event) (offset-x 'real :default 0) (offset-y 'real :default 0))
  ;; (declare (ignorable record offset-x offset-y))
  (let* ((stream (find-pane-named *application-frame* 'keyboard-gui-pane))
         (damaged-region +nowhere+)
         (event (presentation-object record))
         (event-beat (beat event))
         (old-sustain (event-value event :sustain)))
    (erase-output-record record stream nil)
    (block tracking-pointer
      (tracking-pointer (stream)
        (:pointer-motion (x)
                         (let* ((x-beat (x-pixel-to-beat-quantized x *application-frame*))
                                (new-sustain (max (slot-value *application-frame* 'grid-size) (- x-beat event-beat)))
                                (rect (multiple-value-list (bounding-rectangle* record))))
                           (when (/= old-sustain new-sustain)
                             (handle-repaint stream damaged-region)
                             (setf (elt rect 2) (* (slot-value *application-frame* 'beat-size)
                                                   (+ new-sustain event-beat))
                                   damaged-region (apply 'make-rectangle* rect)
                                   (event-value event :sustain) new-sustain
                                   old-sustain new-sustain)
                             (with-output-recording-options (stream :record nil :draw t)
                               (present event 'event :stream stream :view +graphical-view+)))))
        (:pointer-button-release ()
                                 (return-from tracking-pointer nil))))
    (stream-add-output-record stream record)))

(define-presentation-to-command-translator event-resizing-translator
    (event com-resize-event keyboard-gui
     :tester ((object presentation x)
              (when (hovering-for-resize-p x presentation)
                (climi::set-sheet-pointer-cursor ; (setf pointer-cursor) doesn't seem to work, so we do this
                 (find-port)
                 (find-pane-named *application-frame* 'keyboard-gui-pane)
                 :horizontal-scroll)
                t))
     :pointer-documentation "Resize event"
     :menu nil)
    (object presentation window x y)
  (list presentation x y))

(define-command-table keyboard-gui-view-command-table)

(define-command-table keyboard-gui-tools-command-table
  :inherit-from (thundersnow-common-tools-command-table)
  :inherit-menu t)

(define-command-table keyboard-gui-help-command-table
  :inherit-from (thundersnow-common-help-command-table)
  :inherit-menu t)

(defun keyboard-gui ()
  "Open a keyboard-gui."
  (make-or-find-application-frame 'keyboard-gui))
