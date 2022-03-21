;;;; tempo-pane.lisp - tempo display/control gadget.

(in-package #:thundersnow/common-clim)

(defclass tempo-pane (basic-gadget)
  ())

(defmethod activate-gadget :after ((tempo-pane tempo-pane))
  (clime:schedule-event tempo-pane (make-instance 'timer-event :sheet tempo-pane) 0.1))

(defmethod handle-repaint ((tempo-pane tempo-pane) region)
  (declare (ignore region))
  (let ((region (sheet-region tempo-pane))
        (clock cl-patterns:*clock*))
    (draw-rectangle* tempo-pane 0 0 (rectangle-width region) (rectangle-height region)
                     :filled t
                     :ink (if clock
                              (let ((c (expt (- 1 (mod (- (beat *clock*) (time-dur (clock-latency clock))) 1.0)) 3)))
                                (make-rgb-color (* c 0.5) (+ 0.5 (* 0.5 c)) (* c 0.5)))
                              (make-gray-color 0.5)))
    (draw-text tempo-pane
               (if clock
                   (let ((tempo (cl-patterns:tempo clock)))
                     (format nil "~f BPM~%~$ Hz" (* 60 tempo) tempo))
                   (format nil "null *clock*~%(click to create)"))
               (bounding-rectangle-center region)
               :align-x :center :align-y :center)))

(defmethod handle-event ((tempo-pane tempo-pane) (event timer-event))
  (handle-repaint tempo-pane (sheet-region tempo-pane))
  (when (gadget-active-p tempo-pane)
    (clime:schedule-event tempo-pane (make-instance 'timer-event :sheet tempo-pane) 0.01)))

(defmethod handle-event ((tempo-pane tempo-pane) (event climi::pointer-button-press-event))
  (if *clock*
      (execute-frame-command *application-frame* (list 'com-set-tempo))
      (start-clock-loop :tempo 110/60)))

(defmethod handle-event ((tempo-pane tempo-pane) (event climi::pointer-scroll-event))
  (when *clock*
    (incf (tempo *clock*) (* -1/600 (slot-value event 'climi::delta-y)))))
