;;;; scope.lisp - scope gadget.

;;; NOTES:
;; 06:42:53 scymtym:     drawing a line plot with a single DRAW-POLYGON* call can be much more efficient than multiple draw calls, depending on the backend

(in-package #:thundersnow/common)

(defclass scope (basic-gadget)
  ((wave :initarg :wave :initform (make-array 200 :element-type 'real :initial-element 0d0) :reader scope-wave :type array :documentation "The array containing the scope's waveform data.")
   (redraw-delay :initarg :redraw-delay :initform 0.05 :accessor redraw-delay :type (real 0) :documentation "The time in seconds to wait before the next redraw.")))

(defmethod (setf scope-wave) (value scope)
  (setf (slot-value scope 'wave) value
        (pane-needs-redisplay scope) t))

(defmethod activate-gadget ((scope scope))
  (clime:schedule-event scope (make-instance 'timer-event :sheet scope) (redraw-delay scope)))

(defmethod handle-repaint ((scope scope) region)
  (declare (ignore region))
  (with-slots (wave) scope
    (draw-polygon* scope (wave-polygon-coord-seq wave scope) :filled nil :closed nil :ink +white+)
    ;; (loop :for idx :from 0
    ;;       :for e :across wave
    ;;       :for scaled-x := (* width (/ idx length))
    ;;       :do (draw-line* scope scaled-x hd2 scaled-x (+ hd2 (* hd2 e))
    ;;                       :ink +white+))
    ))

(defmethod handle-event ((scope scope) (event timer-event))
  (when (pane-needs-redisplay scope)
    (handle-repaint scope (sheet-region scope)))
  (when (gadget-active-p scope)
    (clime:schedule-event scope (make-instance 'timer-event :sheet scope) (redraw-delay scope))))

(defmethod handle-event ((scope scope) (event pointer-button-press-event))
  (setf (scope-wave scope) (make-array 200 :element-type 'real
                                           :initial-contents (loop :repeat 200 :collect (random-range -1.0 1.0)))))
