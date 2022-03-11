;;;; knob.lisp - knob gadget.
;; TODO:
;; - implement "highlight" on hover
;; - disarm when the button is released outside of the geometry after a drag
;; - implement "orientation"; i.e. `oriented-gadget-mixin' - to control whether to drag horizontally or vertically (maybe we can make a "both" option as well?)
;; - implement controlspec-esque functionality (i.e. deferring to the parameters of it if there is one set, including min, max, warp, step, default, units, etc)
;; - better modifier key functionality; i.e. shift should also work, and combining multiple modifiers (i.e. ctrl+alt or ctrl+shift) should mix their effects
;; - implement changing value by mouse scrolling

(in-package #:thundersnow/common-clim)

(defclass knob (labelled-gadget-mixin value-gadget range-gadget-mixin)
  ((default-value :initarg :default-value :writer (setf default-value) :documentation "The default value that the knob should return to when \"reset\". If unbound, defaults to the center of the `gadget-range'.")
   (dead-angle :initarg :dead-angle :initform 0.5 :documentation "The \"dead zone\"; the angle in radians at the bottom of the knob that the indicator cannot point to.")
   (knob-margin :initarg :knob-margin :initform 2 :accessor knob-margin :documentation "Amount of empty space to put around the knob.")
   (indicator :initarg :indicator :initform +red+ :documentation "The color of the knob's value indicator.")
   (knob-background :initarg :knob-background :initform +gray40+ :documentation "The background color of the knob.")
   (dead-area-background :initarg :dead-area-background :documentation "The background color of the knob's \"dead zone\".")
   (last-pointer-coordinates :type list :documentation "The last seen pointer coordinates; used in drag calculations."))
  (:documentation "One-dimensional control surface controlled by dragging the mouse. Similar to a `slider' but with a radial output style."))

(defmethod initialize-instance :after ((knob knob) &key &allow-other-keys)
  (unless (slot-boundp knob 'dead-area-background)
    (multiple-value-bind (i h s) (color-ihs (slot-value knob 'knob-background))
      (setf (slot-value knob 'dead-area-background) (make-ihs-color (/ i 2) h s))))
  (unless (gadget-value knob)
    (setf (gadget-value knob) (default-value knob))))

(defmethod (setf gadget-value) :after (value (knob knob) &key &allow-other-keys)
  (handle-repaint knob (sheet-region knob)))

(defmethod default-value ((knob knob))
  (if (slot-boundp knob 'default-value)
      (slot-value knob 'default-value)
      ;; FIX: when mutility/cl-patterns ranges/control-specs are implemented, check for a default from those first
      (+ (gadget-min-value knob)
         (/ (gadget-range knob) 2))))

(defun knob-ellipse (knob)
  "Get the ellipse of KNOB's actual control region."
  (with-slots (knob-margin) knob
    (let ((wd2 (- (/ (bounding-rectangle-width knob) 2) knob-margin))
          (hd2 (- (/ (bounding-rectangle-height knob) 2) knob-margin)))
      (make-ellipse (bounding-rectangle-center knob) wd2 0 0 hd2))))

(defun knob-angle-clim-angle (knob-angle)
  "Convert a KNOB-ANGLE (where 0 is down, 0.5pi is left, etc) to a CLIM angle (where 0 is right, 0.5pi is up, etc).

See also: `clim-angle-knob-angle'"
  (mod (- 1.5pi knob-angle) 2pi))

(defun clim-angle-knob-angle (clim-angle)
  "Convert a CLIM-ANGLE (where 0 is right, 0.5pi is up, etc) to a knob angle (where 0 is down, 0.5pi is left, etc).

See also: `knob-angle-clim-angle'"
  (abs (- (mod clim-angle 2pi) 1.5pi)))

(defun knob-angle-point* (knob angle)
  "Get x and y values for the point on KNOB's ellipse at ANGLE, where 0 and 2pi are down, 0.5pi is left, pi is up, and 1.5pi is right. Does not take into account the dead-angle.

See also: `knob-angle-point', `knob-value-angle', `knob-value-point*', `knob-value-point'"
  (let ((center (bounding-rectangle-center knob))
        (2x-knob-margin (* 2 (knob-margin knob))))
    (values (- (point-x center)
               (* 1/2
                  (sin angle)
                  (- (bounding-rectangle-width knob) 2x-knob-margin)))
            (+ (point-y center)
               (* 1/2
                  (cos angle)
                  (- (bounding-rectangle-height knob) 2x-knob-margin))))))

(defun knob-angle-point (knob angle)
  "Get a `clim:point' on KNOB's ellipse at ANGLE.

See also: `knob-angle-point*', `knob-value-angle', `knob-value-point*', `knob-value-point'"
  (multiple-value-call #'make-point (knob-angle-point* knob angle)))

(defun knob-value-angle (knob value)
  "Get the knob angle for VALUE on KNOB, taking into account the dead-angle. If VALUE is the knob's min-value, the result is half the dead-angle; if VALUE is the knob's max-value, the result is 2pi minus half the dead-angle, and if VALUE is in the middle of the range, the result is pi.

See also: `knob-angle-point*', `knob-angle-point', `knob-value-point*', `knob-value-point'"
  (let* ((min-value (gadget-min-value knob))
         (dead-angle (slot-value knob 'dead-angle))
         (value-percent (/ (- value min-value) (gadget-range knob))))
    (+ (/ dead-angle 2)
       (* value-percent (- (* 2 pi) dead-angle)))))

(defun knob-value-point* (knob value)
  "Get x and y as values for the point on KNOB's ellipse at VALUE.

See also: `knob-value-point', `knob-angle-point*', `knob-angle-point', `knob-value-angle'"
  (knob-angle-point* knob (knob-value-angle knob value)))

(defun knob-value-point (knob value)
  "Get a `clim:point' on KNOB's ellipse for VALUE.

See also: `knob-value-point', `knob-angle-point*', `knob-angle-point', `knob-value-angle'"
  (multiple-value-call #'make-point (knob-value-point* knob value)))

(defmethod handle-repaint ((knob knob) region)
  (declare (ignore region))
  (with-slots (text-style knob-margin foreground indicator knob-background dead-area-background) knob
    (let ((smaller-text-style (merge-text-styles (make-text-style nil nil :smaller) text-style))
          (center (bounding-rectangle-center knob))
          (ellipse (knob-ellipse knob))
          (value (gadget-value knob))
          (min-value (gadget-min-value knob))
          (max-value (gadget-max-value knob)))
      (multiple-value-call #'draw-ellipse knob
        center (ellipse-radii ellipse)
        :filled t :ink knob-background :line-thickness 1.0)
      (multiple-value-call #'draw-ellipse knob
        center (ellipse-radii ellipse)
        :start-angle (knob-angle-clim-angle (knob-value-angle knob 0))
        :end-angle (knob-angle-clim-angle (knob-value-angle knob 1))
        :filled t :ink dead-area-background :line-thickness 1.0)
      (draw-line knob center (knob-value-point knob value) :ink indicator :line-thickness 2.0)
      ;; (surrounding-output-with-border (knob :background +white+))
      (draw-text knob (format nil "~@[~a~%~]~f" (gadget-maybe-label knob) value) center
                 :text-style text-style :ink foreground :align-x :center :align-y :center)
      (draw-text knob (format nil "~s" min-value) (knob-value-point knob min-value)
                 :text-style smaller-text-style :ink foreground :align-x :right :align-y :bottom)
      (draw-text knob (format nil "~s" max-value) (knob-value-point knob max-value)
                 :text-style smaller-text-style :ink foreground :align-x :left :align-y :bottom))))

(defmethod handle-event :around ((knob knob) (event pointer-event))
  (when (gadget-active-p knob)
    (call-next-method)))

(defmethod handle-event ((knob knob) (event pointer-enter-event))
  (with-slots ((armed climi::armed)) knob
    (unless armed
      (setf armed t)
      (armed-callback knob (gadget-client knob) (gadget-id knob)))))

(defmethod handle-event ((knob knob) (event pointer-exit-event))
  (with-slots ((armed climi::armed)) knob
    (when (eql armed t)
      (setf armed nil)
      (disarmed-callback knob (gadget-client knob) (gadget-id knob)))))

(defmethod handle-event ((knob knob) (event pointer-button-press-event))
  (with-slots ((armed climi::armed) last-pointer-coordinates) knob
    (case (pointer-event-button event)
      (1 ;; left click
       (when (and armed
                  (region-contains-position-p (knob-ellipse knob) (pointer-event-x event) (pointer-event-y event)))
         (setf armed :active
               last-pointer-coordinates (list (climi::pointer-event-native-graft-x event) (climi::pointer-event-native-graft-y event)))))
      (4 ;; right click
       (let ((label (or (gadget-maybe-label knob) "Knob")))
         (case (menu-choose '(("Set value" . set-value)
                              ("Reset value" . reset-value))
                            :label label)
           (set-value
            (when-let ((new-value (accepting-values (t :label label)
                                    (accept 'number :default (gadget-value knob) :prompt "Value"))))
              (setf (gadget-value knob :invoke-callback t) new-value)))
           (reset-value
            (setf (gadget-value knob :invoke-callback t) (default-value knob)))))))))

(defmethod handle-event ((knob knob) (event pointer-button-release-event))
  (with-slots ((armed climi::armed)) knob
    ;; FIX: disarm when button is released outside of the gadget area
    (when (eql armed :active)
      (setf armed t))))

(defmethod handle-event ((knob knob) (event pointer-motion-event))
  (with-slots ((armed climi::armed) last-pointer-coordinates) knob
    (when (eql armed :active)
      (let* ((new-pointer-coordinates (list (climi::pointer-event-native-graft-x event) (climi::pointer-event-native-graft-y event)))
             (drag (mapcar #'- last-pointer-coordinates new-pointer-coordinates))
             (modifier (event-modifier-state event))
             (drag-mul (/ (gadget-range knob) (cond ((logtest modifier +control-key+)
                                                     100000)
                                                    ((logtest modifier +meta-key+)
                                                     100)
                                                    (t 1000)))))
        (setf (gadget-value knob :invoke-callback t) (clamp (+ (gadget-value knob) (* (nth 1 drag) drag-mul))
                                                            (gadget-min-value knob)
                                                            (gadget-max-value knob))
              last-pointer-coordinates new-pointer-coordinates)))))

