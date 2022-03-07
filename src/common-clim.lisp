(in-package #:thundersnow/common-clim)

;;;; common-clim.lisp - common functionality for the mcclim-based interfaces

;;; debug conveniences

(defvar *tmp* nil
  "Temporary variable for convenience during development.")

;;; clim utility functions

(defun all-frames (&key port frame-manager)
  "Get a list of all application frames currently open."
  (let (res)
    (apply 'map-over-frames
           (fn (push _ res))
           `(,@(when port (list :port port))
             ,@(when frame-manager (list :frame-manager frame-manager))))
    res))

(defun make-or-find-application-frame (frame-name &rest args)
  "Make a frame for FRAME-NAME, or return it (without raising it) if one already exists."
  (map-over-frames
   (fn (when (eql (frame-name _) frame-name)
         (return-from make-or-find-application-frame _))))
  (apply #'find-application-frame frame-name :activate t args))

(defun frame-all-panes (frame)
  "Get all panes of FRAME.

See also: `clim:frame-panes'"
  (flatten
   (etypecase frame
     (application-frame
      (when-let ((panes (ensure-list (frame-panes frame))))
        (cons panes (mapcan 'frame-all-panes panes))))
     (pane
      (when-let ((children (sheet-children frame)))
        (cons children (mapcan 'frame-all-panes children)))))))

(defun find-pane (name &optional (frame *application-frame*))
  "Find any pane named NAME."
  (if frame
      (or (find-pane-named frame name)
          (find-pane-named frame (ensure-symbol (concat name '-pane) (package-name (symbol-package name)))))
      (map-over-frames
       (fn (when-let ((pane (find-pane name _)))
             (return-from find-pane pane))))))

(defun all-command-tables ()
  "Get a list of all defined CLIM command tables."
  (keys climi::*command-tables*))

(defun bounding-rectangle-center* (rectangle)
  "Get numbers (as multiple values) for the center of RECTANGLE.

See also: `bounding-rectangle-center'"
  (values (/ (- (bounding-rectangle-max-x rectangle)
                (bounding-rectangle-min-x rectangle))
             2)
          (/ (- (bounding-rectangle-max-y rectangle)
                (bounding-rectangle-min-y rectangle))
             2)))

(defun bounding-rectangle-center (rectangle)
  "Get a `clim:point' for the center of RECTANGLE.

See also: `bounding-rectangle-center*'"
  (multiple-value-call #'make-point (bounding-rectangle-center* rectangle)))

(defun gadget-maybe-label (gadget)
  "Get GADGET's label, or nil if it is the empty string.

See also: `gadget-label'"
  (unless (emptyp (gadget-label gadget))
    (gadget-label gadget)))

;;; mcclim "monkey patching"

;; McCLIM (or its X backend at least) does not provide this class yet so we define it here.
;; McCLIM's lack of this event is tracked in this issue: https://github.com/McCLIM/McCLIM/issues/78
(unless (find-class 'pointer-double-click-event nil)
  (defclass pointer-double-click-event (pointer-button-event)
    ()))

;;; views

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass textual-view (view)
    ()))

(defconstant +textual-view+ (make-instance 'textual-view))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass graphical-view (view)
    ()))

(defconstant +graphical-view+ (make-instance 'graphical-view))

;;; color functions

(defun mix-colors (color-1 color-2 &optional (mix 0.5))
  "Linearly mix between COLOR-1 and COLOR-2. MIX ranges from 0, meaning 100% color-1, to 1, meaning 100% color-2."
  (apply 'make-rgb-color (apply 'mapcar
                                (lambda (c1 c2)
                                  (+ c1 (* mix (- c2 c1))))
                                (mapcar (lambda (c) (multiple-value-list (color-rgb c))) (list color-1 color-2)))))

;;; theming/color functionality (FIX: generalize and move to mutility?)
;; see also: https://github.com/McCLIM/McCLIM/issues/842 ; "Sort out the gadget color situation"

(defvar *theme* (list
                 :background (make-rgb-color 0.3 0.3 0.4)
                 :foreground +black+
                 :grid (make-gray-color 0.8)
                 :note-fill +red+
                 :selected-note-fill +blue+))

(defun theme-color (element) ;; FIX: define setf as well
  "Get the theme's color for a type of GUI element, i.e. :foreground, :background, :accent, etc.

See also: `*theme*'"
  (getf *theme* element))

;;; drawing utilities

;; FIX: make it so the variable can be "pane" OR "stream"
(defmacro with-border ((&optional (background +white+) (thickness 1) &rest additional-args) &body body)
  (let ((padding 2))
    (with-gensyms (background-sym thickness-sym)
      `(let ((,background-sym ,background)
             (,thickness-sym ,thickness))
         (surrounding-output-with-border (pane :padding ,padding :padding-bottom ,(1- padding) :padding-top ,(1- padding) :background ,background-sym :thickness ,thickness-sym ,@additional-args)
           ,@body)))))

;;; presentation utilities

(defun event-presentation-equal (event1 event2) ;; used in `updating-output' for `draw-piano-roll'
  "False if EVENT1 and EVENT2 differ in their CLIM presentation representation."
  (and (eql (sustain event1) (sustain event2))
       (eql (beat event1) (beat event2))
       (eql (event-value event1 :midinote) (event-value event2 :midinote))))

;;; file commands

(define-command-table thundersnow-common-file-command-table)

(define-command (com-set-tempo :name t :menu t
                               :command-table thundersnow-common-file-command-table)
    ((tempo 'string :default (tempo *clock*) :prompt "Tempo (default unit: bps)"))
  (destructuring-bind (value &optional (unit "bps")) (string-split tempo)
    (let ((value (read-from-string value)))
      (assert (numberp value) (value) "The provided tempo must be a number; got ~s instead. If you want to specify a unit it must come after the number." value)
      (let ((unit (upcase-intern unit :keyword)))
        (assert (member unit (list :bps :bpm)) (unit) "UNIT must be either bps or bpm; got ~s instead." unit)
        (when (and (eql unit :bps)
                   (>= value 10))
          (restart-case
              (error "The provided beats per second value was abnormally high (~s beats per second = ~s beats per minute)!~%Are you sure you want to proceed?" value (* value 60))
            (continue ()
              :report "Use this value for the beats per second anyway."
              nil)
            (use-as-bpm ()
              :report "Use this value as beats per minute instead."
              (setf unit :bpm))
            (specify-another-tempo ()
              :report "Specify another value for the beats per second instead."
              (execute-frame-command *application-frame*
                                     (command-line-read-remaining-arguments-for-partial-command
                                      (find-command-table 'thundersnow)
                                      (frame-standard-output *application-frame*)
                                      (list 'com-set-tempo *unsupplied-argument-marker*)
                                      0))
              (invoke-restart 'abort)) ;; FIX: is there some way to avoid the "Command aborted" message?
            (abort ()
              :report (lambda (stream)
                        (format stream "Cancel changing the tempo, leaving it at ~s (~s bpm)." (tempo *clock*) (* (tempo *clock*) 60)))
              (invoke-restart 'abort))))
        (setf (tempo *clock*) (if (eql unit :bps)
                                  value
                                  (/ value 60)))))))

(define-command (com-quit :name t :menu t
                          :command-table thundersnow-common-file-command-table
                          :keystroke (#\q :control))
    ()
  (frame-exit *application-frame*))

;;; edit commands

(define-command-table thundersnow-common-edit-command-table)

;;; view commands

(define-command-table thundersnow-common-view-command-table)

(define-command (com-refresh :name t :menu t
                             :command-table thundersnow-common-view-command-table)
    ()
  ;; make clim redraw stuff? this is just for testing; remove this command later
  nil)

;;; tools commands

(define-command-table thundersnow-common-tools-command-table)

(add-menu-item-to-command-table 'thundersnow-common-tools-command-table "GUIs" :divider nil)

;; FIX: these functions should bring their respective windows to the front

(define-command (com-thundersnow :name t :menu t
                                 :command-table thundersnow-common-tools-command-table)
    ()
  (thundersnow/thundersnow:thundersnow))

(define-command (com-tracker :name t :menu t
                             :command-table thundersnow-common-tools-command-table)
    ()
  (thundersnow/tracker:tracker))

(define-command (com-piano-roll :name t :menu t
                                :command-table thundersnow-common-tools-command-table)
    ()
  (thundersnow/piano-roll:piano-roll))

(define-command (com-stepseq :name t :menu t
                             :command-table thundersnow-common-tools-command-table)
    ()
  (thundersnow/stepseq:stepseq))

(define-command (com-wave-editor :name t :menu t
                                 :command-table thundersnow-common-tools-command-table)
    ()
  (thundersnow/wave-editor:wave-editor))

;;; help commands

(define-command-table thundersnow-common-help-command-table)

(define-command (com-readme :name "README" :menu t
                            :command-table thundersnow-common-help-command-table)
    ()
  (ed (asdf:system-relative-pathname :thundersnow "README.org")))

(define-command (com-repo :name t :menu t
                          :command-table thundersnow-common-help-command-table)
    ()
  (open-url (asdf:system-homepage (asdf:find-system :thundersnow t))))

(define-command (com-bugs :name t :menu t
                          :command-table thundersnow-common-help-command-table)
    ()
  (open-url (asdf:system-bug-tracker (asdf:find-system :thundersnow t))))

(define-command (com-about :name t :menu t
                           :command-table thundersnow-common-help-command-table)
    ()
  (let* ((system (asdf:find-system "thundersnow"))
         (version (asdf:component-version system)))
    (format t "~&thundersnow ~a~%digital audio workstation and live coding laboratory~%a struct.ws project by modula t. worm and contributors~%https://w.struct.ws/thundersnow~%" version)))

;;; knob gadget
;; TODO:
;; - implement "highlight" on hover
;; - disarm when the button is released outside of the geometry after a drag
;; - implement "orientation"; i.e. `oriented-gadget-mixin' - to control whether to drag horizontally or vertically (maybe we can make a "both" option as well?)
;; - implement controlspec-esque functionality (i.e. deferring to the parameters of it if there is one set, including min, max, warp, step, default, units, etc)
;; - better modifier key functionality; i.e. shift should also work, and combining multiple modifiers (i.e. ctrl+alt or ctrl+shift) should mix their effects
;; - implement changing value by mouse scrolling

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

;;; tempo pane

(defclass tempo-pane (basic-gadget)
  ())

(defmethod handle-event ((pane tempo-pane) (event timer-event))
  (let ((region (sheet-region pane))
        (clock cl-patterns:*clock*))
    (draw-rectangle* pane 0 0 (rectangle-width region) (rectangle-height region)
                     :filled t
                     :ink (if clock
                              (let ((c (expt (- 1 (mod (- (beat *clock*) (time-dur (clock-latency clock))) 1.0)) 3)))
                                (make-rgb-color (* c 0.5) (+ 0.5 (* 0.5 c)) (* c 0.5)))
                              (make-gray-color 0.5)))
    (draw-text pane
               (if clock
                   (let ((tempo (cl-patterns:tempo clock)))
                     (format nil "BPM: ~f~%~$ Hz" (* 60 tempo) tempo))
                   (format nil "null *clock*~%(click to create)"))
               (bounding-rectangle-center region)
               :align-x :center :align-y :center))
  (clime:schedule-event pane (make-instance 'timer-event :sheet pane) 0.01))

(defmethod handle-event ((pane tempo-pane) (event climi::pointer-button-press-event))
  (if *clock*
      (execute-frame-command *application-frame* (list 'com-set-tempo))
      (start-clock-loop :tempo 110/60)))

(defmethod handle-event ((pane tempo-pane) (event climi::pointer-scroll-event))
  (when *clock*
    (incf (tempo *clock*) (* 1/600 (slot-value event 'climi::delta-y)))))

;;; scope pane

(defparameter *scope-wave* (make-array 200 :element-type 'double-float :initial-element 0d0))

(defclass scope-pane (basic-gadget)
  ())

(defmethod handle-event ((pane scope-pane) (event timer-event))
  (let* ((rect (bounding-rectangle (sheet-region pane)))
         (width (rectangle-width rect))
         (height (rectangle-height rect))
         (hd2 (/ height 2)))
    (draw-rectangle* pane 0 0 width height :filled t :ink (make-gray-color 0.2))
    (draw-line* pane 0 hd2 width hd2 :ink +white+))
  (clime:schedule-event pane (make-instance 'timer-event :sheet pane) 0.01))

