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

;;; tempo pane

(defclass tempo-pane (basic-gadget)
  ())

(defmethod handle-event ((pane tempo-pane) (event timer-event))
  (let* ((rect (bounding-rectangle (sheet-region pane)))
         (width (rectangle-width rect))
         (height (rectangle-height rect))
         (clock cl-patterns:*clock*))
    (draw-rectangle* pane 0 0 width height
                     :filled t
                     :ink (if clock
                              (let ((c (expt (- 1 (mod (- (beat *clock*) (time-dur (clock-latency clock))) 1.0)) 3)))
                                (make-rgb-color (* c 0.5) (+ 0.5 (* 0.5 c)) (* c 0.5)))
                              (make-gray-color 0.5)))
    (draw-text* pane
                (if clock
                    (format nil "BPM: ~f" (* 60 (cl-patterns:tempo clock)))
                    (format nil "null *clock*~%(click to create)"))
                (/ width 2) (/ height 2)
                :align-x :center
                :align-y :center))
  (clime:schedule-event pane (make-instance 'timer-event :sheet pane) 0.01))

(defmethod handle-event ((pane tempo-pane) (event climi::pointer-button-press-event))
  (if *clock*
      (com-set-tempo)
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

