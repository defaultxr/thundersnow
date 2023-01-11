(in-package #:thundersnow/common)

;;;; common.lisp - functionality common to all thundersnow interfaces.

;;; debug conveniences

(defvar *tmp* nil
  "Temporary variable for convenience during development.")

;;; clim utility functions

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

(defun all-command-tables ()
  "Get a list of all defined CLIM command tables."
  (keys climi::*command-tables*))

(defun all-frames (&key port frame-manager)
  "Get a list of all application frames currently open."
  (let (res)
    (apply #'map-over-frames
           (fn (push _ res))
           (append (when port (list :port port))
                   (when frame-manager (list :frame-manager frame-manager))))
    res))

(defun make-or-find-application-frame (frame-name &rest args)
  "Make a frame for FRAME-NAME, or return it (without raising it) if one already exists."
  (map-over-frames
   (fn (when (eql (frame-name _) frame-name)
         (return-from make-or-find-application-frame _))))
  (apply #'find-application-frame frame-name :activate t args))

(defun frame-all-panes (frame)
  "Get all panes of FRAME.

See also: `clim:frame-panes', `find-pane'"
  (flatten
   (etypecase frame
     (application-frame
      (when-let ((panes (ensure-list (frame-panes frame))))
        (cons panes (mapcan 'frame-all-panes panes))))
     (pane
      (when-let ((children (sheet-children frame)))
        (cons children (mapcan 'frame-all-panes children)))))))

(defun find-pane (name &optional (frame *application-frame*))
  "Find any pane named NAME, optionally limiting the search to FRAME.

See also: `frame-all-panes', `all-frames', `clim:find-pane-named'"
  (if frame
      (let ((frame (if (symbolp frame)
                       (find-application-frame frame :create nil :activate nil)
                       frame)))
        (or (find-pane-named frame name)
            (find-pane-named frame (ensure-symbol (concat name '-pane) (package-name (symbol-package name))))))
      (map-over-frames
       (fn (when-let ((pane (find-pane name _)))
             (return-from find-pane pane))))))

(defun find-record (pane thing &key (test #'eq))
  "Get the output record for THING in PANE. Returns the presentation as a second value"
  (labels ((walk (record)
             (let ((parent (output-record-parent record)))
               (multiple-value-bind (object err) (ignore-errors (presentation-object parent))
                 (when (and (not err)
                            (funcall test object thing))
                   (return-from find-record (values record parent)))))
             (map-over-output-records #'walk record)))
    (walk (stream-output-history pane))))

(defun find-presentation (pane thing &key (test #'eq))
  "Get the presentation for THING in PANE. Returns the output record as a second value."
  (let ((record (find-record pane thing :test test)))
    (values (output-record-parent record) record)))

(defun gadget-maybe-label (gadget)
  "Get GADGET's label, or nil if it is the empty string.

See also: `gadget-label'"
  (unless (emptyp (gadget-label gadget))
    (gadget-label gadget)))

(defmethod unsaved-data-p ((frame application-frame))
  nil)

(defun ask-confirmation (query &key (true-text "Yes") (false-text "No") (option-size :very-large))
  "Open a menu to prompt the user for the answer to a boolean question.

See also: `ask-quit'"
  (menu-choose `((,true-text :value t)
                 (,false-text :value nil))
               :text-style (make-text-style nil nil option-size)
               :label query))

(defun ask-quit (&key (unsaved-data-p (unsaved-data-p *application-frame*)) (unsaved-query "Save data before quitting Thundersnow and ending the Lisp process?") (saved-query "Really quit Thundersnow and end the Lisp process?") (save-text "Save and quit") (quit-text "Quit") (cancel-text "Cancel") (option-size :very-large))
  "Open a menu to prompt the user whether they really want to quit. If UNSAVED-DATA-P is true, use UNSAVED-QUERY to warn of the unsaved data and include SAVE-TEXT as an option to save the data before quitting.

See also: `ask-confirmation'"
  (menu-choose `(,@(when unsaved-data-p `((,save-text :value :save)))
                 (,quit-text :value :quit)
                 (,cancel-text :value nil))
               :text-style (make-text-style nil nil option-size)
               :label (if unsaved-data-p
                          unsaved-query
                          saved-query)))

(defun ask-close (&key (unsaved-data-p (unsaved-data-p *application-frame*)) (unsaved-query "Save data before closing this window?") (saved-query "Really close this window?") (save-text "Save and close") (quit-text "Close") (cancel-text "Cancel") (option-size :very-large))
  (ask-quit :unsaved-data-p unsaved-data-p :unsaved-query unsaved-query :saved-query saved-query :save-text save-text :quit-text quit-text :cancel-text cancel-text :option-size option-size))

;;; mcclim "monkey patching"

;; McCLIM (or its X backend at least) does not provide this class yet so we define it here.
;; McCLIM's lack of this event is tracked in this issue: https://github.com/McCLIM/McCLIM/issues/78
(unless (find-class 'pointer-double-click-event nil)
  (defclass pointer-double-click-event (pointer-button-event)
    ()))

;;; views

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass textual-view (view)
    ())

  (defclass graphical-view (view)
    ()))

(unless (boundp '+graphical-view+)
  (defconstant +graphical-view+ (make-instance 'graphical-view)))

;;; color functions

(defun mix-colors (color-1 color-2 &optional (mix 0.5))
  "Linearly mix between COLOR-1 and COLOR-2. MIX ranges from 0, meaning 100% color-1, to 1, meaning 100% color-2."
  (apply #'make-rgb-color (apply #'mapcar
                                 (lambda (c1 c2)
                                   (+ c1 (* mix (- c2 c1))))
                                 (mapcar (lambda (c) (multiple-value-list (color-rgb c))) (list color-1 color-2)))))

;;; theming/color functionality (FIX: generalize and move to mutility?)
;; see also: https://github.com/McCLIM/McCLIM/issues/842 ; "Sort out the gadget color situation"
;; allow "symbolic names", i.e. "background", "primary", "secondary", "accent", etc.
;; themes should also specify whether they are "dark" or not, so programmatic colors can change their behavior accordingly.
;; perhaps have a "fallback" item, specifying a theme to fall back to if a color is not defined in this one.
;; maybe use mcclim indirect inks instead of colors directly?

(defvar *theme* (list :background (make-gray-color 0.2)
                      :foreground +white+
                      :grid (make-gray-color 0.8)
                      :stopped +red+
                      :playing +green+
                      :ending +red4+
                      :starting +greenyellow+
                      :note-fill +red+
                      :selected-note-fill +blue+))

(defun theme-color (element)
  "Get the theme's color for a type of GUI element, i.e. :foreground, :background, :accent, etc.

See also: `*theme*'"
  (getf *theme* element))

(defun (setf theme-color) (value element)
  (check-type value design)
  (setf (getf *theme* element) value))

;;; drawing utilities

;; FIX: make it so the variable can be "pane" OR "stream"
(defmacro with-border ((&optional (background +white+) (thickness 1) &rest additional-args) &body body)
  (let ((padding 2))
    (with-gensyms (background-sym thickness-sym)
      `(let ((,background-sym ,background)
             (,thickness-sym ,thickness))
         (surrounding-output-with-border (pane :padding ,padding :padding-bottom ,(1- padding) :padding-top ,(1- padding) :background ,background-sym :thickness ,thickness-sym ,@additional-args)
           ,@body)))))

(defun wave-polygon-coord-seq (wave sheet &key (zoom 1))
  "Convert WAVE, an array of waveform data, into a list in coord-seq format usable by `draw-polygon*'."
  (let ((width (bounding-rectangle-width sheet))
        (hd2 (/ (bounding-rectangle-height sheet) 2))
        (length (length wave)))
    (loop :for idx :from 0
          :for e :across wave
          :collect (* width (/ idx length) zoom)
          :collect (+ hd2 (* hd2 e)))))

;;; presentation utilities

(defun event-presentation-equal (event1 event2) ;; used in `updating-output' for `draw-piano-roll'
  "False if EVENT1 and EVENT2 differ in their CLIM presentation representation."
  (and (eql (sustain event1) (sustain event2))
       (eql (beat event1) (beat event2))
       (eql (event-value event1 :midinote) (event-value event2 :midinote))))

;;; common command table

(define-command-table thundersnow-common-command-table)

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

(define-command (com-toggle-metronome :name t :menu t
                                      :command-table thundersnow-common-file-command-table
                                      :keystroke (#\m :meta))
    ()
  (format t "~A metronome.~%" (if (play-or-stop :-metronome) "Started" "Stopped")))

(add-menu-item-to-command-table 'thundersnow-common-file-command-table nil :divider nil :errorp nil)

(define-command (com-close :name t :menu t
                           :command-table thundersnow-common-file-command-table
                           :keystroke (#\w :control))
    ()
  "Close the current frame."
  (when-let ((close-p (ask-close)))
    (when (eql :save close-p)
      (com-save))
    (frame-exit *application-frame*)))

(define-command (com-quit :name t :menu t
                          :command-table thundersnow-common-file-command-table
                          :keystroke (#\q :control))
    ()
  "Completely quit Thundersnow and end the Lisp process."
  (when-let ((quit-p (ask-quit)))
    (when (eql :save quit-p)
      (com-save))
    (uiop:quit)))

;;; edit commands

(define-command-table thundersnow-common-edit-command-table)

;;; view commands

(define-command-table thundersnow-common-view-command-table)

(define-command (com-refresh :name t :menu t
                             :command-table thundersnow-common-view-command-table
                             :keystroke (:f5))
    ()
  ;; make clim redraw stuff? this is just for testing; remove this command later
  nil)

;;; tools commands

(define-command-table thundersnow-common-tools-command-table)

(add-menu-item-to-command-table 'thundersnow-common-tools-command-table "GUIs" :divider nil :errorp nil)

;; FIX: these functions should bring their respective windows to the front

(define-command (com-thundersnow :name t :menu t
                                 :command-table thundersnow-common-tools-command-table)
    ()
  (raise-frame (thundersnow/thundersnow:thundersnow)))

(define-command (com-tracker :name t :menu t
                             :command-table thundersnow-common-tools-command-table)
    ()
  (raise-frame (thundersnow/tracker:tracker)))

(define-command (com-piano-roll :name t :menu t
                                :command-table thundersnow-common-tools-command-table)
    ()
  (raise-frame (thundersnow/piano-roll:piano-roll)))

(define-command (com-stepseq :name t :menu t
                             :command-table thundersnow-common-tools-command-table)
    ()
  (raise-frame (thundersnow/stepseq:stepseq)))

(define-command (com-wave-editor :name t :menu t
                                 :command-table thundersnow-common-tools-command-table)
    ()
  (raise-frame (thundersnow/wave-editor:wave-editor)))

;;; help commands

(define-command-table thundersnow-common-help-command-table)

(define-command (com-about :name t :menu t
                           :command-table thundersnow-common-help-command-table)
    ()
  (format t "~&thundersnow ~a~%digital audio workstation and live coding laboratory~%a struct.ws project by modula t. worm and contributors~%" (asdf:component-version (asdf:find-system "thundersnow")))
  (present "https://w.struct.ws/thundersnow" 'url))

(define-command (com-readme :name "README" :menu t
                            :command-table thundersnow-common-help-command-table)
    ()
  (ed (asdf:system-relative-pathname :thundersnow "README.org")))

(define-command (com-repo :name t :menu t
                          :command-table thundersnow-common-help-command-table)
    ()
  (open-url (asdf:system-homepage (asdf:find-system :thundersnow t))))

(define-command (com-issues :name t :menu t
                            :command-table thundersnow-common-help-command-table)
    ()
  (open-url (asdf:system-bug-tracker (asdf:find-system :thundersnow t))))

(define-presentation-type url ())

(define-presentation-action open (url nil thundersnow-common-command-table
                                  :gesture :select
                                  :pointer-documentation "Open URL in the system's default browser")
                            (url)
  (format t "~&Opening URL ~S in the system's default browser...~%" url)
  (open-url url)
  nil)

(define-presentation-action copy-url (url nil thundersnow-common-command-table
                                      :pointer-documentation "Copy URL address to the system clipboard")
                            (url)
  (publish-selection *standard-output* :clipboard url 'string)
  (format t "~&Copied URL ~S to the system clipboard.~%" url)
  nil)

;;; abstract class for thundersnow application frames
;; provides the functionality common among the various thundersnow GUIs

(defclass thundersnow-application-frame (standard-application-frame)
  ())

(defmethod default-frame-top-level ((frame thundersnow-application-frame) &rest args &key prompt &allow-other-keys)
  (apply #'call-next-method frame :prompt (or prompt (concat (frame-pretty-name frame) ": ")) (remove-from-plist args :prompt)))
