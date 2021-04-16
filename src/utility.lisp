(in-package #:thundersnow/common)

(defun sprint (object)
  "Swank print; sugar to print to the swank output, avoiding any CLIM interactors."
  (print object *debug-io*))

(defun all-command-tables ()
  "Get a list of all defined CLIM command tables."
  (keys climi::*command-tables*))

;;; basic synths and patterns

;; FIX: only do this if cl-collider is loaded.
(unless (synthdef-metadata :default)
  (defsynth default ((gate 1) (freq 440) (amp 0.5) (pan 0) (out 0))
    (let* ((env (env-gen.kr (asr 0.01 1 0.1) :gate gate :act :free))
           (sig (sin-osc.ar freq 0 0.2)))
      (out.ar out (pan2.ar sig pan (* env amp))))))

(pb :-metronome
  :instrument :default
  :dur 1
  :legato 0.1
  :midinote (pif (pnary #'= 0
                        (pnary #'mod (pbeat) 4)) ;; FIX: change the length based on the time signature?
                 69
                 60)
  :quant 1)

;;; music functions

(defun note-text (midinote)
  "Get the friendly text string for MIDINOTE.

See also: `beat-text', `sustain-text'"
  (let ((rounded (round midinote)))
    (concat (unless (= midinote rounded) "~") (note-name rounded) (midinote-octave midinote) " (" midinote ")")))

(defun beat-text (event)
  "Get the beat text string for EVENT.

See also: `note-text', `sustain-text'"
  (concat "b: " (friendly-ratio-string (beat event))))

(defun sustain-text (event)
  "Get the sustain text string for EVENT.

See also: `note-text', `beat-text'"
  (concat "s: " (friendly-ratio-string (sustain event))))

;;; initialization

(defparameter *initialized* nil
  "True after `thundersnow-initialize' has been run and the configuration has been loaded.")

(defun load-init ()
  "Load the user's custom configurations by checking a few common locations."
  (dolist (file (list (concat (uiop:xdg-config-home) "thundersnow/init.lisp")
                      (concat (uiop:getenv "HOME") "/.thundersnow.lisp")))
    (when (load file :if-does-not-exist nil)
      (return-from load-init))))

(defun thundersnow-initialize ()
  "Run thundersnow's initialization routine; read config files, etc."
  (load-init)
  (setf *initialized* t))

;;;; common gui functionality
;;; stuff that is used by all guis; views, theming, commands, etc

;; mcclim (or its X backend at least) does not provide this class so we define it here.
(unless (find-class 'pointer-double-click-event nil)
  (defclass pointer-double-click-event (pointer-button-event)
    ()))

(defun make-or-find-application-frame (frame-name &rest args)
  "Make a frame for FRAME-NAME, or return it (without raising it) if one already exists."
  (map-over-frames
   (fn (when (eql (frame-name _) frame-name)
         (return-from make-or-find-application-frame _))))
  (apply #'find-application-frame frame-name :activate t args))

;;; views

(defclass textual-view (view)
  ())

(defconstant +textual-view+ (make-instance 'textual-view))

;;; color functions

(defun mix-colors (color-1 color-2 &optional (mix 0.5))
  "Linearly mix between COLOR-1 and COLOR-2. MIX ranges from 0, meaning 100% color-1, to 1, meaning 100% color-2."
  (apply 'make-rgb-color (apply 'mapcar
                                (lambda (c1 c2)
                                  (+ c1 (* mix (- c2 c1))))
                                (mapcar (lambda (c) (multiple-value-list (color-rgb c))) (list color-1 color-2)))))

;;; theming/color functionality (FIX: just move to mutility?)
;; see also: https://github.com/McCLIM/McCLIM/issues/842 ; "Sort out the gadget color situation"

(defvar *theme* (list
                 :background (make-rgb-color 0.3 0.3 0.4)
                 :foreground +black+
                 :grid (make-gray-color 0.8)
                 :note-fill +red+
                 :selected-note-fill +blue+))

(defun get-theme-color (element)
  "Get the theme's color for a type of GUI element, i.e. :foreground, :background, :accent, etc.

See also: `*theme*'"
  (getf *theme* element))

;;; drawing utils

;; FIX: make it so the variable can be "pane" OR "stream"
(defmacro with-border ((&optional (background +white+) (thickness 1) &rest additional-args) &body body)
  (let ((padding 2))
    (with-gensyms (background-sym thickness-sym)
      `(let ((,background-sym ,background)
             (,thickness-sym ,thickness))
         (surrounding-output-with-border (pane :padding ,padding :padding-bottom ,(1- padding) :padding-top ,(1- padding) :background ,background-sym :thickness ,thickness-sym ,@additional-args)
           ,@body)))))

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

(define-command (com-piano-roll :name t :menu t
                                :command-table thundersnow-common-tools-command-table)
    ()
  (thundersnow/piano-roll:piano-roll))

(define-command (com-tracker :name t :menu t
                             :command-table thundersnow-common-tools-command-table)
    ()
  (thundersnow/tracker:tracker))

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
    (format t "~&thundersnow ~a~%digital audio workstation and live coding laboratory~%a struct.ws project by modula t. worm and contributors~%" version)))

;;; knob gadget
