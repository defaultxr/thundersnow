(in-package #:thundersnow/common)

;;; convenience constants

(defconstant pi/2 (/ pi 2)
  "Half of pi.")

(defconstant 0.5pi (* pi 0.5)
  "0.5x pi.")

(defconstant 1/2pi (* pi 0.5)
  "0.5x pi.")

(defconstant 1.5pi (* pi 1.5)
  "1.5x pi.")

(defconstant 2pi (* pi 2)
  "2x pi.")

;;; utility/debug functions

(defun sprint (object)
  "Swank/Slynk print; sugar to print to the Swank/Slynk output (`*debug-io*'), avoiding any CLIM interactors."
  (print object *debug-io*))

;;; basic synths and patterns

#+#.(cl:if (cl:find-package "CL-COLLIDER") '(:and) '(:or))
(unless (synthdef-metadata :default)
  (defsynth default ((gate 1) (freq 440) (amp 0.5) (pan 0) (out 0))
    (let* ((env (env-gen.kr (asr 0.01 1 0.1) :gate gate :act :free))
           (sig (sin-osc.ar freq 0 0.2)))
      (out.ar out (pan2.ar sig pan (* env amp))))))

(pb :-metronome
  :instrument :default
  :dur 1
  :legato 0.1
  :midinote (pif (pnary #'zerop (pnary #'mod (pbeat*) 4)) ;; FIX: change the length based on the time signature?
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

