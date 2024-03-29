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

(defconstant tau (* pi 2)
  "Tau; 2x pi. https://tauday.com/tau-manifesto")

(serapeum:defconst +powers-of-two+ (mapcar (curry #'expt 2) (iota 32 :start 1))
  "The first 32 powers of two, starting from 2.")

;;; utility/debug functions

(defun sprint (object)
  "Swank/Slynk print; sugar to print to the Swank/Slynk output (`*debug-io*'), avoiding any CLIM interactors."
  (print object *debug-io*))

;;; math

(defun index-before-greater-than (n list)
  "Get the index of the element just before the first element of LIST greater than N, -1 if the first element is greater, or the index of the last element if no element is greater."
  (let ((idx 0))
    (dolist (i list (1- idx))
      (incf idx)
      (when (> i n)
        (return-from index-before-greater-than (- idx 2))))))

;;; basic synths and patterns

(when (find-package "CL-COLLIDER")
  (load (asdf:system-relative-pathname 'thundersnow "src/cl-collider.lisp")))

(pb :-metronome
  :instrument :default
  :dur 1
  :legato 0.1
  :midinote (pif (pnary #'zerop (pnary #'mod (pbeat*) 4)) ; FIX: change the length based on the time signature?
                 69
                 60)
  :quant 1)

;;; music functions

(defun note-text (midinote)
  "Get the friendly text string for MIDINOTE.

See also: `beat-text', `sustain-text'"
  (let ((rounded (round midinote)))
    (concat (unless (= midinote rounded) "~")
            (midinote-note rounded)
            " (" midinote ")")))

(defun beat-text (event)
  "Get the beat text string for EVENT.

See also: `note-text', `sustain-text'"
  (concat "b: " (friendly-ratio-string (beat event))))

(defun sustain-text (event)
  "Get the sustain text string for EVENT.

See also: `note-text', `beat-text'"
  (concat "s: " (friendly-ratio-string (sustain event))))

;;; standard user interface functions

(defgeneric unsaved-data-p (object)
  (:documentation "True if OBJECT (typically an `application-frame' or `pane') has unsaved data."))

;;; initialization

(defvar *configuration-directory* (uiop:xdg-config-home "thundersnow")
  "The directory Thundersnow's configuration is stored.")

(defun configuration-directory (&optional subdirectory)
  "Get the full path of Thundersnow's configuration directory. With SUBDIRECTORY, get the specified subdirectory of it.

Examples:

;; (configuration-directory) ;=> \"/home/user/.config/thundersnow/\"
;; (configuration-directory \"themes\") ;=> \"/home/user/.config/thundersnow/themes/\""
  (uiop:native-namestring
   (if subdirectory
       (join-path-components *configuration-directory* subdirectory)
       *configuration-directory*)))

(defun (setf configuration-directory) (directory &optional subdirectory)
  (when subdirectory
    (error "Cannot set subdirectories of ~S." 'configuration-directory))
  (setf *configuration-directory* (uiop:native-namestring directory)))

(defvar *initialized* nil
  "True after `thundersnow-initialize' has been run and the configuration has been loaded.")

(defun load-init-file ()
  "Load the user's custom configurations by checking a few common locations."
  (dolist (file (list (merge-pathnames "init.lisp" (configuration-directory))
                      (concat (uiop:getenv "HOME") "/.thundersnow.lisp")))
    (when (load file :if-does-not-exist nil)
      (return-from load-init-file))))

(defun thundersnow-ensure-initialized ()
  "Run thundersnow's initialization routine if it hasn't already been run."
  (unless *initialized*
    (thundersnow-initialize)))

(defun thundersnow-initialize ()
  "Run thundersnow's initialization routine; read config files, etc."
  (load-init-file)
  (setf *initialized* t))

