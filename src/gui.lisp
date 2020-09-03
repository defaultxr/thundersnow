(in-package #:thundersnow/common)

;;;; common gui functionality
;;; stuff that is used by all guis; views, theming, commands, etc

;;; functions

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
