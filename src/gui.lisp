(in-package #:thundersnow)

;;;; common gui functionality
;;; stuff that is used by all guis

(defclass textual-view (view)
  ())

(defconstant +textual-view+ (make-instance 'textual-view))

;;; theming functionality (FIX: just move to mutility?)

(defvar *theme* (list
                 :background (make-rgb-color 0.3 0.3 0.4)
                 :foreground +black+
                 :grid (make-gray-color 0.8)))

(defun get-theme-color (element)
  "Get the theme's color for a type of GUI element, i.e. :foreground, :background, :accent, etc.

See also: `*theme*'"
  (getf *theme* element))

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
  (thundersnow))

(define-command (com-piano-roll :name t :menu t
                                :command-table thundersnow-common-tools-command-table)
    ()
  (piano-roll))

(define-command (com-tracker :name t :menu t
                             :command-table thundersnow-common-tools-command-table)
    ()
  (tracker))

(define-command (com-wave-editor :name t :menu t
                                 :command-table thundersnow-common-tools-command-table)
    ()
  (wave-editor))

;;; help commands

(define-command-table thundersnow-common-help-command-table)

(define-command (com-about :name t :menu t
                           :command-table thundersnow-common-help-command-table)
    ()
  (let* ((system (asdf:find-system "thundersnow"))
         (version (asdf:component-version system)))
    (format t "~&thundersnow ~a~%digital audio workstation and live coding laboratory~%by modula t. worm~%" version)))

(define-command (com-readme :name "README" :menu t
                            :command-table thundersnow-common-help-command-table)
    ()
  (ed (asdf:system-relative-pathname :thundersnow "README.org")))

(define-command (com-repo :name t :menu t
                          :command-table thundersnow-common-help-command-table)
    ()
  (browse-url "https://github.com/defaultxr/thundersnow"))
