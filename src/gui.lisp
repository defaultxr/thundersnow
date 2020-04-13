(in-package #:thundersnow)

;;;; common gui functionality
;;; stuff that is used by all guis

(defclass textual-view (view)
  ())

(defconstant +textual-view+ (make-instance 'textual-view))

;;; file commands

(define-command-table thundersnow-common-file-command-table)

(define-command (com-refresh :name t :menu t
                             :command-table thundersnow-common-file-command-table)
    ()
  ;; make clim redraw stuff? this is just for testing; remove this command later
  nil)

(define-command (com-quit :name t :menu t
                          :command-table thundersnow-common-file-command-table
                          :keystroke (#\q :control))
    ()
  (frame-exit *application-frame*))

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
