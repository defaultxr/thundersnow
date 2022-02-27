;;;; thundersnow.asd - the ASDF system definition for thundersnow.

(asdf:defsystem #:thundersnow
  :name "thundersnow"
  :description "Digital audio workstation and live coding laboratory for Common Lisp"
  :author "modula t."
  :license "Specify license here"
  :version "0.1"
  :homepage "https://w.struct.ws/thundersnow/"
  :bug-tracker "https://github.com/defaultxr/thundersnow/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/thundersnow.git")
  :depends-on (#:alexandria
               #:mutility
               #:mutility/loopy
               #:closer-mop
               #:cl-patterns
               #:bdef
               #:mcclim
               #:drei-mcclim)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utility")
               (:file "common-clim")
               (:file "export")
               (:file "thundersnow")
               (:file "keyboard-gui")
               (:file "piano-roll")
               (:file "stepseq")
               (:file "tracker")
               (:file "wave-editor"))
  ;; https://lispcookbook.github.io/cl-cookbook/scripting.html#with-asdf
  ;; :build-operation "program-op"
  ;; :build-pathname "thundersnow"
  ;; :entry-point "thundersnow:thundersnow"
  :in-order-to ((test-op (test-op "thundersnow/tests"))))

(asdf:defsystem #:thundersnow/nod
  :name "thundersnow/nod"
  :description "Digital audio workstation and live coding laboratory for Common Lisp"
  :author "modula t."
  :license "Specify license here"
  :version "0.1"
  :homepage "https://w.struct.ws/thundersnow/"
  :bug-tracker "https://github.com/defaultxr/thundersnow/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/thundersnow.git")
  :depends-on (#:thundersnow
               #:nodgui)
  :pathname "src/"
  :serial t
  :components ((:file "package-nod")
               (:file "common-nod")
               (:file "wave-editor-nod"))
  ;; https://lispcookbook.github.io/cl-cookbook/scripting.html#with-asdf
  ;; :build-operation "program-op"
  ;; :build-pathname "thundersnow"
  ;; :entry-point "thundersnow:thundersnow"
  )

(asdf:defsystem #:thundersnow/tests
  :name "thundersnow/tests"
  :author "modula t."
  :description "FiveAM-based test suite for thundersnow."
  :license "Specify license here"
  :depends-on (#:thundersnow
               #:fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "package")
               (:file "test")
               (:file "utility")
               (:file "common-clim")
               (:file "common-nod")
               (:file "thundersnow")
               (:file "keyboard-gui")
               (:file "piano-roll")
               (:file "stepseq")
               (:file "tracker")
               (:file "wave-editor")
               (:file "wave-editor-nod"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:thundersnow-tests
                                                         :thundersnow/tests))))
