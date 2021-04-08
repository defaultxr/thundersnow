;;;; thundersnow.asd

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
               #:closer-mop
               #:cl-patterns/supercollider ;; FIX: ensure thundersnow is backend-agnostic
               #:bdef
               #:mcclim)
  :serial t
  :components ((:file "src/package")
               (:file "src/utility")
               (:file "src/export")
               (:file "src/thundersnow")
               (:file "src/piano-roll")
               (:file "src/tracker")
               (:file "src/wave-editor"))
  :in-order-to ((test-op (test-op "thundersnow/tests"))))

(asdf:defsystem #:thundersnow/tests
  :name "thundersnow/tests"
  :author "modula t."
  :description "FiveAM-based test suite for thundersnow."
  :license "Specify license here"
  :depends-on (#:thundersnow
               #:fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "test")
               (:file "utility")
               (:file "thundersnow")
               (:file "piano-roll")
               (:file "tracker"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:thundersnow-tests
                                                         :thundersnow/tests))))
