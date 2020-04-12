;;;; thundersnow.asd

(asdf:defsystem #:thundersnow
  :description "Digital audio workstation and live coding laboratory for Common Lisp"
  :author "modula t. <defaultxr@gmail.com>"
  :license "Specify license here"
  :version "0.1"
  ;; :homepage "https://w.struct.ws/thundersnow/"
  :source-control (:git "git@github.com:defaultxr/thundersnow.git")
  :bug-tracker "https://github.com/defaultxr/thundersnow/issues"
  :depends-on (#:alexandria
               #:mutility
               #:closer-mop
               #:cl-patterns/supercollider ;; FIX: ensure thundersnow is backend-agnostic
               #:bdef
               #:mcclim
               #:sndfile-blob
               #:bodge-sndfile)
  :serial t
  :components ((:file "src/package")
               (:file "src/utility")
               (:file "src/common")
               (:file "src/gui")
               (:file "src/thundersnow")
               (:file "src/piano-roll")
               (:file "src/tracker")
               (:file "src/waveform"))
  :in-order-to ((test-op (test-op "thundersnow/tests"))))

(asdf:defsystem #:thundersnow/tests
  :name "thundersnow/tests"
  :author "modula t. <defaultxr@gmail.com>"
  :description "FiveAM-based test suite for thundersnow."
  :license "Specify license here"
  :depends-on (#:thundersnow
               #:fiveam)
  :components ((:file "t/test")
               (:file "t/utility")
               (:file "t/common")
               (:file "t/gui")
               (:file "t/thundersnow")
               (:file "t/piano-roll")
               (:file "t/tracker"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:thundersnow-tests
                                                         :thundersnow/tests))))
