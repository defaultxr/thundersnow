(defpackage #:thundersnow/tests
  (:use #:cl
        #:thundersnow
        #:alexandria
        #:mutility
        #:fiveam))

(in-package #:thundersnow/tests)

(def-suite thundersnow-tests
  :description "thundersnow tests suite.")

(in-suite thundersnow-tests)

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (undocumented-symbols :thundersnow)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~s"
              undocumented)))
