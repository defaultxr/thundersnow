(in-package #:thundersnow/tests)

;;;; t/test.lisp - basic tests and test utilities/fixtures/etc for the thundersnow test suite.

(def-suite thundersnow-tests
  :description "thundersnow tests suite.")

(in-suite thundersnow-tests)

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (undocumented-symbols :thundersnow)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~s"
              undocumented)))
