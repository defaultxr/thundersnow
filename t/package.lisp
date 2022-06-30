;;;; t/package.lisp - the package definition for thundersnow's test suite.

(uiop:define-package #:thundersnow/tests
  (:nicknames #:ts/t #:ts/tests)
  (:use)
  (:mix #:fiveam
        #:clim
        #:clim-lisp
        #:clim-extensions
        #:thundersnow/common
        #:alexandria
        #:mutility))
