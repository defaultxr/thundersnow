(in-package #:thundersnow/tests)

;;;; t/common-clim.lisp - tests for thundersnow's common CLIM utilities.

(in-suite thundersnow-tests)

(test knob
  (is (every #'identity (mapcar (lambda (knob-angle clim-angle)
                                  (approx= (knob-angle-clim-angle knob-angle) clim-angle))
                                (list 0     0.5pi pi    1.5pi 2pi)
                                (list 1.5pi pi    0.5pi 0     1.5pi)))
      "")
  (is (every #'identity (mapcar (lambda (clim-angle knob-angle)
                                  (approx= (clim-angle-knob-angle clim-angle) knob-angle))
                                (list 0     0.5pi pi    1.5pi 2pi)
                                (list 1.5pi pi    0.5pi 0     1.5pi)))
      ""))
