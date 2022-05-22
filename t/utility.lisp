(in-package #:thundersnow/tests)

;;;; t/utility.lisp -

(in-suite thundersnow-tests)

(test index-before-greater-than
  "Test the `index-before-greater-than' function."
  (is-true (= -1 (ts/c::index-before-greater-than 0 '(2 4 8))))
  (is-true (= 0 (ts/c::index-before-greater-than 3 '(2 4 8))))
  (is-true (= 1 (ts/c::index-before-greater-than 4 '(2 4 8))))
  (is-true (= 2 (ts/c::index-before-greater-than 9 '(2 4 8)))))
