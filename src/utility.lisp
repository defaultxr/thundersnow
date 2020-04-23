(in-package #:thundersnow/common)

(defparameter *swank-output* *standard-output*)

(defmacro with-swank-output (&body body)
  "Run BODY with *standard-output* bound to the swank output (so `print'/etc don't print to the CLIM stream). Useful for debugging."
  `(let ((*standard-output* *swank-output*))
     ,@body))

