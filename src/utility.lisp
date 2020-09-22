(in-package #:thundersnow/common)

(defparameter *swank-output* *standard-output*)

(defmacro with-swank-output (&body body)
  "Run BODY with *standard-output* bound to the swank output (so `print'/etc don't print to the CLIM stream). Useful for debugging."
  `(let ((*standard-output* *swank-output*))
     ,@body))

(defun sprint (object)
  "Swank print; sugar to print to the swank output, avoiding any CLIM interactors."
  (print object *swank-output*))

(defun all-command-tables ()
  "Get a list of all defined CLIM command tables."
  (keys climi::*command-tables*))
