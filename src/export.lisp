(in-package #:thundersnow/common)

(defun ensure-common-symbols-exported ()
  "Ensure all symbols in thundersnow/common are exported. This is done so that thundersnow sub-packages (i.e. thundersnow/thundersnow, thundersnow/piano-roll, etc) only need to (use-package #:thundersnow/common) to get a consistent set of symbols."
  (let ((common-package (find-package :thundersnow/common)))
    (do-symbols (sym common-package)
      ;; sym is wrapped in (list ...) to ensure nil is exported.
      (export (list sym) common-package))))

(ensure-common-symbols-exported)
