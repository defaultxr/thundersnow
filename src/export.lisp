(in-package #:thundersnow/common)

(defun ensure-common-symbols-exported (package)
  "Ensure all symbols in thundersnow/common are exported. This is done so that thundersnow sub-packages (i.e. thundersnow/thundersnow, thundersnow/piano-roll, etc) only need to (use-package #:thundersnow/common) to get a consistent set of symbols."
  (let ((common-package (find-package package)))
    (do-symbols (sym common-package)
      ;; sym is wrapped in (list ...) to ensure nil is exported.
      (handler-bind (#+sbcl ;; FIX: is there a better way to do this?
                     (sb-ext:name-conflict (lambda (e)
                                             (declare (ignore e))
                                             (invoke-restart 'sb-impl::take-new))))
        (export (list sym) common-package)))))

#+nil
(ensure-common-symbols-exported :thundersnow/common)

;; (in-package #:thundersnow/common-clim)
;; (thundersnow/common::ensure-common-symbols-exported :thundersnow/common-clim)

;; (in-package #:thundersnow/common-nod)
;; (thundersnow/common::ensure-common-symbols-exported :thundersnow/common-nod)
