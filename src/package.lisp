;;;; package.lisp

(defpackage #:thundersnow
  (:shadowing-import-from #:alexandria
                          :simple-parse-error)
  (:shadowing-import-from #:cl-patterns
                          :pattern
                          :event
                          :stop
                          :play
                          :quant)
  (:shadowing-import-from #:clim
                          :path)
  (:shadowing-import-from #:cl-collider
                          :mean)
  (:shadowing-import-from #:mutility
                          :wrap)
  (:use #:alexandria
        #:mutility
        #:cl-patterns
        #:bdef
        #:clim-extensions
        #:clim
        #:clim-lisp
        #:cl-collider)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export
   :thundersnow))
