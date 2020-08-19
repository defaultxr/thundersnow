;;;; package.lisp

(defpackage #:thundersnow/common
  (:shadowing-import-from #:alexandria
                          #:simple-parse-error)
  (:shadowing-import-from #:cl-patterns
                          #:pattern
                          #:event
                          #:stop
                          #:play
                          #:quant)
  (:shadowing-import-from #:clim
                          #:path)
  (:shadowing-import-from #:cl-collider
                          #:mean)
  (:shadowing-import-from #:mutility
                          #:wrap
                          #:fold)
  (:use #:alexandria
        #:mutility
        #:cl-patterns
        #:bdef
        #:clim-extensions
        #:clim
        #:clim-lisp
        #:cl-collider)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns)))

(defpackage #:thundersnow/thundersnow
  (:use #:thundersnow/common)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :thundersnow))

(defpackage #:thundersnow/piano-roll
  (:use #:thundersnow/common)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :piano-roll))

(defpackage #:thundersnow/tracker
  (:use #:thundersnow/common)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :tracker))

(defpackage #:thundersnow/wave-editor
  (:use #:thundersnow/common)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :wave-editor))

(defpackage #:thundersnow
  (:use #:thundersnow/common
        #:thundersnow/thundersnow
        #:thundersnow/piano-roll
        #:thundersnow/tracker
        #:thundersnow/wave-editor)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export #:thundersnow
           #:piano-roll
           #:tracker
           #:wave-editor))
