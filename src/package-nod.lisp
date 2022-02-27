;;;; package-nod.lisp - package definition for thundersnow's nodgui-based tools.

(uiop:define-package #:thundersnow/common-nod
  (:nicknames #:ts/nod #:ts/common-nod)
  (:use #:cl)
  (:mix-reexport #:nodgui
                 #:thundersnow/common)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns)))

(uiop:define-package #:thundersnow/wave-editor-nod
  (:nicknames #:ts/wen #:ts/wave-editor-nod)
  (:use)
  (:mix #:thundersnow/common-nod
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :wave-editor))
