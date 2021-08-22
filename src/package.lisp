;;;; package.lisp
;;; FIX: maybe uiop:define-package can be used to make all this more succinct?

(uiop:define-package #:thundersnow/common
  (:use)
  (:mix #:cl
        #:alexandria
        #:mutility
        #:cl-patterns
        #:bdef
        #+#.(cl:if (cl:find-package "CL-COLLIDER") '(:and) '(:or))
        #:cl-collider)
  (:reexport #:alexandria
             #:mutility
             #:cl-patterns
             #:bdef
             #+#.(cl:if (cl:find-package "CL-COLLIDER") '(:and) '(:or))
             #:cl-collider)
  (:export #:sprint
           #:note-text
           #:beat-text
           #:sustain-text

           #:*initialized*
           #:load-init
           #:thundersnow-initialize)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns)))

(uiop:define-package #:thundersnow/common-clim
  (:use)
  (:mix #:clim
        #:clim-lisp
        #:clim-extensions
        #:thundersnow/common)
  (:shadowing-import-from #:cl-patterns
                          #:pattern
                          #:event
                          #:stop
                          #:play
                          #:quant)
  (:reexport #:clim
             #:clim-lisp
             #:clim-extensions
             #:thundersnow/common)
  (:export #:all-command-tables
           #:make-or-find-application-frame

           #:mix-colors
           #:*theme*
           #:get-theme-color

           #:with-border

           #:thundersnow-common-file-command-table
           #:thundersnow-common-edit-command-table
           #:thundersnow-common-view-command-table
           #:thundersnow-common-tools-command-table
           #:thundersnow-common-help-command-table)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns)))

(uiop:define-package #:thundersnow/common-nod
  (:use #:cl)
  (:mix #:nodgui
        #:thundersnow/common)
  (:reexport #:nodgui
             #:thundersnow/common)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns)))

(uiop:define-package #:thundersnow/thundersnow
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :thundersnow))

(uiop:define-package #:thundersnow/keyboard-gui
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :keyboard-gui))

(uiop:define-package #:thundersnow/piano-roll
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :piano-roll))

(uiop:define-package #:thundersnow/stepseq
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :stepseq))

(uiop:define-package #:thundersnow/tracker
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :tracker))

(uiop:define-package #:thundersnow/wave-editor
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :wave-editor))

(uiop:define-package #:thundersnow/wave-editor-nod
  (:use)
  (:mix #:thundersnow/common-nod
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :wave-editor))

(uiop:define-package #:thundersnow
  (:use #:thundersnow/common-clim
        #:thundersnow/thundersnow
        #:thundersnow/piano-roll
        #:thundersnow/tracker
        #:thundersnow/wave-editor)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export #:thundersnow
           #:piano-roll
           #:tracker
           #:stepseq
           #:wave-editor
           #:keyboard-gui))
