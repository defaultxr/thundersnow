;;;; package.lisp
;;; FIX: maybe uiop:define-package can be used to make all this more succinct?

(uiop:define-package #:thundersnow/common
  (:nicknames #:ts/c #:ts/common)
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
  (:nicknames #:ts/c-c #:ts/cc #:ts/clim)
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
  (:export #:all-frames
           #:make-or-find-application-frame
           #:frame-all-panes
           #:find-pane
           #:all-command-tables

           #:textual-view
           #:+textual-view+
           #:graphical-view
           #:+graphical-view+

           #:mix-colors
           #:*theme*
           #:get-theme-color

           #:with-border

           #:event-presentation-equal

           #:thundersnow-common-file-command-table
           #:thundersnow-common-edit-command-table
           #:thundersnow-common-view-command-table
           #:thundersnow-common-tools-command-table
           #:thundersnow-common-help-command-table

           #:tempo-pane
           #:scope-pane)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns)))

(uiop:define-package #:thundersnow/common-nod
  (:nicknames #:ts/nod #:ts/common-nod)
  (:use #:cl)
  (:mix #:nodgui
        #:thundersnow/common)
  (:reexport #:nodgui
             #:thundersnow/common)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns)))

(uiop:define-package #:thundersnow/thundersnow
  (:nicknames #:ts/ts)
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :thundersnow))

(uiop:define-package #:thundersnow/keyboard-gui
  (:nicknames #:ts/kg)
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :keyboard-gui))

(uiop:define-package #:thundersnow/piano-roll
  (:nicknames #:ts/pr)
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :piano-roll))

(uiop:define-package #:thundersnow/stepseq
  (:nicknames #:ts/ss)
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :stepseq))

(uiop:define-package #:thundersnow/tracker
  (:nicknames #:ts/tr)
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :tracker))

(uiop:define-package #:thundersnow/wave-editor
  (:nicknames #:ts/we)
  (:use)
  (:mix #:thundersnow/common-clim
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :wave-editor))

(uiop:define-package #:thundersnow/wave-editor-nod
  (:nicknames #:ts/wen #:ts/wave-editor-nod)
  (:use)
  (:mix #:thundersnow/common-nod
        #:cl)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export :wave-editor))

(uiop:define-package #:thundersnow
  (:nicknames #:ts)
  (:use #:thundersnow/common-clim
        #:thundersnow/thundersnow
        #:thundersnow/piano-roll
        #:thundersnow/tracker
        #:thundersnow/stepseq
        #:thundersnow/wave-editor
        #:thundersnow/keyboard-gui)
  (:local-nicknames (:a :alexandria)
                    (:clp :cl-patterns))
  (:export #:thundersnow
           #:piano-roll
           #:tracker
           #:stepseq
           #:wave-editor
           #:keyboard-gui))
