;;;; package.lisp
;;; FIX: maybe uiop:define-package can be used to make all this more succinct?

(uiop:define-package #:thundersnow/common
  (:nicknames #:ts/c #:ts/common)
  (:use)
  (:mix-reexport #:cl
                 #:alexandria
                 #:mutility
                 #:metabang-bind
                 #:cl-patterns
                 #:bdef
                 ;; #+#.(cl:if (cl:find-package "CL-COLLIDER") '(:and) '(:or))
                 ;; #:cl-collider
                 #:clim-lisp
                 #:clim
                 #:clim-extensions)
  (:shadowing-import-from #:cl-patterns
                          #:pattern
                          #:event
                          #:stop
                          #:play
                          #:quant)
  (:export
   ;; utility.lisp
   #:pi/2 #:0.5pi #:1/2pi #:1.5pi #:2pi
   #:+powers-of-two+

   #:sprint

   #:index-before-greater-than

   #:note-text
   #:beat-text
   #:sustain-text

   #:unsaved-data-p

   #:*initialized*
   #:load-init
   #:thundersnow-ensure-initialized
   #:thundersnow-initialize

   ;; common.lisp

   #:bounding-rectangle-center*
   #:bounding-rectangle-center
   #:all-command-tables
   #:all-frames
   #:make-or-find-application-frame
   #:frame-all-panes
   #:find-pane
   #:find-record
   #:find-presentation

   #:ask-confirmation
   #:ask-quit
   #:ask-close

   #:textual-view
   #:+textual-view+
   #:graphical-view
   #:+graphical-view+

   #:scroll-position-preserving-mixin

   #:mix-colors
   #:*theme*
   #:theme-color

   #:with-border
   #:wave-polygon-coord-seq

   #:event-presentation-equal

   #:backend-task-added
   #:backend-task-removed
   #:backend-task-play-event
   #:update-tempo-information

   #:thundersnow-common-command-table

   #:thundersnow-common-file-command-table
   #:com-set-tempo

   #:com-close
   #:com-quit

   #:thundersnow-common-edit-command-table

   #:thundersnow-common-view-command-table
   #:com-refresh

   #:thundersnow-common-tools-command-table
   #:com-thundersnow
   #:com-tracker
   #:com-piano-roll
   #:com-stepseq
   #:com-wave-editor

   #:thundersnow-common-help-command-table
   #:com-readme
   #:com-repo
   #:com-bugs
   #:com-about

   #:knob
   #:knob-angle-clim-angle
   #:clim-angle-knob-angle
   #:knob-angle-point*
   #:knob-angle-point
   #:knob-value-angle
   #:knob-value-point*
   #:knob-value-point

   #:scope

   #:status-pane

   #:tempo-pane

   #:piano-mode
   #:piano-mode-mixin
   #:frame-recording-octave
   #:frame-task
   #:frame-recording-beat
   #:com-exit-piano-mode)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns)))

(uiop:define-package #:thundersnow/indexer
  (:nicknames #:ts/i)
  (:use)
  (:mix #:thundersnow/common)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns)))

(uiop:define-package #:thundersnow/thundersnow
  (:nicknames #:ts/ts)
  (:use)
  (:mix #:thundersnow/common)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns))
  (:export :thundersnow))

(uiop:define-package #:thundersnow/keyboard-gui
  (:nicknames #:ts/kg)
  (:use)
  (:mix #:thundersnow/common)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns))
  (:export :keyboard-gui))

(uiop:define-package #:thundersnow/piano-roll
  (:nicknames #:ts/pr)
  (:use)
  (:mix #:thundersnow/common)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns))
  (:export :piano-roll))

(uiop:define-package #:thundersnow/stepseq
  (:nicknames #:ts/ss)
  (:use)
  (:mix #:thundersnow/common)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns))
  (:export :stepseq))

(uiop:define-package #:thundersnow/tracker
  (:nicknames #:ts/tr)
  (:use)
  (:mix #:thundersnow/common)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns))
  (:export :tracker))

(uiop:define-package #:thundersnow/wave-editor
  (:nicknames #:ts/we)
  (:use)
  (:mix #:thundersnow/common)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns))
  (:export :wave-editor))

(uiop:define-package #:thundersnow
  (:nicknames #:ts)
  (:use #:thundersnow/common
        #:thundersnow/thundersnow
        #:thundersnow/piano-roll
        #:thundersnow/tracker
        #:thundersnow/stepseq
        #:thundersnow/wave-editor
        #:thundersnow/keyboard-gui)
  (:local-nicknames (#:a #:alexandria)
                    (#:clp #:cl-patterns))
  (:export #:thundersnow
           #:piano-roll
           #:tracker
           #:stepseq
           #:wave-editor
           #:keyboard-gui))
