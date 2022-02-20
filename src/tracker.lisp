(in-package #:thundersnow/tracker)

;;; misc

(defparameter tmp nil)

;;; utilities (FIX):

(defun plist-value (plist key)
  "Get the value of KEY in PLIST. Returns the key the value was derived from as a second value."
  (doplist (k v plist (values nil nil))
    (when (eql k key)
      (return-from plist-value (values v k)))))

(defvar loop-detector nil)

(defun c (h w)
  (if (not loop-detector)
      (error "No loop-detector in dynamic environment.")
      (if (aref loop-detector h w)
          (error "Evaluation loop detected.")
          (progn
            (setf (aref loop-detector h w) t)
            (eval (content (aref (frame-ptrack *application-frame*) h w)))))))

;;; gui

(defclass cell ()
  ((frame :initarg :frame :accessor frame)
   (row :initarg :row :initform nil :accessor row)
   (column :initarg :column :initform nil :accessor column)
   (key :initarg :key :initform nil :accessor key)
   (content :initarg :content)
   (table :initarg :table :initform nil)))

(defmethod print-object ((cell cell) stream)
  (with-slots (row key) cell
    (print-unreadable-object (cell stream :type t)
      (format stream ":row ~s :key ~s" row key))))

(defun make-cell (&rest args)
  (apply #'make-instance 'cell args))

(defmethod content ((cell cell))
  (with-slots (frame row key) cell
    (if (and row key)
        (ptrack-cell frame row key)
        (when (slot-boundp cell 'content)
          (slot-value cell 'content)))))

(defmethod (setf content) (value (cell cell))
  (with-slots (frame row key) cell
    (let ((string (typecase value
                    (string value)
                    (t (write-to-string value)))))
      (if row
          (if key
              (setf (ptrack-cell frame row key) value
                    (ptrack-source-cell frame row key) string)
              (warn "Attempted to set ~s's content to ~s but no key set." cell value))
          (setf (ptrack-source-cell frame :header key) string)))))

(define-presentation-type cell ())

(define-presentation-method present (cell (type cell) stream (view textual-view) &key)
  (with-accessors ((row row) (column column) (content content)) cell
    (format stream "~a" (or content "     "))
    (when-let* ((table (slot-value cell 'table))
                (col-width (when (and (integerp column)
                                      (slot-boundp table 'climi::widths))
                             (elt (slot-value table 'climi::widths) (1+ column))))
                (row-height (when (and (integerp row)
                                       (slot-boundp table 'climi::heights))
                              (elt (slot-value table 'climi::heights) (1+ row)))))
      (draw-line* stream 0 0 col-width row-height :ink +transparent-ink+))))

(define-presentation-method present (cell (type cell) (stream string-stream) (view textual-view) &key)
  (with-accessors ((content content)) cell
    (format stream "~a" (or content ""))))

(defclass cell-row-number (cell) ;; FIX: use this instead of just `cell' ?
  ()
  (:documentation "The cell showing the row number."))

(define-presentation-type cell-row-number ()
  :inherit-from 'cell)

(defclass cell-column-header (cell) ;; FIX: use this instead of just `cell' ?
  ()
  (:documentation "The cell showing the column header."))

(define-presentation-type cell-column-header ()
  :inherit-from 'cell)

(defclass cell-pattern-id (cell-row-number cell-column-header)
  ()
  (:documentation "The cell showing the pattern number."))

(define-presentation-type cell-pattern-id ()
  :inherit-from '(and cell-column-header cell-row-number))

(define-presentation-method present (cell (type cell-pattern-id) stream (view textual-view) &key)
  (with-drawing-options (stream :ink +white+)
    (format stream "~a" (content cell))))

;; (defmethod replay-output-record)

(defclass cell-unparsed-text (textual-dialog-view) ())

(defconstant +cell-unparsed-text-view+ (make-instance 'cell-unparsed-text))

(define-presentation-method present (cell (type cell) stream (view cell-unparsed-text) &key)
  (format stream "~a" (content cell)))

(define-presentation-method present (cell (type cell) stream (view cell-unparsed-text) &key)
  (format stream "~a" (content cell)))

(define-presentation-method accept ((type cell) stream (view cell-unparsed-text) &key)
  (let ((cell (make-cell))) ;; FIX?
    (setf (content cell) (read stream))))

;;; pattern drawing

(defun draw-track (frame pane)
  "Draw the contents of `tracker'."
  (let ((ptrack (frame-ptrack frame))
        tcols)
    (labels ((gen-color (column)
               (case column
                 (0
                  +gray+)
                 (t
                  (make-ihs-color 1 (mod (/ (1- column) 12) 1.0) 0.9))))
             (cell-color (header-p column)
               (let ((color
                       (if-let ((meta (pattern-metadata (frame-ptrack (find-application-frame 'tracker)) :columns)))
                         (or (ignore-errors (elt meta column))
                             (gen-color column))
                         (gen-color column))))
                 (if header-p
                     color
                     (clim-internals::highlight-shade color)))))
      (with-slots ((header cl-patterns::header) (rows cl-patterns::rows)) ptrack
        (let ((keys (keys header))
              table)
          (formatting-table (pane)
            (setf table (stream-current-output-record pane))
            (formatting-row (pane)
              (with-border (+black+ 1)
                (formatting-cell (pane)
                  (present (make-instance 'cell-pattern-id :frame frame :content "p 0") 'cell-pattern-id :stream pane)))
              ;; column headers
              (let ((col 1))
                (doplist (key value header)
                  (prog1
                      (with-border ((cell-color t col))
                        (let ((tcell (formatting-cell (pane)
                                       (present (make-instance 'cell-column-header :frame frame :content (format nil "~s ~s" key value) :key key) 'cell-column-header :stream pane))))
                          (push tcell tcols)
                          tcell))
                    (incf col)))
                (reverse tcols)
                ;; new column button
                (with-border ((cell-color t col))
                  (formatting-cell (pane)
                    (present (make-instance 'cell-column-header :frame frame :content "+" :row :insert-column) 'cell-column-header :stream pane)))))
            ;; FIX: alternate color for every N rows, where N is determined by the beats per bar
            (dolist* (row-num row rows)
              (declare (ignore row))
              (formatting-row (pane)
                ;; row numbers
                (with-border ((cell-color nil 0))
                  (formatting-cell (pane)
                    (present (make-instance 'cell-row-number :frame frame :content row-num :row row-num :column 0 :table table) 'cell-row-number :stream pane)))
                ;; ptrack cells
                (dolist* (col-num key keys)
                  (with-border ((cell-color nil (1+ col-num)))
                    (formatting-cell (pane)
                      (present (make-cell :frame frame :row row-num :column (1+ col-num) :key key :table table) 'cell :stream pane)))))))))
      (fresh-line))))

(define-application-frame tracker ()
  ((ptrack :initarg :ptrack :reader frame-ptrack :initform (ptrack (list :degree (pseries 0 (pwhite 0 4) 16)
                                                                         :dur 1/4)
                                                                   (make-list 16))))
  (:command-table (tracker
		   :inherit-from (tracker-file-command-table
                                  tracker-edit-command-table
                                  tracker-view-command-table
                                  tracker-tools-command-table
                                  tracker-help-command-table)
		   :menu (("File" :menu tracker-file-command-table)
                          ("Edit" :menu tracker-edit-command-table)
                          ("View" :menu tracker-view-command-table)
                          ("Tools" :menu tracker-tools-command-table)
			  ("Help" :menu tracker-help-command-table))))
  (:panes
   (track :application :display-function 'draw-track)
   (interactor :interactor))
  (:layouts
   (default (horizontally () track interactor))
   (track track))
  (:menu-bar t)
  (:pointer-documentation t))

(defgeneric frame-ptrack (frame)
  (:documentation "The `ptrack' associated with FRAME."))

(defmethod frame-ptrack ((symbol symbol))
  (frame-ptrack (find-application-frame symbol)))

(defmethod (setf frame-ptrack) (value (frame tracker))
  (setf (slot-value frame 'ptrack) (etypecase value
                                     (symbol (if (pdef-pattern value)
                                                 value
                                                 (error "No pdef found with the name ~s" value))))))

(defmethod frame-standard-output ((frame tracker))
  (find-pane-named frame 'interactor))

(define-command-table enhanced-accept-values :inherit-from 'accept-values)

;;; ptrack methods

(defmethod ptrack-cell ((tracker tracker) row key &key (if-does-not-exist :error))
  (ptrack-cell (frame-ptrack tracker) row key))

(defmethod (setf ptrack-cell) (value (tracker tracker) row key &key)
  (setf (ptrack-cell (frame-ptrack tracker) row key) value))

(defmethod ptrack-cell ((tracker tracker) row key &key if-does-not-exist)
  (ptrack-cell (frame-ptrack tracker) row key))

(defgeneric ptrack-source-cell (ptrack row key) ;; FIX: don't defer if it doesn't exist?
  (:documentation "Get the text the user entered for ROW and KEY from PTRACK's metadata. If it doesn't exist, defer to `ptrack-cell'.

See also: `ptrack-cell', `ptrack'"))

(defmethod ptrack-source-cell ((tracker tracker) row key)
  (ptrack-source-cell (frame-ptrack tracker) row key))

(defmethod ptrack-source-cell (ptrack row key)
  (or (when-let* ((meta (pattern-metadata ptrack :source-code))
                  (found-row (ignore-errors
                              (elt meta (etypecase row
                                          (null 0)
                                          (number (1+ row))))))
                  (header (slot-value ptrack 'cl-patterns::header))
                  (key-index (/ (position key header) 2)))
        (nth key-index found-row))
      ;; FIX: in the future perhaps we can automatically prettify results from `ptrack-cell'? i.e. by lowercasing?
      (ptrack-cell ptrack row key)))

(defmethod (setf ptrack-source-cell) (value (tracker tracker) row key)
  (setf (ptrack-source-cell (frame-ptrack tracker) row key) value))

;; FIX: need to update this when rows are added/removed
(defmethod (setf ptrack-source-cell) (value (ptrack ptrack) row key)
  (format *debug-io* "setf ptrack-source-cell r ~s k ~s val ~s~%" row key value)
  (with-slots ((header cl-patterns::header) (rows cl-patterns::rows)) ptrack
    ;; generate the source-code metadata if it doesn't yet exist
    (unless (pattern-metadata ptrack :source-code)
      (setf (pattern-metadata ptrack :source-code) (make-list (1+ (length rows))))
      (dotimes (row (1+ (length rows)))
        (setf (nth row (pattern-metadata ptrack :source-code))
              (if (= 0 row)
                  (make-list (length header))
                  (make-list (/ (length header) 2))))))
    (if (member row (list :header :head :h nil))
        (setf (nth 0 (pattern-metadata ptrack :source-code))
              (cl-patterns::plist-set (nth 0 (pattern-metadata ptrack :source-code)) key value))
        (setf (nth (/ (position key header) 2) (nth (1+ row) (pattern-metadata ptrack :source-code))) value))))

(define-presentation-to-command-translator change-pattern
    (cell-pattern-id com-pattern-id tracker
     :pointer-documentation
     ((cell stream)
      (format stream "Change current pattern")))
    (object presentation)
  (list presentation))

(define-tracker-command (com-pattern-id)
    ((presentation 'cell-pattern-id))
  (format t "Changing patterns is not yet implemented.~%"))

;; (define-presentation-action insert-column )

(define-presentation-to-command-translator edit-cell
    (cell com-edit-cell tracker
          :tester
          ((cell)
           (with-slots (key row) cell
             (and (not (eql row :insert-column))
                  (not (null key)))))
          :pointer-documentation
          ((cell stream)
           (with-slots (key row) cell
             (if (null row)
                 (format stream "Edit ~s header" key)
                 (format stream "Edit ~a cell ~s" key row)))))
    (object)
  (list object))

(define-tracker-command (com-insert-column :name t :command-table tracker)
    ((index '(or integer cell-column-header) ;; :display-default (lambda () (concat (random 2)))
            ))
  (sprint 'com-insert-column)
  (sprint index))

(define-presentation-to-command-translator insert-column
    (cell-column-header com-insert-column tracker
                        :tester
                        ((cell)
                         (eql (slot-value cell 'row) :insert-column))
                        :pointer-documentation
                        ((cell stream)
                         (format stream "Insert new column")))
    (object)
  (list (with-slots (ptrack) *application-frame*
          (with-slots (row) object
            (case row
              (:insert-column
               (/ (length (slot-value ptrack 'cl-patterns::header)) 2))
              (nil 0)
              (t (1+ row)))))))

(define-tracker-command (com-insert-row :name t :command-table tracker)
    ((index '(or integer cell-row-number) ;; :display-default (lambda () (concat (random 2)))
            ))
  (sprint 'com-insert-row)
  (sprint index))

(define-presentation-to-command-translator insert-row
    (cell-row-number com-insert-row tracker
                     ;; :tester
                     ;; ((cell)
                     ;;  (eql (slot-value cell 'row) :insert-column))
                     :pointer-documentation
                     ((cell stream)
                      (format stream "Insert new row")))
    (object)
  (list (slot-value object 'row)))

(define-tracker-command (com-test :name t)
    ()
  (print (random 20)))

(define-command-table tracker-file-command-table
  :inherit-from (thundersnow-common-file-command-table)
  :inherit-menu t)

;; FIX: bind play to space only if the interactor is not selected/visible
(define-command (com-play :name t :menu t
                          :command-table tracker-file-command-table
                          ;; :keystroke (#\space)
                          )
    ()
  (play *application-frame*))

(define-command (com-set-pattern :name t :menu "Load Tracker Pattern"
                                 :command-table tracker-file-command-table
                                 :keystroke (#\o :control))
    ((pattern t));; (cell 'cell :gesture :select)
  (setf (frame-ptrack *application-frame*) (etypecase pattern
                                             (pattern pattern)
                                             (symbol (if (pdef-pattern pattern)
                                                         pattern
                                                         (error ""))))))

(define-command-table tracker-edit-command-table
  :inherit-from (thundersnow-common-edit-command-table)
  :inherit-menu t)

(define-command (com-edit-cell :name t :menu t
                               :command-table tracker-edit-command-table)
    ((cell 'cell))
  (setf tmp cell)
  (with-slots (key row) cell
    (let ((prompt (format nil "~a ~a" key row))
          string)
      (accepting-values ()
        (setf string (apply #'accept 'string
                            :view +cell-unparsed-text-view+
                            :prompt prompt ;; for some reason this doesn't work if i put the format here directly?
                            (when-let ((default (ptrack-source-cell *application-frame* (row cell) (key cell))))
                              (list :default default)))))
      (setf (content cell) (read-from-string string nil nil)))))

(define-command-table tracker-view-command-table
  :inherit-from (thundersnow-common-view-command-table)
  :inherit-menu t)

(define-command (com-toggle-interactor :name t :menu t
                                       :command-table tracker-view-command-table
                                       :keystroke (#\i :meta))
    ()
  (let ((pane (find-pane-named *application-frame* 'interactor)))
    (setf (sheet-enabled-p pane) (not (sheet-enabled-p pane)))
    ;; (if (sheet-enabled-p pane)
    ;;     (resize-sheet pane 300 300)
    ;;     (resize-sheet pane 0 0))
    ))

(define-command (com-next-layout :name t :menu t
                                 :command-table tracker-view-command-table
                                 :keystroke (#\l :meta))
    ()
  (let ((layouts (frame-all-layouts *application-frame*)))
    (setf (frame-current-layout *application-frame*) (elt-wrap layouts (1+ (position (frame-current-layout *application-frame*) layouts))))))

(define-command-table tracker-tools-command-table
  :inherit-from (thundersnow-common-tools-command-table)
  :inherit-menu t)

(define-command-table tracker-help-command-table
  :inherit-from (thundersnow-common-help-command-table)
  :inherit-menu t)

(defmethod play ((tracker tracker))
  (play (frame-ptrack tracker)))

(defun tracker (&optional ptrack)
  "Open a tracker."
  (apply #'make-or-find-application-frame 'tracker (when ptrack (list :ptrack ptrack))))

;;; testing out the layout switching bug(?)

(defun draw-foopy (frame stream)
  (present "foo" 'string :stream stream))

(define-application-frame foo ()
  ()
  (:panes
   (foopy :application :display-function 'draw-foopy)
   (interacty :interactor))
  (:layouts
   (default (vertically ()
              foopy interacty))
   (no-interactor foopy)))

(defmethod frame-standard-output ((frame foo))
  (or (find-pane-named frame 'interacty)
      (find-pane-named frame 'foopy)
      ))

(defmethod frame-standard-input ((frame foo))
  (or (find-pane-named frame 'interacty)
      (find-pane-named frame 'foopy)))

(define-foo-command (com-switch-layout :name t :menu t
                                       :keystroke (#\n :meta))
    ()
  (let ((layouts (frame-all-layouts *application-frame*)))
    (setf (frame-current-layout *application-frame*) (elt-wrap layouts (1+ (position (frame-current-layout *application-frame*) layouts))))))

(define-foo-command (com-foo-change :name t)
    ((string 'string :gesture :select))
  (notify-user *application-frame* "HEYO"))
