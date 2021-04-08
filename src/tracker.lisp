(in-package #:thundersnow/tracker)

;;; misc

(defparameter tmp nil)

;;; utilities (FIX):

(defun plist-value (plist key)
  "Get the value of KEY in PLIST. Returns the key the value was derived from as a second value."
  (loop :for (k v) :on plist :by #'cddr
        :if (eql k key)
          :return (values v k)
        :finally (return-from plist-value (values nil nil))))

(defvar loop-detector nil)

(defun c (h w)
  (if (not loop-detector)
      (error "No loop-detector in dynamic environment.")
      (if (aref loop-detector h w)
          (error "Evaluation loop detected.")
          (progn
            (setf (aref loop-detector h w) t)
            (eval (content (aref (frame-ptracker *application-frame*) h w)))))))

;;; gui

(defclass cell ()
  ((frame :initarg :frame :accessor frame)
   (row :initarg :row :initform nil :accessor row)
   (key :initarg :key :initform nil :accessor key)
   (content :initarg :content)))

(defmethod print-object ((cell cell) stream)
  (with-slots (row key) cell
    (print-unreadable-object (cell stream :type t)
      (format stream ":row ~s :key ~s" row key))))

(defun make-cell (&rest args)
  (apply #'make-instance 'cell args))

(defmethod content ((cell cell))
  (with-slots (frame row key) cell
    (if (and row key)
        (ptracker-cell frame row key)
        (when (slot-boundp cell 'content)
          (slot-value cell 'content)))))

(defmethod (setf content) (value (cell cell))
  (with-slots (frame row key) cell
    (let ((value (typecase value
                   (string value)
                   (t (write-to-string value)))))
      (if (and row key)
          (setf (ptracker-source-cell (frame-ptracker frame) row key) value)
          (warn "Attempted to set ~s's content to ~s but no row or key set." cell value)))))

(define-presentation-type cell ())

(define-presentation-method present (cell (type cell) stream (view textual-view) &key)
  (with-accessors ((content content)) cell
    (format stream "~a" (if content content "     "))))

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

(define-presentation-method accept ((type cell) stream (view cell-unparsed-text) &key)
  (let ((cell (make-cell))) ;; FIX?
    (setf (content cell) (read stream))))

;;; pattern drawing

(defun draw-track (frame pane)
  "Draw the contents of `tracker'."
  (let ((ptracker (frame-ptracker frame)))
    (labels ((gen-color (column)
               (case column
                 (0
                  +gray+)
                 (t
                  (make-ihs-color 1 (mod (/ (1- column) 12) 1.0) 0.9))))
             (cell-color (header-p column)
               (let ((color
                       (if-let ((meta (pattern-metadata (frame-ptracker (find-application-frame 'tracker)) :columns)))
                         (or (ignore-errors (elt meta column))
                             (gen-color column))
                         (gen-color column))))
                 (if header-p
                     color
                     (clim-internals::highlight-shade color)))))
      (with-slots ((header cl-patterns::header) (rows cl-patterns::rows)) ptracker
        (let ((keys (keys header)))
          (formatting-table (pane)
            (formatting-row (pane)
              (with-border (+black+ 1 :ink +white+)
                (formatting-cell (pane)
                  (present (make-instance 'cell-pattern-id :frame frame :content "p 0") 'cell-pattern-id :stream pane)))
              ;; column headers
              (let ((col 1))
                (doplist (key value header)
                  (prog1
                      (with-border ((cell-color t col))
                        (formatting-cell (pane)
                          (present (make-instance 'cell-column-header :frame frame :content (format nil "~s ~s" key value) :key key) 'cell-column-header :stream pane)))
                    (incf col)))
                ;; new column button
                (with-border ((cell-color t col))
                  (formatting-cell (pane)
                    (present (make-instance 'cell-column-header :frame frame :content "+" :row :insert-column) 'cell-column-header :stream pane)))))
            ;; FIX: alternate color for every N rows, where N is determined by the beats per bar
            (dolist* (row row-num rows)
              (declare (ignore row))
              (formatting-row (pane)
                ;; row numbers
                (with-border ((cell-color nil 0))
                  (formatting-cell (pane)
                    (present (make-instance 'cell-row-number :frame frame :content row-num :row row-num) 'cell-row-number :stream pane)))
                ;; ptracker cells
                (dolist* (key col-num keys)
                  (with-border ((cell-color nil (1+ col-num)) (random-range 1 5))
                    (formatting-cell (pane)
                      (present (make-cell :frame frame :row row-num :key key) 'cell :stream pane)))))))))
      (fresh-line))))

(define-application-frame tracker ()
  ((ptracker :initarg :ptracker
             ;; :accessor frame-ptracker
             :initform (ptracker (list :degree (pseries 0 (pwhite 0 4) 16) :dur 1/4)
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

(defmethod frame-ptracker ((symbol symbol))
  (frame-ptracker (find-application-frame symbol)))

(defmethod frame-ptracker ((frame application-frame))
  (slot-value frame 'ptracker))

(defmethod (setf frame-ptracker) (value (frame tracker))
  (setf (slot-value frame 'ptracker) (etypecase value
                                       (symbol (if (pdef-pattern value)
                                                   value
                                                   (error "No pdef found with the name ~s" value))))))

(defmethod frame-standard-output ((frame tracker))
  (find-pane-named frame 'interactor))

(define-command-table enhanced-accept-values :inherit-from 'accept-values)

;;; ptracker methods

(defgeneric ptracker-cell (ptracker row &optional key)
  (:documentation "Get the text of a cell from ptracker. Can be used on a ptracker-pstream too to get the generated values from it. Returns four values:

- the value at that row/key
- the name of the key that the specified KEY was derived from, nil if it was not found, or t if it was not provided
- whether the row exists ;; FIX: doesn't work if the row does exist
- whether the output was derived from the a header pattern, or the row itself ;; FIX: doesn't work at all yet

See also: `ptracker-source-cell', `ptracker'"))

(defmethod ptracker-cell ((tracker tracker) row &optional key)
  (apply #'ptracker-cell (frame-ptracker tracker) row (when key (list key))))

(defmethod ptracker-cell ((ptracker ptracker) row &optional key)
  (etypecase row
    (symbol
     (ecase row
       ((:header :head :h nil)
        (with-slots ((header cl-patterns::header)) ptracker
          (plist-value header key)))))
    (integer
     (with-slots ((rows cl-patterns::rows)) ptracker
       (if-let ((row (elt rows row)))
         (plist-value row key)
         (values nil nil nil))))))

(defmethod ptracker-cell ((ptracker ptracker-pstream) row &optional key)
  (etypecase row
    (symbol
     (ptracker-cell ptracker nil row))
    (integer
     (if-let ((event (elt ptracker row)))
       (if key
           (event-value event key)
           event)
       (values nil nil nil)))))

(defmethod ptracker-cell ((ptracker ptracker-pstream) row &optional key)
  (etypecase row
    (symbol
     (ptracker-cell ptracker nil row))
    (integer
     (if-let ((event (elt ptracker row)))
       (if key
           (event-value event key)
           event)
       (values nil nil nil)))))

(defgeneric (setf ptracker-cell) (value ptracker row &optional key))

(defmethod (setf ptracker-cell) (value ptracker row &optional key)
  (if (not (member row (list :header :head :h nil)))
      (if key
          (with-slots ((rows cl-patterns::rows)) ptracker
            (setf (nth row rows) (cl-patterns::plist-set (elt rows row) key value)))
          (error "KEY is currently required."))
      (with-slots ((header cl-patterns::header)) ptracker
        (if key
            (setf (getf header key) value)
            (setf header value)))))

(defgeneric ptracker-source-cell (ptracker row key)
  (:documentation "Get the text the user entered for ROW and KEY from PTRACKER's metadata. If it doesn't exist, defer to `ptracker-cell'.

See also: `ptracker-cell', `ptracker'"))

(defmethod ptracker-source-cell ((tracker tracker) row key)
  (ptracker-source-cell (frame-ptracker tracker) row key))

(defmethod ptracker-source-cell (ptracker row key)
  (or (when-let* ((meta (pattern-metadata ptracker :source-code))
                  (found-row (ignore-errors
                              (elt meta (etypecase row
                                          (null 0)
                                          (number (1+ row))))))
                  (header (slot-value ptracker 'cl-patterns::header))
                  (key-index (/ (position key header) 2)))
        (nth key-index found-row))
      ;; FIX: in the future perhaps we can automatically prettify results from `ptracker-cell'? i.e. by lowercasing?
      (ptracker-cell ptracker row key)))

(defmethod (setf ptracker-source-cell) (value (tracker tracker) row key)
  (setf (ptracker-source-cell (frame-ptracker tracker) row key) value))

;; FIX: need to update this when rows are added/removed
(defmethod (setf ptracker-source-cell) (value (ptracker ptracker) row key)
  (format *debug-io* "setf ptracker-source-cell r ~s k ~s val ~s~%" row key value)
  (with-slots ((header cl-patterns::header) (rows cl-patterns::rows)) ptracker
    ;; generate the source-code metadata if it doesn't yet exist
    (unless (pattern-metadata ptracker :source-code)
      (setf (pattern-metadata ptracker :source-code) (make-list (1+ (length rows))))
      (dotimes (row (1+ (length rows)))
        (setf (nth row (pattern-metadata ptracker :source-code))
              (if (= 0 row)
                  (make-list (length header))
                  (make-list (/ (length header) 2))))))
    (if (member row (list :header :head :h nil))
        (setf (nth 0 (pattern-metadata ptracker :source-code))
              (cl-patterns::plist-set (nth 0 (pattern-metadata ptracker :source-code)) key value))
        (setf (nth (/ (position key header) 2) (nth (1+ row) (pattern-metadata ptracker :source-code))) value))
    (setf (ptracker-cell ptracker row key) (read-from-string value))))

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
  (list (with-slots (ptracker) *application-frame*
          (with-slots (row) object
            (case row
              (:insert-column
               (/ (length (slot-value ptracker 'cl-patterns::header)) 2))
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
  (setf (frame-ptracker *application-frame*) (etypecase pattern
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
  (sprint 'com-edit-cell)
  (setf tmp cell)
  (with-slots (key row) cell
    (let ((prompt (format nil "~a ~a" key row))
          string)
      (accepting-values ()
        (setf string (apply #'accept 'string
                            ;; :view +cell-unparsed-text-view+
                            :prompt prompt ;; for some reason this doesn't work if i put the format here directly?
                            (when-let ((default (ptracker-source-cell *application-frame* (row cell) (key cell))))
                              (list :default default)))))
      (sprint 'here)
      (sprint string)
      (setf (content cell) (if (equal "" string)
                               (progn
                                 (sprint 'fuck)
                                 nil)
                               (progn
                                 (sprint 'you)
                                 string))))))

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
  (play (frame-ptracker tracker)))

(defun tracker (&optional ptracker)
  "Open a tracker."
  (apply 'find-application-frame 'tracker (when ptracker (list :ptracker ptracker))))

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


