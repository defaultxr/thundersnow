;;;; status-pane.lisp - the status pane gadget and associated functionality to generate its output.

(in-package #:thundersnow/common)

;;; utility

(defun system-uptime ()
  "Returns system uptime information as provided by the OS's \"uptime\" command. Returns a plist containing the following values:

- system uptime as a string
- number of users
- 1 minute load average
- 5 minute load average
- 15 minute load average"
  (flet ((trim-spaces (string)
           (string-trim (list #\space) string)))
    (let* ((uptime-output (uiop:run-program "uptime" :output '(:string :stripped t)))
           (up-loc (search " up " uptime-output))
           (users-loc (search " users," uptime-output))
           (uptime-end (position #\comma uptime-output :from-end t :end users-loc))
           (load-average-loc (search "load average: " uptime-output))
           ;; (time (subseq uptime-output 0 up-loc))
           (uptime (subseq uptime-output (+ 4 up-loc) uptime-end))
           (users (subseq uptime-output (1+ uptime-end) users-loc))
           (load-averages (mapcar (rcurry #'parse-float:parse-float :junk-allowed t)
                                  (string-split (subseq uptime-output (+ 14 load-average-loc))
                                                :char-bag #\comma))))
      `(:uptime ,(trim-spaces uptime)
        :users ,(parse-integer users :junk-allowed t)
        :1-minute-load ,(nth 0 load-averages)
        :5-minute-load ,(nth 1 load-averages)
        :15-minute-load ,(nth 2 load-averages)))))

(defun server-status-text ()
  "Generate the server status text string."
  (let* ((backend (find-backend 'supercollider))
         (ugens (backend-num-ugens backend))
         (synths (backend-num-synths backend))
         (groups (backend-num-groups backend))
         (definitions (backend-num-definitions backend))
         (cpu (getf (system-uptime) :1-minute-load)))
    (format nil "u: ~D s: ~D g: ~D d: ~D~%cpu: ~$%" ugens synths groups definitions cpu)))

(defclass status-pane (basic-gadget)
  ())

(defmethod initialize-instance :after ((pane status-pane) &key &allow-other-keys)
  (setf (medium-background pane) (theme-color :background)))

(defmethod activate-gadget :after ((status-pane status-pane))
  (clime:schedule-event status-pane (make-instance 'timer-event :sheet status-pane) 1))

(defmethod handle-repaint ((pane status-pane) region)
  (declare (ignore region))
  (draw-text pane (server-status-text) (bounding-rectangle-center pane) :align-x :center :align-y :center))

(defmethod handle-event ((pane status-pane) (event timer-event))
  (handle-repaint pane (sheet-region pane))
  (when (gadget-active-p pane)
    (clime:schedule-event pane (make-instance 'timer-event :sheet pane) 1)))

;; (defmethod handle-event ((pane status-pane) (event climi::pointer-button-press-event))
;;   (if *clock*
;;       (execute-frame-command *application-frame* (list 'com-set-tempo))
;;       (start-clock-loop :tempo 110/60)))

;; (defmethod handle-event ((pane status-pane) (event climi::pointer-scroll-event))
;;   (when *clock*
;;     (incf (tempo *clock*) (* -1/600 (slot-value event 'climi::delta-y)))))
