;;;; backend.lisp - thundersnow registers itself as a cl-patterns backend so it can receive information about active tasks.

(in-package #:thundersnow/common)

(defclass thundersnow-backend (backend)
  ((cl-patterns::name :initform "thundersnow"))
  (:documentation "cl-patterns thundersnow backend."))

(defmethod cl-patterns::backend-tempo-change-at ((backend thundersnow-backend) clock timestamp)
  (update-tempo-information (tempo clock)))

(defmethod cl-patterns::backend-task-added ((backend thundersnow-backend) task)
  (map-over-frames (lambda (frame)
                     (backend-task-added frame task))))

(defmethod cl-patterns::backend-task-removed ((backend thundersnow-backend) task)
  (map-over-frames (lambda (frame)
                     (backend-task-removed frame task))))

(defmethod cl-patterns::backend-play-event ((backend thundersnow-backend) event task)
  (map-over-frames (lambda (frame)
                     (backend-task-play-event frame task event))))

(defmethod cl-patterns::backend-timestamps-for-event ((backend thundersnow-backend) event task)
  (let* ((beat (+ (beat event) (beat task)))
         (ebeat (+ beat (dur event))))
    (list (clp::absolute-beats-to-timestamp beat (clp::task-clock task))
          (clp::absolute-beats-to-timestamp ebeat (clp::task-clock task)))))

(backend-start 'thundersnow-backend)
