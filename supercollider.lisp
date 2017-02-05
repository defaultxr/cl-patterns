(in-package :cl-patterns)

(defgeneric play-sc (item))

(defmethod play-sc ((item t))
  (unless (eq (get-event-value item :type) :rest)
    (let ((params (copy-list (event-plist item))))
      (remf params :instrument)
      (sc:at (+ (or (raw-get-event-value item :latency) 0.1) (or (raw-get-event-value item :unix-time-at-start) (sc:now)))
        (sc:synth (instrument item) params)))))

(setf *event-output-function* 'play-sc)
