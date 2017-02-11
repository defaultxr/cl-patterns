(in-package :cl-patterns)

(defgeneric play-sc (item))

(defmethod play-sc ((item t))
  (unless (eq (get-event-value item :type) :rest)
    (let ((params (copy-list (event-plist item)))
          (time (+ (or (raw-get-event-value item :latency) 0.1) (or (raw-get-event-value item :unix-time-at-start) (sc:now))))
          (node nil))
      (remf params :instrument)
      (let ((node (sc:at time
                    (setf node (sc:synth (instrument item) params)))))
        (when (sc:has-gate-p node)
          (sc:at (+ time (dur-time (sustain item)))
            (sc:release node)))))))

(setf *event-output-function* 'play-sc)
