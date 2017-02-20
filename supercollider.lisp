(in-package :cl-patterns)

(defgeneric play-sc (item))

(defmethod play-sc ((item t))
  (unless (eq (get-event-value item :type) :rest)
    (let ((params (copy-list (event-plist item)))
          (time (+ (or (raw-get-event-value item :latency) *latency*) (or (raw-get-event-value item :unix-time-at-start) (sc:now)))))
      (remf params :instrument)
      ;; FIX: need to also auto-convert event parameters to the proper ones if they exist in the synthdef. i.e. convert legato to sustain if synthdef has a 'sustain' arg.
      ;; FIX: auto-convert bufnum.
      (let ((node (sc:at time
                    (sc:synth (instrument item) params))))
        (when (sc:has-gate-p node)
          (sc:at (+ time (dur-time (sustain item)))
            (sc:release node)))))))

(setf *event-output-function* 'play-sc)
