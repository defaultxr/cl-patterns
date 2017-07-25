(in-package :cl-patterns)

(midihelper::start-midihelper)

(defgeneric play-midi (item))

(defmethod play-midi ((item t))
  (unless (eq (get-event-value item :type) :rest)
    (let* ((channel (if (numberp (instrument item)) (instrument item) 0))
           (note (truncate (get-event-value item :midinote)))
           (velocity (round (* 127 (get-event-value item :amp))))
           ;; (time (+ (or (raw-get-event-value item :latency) *latency*) (or (raw-get-event-value item :unix-time-at-start) (sc:now))))
           )
      (midihelper::send-event (midihelper::ev-noteon channel note velocity))
      (sleep (dur-time (sustain item))) ;; FIX: definitely don't do this.
      (midihelper::send-event (midihelper::ev-noteoff channel note velocity)))))

(setf *event-output-function* 'play-midi)
