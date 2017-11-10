(in-package :cl-patterns)

(midihelper:start-midihelper)

(defparameter *midi-channels-instruments* (make-list 16 :initial-element 0))

(defun play-midi (item &optional task)
  (declare (ignore task))
  (unless (eq (get-event-value item :type) :rest)
    (let* ((channel 0) ;; (if (numberp (instrument item)) (instrument item) 0)
           (pgm (and (typep (instrument item) 'number)
                     (truncate (instrument item))))
           (note (truncate (get-event-value item :midinote)))
           (velocity (round (* 127 (get-event-value item :amp))))
           ;; (cbeats (slot-value (slot-value task 'clock) 'beats))
           (time (+ (or (raw-get-event-value item :latency) *latency*)
                    (or (raw-get-event-value item :unix-time-at-start) (sc:now)))))
      (bt:make-thread (lambda ()
                        (sleep (- time (unix-time-byulparan)))
                        (when (and pgm
                                   (not (= pgm (nth channel *midi-channels-instruments*))))
                          (midihelper:send-event (midihelper:ev-pgmchange channel pgm)))
                        (midihelper:send-event (midihelper:ev-noteon channel note velocity))
                        (sleep (dur-time (sustain item)))
                        (midihelper:send-event (midihelper:ev-noteoff channel note velocity)))))))

(setf *event-output-function* 'play-midi)
