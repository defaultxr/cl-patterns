(in-package :cl-patterns)

(midihelper:start-midihelper)

(defparameter *midi-channels-instruments* (make-list 16 :initial-element 0))

;;; backend functions

(defun is-midi-event-p (event)
  (or (eq (event-value event :type) :midi)
      (typep (event-value event :instrument) 'number)))

(defun play-midi (event &optional task)
  (declare (ignore task))
  (let* ((channel (alexandria:clamp (or (event-value event :channel)
                                        0)
                                    0 15))
         (pgm (alexandria:clamp (and (typep (event-value event :instrument) 'number)
                                     (truncate (event-value event :instrument)))
                                0 127))
         (pan (alexandria:clamp (ceiling (* 63.5 (1+ (event-value event :pan)))) 0 127))
         (note (alexandria:clamp (truncate (event-value event :midinote)) 0 127))
         (velocity (alexandria:clamp (round (* 127 (event-value event :amp))) 0 127)) ;; FIX: maybe this shouldn't be linear?
         (time (local-time:timestamp+ (or (raw-event-value event :timestamp-at-start) (local-time:now))
                                      (truncate (* (or (raw-event-value event :latency) *latency*) 1000000000))
                                      :nsec)))
    (bt:make-thread (lambda ()
                      (sleep (local-time:timestamp-difference time (local-time:now)))
                      (when (and pgm
                                 (not (= pgm (nth channel *midi-channels-instruments*))))
                        (midihelper:send-event (midihelper:ev-pgmchange channel pgm)))
                      (midihelper:send-event (midihelper:ev-cc channel 10 pan))
                      (midihelper:send-event (midihelper:ev-noteon channel note velocity))
                      (sleep (dur-time (sustain event)))
                      (midihelper:send-event (midihelper:ev-noteoff channel note velocity)))
                    :name "cl-patterns temporary midi note thread")))

(defun release-midi (&rest foo)
  (declare (ignore foo))
  ;; FIX
  nil
  )

(defun release-midi-at (&rest foo)
  (declare (ignore foo))
  ;; FIX
  nil
  )

(defun timestamp-to-midi (timestamp)
  (declare (ignore timestamp))
  ;; FIX
  nil
  )

(register-backend :midi
                  :respond-p 'is-midi-event-p
                  :play 'play-midi
                  :release 'release-midi
                  :release-at 'release-midi-at)

(enable-backend :midi)
