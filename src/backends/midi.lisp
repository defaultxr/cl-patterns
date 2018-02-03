(in-package :cl-patterns)

(midihelper:start-midihelper)

(defparameter *midi-channels-instruments* (make-list 16 :initial-element 0))

;; backend functions

(defun is-midi-event-p (event)
  (or (eq (event-value event :type) :midi)
      (typep (event-value event :instrument) 'number)))

(defun play-midi (event &optional task)
  (declare (ignore task))
  (let* ((channel (or (event-value event :channel)
                      0))
         (pgm (and (typep (event-value event :instrument) 'number)
                   (truncate (event-value event :instrument))))
         (note (truncate (event-value event :midinote)))
         (velocity (round (* 127 (event-value event :amp)))) ;; FIX: maybe this shouldn't be linear
         (time (local-time:timestamp+ (or (raw-get-event-value event :timestamp-at-start) (local-time:now))
                                      (truncate (* (or (raw-get-event-value event :latency) *latency*) 1000000000))
                                      :nsec)))
    (bt:make-thread (lambda ()
                      (sleep (local-time:timestamp-difference time (local-time:now)))
                      (when (and pgm
                                 (not (= pgm (nth channel *midi-channels-instruments*))))
                        (midihelper:send-event (midihelper:ev-pgmchange channel pgm)))
                      (midihelper:send-event (midihelper:ev-noteon channel note velocity))
                      (sleep (dur-time (sustain event)))
                      (midihelper:send-event (midihelper:ev-noteoff channel note velocity))))))

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
                  :respond-p #'is-midi-event-p
                  :play #'play-midi
                  :release #'release-midi
                  :release-at #'release-midi-at
                  :timestamp-conversion #'timestamp-to-midi)

(enable-backend :midi)
