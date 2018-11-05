(in-package :cl-patterns)

(midihelper:start-midihelper)

;;; utility functions

(defun midi-truncate-clamp (number &optional (max 127))
  "Truncate NUMBER and clamp it to the range 0..MAX (default 127)."
  (declare (number number))
  (alexandria:clamp (truncate number) 0 max))

(defun bipolar-1-to-midi (number)
  "Convert the range -1..1 to 0..127."
  (alexandria:clamp (ceiling (* 63.5 (1+ number))) 0 127))

(defun unipolar-1-to-midi (number)
  "Convert the range 0..1 to 0..127."
  (alexandria:clamp (round (* 127 number)) 0 127))

(defun frequency-to-midi (frequency)
  "Convert FREQUENCY to a MIDI note number (rounding to ensure it's an integer).

Note that this function is meant for use with the MIDI backend; for frequency-to-midinote conversion without rounding, see `freq-midinote' instead."
  (round (freq-midinote frequency)))

(defun midi-panic (&optional channel)
  "Stop all notes on CHANNEL, or all channels if CHANNEL is nil."
  (loop :for c :in (or (alexandria:ensure-list channel) (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
     :do (midihelper:send-event (midihelper:ev-cc c 123 0))))

;;; cc mapping
;; http://nickfever.com/music/midi-cc-list

(defparameter *midi-channels-instruments* (make-list 16 :initial-element 0))

(defparameter *cc-table* (make-hash-table)
  "Hash table mapping CC numbers to metadata and event key names to CC numbers.")

(defun set-cc-mapping (cc-number description &optional event-key (mapping 'identity))
  "Set a mapping for CC-NUMBER. When EVENT-KEY is seen in an event being played by the MIDI backend, its value will be converted using the function specified by MAPPING, and then that value will be set for CC number CC-NUMBER just before the note itself is triggered. DESCRIPTION is a description of what the CC controls."
  (setf (gethash cc-number *cc-table*)
        (list cc-number description event-key mapping))
  (when event-key
    (setf (gethash event-key *cc-table*)
          cc-number)))

(defun get-cc-mapping (key)
  "Return the CC mapping parameters for a CC number or an event key."
  (let ((key-map (gethash key *cc-table*)))
    (if (numberp key-map)
        (gethash key-map *cc-table*)
        key-map)))

;; set default cc mappings
(map nil (lambda (x) (apply 'set-cc-mapping x))
     '((1 "Vibrato/Modulation" :vibrato unipolar-1-to-midi)
       (8 "Balance" :balance bipolar-1-to-midi)
       (10 "Pan" :pan bipolar-1-to-midi)
       (71 "Resonance/Timbre" :res unipolar-1-to-midi)
       (72 "Release" :release midi-truncate-clamp)
       (73 "Attack" :attack midi-truncate-clamp)
       (74 "Cutoff/Brightness" :ffreq frequency-to-midi)
       (84 "Portamento amount" :porta midi-truncate-clamp)
       (91 "Reverb" :reverb midi-truncate-clamp)
       (92 "Tremolo" :tremolo midi-truncate-clamp)
       (93 "Chorus" :chorus midi-truncate-clamp)
       (94 "Phaser" :phaser midi-truncate-clamp)))

;;; backend functions

(defun is-midi-event-p (event)
  (or (eq (event-value event :type) :midi)
      (typep (event-value event :instrument) 'number)))

(defun play-midi (event &optional task)
  (declare (ignore task))
  (let* ((channel (midi-truncate-clamp (or (event-value event :channel) 0) 15))
         (pgm (midi-truncate-clamp (event-value event :instrument)))
         (note (midi-truncate-clamp (event-value event :midinote)))
         (velocity (unipolar-1-to-midi (event-value event :amp))) ;; FIX: maybe this shouldn't be linear?
         (time (local-time:timestamp+ (or (raw-event-value event :timestamp-at-start) (local-time:now))
                                      (truncate (* (or (raw-event-value event :latency) *latency*) 1000000000))
                                      :nsec))
         (extra-params (loop :for key :in (keys event)
                          :for cc-mapping = (get-cc-mapping key)
                          :if cc-mapping
                          :collect (list (car cc-mapping) (funcall (nth 3 cc-mapping) (event-value event key))))))
    (bt:make-thread (lambda ()
                      (sleep (local-time:timestamp-difference time (local-time:now)))
                      (when (and pgm
                                 (not (= pgm (nth channel *midi-channels-instruments*))))
                        (midihelper:send-event (midihelper:ev-pgmchange channel pgm)))
                      (loop :for i :in extra-params
                         :do (midihelper:send-event (midihelper:ev-cc channel (car i) (cadr i))))
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

(export '(midi-truncate-clamp bipolar-1-to-midi unipolar-1-to-midi frequency-to-midi midi-panic set-cc-mapping get-cc-mapping))
