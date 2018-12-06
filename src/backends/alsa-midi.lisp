(in-package :cl-patterns)

;; FIX: add bend?

(midihelper:start-midihelper)

;;; utility functions

(defun alsa-midi-panic (&optional channel)
  "Stop all notes on CHANNEL, or all channels if CHANNEL is nil."
  (loop :for c :in (or (alexandria:ensure-list channel) (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
     :do (midihelper:send-event (midihelper:ev-cc c 123 0))))

;;; cc mapping
;; http://nickfever.com/music/midi-cc-list

(defparameter *alsa-midi-channels-instruments* (make-list 16 :initial-element 0))

(defparameter *alsa-midi-cc-table* (make-hash-table)
  "Hash table mapping CC numbers to metadata and event key names to CC numbers.")

(defun set-alsa-midi-cc-mapping (cc-number description &optional event-key (mapping 'midi-truncate-clamp))
  "Set a mapping for CC-NUMBER. When EVENT-KEY is seen in an event being played by the MIDI backend, its value will be converted using the function specified by MAPPING, and then that value will be set for CC number CC-NUMBER just before the note itself is triggered. DESCRIPTION is a description of what the CC controls."
  (setf (gethash cc-number *alsa-midi-cc-table*)
        (list cc-number description event-key mapping))
  (when event-key
    (setf (gethash event-key *alsa-midi-cc-table*)
          cc-number)))

(defun get-alsa-midi-cc-mapping (key)
  "Return the CC mapping parameters for a CC number or an event key."
  (let ((key-map (gethash key *alsa-midi-cc-table*)))
    (if (numberp key-map)
        (gethash key-map *alsa-midi-cc-table*)
        key-map)))

;; set default cc mappings
(map nil (lambda (x) (apply 'set-alsa-midi-cc-mapping x))
     '((1 "Vibrato/Modulation" :vibrato unipolar-1-to-midi)
       (8 "Balance" :balance bipolar-1-to-midi)
       (10 "Pan" :pan bipolar-1-to-midi)
       (71 "Resonance/Timbre" :res unipolar-1-to-midi)
       (72 "Release" :release)
       (73 "Attack" :attack)
       (74 "Cutoff/Brightness" :ffreq frequency-to-midi)
       (84 "Portamento amount" :porta)
       (91 "Reverb" :reverb)
       (92 "Tremolo" :tremolo)
       (93 "Chorus" :chorus)
       (94 "Phaser" :phaser)))

;;; backend functions

(defun is-alsa-midi-event-p (event)
  (or (eq (event-value event :type) :midi)
      (typep (event-value event :instrument) 'number)))

(defun play-alsa-midi (event &optional task)
  (let* ((channel (midi-truncate-clamp (or (event-value event :channel) 0) 15))
         (pgm (midi-truncate-clamp (event-value event :instrument)))
         (note (midi-truncate-clamp (event-value event :midinote)))
         (velocity (unipolar-1-to-midi (event-value event :amp))) ;; FIX: maybe this shouldn't be linear?
         (time (local-time:timestamp+ (or (raw-event-value event :timestamp-at-start) (local-time:now))
                                      (truncate (* (or (raw-event-value event :latency) *latency*) 1000000000))
                                      :nsec))
         (extra-params (loop :for key :in (keys event)
                          :for cc-mapping = (get-alsa-midi-cc-mapping key)
                          :if cc-mapping
                          :collect (list (car cc-mapping) (funcall (nth 3 cc-mapping) (event-value event key))))))
    (bt:make-thread (lambda ()
                      (sleep (local-time:timestamp-difference time (local-time:now)))
                      (when (and pgm
                                 (not (= pgm (nth channel *alsa-midi-channels-instruments*))))
                        (midihelper:send-event (midihelper:ev-pgmchange channel pgm)))
                      (loop :for i :in extra-params
                         :do (midihelper:send-event (midihelper:ev-cc channel (car i) (cadr i))))
                      (midihelper:send-event (midihelper:ev-noteon channel note velocity))
                      (sleep (dur-time (sustain event) (tempo (slot-value task 'clock))))
                      (midihelper:send-event (midihelper:ev-noteoff channel note velocity)))
                    :name "cl-patterns temporary alsa midi note thread")))

(defun release-alsa-midi (&rest foo)
  (declare (ignore foo))
  ;; FIX
  nil
  )

(defun release-alsa-midi-at (&rest foo)
  (declare (ignore foo))
  ;; FIX
  nil
  )

(defun timestamp-to-alsa-midi (timestamp) ;; FIX: remove?
  (declare (ignore timestamp))
  ;; FIX
  nil
  )

(register-backend :alsa-midi
                  :respond-p 'is-alsa-midi-event-p
                  :play 'play-alsa-midi
                  :release 'release-alsa-midi
                  :release-at 'release-alsa-midi-at)

(enable-backend :alsa-midi)

(export '(alsa-midi-panic set-alsa-midi-cc-mapping get-alsa-midi-cc-mapping))
