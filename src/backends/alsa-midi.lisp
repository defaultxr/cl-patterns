;;;; alsa-midi.lisp - backend to output MIDI via ALSA.
;; FIX: add bend?
;; FIX: allow non-midinote frequencies to be sent as midinote+midi detune (i.e. auto-convert :freq to those keys when enabled)

(in-package #:cl-patterns)

(defclass alsa-midi (backend)
  ((name :initform "ALSA MIDI")))

;;; utility functions

(defun alsa-midi-panic (&key channel manually-free)
  "Stop all notes on CHANNEL, or all channels if CHANNEL is nil."
  (dolist (c (or (ensure-list channel) (iota 16)))
    (if manually-free
        (dolist (note (iota 128))
          (midihelper:send-event (midihelper:ev-noteoff c note 0)))
        (midihelper:send-event (midihelper:ev-cc c 123 0)))))

;;; instrument mapping

(defvar *alsa-midi-instrument-map* (make-hash-table :test #'equal)
  "Hash table mapping instrument names (as symbols) to MIDI program numbers. Any events whose :instrument is not a number and not found in this table will be ignored by the backend.")

(defun alsa-midi-instrument-program-number (instrument)
  "Get INSTRUMENT's program number according to `*alsa-midi-instrument-map*', or nil if INSTRUMENT was not found in the map. If INSTRUMENT is a number, coerce it to a MIDI-compatible integer (0-127)."
  (when-let ((num (typecase instrument
                    (number instrument)
                    (symbol (gethash instrument *alsa-midi-instrument-map*)))))
    (midi-truncate-clamp num)))

(defun (setf alsa-midi-instrument-program-number) (value instrument)
  "Set a mapping from INSTRUMENT (an instrument name as a string or symbol) to a MIDI program number. Setting an instrument to nil with this function removes it from the map.

See also: `alsa-midi-instrument-program-number'"
  (check-type value (or null (integer 0 127)))
  (if value
      (setf (gethash instrument *alsa-midi-instrument-map*) value)
      (remhash instrument *alsa-midi-instrument-map*)))

;;; cc mapping
;; http://nickfever.com/music/midi-cc-list

(defvar *alsa-midi-cc-map* (make-hash-table)
  "Hash table mapping CC numbers to metadata and event key names to CC numbers.")

(defun alsa-midi-auto-cc-mapping (key)
  "Get auto-generated CC mapping parameters for a KEY of the format \"CC-N\", where N is a valid CC number."
  (let ((sym-name (symbol-name key)))
    (when (and (>= (length sym-name) 3)
               (string= "CC-" sym-name :end2 3))
      (let ((num (parse-integer (subseq sym-name 2) :junk-allowed t)))
        (when (and (integerp num)
                   (<= 0 num 127))
          (list num (format nil "CC ~S" num) key 'identity))))))

(defun alsa-midi-cc-mapping (key)
  "Get the CC mapping parameters for a CC number or an event key."
  (let ((key-map (or (gethash key *alsa-midi-cc-map*)
                     (alsa-midi-auto-cc-mapping key))))
    (if (numberp key-map)
        (gethash key-map *alsa-midi-cc-map*)
        key-map)))

(uiop:with-deprecation (:warning)
  (defun get-alsa-midi-cc-mapping (key)
    "Deprecated in favor of `alsa-midi-cc-mapping'."
    (alsa-midi-cc-mapping key)))

(defun alsa-midi-set-cc-mapping (cc-number description &optional event-key (mapper 'midi-truncate-clamp))
  "Set a mapping for CC-NUMBER. When EVENT-KEY is seen in an event being played by the alsa-midi backend, its value will be converted using the function specified by MAPPER, and then that value will be set for CC number CC-NUMBER just before the note itself is triggered. DESCRIPTION is a description of what the CC controls."
  (setf (gethash cc-number *alsa-midi-cc-map*)
        (list cc-number description event-key mapper))
  (dolist (event-key (ensure-list event-key))
    (setf (gethash event-key *alsa-midi-cc-map*) cc-number)))

(uiop:with-deprecation (:warning)
  (defun set-alsa-midi-cc-mapping (cc-number description &optional event-key (mapper 'midi-truncate-clamp))
    "Deprecated in favor of `alsa-midi-set-cc-mapping'."
    (alsa-midi-set-cc-mapping cc-number description event-key mapper)))

(defun alsa-midi-remap-key-value (key value)
  "Remap KEY and VALUE to their equivalent MIDI CC parameter and range. Returns a list of the format (CC-NUMBER CC-VALUE), or nil if the key is not a recognized CC key."
  (if-let ((cc-mapping (alsa-midi-cc-mapping key)))
    (list (car cc-mapping) (funcall (nth 3 cc-mapping) value))
    (let ((sym-name (symbol-name key)))
      (if (and (>= (length sym-name) 3)
               (string= "CC-" sym-name :end2 3))
          (list (midi-truncate-clamp (parse-integer (subseq sym-name 3))) (midi-truncate-clamp value))
          nil))))

;; set default cc mappings
(mapc (fn (apply #'alsa-midi-set-cc-mapping _))
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

;;; current channel instruments

(defvar *alsa-midi-channels-instruments* (make-list 16)
  "List mapping MIDI channel numbers to the program number currently playing on them.")

;;; backend functions

(defmethod backend-start ((backend alsa-midi) &key)
  (unless (elt (midihelper:inspect-midihelper) 5)
    (midihelper:start-midihelper)))

(defmethod backend-stop ((backend alsa-midi))
  (midihelper:stop-midihelper))

(defmethod backend-instrument-controls ((backend alsa-midi) instrument)
  (remove-if #'numberp (keys *alsa-midi-cc-map*)))

(defmethod backend-all-instruments ((backend alsa-midi))
  (keys *alsa-midi-instrument-map*))

(defmethod backend-all-nodes ((backend alsa-midi))
  nil)

(defmethod backend-node-p ((backend alsa-midi) object)
  nil)

(defmethod backend-panic ((backend alsa-midi))
  (alsa-midi-panic))

(defmethod backend-timestamps-for-event ((backend alsa-midi) event task)
  nil)

(defmethod backend-proxys-node ((backend alsa-midi) id)
  nil)

(defmethod backend-control-node-at ((backend alsa-midi) time (channel number) params)
  ;; FIX
  #+nil (midihelper:ev-cc channel note velocity))

(defmethod backend-control-node-at ((backend alsa-midi) time node params)
  nil)

(defmethod backend-play-event ((backend alsa-midi) event task)
  (let ((pgm (alsa-midi-instrument-program-number (event-value event :instrument))))
    (when (or pgm (find :alsa-midi (ensure-list (event-value event :backend))))
      (let* ((type (event-value event :type))
             (channel (midi-truncate-clamp (or (event-value event :channel)
                                               (event-value event :chan)
                                               0)
                                           15))
             (pgm (or pgm 0))
             (note (midi-truncate-clamp (event-value event :midinote)))
             (velocity (unipolar-1-to-midi (event-value event :amp))) ;; FIX: maybe this shouldn't be linear?
             (time (or (raw-event-value event :timestamp-at-start) (local-time:now)))
             (extra-params (loop :for key :in (keys event)
                                 :for cc-mapping := (alsa-midi-remap-key-value key (event-value event key))
                                 :if cc-mapping
                                   :collect cc-mapping)))
        (bt:make-thread
         (lambda ()
           (sleep (max 0 (local-time:timestamp-difference time (local-time:now))))
           (when (and pgm
                      (not (eql pgm (nth channel *alsa-midi-channels-instruments*))))
             (midihelper:send-event (midihelper:ev-pgmchange channel pgm))
             (setf (nth channel *alsa-midi-channels-instruments*) pgm))
           (dolist (param extra-params)
             (midihelper:send-event (midihelper:ev-cc channel (car param) (cadr param))))
           (unless (eql type :set)
             (midihelper:send-event (midihelper:ev-noteon channel note velocity))
             (sleep (max 0 (dur-time (sustain event) (tempo (task-clock task)))))
             (midihelper:send-event (midihelper:ev-noteoff channel note velocity))))
         :name "cl-patterns temporary alsa midi note thread")))))

(defmethod backend-task-removed ((backend alsa-midi) task)
  ;; FIX
  )

(export '(alsa-midi-panic alsa-midi-instrument-program-number alsa-midi-cc-mapping get-alsa-midi-cc-mapping alsa-midi-set-cc-mapping set-alsa-midi-cc-mapping alsa-midi-remap-key-value))
