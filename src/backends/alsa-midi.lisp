;;;; alsa-midi.lisp - backend to output MIDI via ALSA.

(in-package #:cl-patterns)

(defclass alsa-midi (backend)
  ((name :initform "ALSA MIDI"))
  (:documentation "Backend for controlling hardware and software via ALSA MIDI on Linux."))

;;; utility functions

(defun alsa-midi-panic (&key channel manually-free)
  "Stop all notes on CHANNEL, or all channels if CHANNEL is nil."
  (dolist (c (or (ensure-list channel) (iota 16)))
    (if manually-free
        (dolist (note (iota 128))
          (midihelper:send-event (midihelper:ev-noteoff c note 0)))
        (midihelper:send-event (midihelper:ev-cc c 123 0)))))

;;; pitch bending

(defvar *alsa-midi-pitchbend-range* 2
  "Number of semitones in the pitchbend range of the MIDI device.")

(defun midi-pitchbend (bend midinote)
  "Return a pitchbend MIDI value, given BEND, on a range of -1.0..1.0, and MIDINOTE. If BEND is non-nil, convert it to the appropriate number for the MIDI message. Otherwise, if MIDINOTE has a decimal part, return the amount of pitchbend necessary to detune it. This allows for microtonality to be expressed as a fraction of a midinote, or auto-converted from the :freq event key. If both BEND and a fractional MIDINOTE are given, only BEND is considered."
  (cond (bend (bipolar-1-to-midi-pitchbend bend))
	(midinote (let ((decimal (mod midinote 1)))
		    (unless (zerop decimal)
		      (bipolar-1-to-midi-pitchbend
		       (/ decimal *alsa-midi-pitchbend-range*)))))))

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

(defun alsa-midi-set-cc-mapping (cc-number description &optional event-key (mapper 'midi-truncate-clamp))
  "Set a mapping for CC-NUMBER. When EVENT-KEY is seen in an event being played by the alsa-midi backend, its value will be converted using the function specified by MAPPER, and then that value will be set for CC number CC-NUMBER just before the note itself is triggered. DESCRIPTION is a description of what the CC controls."
  (setf (gethash cc-number *alsa-midi-cc-map*)
        (list cc-number description event-key mapper))
  (dolist (event-key (ensure-list event-key))
    (setf (gethash event-key *alsa-midi-cc-map*) cc-number)))

(defun alsa-midi-remap-key-value (key value)
  "Remap KEY and VALUE to their equivalent MIDI CC parameter and range. Returns a list of the format (CC-NUMBER CC-VALUE), or nil if the key is not a recognized CC key."
  (if-let ((cc-mapping (alsa-midi-cc-mapping key)))
    (list (car cc-mapping) (funcall (nth 3 cc-mapping) value))
    (let ((sym-name (symbol-name key)))
      (if (and (>= (length sym-name) 3)
               (string= "CC-" sym-name :end2 3))
          (list (midi-truncate-clamp (parse-integer (subseq sym-name 3))) (midi-truncate-clamp value))
          nil))))

;;; default cc mappings
;; http://nickfever.com/music/midi-cc-list
;; https://anotherproducer.com/online-tools-for-musicians/midi-cc-list/
;; https://www.presetpatch.com/midi-cc-list.aspx

(mapc (fn (apply #'alsa-midi-set-cc-mapping _))
      '((0 "Bank select (MSB)" :bank-msb)
        (1 "Modulation/Vibrato" (:mod :vibrato :wheel) unipolar-1-to-midi)
        (2 "Breath controller" :breath unipolar-1-to-midi)
        (4 "Foot pedal" :foot-pedal unipolar-1-to-midi)
        (5 "Portamento time" (:porta-time :portamento-time) unipolar-1-to-midi)
        (7 "Volume" :volume unipolar-1-to-midi)
        (8 "Balance" :balance bipolar-1-to-midi)
        (10 "Pan" :pan bipolar-1-to-midi)
        (11 "Expression pedal" (:expression :expression-pedal) unipolar-1-to-midi)
        (12 "Effect 1" :effect-1 unipolar-1-to-midi)
        (13 "Effect 2" :effect-2 unipolar-1-to-midi)
        (32 "Bank select (LSB)" :bank-lsb)
        (33 "Modulation (LSB)" (:mod-lsb :vibrato-lsb :wheel-lsb))
        (34 "Breath controller (LSB)" :breath-lsb)
        (36 "Foot pedal (LSB)" :foot-pedal-lsb)
        (37 "Portamento time (LSB)" (:porta-time-lsb :portamento-time-lsb))
        (39 "Volume (LSB)" :volume-lsb)
        (42 "Pan (LSB)" :pan-lsb)
        (43 "Expression pedal (LSB)" (:expression-lsb :expression-pedal-lsb))
        (44 "Effect 1 (LSB)" :effect-1-lsb)
        (45 "Effect 2 (LSB)" :effect-2-lsb)
        (64 "Sustain pedal on/off" :sustain-pedal boolean-to-midi)
        (65 "Portamento on/off" :sustain-pedal boolean-to-midi)
        (66 "Sostenuto on/off" :sostenuto boolean-to-midi)
        (67 "Soft pedal on/off" :soft-pedal boolean-to-midi)
        (68 "Legato on/off" (:legato-on :legato-enable) boolean-to-midi)
        (69 "Hold pedal on/off" (:hold-pedal-on :hold-pedal-enable :hold-on :hold-enable) boolean-to-midi)
        (71 "Filter resonance/Timbre" :res unipolar-1-to-midi)
        (72 "Amp envelope release" :release)
        (73 "Amp envelope attack" :attack)
        (74 "Filter cutoff/Brightness" :ffreq frequency-to-midi)
        (76 "Vibrato rate" :vibrato-rate unipolar-1-to-midi)
        (77 "Vibrato depth" :vibrato-depth unipolar-1-to-midi)
        (78 "Vibrato delay" :vibrato-delay unipolar-1-to-midi)
        (84 "Portamento amount" (:porta :portamento :portamento-amount) unipolar-1-to-midi)
        (91 "Reverb amount" (:reverb :reverb-amount) unipolar-1-to-midi)
        (92 "Tremolo amount" (:tremolo :tremolo-amount) unipolar-1-to-midi)
        (93 "Chorus amount" (:chorus :chorus-amount) unipolar-1-to-midi)
        (94 "Detune amount" (:detuning :detune-amount) unipolar-1-to-midi)
        (95 "Phaser amount" (:phaser :phaser-amount) unipolar-1-to-midi)
        (120 "Channel mute/Sound off" :channel-mute boolean-to-midi)
        (121 "Reset controllers" :reset-controllers boolean-to-midi)
        (122 "Local keyboard enable" :local-keyboard-enable boolean-to-midi)
        (123 "Release all notes/MIDI panic" (:release-all :midi-panic :all-notes-off) boolean-to-midi)
        (124 "OMNI mode off" (:omni-mode-off :omni-mode-disable))
        (125 "OMNI mode on" (:omni-mode-on :omni-mode-enable))
        (126 "Mono mode on/Unison mode on" (:mono-mode-on :mono-mode-enable))
        (127 "Poly mode on" (:poly-mode-on :poly-mode-enable))))

(defvar *undefined-midi-ccs* (a 3 9 14 15 20..31 85..90 102..119)
  "List of MIDI CC numbers that are left undefined (\"free\") in the MIDI specification.")

;;; current channel instruments

(defvar *alsa-midi-channels-instruments* (make-list 16)
  "List mapping MIDI channel numbers to the program number currently playing on them.")

;;; backend functions

(defmethod backend-start ((backend alsa-midi) &key)
  (unless (elt (midihelper:inspect-midihelper) 5)
    (midihelper:start-midihelper)))

(defmethod backend-stop ((backend alsa-midi))
  (midihelper:stop-midihelper)
  backend)

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
             (note (midi-truncate-clamp (event-value event :midinote)))
             (velocity (unipolar-1-to-midi (event-value event :amp))) ; FIX: maybe this shouldn't be linear?
	     (bend (midi-pitchbend (event-value event :bend)
				   (event-value event :midinote)))
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
             (midihelper:send-event (midihelper:ev-cc channel (first param) (second param))))
	   (when bend
	     (midihelper:send-event (midihelper:ev-pitchbend channel bend)))
           (unless (eql type :set)
             (midihelper:send-event (midihelper:ev-noteon channel note velocity))
             (sleep (max 0 (dur-duration (sustain event) (tempo (task-clock task)))))
             (midihelper:send-event (midihelper:ev-noteoff channel note velocity))))
         :name "cl-patterns temporary alsa midi note thread")))))

(defmethod backend-task-removed ((backend alsa-midi) task)
  ;; FIX
  )

(export '(alsa-midi alsa-midi-panic *alsa-midi-pitchbend-range* alsa-midi-instrument-program-number alsa-midi-cc-mapping alsa-midi-set-cc-mapping alsa-midi-remap-key-value))
