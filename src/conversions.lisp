;;;; conversions.lisp - functionality to convert between different units.

(in-package #:cl-patterns)

;;; defconversion

(defvar *conversions* (make-hash-table)
  "Hash table storing conversion function definitions and metadata.")

;; FIX: maybe separate out name into FROM and TO, so some conversions can be auto-generated?
(defmacro defconversion (name lambda-list &body body)
  "Define a conversion, storing function definition information so it can be used later to define \"pseudo-ugens\" for synthesis definitions in supported backends."
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (setf (gethash ',name *conversions*)
           (list :lambda-list ',lambda-list
                 :body ',body))))

;;; polarity

(defconversion bipolar-unipolar (number)
  "Convert a bipolar number (-1 to 1) to a unipolar number (0 to 1).

See also: `unipolar-bipolar'"
  (+ 1/2 (/ number 2)))

(defconversion unipolar-bipolar (number)
  "Convert a bipolar number (-1 to 1) to a unipolar number (0 to 1).

See also: `bipolar-unipolar'"
  (1- (* 2 number)))

;;; volume

(defconversion amp-db (amp)
  "Convert amplitude to decibels."
  (* 20 (log amp 10)))

(defconversion db-amp (db)
  "Convert decibels to amplitude."
  (expt 10 (/ db 20)))

;;; duration

(defconversion dur-time (dur &optional tempo)
  "Convert DUR in beats to time in seconds according to TEMPO in beats per second."
  (/ dur (or tempo
             (and *clock*
                  (tempo *clock*))
             1)))

(defconversion time-dur (time &optional tempo)
  "Convert TIME in seconds to duration in beats according to TEMPO in beats per second."
  (* time (or tempo
              (and *clock*
                   (tempo *clock*))
              1)))

(defconversion dur-freq (dur &optional tempo)
  "Convert DUR in beats to frequency in Hz according to TEMPO in beats per second."
  (/ 1 (/ dur (or tempo
                  (and *clock*
                       (tempo *clock*))
                  1))))

(defconversion freq-dur (freq &optional tempo)
  "Convert FREQ in Hz to duration in beats according to TEMPO in beats per second."
  (/ (or tempo
         (and *clock*
              (tempo *clock*))
         1)
     freq))

;;; pitch
;; pitch units hierarchy:
;; - freq
;;     - ctranspose - chromatic transposition, in 12ET units. added to :midinote.
;;     - midinote - midi note number.
;;       - root - scale root, given in 12ET midi note increments.
;;       - octave - octave number for :note=0. the default is 4, mapping note 0 onto midi note 60 by default.
;;       - steps-per-octave - how many note units map onto the octave. supports non-12ET temperaments.
;;       - gtranspose - non-12ET transposition, in :note units. added to note.
;;       - note - note number in any division of the octave. 0 is the scale root.
;;         - degree - scale degree.
;;         - scale - mapping of scale degrees onto semitones. major, for instance, is (0 2 4 5 7 9 11)
;;         - steps-per-octave - same as above.
;;         - mtranspose - modal transposition; added to degree.
;;
;; more info:
;; https://doc.sccode.org/Tutorials/A-Practical-Guide/PG_07_Value_Conversions.html
;; https://depts.washington.edu/dxscdoc/Help/Classes/Event.html
;; https://depts.washington.edu/dxscdoc/Help/Classes/Integer.html

;; rate

(defconversion rate-freq (rate &optional (base-freq 440))
  "Convert a playback rate RATE to a frequency, based on BASE-FREQ."
  (* base-freq rate))

(defconversion freq-rate (freq &optional (base-freq 440))
  "Convert the frequency FREQ to a playback rate, based on BASE-FREQ. Useful to convert musical pitch information to a number usable for sample playback synths."
  (/ freq base-freq))

;; FIX: need `note-rate' and `rate-note' functions

(defvar *c4-midinote* 60
  "The MIDI note number of note C, octave 4. This sets the global tuning of the various pitch conversion functions.")

;; midinote

(defconversion midinote-freq (midinote)
  "Convert a midi note number to a frequency."
  (* 440d0 (expt 2d0 (/ (- midinote 69d0) 12))))

(defconversion freq-midinote (freq)
  "Convert a frequency to a midi note number."
  (+ 69d0 (* 12d0 (log (/ freq 440) 2d0))))

(defconversion rate-midinote (rate &optional (base-note 69))
  "Convert a playback rate to a midinote."
  (freq-midinote (rate-freq rate (midinote-freq base-note))))

(defconversion midinote-rate (midinote &optional (base-note 69))
  "Convert a midinote to a playback rate."
  (freq-rate (midinote-freq midinote) (midinote-freq base-note)))

(defconversion ratio-midi (ratio) ; FIX: rename to ratio-midi-transposition?
  "Convert a frequency ratio to a difference in MIDI note numbers."
  (* 12 (log ratio 2)))

(defconversion midi-ratio (midi) ; FIX: rename to midi-transposition-ratio?
  "Convert a MIDI note number difference to a frequency ratio."
  (expt 2 (/ midi 12)))

;; octave

(defconversion octave-freq (octave)
  "Get the base frequency of OCTAVE."
  (midinote-freq (octave-midinote octave)))

(defconversion freq-octave (freq)
  "Get the octave number that the frequency FREQ occurs in."
  (midinote-octave (freq-midinote freq)))

(defconversion octave-midinote (octave)
  "Get the base (lowest) midi note number for OCTAVE."
  (+ *c4-midinote* (* 12 (- octave 4))))

(defconversion midinote-octave (midinote)
  "Get the octave number that MIDINOTE occurs in."
  (+ 4 (floor (- midinote *c4-midinote*) 12)))

;; note

(defconversion note-freq (note &rest args &key (root 0) (octave 4) (steps-per-octave 12) (gtranspose 0) (octave-ratio 2))
  "Given a note, return its frequency in hertz, taking into account the ROOT and OCTAVE if provided.

See also: `note-midinote'"
  (declare (ignore root octave steps-per-octave gtranspose octave-ratio))
  (midinote-freq (apply #'note-midinote note args)))

(defconversion freq-note (freq)
  "Get the note for the frequency provided."
  (midinote-note (freq-midinote freq)))

(defconversion note-midinote (note &key (root 0) (octave 4 octave-provided-p) (steps-per-octave 12) (gtranspose 0) (octave-ratio 2))
  "Given a note, return its midi note number, taking into account the OCTAVE if provided.

See also: `note-freq'"
  (when (string-designator-p note)
    (let ((note-has-digit-p (position-if #'digit-char-p (string note))))
      (destructuring-bind (name note-octave) (note-name-and-octave note)
        (when (and octave-provided-p note-has-digit-p)
          (warn "Ignoring ~S provided as OCTAVE to ~S because the note ~S already specifies the octave" octave 'note-midinote note))
        (return-from note-midinote
          (note-midinote (note-chromatic-index name)
                         :root root
                         :octave (if note-has-digit-p note-octave octave)
                         :steps-per-octave steps-per-octave
                         :gtranspose gtranspose)))))
  (+ (* (+ (/ (+ note gtranspose root) steps-per-octave)
           octave
           -4)
        (* 12 (log octave-ratio 2d0)))
     *c4-midinote*))

(defconversion midinote-note (midinote)
  "Get the note name of MIDINOTE."
  (make-keyword (concat (chromatic-index-note (mod midinote 12))
                        (midinote-octave midinote))))

(defconversion note-octave (note)
  "Get the octave of NOTE."
  (second (note-name-and-octave note)))

(defconversion octave-note (octave)
  "Get the note of OCTAVE."
  (make-keyword (concat :c octave)))

;; chromatic-index

(defparameter *note-names* '((:c :b#) (:c# :db) (:d)
                             (:d# :eb) (:e :fb) (:f :e#)
                             (:f# :gb) (:g) (:g# :ab)
                             (:a) (:a# :bb) (:b :cb))
  "List of note names in the equal temperament 12-tone tuning.")

(defconversion chromatic-index-freq (chromatic-index &optional (octave 4))
  "Get the frequency of CHROMATIC-INDEX."
  (midinote-freq (chromatic-index-midinote chromatic-index octave)))

(defconversion freq-chromatic-index (freq)
  "Get the chromatic index of FREQ."
  (midinote-chromatic-index (freq-midinote freq)))

(defconversion chromatic-index-midinote (chromatic-index &optional (octave 4))
  "Get the midinote of CHROMATIC-INDEX."
  (+ chromatic-index
     *c4-midinote*
     (* 12 (- octave 4))))

(defconversion midinote-chromatic-index (midinote)
  "Get the chromatic index of MIDINOTE."
  (note-chromatic-index (midinote-note midinote)))

(defconversion note-chromatic-index (note) ; FIX: should we allow tuning/scale args here, to ensure integers are within the chromatic-index range?
  "Get the chromatic index of NOTE."
  (etypecase note
    (number note)
    (string-designator
     (position (chromatic-index-note note) *note-names*
               :test (lambda (item list)
                       (member item list :test #'string=))))))

(defconversion chromatic-index-note (chromatic-index)
  "Get the note of CHROMATIC-INDEX.

Note that this function is not aware of context and thus always returns the first known name of each note, not necessarily the one that is \"correct\"."
  (etypecase chromatic-index
    (string-designator
     (car (note-name-and-octave chromatic-index)))
    (integer
     (car (elt-wrap *note-names* chromatic-index)))))

;; degree

(defun degree-key (degree &key (scale (scale :major)) steps-per-octave (accidental 0))
  "Get the note in SCALE at the index DEGREE, possibly overriding STEPS-PER-OCTAVE."
  (check-type scale (or scale string-designator (and list (not null))))
  (when (string-designator-p scale)
    (let ((found-scale (scale scale)))
      (unless found-scale
        (error "No scale defined with name ~S." scale))
      (return-from degree-key
        (degree-key degree :scale found-scale :steps-per-octave steps-per-octave))))
  (let* ((list (typecase scale
                 (list scale)
                 (scale (scale-notes scale))))
         (steps-per-octave (or steps-per-octave (scale-steps-per-octave scale))) ; FIX: need to implement scale-steps-per-octave
         (base-key (+ (* steps-per-octave (floor degree (length list)))
                      (elt-wrap list (truncate degree)))))
    (+ base-key (* accidental (/ steps-per-octave 12d0)))))

(defconversion degree-freq (degree &rest args &key (root 0) (octave 4) (scale :major))
  "Get the frequency of DEGREE, based on the ROOT, OCTAVE, and SCALE, if provided."
  (declare (ignore root octave scale))
  (midinote-freq (apply #'degree-midinote degree args)))

(defconversion freq-degree (freq &key root octave (scale :major))
  "Convert a frequency to a scale degree."
  (midinote-degree (freq-midinote freq)
                   :root root
                   :octave octave
                   :scale scale))

(defconversion degree-midinote (degree &key (root 0) (octave 4) (scale :major))
  "Get the midi note number of DEGREE, taking into account the ROOT, OCTAVE, and SCALE, if provided."
  (let ((steps-per-octave (scale-steps-per-octave scale)))
    (note-midinote (degree-note degree
                                :scale scale
                                :steps-per-octave steps-per-octave)
                   :root root
                   :octave octave
                   :steps-per-octave steps-per-octave
                   :octave-ratio (tuning-octave-ratio (scale-tuning scale)))))

(defconversion midinote-degree (midinote &key (root 0) (scale :major))
  "Get the degree of MIDINOTE, taking into account the ROOT, OCTAVE, and SCALE, if provided."
  (note-degree (midinote-note midinote)
               :root root
               :scale scale))

(defconversion degree-octave (degree)
  "Get the octave of DEGREE."
  (declare (ignore degree))
  4)

(defconversion octave-degree (octave)
  "Get the degree of OCTAVE."
  (declare (ignore octave))
  0)

(defconversion degree-chromatic-index (degree &rest args &key (scale :major) steps-per-octave (mtranspose 0))
  "Get the chromatic index of DEGREE."
  (declare (ignore scale steps-per-octave mtranspose))
  (note-chromatic-index (apply #'degree-note degree args)))

(defconversion chromatic-index-degree (chromatic-index &rest args &key (root 0) (scale :major))
  "Get the degree of CHROMATIC-INDEX."
  (declare (ignore root scale))
  (apply #'note-degree
         (chromatic-index-note chromatic-index)
         args))

(defconversion degree-note (degree &key (scale :major) steps-per-octave (mtranspose 0))
  "Get the note number of DEGREE in SCALE."
  (degree-key (+ degree mtranspose)
              :scale scale
              :steps-per-octave (or steps-per-octave (scale-steps-per-octave scale))))

(defconversion note-degree (note &key (root 0) (scale :major))
  "Get the degree of a note in the provided scale. If the note is not in the provided scale, truncate to the nearest note that is."
  (let* ((transposed-scale (mapcar (fn (+ _ root))
                                   (scale-notes scale)))
         (res (nearest (mod (note-chromatic-index note)
                            (scale-steps-per-octave scale))
                       transposed-scale)))
    (position res transposed-scale :test #'=)))

;;; MIDI stuff (FIX: move to midi backend? how many of these are actually needed?)

(defun midi-truncate-clamp (number &optional (max 127))
  "Truncate NUMBER and clamp it to the range 0..MAX (default 127)."
  (declare (number number))
  (clamp (truncate number) 0 max))

(defconversion bipolar-1-to-midi (number)
  "Convert the range -1..1 to 0..127."
  (clamp (ceiling (* 63.5 (1+ number))) 0 127))

(defconversion unipolar-1-to-midi (number)
  "Convert the range 0..1 to 0..127."
  (clamp (round (* 127 number)) 0 127))

(defconversion frequency-to-midi (frequency)
  "Convert FREQUENCY to a MIDI note number (rounding to ensure it's an integer).

Note that this function is meant for use with the MIDI backend; for frequency-to-midinote conversion without rounding, see `freq-midinote' instead."
  (round (freq-midinote frequency)))

(defconversion boolean-to-midi (boolean)
  "Convert BOOLEAN to a MIDI value.

Examples:

;; ;; false/\"off\" values:
;; (boolean-to-midi nil) ;=> 0
;; (boolean-to-midi 0) ;=> 0
;; (boolean-to-midi 63) ;=> 63

;; ;; true/\"on\" values:
;; (boolean-to-midi t) ;=> 127
;; (boolean-to-midi 64) ;=> 64
;; (boolean-to-midi 127) ;=> 127

See also: `parse-boolean'"
  (typecase boolean
    (integer boolean)
    (rational (if (>= boolean 0.5) 127 0))
    (boolean (if boolean 127 0))))

(defconversion bipolar-1-to-midi-pitchbend (number)
  "Convert the range -1..1 to pitchbend message values used by the `alsa-midi' backend."
  (clamp (ceiling (* 8192 number)) -8192 8191))
