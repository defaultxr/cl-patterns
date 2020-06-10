(in-package #:cl-patterns)

;;; NOTES:
;; - 'key' is a scale including the root note. i.e. "g major" or the like.
;; - http://harmoniccycle.com/hc/music-08-chord-calc.htm
;; - https://www.reddit.com/r/musictheory/comments/8kycvx/online_music_theory_calculator/
;; - https://www.reddit.com/r/musictheory/comments/17v5pp/does_anyone_know_of_a_keyscale_calculator_that/
;; - http://harmoniccycle.com/hc/music-08-chord-calc.htm
;; - http://openmusictheory.com/scales.html
;; - https://atactionpark.github.io/
;; - https://www.solfej.io/chords/c-major - scale and chord finder site

;;; utility

(defparameter *abbreviations*
  (list
   (list "MAJOR" "MAJ")
   (list "MINOR" "MIN")
   (list "AUGMENTED" "AUG")
   (list "DOMINANT" "DOM")
   (list "DIMINISHED" "DIM")
   (list "SUSPENDED" "SUS")
   (list "TRIAD" "TRI")
   (list "OTONALITY" "O")
   (list "UTONALITY" "U")
   (list "PENTATONIC" "PENT")
   (list "HARMONIC" "HARM"))
  "List of abbreviations that should be used by `generate-aliases' to auto-generate aliases for `tuning's, `scale's, `chord's, etc.")

(defun generate-aliases (name)
  "Generate a list of aliases for NAME by mapping various combinations of `*abbreviations*'.

See also: `*abbreviations*'"
  (when (symbolp name)
    (return-from generate-aliases
      (generate-aliases (symbol-name name))))
  (remove-duplicates
   (mapcar #'friendly-symbol
           (let* (res
                  (name (string-upcase name))
                  (abbrs-found (remove-if-not
                                (lambda (a)
                                  (search (car a) name))
                                *abbreviations*)))
             (dotimes (n (1+ (length abbrs-found)) res)
               (map-combinations
                (lambda (combination)
                  (let ((cur-name name))
                    (dolist (c combination)
                      (setf cur-name (string-replace cur-name (car c) (cadr c))))
                    (push cur-name res)
                    (push (remove #\space cur-name) res)))
                abbrs-found
                :length n))))))

;;; notes

(defparameter *note-names* '((:c :b#) (:c# :db) (:d) (:d# :eb) (:e :fb) (:f :e#) (:f# :gb) (:g) (:g# :ab) (:a) (:a# :bb) (:b :cb))
  "List of note names in the equal temperament 12-tone tuning.")

(defun note-name-and-octave (note)
  "Given a note name, return a list consisting of its note number and its octave (defaulting to 5 if it's not specified).

See also: `note-number', `note-name'"
  (let* ((str (string note))
         (note (remove-if #'digit-char-p str))
         (octave (remove-if-not #'digit-char-p str)))
    (list (if (emptyp note) :c (make-keyword note))
          (if (emptyp octave) 5 (parse-integer octave)))))

(defun note-number (note)
  "Given a note name or note number, return the note number.

See also: `note-name', `note-name-and-octave', `note-midinote'"
  (etypecase note
    (number note)
    (symbol
     (destructuring-bind (name octave) (note-name-and-octave note)
       (+ (position name *note-names* :test #'position) (* 12 (- octave 5)))))))

(defun note-name (note-number)
  "Given a note number, return its note name.

Note that this function is not aware of context and thus always returns the first known name of each note, not necessarily the one that is \"correct\".

See also: `note-number', `note-midinote'"
  (car (elt-wrap *note-names* note-number)))

(defun sharp-or-flat (string)
  "Given STRING, return a number representing how many semitones above or below its number it represents, by counting sharps (#) and flats (b)."
  (+ (* -1 (count #\b string :test #'string-equal))
     (count #\# string :test #'string-equal)))

(defun index-and-offset (num)
  "Return a cons cell consisting of the input number and the offset its sharps/flats represent."
  (etypecase num
    (number (cons num 0))
    (symbol (index-and-offset (string num)))
    (string (cons (parse-integer num :junk-allowed t)
                  (sharp-or-flat num)))))

;;; tunings

(defgeneric tuning-name (tuning)
  (:documentation "The full name of the tuning."))

(defgeneric tuning-pitches (tuning)
  (:documentation "The actual semitone values of the tuning."))

(defgeneric tuning-octave-ratio (tuning)
  (:documentation "The octave ratio of the tuning (i.e. what to multiply a pitch by to increase its octave by 1)."))

(defclass tuning (standard-object #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((name :initarg :name :accessor tuning-name :documentation "The full name of the tuning.")
   (pitches :initarg :pitches :accessor tuning-pitches :documentation "The actual semitone values of the tuning.")
   (octave-ratio :initarg :octave-ratio :accessor tuning-octave-ratio :documentation "The octave ratio of the tuning (i.e. what to multiply a pitch by to increase its octave by 1)."))
  (:documentation "Tuning definition."))

(defmethod print-object ((this tuning) stream)
  (format stream "(~s ~s)" 'tuning (tuning-name this)))

(defvar *tunings* (make-hash-table)
  "Hash table mapping names and aliases to tuning definition objects.")

(defun define-tuning (name pitches octave-ratio &optional aliases)
  "Define a musical tuning.

See also: `tuning', `define-scale', `define-chord'"
  (let ((key (friendly-symbol name))
        (tuning (make-instance 'tuning
                               :name name
                               :pitches pitches
                               :octave-ratio octave-ratio)))
    (setf (gethash key *tunings*) tuning)
    (dolist (alias (remove-duplicates (append (generate-aliases key) aliases)))
      (let ((existing (gethash alias *tunings*)))
        (if existing
            (if (symbolp existing)
                (progn
                  (warn "Replacing existing alias \"~s\" (for ~s) with an alias for ~s..." alias existing tuning)
                  (setf (gethash alias *tunings*) key))
                (unless (eq existing tuning)
                  (warn "Ignoring alias ~s that points to ~s (while trying to add it as an alias for ~s)." alias existing tuning)))
            (setf (gethash alias *tunings*) key))))))

(defun all-tunings ()
  "Get a list of all defined tunings."
  (remove-if-not (lambda (value)
                   (typep value 'tuning))
                 (hash-table-values *tunings*)))

(defgeneric tuning (object)
  (:documentation "Get a tuning by name."))

(defmethod tuning ((object symbol))
  (let ((tuning (gethash object *tunings*)))
    (when tuning
      (if (symbolp tuning)
          (tuning tuning)
          tuning))))

(defmethod tuning ((object string))
  (tuning (friendly-symbol object)))

(defmethod tuning ((object tuning))
  object)

(defmethod tuning-name ((this symbol))
  (tuning-name (tuning this)))

(defmethod tuning-pitches ((this symbol))
  (tuning-pitches (tuning this)))

(defmethod tuning-octave-ratio ((this symbol))
  (tuning-octave-ratio (tuning this)))

;;; Scala (.scl) scale file support
;; http://www.huygens-fokker.org/scala/scl_format.html

(defun load-scala-scale (file &optional aliases)
  "Load a Scala (.scl) scale file and define a tuning and scale from it.

Note that Scala refers to these as \"scales\" but in cl-patterns they are known as tunings."
  (with-open-file (stream file :direction :input :if-does-not-exist :error)
    (let* ((lines (loop :for line = (read-line stream nil 'eof)
                     :if (eql line 'eof)
                     :do (loop-finish)
                     :unless (char= #\! (elt line 0))
                     :collect (string-left-trim '(#\space) line)))
           (name (car lines))
           (count (parse-integer (cadr lines)))
           (pitches (append (list 0)
                            (mapcar (lambda (line) (let ((line (subseq line
                                                                       (position-if (lambda (char) (char/= #\space char)) line)
                                                                       (position-if (lambda (char) (char= #\space char)) line))))
                                                     (if (position #\. line :test #'char=) ;; if it has a . it's a cents value
                                                         (/ (read-from-string line) 100) ;; cents
                                                         (ratio-midi (read-from-string line)))))
                                    (cddr lines))))
           (octave-ratio (car (last pitches)))
           (pitches (butlast pitches))
           (aliases (append (list (file-namestring (subseq file 0 (search ".scl" file :from-end t)))) aliases)))
      (when (/= (length pitches) count)
        (warn "There are ~a pitches listed in ~a but the file says there should be ~a pitches." (length pitches) file count))
      (unless (loop :for i :in (all-tunings)
                 :if (equal (tuning-pitches i) pitches)
                 :return (progn
                           ;; (warn "~&Tuning already exists as ~a; adding aliases: ~a" (tuning-name i) aliases)
                           ;; (apply #'define-tuning-aliases (tuning-name i) aliases)
                           ;; FIX: should we just remove the already-defined tuning and re-add with all the aliases, or was there a reason we were doing define-tuning-aliases separately?
                           ;; FIX: define scale aliases too
                           t))
        (define-tuning name pitches octave-ratio aliases)
        (define-scale name (iota (length pitches)) name aliases)))))

;;; scales

(defgeneric scale-name (scale)
  (:documentation "The full name of the scale."))

(defgeneric scale-notes (scale)
  (:documentation "The degrees of the scale."))

(defgeneric scale-tuning (scale)
  (:documentation "The scale's `tuning'."))

(defclass scale (standard-object #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((name :initarg :name :accessor scale-name :documentation "The full name of the scale.")
   (notes :initarg :notes :accessor scale-notes :documentation "The degrees of the scale.") ;; FIX: rename to degrees?
   (tuning :initarg :tuning :accessor scale-tuning :documentation "The scale's `tuning'."))
  (:documentation "Scale definition."))

(defmethod print-object ((this scale) stream)
  (format stream "(~s ~s)" 'scale (scale-name this)))

(defvar *scales* (make-hash-table)
  "Hash table mapping names and aliases to scale definition objects.")

(defun define-scale (name notes &optional (tuning :et12) aliases)
  "Define a musical scale.

See also: `scale', `define-tuning', `define-chord'"
  (let ((key (friendly-symbol name))
        (scale (make-instance 'scale
                              :name name
                              :notes notes
                              :tuning tuning)))
    (unless (tuning tuning)
      (warn "Tuning ~s does not exist." tuning))
    (setf (gethash key *scales*) scale)
    (dolist (alias (remove-duplicates (append (generate-aliases key) aliases)))
      (let ((existing (gethash alias *scales*)))
        (if existing
            (if (symbolp existing)
                (progn
                  (warn "Replacing existing alias \"~s\" (for ~s) with an alias for ~s..." alias existing scale)
                  (setf (gethash alias *scales*) key))
                (unless (eq existing scale)
                  (warn "Ignoring alias ~s that points to ~s (while trying to add it as an alias for ~s)." alias existing scale)))
            (setf (gethash alias *scales*) key))))))

(defun all-scales ()
  "Get a list of all defined scales."
  (remove-if-not (lambda (value)
                   (typep value 'scale))
                 (hash-table-values *scales*)))

(defgeneric scale (object)
  (:documentation "Get either a musical scale object by its name, or get the name of the scale of an event."))

(defmethod scale ((object symbol))
  (let ((scale (gethash object *scales*)))
    (when scale
      (if (symbolp scale)
          (scale scale)
          scale))))

(defmethod scale ((object string))
  (scale (friendly-symbol object)))

(defmethod scale ((object scale))
  object)

(defmethod scale-name ((this symbol))
  (scale-name (scale this)))

(defmethod scale-notes ((this symbol))
  (scale-notes (scale this)))

(defmethod scale-tuning ((this symbol))
  (scale-tuning (scale this)))

(defun scale-midinotes (scale &key (root :c) (octave 5))
  "Given a scale, return its midi note numbers. OCTAVE can be a number, a 2-element list denoting an octave range, or :all, for the full octave range (0-9)."
  (typecase octave
    (symbol (if (eql :all octave)
                (scale-midinotes scale :root root :octave (list 0 9))
                (error "Invalid OCTAVE argument for scale-midinotes; try :all, a number, or a 2-element list denoting a range instead.")))
    (number (scale-midinotes scale :root root :octave (list octave octave)))
    (cons
     (let ((scale (scale scale))
           (root (note-number root)))
       (loop :for i :from (car octave) :upto (cadr octave)
          :append (mapcar (lambda (note) (note-midinote note :root root :octave i)) (scale-notes scale)))))))

;;; chords

(defgeneric chord-name (chord)
  (:documentation "The octave ratio of the chord (i.e. what to multiply a pitch by to increase its octave by 1)."))

(defgeneric chord-indexes (chord)
  (:documentation "The indexes of the notes from the scale that the chord is made up of."))

(defgeneric chord-scale (chord)
  (:documentation "The scale that the chord is derived from."))

(defclass chord (standard-object #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((name :initarg :name :accessor chord-name :documentation "The full name of the chord.")
   (indexes :initarg :indexes :accessor chord-indexes :documentation "The indexes of the notes from the scale that the chord is made up of.")
   (scale :initarg :scale :accessor chord-scale :documentation "The scale that the chord is derived from."))
  (:documentation "Chord definition."))

(defmethod print-object ((this chord) stream)
  (format stream "(~s ~s)" 'chord (chord-name this)))

(defvar *chords* (make-hash-table)
  "Hash table mapping names and aliases to chord definition objects.")

(defun define-chord (name indexes scale &optional aliases)
  "Define a musical chord.

See also: `scale', `define-tuning', `define-scale'"
  (let ((key (friendly-symbol name))
        (chord (make-instance 'chord
                              :name name
                              :scale scale
                              :indexes indexes)))
    (unless (scale scale)
      (warn "Scale ~s does not exist." scale))
    (setf (gethash key *chords*) chord)
    (dolist (alias (remove-duplicates (append (generate-aliases key) aliases)))
      (let ((existing (gethash alias *chords*)))
        (if existing
            (if (symbolp existing)
                (progn
                  (warn "Replacing existing alias \"~s\" (for ~s) with an alias for ~s..." alias existing chord)
                  (setf (gethash alias *chords*) key))
                (unless (eq existing chord)
                  (warn "Ignoring alias ~s that points to ~s (while trying to add it as an alias for ~s)." alias existing chord)))
            (setf (gethash alias *chords*) key))))))

(defun all-chords ()
  "Get a list of all defined chords."
  (remove-if-not (lambda (value)
                   (typep value 'chord))
                 (hash-table-values *chords*)))

(defgeneric chord (object)
  (:documentation "Get a chord by name."))

(defmethod chord ((object symbol))
  (let ((chord (gethash object *chords*)))
    (when chord
      (if (symbolp chord)
          (chord chord)
          chord))))

(defmethod chord ((object string))
  (chord (friendly-symbol object)))

(defmethod chord ((object chord))
  object)

(defmethod chord-name ((this symbol))
  (chord-name (chord this)))

(defmethod chord-scale ((this symbol))
  (chord-scale (chord this)))

(defmethod chord-indexes ((this symbol))
  (chord-indexes (chord this)))

(defmethod describe-object ((chord chord) stream)
  (with-slots (name scale) chord
    (format stream "~&~s is a chord named ~a,~%with possible abbreviations ~a.~%It takes notes from the ~a scale, which has notes ~s.~%Therefore, this chord contains the notes ~s,~%which are also known as ~a.~%"
            chord
            name
            (loop :for i :in (keys *chords*)
                  :if (eql (getf *chords* i) (friendly-symbol (chord-name chord)))
                    :collect i)
            scale
            (scale-notes (scale scale))
            (chord-notes chord)
            (mapcar #'note-name (chord-notes chord)))))

(defun chord-notes (chord)
  "Return a list consisting of the note numbers for CHORD."
  (mapcar (lambda (idx)
            (let ((io (index-and-offset idx)))
              (+ (nth (car io) (scale-notes (scale (chord-scale chord))))
                 (cdr io))))
          (chord-indexes chord)))

(defun chord-midinotes (chord &optional root (octave 5))
  "Get a list of the midi note numbers in the specified chord."
  (flet ((mchord (root chord)
           (mapcar #'+
                   (circular-list (+ (* 12 octave) (note-number root)))
                   (chord-notes (chord chord)))))
    (if (null chord)
        (progn
          (error "Not done yet.")
          nil)
        (mchord root chord))))

;;; base set of tunings and scales (copied from SuperCollider)

(dolist (tuning
          (flet ((mul-iota (mul num)
                   (mapcar (lambda (n)
                             (* n mul))
                           (iota num))))
            `(;; twelve-tone tunings
              ("Equal Temperament 12" ,(iota 12) 2 (:et12))

              ("Pythagorean" (t 1 256/243 9/8 32/27 81/64 4/3 729/512 3/2 128/81 27/16 16/9 243/128) 2 (:pyth))
              ("5-Limit Just Intonation" (t 1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 2 (:just))
              ("Septimal Tritone Just Intonation" (t 1 16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 9/5 15/8) 2 (:sept1))
              ("7-Limit Just Intonation" (t 1 16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 7/4 15/8) 2 (:sept2))
              ("Meantone, 1/4 Syntonic Comma" (0 0.755 1.93 3.105 3.86 5.035 5.79 6.965 7.72 8.895 10.07 10.82) 2 (:mean4))
              ("Meantone, 1/5 Pythagorean Comma" (0 0.804 1.944 3.084 3.888 5.028 5.832 6.972 7.776 8.916 10.056 10.86) 2 (:mean5))
              ("Meantone, 1/6 Pythagorean Comma" (0 0.86 1.96 3.06 3.92 5.02 5.88 6.98 7.84 8.94 10.04 10.9) 2 (:mean6))
              ("Kirnberger III" (t 1 256/243 ,(/ (sqrt 5) 2) 32/27 5/4 4/3 45/32 ,(expt 5 0.25) 128/81 ,(/ (expt 5 0.75) 2) 16/9 15/8) 2 (:kirnberger))
              ("Werckmeister III" (0 0.92 1.93 2.94 3.915 4.98 5.9 6.965 7.93 8.895 9.96 10.935) 2 (:werckmeister))
              ("Vallotti" (0 0.94135 1.9609 2.98045 3.92180 5.01955 5.9218 6.98045 7.9609 8.94135 10 10.90225) 2)
              ("Young" (0 0.9 1.96 2.94 3.92 4.98 5.88 6.98 7.92 8.94 9.96 10.9) 2)
              ("Mayumi Reinhard" (t 1 14/13 13/12 16/13 13/10 18/13 13/9 20/13 13/8 22/13 13/7 208/105) 2 (:reinhard))
              ("Wendy Carlos Harmonic" (t 1 17/16 9/8 19/16 5/4 21/16 11/8 3/2 13/8 27/16 7/4 15/8) 2 (:wcHarm))
              ("Wendy Carlos Super Just" (t 1 17/16 9/8 6/5 5/4 4/3 11/8 3/2 13/8 5/3 7/4 15/8) 2 (:wcSJ))
              ("Chinese Shi-er-lu scale" (t 1 2187/2048 9/8 19683/16384 81/64 177147/131072 729/612 3/2 6561/4096 27/16 59049/32768 243/128) 2 (:lu))

              ;; more than twelve-tone equal temperament
              ("Equal Temperament 19" ,(mul-iota 12/19 19) 2 (:et19))
              ("Equal Temperament 22" ,(mul-iota 6/11 22) 2 (:et22))
              ("Equal Temperament 24" ,(mul-iota 1/2 24) 2 (:et24))
              ("Equal Temperament 31" ,(mul-iota 12/31 31) 2 (:et31))
              ("Equal Temperament 41" ,(mul-iota 12/41 41) 2 (:et41))
              ("Equal Temperament 53" ,(mul-iota 12/53 53) 2 (:et53))

              ;; non-twelve-tone just intonation
              ("Ben Johnston" (t 1 25/24 135/128 16/15 10/9 9/8 75/64 6/5 5/4 81/64 32/25 4/3 27/20 45/32 36/25 3/2 25/16 8/5 5/3 27/16 225/128 16/9 9/5 15/8 48/25) 2 (:johnston))
              ("Harry Partch" (t 1 81/80 33/32 21/20 16/15 12/11 11/10 10/9 9/8 8/7 7/6 32/27 6/5 11/9 5/4 14/11 9/7 21/16 4/3 27/20 11/8 7/5 10/7 16/11 40/27 3/2 32/21 14/9 11/7 8/5 18/11 5/3 27/16 12/7 7/4 16/9 9/5 20/11 11/6 15/8 40/21 64/33 160/81) 2 (:partch))
              ("Jon Catler" (t 1 33/32 16/15 9/8 8/7 7/6 6/5 128/105 16/13 5/4 21/16 4/3 11/8 45/32 16/11 3/2 8/5 13/8 5/3 27/16 7/4 16/9 24/13 15/8) 2 (:catler))
              ("John Chalmers" (t 1 21/20 16/15 9/8 7/6 6/5 5/4 21/16 4/3 7/5 35/24 3/2 63/40 8/5 5/3 7/4 9/5 28/15 63/32) 2 (:chalmers))
              ("Lou Harrison" (t 1 16/15 10/9 8/7 7/6 6/5 5/4 4/3 17/12 3/2 8/5 5/3 12/7 7/4 9/5 15/8) 2 (:harrison))
              ("Sruti" (t 1 256/243 16/15 10/9 9/8 32/27 6/5 5/4 81/64 4/3 27/20 45/32 729/512 3/2 128/81 8/5 5/3 27/16 16/9 9/5 15/8 243/128) 2 (:sruti))
              ("Wilfrid Perret" (t 1 21/20 35/32 9/8 7/6 6/5 5/4 21/16 4/3 7/5 35/24 3/2 63/40 8/5 5/3 7/4 9/5 15/8 63/32) 2 (:perret))
              ("Michael Harrison 24 tone 7-limit" (t 1 28/27 135/128 16/15 243/224 9/8 8/7 7/6 32/27 6/5 135/112 5/4 81/64 9/7 21/16 4/3 112/81 45/32 64/45 81/56 3/2 32/21 14/9 128/81 8/5 224/135 5/3 27/16 12/7 7/4 16/9 15/8 243/128 27/14) 2 (:michaelharrison))

              ;; harmonic series -- length arbitary
              ("Harmonic Series 24" (t ,@(iota 24 :start 1)) 2 (:harmonic))

              ;; stretched/shrunk octave
              ("Bohlen-Pierce" ,(mul-iota (/ (ratio-midi 3) 13) 12) 3 (:bp))

              ("Wendy Carlos Alpha" ,(mul-iota 0.78 14) ,(midi-ratio (* 15 0.78)) (:wcAlpha))
              ("Wendy Carlos Beta" ,(mul-iota 0.638 18) ,(midi-ratio (* 19 0.638)) (:wcBeta))
              ("Wendy Carlos Gamma" ,(mul-iota 0.351 33) ,(midi-ratio (* 34 0.351)) (:wcGamma)))))
  (let ((t-tuning (cadr tuning)))
    (apply #'define-tuning
           (car tuning)
           (if (eql t (car t-tuning)) ;; tunings lists that start with t need to be ratio-midi'd.
               (mapcar #'ratio-midi (cdr t-tuning))
               t-tuning)
           (cddr tuning))))

(dolist (scale
          '(;; twelve tones per octave

            ;; 5 note scales
            ("Minor Pentatonic" (0 3 5 7 10) :et12 (:pentatonic))
            ("Major Pentatonic" (0 2 4 7 9) :et12)

            ;; other modes of major pentatonic
            ("Ritusen" (0 2 5 7 9) :et12)
            ("Egyptian" (0 2 5 7 10) :et12)

            ("Kumoi" (0 2 3 7 9) :et12)
            ("Hirajoshi" (0 2 3 7 8) :et12)
            ("Iwato" (0 1 5 6 10) :et12)
            ("Chinese" (0 4 6 7 11) :et12)
            ("Indian" (0 4 5 7 10) :et12)
            ("Pelog" (0 1 3 7 8) :et12)

            ("Prometheus" (0 2 4 6 11) :et12)
            ("Scriabin" (0 1 4 7 9) :et12)

            ;; han chinese pentatonic scales
            ("Gong" (0 2 4 7 9) :et12)
            ("Shang" (0 2 5 7 10) :et12)
            ("Jiao" (0 3 5 8 10) :et12)
            ("Zhi" (0 2 5 7 9) :et12)
            ("Yu" (0 3 5 7 10) :et12)

            ;; 6 note scales
            ("Whole Tone" (0 2 4 6 8 10) :et12 (:whole))
            ("Augmented" (0 3 4 7 8 11) :et12)
            ("Augmented 2" (0 1 4 5 8 9) :et12)

            ;; partch's otonalities and utonalities
            ("Partch Otonality 1" (0 8 14 20 25 34) :partch (:partcho1))
            ("Partch Otonality 2" (0 7 13 18 27 35) :partch (:partcho2))
            ("Partch Otonality 3" (0 6 12 21 29 36) :partch (:partcho3))
            ("Partch Otonality 4" (0 5 15 23 30 37) :partch (:partcho4))
            ("Partch Otonality 5" (0 10 18 25 31 38) :partch (:partcho5))
            ("Partch Otonality 6" (0 9 16 22 28 33) :partch (:partcho6))
            ("Partch Utonality 1" (0 9 18 23 29 35) :partch (:partchu1))
            ("Partch Utonality 2" (0 8 16 25 30 36) :partch (:partchu2))
            ("Partch Utonality 3" (0 7 14 22 31 37) :partch (:partchu3))
            ("Partch Utonality 4" (0 6 13 20 28 38) :partch (:partchu4))
            ("Partch Utonality 5" (0 5 12 18 25 33) :partch (:partchu5))
            ("Partch Utonality 6" (0 10 15 21 27 34) :partch (:partchu6))

            ;; hexatonic modes with no tritone
            ("Hex Major 7" (0 2 4 7 9 11) :et12)
            ("Hex Dorian" (0 2 3 5 7 10) :et12)
            ("Hex Phrygian" (0 1 3 5 8 10) :et12)
            ("Hex Sus" (0 2 5 7 9 10) :et12)
            ("Hex Major 6" (0 2 4 5 7 9) :et12)
            ("Hex Aeolian" (0 3 5 7 8 10) :et12)

            ;; 7 note scales
            ("Major" (0 2 4 5 7 9 11) :et12)
            ("Ionian" (0 2 4 5 7 9 11) :et12)
            ("Dorian" (0 2 3 5 7 9 10) :et12)
            ("Phrygian" (0 1 3 5 7 8 10) :et12)
            ("Lydian" (0 2 4 6 7 9 11) :et12)
            ("Mixolydian" (0 2 4 5 7 9 10) :et12)
            ("Aeolian" (0 2 3 5 7 8 10) :et12)
            ("Natural Minor" (0 2 3 5 7 8 10) :et12 (:minor))
            ("Locrian" (0 1 3 5 6 8 10) :et12)

            ("Harmonic Minor" (0 2 3 5 7 8 11) :et12)
            ("Harmonic Major" (0 2 4 5 7 8 11) :et12)

            ("Melodic Minor" (0 2 3 5 7 9 11) :et12)
            ("Melodic Minor Descending" (0 2 3 5 7 8 10) :et12 (:melodicminordesc))
            ("Melodic Major" (0 2 4 5 7 8 10) :et12)

            ("Bartok" (0 2 4 5 7 8 10) :et12)
            ("Hindu" (0 2 4 5 7 8 10) :et12)

            ;; raga modes
            ("Todi" (0 1 3 6 7 8 11) :et12)
            ("Purvi" (0 1 4 6 7 8 11) :et12)
            ("Marva" (0 1 4 6 7 9 11) :et12)
            ("Bhairav" (0 1 4 5 7 8 11) :et12)
            ("Ahirbhairav" (0 1 4 5 7 9 10) :et12)

            ("Super Locrian" (0 1 3 4 6 8 10) :et12)
            ("Romanian Minor" (0 2 3 6 7 9 10) :et12)
            ("Hungarian Minor" (0 2 3 6 7 8 11) :et12)
            ("Neapolitan Minor" (0 1 3 5 7 8 11) :et12)
            ("Enigmatic" (0 1 4 6 8 10 11) :et12)
            ("Spanish" (0 1 4 5 7 8 10) :et12)

            ;; modes of whole tones with added note
            ("Leading Whole Tone" (0 2 4 6 8 10 11) :et12 (:leadingwhole))
            ("Lydian Minor" (0 2 4 6 7 8 10) :et12)
            ("Neapolitan Major" (0 1 3 5 7 9 11) :et12)
            ("Locrian Major" (0 2 4 5 6 8 10) :et12)

            ;; 8 note scales
            ("Diminished" (0 1 3 4 6 7 9 10) :et12)
            ("Diminished 2" (0 2 3 5 6 8 9 11) :et12)

            ;; 12 note scales
            ("Chromatic" (0 1 2 3 4 5 6 7 8 9 10 11) :et12)

            ;; 24 tones per octave

            ("Chromatic 24" (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23) :et24)

            ;; maqam ajam
            ("Ajam" (0 4 8 10 14 18 22) :et24)
            ("Jiharkah" (0 4 8 10 14 18 21) :et24)
            ("Shawq Afza" (0 4 8 10 14 16 22) :et24)

            ;; maqam sikah
            ("Sikah" (0 3 7 11 14 17 21) :et24)
            ("Sikah Descending" (0 3 7 11 13 17 21) :et24 (:sikahdesc))
            ("Huzam" (0 3 7 9 15 17 21) :et24)
            ("Iraq" (0 3 7 10 13 17 21) :et24)
            ("Bastanikar" (0 3 7 10 13 15 21) :et24)
            ("Mustar" (0 5 7 11 13 17 21) :et24)

            ;; maqam bayati
            ("Bayati" (0 3 6 10 14 16 20) :et24)
            ("Karjighar" (0 3 6 10 12 18 20) :et24)
            ("Husseini" (0 3 6 10 14 17 21) :et24)

            ;; maqam nahawand
            ("Nahawand" (0 4 6 10 14 16 22) :et24)
            ("Nahawand Descending" (0 4 6 10 14 16 20) :et24 (:nahawanddesc))
            ("Farahfaza" (0 4 6 10 14 16 20) :et24)
            ("Murassah" (0 4 6 10 12 18 20) :et24)
            ("Ushaq Mashri" (0 4 6 10 14 17 21) :et24)

            ;; maqam rast
            ("Rast" (0 4 7 10 14 18 21) :et24)
            ("Rast Descending" (0 4 7 10 14 18 20) :et24 (:rastdesc))
            ("Suznak" (0 4 7 10 14 16 22) :et24)
            ("Nairuz" (0 4 7 10 14 17 20) :et24)
            ("Yakah" (0 4 7 10 14 18 21) :et24)
            ("Yakah Descending" (0 4 7 10 14 18 20) :et24 (:yakahdesc))
            ("Mahur" (0 4 7 10 14 18 22) :et24)

            ;; maqam hijaz
            ("Hijaz" (0 2 8 10 14 17 20) :et24)
            ("Hijaz Descending" (0 2 8 10 14 16 20) :et24 (:hijazdesc))
            ("Zanjaran" (0 2 8 10 14 18 20) :et24)

            ;; maqam hijazkar
            ("hijazKar" (0 2 8 10 14 16 22) :et24)

            ;; maqam saba
            ("Saba" (0 3 6 8 12 16 20) :et24)
            ("Zamzam" (0 2 6 8 14 16 20) :et24)

            ;; maqam kurd
            ("Kurd" (0 2 6 10 14 16 20) :et24)
            ("Kijaz Kar Kurd" (0 2 8 10 14 16 22) :et24)

            ;; maqam nawa athar
            ("Nawa Athar" (0 4 6 12 14 16 22) :et24)
            ("Nikriz" (0 4 6 12 14 18 20) :et24)
            ("Athar Kurd" (0 2 6 12 14 16 22) :et24)))
  (apply #'define-scale scale))

(dolist (chord
          `(("Augmented Triad" (0 2 4#) :major (:augtri))
            ("Diminished Triad" (0 2b 4b) :major (:dimtri))
            ("Major Triad" (0 2 4) :major (:major :majtri))
            ("Minor Triad" (0 2b 4) :major (:minor :mintri))
            ("Suspended 2nd Triad" (0 1 4) :major (:sus2tri))
            ("Suspended 4th Triad" (0 3 4) :major (:sus4tri))
            ("Major 6th" (0 2 4 5) :major (:maj6))
            ("Minor 6th" (0 2b 4 5) :major (:min6))
            ("Augmented 7th" (0 2 4# 6b) :major (:aug7))
            ("Augmented Major 7th" (0 2 4# 6) :major (:augmaj7))
            ("Half Diminished 7th" (0 2b 4b 6b) :major (:halfdim7))
            ("Diminished 7th" (0 2b 4b 6bb) :major (:dim7))
            ("Dominant 7th" (0 2 4 6b) :major (:dom7))
            ("Dominant 7th Suspended 4th" (0 3 4 6b) :major (:dom7sus4))
            ("Major 7th" (0 2 4 6) :major (:maj7))
            ("Major 7th Suspended 2nd" (0 1 4 6) :major (:maj7sus2))
            ("Major 7th Suspended 4th" (0 3 4 6) :major (:maj7sus4))
            ("Minor 7th" (0 2b 4 6b) :major (:min7))
            ("Minor Major 7th" (0 2b 4 6) :major (:minmaj7))))
  (apply #'define-chord chord))
