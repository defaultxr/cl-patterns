;;;; t/conversions.lisp - tests for unit conversion functions.
;;; TODO:
;; FIX: add more.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;; NOTE: Many of the conversion functions are affected by floating point rounding errors.
;; This is why we use `mutility:approx=' for many of these comparisons rather than `cl:='.

(test amplitude-conversion
  "Test amplitude conversion functions"
  (is-true (let ((input (iota 10 :start 1/10 :step 1/10)))
             (every #'identity (mapcar #'approx=
                                       (mapcar #'db-amp (mapcar #'amp-db input))
                                       input)))
           "db-amp conversion is not equivalent to amp-db conversion"))

(test time-conversion
  "Test time conversion functions"
  (is-true (let ((input (iota 22 :step 1/10)))
             (equal input (mapcar #'time-dur (mapcar #'dur-time input))))
           "time-dur conversion is not equivalent to dur-time conversion"))

(test pitch-midinote-conversion
  "Test pitch/midinote conversion functions"
  (is (every #'identity (mapcar #'=
                                (list 13.75 27.5 55 110 220 440 880 3520)
                                (mapcar #'midinote-freq (list 9 21 33 45 57 69 81 105))))
      "midinote-freq conversion is not correct")
  (is-false (let ((input (iota 128)))
              (position nil (mapcar #'approx=
                                    input
                                    (mapcar #'freq-midinote (mapcar #'midinote-freq input)))))
            "freq-midinote conversion is not equivalent to midinote-freq conversion")
  )

(test pitch-octave-conversion
  "Test pitch/octave conversion functions"
  (is (= 4 (midinote-octave *c4-midinote*))
      "midinote-octave conversion is not correct for note :c4")
  (is (= 3 (midinote-octave (- *c4-midinote* 12)))
      "midinote-octave conversion is not correct for note :c3")
  (is (= 5 (midinote-octave (+ 14 *c4-midinote*)))
      "midinote-octave conversion is not correct for note :d5")
  (is (= *c4-midinote* (octave-midinote 4))
      "octave-midinote is not correct for octave 4")
  (is (= (+ 12 *c4-midinote*) (octave-midinote 5))
      "octave-midinote is not correct for octave 5")
  (is (= (+ -12 *c4-midinote*) (octave-midinote 3))
      "octave-midinote is not correct for octave 3"))

(test degree-key
  "Test the `degree-key' function"
  ;; FIX: check degree-key on some non-12ET scales
  (is (= 4 (degree-key 2 :scale :major :steps-per-octave 13))
      "(degree-key 2 :scale :major :steps-per-octave 13) is not correct")
  (is (= 9 (degree-key 8 :scale :minor :steps-per-octave 7))
      "(degree-key 8 :scale :minor :steps-per-octave 7) is not correct")
  (is (= -14 (degree-key -4 :scale :minor :steps-per-octave 19))
      "(degree-key -4 :scale :minor :steps-per-octave 19) is not correct"))

(test pitch-note-conversion
  "Test pitch/note conversion functions"
  (is (= *c4-midinote* (note-midinote :c4))
      "note-midinote is incorrect for note :c4")
  (is (= (+ 13 *c4-midinote*) (note-midinote :c#5))
      "note-midinote is incorrect for note :c#5")
  (let ((*c4-midinote* 60))
    (is (= 60 (note-midinote :c4))
        "note-midinote is incorrect for note :c4 when *c4-midinote* is set to 60")
    (is (string= :c4 (midinote-note 60))
        "midinote-note is incorrect for midinote 60 when *c4-midinote* is set to 60"))
  (let ((*c4-midinote* 48))
    (is (= 48 (note-midinote :c4))
        "note-midinote is incorrect for note :c4 when *c4-midinote* is set to 48")
    (is (string= :c4 (midinote-note 48))
        "midinote-note is incorrect for midinote 48 when *c4-midinote* is set to 48"))
  (let ((*c4-midinote* 69))
    (is (= 69 (note-midinote :c4))
        "note-midinote is incorrect for note :c4 when *c4-midinote* is set to 69")
    (is (string= :a4 (midinote-note 69))
        "midinote-note is incorrect for midinote 69 when *c4-midinote* is set to 69")))

(test pitch-chromatic-index-conversion
  "Test pitch/chromatic-index conversion functions"
  (is (eql :c (chromatic-index-note 0))
      "chromatic-index-note is incorrect for chromatic index 0")
  (is (= 0 (note-chromatic-index :c))
      "note-chromatic-index is incorrect for note :c")
  (is (= 0 (note-chromatic-index :c5))
      "note-chromatic-index is incorrect for note :c5")
  (is (= 9 (note-chromatic-index :a5))
      "note-chromatic-index is incorrect for note :c5"))

(test pitch-degree-conversion
  "Test pitch/degree conversion functions"
  (is (= (degree-freq 0)
         (midinote-freq (degree-midinote 0)))
      "degree-freq and degree-midinote are not equivalent")
  (for-all ((degree (gen-integer :min 0 :max (reduce #'max (mapcar (fn (length (scale-notes _)))
                                                                   (all-scales)))))
            (scale (lambda () (random-elt (all-scales)))))
    (let ((note (degree-note degree :scale scale)))
      (is-true (= (mod degree (length (scale-notes scale)))
                  (note-degree note :scale scale))
               "degree-note and note-degree are not equivalent"))))

(test rate-conversion
  "Test rate conversion functions"
  (is (equal (list 55 110 220 440 880 1760)
             (mapcar #'rate-freq (list 1/8 1/4 1/2 1 2 4)))
      "rate-freq conversion is not correct")
  (is (= 300
         (rate-freq 1 300))
      "rate-freq conversion is not correct when base-freq is provided")
  (is (equal (list 1/8 1/4 1/2 1 2 4)
             (mapcar #'freq-rate (list 55 110 220 440 880 1760)))
      "freq-rate conversion is not correct")
  (is (= 2
         (freq-rate 2600 1300))
      "freq-rate conversion is not correct when base-freq is provided"))
