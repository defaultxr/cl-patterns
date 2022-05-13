;;;; t/scales.lisp - tests for music theory-related functionality such as notes, tunings, scales, and chords.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test tuning
  "Test tuning functionality"
  (is (= 12 (tuning-steps-per-octave :pyth))
      "tuning-steps-per-octave is incorrect for the pythagorean scale")
  (is (= 19 (tuning-steps-per-octave :et19))
      "tuning-steps-per-octave is incorrect for the equal temperament 19 scale")
  ;; FIX: add more
  )

(test scale
  "Test scale functionality"
  (is (= 12 (scale-steps-per-octave :major))
      "scale-steps-per-octave is incorrect for the major scale")
  (is (= 24 (scale-steps-per-octave :ajam))
      "scale-steps-per-octave is incorrect for the ajam scale")
  (is (equal (list 0 2 4 5 7 9 11 12 14 16 17 19 21 23 24 26 28 29 31 33 35 36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72 74 76 77 79 81 83 84 86 88 89 91 93 95 96 98 100 101 103 105 107 108 110 112 113 115 117 119)
             (scale-midinotes :major :octave :all))
      "scale-midinotes is not correct for :major :octave :all")
  ;; FIX: add more
  )

(test chords
  "Test chord functionality"
  (is (typep (chord :major) 'chord)
      "chord doesn't return a chord object for :major as input")
  (is (equal (list 0 2 4)
             (chord-indexes (chord :major)))
      "major chord has incorrect indexes")
  (is (equal (list 0 4 7)
             (chord-note-numbers (chord :major)))
      "major chord has incorrect note numbers")
  ;; FIX: add more
  )

