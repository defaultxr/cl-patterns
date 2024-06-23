;;;; t/scales.lisp - tests for music theory-related functionality such as notes, tunings, scales, and chords.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test tone-matrix
  "Test tone matrix functionality"
  (let ((input '(3 7 e 5 1 0 2 4 6 8 t 9)))
    (is (equal '((3 7 e 5 1 0 2 4 6 8 t 9)
                 (e 3 7 1 9 8 t 0 2 4 6 5)
                 (7 e 3 9 5 4 6 8 t 0 2 1)
                 (1 5 9 3 e t 0 2 4 6 8 7)
                 (5 9 1 7 3 2 4 6 8 t 0 e)
                 (6 t 2 8 4 3 5 7 9 e 1 0)
                 (4 8 0 6 2 1 3 5 7 9 e t)
                 (2 6 t 4 0 e 1 3 5 7 9 8)
                 (0 4 8 2 t 9 e 1 3 5 7 6)
                 (t 2 6 0 8 7 9 e 1 3 5 4)
                 (8 0 4 t 6 5 7 9 e 1 3 2)
                 (9 1 5 e 7 6 8 t 0 2 4 3))
               (tone-matrix input))
        "tone-matrix is incorrect for ~S" input))
  (let ((input '(5 6 9 8 7 4 3 11 0 1 2 10)))
    (is (equal '((5 6 9 8 7 4 3 11 0 1 2 10)
                 (4 5 8 7 6 3 2 10 11 0 1 9)
                 (1 2 5 4 3 0 11 7 8 9 10 6)
                 (2 3 6 5 4 1 0 8 9 10 11 7)
                 (3 4 7 6 5 2 1 9 10 11 0 8)
                 (6 7 10 9 8 5 4 0 1 2 3 11)
                 (7 8 11 10 9 6 5 1 2 3 4 0)
                 (11 0 3 2 1 10 9 5 6 7 8 4)
                 (10 11 2 1 0 9 8 4 5 6 7 3)
                 (9 10 1 0 11 8 7 3 4 5 6 2)
                 (8 9 0 11 10 7 6 2 3 4 5 1)
                 (0 1 4 3 2 11 10 6 7 8 9 5))
               (tone-matrix input))
        "tone-matrix is incorrect for ~S" input))
  (let ((input '(0 2 1 3)))
    (is (equal '((0 2 1 3)
                 (2 0 3 1)
                 (3 1 0 2)
                 (1 3 2 0))
               (tone-matrix input))
        "tone-matrix is incorrect for ~S" input)
    (is (equal '((3 1 2 0)
                 (1 3 0 2)
                 (2 0 1 3)
                 (0 2 3 1))
               (tone-matrix input :retrograde))
        "tone-matrix is incorrect for ~S ~S" input :retrograde)
    (is (equal '((0 2 3 1)
                 (2 0 1 3)
                 (1 3 0 2)
                 (3 1 2 0))
               (tone-matrix input :inversion))
        "tone-matrix is incorrect for ~S ~S" input :inversion)
    (is (equal '((1 3 2 0)
                 (3 1 0 2)
                 (2 0 3 1)
                 (0 2 1 3))
               (tone-matrix input :retrograde-inversion))
        "tone-matrix is incorrect for ~S ~S" input :retrograde-inversion))
  (let ((input '(0 1 2)))
    (is (equal '((0 1 2)
                 (2 0 1)
                 (1 2 0))
               (tone-matrix input))
        "tone-matrix is incorrect for ~S" input)
    (is (equal '((0 2 1)
                 (1 0 2)
                 (2 1 0))
               (tone-matrix input :inversion))
        "tone-matrix is incorrect for ~S ~S" input :inversion)
    (is (equal '((2 1 0)
                 (1 0 2)
                 (0 2 1))
               (tone-matrix input :retrograde))
        "tone-matrix is incorrect for ~S ~S" input :retrograde)
    (is (equal '((1 2 0)
                 (2 0 1)
                 (0 1 2))
               (tone-matrix input :retrograde-inversion))
        "tone-matrix is incorrect for ~S ~S" input :retrograde-inversion)))

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
  (is (equal (list 0 2 4 5 7 9 11 12 14 16
                   17 19 21 23 24 26 28 29 31 33
                   35 36 38 40 41 43 45 47 48 50
                   52 53 55 57 59 60 62 64 65 67
                   69 71 72 74 76 77 79 81 83 84
                   86 88 89 91 93 95 96 98 100 101
                   103 105 107 108 110 112 113 115 117 119)
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

