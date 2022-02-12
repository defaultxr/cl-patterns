(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;;; t/scales.lisp - tests for music theory-related functionality such as notes, tunings, scales, and chords.

;;; scales (FIX)

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

