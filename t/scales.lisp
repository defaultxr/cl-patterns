(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; scales (FIX)

(test chords
  "Test chord functionality"
  (is (typep (chord :major) 'chord)
      "chord function returns a chord object for :major as input")
  (is (equal (list 0 2 4)
             (chord-indexes (chord :major)))
      "Major chord has correct indexes")
  (is (equal (list 0 4 7)
             (chord-note-numbers (chord :major)))
      "Major chord has correct note numbers")
  ;; FIX: add more
  )

