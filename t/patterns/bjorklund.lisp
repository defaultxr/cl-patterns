;;;; t/bjorklund.lisp - tests for `bjorklund', `pbjorklund', and related functions.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test bjorklund
  (is (equal (list 1 0 1 0)
             (bjorklund 2 4))
      "bjorklund gives incorrect results for 2 pulses and 4 steps")
  (is (equal (list 0 1 0 1)
             (bjorklund 2 4 1))
      "bjorklund gives incorrect results for 2 pulses, 4 steps, and 1 offset")
  (is (equal (list 1 0 1 0 1)
             (bjorklund 3 5))
      "bjorklund gives incorrect results for 3 pulses and 5 steps")
  (is (equal (list 1 0 0 1 0 0 0)
             (bjorklund 2 7))
      "bjorklund gives incorrect results for 2 pulses and 7 steps")
  (is (equal (list 1 0 0 1 0 1 0 0 1 0 1 0 0)
             (bjorklund 5 13))
      "bjorklund gives incorrect results for 2 pulses and 4 steps"))

(test pbjorklund
  "Test pbjorklund"
  (for-all* ((pulses (gen-integer :min 1 :max 128))
             (steps (gen-integer :min pulses :max 256)))
    (is (equal (bjorklund pulses steps)
               (mapcar (fn (if (rest-p _) 0 1))
                       (next-upto-n (pbjorklund pulses steps :repeats 1) 400)))
        "pbjorklund and bjorklund give differing results for ~S pulses and ~S steps" pulses steps)))
