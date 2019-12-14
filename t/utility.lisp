(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test gete
  "Test the `gete' function"
  (is (equal (iota *max-pattern-yield-length*)
             (gete (next-upto-n (pbind :foo (pseries))) :foo))
      "gete returns incorrect results"))

(test string-keyword
  "Test the `string-keyword' function"
  (is (eql :foo
           (cl-patterns::string-keyword "foo"))
      "string-keyword doesn't convert strings to keywords correctly"))

(test elt-wrap
  "Test the `elt-wrap' function"
  (is (= 3
         (cl-patterns::elt-wrap (list 0 1 2 3) 7))
      "elt-wrap returns incorrect results"))

(test normalized-sum
  "Test the `normalized-sum' function"
  (is (= 1
         (reduce #'+ (cl-patterns::normalized-sum (list 0 1 2 3 4 5))))
      "normalized-sum returns a list whose elements add up to 1"))

(test cumulative-list
  "Test the `cumulative-list' function"
  (is (equal (list 0 0 2 4 7)
             (cl-patterns::cumulative-list (list 0 0 2 2 3)))
      "cumulative-list returns incorrect results"))

(test index-of-greater-than
  "Test the `index-of-greater-than' function"
  (is (= 4
         (cl-patterns::index-of-greater-than 3 (list 1 2 3 3 4 4)))
      "index-of-greater-than returns incorrect results"))

(test flatten-1
  "Test the `flatten-1' function"
  ;; FIX
  )

(test most-x
  "Test the `most-x' function"
  ;; FIX
  )

(test plist-set
  "Test the `plist-set' function"
  (is (equal (list :foo :bar :baz :qux)
             (cl-patterns::plist-set (list :foo :bar) :baz :qux))
      "plist-set returns incorrect results")
  (is (null (cl-patterns::plist-set (list :foo :bar) :foo nil))
      "plist-set doesn't remove items from the plist when VALUE is nil"))

(test keys
  "Test `keys' and its various methods"
  (is (null (keys nil))
      "keys doesn't return NIL when its input is nil")
  (is (equal (list :foo :baz)
             (keys (list :foo :bar :baz :qux)))
      "keys doesn't work correctly for plists")
  (is (let ((hash (make-hash-table)))
        (setf (gethash :foo hash) 1
              (gethash :bar hash) 2
              (gethash :baz hash) 3
              (gethash :qux hash) 4)
        (let ((k (keys hash)))
          (mapcar (lambda (x) (member x k)) (list :foo :bar :baz :qux))))
      "keys doesn't work correctly for hashes")
  (is (equal (list :foo :bar)
             (keys (event :foo 1 :bar 2)))
      "keys doesn't work correctly for events"))

(test sign
  "Test the `sign' function"
  (is (= 1 (cl-patterns::sign 3))
      "sign doesn't work correctly for positive integers")
  (is (= -1 (cl-patterns::sign -4.2))
      "sign doesn't work correctly for negative numbers")
  (is (= 0 (cl-patterns::sign 0.0))
      "sign doesn't work correctly for zero"))

(test wrap
  "Test the `wrap' function"
  (is (= 3
         (cl-patterns::wrap 3 0 4))
      "wrap incorrectly affects numbers within the given range")
  (is (= 3
         (cl-patterns::wrap 7 0 4))
      "wrap doesn't wrap numbers outside the given range correctly")
  (is (= 3
         (cl-patterns::wrap 3 1 4))
      "wrap doesn't wrap numbers within the given range correctly (when the bottom is non-zero)")
  (is (= -1
         (cl-patterns::wrap 4 -1 4))
      "wrap doesn't wrap numbers within the given range correctly (when the bottom is non-zero)"))

(test round-by
  "Test the `round-by' function"
  (is (= 3.5
         (cl-patterns::round-by 3.26 0.5))
      "round-by gives incorrect results")
  (is (= 3.25
         (cl-patterns::round-by 3.25 0.25))
      "round-by gives incorrect results when its input is a multiple of the divisor"))

(test round-by-direction
  "Test the `round-by-direction' function"
  (is-true (= 2.04
              (cl-patterns::round-by-direction 2.03 0.02))
           "round-by-direction gives incorrect results for positive numbers")
  (is-true (= -2.02
              (cl-patterns::round-by-direction -2.03 0.02))
           "round-by-direction gives incorrect results for negative numbers")
  (is-true (= 8
              (cl-patterns::round-by-direction 5 4))
           "round-by-direction gives incorrect results for arguments 5, 4")
  (is-true (= 3.5
              (cl-patterns::round-by-direction 3.9 -0.5))
           "round-by-drection gives incorrect results when rounding down")
  (is-true (= (+ 2 1/10)
              (cl-patterns::round-by-direction (+ 2 19/100) -1/10))
           "round-by-direction gives incorrect results for rounding down ratios"))

(test random-coin
  "Test the `random-coin' function"
  (is (every (lambda (x) (eql t x))
             (loop :repeat 200 :collect (random-coin 1)))
      "random-coin returned a non-t value even though the PROBABILITY was 1")
  (is (every 'null
             (loop :repeat 200 :collect (random-coin 0)))
      "random-coin returned a non-nil value even though the PROBABILITY was 0"))

(test random-range
  "Test the `random-range' function"
  (let ((result (loop :repeat 200
                   :collect (random-range 10))))
    (is (every (lambda (x) (eql t x))
               (mapcar (lambda (x) (and (>= x 0)
                                        (<= x 10)))
                       result))
        "random-range did not return only items between 0 and LOW, inclusive, when only one argument was provided (result: ~a)"
        result))
  (let ((result (loop :repeat 200
                   :collect (random-range 0 10))))
    (is (every (lambda (x) (eql t x))
               (mapcar (lambda (x) (and (>= x 0)
                                        (<= x 10)))
                       result))
        "random-range did not return only items between LOW and HIGH values, inclusive (result: ~a)"
        result))
  (let ((result (loop :repeat 200
                   :collect (random-range 200.0))))
    (is (every (lambda (x) (typep x 'float))
               result)
        "random-range returned a non-float when its argument was a float (result: ~a)"
        result))
  (let ((result (loop :repeat 200
                   :collect (random-range 0 1.0))))
    (is (every (lambda (x) (typep x 'float))
               result)
        "random-range returned a non-float even though one of its arguments was a float (result: ~a)"
        result)))

(test exponential-random-range
  "Test the `exponential-random-range' function"
  ;; FIX
  )

(test seq
  "Test the `seq' function"
  (is (equal (list 0 1 2 3)
             (seq :start 0 :end 3))
      "seq doesn't work correctly when START is lower than END")
  (is (equal (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)
             (seq :start 20 :end 3))
      "seq doesn't work correctly when START is higher than END")
  (is (equal (list 0 2 4 6)
             (seq :start 0 :step 2 :end 6))
      "seq doesn't work correctly with START, STEP, and END"))

(test seq-range
  "Test the `seq-range' function"
  ;; FIX
  )

(test next-beat-for-quant
  "Test the `next-beat-for-quant' function"
  (is-true (= 0 (next-beat-for-quant 4 0))
           "next-beat-for-quant returned the wrong result")
  (is-true (= 4 (next-beat-for-quant 4 2))
           "next-beat-for-quant returned the wrong result")
  (is-true (= 5 (next-beat-for-quant (list 4 1) 3))
           "next-beat-for-quant returned the wrong result for a QUANT with phase")
  (is-true (= 3 (next-beat-for-quant (list 4 -1) 3))
           "next-beat-for-quant returned the wrong result for a QUANT with negative phase")
  (is-true (= 1 (next-beat-for-quant 0.25 1.1 -1))
           "next-beat-for-quant returned the wrong result for a negative DIRECTION")
  ;; FIX: test more of the negative DIRECTION
  )

(test beat
  "Test the `beat' function"
  (is (= 8
         (let ((pstr (as-pstream (pbind :dur (pn 1 8)))))
           (next-upto-n pstr)
           (beat pstr)))
      "beat returns incorrect results")
  (is-true (equal (list 0 1 2 3)
                  (gete (next-n (pbind :dur 1 :x (pfunc (lambda () (beat *event*)))) 4) :x))
           "*event*'s beat is not correct in patterns"))
