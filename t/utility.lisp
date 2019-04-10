(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test gete
  "Test the `gete' function"
  (is (equal (alexandria:iota *max-pattern-yield-length*)
             (gete (next-upto-n (pbind :foo (pseries))) :foo))
      "gete returns correct results"))

(test string-keyword
  "Test the `string-keyword' function"
  (is (eq :foo
          (cl-patterns::string-keyword "foo"))
      "string-keyword correctly converts strings to keywords"))

(test nth-wrap
  "Test the `nth-wrap' function"
  (is (= 3
         (cl-patterns::nth-wrap 7 (list 0 1 2 3)))
      "nth-wrap returns correct results"))

(test normalized-sum
  "Test the `normalized-sum' function"
  (is (= 1
         (reduce #'+ (cl-patterns::normalized-sum (list 0 1 2 3 4 5))))
      "normalized-sum returns a list whose elements add up to 1"))

(test cumulative-list
  "Test the `cumulative-list' function"
  (is (equal (list 0 0 2 4 7)
             (cl-patterns::cumulative-list (list 0 0 2 2 3)))
      "cumulative-list returns correct results"))

(test index-of-greater-than
  "Test the `index-of-greater-than' function"
  (is (= 4
         (cl-patterns::index-of-greater-than 3 (list 1 2 3 3 4 4)))
      "index-of-greater-than returns correct results"))

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
      "plist-set returns correct results")
  (is (null (cl-patterns::plist-set (list :foo :bar) :foo nil))
      "plist-set removes items from the plist when VALUE is nil"))

(test keys
  "Test `keys' and its various methods"
  (is (null (keys nil))
      "keys returns NIL when its input is nil")
  (is (equal (list :foo :baz)
             (keys (list :foo :bar :baz :qux)))
      "keys works correctly for plists")
  (is (let ((hash (make-hash-table)))
        (setf (gethash :foo hash) 1
              (gethash :bar hash) 2
              (gethash :baz hash) 3
              (gethash :qux hash) 4)
        (let ((k (keys hash)))
          (mapcar (lambda (x) (member x k)) (list :foo :bar :baz :qux))))
      "keys works correctly for hashes")
  (is (equal (list :foo :bar)
             (keys (event :foo 1 :bar 2)))
      "keys works correctly for events"))

(test sign
  "Test the `sign' function"
  (is (= 1 (cl-patterns::sign 3))
      "sign works correctly for positive integers")
  (is (= -1 (cl-patterns::sign -4.2))
      "sign works correctly for negative numbers")
  (is (= 0 (cl-patterns::sign 0.0))
      "sign works correctly for zero"))

(test wrap
  "Test the `wrap' function"
  (is (= 3
         (cl-patterns::wrap 3 0 4))
      "wrap doesn't affect numbers within the given range")
  (is (= 3
         (cl-patterns::wrap 7 0 4))
      "wrap correctly wraps numbers outside the given range")
  (is (= 3
         (cl-patterns::wrap 3 1 4))
      "wrap correctly wraps numbers within the given range (when the bottom is non-zero)")
  (is (= -1
         (cl-patterns::wrap 4 -1 4))
      "wrap correctly wraps numbers within the given range (when the bottom is non-zero)"))

(test round-up
  "Test the `round-up' function"
  (is (= 2.04
         (cl-patterns::round-up 2.03 0.02))
      "round-up gives correct results for positive numbers")
  (is (= -2.02
         (cl-patterns::round-up -2.03 0.02))
      "round-up gives correct results for negative numbers")
  (is (= 8
         (cl-patterns::round-up 5 4))
      "round-up gives correct results for arguments 5, 4"))

(test random-coin
  "Test the `random-coin' function"
  (is (every (lambda (x) (eq t x))
             (loop :repeat 200 :collect (random-coin 1)))
      "random-coin always returns t for PROBABILITY of 1")
  (is (every 'null
             (loop :repeat 200 :collect (random-coin 0)))
      "random-coin always returns nil for PROBABILITY of 0"))

(test random-range
  "Test the `random-range' function"
  (is (every (lambda (x) (eq t x))
             (mapcar (lambda (x) (and (>= x 0)
                                      (<= x 10)))
                     (loop :repeat 200
                        :collect (random-range 10))))
      "when only LOW is provided, random-range only returns items between 0 and LOW, inclusive")
  (is (every (lambda (x) (eq t x))
             (mapcar (lambda (x) (and (>= x 0)
                                      (<= x 10)))
                     (loop :repeat 200
                        :collect (random-range 0 10))))
      "random-range only returns items between its LOW and HIGH values, inclusive")
  (is (every (lambda (x) (typep x 'float))
             (loop :repeat 200
                :collect (random-range 200.0)))
      "random-range returns floats when its argument is a float")
  (is (every (lambda (x) (typep x 'float))
             (loop :repeat 200
                :collect (random-range 0 1.0)))
      "random-range returns floats when two arguments are provided but only one is a float"))

(test exponential-random-range
  "Test the `exponential-random-range' function"
  ;; FIX
  )

(test seq
  "Test the `seq' function"
  (is (equal (list 0 1 2 3)
             (seq :start 0 :end 3))
      "seq works correctly when START is lower than END")
  (is (equal (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)
             (seq :start 20 :end 3))
      "seq works correctly when START is higher than END")
  (is (equal (list 0 2 4 6)
             (seq :start 0 :step 2 :end 6))
      "seq works correctly with START, STEP, and END"))

(test seq-range
  "Test the `seq-range' function"
  ;; FIX
  )

(test next-beat-for-quant
  "Test the `next-beat-for-quant' function"
  (is-true (= 0 (next-beat-for-quant 4 0))
           "next-beat-for-quant returns the current beat when applicable")
  (is-true (= 4 (next-beat-for-quant 4 2))
           "next-beat-for-quant returns the next beat and correctly coerces a number to a quant")
  (is-true (= 5 (next-beat-for-quant (list 4 1) 3))
           "next-beat-for-quant correctly takes phase into account")
  (is-true (= 3 (next-beat-for-quant (list 4 -1) 3))
           "next-beat-for-quant correctly takes negative phase into account"))

(test beat
  "Test the `beat' function"
  (is (= 8
         (let ((pstr (as-pstream (pbind :dur (pn 1 8)))))
           (next-upto-n pstr)
           (beat pstr)))
      "beat returns correct results")
  (is-true (equal (list 0 1 2 3)
                  (gete (next-n (pbind :dur 1 :x (pfunc (lambda () (beat *event*)))) 4) :x))
           "beat is accessible and correct in patterns"))
