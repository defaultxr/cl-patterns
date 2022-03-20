(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;;; t/utility.lisp - tests for cl-patterns utility functions.

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

(test mapcar-longest
  "Test the `mapcar-longest' function"
  (is (equal (list 1 3 3)
             (cl-patterns::mapcar-longest #'+ (list 0 1) (list 1) (list 0 1 2)))
      "mapcar-longest returns incorrect results")
  (is (equal (list 0 2 2 4 4 6)
             (cl-patterns::mapcar-longest #'+ (list 0 1) (list 0 1 2 3 4 5)))
      "mapcar-longest doesn't wrap indexes of the shorter lists correctly")
  (is (equal (list (list 1 1) t)
             (multiple-value-list (multi-channel-funcall #'dur (list (event :delta 1) (event :delta 2)))))
      "mapcar-longest doesn't include additional return values from its last call"))

(test last-dur
  "Test the `last-dur' function"
  (is (= 4 (cl-patterns::last-dur (list (event :dur 3 :beat 1) (event :dur 2 :beat 0))))
      "last-dur returns incorrect results"))

(test multi-channel-funcall
  "Test the `multi-channel-funcall' function"
  (is (equal 3
             (cl-patterns::multi-channel-funcall #'+ 2 1))
      "multi-channel-funcall doesn't return a lone value when all its inputs are lone values"))

(test plist-set
  "Test the `plist-set' function"
  (is (equal (list :foo :bar :baz :qux)
             (cl-patterns::plist-set (list :foo :bar) :baz :qux))
      "plist-set returns incorrect results"))

(test pyramid
  "Test the `pyramid' function"
  (let ((list (iota 11)))
    (let ((out (pyramid list 1))
          (res (list 0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5 0 1 2 3 4 5 6 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 10)))
      (is (equal out res)
          "(pyramid x 1) is not correct"))
    (let ((out (pyramid list 2))
          (res (list 10 9 10 8 9 10 7 8 9 10 6 7 8 9 10 5 6 7 8 9 10 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 10)))
      (is (equal out res)
          "(pyramid x 2) is not correct"))
    (let ((out (pyramid list 3))
          (res (list 0 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 0 1 2 3 4 5 0 1 2 3 4 0 1 2 3 0 1 2 0 1 0)))
      (is (equal out res)
          "(pyramid x 3) is not correct"))
    (let ((out (pyramid list 4))
          (res (list 0 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 2 3 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10 4 5 6 7 8 9 10 5 6 7 8 9 10 6 7 8 9 10 7 8 9 10 8 9 10 9 10 10)))
      (is (equal out res)
          "(pyramid x 4) is not correct"))
    (let ((out (pyramid list 5))
          (res (list 0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5 0 1 2 3 4 5 6 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 0 1 2 3 4 5 0 1 2 3 4 0 1 2 3 0 1 2 0 1 0)))
      (is (equal out res)
          "(pyramid x 5) is not correct"))
    (let ((out (pyramid list 6))
          (res (list 10 9 10 8 9 10 7 8 9 10 6 7 8 9 10 5 6 7 8 9 10 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 2 3 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10 4 5 6 7 8 9 10 5 6 7 8 9 10 6 7 8 9 10 7 8 9 10 8 9 10 9 10 10)))
      (is (equal out res)
          "(pyramid x 6) is not correct"))
    (let ((out (pyramid list 7))
          (res (list 0 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 0 1 2 3 4 5 0 1 2 3 4 0 1 2 3 0 1 2 0 1 0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5 0 1 2 3 4 5 6 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 10)))
      (is (equal out res)
          "(pyramid x 7) is not correct"))
    (let ((out (pyramid list 8))
          (res (list 0 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 2 3 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10 4 5 6 7 8 9 10 5 6 7 8 9 10 6 7 8 9 10 7 8 9 10 8 9 10 9 10 10 9 10 8 9 10 7 8 9 10 6 7 8 9 10 5 6 7 8 9 10 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 10)))
      (is (equal out res)
          "(pyramid x 8) is not correct"))
    (let ((out (pyramid list 9))
          (res (list 0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5 0 1 2 3 4 5 6 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 2 3 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10 4 5 6 7 8 9 10 5 6 7 8 9 10 6 7 8 9 10 7 8 9 10 8 9 10 9 10 10)))
      (is (equal out res)
          "(pyramid x 9) is not correct"))
    (let ((out (pyramid list 10))
          (res (list 10 9 10 8 9 10 7 8 9 10 6 7 8 9 10 5 6 7 8 9 10 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 0 1 2 3 4 5 0 1 2 3 4 0 1 2 3 0 1 2 0 1 0)))
      (is (equal out res)
          "(pyramid x 10) is not correct"))))

(test near-p
  "Test the `near-p' function"
  (is-true (cl-patterns::near-p 4 1 5)
           "near-p says 4 is not within 1 of 5")
  (is-false (cl-patterns::near-p 4 1)
            "near-p says 4 is within 1 of 0")
  (is-true (cl-patterns::near-p 0.5 1 0)
           "near-p says 0.5 is not within 1 of 0")
  (is-true (cl-patterns::near-p 0.5 0.6 1)
           "near-p says 0.5 is not within 0.6 of 1"))

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

(test find-object-by-id
  "Test the `find-object-by-id' function"
  (with-fixture temporary-pdef-dictionary ()
    (is-false (cl-patterns::find-object-by-id '#:does-not-exist :default nil)
              "find-object-by-id doesn't return nil for undefined keys")
    (signals (simple-error "find-object-by-id doesn't return raise an error for undefined keys when :default is :error")
      (cl-patterns::find-object-by-id '#:does-not-exist :default :error))
    (pdef :foo (pn 1 1))
    (is-true (cl-patterns::find-object-by-id :foo)
             "find-object-by-id doesn't return true for defined keys")
    (let ((cl-patterns::*dictionary-lookup-functions* (list 'find-pdef)))
      (is (eq (find-pdef :foo)
              (cl-patterns::find-object-by-id :foo))
          "find-object-by-id doesn't return the object itself when one is defined"))))

(test tempo
  "Test the `tempo' function"
  ;; FIX
  )

(test beat
  "Test the `beat' function"
  (is (= 8
         (let ((pstr (as-pstream (pbind :dur (pn 1 8)))))
           (next-upto-n pstr)
           (beat pstr)))
      "beat returns incorrect results")
  (is-true (equal (list 0 1 2 3)
                  (mapcar (fn (event-value _ :x))
                          (next-n (pbind :dur 1 :x (pfunc (lambda () (beat *event*)))) 4)))
           "*event*'s beat is not correct in patterns"))

(test quant
  "Test the `quant' function"
  ;; FIX
  )

(test play-quant
  "Test the `play-quant' function"
  ;; FIX
  )

(test end-quant
  "Test the `end-quant' function"
  ;; FIX
  )

(test rest-p
  "Test the `rest-p' function"
  ;; FIX
  )

(test play
  "Test the `play' function"
  ;; FIX
  )

(test launch
  "Test the `launch' function"
  ;; FIX
  )

(test stop
  "Test the `stop' function"
  ;; FIX
  )

(test end
  "Test the `end' function"
  ;; FIX
  )

(test eop-p
  "Test the `eop-p' function"
  (is-true (eop-p eop)
           "eop-p is incorrect for the eop object")
  (is-true (if eop
               (null (eop-p nil))
               (eop-p nil))
           "eop-p is incorrect for nil")
  (is-false (eop-p (event))
            "eop-p is incorrect for empty events")
  (is-false (eop-p (event :foo 1))
            "eop-p is incorrect for non-empty events")
  (is-true (eop-p (event :foo 1 :bar eop))
           "eop-p is incorrect for events with eop keys")
  (is-false (eop-p (random-range 0 100))
            "eop-p is incorrect for numbers"))

(test playing-p
  "Test the `playing-p' function"
  ;; FIX
  )

(test loop-p
  "Test the `loop-p' function"
  ;; FIX
  )

(test ended-p
  "Test the `ended-p' function"
  (let ((pstr (as-pstream (pseq (list 1 2 3) 1))))
    (next-n pstr 2)
    (is-false (ended-p pstr)
              "ended-p returns incorrect results for a non-ended pseq")
    (next-n pstr 1)
    (is-false (ended-p pstr)
              "ended-p returns incorrect results for a non-ended pseq")
    (next-n pstr 1)
    (is-true (ended-p pstr)
             "ended-p returns incorrect results for an ended pseq")))

(test play-or-stop
  "Test the `play-or-stop' function"
  ;; FIX
  )

(test play-or-end
  "Test the `play-or-end' function"
  ;; FIX
  )

(test all-instruments
  "Test the `all-instruments' function"
  ;; FIX
  )

(test playing-nodes
  "Test the `playing-nodes' function"
  ;; FIX
  )

(test render
  "Test the `render' function"
  ;; FIX
  )

(test keys
  "Test cl-patterns `keys' methods"
  (is (equal (list :foo :bar)
             (keys (event :foo 1 :bar 2)))
      "keys doesn't work correctly for events")
  (is (equal (list :foo :bar)
             (keys (pbind :foo 1 :bar 2)))
      "keys doesn't work correctly for pbinds")
  (with-fixture temporary-pdef-dictionary ()
    (pdef :test-pat (pbind :foo 1 :bar :2))
    (is (equal (list :foo :bar)
               (keys (pdef :test-pat)))
        "keys doesn't work correctly for pdefs")))
