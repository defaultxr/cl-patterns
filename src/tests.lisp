(defpackage #:cl-patterns/tests
  (:use :cl
        :cl-patterns
        :fiveam))

(in-package #:cl-patterns/tests)

(def-suite cl-patterns-tests
    :description "cl-patterns tests suite.")

(in-suite cl-patterns-tests)

;;; utility

(test gete
  (is (equal (alexandria:iota *max-pattern-yield-length*)
             (gete (next-upto-n (pbind :foo (pseries))) :foo))
      "gete returns correct results"))

(test string-keyword
  (is (eq :foo
          (cl-patterns::string-keyword "foo"))
      "string-keyword correctly converts strings to keywords"))

(test nth-wrap
  (is (= 3
         (cl-patterns::nth-wrap 7 (list 0 1 2 3)))
      "nth-wrap returns correct results"))

(test normalized-sum
  (is (= 1
         (reduce #'+ (cl-patterns::normalized-sum (list 0 1 2 3 4 5))))
      "normalized-sum returns a list whose elements add up to 1"))

(test index-of-greater-than
  (is (= 4
         (cl-patterns::index-of-greater-than 3 (list 1 2 3 3 4 4)))
      "index-of-greater-than returns correct results"))

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

(test seq
  "Test the 'seq' function"
  (is (equal (list 0 1 2 3)
             (seq :start 0 :end 3))
      "seq works correctly when START is lower than END")
  (is (equal (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)
             (seq :start 20 :end 3))
      "seq works correctly when START is higher than END")
  (is (equal (list 0 2 4 6)
             (seq :start 0 :step 2 :end 6))
      "seq works correctly with START, STEP, and END"))

;;; conversions (FIX: add more)
;; NOTE: Many of the conversion functions are affected by floating point rounding errors.
;; This is why only some numbers are tested for db-amp, freq-midinote, etc.
;; The point here is mainly to guard against regressions, not to ensure that all functions have mathematically correct results.
;; Because as far as I know, it's not possible--at least in SBCL--to get more accurate results than what we have currently.

(test conversions
  "Test unit conversion functions"
  (is (equal
       (list 0.1 0.5 0.7 0.8 0.9 1.0)
       (mapcar #'db-amp (mapcar #'amp-db (list 0.1 0.5 0.7 0.8 0.9 1.0))))
      "db-amp conversion is equivalent to amp-db conversion")
  (is (equal
       (alexandria:iota 22 :step 1/10)
       (mapcar #'time-dur (mapcar #'dur-time (alexandria:iota 22 :step 1/10))))
      "time-dur conversion is equivalent to dur-time conversion")
  (is-false (let ((input (remove-if (lambda (n) (= n 20)) ;; 20 is the only input that has rounding errors. So close to perfect...!
                                    (alexandria:iota 128))))
              (position-if #'null (mapcar #'=
                                          input
                                          (mapcar #'freq-midinote (mapcar #'midinote-freq input)))))
            "freq-midinote conversion is equivalent to midinote-freq conversion")
  (is (every (lambda (x) (eq x t))
             (mapcar #'=
                     (list 13.75 27.5 55 110 220 440 880 3520)
                     (mapcar 'midinote-freq (list 9 21 33 45 57 69 81 105))))
      "midinote-freq conversion is correct")
  (is (equal
       (loop :for i :from 0 :upto 10
          :append (make-list 12 :initial-element i))
       (mapcar #'midinote-octave (alexandria:iota 132)))
      "midinote-octave conversion is correct")
  (is (every (lambda (x) (eq x t))
             (mapcar #'=
                     (list 55 110 220 440 880 1760)
                     (mapcar #'rate-freq (list 1/8 0.25 0.5 1 2 4))))
      "rate-freq conversion is correct")
  (is (= 300
         (rate-freq 1 300))
      "rate-freq conversion is correct when base-freq is provided")
  (is (every (lambda (x) (eq x t))
             (mapcar #'=
                     (list 1/8 0.25 0.5 1 2 4)
                     (mapcar #'freq-rate (list 55 110 220 440 880 1760))))
      "freq-rate conversion is correct")
  (is (= 2
         (freq-rate 2600 1300))
      "freq-rate conversion is correct when base-freq is provided"))

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

;;; event (FIX: add more)

(test event
  "Test event functionality"
  (is (=
       1
       (event-value (event :dur 0 :sustain 1) :sustain))
      "event returns the correct sustain when sustain is provided and dur is 0")
  (is (=
       0.8
       (event-value (event) :sustain))
      "event returns the correct default value for sustain")
  (is (=
       0.5
       (event-value (event :dur 0 :legato 0.5) :legato))
      "event returns the correct legato when legato is provided and dur is 0")
  (is (=
       0.8
       (event-value (event) :legato))
      "event returns the correct default value for legato")
  (is (=
       1
       (event-value (event) :dur))
      "event returns the correct default value for dur")
  (is (eq
       :default
       (event-value (event) :instrument))
      "event returns the correct default value for instrument")
  (is (= (amp-db 0.125)
         (event-value (event :amp 0.125) :db))
      "event correctly converts amp to db")
  (is (= (db-amp -7)
         (event-value (event :db -7) :amp))
      "event correctly converts db to amp"))

(test event-equal
  "Test event-equal"
  (is-true
   (event-equal (event :dur 1) (event :dur 1))
   "event-equal returns true for equivalent events"))

(test every-event-equal
  "Test every-event-equal"
  (is-true (every-event-equal
            (list (event :freq 440))
            (list (event :freq 440)))
           "every-event-equal returns true for two lists of equivalent events")
  (is-false (every-event-equal
             (list (event :dur 1))
             (list))
            "every-event-equal returns false for two lists of different length"))

;;; patterns

(test embedding
  "Test embedding patterns in patterns"
  (is (equal
       (list 0 1 2 3 4 5 nil)
       (next-n (pseq (list 0 (pseq (list 1 (pseq (list 2 3) 1) 4) 1) 5) 1) 7))
      "Stacked pseqs give correct results"))

(test remaining-key
  "Test the remaining key in patterns and pstreams"
  (is (= 5
         (length (next-upto-n (pbind :remaining 5 :foo 1))))
      ":remaining key functions properly when specified in pbind")
  (is (= 5
         (length (next-upto-n (let ((pat (pseq '(1 2 3) :inf)))
                                (setf (slot-value pat 'cl-patterns::remaining) 5)
                                pat))))
      "remaining slot functions properly when setf"))

(test number-key
  "Test the number key in patterns and pstreams"
  (is (equal (list 0 1 2 3 4 5 6 7)
             (let ((pstr (as-pstream (pbind :foo 1))))
               (loop :for i :upto 7
                  :collect (slot-value pstr 'cl-patterns::number)
                  :do (next pstr))))
      "number slot functions properly for patterns")
  (is (equal (list 0 1 2 3 4 5 6 7)
             (let ((pstr (as-pstream (pbind :foo (pk :number)))))
               (mapcar (lambda (e) (event-value e :foo)) (next-upto-n pstr 8))))
      "pstream's number can be accessed by pk"))

(test peek
  "Test peek functionality"
  (is-false (position nil (let ((pstr (as-pstream (pwhite 0 127))))
                            (loop :for i :upto 200 :collect (= (peek pstr) (next pstr)))))
            "peek and next return the same values")
  (is (= 0
         (let ((pstr (as-pstream (pbind :dur 1/3 :foo (pseq '(1 2 3) 1)))))
           (peek pstr)
           (beats-elapsed pstr)))
      "beats-elapsed should not count peeked outputs"))

(test pbind
  "Test pbind functionality"
  (is (equal (list :foo :bar :baz)
             (cl-patterns::keys (next (pbind :foo 1 :bar 2 :baz (pseq '(1 2 3) 1)))))
      "pbind returns events that only have the keys specified")
  (is (= 3
         (length (next-upto-n (pbind :foo 1 :bar 2 :baz (pseq '(1 2 3) 1)))))
      "pbind returns the correct number of events")
  (is (= 77
         (let ((*max-pattern-yield-length* 77))
           (length (next-upto-n (pbind :foo 1 :bar 2)))))
      "pbind returns the correct number of events"))

(test parent ;; FIX: make sure all patterns are given parents
  "Test whether patterns have the correct parent information"
  (is-true (let ((pb (pbind :foo (pseq '(1 2 3)))))
             (eq (cl-patterns::parent-pattern (getf (slot-value pb 'cl-patterns::pairs) :foo))
                 pb))
           "pbind subpatterns have correct parents for pseq")
  (is-true (let ((pb (pbind :foo (pfunc (lambda () (random 5))))))
             (eq (cl-patterns::parent-pattern (getf (slot-value pb 'cl-patterns::pairs) :foo))
                 pb))
           "pbind subpatterns have correct parents for pfunc"))

(test beats-elapsed
  ;; FIX
  )

(test special-keys
  "Test pbind special keys"
  (is-true (every-event-equal
            (list (event :bar 1 :qux 69 :dur 1/3)
                  (event :bar 1 :qux 69 :dur 1/3)
                  (event :bar 1 :qux 69 :dur 1/3)
                  (event :bar 2 :qux 420 :dur 1/2)
                  (event :bar 2 :qux 420 :dur 1/2)
                  (event :bar 2 :qux 666 :dur 1)
                  (event :bar 3 :qux 69 :dur 1/3)
                  (event :bar 3 :qux 69 :dur 1/3)
                  (event :bar 3 :qux 69 :dur 1/3)
                  (event :bar 3 :qux 420 :dur 1/2)
                  (event :bar 3 :qux 420 :dur 1/2)
                  (event :bar 3 :qux 666 :dur 1))
            (next-upto-n (pbind :bar (pseq '(1 2 3) 1)
                                :pr (pseq '(1 2 3))
                                :qux (pseq '(69 420 666))
                                :pdurstutter (pseq '(3 2 1)))))))

;; parent-pbind (FIX)

(test t-pstream
  "Test functionality of non-patterns as pstreams"
  (is (= 1
         (length (next-upto-n 69)))
      "Numbers coerced to pstreams only return one value")
  (is (= 1
         (length (next-upto-n (lambda () (random 420)))))
      "Functions coerced to pstreams only return one value")
  (is (= 3
         (length (next-upto-n (pseq '(1 2 3) 1))))
      "Patterns return the correct number of values when their parameters are values coerced to pstreams")
  (is (= 5
         (let ((*max-pattern-yield-length* 5))
           (length (next-upto-n (pfunc (lambda () (random 64)))))))
      "A function used as an argument for pfunc returns the correct number of values"))

(test remainingp ;; FIX: test this for all patterns that use it.
  "Test the behavior of the `remainingp' function"
  (is (equal (list 1 2 3)
             (next-upto-n (pseq '(1 2 3) 1)))
      "pseq returns the correct number of results")
  (is (= 64
         (let ((*max-pattern-yield-length* 64))
           (length (next-upto-n (pseq '(1 2 3) :inf)))))
      "pseq returns the correct number of results when `next-upto-n' is called with its REPEATS as :inf")
  (is (= 64
         (let ((*max-pattern-yield-length* 64))
           (length (next-upto-n (pseq '(1 2 3) (pseq '(1) :inf))))))
      "pseq returns the correct number of results when its REPEATS is a pattern")
  (is (= 3
         (let ((*max-pattern-yield-length* 64))
           (length (next-upto-n (pseq '(1 2 3) (pseq '(1 0) :inf))))))
      "pseq returns the correct number of results when its REPEATS is a pattern"))

(test pseq
  "Test pseq"
  (is (null
       (next-upto-n (pseq '(1 2 3) 0)))
      "pseq returns 0 results when REPEATS is 0")
  (is (equal
       (list 1 2 3 1 2 3 nil nil)
       (next-n (pseq (list 1 2 3) 2) 8))
      "pseq returns correct results when REPEATS is provided")
  (is (equal
       (list 1 2 3 1 2 3 nil)
       (next-n (pseq (lambda () (list 1 2 3)) 2) 7))
      "pseq returns correct results when LIST is a function")
  (is (string= "" ;; FIX: this should be done for other patterns as well.
               (let* ((s (make-string-output-stream))
                      (*standard-output* s))
                 (as-pstream (pseq '(1 2 3) (lambda () (print 3))))
                 (get-output-stream-string s)))
      "pseq's REPEATS argument is not evaluated until `next' is called")
  (is (equal (list 1 2 3 1 2 3 1 2 3 1 2 3 NIL) ;; FIX: do this for other patterns as well.
             (let* ((foo 1)
                    (bar (as-pstream (pseq '(1 2 3) (pfunc (lambda () foo))))))
               (next-n bar 10) ;=> (1 2 3 1 2 3 1 2 3 1)
               (setf foo 0)
               (next-n bar 3) ;=> (2 3 NIL)
               (slot-value bar 'cl-patterns::history) ;=> (1 2 3 1 2 3 1 2 3 1 2 3 NIL)
               ))
      "pseq returns correct results when its REPEATS is used as a gate")
  (is (equal
       (list 6 7 5 6 7 5)
       (next-upto-n (pseq '(5 6 7) 2 1)))
      "pseq's OFFSET argument works with an integer")
  (is (equal
       (list 6 5 6)
       (next-upto-n (pseq '(5 6 7) 2 (pseq '(1 2 -1) 1))))
      "pseq's OFFSET argument works with a pattern"))

(test pser
  "Test pser"
  (is (equal
       (list 1 2 3 nil nil nil)
       (next-n (pser (list 1 2 3) 3) 6))
      "pser correctly returns three results when its LENGTH is specified")
  (is (equal
       (list 1 2 3 1 2 1 1 2 3 1 2 1)
       (next-upto-n (pser (list 1 2 3) (pseq (list 3 2 1 3 2 1 0) :inf))))
      "pser returns the correct results when its LENGTH is a pattern")
  (is (equal
       (list 1 1 0 0 2 2)
       (next-upto-n (pser (list 0 1 2) :inf (pseq (list 1 0))) 6))
      "pser's OFFSET argument works correctly with a pattern")
  (is (equal
       (list 1 1 0 0 2 2)
       (next-upto-n (pser '(0 1 2) :inf (pseq '(1 0) 3))))
      "pser's OFFSET argument works correctly with a finite pattern"))

(test pk
  "Test pk"
  (is (equal
       (list 3)
       (gete (next-n (pbind :foo (pseq '(3) 1) :bar (pk :foo)) 1) :bar))
      "pk returns correct results")
  (is (equal
       (list 1 2 3 nil)
       (gete (next-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :foo)) 4) :bar))
      "pk returns correct results")
  (is (equal
       (list 2 2 2 nil)
       (gete (next-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :baz 2)) 4) :bar))
      "pk returns correct results when a default is provided and its KEY is not in the source")
  (is (=
       3
       (let ((*event* (event :foo 3)))
         (event-value (next (pbind :bar (pk :foo))) :bar)))
      "*EVENT* is correctly propagated to pbinds when it is bound and pk returns correct results"))

(test prand
  "Test prand"
  (is (not (member nil (mapcar (lambda (x) (member x '(1 2 3))) (next-upto-n (prand '(1 2 3) :inf)))))
      "prand does not produce any values other than the ones provided")
  (is (= 3
         (length (next-upto-n (prand '(1 2 3) 3))))
      "prand returns the correct number of results"))

(test pxrand
  "Test pxrand"
  (is-true
   (block pxrand-test-1
     (let ((prev))
       (dolist (cur (next-n (pxrand (list 1 2)) 10000))
         (when (eq cur prev)
           (return-from pxrand-test-1 nil))
         (setf prev cur))
       t))
   "pxrand does not yield the same item twice in a row"))

(test pwrand
  "Test pwrand"
  (is-false
   (position 0 (next-n (pwrand (list 0 1) (list 0 1)) 1000))
   "pwrand does not yield items whose weight is 0."))

(test pwxrand
  "Test pwxrand"
  (is-true
   (block pwxrand-test-1
     (let ((prev))
       (dolist (cur (next-n (pwxrand (list 1 2)) 10000))
         (when (eq cur prev)
           (return-from pwxrand-test-1 nil))
         (setf prev cur))
       t))
   "pwxrand does not yield the same item twice in a row"))

(test pfunc
  "Test pfunc"
  (is (= 9
         (length (next-upto-n (pfunc (lambda () (random 9))) 9)))
      "pfunc returns the correct number of results")
  (is (= 4
         (next (pfunc (lambda () (+ 2 2)))))
      "pfunc returns correct results"))

(test pr
  "Test pr"
  (is (equal (list 1 1 2 2 3 3 nil)
             (next-n (pr (pseq '(1 2 3) 1) 2) 7))
      "pr returns correct results when its REPEATS is a number")
  (is (equal (list 1 1 2 2 2 3 3 nil)
             (next-n (pr (pseq '(1 2 3) 1) (lambda (e) (if (= e 2) 3 2))) 8))
      "pr returns correct results when its REPEATS is a function")
  (is (equal (list 1 1 2 2 3 3 nil nil)
             (next-n (pr (pseq '(1 2 3) 1) (lambda () 2)) 8))
      "pr returns correct results when its REPEATS is a function that doesn't accept arguments")
  (is (equal (list 3 3 3 3 3 3 3 3 3 3)
             (next-n (pr 3) 10))
      "pr returns correct results when its REPEATS is :inf")
  (is (equal (list 1 1 2 nil)
             (next-n (pr (pseq '(1 2 3) 1) (pseq '(2 1 0) 1)) 4))
      "pr skips elements when REPEATS is 0"))

(test pdef
  ;; FIX
  )

(test plazy
  "Test plazy"
  (is (equal (list 1 2 3 1 2 3 1)
             (next-n (plazy (lambda () (pseq '(1 2 3)))) 7))
      "plazy returns correct results")
  (is (null (next-upto-n (plazy (lambda () nil))))
      "plazy returns correct results when its function returns nil"))

(test plazyn
  "Test plazyn"
  (is (null (next-upto-n (plazyn (lambda () (pseq '(1 2 3))) 0)))
      "plazyn returns 0 results if REPEATS is 0")
  (is (equal '(1 2 3 1 2 3)
             (next-upto-n (plazyn (lambda () (pseq '(1 2 3) 1)) 2)))
      "plazyn returns correct results"))

;; pcycles (FIX)

;; pshift (FIX)

(test pn
  "Test pn"
  (is (equal
       (list 1 nil nil)
       (next-n (pn 1 1) 3))
      "pn returns correct results when its source pattern is a value")
  (is (equal
       (list 3 3 3 nil)
       (next-n (pn 3 3) 4))
      "pn returns correct results when its source pattern is a value")
  (is (equal
       (list 1 2 3 1 2 3 1 2 3 nil nil nil)
       (next-n (pn (pseq '(1 2 3) 1) 3) 12))
      "pn returns correct results when its source pattern is a pattern")
  (is (null (next (pn (pseq '(1 2 3) 0) 1)))
      "pn does not hang when its source pattern returns no values"))

(test pshuf
  "Test pshuf"
  (is (= 5
         (length (next-upto-n (pshuf '(1 2 3 4 5) 1) 32)))
      "pshuf returns the correct number of results when REPEATS is specified")
  (is (= 10
         (length (next-upto-n (pshuf '(1 2 3 4 5) 2) 32)))
      "pshuf returns the correct number of results when REPEATS is specified")
  (is (equal
       (list 1 2 3 4 5)
       (next-upto-n (pseq '(1 2 3 4 5) 1))) ;; this list must be quoted and must be the same as one of the ones used in the pshuf test above.
      "pshuf does not destructively modify its input list"))

(test pwhite
  "Test pwhite"
  (is (block :pwhite-test
        (loop :for i :in (next-upto-n (pwhite 0 1 :inf))
           :if (not (integerp i))
           :do (return-from :pwhite-test nil))
        t)
      "pwhite returns integers when its LO and HI are integers")
  (is (block :pwhite-test
        (loop :for i :in (next-upto-n (pwhite 0.0 1 :inf))
           :if (not (floatp i))
           :do (return-from :pwhite-test nil))
        t)
      "pwhite returns floats when its LO is a float")
  (is (block :pwhite-test
        (loop :for i :in (next-upto-n (pwhite 0 1.0 :inf))
           :if (not (floatp i))
           :do (return-from :pwhite-test nil))
        t)
      "pwhite returns floats when its HI is a float")
  (is (block :pwhite-test
        (loop :for i :in (next-upto-n (pwhite -10 -1 :inf))
           :if (or (not (>= i -10))
                   (not (<= i -1)))
           :do (return-from :pwhite-test nil))
        t)
      "pwhite returns correct results")
  (is (= 7
         (length (next-upto-n (pwhite 0 1 7))))
      "pwhite returns the correct number of results"))

(test pbrown
  ;; FIX
  )

(test pexprand
  ;; FIX
  )

(test pseries
  "Test pseries"
  (is (equal (alexandria:iota 64)
             (next-n (pseries 0 1 :inf) 64))
      "pseries returns correct results")
  (is (equal (list 0 1 1 0 -1 -2)
             (next-upto-n (pseries 0 (pseq '(1 0 -1 -1 -1) 1) :inf)))
      "pseries returns correct results when its STEP is a pattern"))

(test pgeom
  "Test pgeom"
  (is (equal (list 1 2 4 8 16 32 64 128)
             (next-n (pgeom 1 2 :inf) 8))
      "pgeom returns correct results")
  (is (equal (list 1 1 2 6 3.0 2.1)
             (next-upto-n (pgeom 1 (pseq '(1 2 3 0.5 0.7) 1) :inf)))
      "pgeom returns correct results when its GROW is a pattern"))

(test ptrace
  ;; FIX
  )

(test ppatlace
  "Test ppatlace"
  (is (equal (list 1 4 2 5 3 6 7 8 nil)
             (next-n (ppatlace (list (pseq (list 1 2 3) 1) (pseq (list 4 5 6 7 8) 1)) :inf) 9))
      "ppatlace returns correct results when its REPEATS is inf")
  (is (equal (list 1 4 2 5 nil nil nil nil nil)
             (next-n (ppatlace (list (pseq (list 1 2 3)) (pseq (list 4 5 6 7 8))) 2) 9))
      "ppatlace returns correct results when its REPEATS is a number"))

(test pnary
  "Test pnary"
  (is (equal (list 3 4 5)
             (next-upto-n (pnary #'+ (pseq '(1 2 3) 1) 2)))
      "pnary returns correct results with pattern and number as arguments")
  (is (equal (list 4 5 6)
             (next-upto-n (pnary #'+ (pseq '(1 2 3) 1) 2 1)))
      "pnary returns correct results with pattern and two numbers as arguments")
  (is (equal (list 3 0)
             (next-upto-n (pnary (pseq (list #'+ #'-)) 2 (pseq '(1 2) 1))))
      "pnary returns correct results when its operator is a pattern"))

(test pslide
  "Test pslide"
  (is (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 1 0) 13)
             (list 1 2 3 2 3 4 3 4 5 4 5 1 5)))
  (is (equal (next-n (pslide (list 1 2 3 4 5) 2 3 1 0) 13)
             (list 1 2 3 2 3 4 nil nil nil nil nil nil nil)))
  (is (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 1 0 nil) 13)
             (list 1 2 3 2 3 4 3 4 5 4 5 nil 5)))
  (is (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 0 nil) 13)
             (list 1 2 3 nil 1 2 nil nil 1 nil nil nil nil)))
  (is (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 0) 13)
             (list 1 2 3 5 1 2 4 5 1 3 4 5 2)))
  (is (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 1) 13)
             (list 2 3 4 1 2 3 5 1 2 4 5 1 3))))

(test phistory
  "Test phistory"
  (is (equal (list 0 nil 1)
             (next-n (phistory (pseries) (pseq '(0 2 1))) 3))
      "phistory returns correct results, including when outputs that haven't occurred yet are accessed"))

(test pscratch
  "Test pscratch"
  (is (equal (list 1 2 3 0 1 2 3 0 1 2 3 0)
             (next-n (pscratch (pseries 0 1) (pseq (list 1 1 1 -3) :inf)) 12))
      "pscratch returns correct results when using patterns as source and step"))

(test pif
  "Test pif"
  (is (equal (list 1 2 4 5 3 nil 6)
             (next-n (pif (pseq (list t t nil nil t t nil) 1)
                          (pseq (list 1 2 3) 1)
                          (pseq (list 4 5 6) 1))
                     7))
      "pif returns correct results")
  (is (equal (list 1 2 3 nil 4)
             (next-n (pif (pseq '(t t nil nil nil))
                          (pseq '(1 2))
                          (pseq '(3 nil 4)))
                     5))
      "pif returns correct results"))

(test parp
  "Test parp"
  (is-true (every-event-equal
            (list (event :freq 200 :xx 400)
                  (event :freq 200 :xx 200)
                  (event :freq 300 :xx 600)
                  (event :freq 300 :xx 300)
                  (event :freq 400 :xx 800)
                  (event :freq 400 :xx 400)
                  nil)
            (next-n (parp (pbind :freq (pseq (list 200 300 400) 1))
                          (pbind :xx (p* (pk :freq 99) (pseq (list 2 1) 1))))
                    7))
           "parp returns correct results when the arpeggiator pattern references values from the base pattern via pk"))

(test pfin
  "Test pfin"
  (is (= 3
         (length (next-upto-n (pfin (pseq (list 1 2 3) :inf) 3))))
      "pfin correctly limits its source pattern when COUNT is a number")
  (is (= 3
         (length (next-upto-n (pfin (pseq (list 1 2 3) :inf) (pseq (list 3))))))
      "pfin correctly limits its source pattern when COUNT is a pattern"))

(test pfindur
  "Test pfindur"
  (is (= 5
         (reduce #'+ (gete (next-upto-n (pfindur (pbind :dur (pwhite 0.0 1.0)) 5)) :dur)))
      "pfindur patterns have a correct total duration"))

(test psync
  "Test psync"
  (is-true (every-event-equal
            (list (event :dur 5) (event :type :rest :dur 3))
            (next-upto-n (psync (pbind :dur (pseq (list 5) 1)) 4)))
           "psync quantizes up to the next multiple of QUANT")
  (is-true (every-event-equal
            (list (event :dur 5) (event :dur 5) (event :dur 5) (event :dur 5 :delta 1))
            (next-upto-n (psync (pbind :dur (pseq (list 5) 5)) 4 16)))
           "psync limits its source pattern when MAXDUR is provided"))

(test pstutter
  "Test pstutter"
  (is (equal (list 1 1 1 2 2 2 3 3 3)
             (next-upto-n (pstutter (pseq '(1 2 3) 1) 3)))
      "pstutter returns correct results")
  (is (equal (list 2 3 3)
             (next-upto-n (pstutter (pseq '(1 2 3) 1) (pseq '(0 1 2) 1))))
      "pstutter returns correct results when its N is a pattern, and when N is 0"))

(test pdurstutter
  "Test pdurstutter"
  (is (equal (list 2 3/2 3/2)
             (next-upto-n (pdurstutter (pseq '(1 2 3) 1) (pseq '(0 1 2) 1))))
      "pdurstutter returns correct results for value patterns")
  (is (equal (list 1 1/2 1/2) ;; FIX: correct this when events can be compared
             (gete (next-upto-n (pdurstutter (pbind :foo (pseries)) (pseq '(0 1 2) 1))) :dur))
      "pdurstutter returns correct results for event patterns when its N is a pattern, and when N is 0"))

(test pbeats
  "Test pbeats"
  (is (equalp (list 0.0 0.25 0.5 0.75 1.0 1.25)
              (let ((pstr (as-pstream (pbind :foo (pbeats) :dur 0.25))))
                (loop :for i :upto 5
                   :collect (event-value (next pstr) :foo))))
      "pbeats returns correct results")
  ;; FIX: test for peek
  )

(test ptime
  ;; FIX
  )

;; (test psinosc
;;   ;; FIX
;;   )

(test pindex
  "Test pindex"
  (is (equal
       (list 3 2 1 nil nil nil nil)
       (next-n (pindex (list 3 2 1 0) (pseq (list 0 1 2) 1)) 7))
      "pindex returns correct results")
  (is (equal (list 99 98 97 99 98 97 99 98 97)
             (next-n (pindex (list 99 98 97) (pseries 0 1) t) 9))
      "pindex returns correct results when its WRAP-P is t"))

(test prun
  "Test prun"
  (is-true (every-event-equal
            (list (event :foo 1 :bar 4) (event :foo 2 :bar 5) (event :foo 3 :bar 5) (event :foo 4 :bar 6) (event :foo 5 :bar 8))
            (next-upto-n (pbind :foo (pseq '(1 2 3 4 5) 1) :bar (prun (pseq (list 4 5 6 7 8) 1) (pseq (list 1 2 0.5 0.5 1) 1))))) ;; FIX: if the list is quoted instead of generated, it creates garbage..
           "prun returns correct results"))

(test psym ;; FIX: add more
  "Test psym"
  (is-true (let ((cl-patterns::*pdef-dictionary*))
             (pdef :foo1 (pbind :dur (pn 1/2 8)))
             (pdef :foo2 (pbind :dur (pn 2/3 8)))
             (every-event-equal
              (mapcar (lambda (e) (remove-event-value e :pdef))
                      (next-upto-n (psym (pseq (list (list :foo1 :foo2)) 1))))
              (next-upto-n (ppar (list (cl-patterns::pdef-pattern (pdef :foo1))
                                       (cl-patterns::pdef-pattern (pdef :foo2)))))))
           "psym and ppar give the same results when provided the same patterns as inputs"))

(test pchain
  "Test pchain"
  (is-true (every-event-equal
            (list (event :foo 1 :bar 7) (event :foo 2 :bar 8) (event :foo 3 :bar 9) nil)
            (next-n (pchain (pbind :foo (pseq '(1 2 3))) (pbind :bar (pseq '(7 8 9) 1))) 4))
           "pchain correctly combines the outputs from each of its input patterns")
  (is-true (every-event-equal
            (list (event :foo 1 :bar 1) (event :foo 2 :bar 2) (event :foo 3 :bar 3) nil)
            (next-n (pchain (pbind :foo (pseq '(1 2 3) 1)) (pbind :bar (pk :foo))) 4))
           "values from previous patterns are accessible in subsequent patterns when pchain'd"))

(test pdiff
  "Test pdiff"
  (is (equal (list -2 3 -1 nil)
             (next-n (pdiff (pseq (list 3 1 4 3) 1)) 4))))

(test pdelta
  "Test pdelta"
  (is (equal (list 1 1 1 1 1 1 1 1)
             (next-n (pdelta (pseq (list 0 1 2 3)) 4) 8))
      "pdelta gives correct outputs for a basic pattern")
  (is (equal (list 1 1 3 3 1 1 3 3)
             (next-n (pdelta (pseq (list 0 1 2 5)) 4) 8))
      "pdelta gives correct outputs for a pattern \"longer\" than the CYCLE")
  (is (equal (list 1 1 2 0 1 1 2 0)
             (next-n (pdelta (pseq (list 0 1 2 0)) 4) 8))
      "pdelta gives correct outputs for a pattern with successive outputs that decrease")
  (is (equal (list 1 1 2 nil nil)
             (next-n (pdelta (pseq (list 0 1 2 0) 1) 4) 5))
      "pdelta outputs nil when its source pattern ends"))

(test pdrop
  "Test pdrop"
  (is (equal (list 3 4 nil nil)
             (next-n (pdrop (pseq '(1 2 3 4) 1) 2) 4))
      "pdrop correctly drops first N outputs")
  (is (equal (list 1 2 3 nil)
             (next-n (pdrop (pseq '(1 2 3 4 5) 1) -2) 4))
      "pdrop correctly drops last N outputs"))

(test ppar
  "Test ppar"
  (is-true (let ((pat (pbind :dur (pn 4/3 8))))
             (every-event-equal
              (mapcar (lambda (ev) (combine-events ev (event :delta 4/3)))
                      (next-upto-n pat))
              (next-upto-n (ppar (list pat)))))
           "ppar returns correct results when its LIST has only one pattern")
  (is-true (not (typep (cl-patterns::quant (as-pstream (ppar (list (pseq '(1 2 3) 1)))))
                       'pstream))
           "Ensure that patterns that use the default as-pstream method don't have their quant converted to a t-pstream" ;; FIX: rewrite this test so it doesn't depend on ppar not having its own as-pstream method and tests quant conversion more directly
           )
  (is (< 10 (length (next-upto-n (ppar (list (pbind :x (pn 1 4)) (pbind :y (pseries)))) 20)))
      "ppar correctly continues with the rest of the patterns after one ends")
  (is (equal (list 0 1 0 1 0 1 0 1 0 1 1 1)
             (gete (next-upto-n (ppar (list (pbind :dur 1)
                                            (pbind :dur (pn 1 4))))
                                12)
                   :delta))
      "ppar correctly pads its output with rests when one pattern ends early")
  (is (= 8
         (let ((pstr (as-pstream (ppar (list (pbind :dur (pn 1/8 4))
                                             (pbind :dur (pn 1 8))
                                             (pbind :dur (pn 1/16 16)))))))
           (next-upto-n pstr)
           (beats-elapsed pstr)))
      "ppar has the same duration as its longest subpattern"))

;;; bjorklund (FIX)

;;; cycles (FIX)

;;; tracker (FIX)

;;; backend (FIX)

;;; clock (FIX)

;;; sugar (FIX)

;;; cl-collider backend (FIX)

;;; incudine backend (FIX)

;;; midi backend (FIX)
