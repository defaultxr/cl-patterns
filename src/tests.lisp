(defpackage #:cl-patterns/tests
  (:use :cl
        :cl-patterns
        :prove))

(in-package #:cl-patterns/tests)

;;; TODO:
;; * make sure all patterns are given parents

(plan nil)

;;; utility

(is (cl-patterns::round-up 2.03 0.02)
    2.04
    "round-up gives correct results for positive numbers.")

(is (cl-patterns::round-up -2.03 0.02)
    -2.02
    "round-up gives correct results for negative numbers.")

;;; patterns

;; stack (patterns embedded in patterns)

(is (next-n (pseq (list 0 (pseq (list 1 (pseq (list 2 3) 1) 4) 1) 5) 1) 7)
    (list 0 1 2 3 4 5 nil)
    "Stacked pseqs give correct results.")

;; :remaining key

(is (length (next-upto-n (pbind :remaining 5 :foo 1)))
    5
    ":remaining key functions properly when specified in pbind.")

(is (length (next-upto-n (let ((pat (pseq '(1 2 3) :inf)))
                           (setf (slot-value pat 'cl-patterns::remaining) 5)
                           pat)))
    5
    "remaining slot functions properly when setf.")

;; :number key

(is (let ((pstr (as-pstream (pbind :foo 1))))
      (loop :for i :upto 7
         :collect (slot-value pstr 'cl-patterns::number)
         :do (next pstr)))
    (list 0 1 2 3 4 5 6 7)
    "number slot functions properly for patterns.")

(is (let ((pstr (as-pstream (pbind :foo (pk :number)))))
      (mapcar (lambda (e) (event-value e :foo)) (next-upto-n pstr 8)))
    (list 0 1 2 3 4 5 6 7)
    "pstream's number can be accessed by pk.")

;; pbind

(is (cl-patterns::keys (next (pbind :foo 1 :bar 2 :baz (pseq '(1 2 3) 1))))
    (list :foo :bar :baz)
    "pbind returns events that only have the keys specified.")

(is (length (next-upto-n (pbind :foo 1 :bar 2 :baz (pseq '(1 2 3) 1))))
    3
    "pbind returns the correct number of events.")

(is (let ((*max-pattern-yield-length* 77))
      (length (next-upto-n (pbind :foo 1 :bar 2))))
    77
    "pbind returns the correct number of events.")

;; parent

(ok (let ((pb (pbind :foo (pseq '(1 2 3)))))
      (eq (cl-patterns::parent-pattern (getf (slot-value pb 'cl-patterns::pairs) :foo))
          pb))
    "pbind subpatterns have correct parents for pseq.")

(ok (let ((pb (pbind :foo (pfunc (lambda () (random 5))))))
      (eq (cl-patterns::parent-pattern (getf (slot-value pb 'cl-patterns::pairs) :foo))
          pb))
    "pbind subpatterns have correct parents for pfunc.")

;; parent-pbind (FIX)

;; t-pstream

(is (length (next-upto-n 69))
    1
    "Numbers coerced to pstreams only return one value.")

(is (length (next-upto-n (lambda () (random 420))))
    1
    "Functions coerced to pstreams only return one value.")

(is (length (next-upto-n (pseq '(1 2 3) 1)))
    3
    "Patterns return the correct number of values when their parameters are values coerced to pstreams.")

(is (let ((*max-pattern-yield-length* 5))
      (length (next-upto-n (pfunc (lambda () (random 64))))))
    5
    "A function used as an argument for pfunc returns the correct number of values.")

;; remainingp ;; FIX: test this for all patterns that use it.

(is (next-upto-n (pseq '(1 2 3) 1))
    (list 1 2 3)
    "pseq returns the correct number of results.")

(is (let ((*max-pattern-yield-length* 64))
      (length (next-upto-n (pseq '(1 2 3) :inf))))
    64
    "pseq returns the correct number of results when `next-upto-n' is called with its REPEATS as :inf.")

(is (let ((*max-pattern-yield-length* 64))
      (length (next-upto-n (pseq '(1 2 3) (pseq '(1) :inf)))))
    64
    "pseq returns the correct number of results when its REPEATS is a pattern.")

(is (let ((*max-pattern-yield-length* 64))
      (length (next-upto-n (pseq '(1 2 3) (pseq '(1 0) :inf)))))
    3
    "pseq returns the correct number of results when its REPEATS is a pattern.")

;; pseq

(ok (null
     (next-upto-n (pseq '(1 2 3) 0)))
    "pseq returns 0 results when REPEATS is 0.")

(is (next-n (pseq (list 1 2 3) 2) 8)
    (list 1 2 3 1 2 3 nil nil)
    "pseq returns correct results when REPEATS is provided.")

(is (next-n (pseq (lambda () (list 1 2 3)) 2) 7)
    (list 1 2 3 1 2 3 nil)
    "pseq returns correct results when LIST is a function.")

;; FIX: this should be done for other patterns as well.
(is (let* ((s (make-string-output-stream))
           (*standard-output* s))
      (as-pstream (pseq '(1 2 3) (lambda () (print 3))))
      (get-output-stream-string s))
    ""
    "pseq's REPEATS argument is not evaluated until `next' is called.")

;; FIX: do this for other patterns as well.
(is (let* ((foo 1)
           (bar (as-pstream (pseq '(1 2 3) (pfunc (lambda () foo))))))
      (next-n bar 10) ;=> (1 2 3 1 2 3 1 2 3 1)
      (setf foo 0)
      (next-n bar 3) ;=> (2 3 NIL)
      (slot-value bar 'cl-patterns::history))
    (list 1 2 3 1 2 3 1 2 3 1 2 3 NIL)
    "pseq returns correct results when its REPEATS is used as a gate.")

;; pser

(is (next-n (pser (list 1 2 3) 3) 6)
    (list 1 2 3 nil nil nil)
    "pser correctly returns three results when its LENGTH is specified.")

(is (next-upto-n (pser '(1 2 3) (pseq '(3 2 1 3 2 1 0) :inf)))
    (list 1 2 3 1 2 1 1 2 3 1 2 1)
    "pser returns the correct results when its LENGTH is a pattern.")

;; pk

(is (gete (next-n (pbind :foo (pseq '(3) 1) :bar (pk :foo)) 1) :bar)
    (list 3)
    "pk returns correct results.")

(is (gete (next-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :foo)) 4) :bar)
    (list 1 2 3 nil)
    "pk returns correct results.")

(is (gete (next-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :baz 2)) 4) :bar)
    (list 2 2 2 nil)
    "pk returns correct results when a default is provided and its KEY is not in the source.")

;; prand

(ok (not (member nil (mapcar (lambda (x) (member x '(1 2 3))) (next-upto-n (prand '(1 2 3) :inf)))))
    "prand does not produce any values other than the ones provided.")

(is (length (next-upto-n (prand '(1 2 3) 3)))
    3
    "prand returns the correct number of results.")

;; pxrand (FIX)

;; pwxrand (FIX)

;; pfunc

(is (length (next-upto-n (pfunc (lambda () (random 9))) 9))
    9
    "pfunc returns the correct number of results.")

(is (next (pfunc (lambda () (+ 2 2))))
    4
    "pfunc returns correct results.")

;; pr

(is (next-n (pr (pseq '(1 2 3) 1) 2) 7)
    (list 1 1 2 2 3 3 nil)
    "pr returns correct results when its REPEATS is a number.")

(is (next-n (pr (pseq '(1 2 3) 1) (lambda (e) (if (= e 2) 3 2))) 8)
    (list 1 1 2 2 2 3 3 nil)
    "pr returns correct results when its REPEATS is a function.")

(is (next-n (pr (pseq '(1 2 3) 1) (lambda () 2)) 8)
    (list 1 1 2 2 3 3 nil nil)
    "pr returns correct results when its REPEATS is a function that doesn't accept arguments.")

(is (next-n (pr 3) 10)
    (list 3 3 3 3 3 3 3 3 3 3)
    "pr returns correct results when its REPEATS is :inf.")

(is (next-n (pr (pseq '(1 2 3) 1) (pseq '(2 1 0) 1)) 4)
    (list 1 1 2 nil)
    "pr skips elements when REPEATS is 0.")

;; pdef (FIX)

;; plazy

(is (next-n (plazy (lambda () (pseq '(1 2 3)))) 7)
    (list 1 2 3 1 2 3 1)
    "plazy returns correct results.")

(ok (null (next-upto-n (plazy (lambda () nil))))
    "plazy returns correct results when its function returns nil.")

;; plazyn

(ok (null (next-upto-n (plazyn (lambda () (pseq '(1 2 3))) 0)))
    "plazyn returns 0 results if REPEATS is 0.")

(is (next-upto-n (plazyn (lambda () (pseq '(1 2 3) 1)) 2))
    (list 1 2 3 1 2 3)
    "plazyn returns correct results.")

;; pcycles (FIX)

;; pshift (FIX)

;; pn

(is (next-n (pn 1 1) 3)
    (list 1 nil nil)
    "pn returns correct results when its source pattern is a value.")

(is (next-n (pn 3 3) 4)
    (list 3 3 3 nil)
    "pn returns correct results when its source pattern is a value.")

(is (next-n (pn (pseq '(1 2 3) 1) 3) 12)
    (list 1 2 3 1 2 3 1 2 3 nil nil nil)
    "pn returns correct results when its source pattern is a pattern.")

(ok (null (next (pn (pseq '(1 2 3) 0) 1)))
    "pn does not hang when its source pattern returns no values.")

;; pshuf

(is (length (next-upto-n (pshuf '(1 2 3 4 5) 1) 32))
    5
    "pshuf returns the correct number of results when REPEATS is specified.")

(is (length (next-upto-n (pshuf '(1 2 3 4 5) 2) 32))
    10
    "pshuf returns the correct number of results when REPEATS is specified.")

;; pwhite

(ok (block :pwhite-test
      (loop :for i :in (next-upto-n (pwhite 0 1 :inf))
         :if (not (integerp i))
         :do (return-from :pwhite-test nil))
      t)
    "pwhite returns integers when its LO and HI are integers.")

(ok (block :pwhite-test
      (loop :for i :in (next-upto-n (pwhite 0.0 1 :inf))
         :if (not (floatp i))
         :do (return-from :pwhite-test nil))
      t)
    "pwhite returns floats when its LO is a float.")

(ok (block :pwhite-test
      (loop :for i :in (next-upto-n (pwhite 0 1.0 :inf))
         :if (not (floatp i))
         :do (return-from :pwhite-test nil))
      t)
    "pwhite returns floats when its HI is a float.")

(ok (block :pwhite-test
      (loop :for i :in (next-upto-n (pwhite -10 -1 :inf))
         :if (or (not (>= i -10))
                 (not (<= i -1)))
         :do (return-from :pwhite-test nil))
      t)
    "pwhite returns correct results.")

(is (length (next-upto-n (pwhite 0 1 7)))
    7
    "pwhite returns the correct number of results.")

;; pbrown (FIX)

;; pexprand (FIX)

;; pseries

(is (next-n (pseries 0 1 :inf) 64)
    (alexandria:iota 64)
    "pseries returns correct results.")

(is (next-upto-n (pseries 0 (pseq '(1 0 -1 -1 -1) 1) :inf))
    (list 0 1 1 0 -1 -2)
    "pseries returns correct results when its STEP is a pattern.")

;; pgeom

(is (next-n (pgeom 1 2 :inf) 8)
    (list 1 2 4 8 16 32 64 128)
    "pgeom returns correct results.")

(is (next-upto-n (pgeom 1 (pseq '(1 2 3 0.5 0.7) 1) :inf))
    (list 1 1 2 6 3.0 2.1)
    "pgeom returns correct results when its GROW is a pattern.")

;; ptrace (FIX)

;; ppatlace

(is (next-n (ppatlace (list (pseq (list 1 2 3) 1) (pseq (list 4 5 6 7 8) 1)) :inf) 9)
    (list 1 4 2 5 3 6 7 8 nil)
    "ppatlace returns correct results when its REPEATS is inf.")

(is (next-n (ppatlace (list (pseq (list 1 2 3)) (pseq (list 4 5 6 7 8))) 2) 9)
    (list 1 4 2 5 nil nil nil nil nil)
    "ppatlace returns correct results when its REPEATS is a number.")

;; pnary

(is (next-upto-n (pnary #'+ (pseq '(1 2 3) 1) 2))
    (list 3 4 5)
    "pnary returns correct results with pattern and number as arguments.")

(is (next-upto-n (pnary #'+ (pseq '(1 2 3) 1) 2 1))
    (list 4 5 6)
    "pnary returns correct results with pattern and two numbers as arguments.")

(is (next-upto-n (pnary (pseq (list #'+ #'-)) 2 (pseq '(1 2) 1)))
    (list 3 0)
    "pnary returns correct results when its operator is a pattern.")

;; pslide

(is (next-n (pslide (list 1 2 3 4 5) :inf 3 1 0) 13)
    (list 1 2 3 2 3 4 3 4 5 4 5 1 5))

(is (next-n (pslide (list 1 2 3 4 5) 2 3 1 0) 13)
    (list 1 2 3 2 3 4 nil nil nil nil nil nil nil))

(is (next-n (pslide (list 1 2 3 4 5) :inf 3 1 0 nil) 13)
    (list 1 2 3 2 3 4 3 4 5 4 5 nil 5))

(is (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 0 nil) 13)
    (list 1 2 3 nil 1 2 nil nil 1 nil nil nil nil))

(is (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 0) 13)
    (list 1 2 3 5 1 2 4 5 1 3 4 5 2))

(is (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 1) 13)
    (list 2 3 4 1 2 3 5 1 2 4 5 1 3))

;; phistory

(is (next-n (phistory (pseries) (pseq '(0 2 1))) 3)
    (list 0 nil 1)
    "phistory returns correct results, including when outputs that haven't occurred yet are accessed.")

;; pfuture (FIX)

;; pscratch

(is (next-n (pscratch (pseries 0 1) (pseq (list 1 1 1 -3) :inf)) 12)
    (list 1 2 3 0 1 2 3 0 1 2 3 0)
    "pscratch returns correct results when using patterns as source and step.")

;; pif

(is (next-n (pif (pseq (list t t nil nil t t nil) 1)
                 (pseq (list 1 2 3) 1)
                 (pseq (list 4 5 6) 1))
            7)
    (list 1 2 4 5 3 nil 6)
    "pif returns correct results.")

(is (next-n (pif (pseq '(t t nil nil nil))
                 (pseq '(1 2))
                 (pseq '(3 nil 4)))
            5)
    (list 1 2 3 nil 4)
    "pif returns correct results.")

;; ptracker (FIX)

;; parp (FIX)

;; pfin

(is (length (next-upto-n (pfin (pseq '(1 2 3) :inf) 3)))
    3
    "pfin correctly limits its source pattern when COUNT is a number.")

(is (length (next-upto-n (pfin (pseq '(1 2 3) :inf) (pseq '(3)))))
    3
    "pfin correctly limits its source pattern when COUNT is a pattern.")

;; pfindur (FIX)

(is (reduce #'+ (gete (next-upto-n (pfindur (pbind :dur (pwhite 0.0 1.0)) 5)) :dur))
    5.0
    "pfindur patterns have a correct total duration.")

;; pstutter

(is (next-upto-n (pstutter 3 (pseq '(1 2 3) 1)))
    (list 1 1 1 2 2 2 3 3 3)
    "pstutter returns correct results.")

(is (next-upto-n (pstutter (pseq '(0 1 2) 1) (pseq '(1 2 3) 1)))
    (list 2 3 3)
    "pstutter returns correct results when its N is a pattern, and when N is 0.")

;; pdurstutter

(is (next-upto-n (pdurstutter (pseq '(1 2 3) 1) (pseq '(0 1 2) 1)))
    (list 2 3/2 3/2)
    "pdurstutter returns correct results for value patterns.")

(ok (every #'event-equal
           (next-upto-n (pdurstutter (pbind :foo (pseries)) (pseq '(0 1 2) 1)))
           (list (event :foo 1 :dur 1) (event :foo 2 :dur 1/2) (event :foo 2 :dur 1/2)))
    "pdurstutter returns correct results for event patterns when its N is a pattern, and when N is 0.")

;; pbeats

(is (let ((pstr (as-pstream (pbind :foo (pbeats) :dur 0.25))))
      (loop :for i :upto 5
         :collect (event-value (next pstr) :foo)))
    (list 0.0 0.25 0.5 0.75 1.0 1.25)
    "pbeats returns correct results.")

;; ptime (FIX)

;; psinosc (FIX)

;; pindex

(is (next-n (pindex (list 3 2 1 0) (pseq (list 0 1 2) 1) 2) 7)
    (list 3 2 1 3 2 1 nil)
    "pindex returns correct results.")

(is (next-n (pindex (list 99 98 97) (pseries 0 1 4) 2 t) 9)
    (list 99 98 97 99 99 98 97 99 nil)
    "pindex returns correct results when its WRAP-P is t.")

;; pbjorklund (FIX)

;; prun

(ok (every #'event-equal
           (next-upto-n (pbind :foo (pseq '(1 2 3 4 5) 1) :bar (prun (pseq '(4 5 6 7 8) 1) (pseq '(1 2 0.5 0.5 1) 1))))
           (list (event :foo 1 :bar 4) (event :foo 2 :bar 5) (event :foo 3 :bar 5) (event :foo 4 :bar 6) (event :foo 5 :bar 8)))
    "prun returns correct results.")

;; FIX - add more

;; psym (FIX)



;;; conversions (FIX - add more)

(is (db-amp (amp-db 0.5))
    0.5
    "db-to-amp conversion is equivalent to amp-to-db conversion.")

;;; events (FIX - add more)

(is (event-value (event :dur 0 :sustain 1) :sustain)
    1
    "event returns the correct sustain when sustain is provided and dur is 0.")

(is (event-value (event) :sustain)
    0.8
    "event returns the correct default value for sustain.")

(is (event-value (event :dur 0 :legato 0.5) :legato)
    0.5
    "event returns the correct legato when legato is provided and dur is 0.")

(is (event-value (event) :legato)
    0.8
    "event returns the correct default value for legato.")

(is (event-value (event) :dur)
    1
    "event returns the correct default value for dur.")

(is (event-value (event) :instrument)
    :default
    "event returns the correct default value for instrument.")

(is (event-value (event :amp 0.125) :db)
    (amp-db 0.125)
    "event correctly converts amp to db.")

(is (event-value (event :db -7) :amp)
    (db-amp -7)
    "event correctly converts db to amp.")

;;; clock (FIX)

;;; tsubseq

;; (let* ((pb (pbind :dur 1/3)))
;;   (ok (= 2/3 (reduce #'+ (gete (tsubseq pb 1 1.5) :dur))))
;;   (ok (= 2/3 (reduce #'+ (gete (tsubseq (as-pstream pb) 1 1.5) :dur))))
;;   (ok (= 2/3 (reduce #'+ (gete (tsubseq (next-n pb 15) 1 1.5) :dur)))))

;; (let* ((pb (pbind :dur 1/3)))
;;   (ok (= 0.25 (reduce #'+ (gete (tsubseq* pb 1.25 1.5) :dur))))
;;   (ok (= 0.25 (reduce #'+ (gete (tsubseq* (as-pstream pb) 1.25 1.5) :dur))))
;;   (ok (= 0.25 (reduce #'+ (gete (tsubseq* (next-n pb 15) 1.25 1.5) :dur)))))

(finalize)
