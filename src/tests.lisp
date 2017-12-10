(defpackage #:cl-patterns/tests
  (:use :cl
        :cl-patterns
        :prove))

(in-package #:cl-patterns/tests)

;;; TODO:
;; * make sure all patterns are given parents

(plan 96)

;;; utility

(ok (= 2.04
       (cl-patterns::round-up 2.03 0.02))
    "round-up gives correct results for positive numbers.")

(ok (= -2.02
       (cl-patterns::round-up -2.03 0.02))
    "round-up gives correct results for negative numbers.")

;;; patterns

;; stack (patterns embedded in patterns)

(ok (equal
     (list 0 1 2 3 4 5 nil)
     (next-n (pseq (list 0 (pseq (list 1 (pseq (list 2 3)) 4)) 5)) 7))
    "Stacked pseqs give correct results.")

;; :remaining key

(ok (= 5
       (length (next-upto-n (pbind :remaining 5 :foo 1))))
    ":remaining key functions properly when specified in pbind.")

(ok (= 5
       (length (next-upto-n (let ((pat (pseq '(1 2 3) :inf)))
                              (setf (slot-value pat 'cl-patterns::remaining) 5)
                              pat))))
    "remaining slot functions properly when setf.")

;; :number key

(ok (equal (list 0 1 2 3 4 5 6 7)
           (let ((pstr (as-pstream (pbind :foo 1))))
             (loop :for i :upto 7
                :collect (slot-value pstr 'cl-patterns::number)
                :do (next pstr))))
    "number slot functions properly for patterns.")

(ok (equal (list 0 1 2 3 4 5 6 7)
           (let ((pstr (as-pstream (pbind :foo (pk :number)))))
             (mapcar (lambda (e) (get-event-value e :foo)) (next-upto-n pstr 8))))
    "pstream's number can be accessed by pk.")

;; pbind

(ok (equal (list :foo :bar :baz)
           (cl-patterns::keys (next (pbind :foo 1 :bar 2 :baz (pseq '(1 2 3))))))
    "pbind returns events that only have the keys specified.")

(ok (= 3
       (length (next-upto-n (pbind :foo 1 :bar 2 :baz (pseq '(1 2 3))))))
    "pbind returns the correct number of events.")

(ok (= 77
       (let ((*max-pattern-yield-length* 77))
         (length (next-upto-n (pbind :foo 1 :bar 2)))))
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

(ok (= 1
       (length (next-upto-n 69)))
    "Numbers coerced to pstreams only return one value.")

(ok (= 1
       (length (next-upto-n (lambda () (random 420)))))
    "Functions coerced to pstreams only return one value.")

(ok (= 3
       (length (next-upto-n (pseq '(1 2 3) 1))))
    "Patterns return the correct number of values when their parameters are values coerced to pstreams.")

(ok (= 5
       (let ((*max-pattern-yield-length* 5))
         (length (next-upto-n (pfunc (lambda () (random 64)))))))
    "A function used as an argument for pfunc returns the correct number of values.")

;; remainingp ;; FIX: test this for all patterns that use it.

(ok (equal (list 1 2 3)
           (next-upto-n (pseq '(1 2 3) 1)))
    "pseq returns the correct number of results.")

(ok (= 64
       (let ((*max-pattern-yield-length* 64))
         (length (next-upto-n (pseq '(1 2 3) :inf)))))
    "pseq returns the correct number of results when `next-upto-n' is called with its REPEATS as :inf.")

(ok (= 64
       (let ((*max-pattern-yield-length* 64))
         (length (next-upto-n (pseq '(1 2 3) (pseq '(1) :inf))))))
    "pseq returns the correct number of results when its REPEATS is a pattern.")

(ok (= 3
       (let ((*max-pattern-yield-length* 64))
         (length (next-upto-n (pseq '(1 2 3) (pseq '(1 0) :inf))))))
    "pseq returns the correct number of results when its REPEATS is a pattern.")

;; pseq

(ok (null
     (next-upto-n (pseq '(1 2 3) 0)))
    "pseq returns 0 results when REPEATS is 0.")

(ok (equal
     (list 1 2 3)
     (next-upto-n (as-pstream (pseq (list 1 2 3)))))
    "pseq returns correct results when REPEATS is not provided.")

(ok (equal
     (list 1 2 3 1 2 3 nil nil)
     (next-n (pseq (list 1 2 3) 2) 8))
    "pseq returns correct results when REPEATS is provided.")

(ok (equal
     (list 1 2 3 1 2 3 nil)
     (next-n (pseq (lambda () (list 1 2 3)) 2) 7))
    "pseq returns correct results when LIST is a function.")

(ok (string= "" ;; FIX: this should be done for other patterns as well.
             (let* ((s (make-string-output-stream))
                    (*standard-output* s))
               (as-pstream (pseq '(1 2 3) (lambda () (print 3))))
               (get-output-stream-string s)))
    "pseq's REPEATS argument is not evaluated until `next' is called.")

(ok (equal (list 1 2 3 1 2 3 1 2 3 1 2 3 NIL) ;; FIX: do this for other patterns as well.
           (let* ((foo 1)
                  (bar (as-pstream (pseq '(1 2 3) (pfunc (lambda () foo))))))
             (next-n bar 10) ;=> (1 2 3 1 2 3 1 2 3 1)
             (setf foo 0)
             (next-n bar 3) ;=> (2 3 NIL)
             (slot-value bar 'cl-patterns::history) ;=> (1 2 3 1 2 3 1 2 3 1 2 3 NIL)
             ))
    "pseq returns correct results when its REPEATS is used as a gate.")

;; pser

(ok (equal
     (list 1 nil)
     (next-n (pser (list 1 2 3)) 2))
    "pser correctly returns one result as default.")

(ok (equal
     (list 1 2 3 nil nil nil)
     (next-n (pser (list 1 2 3) 3) 6))
    "pser correctly returns three results when its LENGTH is specified.")

(ok (equal
     (list 1 2 3 1 2 1 1 2 3 1 2 1)
     (next-upto-n (pser '(1 2 3) (pseq '(3 2 1 3 2 1 0) :inf))))
    "pser returns the correct results when its LENGTH is a pattern.")

;; pk

(ok (equal
     (list 3)
     (gete (next-n (pbind :foo (pseq '(3) 1) :bar (pk :foo)) 1) :bar))
    "pk returns correct results.")

(ok (equal
     (list 1 2 3 nil)
     (gete (next-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :foo)) 4) :bar))
    "pk returns correct results.")

(ok (equal
     (list 2 2 2 nil)
     (gete (next-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :baz 2)) 4) :bar))
    "pk returns correct results when a default is provided and its KEY is not in the source.")

;; prand

(ok (not (member nil (mapcar (lambda (x) (member x '(1 2 3))) (next-upto-n (prand '(1 2 3) :inf)))))
    "prand does not produce any values other than the ones provided.")

(ok (= 3
       (length (next-upto-n (prand '(1 2 3) 3))))
    "prand returns the correct number of results.")

;; pxrand (FIX)

;; pwxrand (FIX)

;; pfunc

(ok (= 9
       (length (next-upto-n (pfunc (lambda () (random 9))) 9)))
    "pfunc returns the correct number of results.")

(ok (= 4
       (next (pfunc (lambda () (+ 2 2)))))
    "pfunc returns correct results.")

;; pr

(ok (equal (list 1 1 2 2 3 3 nil)
           (next-n (pr (pseq '(1 2 3)) 2) 7))
    "pr returns correct results when its REPEATS is a number.")

(ok (equal (list 1 1 2 2 2 3 3 nil)
           (next-n (pr (pseq '(1 2 3)) (lambda (e) (if (= e 2) 3 2))) 8))
    "pr returns correct results when its REPEATS is a function.")

(ok (equal (list 1 1 2 2 3 3 nil nil)
           (next-n (pr (pseq '(1 2 3)) (lambda () 2)) 8))
    "pr returns correct results when its REPEATS is a function that doesn't accept arguments.")

(ok (equal (list 3 3 3 3 3 3 3 3 3 3)
           (next-n (pr 3) 10))
    "pr returns correct results when its REPEATS is :inf.")

(ok (equal (list 1 1 2 nil)
           (next-n (pr (pseq '(1 2 3)) (pseq '(2 1 0))) 4))
    "pr skips elements when REPEATS is 0.")

;; pdef (FIX)

;; plazy

(ok (equal (list 1 2 3 1 2 3 1)
           (next-n (plazy (lambda () (pseq '(1 2 3)))) 7))
    "plazy returns correct results.")

(ok (null (next-upto-n (plazy (lambda () nil))))
    "plazy returns correct results when its function returns nil.")

;; plazyn

(ok (null (next-upto-n (plazyn (lambda () (pseq '(1 2 3))) 0)))
    "plazyn returns 0 results if REPEATS is 0.")

(ok (equal '(1 2 3 1 2 3)
           (next-upto-n (plazyn (lambda () (pseq '(1 2 3))) 2)))
    "plazyn returns correct results.")

;; pcycles (FIX)

;; pshift (FIX)

;; pn

(ok (equal
     (list 1 nil nil)
     (next-n (pn 1 1) 3))
    "pn returns correct results when its source pattern is a value.")

(ok (equal
     (list 3 3 3 nil)
     (next-n (pn 3 3) 4))
    "pn returns correct results when its source pattern is a value.")

(ok (equal
     (list 1 2 3 1 2 3 1 2 3 nil nil nil)
     (next-n (pn (pseq '(1 2 3) 1) 3) 12))
    "pn returns correct results when its source pattern is a pattern.")

(ok (null (next (pn (pseq '(1 2 3) 0) 1)))
    "pn does not hang when its source pattern returns no values.")

;; pshuf

(ok (= 5
       (length (next-upto-n (pshuf '(1 2 3 4 5)) 32)))
    "pshuf returns the correct number of results when no REPEATS is specified.")

(ok (= 5
       (length (next-upto-n (pshuf '(1 2 3 4 5) 1) 32)))
    "pshuf returns the correct number of results when REPEATS is specified.")

(ok (= 10
       (length (next-upto-n (pshuf '(1 2 3 4 5) 2) 32)))
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

(ok (= 7
       (length (next-upto-n (pwhite 0 1 7))))
    "pwhite returns the correct number of results.")

;; pbrown (FIX)

;; pexprand (FIX)

;; pseries

(ok (equal (alexandria:iota 64)
           (next-n (pseries 0 1 :inf) 64))
    "pseries returns correct results.")

(ok (equal (list 0 1 1 0 -1 -2)
           (next-upto-n (pseries 0 (pseq '(1 0 -1 -1 -1) 1) :inf)))
    "pseries returns correct results when its STEP is a pattern.")

;; pgeom

(ok (equal (list 1 2 4 8 16 32 64 128)
           (next-n (pgeom 1 2 :inf) 8))
    "pgeom returns correct results.")

(ok (equal (list 1 1 2 6 3.0 2.1)
           (next-upto-n (pgeom 1 (pseq '(1 2 3 0.5 0.7)) :inf)))
    "pgeom returns correct results when its GROW is a pattern.")

;; ptrace (FIX)

;; ppatlace

(ok (equal (list 1 4 2 5 3 6 7 8 nil)
           (next-n (ppatlace (list (pseq (list 1 2 3)) (pseq (list 4 5 6 7 8))) :inf) 9))
    "ppatlace returns correct results when its REPEATS is inf.")

(ok (equal (list 1 4 2 5 nil nil nil nil nil)
           (next-n (ppatlace (list (pseq (list 1 2 3)) (pseq (list 4 5 6 7 8))) 2) 9))
    "ppatlace returns correct results when its REPEATS is a number.")

;; pnary

(ok (equal (list 3 4 5)
           (next-upto-n (pnary #'+ (pseq '(1 2 3) 1) 2)))
    "pnary returns correct results with pattern and number as arguments.")

(ok (equal (list 4 5 6)
           (next-upto-n (pnary #'+ (pseq '(1 2 3) 1) 2 1)))
    "pnary returns correct results with pattern and two numbers as arguments.")

(ok (equal (list 3 0)
           (next-upto-n (pnary (pseq (list #'+ #'-)) 2 (pseq '(1 2) 1))))
    "pnary returns correct results when its operator is a pattern.")

;; pslide

(ok (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 1 0) 13)
           (list 1 2 3 2 3 4 3 4 5 4 5 1 5)))

(ok (equal (next-n (pslide (list 1 2 3 4 5) 2 3 1 0) 13)
           (list 1 2 3 2 3 4 nil nil nil nil nil nil nil)))

(ok (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 1 0 nil) 13)
           (list 1 2 3 2 3 4 3 4 5 4 5 nil 5)))

(ok (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 0 nil) 13)
           (list 1 2 3 nil 1 2 nil nil 1 nil nil nil nil)))

(ok (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 0) 13)
           (list 1 2 3 5 1 2 4 5 1 3 4 5 2)))

(ok (equal (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 1) 13)
           (list 2 3 4 1 2 3 5 1 2 4 5 1 3)))

;; phistory

(ok (equal (list 0 nil 1)
           (next-n (phistory (pseries) (pseq '(0 2 1))) 3))
    "phistory returns correct results, including when outputs that haven't occurred yet are accessed.")

;; pfuture (FIX)

;; pscratch

(ok (equal (list 1 2 3 0 1 2 3 0 1 2 3 0)
           (next-n (pscratch (pseries 0 1) (pseq (list 1 1 1 -3) :inf)) 12))
    "pscratch returns correct results when using patterns as source and step.")

;; pif

(ok (equal (list 1 2 4 5 3 nil 6)
           (next-n (pif (pseq (list t t nil nil t t nil))
                        (pseq (list 1 2 3))
                        (pseq (list 4 5 6)))
                   7))
    "pif returns correct results.")

(ok (equal (list 1 2 3 nil 4)
           (next-n (pif (pseq '(t t nil nil nil))
                        (pseq '(1 2))
                        (pseq '(3 nil 4)))
                   5))
    "pif returns correct results.")

;; ptracker (FIX)

;; parp (FIX)

;; pfin

(ok (= 3
       (length (next-upto-n (pfin (pseq '(1 2 3) :inf) 3))))
    "pfin correctly limits its source pattern when COUNT is a number.")

(ok (= 3
       (length (next-upto-n (pfin (pseq '(1 2 3) :inf) (pseq '(3))))))
    "pfin correctly limits its source pattern when COUNT is a pattern.")

;; pfindur (FIX)

(ok (= 5 ;; FIX - fails
       (reduce #'+ (gete (next-upto-n (pfindur (pbind :dur (pwhite 0.0 1.0)) 5)) :dur)))
    "pfindur patterns have a correct total duration.")

;; pstutter

(ok (equal (list 1 1 1 2 2 2 3 3 3)
           (next-upto-n (pstutter 3 (pseq '(1 2 3)))))
    "pstutter returns correct results.")

(ok (equal (list 2 3 3)
           (next-upto-n (pstutter (pseq '(0 1 2)) (pseq '(1 2 3)))))
    "pstutter returns correct results when its N is a pattern, and when N is 0.")

;; pdurstutter

(ok (equal (list 2 3/2 3/2)
           (next-upto-n (pdurstutter (pseq '(1 2 3)) (pseq '(0 1 2)))))
    "pdurstutter returns correct results for value patterns.")

(ok (equal (list 1 1/2 1/2) ;; FIX: correct this when events can be compared
           (gete (next-upto-n (pdurstutter (pbind :foo (pseries)) (pseq '(0 1 2)))) :dur))
    "pdurstutter returns correct results for event patterns when its N is a pattern, and when N is 0.")

;; pbeats

(ok (equal (list 0.0 0.25 0.5 0.75 1.0 1.25)
           (let ((pstr (as-pstream (pbind :foo (pbeats) :dur 0.25))))
             (loop :for i :upto 5
                :collect (get-event-value (next pstr) :foo))))
    "pbeats returns correct results.")

;; psinosc (FIX)

;; pindex

(ok (equal
     (list 3 2 1 3 2 1 nil)
     (next-n (pindex (list 3 2 1 0) (pseq (list 0 1 2)) 2) 7))
    "pindex returns correct results.")

(ok (equal (list 99 98 97 99 99 98 97 99 nil)
           (next-n (pindex (list 99 98 97) (pseries 0 1 4) 2 t) 9))
    "pindex returns correct results when its WRAP-P is t.")

;; pbjorklund (FIX)

;; prun (FIX)

(ok (equal (list (event :foo 1 :bar 4) (event :foo 2 :bar 5) (event :foo 3 :bar 5) (event :foo 4 :bar 6) (event :foo 5 :bar 8))
           (next-upto-n (pbind :foo (pseq '(1 2 3 4 5)) :bar (prun (pseq '(4 5 6 7 8)) (pseq '(1 2 0.5 0.5 1))))))
    "prun returns correct results.")

;;; conversions (FIX - add more)

(ok (=
     (db-amp (amp-db 0.5))
     0.5)
    "db-to-amp conversion is equivalent to amp-to-db conversion.")

;;; events (FIX - add more)

(ok (=
     1
     (get-event-value (event :dur 0 :sustain 1) :sustain))
    "event returns the correct sustain when sustain is provided and dur is 0.")

(ok (=
     0.8
     (get-event-value (event) :sustain))
    "event returns the correct default value for sustain.")

(ok (=
     0.5
     (get-event-value (event :dur 0 :legato 0.5) :legato))
    "event returns the correct legato when legato is provided and dur is 0.")

(ok (=
     0.8
     (get-event-value (event) :legato))
    "event returns the correct default value for legato.")

(ok (=
     1
     (get-event-value (event) :dur))
    "event returns the correct default value for dur.")

(ok (eq
     :default
     (get-event-value (event) :instrument))
    "event returns the correct default value for instrument.")

(ok (= (amp-db 0.125)
       (get-event-value (event :amp 0.125) :db))
    "event correctly converts amp to db.")

(ok (= (db-amp -7)
       (get-event-value (event :db -7) :amp))
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
