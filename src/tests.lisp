(defpackage #:cl-patterns/tests
  (:use :cl
        :cl-patterns
        :prove))

(in-package #:cl-patterns/tests)

;;; TODO:
;; * make sure all patterns are given parents

(plan 38)

;;; patterns

;; :remaining key

(ok (equal ;; FIX: should test this for other patterns as well, not just pseq.
     (list 0 1 2 3 4 3 4 nil nil nil nil nil)
     (next-n (pseq (list 0 (pseq '(1 2) 1) (pseq '(3 4) 2)) 1) 12)))

;; :number key (FIX)

;; pbind (FIX)

;; parent

(ok (let ((pb (pbind :foo (pseq '(1 2 3)))))
      (eq (cl-patterns::parent (getf (slot-value pb 'cl-patterns::pairs) :foo))
          pb)))

(ok (let ((pb (pbind :foo (pfunc (lambda () (random 5))))))
      (eq (cl-patterns::parent (getf (slot-value pb 'cl-patterns::pairs) :foo))
          pb)))

;; parent-pbind (FIX)

;; pseq

(ok (=
     1
     (next (pseq (list 1 2 3)))))

(ok (equal
     (next-n (pseq (list 1 2 3)) 3)
     (next-n (as-pstream (pseq (list 1 2 3))) 3)))

(ok (equal
     (next-n (pseq (list 1 2 3 1 2 3 nil nil)) 8)
     (next-n (pseq (list 1 2 3) 2) 8)))

(ok (equal
     (list 1 2 3 1 2 3 nil)
     (next-n (pseq (lambda () (list 1 2 3)) 2) 7)))

;; pser

(ok (equal
     (list 1 nil)
     (next-n (pser (list 1 2 3)) 2)))

(ok (equal
     (list 1 2 3 nil nil nil)
     (next-n (pser (list 1 2 3) 3) 6)))

;; pk

(ok (equal
     (list 3)
     (gete (next-n (pbind :foo (pseq '(3) 1) :bar (pk :foo)) 1) :bar)))

(ok (equal
     (list 1 2 3 nil)
     (gete (next-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :foo)) 4) :bar)))

(ok (equal
     (list 2 2 2 nil)
     (gete (next-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :baz 2)) 4) :bar)))

;; prand (FIX)

;; pxrand (FIX)

;; pfunc

(ok (next-n (pfunc (lambda () (random 9))) 9))

;; pr

(ok (equal (list 1 1 2 2 3 3 nil)
           (next-n (pr (pseq '(1 2 3)) 2) 7)))

(ok (equal (list 1 1 1)
           (next-n (pr (pseq '(1 2 3))) 3)))

(ok (equal (list 3 3 3)
           (next-n (pr 3) 3)))

;; pdef (FIX)

;; plazy (FIX)

;; plazyn (FIX)

;; pcycles (FIX)

;; pshift (FIX)

;; pn

(ok (equal
     (list 1 nil nil)
     (next-n (pn 1 1) 3)))

(ok (equal
     (list 1 2 3 1 2 3 1 2 3 nil nil nil)
     (next-n (pn (pseq '(1 2 3) 1) 3) 12)))

;; pshuf

(ok (= 5
       (length (next-upto-n (pshuf '(1 2 3 4 5)) 32))))

(ok (= 5
       (length (next-upto-n (pshuf '(1 2 3 4 5) 1) 32))))

(ok (= 10
       (length (next-upto-n (pshuf '(1 2 3 4 5) 2) 32))))

;; pwhite (FIX)

;; pbrown (FIX)

;; pseries (FIX)

;; pgeom (FIX)

;; ptrace (FIX)

;; ppatlace

(ok (equal
     (next-n (ppatlace (list (pseq (list 1 2 3)) (pseq (list 4 5 6 7 8))) :inf) 9)
     (list 1 4 2 5 3 6 7 8 nil)))

(ok (equal
     (next-n (ppatlace (list (pseq (list 1 2 3)) (pseq (list 4 5 6 7 8))) 2) 9)
     (list 1 4 2 5 nil nil nil nil nil)))

;; pnary (FIX)

;; pslide (FIX - add more?)

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

;; phistory (FIX)

;; pscratch (FIX)

;; pif

(ok (equal (next-n (pif (pseq (list t t nil nil t t nil))
                        (pseq (list 1 2 3))
                        (pseq (list 4 5 6)))
                   7)
           (list 1 2 4 5 3 nil 6)))

(ok (equal (next-n (pif (pseq '(t t nil nil nil))
                        (pseq '(1 2))
                        (pseq '(3 nil 4)))
                   5)
           (list 1 2 3 nil 4)))

;; parp (FIX)

;; pfin (FIX)

;; pfindur (FIX)

;; pstutter (FIX)

;; pdurstutter (FIX)

;;; conversions (FIX - add more)

(ok (=
     (db-amp (amp-db 0.5))
     0.5))

;;; events (FIX - add more)

(ok (=
     1
     (sustain (event :dur 0 :sustain 1))))

(ok (=
     0.8
     (sustain (event))))

(ok (=
     0.5
     (legato (event :dur 0 :legato 0.5))))

(ok (=
     0.8
     (legato (event))))

(ok (=
     1
     (dur (event))))

(ok (eq
     :default
     (instrument (event))))

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
