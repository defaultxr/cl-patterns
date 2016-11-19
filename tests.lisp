(in-package :cl-patterns)

(require :prove)
(use-package :prove)

;; (plan)

;; patterns

(ok (eq 1 (next (pseq '(1 2 3)))))

(ok (equal (next-n (pseq '(1 2 3)) 3) (next-n (as-pstream (pseq '(1 2 3))) 3)))

;; conversions

(ok (eq (db-amp (amp-db 0.5)) 0.5))

;; events

(ok (= 1 (sustain (event :dur 0 :sustain 1))))

(ok (= 0.5 (legato (event :dur 0 :legato 0.5))))

(ok (= 1 (dur (event))))

(ok (eq :default (instrument (event))))

(ok (eq ))

;; tsubseq

(let* ((pb (pbind :dur 1/3)))
  (ok (= 2/3 (reduce #'+ (gete (tsubseq pb 1 1.5) :dur))))
  (ok (= 2/3 (reduce #'+ (gete (tsubseq (as-pstream pb) 1 1.5) :dur))))
  (ok (= 2/3 (reduce #'+ (gete (tsubseq (next-n pb 15) 1 1.5) :dur)))))

(let* ((pb (pbind :dur 1/3)))
  (ok (= 0.25 (reduce #'+ (gete (tsubseq* pb 1.25 1.5) :dur))))
  (ok (= 0.25 (reduce #'+ (gete (tsubseq* (as-pstream pb) 1.25 1.5) :dur))))
  (ok (= 0.25 (reduce #'+ (gete (tsubseq* (next-n pb 15) 1.25 1.5) :dur)))))

(finalize)
