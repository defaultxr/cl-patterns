(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;;; t/track.lisp - test `ptrack' and related functionality.

(test ptrack
  "Test ptrack"
  (is (every-event-equal
       (list
        (event :degree 0 :dur 1/2)
        (event :degree 1 :dur 1/2 :foo 3)
        (event :degree 99 :dur 1/2)
        (event :degree 99 :dur 2)
        (event :degree 4 :dur 4)
        (event :degree 2 :dur 1/2)
        (event :degree 99 :dur 2 :bar 3)
        (event :degree 7 :dur 1/2))
       (next-upto-n
        (ptrack
         (list :degree (pseries 0 1 8) :dur 1/2)
         (list
          (list)
          (list :foo 3)
          (list 99)
          (list 99 2)
          (list :dur 4)
          (list 99 :degree 2)
          (list 99 :dur 2 :bar 3)
          (list)
          (list 99)))))
      "ptrack yields incorrect results")
  (is (every-event-equal
       (list
        (event :foo 1)
        (event :foo 1)
        (event :foo 1))
       (next-upto-n
        (ptrack (list :foo 1) (list (list :foo 1)) :repeats 3)))
      "ptrack does not limit by repeats")
  (is (every-event-equal
       (list
        (event :foo 0)
        (event :foo 1)
        (event :foo 2))
       (next-upto-n
        (ptrack (list :foo 1) (list (list :foo (pseries))) :repeats 3)))
      "patterns embedded in ptrack rows are not used to generate values in ptrack's output events")
  (is (equal (list t t t)
             (mapcar (fn (numberp (event-value _ :foo)))
                     (next-upto-n
                      (ptrack (list :foo 1) (list (list :foo (pseries))) :repeats 3))))
      "functions in ptrack rows are not evalated to generate values in output events")
  (is (every-event-equal
       (list
        (event :dur 1/4 :type :rest)
        (event :dur 1/4 :type :rest)
        (event :dur 1/4)
        (event :dur 1/4 :foo 99))
       (next-n
        (ptrack (list :dur 1/4)
                  (list
                   :r
                   :rest
                   (list)
                   (list :foo 99)))
        4))
      "ptrack does not coerce to rests")
  (is (every-event-equal
       (list
        (event :dur 1/4 :foo 3)
        (event :dur 1/4 :foo 3))
       (next-n
        (ptrack (list :dur 1/4)
                  (list
                   (event :foo 3)))
        2))
      "ptrack does not accept lines as events")
  (is (every-event-equal
       (list
        (event :midinote 60 :dur 1)
        (event :midinote 50 :dur 3/4)
        (event :midinote 40 :dur 1/2)
        (event :midinote 60 :dur 1)
        (event :midinote 50 :dur 3/4)
        (event :midinote 40 :dur 1/2))
       (next-n (ptrack (list :midinote 70 :dur 1/4)
                         (list
                          (list 60)
                          (list '-)
                          (list '-)
                          (list '-)
                          (list 50)
                          (list '-)
                          (list '-)
                          (list 40)
                          (list '-)))
               6))
      "ptrack does not continue the previous note when a line is a dash")
  (is (every-event-equal
       (list
        (event :midinote 60)
        (event :freq 40)
        (event :midinote 60)
        (event :freq 40))
       (next-n (ptrack (list :midinote 60)
                         (list
                          (list 60)
                          (list :freq 40)))
               4))
      "ptrack steps don't override equivalent event keys"))

(test pt
  "Test pt"
  (is (every-event-equal
       (list
        (event :foo 2 :bar 0 :dur 1/4)
        (event :foo 1 :bar 1 :dur 1/4)
        (event :foo 9 :bar 90 :dur 1/2)
        (event :foo 1 :bar 4 :dur 1/4 :type :rest)
        (event :foo 2 :bar 5 :dur 1/4)
        (event :foo 1 :bar 6 :dur 1/4))
       (next-n (pt (:foo 1 :bar (pseries) :dur 1/4)
                   (2)
                   ()
                   (9 :bar 90)
                   (-)
                   :r)
               6))
      "pt does not expand properly"))
