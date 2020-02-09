(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; tracker

(test ptracker
  "Test ptracker"
  (is (every-event-equal
       (list
        (event :degree 0 :dur 1/2)
        (event :degree 1 :foo 3 :dur 1/2)
        (event :degree 99 :dur 1/2)
        (event :degree 99 :dur 2)
        (event :degree 4 :dur 4)
        (event :degree 2 :dur 1/2)
        (event :degree 99 :dur 2 :bar 3)
        (event :degree 7 :dur 1/2))
       (next-upto-n
        (ptracker
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
          (list 99)))))))
