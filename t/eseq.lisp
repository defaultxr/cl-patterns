(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;;; t/eseq.lisp - tests for `eseq' and related functionality.

(test eseq
  "Test basic eseq functionality"
  (is (= 2
         (eseq-length (eseq (list (event :foo 1) (event :foo 2)))))
      "eseq-length does not return correct results")
  (is (apply #'<= (mapcar #'beat
                          (eseq-events (eseq (list
                                              (event :beat 0)
                                              (event :beat 1)
                                              (event :beat 2)
                                              (event :beat 3))))))
      "eseq does not keep its events in order by beat")
  (is-true (eop-p (lastcar (next-n (eseq (list (event :beat 0))) 2)))
           "eseq does not yield eop at its end"))

(test bsubseq-eseq
  "Test the bsubseq function on eseqs"
  (is-true
   (every-event-equal
    (list
     (event :beat 1))
    (bsubseq (eseq (list
                    (event :beat 0)
                    (event :beat 1)
                    (event :beat 2)
                    (event :beat 3)))
             1 2))
   "every-event-equal doesn't select the correct event")
  (is-true
   (every-event-equal
    (list
     (event :beat 1)
     (event :beat 2))
    (bsubseq (eseq (list
                    (event :beat 0)
                    (event :beat 1)
                    (event :beat 2)
                    (event :beat 3)))
             1 3))
   "every-event-equal doesn't select the correct events"))
