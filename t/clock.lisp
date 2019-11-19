(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; clock

(test absolute-beats-to-timestamp
  "Test the `next-beat-for-quant' function"
  )

(test clock
  "Test basic clock functionality"
  (let ((clock (make-clock 3/4)))
    (is-true (= 3/4 (tempo clock))
             "clock's tempo is not set properly at creation time"))
  (let ((*clock* (make-clock)))
    (debug-clear-events)
    (play (pbind :dur (pn 1 4)))
    (clock-process *clock* 5)
    (is-true ;; when this test fails it's usually due to the `pstream-elt-future' function being wrong
     (apply #'/= (mapcar (lambda (e) (event-value e :beat-at-start))
                         (debug-recent-events 4)))
     "events that should be separated are being played simultaneously"))
  (disable-backend :debug))

(test tempo-change
  "Test clock tempo-changing functionality")

