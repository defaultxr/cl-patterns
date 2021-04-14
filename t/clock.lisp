(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; clock

(test absolute-beats-to-timestamp
  "Test the `next-beat-for-quant' function"
  )

(test clock
  "Test basic clock functionality"
  (with-fixture with-debug-backend-and-clock (3/4)
    (is-true (= 3/4 (tempo *clock*))
             "clock's tempo is not set properly at creation time")
    (play (pbind :dur (pn 1 4)))
    (clock-process *clock* 5)
    (is-true ;; when this test fails it's usually due to the `pstream-elt-future' function being wrong
     (apply #'/= (mapcar (lambda (e) (event-value e :beat-at-start))
                         (debug-recent-events 4)))
     "events that should be separated are being played simultaneously")))

(test tempo-change
  "Test clock tempo-changing functionality")

(test swap-patterns
  "Test clock pattern swapping functionality (i.e. `end-quant')"
  (let ((cl-patterns::*pdef-dictionary* (make-hash-table)))
    (with-fixture with-debug-backend-and-clock ()
      (pdef 'test (pbind :x (pseries) :dur 1 :end-quant 4))
      (play (pdef 'test))
      (clock-process *clock* 5)
      (pdef 'test (pbind :x (pseries 0 -1) :end-quant 4))
      (clock-process *clock* 6)
      (let ((recents (mapcar (lambda (e) (event-value e :x)) (nreverse (debug-recent-events 11)))))
        (is-true (equal (list 0 1 2 3 4 5 6 7 8 0 -1) recents)
                 "clock does not swap redefined pdefs at the correct time according to their end-quant (got ~s)"
                 recents)))
    (with-fixture with-debug-backend-and-clock ()
      (pdef 'test (pbind :x (pseries 0 1 4)))
      (play (pdef 'test))
      (clock-process *clock* 2)
      (pdef 'test (pbind :x (pseries 0 -1 4)))
      (clock-process *clock* 4)
      (let ((recents (mapcar (lambda (e) (event-value e :x)) (nreverse (debug-recent-events 6)))))
        (is-true (equal (list 0 1 2 3 0 -1) recents)
                 "clock does not swap redefined pdefs at their end by default (got ~s)"
                 recents)))))

(test play-expired-events
  "Test the clock's play-expired-events setting"
  (with-fixture with-debug-backend-and-clock ()
    (setf (slot-value *clock* 'cl-patterns::play-expired-events) t)
    (play (pbind :dur (pn 1 4)))
    (setf (beat *clock*) 5)
    (clock-process *clock* 2)
    (let ((recent (debug-recent-events 4)))
      (is-true (= 4 (length recent))
               "clock does not play expired events when play-expired-events is true")))
  (with-fixture with-debug-backend-and-clock ()
    (setf (slot-value *clock* 'cl-patterns::play-expired-events) nil)
    (play (pbind :dur (pn 1/4 4)))
    (setf (beat *clock*) 5)
    (sleep 1)
    (clock-process *clock* 2)
    (let ((recent (debug-recent-events 4)))
      (is-true (= 0 (length recent))
               "clock does not skip expired events when play-expired-events is false; got ~s" recent))))
