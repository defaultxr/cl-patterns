(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; clock

(test absolute-beats-to-timestamp
  "Test the `next-beat-for-quant' function"
  )

(test clock
  "Test basic clock functionality"
  (let ((*clock* (make-clock 1)))
    (is-true (= 1 (tempo *clock*))
             "clock's tempo is correct")))

(test tempo-change
  "Test clock tempo-changing functionality")

