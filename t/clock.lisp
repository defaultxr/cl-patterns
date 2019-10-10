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
             "clock's tempo is not set properly at creation time")))

(test tempo-change
  "Test clock tempo-changing functionality")

