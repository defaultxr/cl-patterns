;;;; t/patterns/batch.lisp - test batch pattern abstract classes and their functionality.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test serial-pstream
  "Test serial pstream behaviors")

(test batch-pstream
  "Test batch pstream behaviors")

(test serializing-pstreams
  "Test serializing batch pstreams"
  )

;; FIX: seems to have an infinite loop issue, problem is likely inside batched-pstream's `pull' method:
;; (test batching-pstreams
;;   "Test batching serial pstreams"
;;   (let* ((evs (list (event :beat 0)
;;                     (event :beat 1)
;;                     (event :beat 3)
;;                     (event :beat 4)))
;;          (batch-pstr (as-batch-pstream (pseq evs))))
;;     (is (every-event-equal (subseq evs 0 2) (pull batch-pstr 2)))
;;     (is (every-event-equal nil (pull batch-pstr 1)))
;;     (is (every-event-equal (subseq evs 2 3) (pull batch-pstr 1)))
;;     (is (every-event-equal (subseq evs 3) (dprint (pull batch-pstr 4))))))
