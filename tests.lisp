(require :prove)
(use-package :prove)

(plan)

(ok (eq 1 (next (pseq '(1 2 3)))))

(finalize)
