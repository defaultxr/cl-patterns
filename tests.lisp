(in-package :cl-patterns)

(require :prove)
(use-package :prove)

(plan)

(ok (eq 1 (next (pseq '(1 2 3)))))

(ok (eq (db-amp (amp-db 0.5)) 0.5))

(finalize)
