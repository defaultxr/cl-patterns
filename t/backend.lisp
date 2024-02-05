;;;; t/backend.lisp - tests and helpers for general non-specific backend functionality.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test backend-input-processors
  "Test backend input processors"
  (with-fixture debug-backend-and-clock ()
    (let ((backend (find-backend 'debug-backend)))
      (push (lambda (ev) (incf (freq ev))) (backend-input-processors backend))
      (play (pbind :freq (pseq (list 1 2 3))))
      (clock-process *clock* 3)
      (is (equal (list 2 3 4)
                 (mapcar #'freq (nreverse (debug-backend-recent-events backend 3))))
          "backend-input-processors don't work"))))
