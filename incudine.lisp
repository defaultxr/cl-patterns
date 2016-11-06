(in-package :cl-patterns)

;; (require :incudine)

(defgeneric play-incudine (item))

;; (defun simple-test (time)
;;   (set-controls 1 :freq (+ 100 (random 1000)) :amp (+ .1 (random .3)))
;;   (let ((next (+ time #[1 beat])))
;;     (at next #'simple-test next)))

(defmethod play-incudine ((item t))
  (apply #'incudine:set-controls 3 (event-plist item)))

