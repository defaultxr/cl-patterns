(in-package :cl-patterns)

(require :sc)

(defgeneric play-sc (item))

(defmethod play-sc ((item t))
  (eval (append (list (re-intern (instrument item) :sc)))))

