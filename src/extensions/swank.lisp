(in-package #:cl-patterns)

;; show the arguments of the function being called in pnary
(defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'pnary)) argument-forms)
  (swank::compute-enriched-decoded-arglist 'apply argument-forms))
