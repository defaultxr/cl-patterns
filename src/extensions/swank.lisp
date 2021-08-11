(in-package #:cl-patterns)

;; show the arguments of the function being called in pnary
(defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'pnary)) argument-forms)
  (let ((function-name-form (car argument-forms)))
    (when (and (listp function-name-form)
               (length= 2 function-name-form)
               (member (car function-name-form) '(quote function) :test #'eq))
      (let ((function-name (cadr function-name-form)))
        (when (fboundp function-name)
          (let ((function-arglist (swank::compute-enriched-decoded-arglist function-name (cdr argument-forms))))
            (push 'operator (swank::arglist.required-args function-arglist))
            (return-from swank::compute-enriched-decoded-arglist
              (values function-arglist (list function-name-form) t)))))))
  (call-next-method))
