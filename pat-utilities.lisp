(in-package :cl-patterns)

(defun output (&rest strings)
  "Concatenates and prints strings."
  (fresh-line)
  (format t "~{~A~}~%" (remove-if #'null strings))
  (finish-output))

(defun repeat (item num)
  "Returns a list containing 'num' items. If 'item' is a function, return a list of 'num' of the result of that function."
  (when (> num 0)
    (cons (if (eq 'function (type-of item))
              (funcall item)
              item)
          (repeat item (- num 1)))))

(setf (fdefinition 'r) #'repeat)

(defun re-intern (symbol package)
  (intern (symbol-name symbol) package))

(defun as-keyword (symbol)
  (re-intern symbol :keyword))

