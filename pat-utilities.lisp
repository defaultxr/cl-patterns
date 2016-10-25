(in-package :cl-patterns)

;; NOTES: use alexandria:clamp instead of clip

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

(defun wrap (number bottom top)
  "Wraps a number between BOTTOM and TOP, similar to `mod'."
  (+ (mod (- number bottom) (- top bottom)) bottom))

(defun re-intern (symbol package)
  "Interns a symbol from one package into a different package."
  (intern (symbol-name symbol) package))

(defun as-keyword (symbol)
  "Turns a symbol into a keyword."
  (re-intern symbol :keyword))

