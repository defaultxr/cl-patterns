(in-package :cl-patterns)

(defun output (&rest strings)
  "Concatenates and prints strings."
  (fresh-line)
  (format t "~{~A~}~%" (remove-if #'null strings))
  (finish-output))
