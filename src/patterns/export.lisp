(in-package #:cl-patterns)

;;;; export.lisp
;;; just export all of the patterns in `*patterns*'

(dolist (pattern *patterns*)
  (export pattern)
  (let ((pstream-class (intern (concat (symbol-name pattern) "-PSTREAM") 'cl-patterns)))
    (when (find-class pstream-class nil)
      (export pstream-class))))
