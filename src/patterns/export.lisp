(in-package #:cl-patterns)

;;;; export.lisp - automatically export all the patterns in `*patterns*'.

(dolist (pattern *patterns*)
  (export pattern)
  (let ((pstream-class (intern (concat (symbol-name pattern) "-PSTREAM") 'cl-patterns)))
    (when (find-class pstream-class nil)
      (export pstream-class))))
