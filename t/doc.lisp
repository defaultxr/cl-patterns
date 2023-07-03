;;;; t/doc.lisp - tests to ensure all patterns, functions, classes, etc are documented in docstrings, doc/, etc.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; utility

(defun find-code-text (string)
  "Find all instances of text surrounded by tildes in STRING prior to the first \" - \"."
  (let ((dash-loc (search " - " string)))
    (mapcar (lambda (str) (string-trim (list #\~) str))
            (cl-ppcre:all-matches-as-strings "~([^~]*?)~" string :end dash-loc))))

(defun find-missing-keys (file header-name keys)
  "Get a list of all KEYS in FILE under a header that contains HEADER-NAME."
  (multiple-value-bind (header content) (file-extract-org-header file header-name)
    (declare (ignore header))
    (set-difference keys
                    (mapcan #'find-code-text (stream-extract-org-headers (make-string-input-stream content)))
                    :test #'string-equal)))

;;; org files

(defparameter *patterns.org* (asdf:system-relative-pathname :cl-patterns "doc/patterns.org"))

(defparameter *special-keys.org* (asdf:system-relative-pathname :cl-patterns "doc/special-keys.org"))

;;; tests

(test patterns.org
  "Make sure all patterns are listed in patterns.org"
  (let* ((list-items (file-extract-org-lists *patterns.org*))
         (code-texts (flatten (mapcar #'find-code-text list-items)))
         (missing (remove-duplicates
                   (remove-if (lambda (pat)
                                (position (symbol-name pat) code-texts :test #'string-equal))
                              (all-patterns)))))
    (is-false missing
              "some patterns are not documented in patterns.org: ~A" missing)))

(test special-keys.org
  "Make sure the special keys are documented"
  (let ((missing (find-missing-keys *special-keys.org*
                                    "pbind special init keys"
                                    (keys cl-patterns::*pbind-special-init-keys*))))
    (is-false missing
              "some pbind init keys are not documented: ~A" missing))
  (let ((missing (find-missing-keys *special-keys.org*
                                    "pbind special wrap keys"
                                    (keys cl-patterns::*pbind-special-wrap-keys*))))
    (is-false missing
              "some pbind wrap keys are not documented: ~A" missing))
  (let ((missing (find-missing-keys *special-keys.org*
                                    "pbind special process keys"
                                    (keys cl-patterns::*pbind-special-process-keys*))))
    (is-false missing
              "some pbind process keys are not documented: ~A" missing)))
