(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;;; t/doc.lisp - tests to ensure all patterns, functions, classes, etc are documented in docstrings, doc/, etc.

;;; cl-org-mode utility functions

(defun child-nodes (node)
  "Get the child nodes of NODE."
  (slot-value node 'cl-org-mode::child-nodes))

(defun node-text (node)
  "Get the text of NODE."
  (slot-value node 'cl-org-mode::text))

(defun text-list-items (string)
  "Get a list of all unordered list items in STRING."
  (loop :for res :in (cl-ppcre:all-matches-as-strings (cl-ppcre:create-scanner "^- .*$" :multi-line-mode t) string)
     :collect (subseq res 2)))

(defun find-org-header (node contains)
  "Recursively search NODE and its child nodes to find a header that contains CONTAINS."
  (labels ((outline-node-p (node)
             (typep node 'cl-org-mode::outline-node))
           (recurse (node contains)
             (loop :for header :in (child-nodes node)
                :if (and (outline-node-p header)
                         (search contains (slot-value header 'cl-org-mode::heading)))
                :return header
                :if (outline-node-p header)
                :do (recurse header contains))))
    (recurse node contains)))


(defun find-code-text (string)
  "Find all instances of text surrounded by tildes in STRING prior to the first \" - \"."
  (let ((dash-loc (search " - " string)))
    (mapcar (lambda (str) (string-trim (list #\~) str))
            (cl-ppcre:all-matches-as-strings "~([^~]*?)~" string :end dash-loc))))

(defun find-missing-keys (node header-name keys)
  "Search for any missing in NODE under a header that contains HEADER-NAME.

Returns nil if none of the keys are missing, otherwise returns the list of undocumented keys."
  (let ((wrap-header (find-org-header node header-name)))
    (loop :for key :in keys
       :if (not (find-org-header wrap-header (string-downcase (symbol-name key))))
       :collect key)))

;;; org files

(defparameter *special-keys.org* (cl-org-mode::read-org-file (asdf:system-relative-pathname :cl-patterns "doc/special-keys.org")))

(defparameter *patterns.org* (cl-org-mode::read-org-file (asdf:system-relative-pathname :cl-patterns "doc/patterns.org")))

;;; tests

(test patterns.org
  "Make sure all patterns are listed in patterns.org"
  (let* ((nodes (child-nodes *patterns.org*))
         (list-items (labels ((process-nodes (nodes)
                                (loop :for node :in nodes
                                      :if (typep node 'cl-org-mode::text-node)
                                        :append (text-list-items (node-text node))
                                      :else
                                        :if (typep node 'cl-org-mode::outline-node)
                                          :append (process-nodes (child-nodes node)))))
                       (process-nodes nodes)))
         (code-texts (flatten (mapcar #'find-code-text list-items)))
         (missing (remove-if (lambda (pat) (position pat code-texts :test #'string-equal)) (mapcar #'symbol-name (all-patterns)))))
    (is-false missing
              "some patterns are not documented in patterns.org: ~a"
              missing)))

(test special-keys.org
  "Make sure the special keys are documented"
  (let ((missing (find-missing-keys *special-keys.org*
                                    "pbind special init keys"
                                    (keys cl-patterns::*pbind-special-init-keys*))))
    (is-false missing
              "some pbind init keys are not documented: ~a"
              missing))
  (let ((missing (find-missing-keys *special-keys.org*
                                    "pbind special wrap keys"
                                    (keys cl-patterns::*pbind-special-wrap-keys*))))
    (is-false missing
              "some pbind wrap keys are not documented: ~a"
              missing))
  (let ((missing (find-missing-keys *special-keys.org*
                                    "pbind special process keys"
                                    (keys cl-patterns::*pbind-special-process-keys*))))
    (is-false missing
              "some pbind process keys are not documented: ~a"
              missing)))
