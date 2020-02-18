(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; org-mode utility functions

(defun child-nodes (node)
  "Get the child nodes of NODE."
  (slot-value node 'cl-org-mode::child-nodes))

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

(defun find-text-list-items (string)
  "Get a list of all unordered list items in STRING."
  (loop :for res :in (cl-ppcre:all-matches-as-strings (cl-ppcre:create-scanner "^- .*$" :multi-line-mode t) string)
     :collect (subseq res 2)))

(defun find-code-text (string)
  "Find the first instance of text surrounded by tildes in STRING."
  (let ((first (1+ (position #\~ string))))
    (subseq string first (position #\~ string :start first))))

(defun find-missing-keys (header-name keys)
  "Search for any missing keys from KEYS under a header that contains HEADER-NAME.

Returns nil if none of the keys are missing, otherwise returns the list of undocumented keys."
  (let ((wrap-header (find-org-header *special-keys.org* header-name)))
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
                                   :append (find-text-list-items (slot-value node 'cl-org-mode::text))
                                   :else
                                   :if (typep node 'cl-org-mode::outline-node)
                                   :append (process-nodes (child-nodes node)))))
                       (process-nodes nodes)))
         (code-texts (mapcar #'find-code-text list-items))
         (missing (remove-if (lambda (pat) (position pat code-texts :test #'string-equal)) (mapcar #'symbol-name (all-patterns)))))
    (is-false missing
              "some patterns are not documented in patterns.org: ~a"
              missing)))

(test special-keys.org
  "Make sure the special keys are documented"
  (let ((missing (find-missing-keys "pbind special init keys" (keys cl-patterns::*pbind-special-init-keys*))))
    (is-false missing
              "some pbind init keys are not documented: ~a"
              missing))
  (let ((missing (find-missing-keys "pbind special wrap keys" (keys cl-patterns::*pbind-special-wrap-keys*))))
    (is-false missing
              "some pbind wrap keys are not documented: ~a"
              missing))
  (let ((missing (find-missing-keys "pbind special process keys" (keys cl-patterns::*pbind-special-process-keys*))))
    (is-false missing
              "some pbind process keys are not documented: ~a"
              missing)))
