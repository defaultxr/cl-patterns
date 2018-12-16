(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; doc (FIX - add more)

(defparameter *special-keys.org* (cl-org-mode::read-org-file (asdf:system-relative-pathname :cl-patterns "doc/special-keys.org")))

(defun find-org-header (node contains)
  "Recursively search NODE and its child nodes to find a header that contains CONTAINS."
  (labels ((outline-node-p (node)
             (typep node 'cl-org-mode::outline-node))
           (recurse (node contains)
             (loop :for header :in (slot-value node 'cl-org-mode::child-nodes)
                :if (and (outline-node-p header)
                         (search contains (slot-value header 'cl-org-mode::heading)))
                :return header
                :if (outline-node-p header)
                :do (recurse header contains))))
    (recurse node contains)))

(defun find-missing-keys (header-name keys)
  "Search for any missing keys from KEYS under a header that contains HEADER-NAME.

Returns nil if none of the keys are missing, otherwise returns the list of undocumented keys."
  (let ((wrap-header (find-org-header *special-keys.org* header-name)))
    (loop :for key :in keys
       :if (not (find-org-header wrap-header (string-downcase (symbol-name key))))
       :collect key)))

(test special-keys.org ;; FIX: should give a more descriptive failure message listing the missing keys
  "Make sure the special keys are documented"
  (is-false (find-missing-keys "pbind special init keys" (keys cl-patterns::*pbind-special-init-keys*))
            "All pbind special wrap keys are documented")
  (is-false (find-missing-keys "pbind special wrap keys" (keys cl-patterns::*pbind-special-wrap-keys*))
            "All pbind special wrap keys are documented")
  (is-false (find-missing-keys "pbind special process keys" (keys cl-patterns::*pbind-special-process-keys*))
            "All pbind special process keys are documented"))
