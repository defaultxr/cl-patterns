(defpackage #:cl-patterns/tests
  (:use #:cl
        #:cl-patterns
        #:alexandria
        #:fiveam))

(in-package #:cl-patterns/tests)

(def-suite cl-patterns-tests
    :description "cl-patterns tests suite.")

(in-suite cl-patterns-tests)

(defparameter *previously-enabled-backends* nil
  "The list of backends that were enabled before the cl-patterns test suite was run.")

(test enable-debug-backend
  "Enable the debug backend to capture events"
  (setf *previously-enabled-backends* (enabled-backends))
  (mapc 'disable-backend *previously-enabled-backends*)
  (enable-backend :debug))

(defun undocumented-symbols (package)
  "Get a list of all the undocumented external symbols in PACKAGE."
  (let (symbols)
    (do-external-symbols (sym package symbols)
      (unless (position-if (lambda (type)
                             (documentation sym type))
                           (list 'function 'variable 'method-combination 'compiler-macro 'setf 'structure 'type))
        (push sym symbols)))))

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (undocumented-symbols :cl-patterns)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~s"
              undocumented)))
