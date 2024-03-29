;;;; t/test.lisp - basic tests and test utilities/fixtures/etc for the cl-patterns test suite.

(defpackage #:cl-patterns/tests
  (:use #:cl
        #:cl-patterns
        #:alexandria
        #:mutility
        #:fiveam))

(in-package #:cl-patterns/tests)

(def-suite cl-patterns-tests
  :description "cl-patterns tests suite.")

(in-suite cl-patterns-tests)

(defmacro skip-unless (condition &body body)
  "Generate a `fiveam::test-skipped' result if CONDITION is not true; otherwise, run BODY."
  `(if (not ,condition) (skip "Skipped due to null ~S" ',condition) ,@body))

(defmacro skips-unless (condition &body body)
  "Generate a `fiveam::test-skipped' result for each item in BODY if CONDITION is not true; otherwise, run BODY.

See also: `skip-unless'"
  `(if (not ,condition)
       (progn ,@(loop :repeat (length body) :collect `(skip "Skipped due to null ~S" ',condition)))
       (progn ,@body)))

(def-fixture debug-backend-and-clock (&rest clock-args)
  "Temporarily set the backend and clock for testing."
  (let* ((*clock* (apply #'make-clock clock-args))
         (cl-patterns::*backends* nil)
         (backend (make-backend 'debug-backend)))
    (debug-backend-clear-recent-events backend)
    (unwind-protect (&body)
      (debug-backend-clear-recent-events backend)
      (backend-stop backend))))

(def-fixture temporary-pdef-dictionary ()
  "Temporarily create a new pdef dictionary for testing."
  (let ((cl-patterns::*pdef-dictionary* (make-hash-table)))
    (&body)))

(test system-attributes
  "Check that the system has all the standard attributes"
  (let ((missing (system-missing-attributes '#:cl-patterns)))
    (is-false missing
              "The system definition is missing attributes: ~S" missing)))

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (package-undocumented-symbols '#:cl-patterns)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~S" undocumented)))

(test docstrings-broken-links
  "Check for any broken links in docstrings of exported symbols"
  (let ((symbols (package-docstrings-with-broken-links '#:cl-patterns)))
    (is-false symbols
              "some exported symbols have docstrings that contain broken links: ~S" symbols)))
