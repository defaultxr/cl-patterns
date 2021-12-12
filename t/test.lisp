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

(def-fixture debug-backend-and-clock (&rest clock-args)
  "Temporarily set the backend and clock for testing."
  (let ((previously-enabled-backends (enabled-backends))
        (*clock* (apply 'make-clock clock-args)))
    (dolist (backend (enabled-backends))
      (disable-backend backend :stop nil))
    (enable-backend :debug)
    (debug-clear-events)
    (&body)
    (debug-clear-events)
    (disable-backend :debug)
    (mapc 'enable-backend previously-enabled-backends)))

(def-fixture temporary-pdef-dictionary ()
  "Temporarily create a new pdef dictionary for testing."
  (let ((cl-patterns::*pdef-dictionary* (make-hash-table)))
    (&body)))

(test system-attributes
  "Check that the system has all the standard attributes"
  (let ((missing (system-missing-attributes :cl-patterns)))
    (is-false missing
              "The asdf system definition is missing attributes: ~s" missing)))

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (undocumented-symbols :cl-patterns)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~s" undocumented)))

(test docstrings-broken-links
  "Check for any broken links in docstrings of exported symbols"
  (let ((symbols (docstrings-with-broken-links :cl-patterns)))
    (is-false symbols
              "some exported symbols have docstrings that contain broken links: ~s" symbols)))
