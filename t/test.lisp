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

(def-fixture with-debug-backend-and-clock (&rest clock-args)
  "Temporarily set the backend and clock for testing."
  (let ((previously-enabled-backends (enabled-backends))
        (*clock* (apply 'make-clock clock-args)))
    (mapc 'disable-backend (enabled-backends))
    (enable-backend :debug)
    (debug-clear-events)
    (&body)
    (debug-clear-events)
    (disable-backend :debug)
    (mapc 'enable-backend previously-enabled-backends)))

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (undocumented-symbols :cl-patterns)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~s"
              undocumented)))
