(defpackage #:cl-patterns/tests
  (:use :cl
        :cl-patterns
        :fiveam))

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
