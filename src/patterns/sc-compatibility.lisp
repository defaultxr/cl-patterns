;;;; sc-compatibility.lisp - SuperCollider-compatible versions of cl-patterns patterns.
;;; Defines functionality in the cl-patterns/sc-compatibility package when the cl-patterns/sc-compatibility system is loaded.
;;; This is a major work in progress and is considered low priority, so don't expect much.

(in-package #:cl-patterns/sc-compatibility)

;;; punop

(defun punop (operator pattern)
  (pnary operator pattern))

;;; pbinop

(defun pbinop (operator pattern-a pattern-b)
  (pnary operator pattern-a pattern-b))

;;; pnaryop

(defun pnaryop (operator pattern arglist)
  (apply #'pnary operator pattern arglist))

;;; ptime

(defun ptime ()
  (pbeat))
