;;;; t/render.lisp - tests for generic render functionality.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test render
  "Test the `render' function"
  (is (pstream-p (render (pseq (list 1 2 3)) :pstream))
      "render :pstream does not coerce to a pstream")
  (is (listp (render (pseq (list 1 2 3)) :list))
      "render :list does not coerce to a list")
  (is (length= 7 (render (pseq (list 1 2)) :list :max-length 7))
      "render :list with :max-length 7 renders the wrong number of items")
  (is (= 3 (reduce #'+ (mapcar #'dur (render (pbind :dur (pn 1 3)) :list :max-duration 8))))
      "render :list with :max-duration 8 on a 3-dur pattern does not result in 3-dur output"))
