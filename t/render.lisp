;;;; t/render.lisp - tests for generic render functionality.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test render
  "Test the `render' function"
  (is (typep (render (pbind :dur 1) :eseq) 'eseq)
      "render :eseq does not coerce to an eseq")
  (is (typep (render (pseq (list 1 2 3)) :pstream) 'pstream)
      "render :pstream does not coerce to a pstream")
  (is (typep (render (pseq (list 1 2 3)) :list) 'list)
      "render :list does not coerce to a list")
  (is (length= 7 (render (pseq (list 1 2)) :list :max-length 7))
      "render :list with :max-length 7 renders the wrong number of items"))

