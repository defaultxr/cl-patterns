(in-package #:cl-patterns)

;;;; generic-cl.lisp - methods implementing generic-cl functionality for cl-patterns classes.
;;; https://alex-gutev.github.io/generic-cl/

(defmethod generic-cl:get (key (object event) &optional default)
  (multiple-value-bind (result exists) (event-value object key)
    (values (or result default) exists)))

(defmethod generic-cl:coerce ((pattern pattern) (type (eql 'pattern)))
  pattern)

(defmethod generic-cl:coerce ((pattern pattern) (type (eql 'pstream)))
  (as-pstream pattern))

(defmethod generic-cl:coerce ((pattern pattern) (type (eql 'eseq)))
  (as-eseq pattern))

(defmethod generic-cl:coerce ((scale scale) (type (eql 'list)))
  (scale-notes scale))

(defmethod generic-cl:coerce ((tuning tuning) (type (eql 'list)))
  (tuning-pitches tuning))

(defmethod generic-cl:at ((pstream pstream))
  (last-output pstream))

(defmethod generic-cl:endp ((pstream pstream))
  (ended-p pstream))

(defmethod generic-cl:advance ((pstream pstream))
  (next pstream))
