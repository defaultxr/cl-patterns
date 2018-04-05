(in-package :cl-patterns)

(named-readtables:defreadtable :cl-patterns
  (:merge :standard)
  (:dispatch-macro-char #\# #\T #'cl-patterns::tracker-shorthand))

(defmethod tempo ((number number))
  ;; convenience method to quickly set the tempo of the default clock in bpm
  (setf (tempo *clock*) number))
