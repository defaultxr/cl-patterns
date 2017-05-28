(in-package :cl-patterns)

(named-readtables:defreadtable :cl-patterns
  (:merge :standard)
  (:dispatch-macro-char #\# #\T #'cl-patterns::tracker-shorthand))
