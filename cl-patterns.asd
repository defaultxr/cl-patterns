(asdf:defsystem #:cl-patterns
  :name "cl-patterns"
  :author "modula t. defaultxr@gmail.com"
  :description "SuperCollider-inspired patterns library for Common Lisp."
  :version "0.1"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:closer-mop
               #:named-readtables
               #:split-sequence
               #:local-time)
  :serial t
  :components ((:file "package")
               (:file "src/pat-utilities")
               (:file "src/patterns")
               (:file "src/event")
               (:file "src/scales")
               (:file "src/clock")
               (:file "src/readtable")))
