(asdf:defsystem #:cl-patterns
  :name "cl-patterns"
  :author "modula t. defaultxr@gmail.com"
  :description "SuperCollider-inspired patterns library for Common Lisp."
  :version "0.1"
  :depends-on (#:alexandria
               #:closer-mop)
  :weakly-depends-on (#:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "pat-utilities")
               (:file "patterns")
               (:file "event")
               ;; (:file "supercollider")
               ;; (:file "incudine")
               ))
