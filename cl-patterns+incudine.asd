(asdf:defsystem #:cl-patterns+incudine
  :name "cl-patterns+incudine"
  :author "modula t. defaultxr@gmail.com"
  :description "SuperCollider-inspired patterns library for Common Lisp with Incudine backend."
  :version "0.1"
  :depends-on (#:cl-patterns
               #:incudine)
  :serial t
  :components ((:file "src/incudine")))
