(asdf:defsystem #:cl-patterns+supercollider
  :name "cl-patterns+supercollider"
  :author "modula t. defaultxr@gmail.com"
  :description "SuperCollider-inspired patterns library for Common Lisp with SuperCollider backend."
  :version "0.1"
  :depends-on (#:cl-patterns
               #:sc)
  :serial t
  :components ((:file "supercollider")))
