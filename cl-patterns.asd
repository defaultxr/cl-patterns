(asdf:defsystem #:cl-patterns
  :name "cl-patterns"
  :author "modula t. defaultxr@gmail.com"
  :description "SuperCollider-inspired patterns library for Common Lisp."
  :version "0.2"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:closer-mop
               #:named-readtables
               #:split-sequence
               #:local-time
               #:swank ;; FIX: remove this.
               )
  :serial t
  :components ((:file "package")
               (:file "src/pat-utilities")
               (:file "src/patterns")
               (:file "src/event")
               (:file "src/scales")
               (:file "src/clock")
               (:file "src/readtable"))
  :in-order-to ((test-op (test-op "cl-patterns/tests"))))

(asdf:defsystem #:cl-patterns/tests
  :name "cl-patterns tests"
  :depends-on (#:cl-patterns
               #:prove)
  :components ((:file "src/tests"))
  :perform (test-op (op c)
                    (uiop:symbol-call :prove :run c)))
