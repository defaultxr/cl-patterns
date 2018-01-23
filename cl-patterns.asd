(asdf:defsystem #:cl-patterns
  :name "cl-patterns"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp."
  :version "0.3"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:closer-mop
               #:named-readtables
               #:split-sequence
               #:local-time)
  :serial t
  :components ((:file "package")
               (:file "src/utility")
               (:file "src/conversions")
               (:file "src/event")
               (:file "src/patterns")
               (:file "src/cycles")
               (:file "src/scales")
               (:file "src/backend")
               (:file "src/clock")
               (:file "src/readtable"))
  :in-order-to ((test-op (test-op "cl-patterns/tests"))))

(asdf:defsystem #:cl-patterns/supercollider
  :name "cl-patterns/supercollider"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with SuperCollider backend."
  :version "0.3"
  :depends-on (#:cl-patterns
               #:sc)
  :serial t
  :components ((:file "src/supercollider")))

(asdf:defsystem #:cl-patterns/incudine
  :name "cl-patterns/incudine"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with Incudine backend."
  :version "0.3"
  :depends-on (#:cl-patterns
               #:swank ;; FIX: remove this.
               #:incudine)
  :serial t
  :components ((:file "src/incudine")))

(asdf:defsystem #:cl-patterns/midi
  :name "cl-patterns/midi"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with ALSA MIDI backend."
  :version "0.3"
  :depends-on (#:cl-patterns
               #:cl-alsaseq)
  :serial t
  :components ((:file "src/midi")))

(asdf:defsystem #:cl-patterns/tests
  :name "cl-patterns tests suite."
  :depends-on (#:cl-patterns
               #:prove)
  :components ((:file "src/tests"))
  :perform (test-op (op c)
                    (uiop:symbol-call :prove :run c)))
