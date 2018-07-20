(asdf:defsystem #:cl-patterns
  :name "cl-patterns"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp."
  :license "GNU General Public License v3.0"
  :version "0.4"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:closer-mop
               #:named-readtables
               #:split-sequence
               #:local-time
               #:dissect)
  :serial t
  :components ((:file "package")
               (:file "src/utility")
               (:file "src/conversions")
               (:file "src/scales")
               (:file "src/event")
               (:file "src/patterns/patterns")
               (:file "src/patterns/bjorklund")
               (:file "src/patterns/cycles")
               (:file "src/patterns/tracker")
               (:file "src/backend")
               (:file "src/clock")
               (:file "src/sugar"))
  :in-order-to ((test-op (test-op "cl-patterns/tests"))))

(asdf:defsystem #:cl-patterns/supercollider
  :name "cl-patterns/supercollider"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with SuperCollider backend."
  :license "GNU General Public License v3.0"
  :version "0.4"
  :depends-on (#:cl-patterns
               #:cl-collider)
  :serial t
  :components ((:file "src/backends/supercollider")))

(asdf:defsystem #:cl-patterns/incudine
  :name "cl-patterns/incudine"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with Incudine backend."
  :license "GNU General Public License v3.0"
  :version "0.4"
  :depends-on (#:cl-patterns
               #:swank
               #:incudine)
  :serial t
  :components ((:file "src/backends/incudine")))

(asdf:defsystem #:cl-patterns/midi
  :name "cl-patterns/midi"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with ALSA MIDI backend."
  :license "GNU General Public License v3.0"
  :version "0.4"
  :depends-on (#:cl-patterns
               #:cl-alsaseq)
  :serial t
  :components ((:file "src/backends/midi")))

(asdf:defsystem #:cl-patterns/tests
  :name "cl-patterns/tests"
  :author "modula t. <defaultxr@gmail.com>"
  :description "FiveAM-based tests suite for cl-patterns."
  :license "GNU General Public License v3.0"
  :depends-on (#:cl-patterns
               #:fiveam)
  :components ((:file "src/tests"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:cl-patterns-tests
                                                         :cl-patterns/tests))))
