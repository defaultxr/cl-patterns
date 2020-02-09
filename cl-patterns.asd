(asdf:defsystem #:cl-patterns
  :name "cl-patterns"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp."
  :license "MIT"
  :version "0.6"
  :homepage "https://w.struct.ws/cl-patterns/"
  :bug-tracker "https://github.com/defaultxr/cl-patterns/issues"
  :source-control (:git "git@github.com:defaultxr/cl-patterns.git")
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:closer-mop
               #:named-readtables
               #:split-sequence
               #:local-time
               #:dissect)
  :serial t
  :components ((:file "src/package")
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
               ;; implementations don't currently push a symbol to *features* for extensible sequences.
               ;; the following line tests for the sequence package, which should only be available when extensible-sequence functionality is available.
               #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or))
               (:file "src/sequence-extensions"))
  :in-order-to ((test-op (test-op "cl-patterns/tests"))))

(asdf:defsystem #:cl-patterns/debug
  :name "cl-patterns/debug"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with debug backend."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns)
  :serial t
  :components ((:file "src/backends/debug")))

(asdf:defsystem #:cl-patterns/supercollider
  :name "cl-patterns/supercollider"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with SuperCollider backend."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns
               #:cl-collider)
  :serial t
  :components ((:file "src/backends/supercollider")))

(asdf:defsystem #:cl-patterns/incudine
  :name "cl-patterns/incudine"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with Incudine backend."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns
               #:incudine)
  :serial t
  :components ((:file "src/backends/incudine")))

(asdf:defsystem #:cl-patterns/alsa-midi
  :name "cl-patterns/alsa-midi"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with ALSA MIDI backend."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns
               #:cl-alsaseq)
  :serial t
  :components ((:file "src/backends/alsa-midi")))

(asdf:defsystem #:cl-patterns/midifile
  :name "cl-patterns/midifile"
  :author "modula t. <defaultxr@gmail.com>"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp with MIDI file functionality."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns
               #:midi)
  :serial t
  :components ((:file "src/formats/midifile")))
(asdf:defsystem #:cl-patterns/tests
  :name "cl-patterns/tests"
  :author "modula t. <defaultxr@gmail.com>"
  :description "FiveAM-based tests suite for cl-patterns."
  :license "MIT"
  :depends-on (#:cl-patterns/debug
               #:fiveam
               #:cl-org-mode
               #:cl-ppcre)
  :components ((:file "t/test")
               (:file "t/utility")
               (:file "t/conversions")
               (:file "t/event")
               (:file "t/patterns")
               ;; (:file "t/bjorklund")
               ;; (:file "t/cycles")
               ;; (:file "t/backend")
               (:file "t/tracker")
               (:file "t/clock")
               (:file "t/doc"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:cl-patterns-tests
                                                         :cl-patterns/tests))))
