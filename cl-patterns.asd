(asdf:defsystem #:cl-patterns
  :name "cl-patterns"
  :description "Pattern library for algorithmic music composition and performance in Common Lisp"
  :author "modula t."
  :license "MIT"
  :version "0.6"
  :homepage "https://w.struct.ws/cl-patterns/"
  :bug-tracker "https://github.com/defaultxr/cl-patterns/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/cl-patterns.git")
  :depends-on (#:alexandria
               #:mutility
               #:mutility/loopy
               #:bordeaux-threads
               #:closer-mop
               #:named-readtables
               #:local-time
               #:dissect)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utility")
               ;; extensions/swank.lisp or extensions/slynk.lisp conditionally loaded at the end of utility.lisp
               (:file "conversions")
               (:file "scales")
               (:file "event")
               (:module patterns
                :components ((:file "patterns")
                             (:file "pdef")
                             (:file "bjorklund")
                             (:file "cycles")
                             ;; (:file "metropolis")
                             (:file "tracker")
                             (:file "export")))
               (:file "eseq")
               (:file "backend")
               (:file "clock")
               (:file "triggy")
               ;; implementations don't currently push a symbol to *features* for extensible sequences.
               ;; the following line tests for the sequence package, which should only be available when extensible-sequence functionality is available.
               #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or))
               (:file "extensions/sequence"))
  :in-order-to ((test-op (test-op "cl-patterns/tests"))))

(asdf:defsystem #:cl-patterns/sugar ;; deprecated, don't use
  :name "cl-patterns/sugar"
  :description "cl-patterns with additional syntax sugar exports. Deprecated system; use the mutility library or manually import functions instead."
  :author "modula t."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns)
  :serial t
  :components ((:file "src/sugar")))

(asdf:defsystem #:cl-patterns/debug
  :name "cl-patterns/debug"
  :description "cl-patterns with debug backend"
  :author "modula t."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns)
  :serial t
  :components ((:file "src/backends/debug")))

(asdf:defsystem #:cl-patterns/supercollider
  :name "cl-patterns/supercollider"
  :description "cl-patterns with SuperCollider/cl-collider backend"
  :author "modula t."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns
               #:cl-collider)
  :serial t
  :components ((:file "src/backends/supercollider")
               (:file "src/formats/supercollider-score")))

(asdf:defsystem #:cl-patterns/incudine
  :name "cl-patterns/incudine"
  :description "cl-patterns with Incudine backend"
  :author "modula t."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns
               #:incudine)
  :serial t
  :components ((:file "src/backends/incudine")))

(asdf:defsystem #:cl-patterns/alsa-midi
  :name "cl-patterns/alsa-midi"
  :description "cl-patterns with ALSA MIDI backend"
  :author "modula t."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns
               #:cl-alsaseq)
  :serial t
  :components ((:file "src/backends/alsa-midi")))

(asdf:defsystem #:cl-patterns/midifile
  :name "cl-patterns/midifile"
  :description "cl-patterns with MIDI file functionality"
  :author "modula t."
  :license "MIT"
  :version "0.6"
  :depends-on (#:cl-patterns
               #:midi)
  :serial t
  :components ((:file "src/formats/midifile")))
(asdf:defsystem #:cl-patterns/tests
  :name "cl-patterns tests"
  :description "FiveAM-based test suite for cl-patterns"
  :author "modula t."
  :license "MIT"
  :depends-on (#:cl-patterns/debug
               #:fiveam
               #:mutility/test-helpers
               #:cl-org-mode
               #:cl-ppcre)
  :pathname "t/"
  :serial t
  :components ((:file "test")
               (:file "utility")
               (:file "conversions")
               (:file "event")
               (:file "patterns")
               (:file "pdef")
               (:file "bjorklund")
               (:file "cycles")
               (:file "tracker")
               (:file "eseq")
               ;; (:file "backend")
               (:file "clock")
               (:file "doc"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:cl-patterns-tests
                                                         :cl-patterns/tests))))
