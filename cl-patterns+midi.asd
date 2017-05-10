(asdf:defsystem #:cl-patterns+midi
  :name "cl-patterns+midi"
  :author "modula t. defaultxr@gmail.com"
  :description "SuperCollider-inspired patterns library for Common Lisp with ALSA MIDI backend."
  :version "0.1"
  :depends-on (#:cl-patterns
               #:cl-alsaseq)
  :serial t
  :components ((:file "src/midi")))
