(defpackage #:cl-patterns
  (:use #:cl)
  ;; (:import-from :utilities #:)
  (:export
   #:plist-keys
   #:as-stream
   #:next
   #:next-n
   #:play

   #:defpattern
   
   #:pbind
   #:pseq
   #:pk
   #:prand
   #:pfunc
   
   #:event

   #:instrument
   #:group
   #:out

   #:amp
   #:db
   #:amp-db
   #:db-amp
   
   #:pan
   
   #:tempo
   #:dur
   #:legato
   #:sustain
   #:delta
   #:timing-offset
   #:delta-dur
   #:dur-delta
   
   #:freq
   #:midinote
   #:steps-per-octave
   #:other-params

   #:gete))
