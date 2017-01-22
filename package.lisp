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
   #:pbind-pstream

   #:pseq
   #:pk
   #:prand
   #:pxrand
   #:pfunc
   #:pr
   #:pdef
   #:plazy
   #:plazyn
   #:pcycles
   #:pshift
   #:pn
   #:pshuf
   #:pwhite
   #:pseries
   #:pgeom
   #:ptrace
   #:ppatlace

   #:event
   #:*event-output-function*

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
