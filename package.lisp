(defpackage #:cl-patterns
  (:use #:cl)
  (:export
   #:plist-keys
   #:as-stream
   #:next
   #:next-n
   #:next-upto-n
   #:play

   #:defpattern
   
   #:pbind
   #:pbind-pstream

   ;; NOTE: pattern classes defined with defpattern are automatically exported.

   #:event
   #:*event*
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
   #:dur-time
   #:time-dur
   
   #:freq
   #:midinote
   #:freq-midinote
   #:midinote-freq
   #:steps-per-octave
   #:other-params

   #:gete

   #:*clock*
   #:make-clock
   #:play
   #:stop))
