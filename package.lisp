(defpackage #:cl-patterns
  (:use #:cl)
  (:export
   #:plist-keys
   #:as-pstream
   #:next
   #:next-n
   #:next-upto-n
   #:pstream-nth

   #:defpattern
   
   #:pbind

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
   #:other-params

   #:get-event-value
   #:set-event-value
   #:gete

   #:*clock*
   #:make-clock
   #:play
   #:stop
   #:play-or-stop))
