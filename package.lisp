(defpackage #:cl-patterns
  (:nicknames :clp)
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

   #:p+
   #:p-
   #:p*
   #:p/

   #:bjorklund

   #:event
   #:*event*
   #:*event-output-function*
   #:combine-events

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
