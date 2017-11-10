(defpackage #:cl-patterns
  (:nicknames :clp)
  (:use #:cl)
  (:export
   #:pattern
   #:as-pstream
   #:next
   #:next-n
   #:next-upto-n
   #:pstream
   #:pstream-nth
   #:pstream-nth-future
   #:beats-elapsed

   #:defpattern
   #:*max-pattern-yield-length*
   
   #:pbind
   #:pb
   #:pmono

   ;; NOTE: pattern classes defined with defpattern are automatically exported.

   #:p+
   #:p-
   #:p*
   #:p/

   #:bjorklund

   #:event
   #:*event*
   #:keys
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
