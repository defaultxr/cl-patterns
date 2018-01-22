(defpackage #:cl-patterns
  (:nicknames :clp)
  (:use #:cl)
  (:export

   ;;; utility.lisp

   #:gete
   #:keys

   ;;; conversions.lisp

   #:amp-db
   #:db-amp

   #:dur-time
   #:time-dur
   #:delta-dur
   #:dur-delta

   #:midinote-freq
   #:freq-midinote
   #:freq-octave
   #:midinote-octave
   #:midinote-degree
   #:note-midinote
   #:degree-note
   #:degree-midinote
   #:degree-freq
   #:ratio-midi
   #:midi-ratio

   ;;; event.lisp

   #:event
   #:*event*
   #:*event-output-function*
   #:combine-events
   #:event-plist

   #:event-value
   #:get-event-value
   #:set-event-value

   #:instrument
   #:group
   #:out

   #:amp
   #:db

   #:pan

   #:tempo
   #:dur
   #:legato
   #:sustain
   #:delta
   #:timing-offset

   #:freq
   #:midinote

   ;;; patterns.lisp

   #:pattern
   #:as-pstream
   #:next
   #:next-n
   #:next-upto-n
   #:pstream
   #:pstream-nth
   #:pstream-nth-future
   #:parent-pattern
   #:parent-pbind
   #:beats-elapsed

   #:defpattern
   #:*max-pattern-yield-length*

   #:pbind
   #:pb
   #:pmono

   ;; NOTE: pattern classes defined with `defpattern' are automatically exported.

   #:p+
   #:p-
   #:p*
   #:p/

   #:bjorklund

   ;;; cycles.lisp

   ;;; scales.lisp

   #:*note-names*
   #:note-number
   #:note-name
   #:scale-midinotes

   #:define-scale
   #:all-scales
   #:scale
   #:scale-name
   #:scale-notes
   #:scale-tuning

   #:define-tuning
   #:all-tunings
   #:tuning
   #:tuning-name
   #:tuning-tuning
   #:tuning-octave-ratio

   #:load-scala-scale

   ;;; backend.lisp

   #:register-backend
   #:all-backends
   #:enable-backend
   #:disable-backend

   ;;; clock.lisp

   #:*clock*
   #:make-clock
   #:play
   #:stop
   #:play-or-stop
   #:pdefs-playing

   ;;; readtable.lisp

   ))
