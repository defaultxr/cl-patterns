(defpackage #:cl-patterns
  (:use #:cl)
  (:export

   ;;; utility.lisp

   #:*event*
   #:*clock*
   #:gete
   #:keys

   #:random-coin
   #:random-range
   #:exponential-random-range
   #:seq
   #:seq-range
   #:next-beat-for-quant

   #:tempo
   #:beat
   #:play
   #:launch
   #:stop
   #:end
   #:playing-p
   #:loop-p
   #:play-or-stop
   #:play-or-end

   #:midi-truncate-clamp
   #:bipolar-1-to-midi
   #:unipolar-1-to-midi
   #:frequency-to-midi

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
   #:freq-rate
   #:rate-freq
   #:midinote-rate
   #:rate-midinote

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

   #:define-chord
   #:all-chords
   #:chord
   #:chord-name
   #:chord-scale
   #:chord-indexes
   #:chord-note-numbers
   #:chord-midinotes

   ;;; event.lisp

   #:event
   #:combine-events
   #:copy-event
   #:split-event-by-lists
   #:combine-events-via-lists
   #:event-plist
   #:event-equal
   #:every-event-equal
   #:events-differing-keys
   #:events-lists-differing-keys

   #:event-value
   #:get-event-value
   #:set-event-value
   #:remove-event-value

   #:instrument
   #:group
   #:out

   #:amp
   #:db

   #:pan

   #:dur
   #:legato
   #:sustain
   #:delta
   #:timing-offset

   #:freq
   #:midinote
   #:rate

   ;;; patterns.lisp

   #:pattern
   #:as-pstream
   #:all-patterns
   #:peek
   #:peek-n
   #:peek-upto-n
   #:next
   #:next-n
   #:next-upto-n
   #:pstream
   #:pstream-nth
   #:pstream-nth-future
   #:pstream-elt
   #:pstream-elt-future
   #:parent-pattern
   #:parent-pbind
   #:pstream-count

   #:defpattern
   #:*max-pattern-yield-length*

   #:pbind
   #:pb
   #:pmono

   ;; NOTE: pattern classes defined with `defpattern' are automatically exported.

   #:all-pdefs
   #:pdef-pattern

   #:p+
   #:p-
   #:p*
   #:p/

   #:pf

   ;;; bjorklund.lisp

   #:bjorklund

   ;;; cycles.lisp

   ;;; tracker.lisp

   #:pt

   ;;; backend.lisp

   #:register-backend
   #:all-backends
   #:enabled-backends
   #:enable-backend
   #:disable-backend
   #:start-backend
   #:stop-backend

   ;;; clock.lisp

   #:*performance-mode*
   #:*performance-errors*

   #:make-clock
   #:clock-loop
   #:start-clock-loop
   #:pdefs-playing

   ))
