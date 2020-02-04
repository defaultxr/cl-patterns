(defpackage #:cl-patterns
  (:use #:cl
        #:alexandria)
  (:export

   ;;; utility.lisp

   #:*event*
   #:*clock*
   #:gete
   #:multi-channel-funcall
   #:keys

   #:random-coin
   #:random-range
   #:exponential-random-range
   #:gauss
   #:seq
   #:seq-range
   #:next-beat-for-quant
   #:rerange

   #:tempo
   #:beat
   #:quant
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
   #:tuning-pitches
   #:tuning-octave-ratio

   #:load-scala-scale

   #:define-chord
   #:all-chords
   #:chord
   #:chord-name
   #:chord-scale
   #:chord-indexes
   #:chord-notes
   #:chord-midinotes

   ;;; event.lisp

   #:event
   #:event-p
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
   #:e
   #:remove-event-value

   #:instrument

   #:amp
   #:db

   #:pan

   #:dur
   #:legato
   #:sustain
   #:delta

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
   #:pattern-metadata
   #:pstream
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

   #:pseries*
   #:pgeom*

   #:p+
   #:p-
   #:p*
   #:p/
   #:prerange

   #:pf
   #:ppc

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

   #:task-pattern
   #:pattern-tasks
   #:playing-pdefs

   #:make-clock
   #:clock-tasks
   #:clock-process
   #:clock-loop
   #:start-clock-loop))
