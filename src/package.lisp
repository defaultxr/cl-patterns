(defpackage #:cl-patterns
  (:use #:cl
        #:alexandria
        #:mutility)
  (:export

   ;;; utility.lisp

   #:*event*
   #:*clock*

   #:multi-channel-funcall
   #:next-beat-for-quant
   #:rerange

   #:tempo
   #:beat
   #:quant
   #:rest-p
   #:play
   #:launch
   #:stop
   #:end
   #:playing-p
   #:loop-p
   #:play-or-stop
   #:play-or-end

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

   #:midi-truncate-clamp
   #:bipolar-1-to-midi
   #:unipolar-1-to-midi
   #:frequency-to-midi

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

   #:defpattern
   #:*max-pattern-yield-length*

   #:pattern
   #:as-pstream
   #:pattern-as-pstream
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

   #:pbind
   #:pb
   #:pmono

   ;; pattern classes defined with `defpattern' are automatically exported.

   #:pf

   #:find-pdef
   #:all-pdefs
   #:pdef-pattern

   #:pseries*
   #:pgeom*

   #:p+
   #:p-
   #:p*
   #:p/

   #:ppc

   ;;; bjorklund.lisp

   #:bjorklund

   ;;; cycles.lisp

   ;;; tracker.lisp

   #:pt

   ;;; eseq.lisp

   #:eseq

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
