(uiop:define-package #:cl-patterns
  (:use #:cl
        #:alexandria
        #:mutility)
  (:export

   ;;; utility.lisp

   #:*cl-patterns-temporary-directory*

   #:*event*
   #:*clock*
   #:eop

   #:multi-channel-funcall
   #:pyramid
   #:next-beat-for-quant
   #:rerange

   #:tempo
   #:beat
   #:quant
   #:play-quant
   #:end-quant
   #:rest-p
   #:play
   #:launch
   #:stop
   #:end
   #:eop-p
   #:playing-p
   #:loop-p
   #:ended-p
   #:play-or-stop
   #:play-or-end
   #:all-instruments
   #:playing-nodes

   #:render

   ;;; conversions.lisp

   #:amp-db
   #:db-amp

   #:dur-time
   #:time-dur
   #:dur-freq
   #:freq-dur

   #:midinote-freq
   #:freq-midinote
   #:freq-octave
   #:midinote-octave
   #:midinote-degree
   #:freq-degree
   #:note-midinote
   #:midinote-note
   #:note-freq
   #:freq-note
   #:degree-note
   #:note-degree
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
   #:event-plist
   #:event-value
   #:remove-event-value
   #:e

   #:event-equal
   #:every-event-equal
   #:events-differing-keys
   #:events-lists-differing-keys
   #:combine-events
   #:copy-event
   #:split-event-by-lists
   #:combine-events-via-lists

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
   ;; NOTE: patterns in `*patterns*' (such as those defined with `defpattern') are automatically exported in export.lisp.

   #:defpattern
   #:*max-pattern-yield-length*

   #:pattern
   #:pattern-p
   #:pstream-p
   #:as-pstream
   #:pattern-as-pstream
   #:t-pstream
   #:all-patterns
   #:pattern-parent
   #:parent-pattern
   #:parent-pbind
   #:pattern-children
   #:pattern-metadata

   #:peek
   #:peek-n
   #:peek-upto-n
   #:next
   #:next-n
   #:next-upto-n

   #:pstream
   #:pstream-elt
   #:pstream-elt-future
   #:pstream-count

   #:*post-pattern-output-processors*
   #:instrument-mapping

   #:bsubseq

   #:prest

   ;;; pdef.lisp

   #:pdef-p
   #:all-pdefs
   #:all-pdef-names
   #:playing-pdefs
   #:find-pdef
   #:pdef-name
   #:pdef-key
   #:pdef-pattern
   #:pdef-pstream
   #:pdef-task

   ;;; bjorklund.lisp

   #:bjorklund

   ;;; cycles.lisp

   ;;; track.lisp

   #:ptrack-row
   #:ptrack-cell

   #:pt

   ;;; eseq.lisp

   #:eseq
   #:eseq-p
   #:eseq-events
   #:eseq-length
   #:eseq-add
   #:eseq-remove
   #:as-eseq
   #:eseq-pstream

   ;;; backend.lisp

   #:register-backend
   #:all-backends
   #:enabled-backends
   #:enable-backend
   #:disable-backend
   #:start-backend
   #:stop-backend

   ;;; clock.lisp

   #:task-pattern
   #:pattern-tasks

   #:make-clock
   #:clock-latency
   #:clock-tasks
   #:clock-process
   #:clock-condition-handler
   #:clock-caught-conditions

   #:*performance-mode*
   #:*performance-errors*

   #:clock-loop
   #:start-clock-loop))
