;;;; package.lisp - definition for the cl-patterns package.
;;; Note that additional exports are done in other files as well such as export.lisp.

(uiop:define-package #:cl-patterns
  (:mix #:cl
        #:alexandria
        #:trivial-types
        #:mutility)
  (:export

   ;;; utility.lisp

   #:*event*
   #:*clock*
   #:eop

   #:note-name-and-octave

   #:multi-channel-funcall
   #:pyramid
   #:transpose
   #:next-beat-for-quant
   #:rerange

   #:tempo
   #:bpm
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
   #:play-solo
   #:play-swap
   #:all-instruments
   #:playing-nodes

   ;;; conversions.lisp

   #:bipolar-unipolar
   #:unipolar-bipolar

   #:amp-db
   #:db-amp

   #:dur-duration
   #:duration-dur
   #:dur-time
   #:time-dur
   #:dur-freq
   #:freq-dur

   #:rate-freq
   #:freq-rate

   #:*c4-midinote*

   #:midinote-freq
   #:freq-midinote
   #:rate-midinote
   #:midinote-rate
   #:ratio-midi
   #:midi-ratio

   #:octave-freq
   #:freq-octave
   #:octave-midinote
   #:midinote-octave

   #:note-freq
   #:freq-note
   #:note-midinote
   #:midinote-note
   #:note-octave
   #:octave-note

   #:*note-names*
   #:chromatic-index-freq
   #:freq-chromatic-index
   #:chromatic-index-midinote
   #:midinote-chromatic-index
   #:chromatic-index-note
   #:note-chromatic-index

   #:degree-key

   #:degree-freq
   #:freq-degree
   #:degree-midinote
   #:midinote-degree
   #:degree-octave
   #:octave-degree
   #:degree-chromatic-index
   #:chromatic-index-degree
   #:degree-note
   #:note-degree

   #:midi-truncate-clamp
   #:bipolar-1-to-midi
   #:unipolar-1-to-midi
   #:frequency-to-midi
   #:bipolar-1-to-midi-pitchbend

   ;;; scales.lisp

   #:redefining-melodic-alias

   #:tone-matrix

   #:define-tuning
   #:all-tunings
   #:tuning
   #:tuning-name
   #:tuning-pitches
   #:tuning-steps-per-octave
   #:tuning-octave-ratio

   #:load-scala-scale

   #:define-scale
   #:all-scales
   #:scale
   #:scale-name
   #:scale-notes
   #:scale-tuning
   #:scale-steps-per-octave
   #:scale-midinotes

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
   #:combine-events
   #:copy-event
   #:split-event-by-lists
   #:combine-events-via-lists

   #:instrument

   #:amp
   #:db

   #:pan

   #:delta
   #:dur
   #:duration

   #:sustain
   #:legato

   #:freq
   #:midinote
   #:rate

   ;;; patterns.lisp
   ;; NOTE: patterns in `*patterns*' (such as those defined with `defpattern') are automatically exported in export.lisp.

   #:defpattern
   #:*max-pattern-yield-length*
   #:*default-pattern-length*
   #:*default-pattern-repeats*

   #:pattern
   #:pattern-p
   #:all-patterns

   #:pattern-source
   #:pattern-parent
   #:pattern-children
   #:pstream-count
   #:pattern-metadata

   #:pstream-p
   #:as-pstream
   #:pattern-as-pstream

   #:t-pstream
   #:t-pstream-p
   #:t-pstream-value
   #:t-pstream-length

   #:peek
   #:peek-n
   #:peek-upto-n
   #:next
   #:next-n
   #:next-upto-n

   #:pstream
   #:pstream-elt
   #:pstream-elt-future

   #:*post-pattern-output-processors*
   #:instrument-mapping

   #:bsubseq

   #:prest

   ;;; pdef.lisp

   #:pdef-p
   #:all-pdefs
   #:all-pdef-names
   #:playing-pdefs
   #:playing-pdef-names
   #:find-pdef
   #:pdef-name
   #:pdef-pattern
   #:pdef-pstream
   #:pdef-task

   ;;; bjorklund.lisp

   #:bjorklund

   ;;; cycles.lisp

   ;;; track.lisp

   #:ptrack-rows
   #:ptrack-row
   #:ptrack-cell

   #:pt

   ;;; batch.lisp

   #:pull
   #:serial-pattern ; FIX: move
   #:serial-pstream ; FIX: move
   #:batch-pattern
   #:batch-pstream
   #:serialized-pstream ; FIX: move
   #:as-serial-pstream ; FIX: move
   #:batched-pstream
   #:as-batch-pstream

   ;;; backend.lisp

   #:backend
   #:backend-name
   #:backend-enabled-p
   #:backend-started-p
   #:backend-input-processors
   #:backend-metadata

   #:all-backend-types
   #:all-backends
   #:backend-p
   #:find-backend
   #:make-backend
   #:backend-start
   #:backend-stop

   ;; deprecated functions
   #:enabled-backends

   ;;; render.lisp

   #:*cl-patterns-temporary-directory*
   #:render

   ;;; clock.lisp

   #:task-pattern
   #:task-backend-resources
   #:pattern-tasks

   #:make-clock
   #:clock-latency
   #:clock-tasks
   #:clock-process
   #:clock-condition-handler
   #:clock-caught-conditions
   #:clock-caught-conditions-extra-data-function

   #:clock-loop
   #:start-clock-loop)

  ;; we include this here so that all defined patterns are re-exported if we re-evaluate the define-package form
  #.(if (and (uiop:find-symbol* '*patterns* 'cl-patterns nil)
             (symbol-value (uiop:find-symbol* '*patterns* 'cl-patterns nil)))
        `(:export ,@(symbol-value (uiop:find-symbol* '*patterns* 'cl-patterns)))
        (values)))
