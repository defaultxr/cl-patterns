;; to use cl-patterns with its SuperCollider backend, you'll first need to load the cl-patterns/supercollider system with quicklisp:

(ql:quickload :cl-patterns/supercollider)

;; ...this will take care of loading cl-collider for you if it's not already loaded.

;; once that is done, start the SuperCollider server if you haven't already:

(cl-patterns:start-backend :supercollider)

;; (Note: if you get an error, make sure that sc:*sc-synth-program* is the same as the output of "which scsynth" from your command line. You may also need to set sc:*sc-plugin-paths* if you get errors about UGens not being installed.)

;; then define a few synths...

(in-package :cl-collider)

(defsynth kik ((freq 440) (out 0))
  (let* ((env (env-gen.kr (env (list 0 1 0) (list 0.001 1)) :act :free))
         (fenv (env-gen.kr (env (list 1 0) (list 0.25)) :level-scale freq))
         (sig (sin-osc.ar fenv 0 0.2)))
    (out.ar out (pan2.ar sig 0 env))))

(defsynth default ((gate 1) (freq 440) (out 0))
  (let* ((env (env-gen.kr (asr 0.01 1 0.1) :gate gate :act :free))
         (sig (sin-osc.ar freq 0 0.2)))
    (out.ar out (pan2.ar sig 0 env))))

;; then, enable cl-patterns's supercollider backend:

(in-package :cl-patterns)

;; start the clock that patterns will be played on:

;; the clock keeps tempo in beats per second; thus 110/60 = 110 beats per minute
(start-clock-loop :tempo 110/60)

;; ...and then go ahead and write some patterns!

(pb :foo ;; define a new pattern named :foo
  :instrument :kik ;; use the :kik synth we defined above
  :quant 4 ;; make sure the pattern will only start on a beat that is divisible by 4, to stay in sync
  :dur 1 ;; give each event a duration of 1 beat
  :pfin 4 ;; limit the length of the pattern to 4 events (the default is infinite events)
  )

(pb :bar
  :instrument :default
  :quant 4
  :dur 1/2
  :scale :major ;; select the major scale
  :degree (pwhite 0 7) ;; pick a random note from the first 7 notes in the selected scale
  :pfindur 4 ;; limit the length of the pattern to 4 beats. pfindur causes the pattern to be limited based on its duration in beats, rather than the number of events.
  )

;; start playing the defined patterns:

(play :foo)

(play :bar)

;; a pattern will loop by default when played. so even though the above patterns have finite durations (as set with the :pfin and :pfindur keys), they will continue to play until you specifically stop them.
;; while they're playing you can modify them by editing the code and re-evaluating it. the changes you make will be reflected the next time the patterns loops.

;; when you're done with them, you can stop playing the patterns like so:

(end :foo)

(end :bar)

;; the #'end function is used to make patterns stop at the end of their current loop, which is usually more musically desirable than stopping in the middle of the phrase.
;; however, if you do want a pattern to stop immediately (or if you need it to because you defined it with an infinite length), you can use #'stop instead.

;; to continue learning cl-patterns, you can use the documentation in the doc/ directory.
;; in particular, tutorial.org is a tutorial meant for people new to cl-patterns.
;; sc-differences.org may be useful if you're used to SuperCollider's patterns system.
;; you can also introspect the library, for example to get a list of all defined pattern types:

(all-patterns)

;; then you can use Lisp's standard #'describe function to get information about any patterns defined.
