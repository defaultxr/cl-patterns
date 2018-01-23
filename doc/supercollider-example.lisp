(in-package :sc)

;; start the sound server...

(setf *s* (make-external-server "localhost" :port 4444))

(server-boot *s*)

;; define a few synths...

(defsynth kik ((freq 440) (out 0))
  (let* ((env (env-gen.kr (env (list 0 1 0) (list 0.001 1)) :act :free))
         (fenv (env-gen.kr (env (list 1 0) (list 0.25)) :level-scale freq))
         (sig (sin-osc.ar fenv 0 0.2)))
    (out.ar out (pan2.ar sig 0 env))))

(defsynth default ((gate 1) (freq 440) (out 0))
  (let* ((env (env-gen.kr (asr 0.01 1 0.1) :gate gate :act :free))
         (sig (sin-osc.ar freq 0 0.2)))
    (out.ar out (pan2.ar sig 0 env))))

(in-package :cl-patterns)

;; start the clock...

;; the clock keeps tempo in beats per second; thus (/ 110 60) = 110 beats per minute
(defparameter *clock* (make-clock (/ 110 60)))

;; ...and then go ahead and write some patterns!

(pdef :foo (pbind :instrument :kik ;; use the :kik synth
                  :quant 4 ;; make sure the pattern will only start on a beat that is divisible by 4, to stay in sync
                  :dur 1 ;; each event is 1 beat long
                  :pfin 4 ;; limit the length of the pattern to 4 events (the default is infinite events)
                  ))

(pdef :bar (pbind :instrument :default
                  :quant 4
                  :dur 1/2
                  :scale :major ;; select the major scale
                  :degree (pwhite 0 7) ;; pick a random note from the first 7 notes in the selected scale
                  :pfindur 4 ;; limit the length of the pattern to 4 beats (pfindur limits based on beats, not number of events)
                  ))

;; start playing the defined patterns:

(play :foo)

(play :bar)

;; stop playing

(stop :foo)

(stop :bar)
