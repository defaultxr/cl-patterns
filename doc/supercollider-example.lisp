(in-package :sc)

;; start the sound server...

(setf *s* (make-external-server "localhost" :port 4444))

(server-boot *s*)

;; define a few synths...

(defsynth* kik ((freq 440))
  (let* ((env (env-gen.kr (env (list 0 1 0) (list 0.001 1)) :act :free))
         (fenv (env-gen.kr (env (list 1 0) (list 0.25)) :level-scale freq))
         (sig (sin-osc.ar fenv 0 0.2)))
    (out.ar out (pan2.ar sig 0 env))))

(defsynth* default ((gate 1) (freq 440))
  (let* ((env (env-gen.kr (asr 0.01 1 0.1) :gate gate :act :free))
         (sig (sin-osc.ar freq 0 0.2)))
    (out.ar out (pan2.ar sig 0 env))))

;; note that we use defsynth* instead of defsynth.
;; this is because cl-collider doesn't yet keep track of synth definition metadata...
;; ...metadata that cl-patterns needs in order to send the correct messages to scsynth.
;; in the future this might not be necessary.

(in-package :cl-patterns)

;; start the clock...

(defparameter *clock* (make-clock (/ 110 60))) ;; the clock keeps tempo in beats per second; (/ 110 60) = 110BPM in beats per second.

;; ...and then go ahead and write some patterns!
