(in-package :sc)

(setf *s* (make-external-server "localhost" :port 4444))

(server-boot *s*)

(defsynth* kik ((freq 440))
  (let* ((env (env-gen.kr (env (list 0 1 0) (list 0.001 1)) :act :free))
         (fenv (env-gen.kr (env (list 1 0) (list 0.25)) :level-scale freq))
         (sig (sin-osc.ar fenv 0 0.2)))
    (out.ar out (pan2.ar sig 0 env))))

(defsynth* default ((gate 1) (freq 440))
  (let* ((env (env-gen.kr (asr 0.01 1 0.1) :gate gate :act :free))
         (sig (sin-osc.ar freq 0 0.2)))
    (out.ar out (pan2.ar sig 0 env))))

(in-package :cl-patterns)

(defparameter *clock* (make-clock (/ 110 60)))
