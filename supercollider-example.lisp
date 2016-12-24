(in-package :sc)

(setf *sc-synth-program* "/usr/bin/scsynth")
(push "/usr/lib/SuperCollider/plugins/" *sc-plugin-paths*)
(push "/usr/share/SuperCollider/Extensions/" *sc-plugin-paths*)
(setf *s* (make-external-server "localhost" :port 4444))

(server-boot *s*)

(defsynth kik ((freq 440))
  (let* ((env (env-gen.kr (env (list 0 1 0) (list 0.001 1)) :act :free))
         (fenv (env-gen.kr (env (list 1 0) (list 0.25)) :level-scale freq))
         (sig (sin-osc.ar [fenv fenv] 0 .2)))
    (out.ar 0 (* env sig))))

(defsynth sine ((gate 1) (freq 440))
  (let* ((env (env-gen.kr (asr 0.01 1 0.1) :gate gate :act :free))
         (sig (sin-osc.ar [freq freq] 0 .2)))
    (out.ar 0 (* env sig))))
