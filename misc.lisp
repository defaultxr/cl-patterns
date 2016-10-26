;; macro for k---s--- shit

(defmacro ds (&key map &rest items)
  (let ((res (mapcar #'symbol-name items)))
    `(list ,@res)))

(print (ds k - - -
           s - - -
           k - - -
           s - - -))

(pb :foo
    :parent :xx
    :instrument :bar
    :fuck 'baz)

;; midi stuff

(ql:quickload :midi)

(defmethod play ((item midi:note-on-message))
  (output "It's a midi message, yo."))

(defparameter midifile (midi:read-midi-file #P"~/misc/midi/F-Zero_X_-_Title_BGM.mid"))

(play (nth 5 (nth 2 (midi:midifile-tracks midifile))))

;; testing

(mapc #!(play-plist %)
      (next-n (pbind :instrument :kik :note (prand #[30 40 50 70 80 90])) 3))

(play '(:foo 1 :bar 2))

(slot-value (make-instance 'event :amp 3) 'amp)

;; sc stuff

(in-package :sc)

(defparameter *synth* (sine-wave))
(ctrl *synth* :note 72)
(bye *synth*)

(ql:quickload :sc)
(in-package :sc)
(setf *sc-synth-program* "/usr/bin/scsynth")
(push "/usr/lib/SuperCollider/plugins/" *sc-plugin-paths*)
(push "/usr/share/SuperCollider/Extensions/" *sc-plugin-paths*)
(setf *s* (make-external-server "localhost" :port 4444))
(setf *s* (make-external-server "localhost" :port 57110 :just-connect-p t))
(server-boot *s*)

(setf foo (play (sin-osc.ar [440 441] 0 .2)))

(server-quit *s*)

(stop)
;; to free a node, use the #'bye function.

(proxy :fltr
       (with-controls ((speed 0.3) (out 0))
         (let ((sig (rlpf.ar (in.ar out 2) (range (sin-osc.kr speed) 20 2000) 0.3)))
           (replace-out.ar out sig))))

(defsynth saw ((gate 1) (freq 440) (sustain 1) (tempo 1) (amp 0.5) (pan 0) (out 0))
  (let* ((env (env-gen.kr (adsr 0.001 0.1 0.5 0.25) :gate gate :act :free))
         (sig (saw.ar [freq freq])))
    (out.ar out (b2 sig pan (* env amp)))))

(defun b2 (in &optional (pan 0) (level 1))
  (if (eq 'cons (type-of in))
      (balance2.ar (car in) (cadr in) pan level)
      (pan2.ar in pan level)))

(defsynth sine-wave ((note 60))
  (let* ((freq (midicps note))
         (sig (sin-osc.ar [freq (+ freq 2)] 0 .2)))
    (out.ar 0 sig)))

(kik)

(defsynth kik ((freq 440))
  (let* ((env (env-gen.kr (env (list 0 1 0) (list 0.001 1)) :act :free))
         (fenv (env-gen.kr (env (list 1 0) (list 1)) :level-scale freq))
         (sig (sin-osc.ar [fenv fenv] 0 .2)))
    (out.ar 0 (* env sig))))

(defsynth fmp ((gate 1) (freq 440))
  (let* ((env (env-gen.kr (adsr 0.01 0.2 0.5 0.9) :gate gate :act :free))
         (sig (sin-osc.ar [freq (+ freq 1)] 0 .2)))
    (out.ar 0 (* env sig))))

(defparameter *go* nil)

(loop :while *go*
   :do (kik :note (random-choice '(50 60 70 80 90 100)))
   (sleep (random-range 0.1 3.0)))

(defparameter *synth* (kik))

(ctrl *synth* :note (random-choice '(30 40 50 60 72)))

(bye *synth*)

(proxy :sinesynth
       (sin-osc.ar [440 441] 0 .2))

(proxy :sinesynth
   (with-controls ((lfo-speed 4))
     (sin-osc.ar (* [440 441] (range (lf-noise0.ar [lfo-speed (+ lfo-speed .2)]) 0 1)) 0 .2)
      )
   :fade-time 0.0)

(proxy :sinesynth
       ;; (sin-osc.ar (* [440 441] (range (lf-noise0.ar [lfo-speed (+ lfo-speed .2)]) 0 1)) 0 .2)
       (let ((poller (poll.kr (impulse.kr 2) (sin-osc.kr 3) :trigid 1)))
         poller
         (sin-osc.ar 440))
       :fade-time 0.0)

(proxy :sinesynth
       ;; (sin-osc.ar (* [440 441] (range (lf-noise0.ar [lfo-speed (+ lfo-speed .2)]) 0 1)) 0 .2)
       (progn
         ;; (send-reply.ar (impulse.ar 2) "/yo" (white-noise.ar))
         ;; (poll.ar (impulse.ar 2) (white-noise.ar) 'poll 1)
         (sin-osc.ar [440 441] :mul 0.2)
         (dc.ar [0 0])
         )
       :fade-time 0.0)

(ctrl (proxy :sinesynth) :lfo-speed 0.1)

(ctrl (proxy :sinesynth) :gate 0)

(proxy :sine
       (pan2.ar (sin-osc.ar 440 0 (env-gen.kr (perc .0 .4 .2) :gate (impulse.kr (mouse-x)))))
       :fade-time 0)

(stop)

(print (perc 0 .2 .4))

;; mapping

;; (defun linlin)

	;; linlin { arg inMin, inMax, outMin, outMax, clip=\minmax;
	;; 	// linear to linear mapping
	;; 	switch(clip,
	;; 		\minmax, {
	;; 			if (this <= inMin, { ^outMin });
	;; 			if (this >= inMax, { ^outMax });
	;; 		},
	;; 		\min, {
	;; 			if (this <= inMin, { ^outMin });
	;; 		},
	;; 		\max, {
	;; 			if (this >= inMax, { ^outMax });
	;; 		}
	;; 	);
	;; 	^(this-inMin)/(inMax-inMin) * (outMax-outMin) + outMin;
	;; }

	;; linexp { arg inMin, inMax, outMin, outMax, clip=\minmax;
	;; 	// linear to exponential mapping
	;; 	switch(clip,
	;; 		\minmax, {
	;; 			if (this <= inMin, { ^outMin });
	;; 			if (this >= inMax, { ^outMax });
	;; 		},
	;; 		\min, {
	;; 			if (this <= inMin, { ^outMin });
	;; 		},
	;; 		\max, {
	;; 			if (this >= inMax, { ^outMax });
	;; 		}
	;; 	);
	;; 	^pow(outMax/outMin, (this-inMin)/(inMax-inMin)) * outMin
	;; }

	;; explin { arg inMin, inMax, outMin, outMax, clip=\minmax;
	;; 	// exponential to linear mapping
	;; 	switch(clip,
	;; 		\minmax, {
	;; 			if (this <= inMin, { ^outMin });
	;; 			if (this >= inMax, { ^outMax });
	;; 		},
	;; 		\min, {
	;; 			if (this <= inMin, { ^outMin });
	;; 		},
	;; 		\max, {
	;; 			if (this >= inMax, { ^outMax });
	;; 		}
	;; 	);
	;; 	^(log(this/inMin)) / (log(inMax/inMin)) * (outMax-outMin) + outMin;
	;; }

	;; expexp { arg inMin, inMax, outMin, outMax, clip=\minmax;
	;; 	// exponential to exponential mapping
	;; 	switch(clip,
	;; 		\minmax, {
	;; 			if (this <= inMin, { ^outMin });
	;; 			if (this >= inMax, { ^outMax });
	;; 		},
	;; 		\min, {
	;; 			if (this <= inMin, { ^outMin });
	;; 		},
	;; 		\max, {
	;; 			if (this >= inMax, { ^outMax });
	;; 		}
	;; 	);
	;; 	^pow(outMax/outMin, log(this/inMin) / log(inMax/inMin)) * outMin;
	;; }
