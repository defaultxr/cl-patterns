;;; free

(in-package :sc)

(defgeneric free (item)
  (:documentation "Free a synthesis object (node, bus, buffer, etc)"))

(defmethod free ((item node))
  (with-node (item id server)
    (message-distribute item (list 11 id) server)))

(defmethod free ((item buffer)))

(defmethod free ((item bus)))

(setf (reply-log-p *s*) t) ;; dump all OSC messages

(defun query-tree (&optional (group-id 0) (server *s*))
  (send-message server "/g_queryTree" group-id))

;;; scheduler

(slot-value *scheduler* 'in-queue)

;;; routines

;; check out: cl-cont library

;;; messing around

(in-package :cl-patterns)

(defvar buf (sc:buffer-read "~/snd/gamecube.wav"))

(defun ctime ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(play (pbind :instrument :kik :dur (pseq '(1) 4)))

(play
 (pbind
  :instrument :spcomb
  :bufnum buf
  :decay 0.1
  :freq 200
  :inject (pr (pbind
               :dur (pseq (mapcar #!(+ -0 %) '(1/6 1)) :inf)
               :rate (pseq '(1) 16)
               :rate (pseq (list 1.5 0.5) 4)
               )
              (pseq '(9 3) :inf))
  :amp (pfunc (lambda () (* 0.25 (+ 1 (sin (* 3 (ctime)))))))
  :pan (pseq '(-1 -0.75 -0.5 -0.25 0 0.25 0.5 0.75 1) :inf)
  ;; :pan (pfunc (lambda () (sin (ctime))))
  :start 0.5))

(play
 (pseq
  (list
   (pbind
    :instrument :kik
    :dur (pseq '(1 1/2) :inf)
    :rate (pseq '(0.5 0.75) 4)
    :start 0.5))))

(play (pr
       (pseq (list
              (event :instrument :kik :freq 4000 :dur 1/8)
              (event :instrument :kik :freq 3000 :dur 1/8)
              (event :instrument :kik :freq 2000 :dur 1/8)
              (event :instrument :kik :freq 1000 :dur 1/8)
              ))
       (pseq '(1 2 3))))

(play
 (pbind
  :instrument :kik
  :type (pseq '(:note :rest :note) :inf)
  :freq (pgeom 5000 0.5 6)
  :dur 1/8))

(pdef :xx (pbind
           :pdef :xx
           :instrument :kik
           :type (pseq '(:note :rest :note) :inf)
           :freq (pseq '(1000 500 250 100 50 25) 10)
           :dur 1/16))

(play (pdef :xx))

(defun array-range (start &key (step 1) steps end)
  (format t "~a ~a ~a ~a" start step steps end))

(play (pbind :instrument :kik :freq (pseq (list 1000) 9) :dur 1/64))

;;;

(in-package :cl-patterns)

;; FIX: is it possible to temporarily change the behavior of a character when a macro is being read?
;; i.e. so that newlines can be converted to symbols within a macro
;; so that a tracker-style macro pattern generator can be written
;; i.e. for something like
;; (tracker
;; kick 400
;; snare 300
;; kick 200
;; snare 100)
;; so that the program can tell that 'kick 400' and 'snare 300' should be grouped, and that the newline has different meaning than regular spaces
;; https://gist.github.com/chaitanyagupta/9324402
;; maybe it'd be possible to just change the macro character for \n just in a package, and treat it normally everywhere but inside that macro?
;; sounds difficult though..

(defun semicolon-reader (stream char)
  (declare (ignore char))
  ;; First swallow the rest of the current input line.
  ;; End-of-file is acceptable for terminating the comment.
  (do () ((char= (read-char stream nil #\newline t) #\newline)))
  ;; Return zero values.
  (values))

(set-macro-character #\; #'semicolon-reader)

(defmacro foo (&body bar)
  `(write-to-string ',bar))

;;; midi stuff

(ql:quickload :midi)

(defmethod play ((item midi:note-on-message))
  (output "It's a midi message, yo."))

(defparameter midifile (midi:read-midi-file #P"~/misc/midi/F-Zero_X_-_Title_BGM.mid"))

(play (nth 5 (nth 2 (midi:midifile-tracks midifile))))

;;; testing

(mapc #!(play-plist %)
      (next-n (pbind :instrument :kik :note (prand #[30 40 50 70 80 90])) 3))

(play '(:foo 1 :bar 2))

(slot-value (make-instance 'event :amp 3) 'amp)

;;; sc stuff

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
(server-boot *s*)

(setf foo (play (sin-osc.ar [440 441] 0 .2)))

(server-quit *s*)

(stop)
;; to free a node, use the #'bye function.

(defun b2 (in &optional (pan 0) (level 1)) ;; pseudo-ugen
  (if (eq 'cons (type-of in))
      (balance2.ar (car in) (cadr in) pan level)
      (pan2.ar in pan level)))

(proxy :fltr
       (with-controls ((speed 0.3) (out 0))
         (let ((sig (rlpf.ar (in.ar out 2) (range (sin-osc.kr speed) 20 2000) 0.3)))
           (replace-out.ar out sig))))

(defsynth saw ((gate 1) (freq 440) (sustain 1) (tempo 1) (amp 0.5) (pan 0) (out 0))
  (let* ((env (env-gen.kr (adsr 0.001 0.1 0.5 0.25) :gate gate :act :free))
         (sig (saw.ar [freq freq])))
    (out.ar out (b2 sig pan (* env amp)))))

(defsynth spt ((gate 1) (bufnum 0) (rate 1) (start 0) (amp 0.5) (pan 0) (out 0))
  (let* ((env (env-gen.kr (asr 0.001 1 0.001) :gate gate :act :free))
         (sig (play-buf.ar 2 bufnum (* rate (buf-rate-scale.kr bufnum)) :start-pos (* (buf-frames.ir bufnum) start))))
    (out.ar out (b2 sig pan (* env amp)))))

(defsynth sp ((bufnum 0) (rate 1) (start 0) (amp 0.5) (pan 0) (out 0))
  (let ((sig (play-buf.ar 2 bufnum (* rate (buf-rate-scale.kr bufnum)) :start-pos (* (buf-frames.ir bufnum) start) :act :free)))
    (out.ar out (b2 sig pan amp))))

(defsynth spcomb ((bufnum 0) (rate 1) (freq 400) (decay 1.0) (start 0) (amp 0.5) (pan 0) (out 0))
  (let ((sig (play-buf.ar 2 bufnum (* rate (buf-rate-scale.kr bufnum)) :start-pos (* (buf-frames.ir bufnum) start) :act :free)))
    (out.ar out (b2 (comb-c.ar sig 0.5 (/ 1 freq) decay) pan amp))))

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

;;; mapping

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

;;; buffer - return a sound from an object
;; object can be:
;; URL (use youtube-dl --extract-audio)
;; pathname (read it as a sound file)
;; envelope (convert it to a wavetable)
;; string (text to speech)

(defgeneric buffer (object))

(defmethod buffer ((object pathname)))

(defmethod buffer ((object string)))

;;; incudine

(in-package :incudine.scratch)

(rt-start)

(dump (node 0))

(free 1)

(dsp! env-test (gate amp dur)
  (stereo (* (envelope (make-adsr 0.01 0.1 0.5 0.2) gate 1 #'free)
             (white-noise amp))))

(dsp! apf-test (gate amp)
      (stereo (* (envelope (make-local-adsr 0.01 0.1 0.5 0.2) gate 1 #'free)
                 (fb-comb (white-noise amp) 0.01 (lag (lin-mouse-y 0 0.01) 0.1) (lin-mouse-x 2 0)))))

(~ )
