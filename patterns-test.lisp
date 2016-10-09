;; testing patterns

(in-package :cl-patterns)

(mapc #!(play-plist %)
      (next-n (pbind :instrument :kik :note (prand #[30 40 50 70 80 90])) 3))

(defgeneric play (item))

(defmethod play ((item cons))
  (output "It's a cons, yo."))

(defmethod play ((item midi:note-on-message))
  (output "It's a midi message, yo."))

(ql:quickload :midi)

(defparameter midifile (midi:read-midi-file #P"~/misc/midi/F-Zero_X_-_Title_BGM.mid"))

(play (nth 5 (nth 2 (midi:midifile-tracks midifile))))

(play '(:foo 1 :bar 2))

(defclass pattern ()
  ((stream :initarg :stream :accessor :stream)
   (remaining :initarg :remaining :accessor :remaining :initform nil)
   (number :accessor :number :initform 0))
  (:documentation "Abstract pattern superclass."))

(defgeneric next (pattern))

;; (defmethod next :around ((pattern pattern)) ;; OLD
;;   (labels ((get-value (pattern)
;;              (incf (slot-value pattern 'number))
;;              (let ((res (funcall (slot-value pattern 'stream))))
;;                (typecase res
;;                  (pattern (next res))
;;                  (t res)))))
;;     (if (null (slot-value pattern 'remaining))
;;         (get-value pattern)
;;         (when (> (slot-value pattern 'remaining) 0)
;;           (decf (slot-value pattern 'remaining))
;;           (get-value pattern)))))

(defmethod next :around ((pattern pattern))
  (labels ((get-value (pattern)
             (let ((res (call-next-method)))
               (typecase res
                 (pattern (next res))
                 (t res)))))
    (if (null (slot-value pattern 'remaining))
        (get-value pattern)
        (when (> (slot-value pattern 'remaining) 0)
          (decf (slot-value pattern 'remaining))
          (get-value pattern)))))

(defmethod next :after ((pattern pattern))
  (incf (slot-value pattern 'number)))

(defmethod next ((pattern function))
  (funcall pattern))

(defmethod next ((pattern t))
  pattern)

(defgeneric next-n (pattern n))

(defmethod next-n (pattern (n number))
  (loop
     :for i from 0 below n
     :collect (next pattern)))

(defclass pbind (pattern)
  ((pairs :initarg :pairs :accessor :pairs :initform (list))))

(defun pbind (&rest pairs) ;; FIX
  (make-instance 'pbind
                 :pairs pairs
                 :stream
                 (labels ((pbind-accumulator (pairs chash)
                            (setf (getf chash (as-keyword (car pairs)))
                                  (cadr pairs))
                            (if (not (null (cddr pairs)))
                                (pbind-accumulator (cddr pairs) chash)
                                chash)))
                   (let ((next pattern)) (as-stream (pbind-accumulator pairs (list)))
                     (lambda ()
                       (multiple-value-bind (cur nxt) (get-result next)
                         (setf next nxt)
                         cur))))))

(defclass ptn ()
  ((list :initarg :list :accessor :list)))

(defclass pseq (pattern)
  ((list :initarg :list :accessor :list)))

;; (defun pseq (list &optional repeats) ;; OLD
;;   (make-instance 'pseq
;;                  :list list
;;                  :remaining (when repeats (* repeats (length list)))
;;                  :stream
;;                  (let ((list-2 list))
;;                    (lambda ()
;;                      (let ((res (car list-2)))
;;                        (setf list-2 (append (cdr list-2) (list (car list-2))))
;;                        res)))))

(defun pseq (list &optional repeats)
  (make-instance 'pseq
                 :list list
                 :remaining (when repeats (* repeats (length list)))))

(defmethod next ((pattern pseq))
  (nth (wrap (slot-value pattern 'number) 0 (1- (length (slot-value pattern 'list))))
       (slot-value pattern 'list)))

(defclass pk (pattern)
  ((key :initarg :key :accessor :key)
   (default :initarg :default :accessor :default :initform 1)))

(defun pk (key &optional (default 1))
  (make-instance 'pk
                 :key key
                 :default default
                 :stream
                 (lambda ()
                   (or (getf *event* key) default))))

;; clos stuff

(defgeneric idk (arg))

(defmethod idk :around ((arg ptn))
  (output "Fuck off i ain't callin the next method.")
  ;; (call-next-method)
  )

(defclass child (ptn)
  ((foo :initarg :foo :accessor :foo)))

(defmethod idk ((arg child))
  (output "Hey from the child.")
  (slot-value arg 'list))

(defparameter this (make-instance 'child :list '(1 2 3)))

(print (idk this))

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

(stop)

(defsynth sine-wave ((note 60))
  (let* ((freq (midicps note))
         (sig (sin-osc.ar [freq (+ freq 2)] 0 .2)))
    (out.ar 0 sig)))

(defsynth kik ((note 60))
  (let* ((freq (midicps note))
         (env (env-gen.kr (env (list 0 1 0) (list 0.001 1)) :act :free))
         (fenv (env-gen.kr (env (list 1 0) (list 1)) :level-scale freq))
         (sig (sin-osc.ar [fenv fenv] 0 .2)))
    (out.ar 0 (* env sig))))

(ql:quickload :utilities)

(use-package :utilities)

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
