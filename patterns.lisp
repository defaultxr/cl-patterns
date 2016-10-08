(in-package #:cl-patterns)

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
                                  (lambda (stream char1 char2)
                                      (apply fn
                                             (read-delimited-list right stream t))))))

(defdelim #\[ #\] (&rest args)
  `(list ,@args))

;; (ql:quickload '(:series))

;; (defun pseq (list &optional (repeats 1))
;;   "Pseq"
;;   (let ((current -1))
;;     (lambda ()
;;       (setf current (+ current 1))
;;       (nth current list))))

;; (defclass pattern ()
;;   (;; (length :initform 1)
;;    (length :accessor pattern-length :initarg :length :initform 1 :type (or null number))
;;    (series :accessor series :initarg :series :initform (series:scan (list)))))

;; (defun pseq (list &optional (repeats 1))
;;   (make-instance 'pattern
;;                  :length (* (length list) repeats)
;;                  :series (series:scan list)))

;; (defgeneric as-series (item))

;; (defmethod as-series ((item pattern))
;;   (series item))

;; (defun next-in-pattern (pattern)
;;   "Gets the next value in a pattern."
;;   (funcall pattern))

;; (setf foo (scan-fn 'number
;;                    (lambda () (random-choice (list 1 2 3)))
;;                    (lambda (x) (+ 2 x))
;;                    (lambda (x) (< 15 x))))

;; test

;;;;

(defun re-intern (symbol package)
  (intern (symbol-name symbol) package))

(defun as-keyword (symbol)
  (re-intern symbol :keyword))

(defun pat-eval (code)
  (let ((res (eval code)))
    (if (eq 'function (type-of res))
        (values (funcall res) res)
        (values res code))))

(defparameter *event* (list))

(defun plist-keys (plist)
  (labels ((accumulator (plist keys)
             (if (null plist)
                 keys
                 (accumulator (cddr plist) (append (list (car plist)) keys)))))
    (accumulator plist (list))))

(defun get-result (pattern-plist)
  (let ((*event* (list))
        (next (list)))
    (loop :for key in (plist-keys pattern-plist)
       :do (multiple-value-bind (res nxt) (pat-eval (getf pattern-plist key))
             (setf (getf *event* key) res)
             (setf (getf next key) nxt)))
    (values *event* next)))

(defun as-stream (pattern)
  (let ((next pattern))
    (lambda ()
      (multiple-value-bind (cur nxt) (get-result next)
        (setf next nxt)
        cur))))

(defun next (stream)
  (funcall stream))

(defun next-n (stream n)
  (loop
     :for i from 0 below n
     :collect (next stream)))

(defparameter *play-all* t)

(defparameter *trace-patterns* t)

(defun plist-remove (place indicator)
  (let ((plist place))
    (remf plist indicator)
    plist))

(defun play-plist (plist)
  (when *trace-patterns*
    (print plist))
  (when (getf plist :instrument)
    (eval (append (list (re-intern (getf plist :instrument) :sc)) (plist-remove plist :instrument)))))

(defun play-pattern (pattern)
  (let ((stream (as-stream pattern)))
    (loop
       :while *play-all*
       :do (let ((result (funcall stream)))
             (print result)
             (sleep (or (getf result :sleep) 1))))))

;; (defmacro pbind (&rest pairs)
;;   `(pbind-accumulator ',pairs (list)))

(defun pbind (&rest pairs)
  (labels ((pbind-accumulator (pairs chash)
             (setf (getf chash (as-keyword (car pairs)))
                   (cadr pairs))
             (if (not (null (cddr pairs)))
                 (pbind-accumulator (cddr pairs) chash)
                 chash)))
    (as-stream (pbind-accumulator pairs (list)))))

(defparameter *patterns* (list))

(defmacro defpattern (name arguments &body body)
  (let ((dash-pat (intern (concat (write-to-string name) "-PAT"))))
    `(progn
       ;; (defun ,dash-pat ,arguments
       ;;   ,@body)
       ;; (defun ,name ,arguments
       ;;   (list ',dash-pat ,@arguments))
       (defun ,name ,arguments
         ,@body)
       (push ',name *patterns*))))

(defpattern pk (key &optional (default 1))
  (lambda ()
    (or (getf *event* key) default)))

;; (defun ) ;; function to limit other patterns

(defpattern pseq (list &optional repeats)
  (let ((list-2 list)
        (length (and repeats (* repeats (length list)))))
    (lambda ()
      (let ((res (car list-2)))
        (when (not (null length)) (decf length))
        (setf list-2 (append (cdr list-2) (list (car list-2))))
        (if (not (null length))
            (when (>= length 0)
              res)
            res)))))

(defpattern prand (list &optional length)
  (let ((length length))
    (lambda ()
      (if length
          (when (> length 0)
            (decf length)
            (random-choice list))
          (random-choice list)))))

(defpattern pxrand (list &optional length)
  (assert (> (length list) 1))
  (let ((length length))
    (lambda ()
      (if length
          (when (> length 0)
            (decf length)
            (random-choice list))
          (random-choice list)))))

(defpattern pfunc (lambda)
  lambda)

;;

#|(in-package :sc)

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

(ctrl (proxy :sinesynth) :lfo-speed 0.1)

(ctrl (proxy :sinesynth) :gate 0)
|#
