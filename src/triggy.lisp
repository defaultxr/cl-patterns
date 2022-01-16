(in-package #:cl-patterns)

;;; "trigger patterns"
;; patterns that wait for input events and process them as they're received
;; similar to how triggers work in modular synthesis
;;
;;* ideas:
;;- trignet - a network of trigger patterns
;;- deftrignet - same but with a name
;;- trigger patterns system has 2 main packages: one where all trigger patterns are defined with the input as their first argument, and one where it isn't and all inputs are derived from the :output slot of each.
;;  - perhaps instead of packages it should just be a different suffix on the name of each trigger pattern, i.e. ~*~ for the input ones?
;;- obviously trigger patterns should be compatible with regular patterns
;;  - ~precv~ to receive triggers, ~psend~ to send?
;;
;;* standard methods/functions for trigger patterns:
;;- tsend - send a trig an input
;;- trecv - receive an input
;;- stop - immediately stop and destroy the trig
;;- end - stop accepting input and destroy self when ready

(defclass tpattern ()
  ((to :initarg :to :initform nil :accessor tpattern-to :type list :documentation "List of objects to send outputs to.")
   (start-beat :initarg :start-beat :initform (next-beat-for-quant 1 (beat *clock*)) :type number :documentation "The beat that this tpattern started on.")
   (ended-p :initform nil :accessor ended-p :documentation "Whether the tpattern should be trying to end now.")
   (stopped-p :initform nil :accessor stopped-p :documentation "Whether the tpattern should stop immediately.")
   (run-thread :initform nil :accessor tpattern-run-thread :type (or null bt:thread) :documentation "The thread running the `t-run' method of the object, if the method is defined for the object."))
  ;; (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Basic tpattern class."))

(defmethod initialize-instance :after ((this tpattern) &key)
  ;; (closer-mop:set-funcallable-instance-function
  ;;  this
  ;;  (fdefinition (slot-value this )))
  (with-slots (stopped-p run-thread) this
    (when-let ((method (find-method #'t-run nil (list (class-of this)) nil)))
      (setf run-thread (bt:make-thread (lambda ()
                                         (loop :until stopped-p
                                               :for out := (t-run this)
                                               :do (sleep (if (and (numberp out)
                                                                   (plusp out))
                                                              out
                                                              1/200))))
                                       :name (format nil "~s thread" this))))))

(defgeneric tpattern-to (object)
  (:documentation "List of objects that OBJECT should send outputs to."))

(defgeneric t-run (object)
  (:documentation "Run OBJECT, processing its inputs and outputs."))

(defmethod stop ((tpattern tpattern))
  (with-slots (run-thread) tpattern
    (bt:destroy-thread run-thread)))

(defun tsend (object &rest input)
  "Send INPUT to OBJECT."
  (apply 'trecv object input))

(defgeneric trecv (object input &key &allow-other-keys)
  (:documentation "Receiver method for OBJECT receiving INPUT."))

(defmethod trecv ((this clock) (input event) &key)
  (play (combine-events input (event :play-quant 0 :latency 0))))

(defmethod trecv ((this clock) (input t) &key)
  (format t "~&Clock ~s received ~s.~%" this input))

(defun t-send-output (sender output)
  (with-slots (to) sender
    (dolist (receiver (or to (list *clock*)))
      (tsend receiver output))))

(defmethod trecv ((this tpattern) input &key)
  (t-send-output this input))

(defclass tdelay (tpattern)
  ((delay :initarg :delay :initform 1 :accessor tdelay-delay :type number :documentation "Default delay time in beats.")
   (buffer :initform nil :accessor tdelay-buffer :type list :documentation "The list of inputs in the delay buffer and the times they're supposed to be output."))
  (:documentation "Delay inputs by a given amount of time."))

(defmethod initialize-instance :after ((this tdelay) &key)
  (bt:make-thread (lambda () (t-run this)) :name (format nil "~s thread" this)))

(defmethod t-run ((this tdelay))
  (with-slots (buffer) this
    (let ((ready (remove-if (fn (> (car _) (beat *clock*))) buffer)))
      (dolist (item ready)
        (removef buffer item :test #'equal) ;; FIX: lock this
        (t-send-output this (cadr item))))))

(defgeneric tdelay-add (tdelay input &optional delay)
  (:documentation "Add INPUT to TDELAY's delay line at DELAY seconds (defaults to the value of tdelay's delay slot)."))

(defmethod tdelay-add ((this tdelay) input &optional (delay (slot-value this 'delay)))
  (push (list (+ (beat *clock*) delay) input) (slot-value this 'buffer)))

(defmethod trecv ((this tdelay) input &key (delay (slot-value this 'delay)))
  (tdelay-add this input delay))

(defclass tmaybe (tpattern)
  ((probability :initarg :probability :initform 1/2 :accessor tmaybe-probability :type number :documentation "Probability of outputting the input."))
  (:documentation "Randomly pass inputs with a given probability."))

(defmethod trecv ((this tmaybe) input &key (probability (slot-value this 'probability)))
  (when (random-coin probability)
    (t-send-output this input)))

(defclass tmetro (tpattern)
  ((freq :initarg :freq :initform 1 :accessor freq :documentation "The frequency in Hz to send the output.")
   (output :initarg :output :initform (event) :accessor tmetro-output :documentation "The output to send."))
  (:documentation "Automatically send a specified output at a regular rate."))

(defmethod initialize-instance :after ((this tmetro) &key)
  (bt:make-thread (lambda () (t-run this)) :name (format nil "~s thread" this)))

(defmethod t-run ((this tmetro))
  (with-slots (freq output) this
    (t-send-output this output)
    (/ 1 freq)))

(defmethod trecv ((this tmetro) input &key)
  (declare (ignore input))
  nil)

