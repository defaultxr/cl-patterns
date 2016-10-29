(in-package :cl-patterns)

;; FIX - maybe need to export instrument, group, out, amp, etc...

(defclass event ()
  ((instrument :initarg :instrument :accessor :instrument :initform :default)
   (group :initarg :group :accessor :group)
   (out :initarg :out :accessor :out)
   (amp :initarg :amp :accessor :amp)
   (pan :initarg :pan :accessor :pan)
   (tempo :initarg :tempo :accessor :tempo)
   (delta :initarg :delta :accessor :delta :initform 1)
   (sustain :initarg :sustain :accessor :sustain :initform 1)
   (timing-offset :initarg :timing-offset :accessor :timing-offset)
   ;; (strum )
   (freq :initarg :freq :accessor :freq :initform 440)
   (steps-per-octave :initarg :steps-per-octave :accessor :steps-per-octave :initform 12)
   ;; may need to keep more keys to determine the note from the freq????
   (other-params :initarg :other-params :accessor :other-params :initform (list)))
  (:documentation "Class representing a musical event."))

(defun event (&rest params)
  (labels ((accumulator (pairs)
             (when (not (null (car pairs)))
                 (set-event-val ev (re-intern (car pairs)) (cadr pairs))
                   (accumulator (cddr pairs)))))
    (let ((ev (make-instance 'event)))
      (accumulator params)
      ev)))

(defun set-event-val (event slot value)
  (if (position slot *event-basic-parameters*)
      (funcall (fdefinition (list 'setf slot)) value event)
      (setf (getf (slot-value event 'other-params) slot) value)))

(defun get-event-val (event slot)
  (if (and (fboundp slot)
           (eq 'standard-generic-function (type-of (fdefinition slot))))
      (funcall slot event)
      (getf (slot-value event 'other-params) slot)))

(defparameter *event-output-function* 'play-sc
  "Which function to `play' an event with.")

(defmethod play ((item event))
  (funcall *event-output-function* item))

(defgeneric keys (item))

(defmethod keys ((item event))
  (append
   (remove-if-not (lambda (x) (slot-boundp item (re-intern x)))
                  (list 'instrument 'group 'out 'amp 'pan 'tempo 'delta 'sustain 'timing-offset 'freq 'steps-per-octave))
   (keys (slot-value item 'other-params))))

(defmethod keys ((item cons))
  (labels ((accum (list)
             (cons (car list)
                   (when (cddr list)
                     (accum (cddr list))))))
    (accum item)))

(defmethod keys ((item null))
  nil)

(defun plist-set (plist key val) ;; FIX: use this to add keys to plists so that their order isn't changed
  (if (getf plist key)
      (progn
        (setf (getf plist key) val)
        plist)
      (append plist (list key val))))

(defun event-plist (event)
  (let ((plist '()))
    (loop :for i in (keys event)
       :do (when (not (null i))
             (setf plist (plist-set plist i (get-event-val event i)))))
    plist))

(defmethod print-object ((item event) stream)
  (format stream "(EVENT~{ :~s ~s~})" (event-plist item)))

(defparameter *event-basic-parameters* nil
  "The list of all the \"basic\" parameters that can be set on an event. Everything else gets shoved into the other-params slot.")

(defmacro event-method (name &optional documentation)
  "Creates the generic functions and the methods for reading and writing to the NAME slot for an event and reading it from a plist."
  `(progn
     (setf *event-basic-parameters* (append *event-basic-parameters* (list ',name)))
     (defgeneric ,name (item) (:documentation ,documentation))
     (defmethod ,name ((item event))
       (slot-value item ',name))
     (defmethod ,name ((item cons)) ;; unfortunately it's only possible to get the value from a plist, not set it...
       (getf item ,(as-keyword name)))
     (defgeneric (setf ,name) (value item))
     (defmethod (setf ,name) (value (item event))
       (setf (slot-value item ',name) value))))

(defmacro event-translation-method (name1 name2)
  (let ((sname1 (symbol-name name1))
        (sname2 (symbol-name name2)))
    `(progn
       (setf *event-basic-parameters* (append *event-basic-parameters* (list ',name1)))
       (defgeneric ,name1 (item))
       (defmethod ,name1 ((item event))
         (,(intern (string-upcase (concatenate 'string sname2 "-" sname1))) (,name2 item)))
       (defmethod (setf ,name1) (value (item event))
         (setf (slot-value item ',name2) (,(intern (string-upcase (concatenate 'string sname1 "-" sname2))) value))))))

;; (defgeneric db (item))

;; (defmethod db ((item event))
;;   (amp-db (amp item)))

;; (defmethod db ((item number))
;;   (amp-db item))

(defun amp-db (amp)
  "Convert amplitude to dB."
  (* 20 (log amp 10)))

(defun db-amp (db)
  "Convert dB to amplitude."
  (expt 10 (* db 0.05)))

(event-translation-method db amp)

;; (defmethod (setf db) (value (item event))
;;   (setf (slot-value item 'amp) (db-amp value)))

(event-method instrument)

(event-method group)

(event-method out)

(event-method amp)

(event-method pan)

(event-method tempo)

(event-method delta)

(event-method sustain)

(defgeneric legato (item))

(defmethod legato ((item event))
  (* (slot-value item 'delta)
     (slot-value item 'sustain)))

(defgeneric (setf legato) (value item))

(defmethod (setf legato) (value (item event))
  (setf (sustain item) (* (delta item) value)))

(event-method timing-offset)

(event-method freq)

(event-method steps-per-octave)

(defun midinote-freq (midinote)
  "Convert a midi note number to a frequency."
  (* 440 (expt 2 (/ (- midinote 69) 12))))

(defun freq-midinote (freq)
  "Convert a frequency to a midi note number."
  (+ 69 (* 12 (log (/ freq 440) 2))))

(event-translation-method midinote freq)

(event-method other-params)
