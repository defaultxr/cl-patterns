(in-package :cl-patterns)

;; FIX - maybe need to export instrument, group, out, amp, etc...

(defclass event ()
  ((instrument :initarg :instrument :accessor :instrument)
   (group :initarg :group :accessor :group)
   (out :initarg :out :accessor :out)
   (amp :initarg :amp :accessor :amp)
   (pan :initarg :pan :accessor :pan)
   (tempo :initarg :tempo :accessor :tempo)
   (delta :initarg :delta :accessor :delta)
   (sustain :initarg :sustain :accessor :sustain)
   (timing-offset :initarg :timing-offset :accessor :timing-offset)
   ;; (strum )
   (freq :initarg :freq :accessor :freq)
   (steps-per-octave :initarg :steps-per-octave :accessor :steps-per-octave)
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
      (setf (getf (slot-value event 'other-params) (as-keyword slot)) value)))

(defun get-event-val (event slot)
  (let ((slot (re-intern slot)))
    (if (and (fboundp slot)
             (eq 'standard-generic-function (type-of (fdefinition slot))))
        (funcall slot event)
        (getf (slot-value event 'other-params) (as-keyword slot)))))

(defun combine-events (event1 event2)
  "Returns an event that inserts all the items in EVENT2 into EVENT1, overwriting any that exist.")

(defun play-test (item)
  (format t "Playing: ~s~%" item))

(defparameter *event-output-function* 'play-test
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
             (setf plist (plist-set plist (as-keyword i) (get-event-val event i)))))
    plist))

(defmethod print-object ((item event) stream)
  (format stream "(~s~{ ~s ~s~})" 'event (event-plist item)))

(defparameter *event-basic-parameters* nil
  "The list of all the \"basic\" parameters that can be set on an event. Everything else gets shoved into the other-params slot.")

(defmacro event-method (name default &optional documentation)
  "Creates the generic functions and the methods for reading and writing to the NAME slot for an event and reading it from a plist."
  `(progn
     (setf *event-basic-parameters* (append *event-basic-parameters* (list ',name)))
     (defgeneric ,name (item) (:documentation ,documentation))
     (defmethod ,name ((item event))
       (if (slot-boundp item ',name)
           (slot-value item ',name)
           ,default))
     (defmethod ,name ((item cons)) ;; unfortunately it's only possible to get the value from a plist, not set it...
       (getf item ,(as-keyword name)))
     (defgeneric (setf ,name) (value item))
     (defmethod (setf ,name) (value (item event))
       (setf (slot-value item ',name) value))))

(defmacro event-translation-method (destination source)
  (let ((sdestination (symbol-name destination))
        (ssource (symbol-name source)))
    `(progn
       (setf *event-basic-parameters* (append *event-basic-parameters* (list ',destination)))
       (defgeneric ,destination (item))
       (defmethod ,destination ((item event))
         (,(intern (string-upcase (concatenate 'string ssource "-" sdestination))) (,source item)))
       (defmethod (setf ,destination) (value (item event))
         (setf (slot-value item ',source) (,(intern (string-upcase (concatenate 'string sdestination "-" ssource))) value))))))

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

(event-method instrument :default)

(event-method group 0)

(event-method out 0)

(event-method amp 0.5)

(event-method pan 0)

(event-method tempo 1)

(event-method delta 1)

(event-method sustain 1)

(defgeneric legato (item))

(defmethod legato ((item event))
  (* (slot-value item 'delta)
     (slot-value item 'sustain)))

(defgeneric (setf legato) (value item))

(defmethod (setf legato) (value (item event))
  (setf (sustain item) (* (delta item) value)))

(event-method timing-offset 0)

(event-method freq 440)

(event-method steps-per-octave 12)

(defun midinote-freq (midinote)
  "Convert a midi note number to a frequency."
  (* 440 (expt 2 (/ (- midinote 69) 12))))

(defun freq-midinote (freq)
  "Convert a frequency to a midi note number."
  (+ 69 (* 12 (log (/ freq 440) 2))))

(event-translation-method midinote freq)

(event-method other-params (list))

;;

(defun gete (list key)
  "Get a list of the value of KEY for each element in LIST."
  (mapcar (lambda (event)
            (get-event-val event key))
          list))
