(in-package :cl-patterns)

;; FIX - maybe need to export instrument, group, out, amp, etc...

(defclass event ()
  ((instrument :initarg :instrument :accessor :instrument :initform :default)
   (group :initarg :group :accessor :group :initform 0)
   (out :initarg :out :accessor :out :initform 0)
   (amp :initarg :amp :accessor :amp :initform 0.5)
   (pan :initarg :pan :accessor :pan :initform 0)
   (tempo :initarg :tempo :accessor :tempo)
   (delta :initarg :delta :accessor :delta :initform 1)
   (sustain :initarg :sustain :accessor :sustain :initform 1)
   (timing-offset :initarg :timing-offset :accessor :timing-offset :initform 0)
   ;; (strum )
   (freq :initarg :freq :accessor :freq :initform 440)
   (steps-per-octave :initarg :steps-per-octave :accessor :steps-per-octave :initform 12)
   ;; may need more keys to determine the note from the freq????
   (other-params :initarg :other-params :accessor :other-params :initform (make-hash-table)))
  (:documentation "Class representing a musical event."))

(defparameter *event-output-type* :sc
  (:documentation "Says what method to use to `play' an event."))

(defmethod play ((item event))
  (if (eq *event-output-type* :sc)
      (play-sc item)
      (error "Unknown event output type.")))

;; (defmethod print-object ((item event) stream)
;;   (format stream "Event."))

(defmacro event-method (name &optional (slot-name name)) ;; FIX - add argument for documentation, maybe remove slot-name?
  "Creates the generic functions and the methods for reading and writing to the NAME slot for an event."
  `(progn
     (defgeneric ,name (item))
     (defmethod ,name ((item event))
       (slot-value item ',slot-name))
     (defgeneric (setf ,name) (value item))
     (defmethod (setf ,name) (value (item event))
       (setf (slot-value item ',slot-name) value))))

;; (defgeneric db (item))

;; (defmethod db ((item event))
;;   1)

;; (defmethod db ((item number))
;;   number)

;; (defun amp-db (amp)
;;   "Converts amplitude to dB."
;;   )

(defmethod (setf db) (value (item event))
  (setf (slot-value item 'amp) (* 2 value))) ;; FIX - this is just dummy code to test. make actual amp-db conversions.

(defun event (&rest pairs) ;; FIX
  )

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

(event-method other-params)
