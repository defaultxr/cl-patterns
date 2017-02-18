(in-package :cl-patterns)

;; FIX: may need more keys to determine note based on freq...?
;; FIX: add 'strum'

(defclass event ()
  ((other-params :initarg :other-params :initform (list)))
  (:documentation "Class representing a musical event."))

(defun event (&rest params)
  "Create an event, using the PARAMS as its keys/values."
  (let ((ev (make-instance 'event)))
    (labels ((accumulator (pairs)
               (when (not (null (car pairs)))
                 (set-event-value ev (alexandria:ensure-symbol (car pairs) 'cl-patterns) (cadr pairs))
                 (accumulator (cddr pairs)))))
      (accumulator params)
      ev)))

(defparameter r 'r
  "Rest.")

(defparameter *latency* 0.1
  "Default latency for events.")

(defun raw-set-event-value (event slot value)
  "Set the value of SLOT to VALUE in EVENT without running any conversion functions."
  (with-slots (other-params) event
    (setf other-params (plist-set other-params (alexandria:make-keyword slot) value))))

(defun set-event-value (event slot value)
  "Set the value of SLOT to VALUE in EVENT, running any conversion functions that exist."
  (if (fboundp (list 'setf slot))
      (funcall (fdefinition (list 'setf slot)) value event)
      (raw-set-event-value event slot value)))

(defun remove-event-value (event slot)
  "Removes SLOT from EVENT."
  (with-slots (other-params) event
    (setf other-params (alexandria:remove-from-plist other-params (alexandria:make-keyword slot)))))

(defun raw-get-event-value (event slot)
  "Get the value of SLOT in EVENT without running any conversion functions."
  (getf (slot-value event 'other-params) (alexandria:make-keyword slot)))

(defun get-event-value (event slot)
  "Return the value of SLOT in EVENT, running any necessary conversion functions."
  (let ((slot (alexandria:ensure-symbol slot 'cl-patterns)))
    (if (and (fboundp slot)
             (eq 'standard-generic-function (type-of (fdefinition slot))))
        (funcall slot event)
        (raw-get-event-value event slot))))

(defun combine-events (event1 event2)
  "Returns an event that inserts all the items in EVENT2 into EVENT1, overwriting any that exist."
  (let ((result event1))
    (loop :for key :in (keys event2)
       :do (set-event-value result key (get-event-value event2 key)))
    result))

(defun play-test (item)
  "Simply output information about the event that's being played. Useful for diagnostics when no audio output is available."
  (format t "Playing ~s at ~f.~%" item (/ (get-internal-real-time) internal-time-units-per-second)))

(defparameter *event-output-function* 'play-test
  "Which function to `play' an event with.")

(defmethod play ((item event))
  (funcall *event-output-function* item))

(defgeneric keys (item))

(defmethod keys ((item event))
  (keys (slot-value item 'other-params)))

(defmethod keys ((item cons))
  (labels ((accum (list)
             (cons (car list)
                   (when (cddr list)
                     (accum (cddr list))))))
    (accum item)))

(defmethod keys ((item null))
  nil)

(defun plist-set (plist key val)
  (if (getf plist key)
      (progn
        (setf (getf plist key) val)
        plist)
      (append plist (list key val))))

(defun event-plist (event)
  "Returns a plist of all the keys and values from the event, in order."
  (slot-value event 'other-params))

(defmethod print-object ((item event) stream)
  (format stream "(~s~{ ~s ~s~})" 'event (event-plist item)))

(defmacro event-method (name default &optional documentation)
  "Creates the generic functions and the methods for reading and writing to the NAME slot for an event and reading it from a plist."
  `(progn
     (defgeneric ,name (item) (:documentation ,documentation))
     (defmethod ,name ((item event))
       (let ((res (getf (event-plist item) ,(alexandria:make-keyword name))))
         (if (not (null res))
             res
             ,default)))
     (defmethod ,name ((item cons)) ;; unfortunately it's only possible to get the value from a plist, not set it...
       (getf item ,(alexandria:make-keyword name)))
     (defgeneric (setf ,name) (value item))
     (defmethod (setf ,name) :around (value (item event))
                (if (typep value (or 'string 'symbol)) ;; FIX
                    (raw-set-event-value item :type :rest)
                    (call-next-method)))
     (defmethod (setf ,name) (value (item event))
       (raw-set-event-value item ,(alexandria:make-keyword name) value))))

(defmacro event-translation-method (destination source)
  (let ((sdestination (symbol-name destination))
        (ssource (symbol-name source)))
    `(progn
       (defgeneric ,destination (item))
       (defmethod ,destination ((item event))
         (,(intern (string-upcase (concatenate 'string ssource "-" sdestination))) (,source item)))
       (defmethod (setf ,destination) (value (item event))
         (raw-set-event-value item ',source (,(intern (string-upcase (concatenate 'string sdestination "-" ssource))) value))
         ))))

(defun amp-db (amp)
  "Convert amplitude to dB."
  (* 20 (log amp 10)))

(defun db-amp (db)
  "Convert dB to amplitude."
  (expt 10 (* db 0.05)))

(event-translation-method db amp)

(event-method instrument :default)

(event-method group 0)

(event-method out 0)

(event-method amp 0.5)

(event-method pan 0)

(event-method tempo *tempo*)

(defgeneric delta (item))

(defmethod delta ((item event))
  (or (raw-get-event-value item :delta)
      (dur item)))

(defgeneric (setf delta) (value item))

(defmethod (setf delta) (value (item event))
  (raw-set-event-value item :delta value))

(defgeneric dur (item))

(defmethod dur ((item event))
  (or (raw-get-event-value item :dur)
      1))

(defgeneric (setf dur) (value item))

(defmethod (setf dur) (value (item event))
  (raw-set-event-value item :dur value))

(defgeneric sustain (item))

(defmethod sustain ((item event))
  (or (raw-get-event-value item :sustain)
      (* (legato item)
         (dur item))))

(defgeneric (setf sustain) (value item))

(defmethod (setf sustain) (value (item event))
  (remove-event-value item :legato)
  (raw-set-event-value item :sustain value))

(event-method timing-offset 0)

(defgeneric legato (item))

(defmethod legato ((item event))
  (or (raw-get-event-value item :legato)
      (when (and (raw-get-event-value item :sustain) (raw-get-event-value item :dur))
        (* (sustain item)
           (dur item)))
      0.8))

(defgeneric (setf legato) (value item))

(defmethod (setf legato) (value (item event))
  (remove-event-value item :sustain)
  (raw-set-event-value item :legato value))

(defun delta-dur (delta)
  delta)

(defun dur-delta (dur)
  dur)

(defun dur-time (dur &optional (tempo *tempo*))
  "Convert duration in beats to time in seconds according to TEMPO in beats per second."
  (/ dur tempo))

(defun time-dur (time &optional (tempo *tempo*))
  "Convert TIME in seconds to duration in beats according to TEMPO in beats per second."
  (* time tempo))

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
