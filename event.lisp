(in-package :cl-patterns)

;; FIX: add 'strum'
;; FIX: make versions of these generic functions that will work with supercollider ugens; put them in cl-collider-extensions.lisp..
;; FIX: need to test weird scales/tunings to make sure they're converting correctly, etc.

;;; event glue

(defclass event ()
  ((other-params :initarg :other-params :initform (list)))
  (:documentation "Class representing a musical event."))

(defun event (&rest params)
  "Create an event, using the PARAMS as its keys/values."
  (assert (= 0 (mod (length params) 2)))
  (let ((ev (make-instance 'event)))
    (labels ((accumulator (pairs)
               (when (not (null (car pairs)))
                 (set-event-value ev (alexandria:ensure-symbol (car pairs) 'cl-patterns) (cadr pairs))
                 (accumulator (cddr pairs)))))
      (accumulator params)
      ev)))

(defparameter r :rest
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
      (if (null val)
          (alexandria:remove-from-plist plist key)
          (progn
            (setf (getf plist key) val)
            plist))
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
         (raw-set-event-value item ',source (,(intern (string-upcase (concatenate 'string sdestination "-" ssource))) value))))))

;;; instrument/group/out

(event-method instrument :default)

(event-method group 0)

(event-method out 0)

;;; amp/pan

(event-method amp 0.5)

(event-translation-method db amp)

(defun amp-db (amp)
  "Convert amplitude to dB."
  (* 20 (log amp 10)))

(defun db-amp (db)
  "Convert dB to amplitude."
  (expt 10 (* db 0.05)))

(event-method pan 0)

;;; dur

(event-method tempo (if (and (boundp '*clock*) (not (null *clock*)))
                        (tempo *clock*)
                        1))

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

(defun dur-time (dur &optional (tempo (if (and (boundp '*clock*) (not (null *clock*)))
                                          (tempo *clock*)
                                          1)))
  "Convert duration in beats to time in seconds according to TEMPO in beats per second."
  (/ dur tempo))

(defun time-dur (time &optional (tempo (if (and (boundp '*clock*) (not (null *clock*)))
                                           (tempo *clock*)
                                           1)))
  "Convert TIME in seconds to duration in beats according to TEMPO in beats per second."
  (* time tempo))

(event-method quant 1)

;;; freq

(defun remove-freq-info (event)
  (remove-event-value event :freq)
  (remove-event-value event :midinote)
  (remove-event-value event :degree))

(defun get-freq-info (event)
  "Return a keyword representing the type of freq info the event currently holds."
  (let ((keys (keys event)))
    (or (and (position :freq keys) :freq)
        (and (position :midinote keys) :midinote)
        (and (position :degree keys) :degree)
        :freq)))

(event-method freq 440)

(defmethod freq ((item event))
  (case (get-freq-info item)
    (:freq (or (raw-get-event-value item :freq) 440))
    (:midinote (midinote-freq (get-event-value item :midinote)))
    (:degree (degree-freq (get-event-value item :degree)
                          (get-event-value item :root)
                          (get-event-value item :octave)
                          (get-event-value item :scale)))))

(defmethod (setf freq) (value (item event)) 
  (when (position :octave (keys item))
    (set-event-value item :octave (truncate (/ (freq-midinote value) 12))))
  (remove-freq-info item)
  (raw-set-event-value item :freq value))

(defun midinote-freq (midinote)
  "Convert a midi note number to a frequency."
  (* 440 (expt 2 (/ (- midinote 69) 12))))

(defun freq-midinote (freq)
  "Convert a frequency to a midi note number."
  (+ 69 (* 12 (log (/ freq 440) 2))))

(event-method midinote 69)

(defmethod midinote ((item event))
  (case (get-freq-info item)
    (:freq (freq-midinote (get-event-value item :freq)))
    (:midinote (raw-get-event-value item :midinote))
    (:degree (degree-midinote (get-event-value item :degree)
                              (get-event-value item :root)
                              (get-event-value item :octave)
                              (get-event-value item :scale)))))

(defmethod (setf midinote) (value (item event))
  (when (position :octave (keys item))
    (set-event-value item :octave (truncate (/ value 12))))
  (remove-freq-info item)
  (raw-set-event-value item :midinote value))

(event-method degree 5)

(defmethod degree ((item event)) ;; FIX: this can return NIL. i.e. (degree (event :midinote 0))
  (let ((root (get-event-value item :root))
        (octave (get-event-value item :octave))
        (scale (get-event-value item :scale)))
    (case (get-freq-info item)
      (:freq (midinote-degree (freq-midinote (get-event-value item :freq))
                              root octave scale))
      (:midinote (midinote-degree (get-event-value item :midinote)
                                  root octave scale))
      (:degree (raw-get-event-value item :degree)))))

(defmethod (setf degree) (value (item event))
  (remove-freq-info item)
  (raw-set-event-value item :degree value))

(event-method root 0)

(defmethod (setf root) (value (item event))
  (raw-set-event-value item :root value))

(event-method octave 5)

(defmethod (setf octave) (value (item event))
  (raw-set-event-value item :octave value))

(defun freq-octave (freq)
  (truncate (/ (freq-midinote freq) 12)))

(event-method scale :major)

(defun midinote-degree (midinote &optional root octave scale)
  (let ((root (or root )) ;; FIX
        (octave (or octave (truncate (/ midinote 12)))))
    (position midinote (mapcar (lambda (n) (+ (* octave 12) root n))
                               (scale-degrees (scale (or scale :major)))))))

(defun note-midinote (note &optional root octave scale)
  (let ((root (or root (if (and (boundp '*event*) (not (null *event*)))
                           (get-event-value *event* 'root)
                           0)))
        (octave (or octave (if (and (boundp '*event*) (not (null *event*)))
                               (get-event-value *event* 'octave)
                               5)))
        (scale (scale (or scale (if (and (boundp '*event*) (not (null *event*)))
                                    (get-event-value *event* 'scale)
                                    :major)))))
    (+ (* (+ (/ (+ note root)
                (length (tuning-tuning (tuning (scale-tuning scale)))))
             octave
             -5)
          (* 12 (log (tuning-octave-ratio (tuning (scale-tuning scale))) 2)))
       60)))

(defun degree-note (degree &optional (scale :major))
  (let ((degrees (scale-degrees (scale scale))))
    (+ (nth-wrap degree degrees)
       (* (length (tuning-tuning (tuning (scale-tuning (scale scale)))))
          (truncate (/ degree (length degrees)))))))

(defun degree-midinote (degree &optional root octave scale)
  (note-midinote (degree-note degree scale) root octave scale))

(defun degree-freq (degree &optional root octave scale)
  (midinote-freq (degree-midinote degree root octave scale)))

(defun scale-midinotes (&optional (scale :major)) ;; FIX: this is for testing; remove it later
  (let ((scale (scale scale)))
    (print scale)
    (gete (next-n
           (pbind :n (pseries 0 1 :inf)
                  :x (lambda () (note-midinote (degree-note (get-event-value *event* :n) scale) 0 5 scale)))
           (length (scale-degrees scale)))
          :x)))

(defun ratio-midi (ratio)
  (* 12 (log ratio 2)))

(defun midi-ratio (midi)
  (expt 2 (/ midi 12)))

(event-method other-params (list))
