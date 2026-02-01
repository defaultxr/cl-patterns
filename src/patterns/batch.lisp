;;;; batch.lisp - batch-based patterns.
;;; Batch-based patterns differ from serial patterns in that they yield outputs in batches of zero or more outputs rather than one at a time.
;;; Batches are requested using the `pull' function rather than `next'. `pull' differs in that it includes a DUR parameter which notes the dur in beats worth of outputs to yield for.
;;; For example, (pull my-pattern 4) gets a list worth of the next 4 beats worth of outputs from my-pattern.

(in-package #:cl-patterns)

(defgeneric pull (pattern dur)
  (:documentation "Get the next batch of outputs from a pstream, function, or other object, advancing the pstream forward in the process.

See also: `next', `next-n', `next-upto-n', `peek'"))

(defmethod pull ((pattern pattern) dur)
  (pull (as-pstream pattern) dur))

(defclass serial-pattern (pattern)
  ()
  (:documentation "Abstract pattern superclass for patterns designed to yield one output at a time.

See also: `batch-pattern', `pattern'"))

(defclass serial-pstream (pstream)
  ()
  (:documentation "A `pstream' that yields one output at a time, via `next'.

See also: `batch-pstream'"))

(defclass batch-pattern (pattern)
  ()
  (:documentation "Abstract pattern superclass for patterns designed to yield outputs in batches.

See also: `serial-pattern', `pattern'"))

(defclass batch-pstream (pstream)
  ()
  (:documentation "A `pstream' that yields zero or more outputs at a time, via `pull'.

See also: `serial-pstream'"))

(defclass serialized-pstream (serial-pstream)
  ((batch-pstream :initarg :batch-pstream :reader batch-pstream :type pstream :documentation "The source pstream being serialized.")
   (batch-dur :initarg :batch-dur :initform 1/8 :reader batch-dur :type interval :documentation "The dur in beats of each `pull' from the `batch-pstream'.")
   (current-batch :initform nil :accessor current-batch :documentation "The outputs yet to be serialized from the most recent batch.")
   (batch-beat :initform 0 :accessor batch-beat :documentation "The start beat of the current batch of the serialized-pstream."))
  (:documentation "A `batch-pstream' whose outputs are being yielded one at a time via `next'."))

(defgeneric as-serial-pstream (pstream &key)
  (:documentation "Make a `serialized-pstream' from PSTREAM."))

(defmethod as-serial-pstream ((pstream pstream) &key (batch-dur 1/8)) ; FIX: remove
  (make-instance 'serialized-pstream :batch-pstream pstream :batch-dur batch-dur))

(defmethod as-serial-pstream ((pstream serial-pstream) &key)
  pstream)

(defmethod as-serial-pstream ((pstream batch-pstream) &key (batch-dur 1/8))
  (make-instance 'serialized-pstream :batch-pstream pstream :batch-dur batch-dur))

(defmethod as-serial-pstream ((pattern pattern) &rest rest &key (batch-dur 1/8))
  (declare (ignore batch-dur))
  (apply 'as-serial-pstream (as-pstream pattern) rest))

(defun serialized-pstream-next (pstream)
  (declare (optimize (debug 3)))
  (with-slots (number batch-pstream batch-dur current-batch batch-beat) pstream
    (when (zerop number)
      (setf current-batch (pull batch-pstream batch-dur)))
    (when (eop-p current-batch)
      (return-from serialized-pstream-next eop))
    (if current-batch
        (let* ((ev (first current-batch))
               (peek-ev (second current-batch))
               (delta (if peek-ev
                          (- (beat peek-ev) (beat ev))
                          (- (+ batch-beat batch-dur)
                             (beat pstream))))
               (res (combine-events (or ev (event :beat (beat pstream))) (event :delta delta))))
          (setf current-batch (cdr current-batch))
          res)
        (let ((delta (- (+ batch-beat batch-dur)
                        (beat pstream))))
          (setf current-batch (pull batch-pstream batch-dur)
                batch-beat (+ batch-beat batch-dur))
          (if (plusp delta)
              (event :type :rest :beat (beat pstream) :delta delta)
              (serialized-pstream-next pstream))))))

(defmethod next ((pstream serialized-pstream))
  (serialized-pstream-next pstream))

(defclass batched-pstream (pstream)
  ((serial-pstream :initarg :serial-pstream :reader serial-pstream :type pstream :documentation "The source pstream being batched."))
  (:documentation "A `serial-pstream' whose outputs are being yielded in batches via `pull'."))

(defgeneric as-batch-pstream (pstream)
  (:documentation "Make a `batched-pstream' from PSTREAM."))

(defmethod as-batch-pstream ((pstream pstream)) ; FIX: remove
  (make-instance 'batched-pstream :serial-pstream pstream))

(defmethod as-batch-pstream ((pstream batch-pstream))
  pstream)

(defmethod as-batch-pstream ((pstream serial-pstream))
  (make-instance 'batched-pstream :serial-pstream pstream))

(defmethod as-batch-pstream ((pattern pattern))
  (as-batch-pstream (as-pstream pattern)))

(defmethod pull ((pstream batched-pstream) dur)
  (when (eop-p (peek (slot-value pstream 'serial-pstream)))
    (return-from pull eop))
  (loop :with serial-pstream := (slot-value pstream 'serial-pstream)
        :with c-beat := (beat pstream)
        :with end := (+ c-beat dur)
        :for ev := (peek serial-pstream)
        :when (eop-p ev)
          :do (loop-finish)
        :if (< (beat ev) end)
          :collect (next serial-pstream)
        :else
          :do (loop-finish)
        :finally (incf (slot-value pstream 'beat) dur)))
