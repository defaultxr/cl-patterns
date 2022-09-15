;;;; eseq.lisp - "event sequence"; editable, efficient, ordered sequence of `event's.
;; eseq is much simpler than regular patterns and its methods make it easier to modify in some situations.
;; for example, eseq might be well-suited to building a piano roll interface around...
;; (see https://github.com/defaultxr/thundersnow for a piano roll implementation)

;;; TODO:
;; FIX: need some way to keep events sorted when their beat is changed. should they notify the eseq?

(in-package #:cl-patterns)

(defgeneric eseq-events (object)
  (:documentation "The list of events in the `eseq'.

Note that eseq's events slot should not be modified directly as it expects its events to be kept in order. Instead, use `eseq-add', `eseq-remove', or setf `eseq-events'."))

(defclass eseq (pattern #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((events :initarg :events :initform (list) :reader eseq-events :type list :documentation #.(documentation 'eseq-events 'function))
   (dur :initarg :dur :initform nil :type (or null number) :documentation "The duration of the eseq. If the slot is nil, it defaults to `last-dur' rounded up to the next multiple of the eseq's `play-quant'.")))

(defmethod print-object ((eseq eseq) stream)
  (format stream "(~S ~S~@[ :METADATA ~S~])" 'eseq (eseq-events eseq) (hash-table-plist (pattern-metadata eseq))))

(defun eseq (&optional events &key source metadata)
  "Create an eseq containing the specified events and metadata."
  (let ((eseq (make-instance 'eseq
                             :source source
                             :metadata (etypecase metadata
                                         (list (plist-hash-table metadata))
                                         (hash-table metadata)))))
    (dolist (event events eseq)
      (eseq-add eseq event))))

(defun eseq-p (object)
  "True if OBJECT is an eseq.

See also: `eseq'"
  (typep object 'eseq))

(defmethod (setf eseq-events) (value (eseq eseq))
  (with-slots (events) eseq
    (dolist (removed (set-difference events value :test #'event-equal))
      (eseq-remove eseq removed))
    (dolist (added (set-difference value events :test #'event-equal))
      (eseq-add eseq added))))

(defmethod play-quant ((eseq eseq))
  (if (slot-boundp eseq 'play-quant)
      (slot-value eseq 'play-quant)
      (list 1)))

(defmethod (setf play-quant) (value (eseq eseq))
  (setf (slot-value eseq 'play-quant) (ensure-list value)))

(defmethod end-quant ((eseq eseq))
  (if (slot-boundp eseq 'end-quant)
      (slot-value eseq 'end-quant)
      (list 1)))

(defmethod (setf end-quant) (value (eseq eseq))
  (setf (slot-value eseq 'end-quant) (ensure-list value)))

(defmethod loop-p ((eseq eseq))
  (if (slot-boundp eseq 'loop-p)
      (slot-value eseq 'loop-p)
      nil))

(defmethod (setf loop-p) (value (eseq eseq))
  (setf (slot-value eseq 'loop-p) value))

(defmethod pattern-metadata ((eseq eseq) &optional key)
  (if key
      (gethash key (slot-value eseq 'metadata))
      (slot-value eseq 'metadata)))

(defun eseq-length (eseq)
  "Get the number of events in ESEQ."
  (length (eseq-events eseq)))

(defgeneric eseq-add (eseq event)
  (:documentation "Add EVENT to ESEQ. If EVENT doesn't have a `beat', it is placed after the last event in ESEQ.

See also: `eseq-remove'"))

(defmethod eseq-add ((eseq eseq) (event event))
  (with-slots (events) eseq
    (unless (beat event)
      (setf (beat event) (last-dur eseq)))
    (let ((n-beat (beat event)))
      (setf events (insert-if (lambda (ev)
                                (>= (beat ev) n-beat))
                              events event)))))

(defmethod eseq-add ((eseq eseq) (events list))
  (mapc #'eseq-add eseq events))

(defgeneric eseq-remove (eseq event)
  (:documentation "Remove EVENT from ESEQ.

See also: `eseq-add'"))

(defmethod eseq-remove ((eseq eseq) (event event))
  (removef (eseq-events eseq) event))

(defmethod eseq-remove ((eseq eseq) (index integer))
  (with-accessors ((events eseq-events)) eseq
    (setf events (append (subseq events 0 index)
                         (subseq events (1+ index))))))

(defmethod eseq-remove ((eseq eseq) (events list))
  (mapc #'eseq-remove eseq events))

(defmethod bsubseq ((list list) start-beat &optional end-beat)
  (remove-if-not (lambda (ev)
                   (let ((beat (beat ev)))
                     (and (<= start-beat beat)
                          (or (not end-beat)
                              (< beat end-beat)))))
                 list))

(defmethod bsubseq ((eseq eseq) start-beat &optional end-beat)
  (bsubseq (eseq-events eseq) start-beat end-beat))

(defmethod last-dur ((eseq eseq))
  (last-dur (eseq-events eseq)))

(defmethod dur ((eseq eseq))
  (or (slot-value eseq 'dur)
      (next-beat-for-quant (car (play-quant eseq)) (last-dur eseq))))

(defgeneric as-eseq (object)
  (:documentation "Convert OBJECT to an `eseq'.

See also: `as-pstream'"))

(defmethod as-eseq ((symbol symbol))
  (as-eseq (find-pdef symbol t)))

(defmethod as-eseq ((list list))
  (eseq list :source list))

(defmethod as-eseq ((event event))
  (eseq (list event) :source event))

(defmethod as-eseq ((pstream pstream))
  (eseq (next-upto-n pstream) :source pstream))

(defmethod as-eseq ((pattern pattern))
  (eseq (next-upto-n pattern) :source pattern))

(defclass eseq-pstream (pstream eseq)
  ((events-remaining :initarg :events-remaining :initform nil :documentation "The list of events left to be played in the pstream.")
   (direct-p :initarg :direct-p :initform nil :documentation "Whether changes to the source eseq should affect this pstream immediately."))
  (:documentation "pstream for `eseq'."))

(defmethod as-pstream ((eseq eseq))
  (with-slots (events) eseq
    (make-instance 'eseq-pstream
                   :events events
                   :events-remaining (copy-list events)
                   :source eseq)))

(defmethod next.events-ordered ((eseq eseq-pstream))
  (with-slots (number beat events source direct-p) eseq
    (if direct-p
        (progn
          (when (zerop number)
            (warn "direct-p is not yet implemented."))
          (when (< number 4)
            (event :dur 1)))
        (when-let* ((n-events (bsubseq events beat))
                    (next (car n-events)))
          (let* ((after (cadr n-events))
                 (after-beat (if after
                                 (beat after)
                                 (dur eseq))))
            (combine-events next (event :delta (- after-beat (beat next)))))))))

(defmethod next ((eseq eseq-pstream)) ;; if events are not necessarily ordered
  (flet ((first-event (events)
           (most #'< events :key #'beat)))
    (with-slots (number beat events source events-remaining direct-p) eseq
      (if direct-p
          (progn
            (when (zerop number)
              (warn "direct-p is not yet implemented."))
            (when (< number 4)
              (event :dur 1)))
          (let ((next (first-event events-remaining)))
            (unless next
              (return-from next eop))
            (let ((delta (- (beat next) (beat eseq))))
              (if (plusp delta)
                  (event :type :rest :delta delta)
                  (progn
                    (removef events-remaining next :test #'eq)
                    (let ((after (first-event events-remaining)))
                      (combine-events next (event :delta (- (if after
                                                                (beat after)
                                                                (dur eseq))
                                                            (beat next)))))))))))))


