(in-package #:cl-patterns)

;;;; eseq - event sequence
;; effectively just a list of events played in order
;; eseq is much simpler than patterns and its methods make it easier to modify for some situations
;; for example, eseq might be well-suited to building a piano roll interface around...
;; (see https://github.com/defaultxr/thundersnow for a piano roll implementation)

(defclass eseq (standard-object #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((events :initarg :events :initform (list) :accessor eseq-events :type list :documentation "The actual list of events that the eseq contains.")
   (dur :initarg :dur :type (or null number) :documentation "The duration of the eseq. If the slot is unbound, defaults to `last-dur' rounded up to the next multiple of the eseq's `quant'.")
   (quant :initarg :quant :documentation "A list of numbers representing when the eseq's pstream can start playing. See the quant key of `pattern' for more information.")
   (loop-p :initarg :loop-p :documentation "Whether or not the eseq should loop when played.")
   (pstream-count :initform 0 :reader pstream-count :documentation "The number of pstreams that have been made of this eseq.")
   (metadata :initarg :metadata :initform (make-hash-table) :type hash-table :documentation "Hash table of additional data associated with the eseq, accessible with the `pattern-metadata' function.")))

(defmethod print-object ((eseq eseq) stream)
  (format stream "(~s ~s~#[~; :METADATA ~s~])" 'eseq (eseq-events eseq) (hash-table-plist (pattern-metadata eseq))))

(defun eseq (&optional events &key metadata)
  "Create an eseq containing the specified events and metadata."
  (let ((eseq (make-instance 'eseq :metadata (etypecase metadata
                                               (list (plist-hash-table metadata))
                                               (hash-table metadata)))))
    (dolist (event events eseq)
      (add-event eseq event))))

(defmethod quant ((eseq eseq))
  (if (slot-boundp eseq 'quant)
      (slot-value eseq 'quant)
      (list 1)))

(defmethod (setf quant) (value (eseq eseq))
  (setf (slot-value eseq 'quant) (ensure-list value)))

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

;; (defmethod add-event ((eseq eseq) (event event))
;;   (let ((pos (position-if
;;               (lambda (ev)
;;                 (> (beat ev) (beat event)))
;;               (eseq-events eseq))))
;;     (setf (nthcdr pos ))))

(defmethod add-event ((eseq eseq) (event event)) ;; FIX: insert in order and optimize based on this
  (push event (eseq-events eseq)))

(defmethod remove-event ((eseq eseq) (event event))
  (removef (eseq-events eseq) event))

(defmethod remove-event ((eseq eseq) (index integer))
  (with-accessors ((events eseq-events)) eseq
    (setf events (append (subseq events 0 index)
                         (subseq events (1+ index))))))

(defun remove-events (eseq events)
  (mapc #'remove-event eseq events))

(defun esubseq (eseq start-beat &optional end-beat)
  "Get a list of all events from ESEQ whose `beat' is within the range START-BEAT to END-BEAT, inclusive."
  (remove-if-not (lambda (ev)
                   (apply '<= start-beat (beat ev) (ensure-list end-beat)))
                 (eseq-events eseq)))

(defun last-dur (eseq)
  "Get the beat position of the ending of the last event in the ESEQ."
  (if-let ((events (eseq-events eseq)))
    (reduce #'max events :key (lambda (ev) (+ (beat ev) (event-value ev :dur))))
    0))

(defmethod dur ((eseq eseq))
  (if (slot-boundp eseq 'dur)
      (slot-value eseq 'dur)
      (round-by-direction (last-dur eseq)
                          (car (quant eseq)))))

;; (defclass eseq-pstream (eseq pstream)
;;   (()))

