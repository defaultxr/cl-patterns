;; OO-style patterns

;; FIX - should another 'defpattern' be written, or is it better to just use defclass directly?

(in-package :cl-patterns)

(defgeneric play (item)
  (:documentation "Plays an item (typically an event) according to the current *event-output-type*."))

(defmethod play ((item cons))
  (output "It's a cons, yo."))

(defclass pattern ()
  ((stream :initarg :stream :accessor :stream)
   (remaining :initarg :remaining :accessor :remaining :initform nil)
   (number :accessor :number :initform 0))
  (:documentation "Abstract pattern superclass."))

(defgeneric next (pattern)
  (:documentation "Returns the next value of a pattern stream, function, or other object, advancing the pattern forward in the process."))

(defmethod next :around ((pattern pattern))
  (labels ((get-value (pattern)
             (let ((res (call-next-method)))
               (typecase res
                 (pattern (next res))
                 (t res)))))
    (if (null (slot-value pattern 'remaining))
        (get-value pattern)
        (when (> (slot-value pattern 'remaining) 0)
          (decf (slot-value pattern 'remaining))
          (get-value pattern)))))

(defmethod next :after ((pattern pattern))
  (incf (slot-value pattern 'number)))

(defmethod next ((pattern function))
  (funcall pattern))

(defmethod next ((pattern t))
  pattern)

(defgeneric next-n (pattern n)
  (:documentation "Returns the next N values of a pattern stream, function, or other object, advancing the pattern forward N times in the process."))

(defmethod next-n (pattern (n number))
  (loop
     :for i from 0 below n
     :collect (next pattern)))

(defclass pbind (pattern)
  ((pairs :initarg :pairs :accessor :pairs :initform (list)))
  (:documentation "A pbind associates keys with values for a pattern stream that returns events."))

(defun pbind (&rest pairs) ;; FIX - :stream should be moved to a method instead of being shoved inside the pbind data.
  "Conveniently creates an instance of the PBIND class."
  (make-instance 'pbind
                 :pairs pairs
                 :stream
                 (labels ((pbind-accumulator (pairs chash)
                            (setf (getf chash (as-keyword (car pairs)))
                                  (cadr pairs))
                            (if (not (null (cddr pairs)))
                                (pbind-accumulator (cddr pairs) chash)
                                chash)))
                   (let ((next pattern)) (as-stream (pbind-accumulator pairs (list)))
                     (lambda ()
                       (multiple-value-bind (cur nxt) (get-result next)
                         (setf next nxt)
                         cur))))))

(defclass pseq (pattern)
  ((list :initarg :list :accessor :list))
  (:documentation "A pseq simply returns values from a list in the same order they were provided."))

(defun pseq (list &optional repeats)
  "Conveniently creates an instance of the PSEQ class."
  (make-instance 'pseq
                 :list list
                 :remaining (when repeats (* repeats (length list)))))

(defmethod next ((pattern pseq))
  (nth (mod (slot-value pattern 'number) (length (slot-value pattern 'list)))
       (slot-value pattern 'list)))

(defclass pk (pattern)
  ((key :initarg :key :accessor :key)
   (default :initarg :default :accessor :default :initform 1))
  (:documentation "A pk returns a value from the current *event* context, returning DEFAULT if that value is nil."))

(defun pk (key &optional (default 1))
  "Conveniently creates an instance of the PK class."
  (make-instance 'pk
                 :key key
                 :default default
                 :stream ;; FIX - create a `next' method instead of using this.
                 (lambda ()
                   (or (getf *event* key) default))))

(defclass prand (pattern)
  ((list :initarg :list :accessor :list))
  (:documentation "A prand returns a random value from LIST."))

(defun prand (list &optional remaining)
  "Conveniently creates an instance of the PRAND class."
  (make-instance 'prand
                 :list list
                 :remaining remaining))

(defmethod next ((pattern prand))
  (alexandria:random-elt (slot-value pattern 'list)))

(defclass pfunc (pattern)
  ((func :initarg :func :accessor :func))
  (:documentation "A pfunc returns the result of the provided function FUNC."))

(defun pfunc (func)
  "Conveniently creates an instance of the PFUNC class."
  (make-instance 'pfunc
                 :func func))

(defmethod next ((pattern pfunc))
  (funcall (slot-value pattern 'func)))
