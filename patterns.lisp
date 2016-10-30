;; OO-style patterns

;; FIX - should another 'defpattern' be written, or is it better to just use defclass directly?
;; FIX - should make as-stream and use that instead of just treating patterns and streams as the same thing.

(in-package :cl-patterns)

(defparameter *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defun make-default-event ()
  (make-instance 'event))

(defgeneric play (item)
  (:documentation "Plays an item (typically an event) according to the current *event-output-function*."))

;; (defmethod play ((item cons))
;;   (output "It's a cons, yo."))

(defparameter *tempo* 1
  "The current tempo in beats per second.")

(defmethod play ((item pattern))
  (let ((cev (next item)))
    (loop :while (not (null cev))
       :do (progn
             (play cev)
             (sleep (/ (delta cev) *tempo*))
             (setf cev (next item))))))

(defgeneric fork (item))

(defmethod fork ((item pattern))
  (bt:make-thread (lambda () (play item))))

(defclass pattern ()
  ((remaining :initarg :remaining :accessor :remaining :initform nil)
   (number :accessor :number :initform 0))
  (:documentation "Abstract pattern superclass."))

;; (defmacro defpattern (name slots args &body next-func)
;;   `(progn
;;      (defclass ,name (pattern)
;;        ,slots)
;;      (defun ,name ,args
;;        (make-instance ',name))))

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

(defclass pstream (pattern)
  (;; (pattern :initarg :pattern :accessor :pattern)
   (remaining :initarg :remaining :accessor :remaining :initform nil)
   (number :accessor :number :initform 0))
  (:documentation "Pattern stream class."))

(defun make-pstream (pattern)
  (make-instance 'pstream
                 ;; :pattern pattern
                 :remaining (slot-value pattern 'remaining)))

(defmethod next ((pattern pstream))
  (next pattern))

(defgeneric next-n (pattern n)
  (:documentation "Returns the next N values of a pattern stream, function, or other object, advancing the pattern forward N times in the process."))

(defmethod next-n (pattern (n number))
  (loop
     :for i from 0 below n
     :collect (next pattern)))

(defclass pbind (pattern)
  ((pairs :initarg :pairs :accessor :pairs :initform (list)))
  (:documentation "A pbind associates keys with values for a pattern stream that returns events."))

(defun pbind (&rest pairs)
  "Create an instance of the PBIND class."
  (make-instance 'pbind
                 :pairs pairs))

(defmethod next ((pattern pbind)) ;; FIX: this doesn't work: (let ((*event* (event :bar 3))) (next-n (x) 2)) - no :bar in results!!
  (labels ((pbind-accumulator (pairs)
             (let ((next-cadr (next (cadr pairs))))
               (set-event-val *event* (re-intern (car pairs)) next-cadr)
               (when (not (null next-cadr)) ;; drop out if one of the values is nil - end of pattern!
                 (if (not (null (cddr pairs)))
                     (pbind-accumulator (cddr pairs))
                     *event*)))))
    (let ((*event* (make-default-event)))
      (pbind-accumulator (slot-value pattern 'pairs)))))

(defclass pseq (pattern)
  ((list :initarg :list :accessor :list))
  (:documentation "A pseq yields values from its list in the same order they were provided."))

(defun pseq (list &optional repeats)
  "Create an instance of the PSEQ class."
  (make-instance 'pseq
                 :list list
                 :remaining (when repeats (* repeats (length list)))))

(defmethod next ((pattern pseq))
  (nth (mod (slot-value pattern 'number) (length (slot-value pattern 'list)))
       (slot-value pattern 'list)))

(defclass pseq-pstream (pseq pstream)
  ())

(defgeneric as-pstream (pattern))

(defmethod next ((pattern pseq-pstream))
  (nth (mod (slot-value pattern 'number) (length (slot-value pattern 'list)))
       (slot-value pattern 'list)))

(defclass pk (pattern)
  ((key :initarg :key :accessor :key)
   (default :initarg :default :accessor :default :initform 1))
  (:documentation "A pk returns a value from the current *event* context, returning DEFAULT if that value is nil."))

(defun pk (key &optional (default 1))
  "Create an instance of the PK class."
  (make-instance 'pk
                 :key key
                 :default default))

(defmethod next ((pattern pk))
  (or (get-event-val *event* (slot-value pattern 'key))
      (slot-value pattern 'default)))

(defclass prand (pattern)
  ((list :initarg :list :accessor :list))
  (:documentation "A prand returns a random value from LIST."))

(defun prand (list &optional remaining)
  "Create an instance of the PRAND class."
  (make-instance 'prand
                 :list list
                 :remaining remaining))

(defmethod next ((pattern prand))
  (alexandria:random-elt (slot-value pattern 'list)))

(defclass pfunc (pattern)
  ((func :initarg :func :accessor :func))
  (:documentation "A pfunc returns the result of the provided function FUNC."))

(defun pfunc (func)
  "Create an instance of the PFUNC class."
  (make-instance 'pfunc
                 :func func))

(defmethod next ((pattern pfunc))
  (funcall (slot-value pattern 'func)))

(defclass pr (pattern)
  ((pattern :initarg :pattern :accessor :pattern)
   (repeats :initarg :repeats :accessor :repeats :initarg :inf)
   (cv :initform nil) ;; current value
   (crr :initform nil) ;; current repeats remaining
   )
  (:documentation "A pr repeats a value from PATTERN REPEATS times before moving on to the next value from PATTERN."))

(defun pr (pattern &optional (repeats :inf))
  (make-instance 'pr
                 :pattern pattern
                 :repeats repeats))

(defmethod next ((pattern pr))
  (if (or (null (slot-value pattern 'cv))
          (<= (slot-value pattern 'crr) 0))
      (progn
        (setf (slot-value pattern 'cv) (next (slot-value pattern 'pattern)))
        (setf (slot-value pattern 'crr) (1- (next (slot-value pattern 'repeats))))
        (slot-value pattern 'cv))
      (progn
        (decf (slot-value pattern 'crr))
        (slot-value pattern 'cv))))

(defclass pdef (pattern)
  ((key :initarg :key :accessor :key)
   (pattern :initarg :pattern :accessor :pattern))
  (:documentation "A named pattern."))

;; (defparameter *pdef-dictionary* '()
;;   "The global pdef dictionary.")

;; (defun pdef-ref (key &optional value)
;;   "Retrieve a value from the global pdef dictionary, or set it if VALUE is provided."
;;   (let ((key (as-keyword key)))
;;     (if (null value)
;;         (getf *pdef-dictionary* key)
;;         (setf (getf *pdef-dictionary* key) value))))

(defmacro create-global-dictionary (name)
  (let* ((name-name (symbol-name name))
         (dict-symbol (intern (string-upcase (concatenate 'string "*" name-name "-dictionary*")))))
    `(progn
       (defparameter ,dict-symbol '()
         ,(concatenate 'string "The global" name-name "dictionary."))
       (defun ,(intern (string-upcase (concatenate 'string name-name "-ref"))) (key &optional value)
         ,(concatenate 'string "Retrieve a value from the global " name-name " dictionary, or set it if VALUE is provided.")
         (let ((key (as-keyword key)))
           (if (null value)
               (getf ,dict-symbol key)
               (setf (getf ,dict-symbol key) value)))))))

(create-global-dictionary pdef)

(defun pdef (key &optional value)
  (pdef-ref key value)
  (make-instance 'pdef
                 :key key
                 :pattern value))

(defmethod next ((pattern pdef))
  (next (pdef-ref (slot-value pattern 'key))))

(defclass plazy (pattern)
  ((func :initarg :func :accessor :func)
   ;; (repeats :initarg :repeats :accessor :repeats)
   (cp :initform nil)))

(defun plazy (func)
  (make-instance 'plazy
                 :func func))

(defmethod next ((pattern plazy))
  (if (null (slot-value pattern 'cp))
      (progn
        (setf (slot-value pattern 'cp) (funcall (slot-value pattern 'func)))
        (next (slot-value pattern 'cp)))
      (next (slot-value pattern 'cp))))
