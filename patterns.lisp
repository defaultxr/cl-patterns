;; OO-style patterns

(in-package :cl-patterns)

;;; pattern glue

(defparameter *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defun make-default-event ()
  (combine-events (event) *event*))

(defgeneric play (item)
  (:documentation "Plays an item (typically an event) according to the current *event-output-function*."))

(defparameter *tempo* 1
  "The current tempo in beats per second.")

(defgeneric fork (item))

(defmacro defpattern (name superclasses slots &optional documentation)
  `(progn
     (defclass ,name ,superclasses
       ,slots
       ,@(when documentation
           `((:documentation ,documentation))))
     (defclass ,(intern (concatenate 'string (symbol-name name) "-PSTREAM")) (,name pstream)
       ())))

;;; pattern

(defclass pattern ()
  ((remaining :initarg :remaining :accessor :remaining :initform nil)
   ;; (number :initarg :number :accessor :number :initform 0)
   )
  (:documentation "Abstract pattern superclass."))

(defmethod play ((item pattern))
  (play (as-pstream item)))

(defmethod fork ((item pattern))
  #+bordeaux-threads
  (bt:make-thread (lambda () (play item)) :name (format nil "fork: ~s" item))
  #-bordeaux-threads
  (let ((func (lambda () (play item))))
    (funcall func)
    func))

(defgeneric next (pattern)
  (:documentation "Returns the next value of a pattern stream, function, or other object, advancing the pattern forward in the process."))

(defmethod next ((pattern pattern))
  (next (as-pstream pattern)))

(defmethod next ((pattern function))
  (funcall pattern))

(defmethod next ((pattern t))
  pattern)

(defgeneric next-n (pattern n)
  (:documentation "Returns the next N values of a pattern stream, function, or other object, advancing the pattern forward N times in the process."))

(defmethod next-n ((pattern pattern) (n number))
  (next-n (as-pstream pattern) n))

;;; pstream

(defclass pstream (pattern)
  ((remaining :initarg :remaining :accessor :remaining :initform nil)
   (number :accessor :number :initform 0))
  (:documentation "Pattern stream class."))

(defmethod play ((item pstream))
  (let ((cev (next item)))
    (loop :while (not (null cev))
       :do (progn
             (play cev)
             (sleep (/ (delta cev) *tempo*))
             (setf cev (next item))))))

(defun make-pstream (pattern)
  (make-instance 'pstream
                 :remaining (slot-value pattern 'remaining)))

(defmethod next :around ((pattern pstream))
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

(defmethod next :after ((pattern pstream))
  (incf (slot-value pattern 'number)))

(defmethod next-n ((pattern pstream) (n number))
  (loop
     :for i :from 0 :below n
     :collect (next pattern)))

(defgeneric as-pstream (pattern))

(defmethod as-pstream ((pattern t))
  pattern)

(defmethod as-pstream ((pattern pattern))
  (let ((slots (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern)))))
    (apply #'make-instance
           (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM"))
           (loop :for slot :in slots
              :collect (as-keyword slot)
              :collect (as-pstream (slot-value pattern slot))))))

;;; pbind

(defclass pbind (pattern)
  ((pairs :initarg :pairs :accessor :pairs :initform (list)))
  (:documentation "A pbind associates keys with values for a pattern stream that returns events."))

(defun pbind (&rest pairs)
  "Create an instance of the PBIND class."
  (make-instance 'pbind
                 :pairs pairs))

(defclass pbind-pstream (pbind pstream)
  ())

(defun as-pstream-pairs (pairs)
  (let ((results (list)))
    (loop :for (key val) :on pairs :by #'cddr
       :do (setf results (append results (list key (as-pstream val)))))
    results))

(defmethod as-pstream ((pattern pbind))
  (let ((slots (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern)))))
    (apply #'make-instance
           (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM"))
           (loop :for slot :in slots
              :collect (as-keyword slot)
              :if (equal :pairs (as-keyword slot))
              :collect (as-pstream-pairs (slot-value pattern 'pairs))
              :else
              :collect (slot-value pattern slot)))))

(defparameter *pbind-special-keys* '())

(defmacro define-pbind-special-key (key &body body)
  "Define a special key for pbind that alters the pattern in a nonstandard way. These functions are called for each event created by the pbind and must return a list or event if the key should inject values into the event stream, or NIL if it should not."
  (let ((keyname (as-keyword key)))
    `(setf (getf *pbind-special-keys* ,keyname)
           (lambda (value)
             ,@body))))

(define-pbind-special-key inject
  value)

(defparameter *pbind-special-post-keys* '())

(defmacro define-pbind-special-post-key (key &body body) ;; FIX: need to actually implement this
  "Define a special key for pbind that does post-processing on the pbind when it is created."
  (let ((keyname (as-keyword key)))
    `(setf (getf *pbind-special-post-keys* ,keyname)
           (lambda (value)
             ,@body))))

(define-pbind-special-post-key pdef ;; FIX
  nil)

(defmethod next ((pattern pbind-pstream))
  (labels ((pbind-accumulator (pairs)
             (let ((next-cadr (next (cadr pairs))))
               (if (position (car pairs) (keys *pbind-special-keys*))
                   (let ((result (funcall (getf *pbind-special-keys* (car pairs)) next-cadr)))
                     (setf *event* (combine-events *event* result)))
                   (set-event-value *event* (re-intern (car pairs)) next-cadr))
               (when (not (null next-cadr)) ;; drop out if one of the values is nil - end of pattern!
                 (if (not (null (cddr pairs)))
                     (pbind-accumulator (cddr pairs))
                     *event*)))))
    (let ((*event* (make-default-event)))
      (pbind-accumulator (slot-value pattern 'pairs)))))

;;; pseq

(defpattern pseq (pattern)
  ((list :initarg :list :accessor :list))
  "A pseq yields values from its list in the same order they were provided.")

(defun pseq (list &optional repeats)
  "Create an instance of the PSEQ class."
  (make-instance 'pseq
                 :list list
                 :remaining (when repeats (* repeats (length list)))))

(defmethod next ((pattern pseq-pstream))
  (nth (mod (slot-value pattern 'number) (length (slot-value pattern 'list)))
       (slot-value pattern 'list)))

;;; pk

(defpattern pk (pattern)
  ((key :initarg :key :accessor :key)
   (default :initarg :default :accessor :default :initform 1))
  "A pk returns a value from the current *event* context, returning DEFAULT if that value is nil.")

(defun pk (key &optional (default 1))
  "Create an instance of the PK class."
  (make-instance 'pk
                 :key key
                 :default default))

(defmethod next ((pattern pk-pstream))
  (or (get-event-value *event* (slot-value pattern 'key))
      (slot-value pattern 'default)))

;;; prand

(defpattern prand (pattern)
  ((list :initarg :list :accessor :list))
  "A prand returns a random value from LIST.")

(defun prand (list &optional remaining)
  "Create an instance of the PRAND class."
  (make-instance 'prand
                 :list list
                 :remaining remaining))

(defmethod next ((pattern prand-pstream))
  (alexandria:random-elt (slot-value pattern 'list)))

;;; pfunc

(defpattern pfunc (pattern)
  ((func :initarg :func :accessor :func))
  "A pfunc returns the result of the provided function FUNC.")

(defun pfunc (func)
  "Create an instance of the PFUNC class."
  (make-instance 'pfunc
                 :func func))

(defmethod next ((pattern pfunc-pstream))
  (funcall (slot-value pattern 'func)))

;;; pr

(defpattern pr (pattern)
  ((pattern :initarg :pattern :accessor :pattern)
   (repeats :initarg :repeats :accessor :repeats :initarg :inf)
   (cv :initarg :cv :initform nil) ;; current value
   (crr :initarg :crr :initform nil) ;; current repeats remaining
   )
  "A pr repeats a value from PATTERN REPEATS times before moving on to the next value from PATTERN.")

(defun pr (pattern &optional (repeats :inf))
  (make-instance 'pr
                 :pattern pattern
                 :repeats repeats))

(defmethod next ((pattern pr-pstream))
  (if (or (null (slot-value pattern 'cv))
          (<= (slot-value pattern 'crr) 0))
      (progn
        (setf (slot-value pattern 'cv) (next (slot-value pattern 'pattern)))
        (setf (slot-value pattern 'crr) (1- (next (slot-value pattern 'repeats))))
        (slot-value pattern 'cv))
      (progn
        (decf (slot-value pattern 'crr))
        (slot-value pattern 'cv))))

;;; pdef

(defpattern pdef (pattern)
  ((key :initarg :key :accessor :key)
   (pattern :initarg :pattern :accessor :pattern))
  "A named pattern.")

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

(defmethod next ((pattern pdef-pstream))
  (next (pdef-ref (slot-value pattern 'key))))

;; plazy

(defpattern plazy (pattern)
  ((func :initarg :func :accessor :func)
   (cp :initarg :cp :initform nil)))

(defun plazy (func)
  (make-instance 'plazy
                 :func func))

(defmethod next ((pattern plazy-pstream))
  (if (null (slot-value pattern 'cp))
      (progn
        (setf (slot-value pattern 'cp) (funcall (slot-value pattern 'func)))
        (next (slot-value pattern 'cp)))
      (next (slot-value pattern 'cp))))

;;; plazyn

(defpattern plazyn (pattern)
  ((func :initarg :func :accessor :func)
   (repeats :initarg :repeats :accessor :repeats)
   (cp :initform nil)
   (crr :initarg :crr :initform nil)))

(defun plazyn (func &optional (repeats :inf))
  (make-instance 'plazyn
                 :func func
                 :repeats repeats
                 :crr repeats))

(defmethod next ((pattern plazyn-pstream))
  (labels ((maybe-funcall ()
             (when (or (eq :inf (slot-value pattern 'crr))
                       (> (slot-value pattern 'crr) 0))
               (setf (slot-value pattern 'cp) (funcall (slot-value pattern 'func)))
               (when (numberp (slot-value pattern 'crr))
                 (decf (slot-value pattern 'crr))))))
    (when (null (slot-value pattern 'cp))
      (maybe-funcall))
    (let ((nv (next (slot-value pattern 'cp))))
      (if (null nv)
          (progn
            (maybe-funcall)
            (next (slot-value pattern 'cp)))
          nv))))
