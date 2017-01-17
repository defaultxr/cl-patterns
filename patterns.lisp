;;; OO-style patterns
;; FIX: guard against negative durs? maybe just clip them to 0? is that safe? probably not???????

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
  (let ((name-pstream (intern (concatenate 'string (symbol-name name) "-PSTREAM"))))
    `(progn
       (defclass ,name ,superclasses
         ,slots
         ,@(when documentation
             `((:documentation ,documentation))))
       (defclass ,name-pstream (,name pstream)
         ())
       (export ',name)
       (export ',name-pstream))))

(defparameter *max-pattern-yield-length* 64
  "The maximum amount of events or values that will be used by patterns like pshift, etc, in order to prevent hangs caused by infinite-length patterns.")

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

(defun next-upto-n (pattern &optional (n *max-pattern-yield-length*))
  "Return a list of up to N values of PATTERN. If PATTERN ends after less than N values, then only that many values will be returned."
  (let ((pstream (as-pstream pattern))
        (results (list))
        (number 0))
    (block loop
      (loop
         (let ((val (next pstream)))
           (if (or (null val)
                   (= number n))
               (return-from loop)
               (progn
                 (setf results (append results (list val)))
                 (incf number))))))
    results))

;;; pstream

(defclass pstream (pattern)
  ((remaining :initarg :remaining :accessor :remaining :initform nil)
   (number :accessor :number :initform 0)
   (pattern-stack :accessor :pattern-stack :initform (list)))
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

(defun remainingp (pattern &optional (key 'remaining))
  "Return t if PATTERN's KEY value is :inf, nil, or greater than 0."
  (or (or (null (slot-value pattern key))
          (eq (slot-value pattern key) :inf))
      (> (slot-value pattern key) 0)))

(defun decf-remaining (pattern &optional (key 'remaining))
  "Decrease PATTERN's KEY value."
  (when (numberp (slot-value pattern key))
    (decf (slot-value pattern key))))

(defmethod next :around ((pattern pstream))
  (labels ((get-value-from-stack (pattern)
             (if (null (slot-value pattern 'pattern-stack))
                 (prog1
                     (get-value pattern)
                   (decf-remaining pattern))
                 (let* ((popped (pop (slot-value pattern 'pattern-stack)))
                        (nv (next popped)))
                   (if (null nv)
                       (get-value-from-stack pattern)
                       (progn
                         (push popped (slot-value pattern 'pattern-stack))
                         nv)))))
           (get-value (pattern)
             (let ((res (call-next-method)))
               (typecase res
                 (pattern
                  (pattern-embed pattern res)
                  (get-value-from-stack pattern))
                 (t res)))))
    (when (remainingp pattern)
      (get-value-from-stack pattern))))

(defmethod pattern-embed ((pattern pstream) (embed pattern))
  (push (as-pstream embed) (slot-value pattern 'pattern-stack)))

(defmethod next :after ((pattern pstream))
  (incf (slot-value pattern 'number)))

(defmethod next-n ((pattern pstream) (n number))
  (loop
     :for i :from 0 :below n
     :collect (next pattern)))

(defgeneric as-pstream (pattern))

(defmethod as-pstream ((pattern t))
  pattern)

(defmethod as-pstream ((pattern pstream))
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

(define-pbind-special-post-key remaining ;; FIX
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

(defun pseq (list &optional (repeats 1))
  "Create an instance of the PSEQ class."
  (make-instance 'pseq
                 :list list
                 :remaining (when (numberp repeats) (* repeats (length list)))))

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

(defun prand (list &optional (remaining :inf))
  "Create an instance of the PRAND class."
  (make-instance 'prand
                 :list list
                 :remaining remaining))

(defmethod next ((pattern prand-pstream))
  (alexandria:random-elt (slot-value pattern 'list)))

;;; pxrand

(defpattern pxrand (pattern)
  ((list :initarg :list :accessor :list)
   (lr :initarg :lr :initform nil) ;; last result
   )
  "A pxrand returns a random value from LIST, never repeating the same one twice.")

(defun pxrand (list &optional remaining)
  "Create an instance of the PXRAND class."
  (assert (> (length (remove-duplicates list)) 1))
  (make-instance 'pxrand
                 :list list
                 :remaining remaining))

(defmethod next ((pattern pxrand-pstream))
  (let ((res (alexandria:random-elt (slot-value pattern 'list))))
    (loop :while (eql res (slot-value pattern 'lr))
       :do (setf res (alexandria:random-elt (slot-value pattern 'list))))
    (setf (slot-value pattern 'lr) res)
    res))

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
        (let ((next-value (next (slot-value pattern 'repeats))))
          (when next-value
            (setf (slot-value pattern 'crr) (1- next-value))
            (slot-value pattern 'cv))))
      (progn
        (decf-remaining pattern 'crr)
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
  (when (not (null value))
    (pdef-ref key (list :pattern value :pstream (as-pstream value))))
  (make-instance 'pdef
                 :key key
                 :pattern value))

(defmethod next ((pattern pdef-pstream))
  (next (getf (pdef-ref (slot-value pattern 'key)) :pstream)))

;;; plazy

(defpattern plazy (pattern)
  ((func :initarg :func :accessor :func)
   (cp :initarg :cp :initform nil)))

(defun plazy (func)
  (make-instance 'plazy
                 :func func))

(defmethod next ((pattern plazy-pstream))
  (if (null (slot-value pattern 'cp))
      (progn
        (setf (slot-value pattern 'cp) (as-pstream (funcall (slot-value pattern 'func))))
        (next (slot-value pattern 'cp)))
      (next (slot-value pattern 'cp))))

;;; plazyn

(defpattern plazyn (pattern)
  ((func :initarg :func :accessor :func)
   (repeats :initarg :repeats :accessor :repeats)
   (cp :initarg :cp :initform nil)
   (crr :initarg :crr :initform nil)))

(defun plazyn (func &optional (repeats :inf))
  (make-instance 'plazyn
                 :func func
                 :repeats repeats
                 :crr repeats))

(defmethod next ((pattern plazyn-pstream))
  (labels ((maybe-funcall ()
             (when (remainingp pattern 'crr);; (or (eq :inf (slot-value pattern 'crr))
               ;;     (> (slot-value pattern 'crr) 0))
               (setf (slot-value pattern 'cp) (as-pstream (funcall (slot-value pattern 'func))))
               (decf-remaining pattern 'crr);; (when (numberp (slot-value pattern 'crr))
               ;;   (decf (slot-value pattern 'crr)))
               )))
    (when (null (slot-value pattern 'cp))
      (maybe-funcall))
    (let ((nv (next (slot-value pattern 'cp))))
      (if (null nv)
          (progn
            (maybe-funcall)
            (next (slot-value pattern 'cp)))
          nv))))

;;; pcycles
;; inspired by tidalcycles

(defpattern pcycles (pattern)
  ((list :initarg :list :accessor :list)
   (pl :initarg :pl :accessor :pl) ;; parsed list
   ))

(defun pcycles-parse-list (list) ;; FIX: maybe make pcycles parse in the 'next' method instead of at construction time.
  (labels ((recurse (list dur)
             (let ((c (car list)))
               (when (not (null c))
                 (if (consp c)
                     (recurse c (* dur (/ 1 (length c))))
                     (cons (event :freq c :dur dur) (recurse (cdr list) dur)))))))
    (recurse list (/ 1 (length list)))))

(defun pcycles (list)
  (make-instance 'pcycles
                 :list list
                 :pl (pcycles-parse-list list)))

(defmethod next ((pattern pcycles-pstream))
  (nth (mod (slot-value pattern 'number) (length (slot-value pattern 'pl)))
       (slot-value pattern 'pl)))

;;; pshift
;; shift a pattern N forward or backward, wrapping around

;; (defpattern pshift (pattern)
;;   ((list :initarg :list :accessor :list)
;;    (shift :initarg :shift :accessor :shift)))

(defun pshift (pattern shift &optional (max-yield *max-pattern-yield-length*)) ;; FIX: maybe dont use pseq internally
  (pseq (alexandria:rotate (next-upto-n pattern max-yield) shift)))

;; (defmethod next ((pattern pshift-pstream)))

;;; pn

(defpattern pn (pattern)
  ((pattern :initarg :pattern :accessor :pattern)
   (repeats :initarg :repeats :accessor :repeats)
   (pps :initarg :pps :initform nil) ;; pattern-pstream
   ))

(defun pn (pattern &optional (repeats :inf))
  (make-instance 'pn
                 :pattern pattern
                 :repeats repeats))

(defmethod as-pstream ((pattern pn)) ;; need this so that PATTERN won't be automatically converted to a pstream when the pn is.
  (let ((repeats (slot-value pattern 'repeats)))
    (make-instance 'pn-pstream
                   :remaining (slot-value pattern 'remaining)
                   :pattern (slot-value pattern 'pattern)
                   :repeats (if (numberp repeats)
                                (1- repeats)
                                repeats)
                   :pps (as-pstream (slot-value pattern 'pattern)))))

(defmethod next ((pattern pn-pstream))
  (let ((nv (next (slot-value pattern 'pps))))
    (when (and
           (null nv)
           (remainingp pattern 'repeats);; (or (eq :inf (slot-value pattern 'repeats))
           ;;     (> (slot-value pattern 'repeats) 0))
           )
      (setf (slot-value pattern 'pps) (as-pstream (slot-value pattern 'pattern)))
      (setf nv (next (slot-value pattern 'pps)))
      (decf-remaining pattern 'repeats);; (when (numberp (slot-value pattern 'repeats))
      ;; (decf (slot-value pattern 'repeats)))
      )
    nv))

;;; pshuf

(defpattern pshuf (pattern)
  ((list :initarg :list :accessor :list)
   (repeats :initarg :repeats :accessor :repeats)
   (sl :initarg :sl :initform nil) ;; shuffled list
   ))

(defun pshuf (list &optional (repeats 1))
  (make-instance 'pshuf
                 :list list
                 :repeats repeats))

(defmethod as-pstream ((pattern pshuf))
  (make-instance 'pshuf-pstream
                 :remaining (slot-value pattern 'remaining)
                 :list (slot-value pattern 'list)
                 :repeats (slot-value pattern 'repeats)
                 :sl (alexandria:shuffle (alexandria:copy-sequence 'list (slot-value pattern 'list)))))

(defmethod next ((pattern pshuf-pstream))
  (nth (mod (slot-value pattern 'number) (length (slot-value pattern 'sl)))
       (slot-value pattern 'sl)))

;;; pwhite

(defpattern pwhite (pattern)
  ((lo :initarg :lo :accessor :lo)
   (hi :initarg :hi :accessor :hi)))

(defun pwhite (&optional (lo 0) (hi 1) (remaining :inf))
  (make-instance 'pwhite
                 :lo lo
                 :hi hi
                 :remaining remaining))

(defmethod next ((pattern pwhite-pstream))
  (let ((rval (- (slot-value pattern 'hi) (slot-value pattern 'lo))))
    (+ (slot-value pattern 'lo)
       (random (if (integerp rval)
                   (1+ rval)
                   rval)))))

;;; pseries

(defpattern pseries (pattern)
  ((start :initarg :start :accessor :start)
   (step :initarg :step :accessor :step)
   (cv :initarg :cv :initform nil)))

(defun pseries (&optional (start 0) (step 1) (remaining :inf))
  (make-instance 'pseries
                 :start start
                 :step step
                 :remaining remaining))

(defmethod as-pstream ((pattern pseries))
  (let ((cv (slot-value pattern 'start)))
    (when (functionp cv)
      (setf cv (funcall cv)))
    (make-instance 'pseries-pstream
                   :start (slot-value pattern 'start)
                   :step (as-pstream (slot-value pattern 'step))
                   :remaining (slot-value pattern 'remaining)
                   :cv cv)))

(defmethod next ((pattern pseries-pstream))
  (prog1
      (slot-value pattern 'cv)
    (incf (slot-value pattern 'cv) (next (slot-value pattern 'step)))))

;;; pgeom

(defpattern pgeom (pattern)
  ((start :initarg :start :accessor :start)
   (grow :initarg :grow :accessor :grow)
   (cv :initarg :cv :initform nil)))

(defun pgeom (&optional (start 0) (grow 1) (remaining :inf))
  (make-instance 'pgeom
                 :start start
                 :grow grow
                 :remaining remaining))

(defmethod as-pstream ((pattern pgeom))
  (let ((cv (slot-value pattern 'start)))
    (when (functionp cv)
      (setf cv (funcall cv)))
    (make-instance 'pgeom-pstream
                   :start (slot-value pattern 'start)
                   :grow (as-pstream (slot-value pattern 'grow))
                   :remaining (slot-value pattern 'remaining)
                   :cv cv)))

(defmethod next ((pattern pgeom-pstream))
  (prog1
      (slot-value pattern 'cv)
    (setf (slot-value pattern 'cv) (* (slot-value pattern 'cv) (next (slot-value pattern 'grow))))))

;;; ptrace

(defpattern ptrace (pattern)
  ((pattern :initarg :pattern :accessor :pattern)
   (key :initarg :key :accessor :key)
   (stream :initarg :stream :accessor :stream)
   (prefix :initarg :prefix :accessor :prefix)))

(defun ptrace (pattern &optional key (stream t) (prefix ""))
  (make-instance 'ptrace
                 :pattern pattern
                 :key key
                 :stream stream
                 :prefix prefix))

(defmethod next ((pattern ptrace-pstream))
  (let ((n (next (slot-value pattern 'pattern)))
        (key (slot-value pattern 'key))
        (s (slot-value pattern 'stream))
        (prefix (slot-value pattern 'prefix)))
    (let ((result (if (not (null key))
                      (get-event-value n key)
                      n)))
      (format s "~a~a~%" prefix result)
      result)))

