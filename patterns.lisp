(in-package :cl-patterns)

;;; pattern glue

(defparameter *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defun make-default-event ()
  (combine-events (event) *event*))

(defgeneric play (item)
  (:documentation "Play an item (typically an event or pattern) according to the current *event-output-function*."))

(defgeneric stop (item)
  (:documentation "Stop an item (typically a playing pattern) according to the current *event-output-function*."))

(defparameter *tempo* 1
  "The current tempo in beats per second.")

(defmacro defpattern (name superclasses slots &optional documentation)
  (let ((name-pstream (intern (concatenate 'string (symbol-name name) "-PSTREAM") 'cl-patterns))
        (super-pstream (if (eq 'pattern (car superclasses))
                           'pstream
                           (intern (concatenate 'string (symbol-name (car superclasses)) "-PSTREAM") 'cl-patterns))))
    `(progn
       (defclass ,name ,superclasses
         ,slots
         ,@(when documentation
             `((:documentation ,documentation))))
       (defclass ,name-pstream (,name ,super-pstream)
         ())
       (export ',name)
       (export ',name-pstream))))

(defparameter *max-pattern-yield-length* 64
  "The maximum amount of events or values that will be used by patterns like pshift, etc, in order to prevent hangs caused by infinite-length patterns.")

;;; pattern

(defclass pattern ()
  ((remaining :initarg :remaining :initform nil)
   (quant :initarg :quant :initform 1))
  (:documentation "Abstract pattern superclass."))

(defgeneric next (pattern)
  (:documentation "Returns the next value of a pattern stream, function, or other object, advancing the pattern forward in the process.")
  (:method-combination pattern))

(defmethod next ((pattern pattern))
  (next (as-pstream pattern)))

(defmethod next ((pattern function))
  (funcall pattern))

(defmethod next ((pattern t))
  pattern)

(defun next-n (pattern n)
  "Returns the next N values of a pattern stream, function, or other object, advancing the pattern forward N times in the process."
  (let ((pstream (as-pstream pattern)))
    (loop
       :for i :from 0 :below n
       :collect (next pstream))))

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
  ((remaining :initarg :remaining :initform nil)
   (number :initform 0)
   (pattern-stack :initform (list))
   (next-time :initarg :next-time :initform 0))
  (:documentation "Pattern stream class."))

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
  (with-slots (pattern-stack) pattern
    (labels ((get-value-from-stack (pattern)
               (if (null pattern-stack)
                   (prog1
                       (get-value pattern)
                     (decf-remaining pattern))
                   (let* ((popped (pop pattern-stack))
                          (nv (next popped)))
                     (if (null nv)
                         (get-value-from-stack pattern)
                         (progn
                           (push popped pattern-stack)
                           nv)))))
             (get-value (pattern)
               (let ((res (call-next-method)))
                 (typecase res
                   (pattern
                    (pattern-embed pattern res)
                    (get-value-from-stack pattern))
                   (t res)))))
      (when (remainingp pattern)
        (get-value-from-stack pattern)))))

(defmethod pattern-embed ((pattern pstream) (embed pattern))
  (push (as-pstream embed) (slot-value pattern 'pattern-stack)))

(defmethod next :after ((pattern pstream)) ;; FIX: maybe merge this into the :around method above?
  (incf (slot-value pattern 'number)))

(defgeneric as-pstream (pattern))

(defmethod as-pstream ((pattern t))
  pattern)

(defmethod as-pstream ((pattern pstream))
  pattern)

(defmethod as-pstream ((pattern pattern))
  (let ((slots (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern)))))
    (apply #'make-instance
           (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM") 'cl-patterns)
           (loop :for slot :in slots
              :collect (alexandria:make-keyword slot)
              :collect (as-pstream (slot-value pattern slot))))))

;;; pbind

(defclass pbind (pattern)
  ((pairs :initarg :pairs :initform (list)))
  (:documentation "A pbind associates keys with values for a pattern stream that returns events."))

(defun pbind (&rest pairs)
  "Create an instance of the PBIND class."
  (let ((res-pairs nil)
        (pattern (make-instance 'pbind)))
    (loop :for (key value) :on pairs :by #'cddr
       :do
       (if (position key *pbind-special-init-keys*)
           (let ((result (funcall (getf *pbind-special-init-keys* key) value pattern)))
             (when result
               (setf res-pairs (append res-pairs result))))
           (setf res-pairs (append res-pairs (list key value)))))
    (setf (slot-value pattern 'pairs) res-pairs)
    pattern))

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
           (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM") 'cl-patterns)
           (loop :for slot :in slots
              :collect (alexandria:make-keyword slot)
              :if (equal :pairs (alexandria:make-keyword slot))
              :collect (as-pstream-pairs (slot-value pattern 'pairs))
              :else
              :collect (slot-value pattern slot)))))

(defparameter *pbind-special-init-keys* '())

(defmacro define-pbind-special-init-key (key &body body)
  "Define a special key for pbind that alters the pbind as part of its initialization (\"pre-processing\"). These functions are called at pbind creation time and must return a list if the key should inject values into the pbind pairs, or NIL if they should not."
  (let ((keyname (alexandria:make-keyword key)))
    `(setf (getf *pbind-special-init-keys* ,keyname)
           (lambda (value pattern)
             (declare (ignorable value pattern))
             ,@body))))

(define-pbind-special-init-key quant
  (setf (slot-value pattern 'quant)
        (if (functionp value)
            (funcall value)
            value))
  (list :quant value))

(define-pbind-special-init-key remaining
  (setf (slot-value pattern 'remaining)
        (if (functionp value)
            (funcall value)
            value))
  nil ;; FIX: should this inject the 'remaining value? probably not - the pbind processing should do that automatically so it's always correct.
  )

(defparameter *pbind-special-post-keys* '())

(defmacro define-pbind-special-post-key (key &body body) ;; FIX: need to actually implement this
  "Define a special key for pbind that does post-processing on the pbind when it is created."
  (let ((keyname (alexandria:make-keyword key)))
    `(setf (getf *pbind-special-post-keys* ,keyname)
           (lambda (value)
             (declare (ignorable value))
             ,@body))))

(define-pbind-special-post-key pdef ;; FIX
  nil)

(defparameter *pbind-special-keys* '())

(defmacro define-pbind-special-key (key &body body)
  "Define a special key for pbind that alters the pattern in a nonstandard way. These functions are called for each event created by the pbind and must return a list or event if the key should inject values into the event stream, or NIL if it should not."
  (let ((keyname (alexandria:make-keyword key)))
    `(setf (getf *pbind-special-keys* ,keyname)
           (lambda (value)
             ,@body))))

(define-pbind-special-key inject
  value)

(defmethod next ((pattern pbind-pstream))
  (labels ((pbind-accumulator (pairs)
             (let ((next-cadr (next (cadr pairs))))
               (if (position (car pairs) (keys *pbind-special-keys*))
                   (let ((result (funcall (getf *pbind-special-keys* (car pairs)) next-cadr)))
                     (setf *event* (combine-events *event* result)))
                   (set-event-value *event* (alexandria:ensure-symbol (car pairs) 'cl-patterns) next-cadr))
               (when (not (null next-cadr)) ;; drop out if one of the values is nil - end of pattern!
                 (if (not (null (cddr pairs)))
                     (pbind-accumulator (cddr pairs))
                     *event*)))))
    (let ((*event* (make-default-event)))
      (pbind-accumulator (slot-value pattern 'pairs)))))

(defmethod as-pstream ((item pbind-pstream))
  item)

(defmethod play ((item pattern))
  (let* ((pstream (as-pstream item))
         (cev (next pstream)))
    (loop :while (not (null cev))
       :do (progn
             (play cev)
             (sleep (dur-time (delta cev) *tempo*))
             (setf cev (next pstream))))))

;;; listpattern

(defpattern listpattern (pattern)
  ((list :initarg :list)
   (repeats :initarg :repeats)
   (crr :initarg :crr :initform nil) ;; current repeats remaining
   ))

;; (defclass listpattern-pstream (listpattern pstream)
;;   (crr :initarg :crr :initform nil))

(defmethod as-pstream ((pattern listpattern))
  (let ((repeats (slot-value pattern 'repeats)))
    (make-instance (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM") 'cl-patterns)
                   :list (slot-value pattern 'list)
                   :repeats repeats
                   :crr repeats)))

(defmethod as-pstream ((pattern listpattern-pstream))
  pattern)

(defmethod next :around ((pattern listpattern-pstream))
  (with-slots (number list) pattern
    (let ((mod (mod number (length list))))
      (when (and (> number 0)
                 (= 0 mod))
        (decf-remaining pattern 'crr))
      (when (remainingp pattern 'crr)
        (let ((res (call-next-method)))
          res)))))

;;; pseq

(defpattern pseq (listpattern)
  ()
  "A pseq yields values from its list in the same order they were provided, repeating the list REPEATS times.")

(defun pseq (list &optional (repeats 1))
  "Create an instance of the PSEQ class."
  (make-instance 'pseq
                 :list list
                 :repeats repeats))

(defmethod next ((pattern pseq-pstream))
  (with-slots (number list) pattern
    (nth (mod number (length list))
         list)))

;; ;;; pser

(defpattern pser (pattern)
  ((list :initarg :list))
  "A pser yields values from its list in the same order they were provided, returning a total of REPEATS values.")

(defun pser (list &optional (repeats 1))
  "Create an instance of the PSER class."
  (make-instance 'pser
                 :list list
                 :remaining repeats))

(defmethod next ((pattern pser-pstream))
  (with-slots (number list) pattern
    (nth (mod number (length list))
         list)))

;;; pk

(defpattern pk (pattern)
  ((key :initarg :key)
   (default :initarg :default :initform 1))
  "A pk returns a value from the current *event* context, returning DEFAULT if that value is nil.")

(defun pk (key &optional (default 1))
  "Create an instance of the PK class."
  (make-instance 'pk
                 :key key
                 :default default))

(defmethod next ((pattern pk-pstream))
  (with-slots (key default) pattern
    (or (get-event-value *event* key)
        default)))

;;; prand

(defpattern prand (pattern)
  ((list :initarg :list))
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
  ((list :initarg :list)
   (lr :initarg :lr :initform nil) ;; last result
   )
  "A pxrand returns a random value from LIST, never repeating the same one twice in a row.")

(defun pxrand (list &optional remaining)
  "Create an instance of the PXRAND class."
  (assert (> (length (remove-duplicates list)) 1))
  (make-instance 'pxrand
                 :list list
                 :remaining remaining))

(defmethod next ((pattern pxrand-pstream))
  (with-slots (list lr) pattern
    (let ((res (alexandria:random-elt list)))
      (loop :while (eql res lr)
         :do (setf res (alexandria:random-elt list)))
      (setf lr res)
      res)))

;;; pfunc
;; NOTE: This implementation doesn't provide the event as an argument to the function like the SuperCollider implementation does.
;; Instead, access the event using the special variable *event*.

(defpattern pfunc (pattern)
  ((func :initarg :func))
  "A pfunc returns the result of the provided function FUNC.")

(defun pfunc (func)
  "Create an instance of the PFUNC class."
  (make-instance 'pfunc
                 :func func))

(defmethod next ((pattern pfunc-pstream))
  (funcall (slot-value pattern 'func)))

;;; pr

(defpattern pr (pattern)
  ((pattern :initarg :pattern)
   (repeats :initarg :repeats :initarg :inf)
   (cv :initarg :cv :initform nil) ;; current value
   (crr :initarg :crr :initform nil) ;; current repeats remaining
   )
  "A pr repeats a value from PATTERN REPEATS times before moving on to the next value from PATTERN.")

(defun pr (pattern &optional (repeats :inf))
  (make-instance 'pr
                 :pattern pattern
                 :repeats repeats))

(defmethod next ((pattern pr-pstream))
  (with-slots (cv crr repeats) pattern
    (when (or (null cv)
              (not (remainingp pattern 'crr)))
      (setf cv (next (slot-value pattern 'pattern)))
      (let ((*event* cv))
        (setf crr (next repeats))))
    (when crr
      (decf-remaining pattern 'crr)
      cv)))

;;; pdef
;; NOTE: the pattern in a pdef will repeat automatically. if the pattern in a pdef is redefined, it switches the next time the current one ends.

(defpattern pdef (pattern) ;; FIX: make play method for pdef to avoid adding it twice to the clock. also need 'reset' method.
  ((key :initarg :key))
  "A named pattern.")

(create-global-dictionary pdef)

(defun pdef (key &optional value)
  (when (not (null value))
    (pdef-ref key (plist-set (pdef-ref key) :pattern value)))
  (make-instance 'pdef
                 :key key))

(defmethod next ((pattern pdef-pstream))
  (with-slots (key) pattern
    (labels ((get-next ()
               (next (getf (pdef-ref key) :pstream))))
      (let ((nv (get-next)))
        (if (null nv)
            (when (not (null (getf (pdef-ref key) :pattern)))
              (pdef-ref key (plist-set (pdef-ref key) :pstream (as-pstream (getf (pdef-ref key) :pattern))))
              (get-next))
            nv)))))

;;; plazy

(defpattern plazy (pattern)
  ((func :initarg :func)
   (cp :initarg :cp :initform nil)))

(defun plazy (func)
  (make-instance 'plazy
                 :func func))

(defmethod next ((pattern plazy-pstream))
  (with-slots (cp func) pattern
    (if (null cp)
        (progn
          (setf cp (as-pstream (funcall func)))
          (next cp))
        (next cp))))

;;; plazyn

(defpattern plazyn (pattern)
  ((func :initarg :func)
   (repeats :initarg :repeats)
   (cp :initarg :cp :initform nil)
   (crr :initarg :crr :initform nil)))

(defun plazyn (func &optional (repeats :inf))
  (make-instance 'plazyn
                 :func func
                 :repeats repeats
                 :crr repeats))

(defmethod next ((pattern plazyn-pstream))
  (with-slots (cp func) pattern
    (labels ((maybe-funcall ()
               (when (remainingp pattern 'crr)
                 (setf cp (as-pstream (funcall func)))
                 (decf-remaining pattern 'crr))))
      (when (null cp)
        (maybe-funcall))
      (let ((nv (next cp)))
        (if (null nv)
            (progn
              (maybe-funcall)
              (next cp))
            nv)))))

;;; pcycles
;; inspired by tidalcycles

(defpattern pcycles (pattern)
  ((list :initarg :list)
   (pl :initarg :pl) ;; parsed list
   ))

(defun pcycles-parse-list (list) ;; FIX: maybe make pcycles parse in the 'next' method instead of at construction time?
  (labels ((recurse (list dur)
             (loop :for i :in list
                :collect (if (consp i)
                             (recurse i (* dur (/ 1 (length i))))
                             (event :freq i :dur dur)))))
    (alexandria:flatten (recurse list (/ 1 (length list))))))

(defun pcycles (list)
  (make-instance 'pcycles
                 :list list
                 :pl (pcycles-parse-list list)))

(defmethod next ((pattern pcycles-pstream))
  (with-slots (number pl) pattern
    (nth (mod number (length pl))
         pl)))

;;; pshift
;; shift a pattern N forward or backward, wrapping around

;; (defpattern pshift (pattern)
;;   ((list :initarg :list)
;;    (shift :initarg :shift)))

(defun pshift (pattern shift &optional (max-yield *max-pattern-yield-length*)) ;; FIX: don't use pseq internally, and make it possible for 'shift' to be a pattern
  (pseq (alexandria:rotate (next-upto-n pattern max-yield) shift)))

;; (defmethod next ((pattern pshift-pstream)))

;;; pn

(defpattern pn (pattern)
  ((pattern :initarg :pattern)
   (repeats :initarg :repeats)
   (pps :initarg :pps :initform nil) ;; pattern-pstream
   ))

(defun pn (pattern &optional (repeats :inf))
  (make-instance 'pn
                 :pattern pattern
                 :repeats repeats))

(defmethod as-pstream ((pattern pn)) ;; need this so that PATTERN won't be automatically converted to a pstream when the pn is.
  (with-slots (repeats remaining) pattern
    (make-instance 'pn-pstream
                   :remaining remaining
                   :pattern (slot-value pattern 'pattern)
                   :repeats repeats
                   :pps (as-pstream (slot-value pattern 'pattern)))))

(defmethod next ((pattern pn-pstream))
  (with-slots (pps) pattern
    (let ((nv (next pps)))
      (if (not (typep pps 'pattern))
          (prog1
              (when (remainingp pattern 'repeats)
                nv)
            (decf-remaining pattern 'repeats))
          (when (remainingp pattern 'repeats)
            (if (null nv)
                (progn
                  (setf pps (as-pstream (slot-value pattern 'pattern)))
                  (setf nv (next pps))
                  (decf-remaining pattern 'repeats)
                  (when (remainingp pattern 'repeats)
                    nv))
                nv))))))

;;; pshuf

(defpattern pshuf (pattern)
  ((list :initarg :list)
   (repeats :initarg :repeats)
   (sl :initarg :sl :initform nil) ;; shuffled list
   ))

(defun pshuf (list &optional (repeats 1))
  (make-instance 'pshuf
                 :list list
                 :repeats repeats))

(defmethod as-pstream ((pattern pshuf))
  (with-slots (remaining list repeats) pattern
    (make-instance 'pshuf-pstream
                   :remaining remaining
                   :list list
                   :repeats repeats
                   :sl (alexandria:shuffle (alexandria:copy-sequence 'list list)))))

(defmethod next ((pattern pshuf-pstream))
  (with-slots (number sl) pattern
    (nth (mod number (length sl))
         sl)))

;;; pwhite

(defpattern pwhite (pattern)
  ((lo :initarg :lo)
   (hi :initarg :hi)))

(defun pwhite (&optional (lo 0) (hi 1) (remaining :inf))
  (make-instance 'pwhite
                 :lo lo
                 :hi hi
                 :remaining remaining))

(defmethod next ((pattern pwhite-pstream))
  (random-range (next (slot-value pattern 'lo)) (next (slot-value pattern 'hi))))

;;; pbrown
;; FIX: maybe should coerce lo, hi, and step to floats if one of them is a float?

(defpattern pbrown (pattern)
  ((lo :initarg :lo)
   (hi :initarg :hi)
   (step :initarg :step)
   (cv :initarg :cv :initarg :cv)))

(defun pbrown (&optional (lo 0) (hi 1) (step 0.125) (remaining :inf))
  (make-instance 'pbrown
                 :lo lo
                 :hi hi
                 :step step
                 :cv (random-range lo hi)
                 :remaining remaining))

(defmethod next ((pattern pbrown-pstream))
  (with-slots (step cv lo hi) pattern
    (let ((nstep (next step)))
      (incf cv (random-range (* -1 nstep) nstep)))
    (setf cv (alexandria:clamp cv (next lo) (next hi)))
    cv))

;;; pseries

(defpattern pseries (pattern)
  ((start :initarg :start)
   (step :initarg :step)
   (cv :initarg :cv :initform nil)))

(defun pseries (&optional (start 0) (step 1) (remaining :inf))
  (make-instance 'pseries
                 :start start
                 :step step
                 :remaining remaining))

(defmethod as-pstream ((pattern pseries))
  (with-slots (start step remaining) pattern
    (let ((cv start))
      (when (functionp cv)
        (setf cv (funcall cv)))
      (make-instance 'pseries-pstream
                     :start start
                     :step (as-pstream step)
                     :remaining remaining
                     :cv cv))))

(defmethod next ((pattern pseries-pstream))
  (with-slots (cv step) pattern
    (prog1
        cv
      (incf cv (next step)))))

;;; pgeom

(defpattern pgeom (pattern)
  ((start :initarg :start)
   (grow :initarg :grow)
   (cv :initarg :cv :initform nil)))

(defun pgeom (&optional (start 0) (grow 1) (remaining :inf))
  (make-instance 'pgeom
                 :start start
                 :grow grow
                 :remaining remaining))

(defmethod as-pstream ((pattern pgeom))
  (with-slots (start grow remaining) pattern
    (let ((cv start))
      (when (functionp cv)
        (setf cv (funcall cv)))
      (make-instance 'pgeom-pstream
                     :start start
                     :grow (as-pstream grow)
                     :remaining remaining
                     :cv cv))))

(defmethod next ((pattern pgeom-pstream))
  (with-slots (cv grow) pattern
    (prog1
        cv
      (setf cv (* cv (next grow))))))

;;; ptrace

(defpattern ptrace (pattern)
  ((pattern :initarg :pattern)
   (key :initarg :key)
   (stream :initarg :stream)
   (prefix :initarg :prefix)))

(defun ptrace (pattern &optional key (stream t) (prefix ""))
  (make-instance 'ptrace
                 :pattern pattern
                 :key key
                 :stream stream
                 :prefix prefix))

(defmethod next ((pattern ptrace-pstream))
  (with-slots (key stream prefix) pattern
    (let* ((n (next (slot-value pattern 'pattern)))
           (result (if (not (null key))
                       (get-event-value n key)
                       n)))
      (format stream "~a~a~%" prefix result)
      result)))

;;; ppatlace

(defpattern ppatlace (listpattern)
  ())

(defun ppatlace (list &optional (repeats 1))
  (make-instance 'ppatlace
                 :list list
                 :repeats repeats))

(defmethod as-pstream ((pattern ppatlace)) ;; need to define this to make pstreams early instead of embedding them.
  (with-slots (repeats list) pattern
    (make-instance 'ppatlace-pstream
                   :list (mapcar #'as-pstream list)
                   :repeats repeats
                   :crr repeats)))

(defmethod next ((pattern ppatlace-pstream))
  (with-slots (number list) pattern
    (let* ((mod (mod number (length list)))
           (result (next (nth mod list))))
      (if (not (null result))
          result
          (progn
            (setf list (remove-if (constantly t) list :start mod :end (1+ mod)))
            (when (> (length list) 0)
              (next pattern)))))))

;;; pnary

(defpattern pnary (pattern)
  ((operator :initarg :operator)
   (patterns :initarg :patterns)))

(defun pnary (operator &rest patterns)
  (make-instance 'pnary
                 :operator operator
                 :patterns patterns))

(defmethod as-pstream ((pattern pnary))
  (with-slots (remaining operator patterns) pattern
    (make-instance 'pnary-pstream
                   :remaining remaining
                   :operator operator
                   :patterns (mapcar #'as-pstream patterns))))

(defmethod next ((pattern pnary-pstream))
  (let ((nexts (mapcar #'next (slot-value pattern 'patterns))))
    (unless (position nil nexts)
      (apply (slot-value pattern 'operator) nexts))))

;;; punop

(defun punop (operator pattern)
  (pnary operator pattern))

;;; pbinop

(defun pbinop (operator pattern-a pattern-b)
  (pnary operator pattern-a pattern-b))

;;; pnaryop

(defun pnaryop (operator pattern arglist)
  (apply #'pnary operator pattern arglist))
