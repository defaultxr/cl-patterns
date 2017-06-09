(in-package :cl-patterns)

;; NOTES:
;; FIX: make fork method.

;;; pattern glue

(defparameter *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defun make-default-event ()
  (combine-events (event) *event*))

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
   (quant :initarg :quant :initform 1 :accessor quant)
   (loop-p :initarg :loop-p :initform nil :accessor loop-p)
   (cleanup-functions :initarg :cleanup-functions :initform (list)))
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
  (let ((pstream (if (typep pattern 'pstream)
                     pattern
                     (as-pstream pattern)))
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
  ((number :initform 0)
   (pattern-stack :initform (list))
   (next-time :initarg :next-time :initform 0)
   (history :initarg :history :initform (list))
   (nodes :initarg :nodes :initform (list)))
  (:documentation "Pattern stream class."))

(defun remainingp (pattern &optional (key 'remaining))
  "Return t if PATTERN's KEY value is :inf, nil, or greater than 0."
  (or (or (null (slot-value pattern key))
          (eq (slot-value pattern key) :inf))
      (plusp (slot-value pattern key))))

(defun decf-remaining (pattern &optional (key 'remaining))
  "Decrease PATTERN's KEY value."
  (when (numberp (slot-value pattern key))
    (decf (slot-value pattern key))))

(defmethod next :around ((pattern pstream))
  (labels ((get-value-from-stack (pattern)
             (if (null (slot-value pattern 'pattern-stack))
                 (prog1
                     (get-value pattern)
                   (incf (slot-value pattern 'number))
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
    (let ((result (when (remainingp pattern)
                    (get-value-from-stack pattern))))
      (alexandria:appendf (slot-value pattern 'history) (list result))
      result)))

(defmethod pattern-embed ((pattern pstream) (embed pattern))
  (push (as-pstream embed) (slot-value pattern 'pattern-stack)))

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

(defmethod as-pstream :around ((pattern pattern))
  (let ((pstream (call-next-method)))
    (setf (slot-value pstream 'remaining) (slot-value pattern 'remaining))
    pstream))

(defmethod pstream-nth (n (pstream pstream)) ;; FIX: should this automatically "peek" at future values when the pstream hasn't advanced to the specified index yet? probably not...
  (nth-wrap n (slot-value pstream 'history)))

;;; pbind

(defclass pbind (pattern)
  ((pairs :initarg :pairs :initform (list)))
  (:documentation "A pbind associates keys with values for a pattern stream that returns events."))

(defun pbind (&rest pairs)
  "Create an instance of the PBIND class."
  (assert (evenp (length pairs)) (pairs))
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
    ;; handle pbind-special-post-keys
    ;; FIX: should maybe remove these keys from the 'pairs of pbind so they aren't included in the output?
    (loop :for (key value) :on (slot-value pattern 'pairs) :by #'cddr
       :do
       (alexandria:when-let* ((func (getf *pbind-special-post-keys* key))
                              (res (funcall func value pattern)))
         (setf pattern res)))
    ;; process :pdef key.
    (alexandria:when-let ((pos (position :pdef (keys pairs))))
      (pdef (nth (1+ pos) pairs) pattern))
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
  "Define a special key for pbind that alters the pbind as part of its initialization (\"pre-processing\"). These functions are called once as the pbind is initializing and must return a list if the key should inject values into the pbind pairs, or NIL if they should not."
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
  (setf (slot-value pattern 'remaining) (next value))
  nil
  )

(define-pbind-special-init-key inst ;; FIX: this should be part of event so it will affect the event as well.
  (list :instrument value))

(defparameter *pbind-special-post-keys* '())

(defmacro define-pbind-special-post-key (key &body body)
  "Define a special key for pbind that does post-processing on the pbind after it has been constructed. Each is run once on the pbind after it has been initialized, altering the type of pattern returned if the return value of the function is non-NIL."
  (let ((keyname (alexandria:make-keyword key)))
    `(setf (getf *pbind-special-post-keys* ,keyname)
           (lambda (value pattern)
             (declare (ignorable value pattern))
             ,@body))))

(define-pbind-special-post-key parp
  (parp pattern value))

(define-pbind-special-post-key pfin
  (pfin value pattern))

(define-pbind-special-post-key pfindur
  (pfindur value pattern))

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
               (unless (null next-cadr)
                 (if (position (car pairs) (keys *pbind-special-keys*))
                     (let ((result (funcall (getf *pbind-special-keys* (car pairs)) next-cadr)))
                       (setf *event* (combine-events *event* result)))
                     (set-event-value *event* (alexandria:ensure-symbol (car pairs) 'cl-patterns) next-cadr))
                 (if (not (null (cddr pairs)))
                     (pbind-accumulator (cddr pairs))
                     *event*)))))
    (let ((*event* (make-default-event)))
      (when (not (null (slot-value pattern 'remaining)))
        (set-event-value *event* :remaining (slot-value pattern 'remaining)))
      (pbind-accumulator (slot-value pattern 'pairs)))))

(defmethod as-pstream ((item pbind-pstream))
  item)

;;; pmono

(defun pmono (instrument &rest pairs)
  (assert (evenp (length pairs)) (pairs))
  (apply #'pbind
         :instrument instrument
         :type :mono
         pairs))

;;; pseq

(defpattern pseq (pattern)
  ((list :initarg :list)
   (repeats :initarg :repeats)
   (crr :initarg :crr :initform nil))
  "A pseq yields values from its list in the same order they were provided, repeating the list REPEATS times.")

(defun pseq (list &optional (repeats 1))
  "Create an instance of the PSEQ class."
  (make-instance 'pseq
                 :list list
                 :repeats repeats))

(defmethod as-pstream ((pattern pseq))
  (with-slots (repeats list) pattern
    (let ((r (next repeats)))
      (make-instance 'pseq-pstream
                     :list (next list)
                     :repeats r
                     :crr r))))

(defmethod next ((pattern pseq-pstream))
  (with-slots (number list) pattern
    (prog1
        (when (remainingp pattern 'crr)
          (nth-wrap number list))
      (when (= (1- (length list)) (mod number (length list)))
        (decf-remaining pattern 'crr)))))

;;; pser

(defpattern pser (pattern)
  ((list :initarg :list)
   (repeats :initarg :repeats)
   (crr :initarg :crr :initform nil))
  "A pser yields values from its list in the same order they were provided, returning a total of REPEATS values.")

(defun pser (list &optional (repeats 1))
  "Create an instance of the PSER class."
  (make-instance 'pser
                 :list list
                 :repeats repeats))

(defmethod as-pstream ((pattern pser))
  (with-slots (list repeats) pattern
    (let ((r (next repeats)))
      (make-instance 'pser-pstream
                     :list (next list)
                     :repeats r
                     :crr r))))

(defmethod next ((pattern pser-pstream))
  (with-slots (number list) pattern
    (when (remainingp pattern 'crr)
      (decf-remaining pattern 'crr)
      (nth (mod number (length list))
           list))))

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

;;; prand ;; FIX - have a separate REPEATS parameter instead of just putting it into remaining

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

;;; pxrand ;; FIX - have a separate REPEATS parameter instead of just putting it into remaining

(defpattern pxrand (pattern)
  ((list :initarg :list)
   (lr :initarg :lr :initform nil) ;; last result
   )
  "A pxrand returns a random value from LIST, never repeating the same one twice in a row.")

(defun pxrand (list &optional remaining)
  "Create an instance of the PXRAND class."
  (assert (> (length (remove-duplicates list)) 1) (list))
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
      (setf crr (if (typep repeats 'function)
                    (if (= (length (cadr (function-lambda-expression repeats))) 0) ;; FIX - might be a better way to do this.
                        (funcall repeats)
                        (funcall repeats cv))
                    (next repeats))))
    (when crr
      (decf-remaining pattern 'crr)
      cv)))

;;; pdef ;; FIX: should still give NILs when the pattern ends, but the clock just automatically reschedules it when it ends (maybe with a settable slot?)
;; NOTE: the pattern in a pdef will repeat automatically. if the pattern in a pdef is redefined, it switches the next time the current one ends.
;; if you want the pdef to stop after its current loop, set it to nil like so: (pdef KEY nil)

;; (pdef-ref KEY) returns the pdef's plist which holds the pattern, pstream, task, etc.
;; (pdef-ref-get KEY :task) returns the task associated with (pdef KEY).
;; (pdef-ref-set KEY :pattern PAT) sets the pattern for (pdef KEY) to PAT.

(defun pdef-ref-get (pdef-key key)
  "Get PDEF-KEY's KEY value from its plist."
  (getf (pdef-ref pdef-key) (alexandria:make-keyword key)))

(defun pdef-ref-set (pdef-key key value)
  "Set PDEF-KEY's KEY in its plist to VALUE."
  (pdef-ref pdef-key (plist-set (pdef-ref pdef-key) (alexandria:make-keyword key) value)))

(defpattern pdef (pattern) ;; FIX: need 'reset' method.
  ((key :initarg :key :reader pdef-key)
   (ps :initarg :ps :initform nil)
   (loop-p :initarg :loop-p :initform t :accessor loop-p))
  "A named pattern.")

(create-global-dictionary pdef)

(defun pdef (key &optional (value nil value-supplied-p))
  (when (or (not (null value))
            value-supplied-p)
    (pdef-ref-set key :pattern value))
  (make-instance 'pdef
                 :key key))

(defmethod pdef-pattern ((object pdef))
  (pdef-ref-get (pdef-key object) :pattern))

(defmethod quant ((object pdef))
  (quant (pdef-pattern object)))

(defmethod as-pstream ((pattern pdef))
  (with-slots (key) pattern
    (make-instance 'pdef-pstream
                   :key key
                   :ps (as-pstream (pdef-pattern pattern)))))

(defmethod next ((pattern pdef-pstream))
  (next (slot-value pattern 'ps)))

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
   (crr :initarg :crr :initform nil) ;; current repeats remaining
   ))

(defun pshuf (list &optional (repeats 1))
  (make-instance 'pshuf
                 :list list
                 :repeats repeats))

(defmethod as-pstream ((pattern pshuf))
  (with-slots (remaining list repeats) pattern
    (let ((list (typecase list
                  (pattern (next-upto-n list))
                  (function (funcall list))
                  (list list))))
      (make-instance 'pshuf-pstream
                     :list list
                     :repeats repeats
                     :sl (alexandria:shuffle (alexandria:copy-sequence 'list list))
                     :crr (next repeats)))))

(defmethod next ((pattern pshuf-pstream))
  (with-slots (number sl) pattern
    (when (and (= 0 (mod number (length sl)))
               (plusp number))
      (decf-remaining pattern 'crr))
    (when (remainingp pattern 'crr)
      (nth (mod number (length sl))
           sl))))

;;; pwhite

(defpattern pwhite (pattern)
  ((lo :initarg :lo)
   (hi :initarg :hi)
   (length :initarg :length)
   (crr :initarg :crr :initform nil)))

(defun pwhite (&optional (lo 0) (hi 1) (length :inf))
  (make-instance 'pwhite
                 :lo lo
                 :hi hi
                 :length length))

(defmethod as-pstream ((pattern pwhite))
  (with-slots (lo hi length) pattern
    (make-instance 'pwhite-pstream
                   :lo lo
                   :hi hi
                   :length length
                   :crr (next length))))

(defmethod next ((pattern pwhite-pstream))
  (when (remainingp pattern 'crr)
    (decf-remaining pattern 'crr)
    (random-range (next (slot-value pattern 'lo)) (next (slot-value pattern 'hi)))))

;;; pbrown
;; FIX: maybe should coerce lo, hi, and step to floats if one of them is a float?

(defpattern pbrown (pattern)
  ((lo :initarg :lo)
   (hi :initarg :hi)
   (step :initarg :step)
   (length :initarg :length)
   (crr :initarg :crr :initform nil)
   (cv :initarg :cv :initform nil)))

(defun pbrown (&optional (lo 0) (hi 1) (step 0.125) (length :inf))
  (make-instance 'pbrown
                 :lo lo
                 :hi hi
                 :step step
                 :length length))

(defmethod as-pstream ((pattern pbrown))
  (with-slots (lo hi step length) pattern
    (make-instance 'pbrown-pstream
                   :lo lo
                   :hi hi
                   :step step
                   :length length
                   :crr (next length)
                   :cv (random-range lo hi))))

(defmethod next ((pattern pbrown-pstream))
  (when (remainingp pattern 'crr)
    (decf-remaining pattern 'crr)
    (with-slots (step cv lo hi) pattern
      (let ((nstep (next step)))
        (incf cv (random-range (* -1 nstep) nstep)))
      (setf cv (alexandria:clamp cv (next lo) (next hi)))
      cv)))

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
    (make-instance 'pseries-pstream
                   :start start
                   :step (as-pstream step)
                   :remaining remaining
                   :cv (next start))))

(defmethod next ((pattern pseries-pstream))
  (with-slots (cv step) pattern
    (when cv
      (let ((nxt (next step)))
        (prog1
            cv
          (if (numberp nxt)
              (incf cv nxt)
              (setf cv nil)))))))

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
    (make-instance 'pgeom-pstream
                   :start start
                   :grow (as-pstream grow)
                   :remaining remaining
                   :cv (next start))))

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

(defpattern ppatlace (pattern)
  ((list :initarg :list)
   (repeats :initarg :repeats)
   (crr :initarg :crr :initform nil)))

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
    (when (remainingp pattern 'crr)
      (when (= (1- (length list)) (mod number (length list)))
        (decf-remaining pattern 'crr))
      (let* ((mod (mod number (length list)))
             (result (next (nth mod list))))
        (if (not (null result))
            result
            (progn
              (setf list (remove-if (constantly t) list :start mod :end (1+ mod)))
              (when (plusp (length list))
                (next pattern))))))))

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

;;; pslide

(defpattern pslide (pattern)
  ((list :initarg :list)
   (repeats :initarg :repeats)
   (len :initarg :len)
   (step :initarg :step)
   (start :initarg :start)
   (wrap-at-end :initarg :wrap-at-end)
   (crr :initarg :crr :initform nil) ;; current repeats remaining
   (cr :initarg :cr :initform nil) ;; current repeats
   (rcs :initarg :rcs :initform nil) ;; remaining from current segment
   (cv :initarg :cv :initform nil) ;; current value
   ))

(defun pslide (list &optional (repeats 1) (len 3) (step 1) (start 0) (wrap-at-end t))
  (make-instance 'pslide
                 :list list
                 :repeats repeats
                 :len len
                 :step step
                 :start start
                 :wrap-at-end wrap-at-end))

(defmethod as-pstream ((pattern pslide))
  (with-slots (list repeats len step start wrap-at-end) pattern
    (make-instance 'pslide-pstream
                   :list list
                   :repeats (as-pstream repeats)
                   :len (as-pstream len)
                   :step (as-pstream step)
                   :start start
                   :wrap-at-end wrap-at-end
                   :crr repeats
                   :cr 0
                   :rcs len
                   :cv start)))

(defmethod next ((pattern pslide-pstream)) 
  (with-slots (list repeats len step start wrap-at-end crr cr rcs cv) pattern
    (labels ((get-next ()
               (if (and (not wrap-at-end)
                        (minusp cv))
                   nil
                   (funcall (if wrap-at-end #'nth-wrap #'nth) cv list))))
      (when (remainingp pattern 'crr) 
        (if (remainingp pattern 'rcs)
            (prog1
                (get-next)
              (decf-remaining pattern 'rcs)
              (incf cv))
            (progn
              (decf-remaining pattern 'crr)
              (setf rcs (next len))
              (incf cr)
              (setf cv (+ start (* step cr)))
              (next pattern)))))))

;;; phistory
;; basically uses pstream-nth to refer to history

;; (pscratch (pseries 0 1) (pseq (list 0 (pseq (list 1 1 1 -3) :inf))))

(defpattern phistory (pattern)
  ((pattern :initarg :pattern)
   (step-pattern :initarg :step-pattern)))

(defun phistory (pattern step-pattern)
  (make-instance 'phistory
                 :pattern pattern
                 :step-pattern step-pattern))

(defmethod as-pstream ((pattern phistory))
  (make-instance 'phistory-pstream
                 :pattern (as-pstream (slot-value pattern 'pattern))
                 :step-pattern (as-pstream (slot-value pattern 'step-pattern))))

(defmethod next ((pattern phistory-pstream))
  (with-slots (pattern step-pattern) pattern
    (alexandria:when-let ((next-step (next step-pattern)))
      (next pattern)
      (pstream-nth next-step pattern))))

;;; pfuture
;; like phistory, but immediately slurps the whole pstream and then allows you to index from its history instead of advancing the source pattern one at a time as normal
;; phistory is kept around because some (time-based) patterns may need to be advanced normally rather than all at once (though it might be kind of a bad idea to try to use phistory, pfuture, or pscratch on those kinds of patterns)
;; FIX: maybe deprecate this and just integrate its functionality into phistory somehow?

(defpattern pfuture (phistory)
  ())

(defun pfuture (pattern step-pattern)
  (make-instance 'pfuture
                 :pattern pattern
                 :step-pattern step-pattern))

(defmethod as-pstream ((pattern pfuture))
  (let ((source-pstream (as-pstream (slot-value pattern 'pattern))))
    (next-upto-n source-pstream)
    (make-instance 'pfuture-pstream
                   :pattern source-pstream
                   :step-pattern (as-pstream (slot-value pattern 'step-pattern)))))

;;; pscratch
;;
;; FIX: pscratch's mechanism is slightly different:
;; supercollider:
;; > Pscratch(Pseries(0, 1), Pseq([1!3, -3].flat, inf)).asStream.nextN(12)
;; [ 0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 0 ]
;;
;; lisp:
;; > (next-n (pscratch (pseries 0 1) (pseq (list 1 1 1 -3) :inf)) 12)
;; (1 2 3 0 1 2 3 0 1 2 3 0)

(defpattern pscratch (pattern)
  ((pattern :initarg :pattern)
   (step-pattern :initarg :step-pattern)
   (ci :initarg :ci :initform 0) ;; current index
   ))

(defun pscratch (pattern step-pattern)
  (make-instance 'pscratch
                 :pattern pattern
                 :step-pattern step-pattern))

(defmethod as-pstream ((pattern pscratch))
  (make-instance 'pscratch-pstream
                 :pattern (as-pstream (slot-value pattern 'pattern))
                 :step-pattern (as-pstream (slot-value pattern 'step-pattern))))

(defmethod next ((pattern pscratch-pstream))
  (with-slots (ci (src-pattern pattern) step-pattern) pattern
    (incf ci (next step-pattern))
    (loop :while (>= ci (length (slot-value src-pattern 'history)))
       :do
       (next src-pattern))
    (pstream-nth ci src-pattern)))

;;; pif

(defpattern pif (pattern)
  ((condition :initarg :condition)
   (true :initarg :true)
   (false :initarg :false)))

(defun pif (condition true false)
  (make-instance 'pif
                 :condition condition
                 :true true
                 :false false))

(defmethod next ((pattern pif-pstream))
  (with-slots (condition true false) pattern
    (if (next condition)
        (next true)
        (next false))))

;;; ptracker (FIX)

(defpattern ptracker (pattern)
  ((head :initarg :head)
   (track :initarg :track)))

(defun ptracker (header &rest tracks)
  (assert (evenp (length header)) (header))
  (let* ((h-ev (apply #'event header))
         (h-keys (keys h-ev))
         (result (list)))
    (loop :for row :in tracks
       :do
       (let ((r-ev (cond ((equal row (list '-))
                          (when (= 0 (length result))
                            (event :type :rest)))
                         ((position (car row) (list :r :rest))
                          (event :type :rest))
                         (t
                          (progn
                            (apply #'event (loop
                                              :for e :in row
                                              :for i :from 0
                                              :append (list (nth i h-keys) e))))))))
         (if r-ev
             (alexandria:appendf result (list (combine-events h-ev r-ev)))
             (incf (dur (car (last result))) (dur h-ev)))))
    (pseq result)))

;; (defmethod next ((pattern ptracker-pstream))
;;   )

(defun tracker-shorthand (stream char subchar)
  "Reader macro for the #T notation."
  (declare (ignore char subchar))
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\; (lambda (stream ignore)
                               (funcall (get-macro-character #\; nil) stream ignore)
                               (values '+ptracker-separator-symbol+)))
    (set-macro-character #\newline (lambda (xx yy)
                                     (declare (ignore xx yy))
                                     (values '+ptracker-separator-symbol+)))
    (let ((val (remove-if #'null (split-sequence:split-sequence '+ptracker-separator-symbol+ (read-preserving-whitespace stream nil nil)))))
      `(cl-patterns::ptracker
        ',(car val) ,@(mapcar (lambda (x)
                                (if (and (= 1 (length x))
                                         (eq (car x) '-))
                                    (list 'list '(quote -))
                                    (append (list 'list) x)))
                              (cdr val))))))

(set-dispatch-macro-character #\# #\T #'tracker-shorthand)

;;; parp

(defpattern parp (pattern)
  ((pattern :initarg :pattern)
   (arpeggiator :initarg :arpeggiator)
   (cpe :initarg :cpe :initform nil) ;; current pattern event
   (cas :initarg :cas :initform nil) ;; current arpeggiator stream
   ))

(defun parp (pattern arpeggiator)
  (make-instance 'parp
                 :pattern pattern
                 :arpeggiator arpeggiator))

(defmethod as-pstream ((pattern parp))
  (let ((pstr (as-pstream (slot-value pattern 'pattern)))
        (arp (slot-value pattern 'arpeggiator)))
    (make-instance 'parp-pstream
                   :pattern pstr
                   :arpeggiator arp
                   :cpe (next pstr)
                   :cas (as-pstream arp))))

(defmethod next ((pattern parp-pstream))
  (with-slots (arpeggiator cpe cas) pattern
    (when (not (null cpe))
      (let ((nxt (let ((*event* cpe))
                   (next cas))))
        (if (null nxt)
            (progn
              (setf cpe (next (slot-value pattern 'pattern)))
              (setf cas (as-pstream arpeggiator))
              (next pattern))
            (combine-events cpe nxt))))))

;;; pfin

(defpattern pfin (pattern)
  ((count :initarg :count)
   (pattern :initarg :pattern)))

(defun pfin (count pattern)
  (make-instance 'pfin
                 :count count
                 :pattern pattern))

(defmethod as-pstream ((pattern pfin))
  (make-instance 'pfin-pstream
                 :count (next (slot-value pattern 'count))
                 :pattern (as-pstream (slot-value pattern 'pattern))))

(defmethod next ((pattern pfin-pstream))
  (with-slots (count number) pattern
    (when (< number count)
      (next (slot-value pattern 'pattern)))))

;;; pfindur

(defpattern pfindur (pattern)
  ((dur :initarg :dur)
   (pattern :initarg :pattern)
   (tolerance :initarg :tolerance)
   (ce :initarg :ce :initform 0) ;; current elapsed
   ))

(defun pfindur (dur pattern &optional (tolerance 0.001))
  (make-instance 'pfindur
                 :dur dur
                 :pattern pattern
                 :tolerance tolerance))

(defmethod as-pstream ((pattern pfindur))
  (make-instance 'pfindur-pstream
                 :dur (next (slot-value pattern 'dur))
                 :pattern (as-pstream (slot-value pattern 'pattern))
                 :tolerance (next (slot-value pattern 'tolerance))))

(defmethod next ((pattern pfindur-pstream))
  (with-slots (dur tolerance ce) pattern
    (alexandria:when-let ((n-event (next (slot-value pattern 'pattern))))
      (when (< (round-up ce tolerance) dur)
        (prog1
            (if (> (round-up (+ (delta n-event) ce) tolerance) dur)
                (combine-events n-event (event :delta (- dur ce)))
                n-event)
          (incf ce (delta n-event)))))))

;;; pstutter

(defpattern pstutter (pattern)
  ((n :initarg :n)
   (pattern :initarg :pattern)
   (ce :initarg :ce :initform nil)
   (crr :initarg :crr :initform 0)))

(defun pstutter (n pattern)
  (make-instance 'pstutter
                 :n n
                 :pattern pattern))

(defmethod next ((pattern pstutter-pstream))
  (with-slots (n ce crr) pattern
    (loop :while (and (not (null crr))
                      (= 0 crr))
       :do
       (setf ce (next (slot-value pattern 'pattern)))
       (setf crr (next n)))
    (when (not (null crr))
      (decf-remaining pattern 'crr)
      ce)))

;;; pdurstutter

(defpattern pdurstutter (pattern)
  ((n :initarg :n)
   (pattern :initarg :pattern)
   (ce :initarg :ce :initform nil)
   (crr :initarg :crr :initform 0)))

(defun pdurstutter (n pattern)
  (make-instance 'pdurstutter
                 :n n
                 :pattern pattern))

(defmethod next ((pattern pdurstutter-pstream))
  (with-slots (n ce crr) pattern
    (loop :while (and (not (null crr))
                      (= 0 crr))
       :do
       (setf crr (next n))
       (let ((e (next (slot-value pattern 'pattern))))
         (setf ce (if (typep e 'event)
                      (combine-events e (event :dur (/ 1 (get-event-value e :dur))))
                      (/ 1 e)))))
    (when (not (null crr))
      (decf-remaining pattern 'crr)
      ce)))

