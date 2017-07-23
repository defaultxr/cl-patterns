(in-package :cl-patterns)

;; NOTES:
;; FIX: make fork method. fork should set the pattern's loop-p to NIL if it's a pdef.
;; FIX: remove :remaining key and just use :pfin instead?

;;; pattern glue

(defparameter *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defun make-default-event ()
  (combine-events (event) *event*))

(defun set-parents (pattern)
  "Set all of PATTERN's subpatterns' \"parent\" slot to PATTERN."
  (labels ((set-parent (list parent)
             "Recurse through LIST, setting the parent of any pattern found to PARENT."
             (cond ((typep list 'list)
                    (mapc (lambda (x) (set-parent x parent)) list))
                   ((typep list 'pattern)
                    (setf (slot-value list 'parent) parent)))))
    (loop :for slot :in (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern)))
       :do
       (set-parent (slot-value pattern slot) pattern))
    pattern))

(defmacro defpattern (name superclasses slots &optional documentation pre-init)
  "Define a pattern. This macro automatically generates the pattern's class, its pstream class, and the function to create an instance of the pattern, and makes them external in the cl-patterns package.

NAME is the name of the pattern. Typically a word or two that describes its function, prefixed with p.

SUPERCLASSES is a list of superclasses of the pattern. Most patterns just subclass the 'pattern' class.

SLOTS is a list of slots that the pattern and pstreams derived from it have. Each slot can either be just a symbol, or a slot definition a la `defclass'. You can provide a default for the slot with the :default key, and you can set a slot as a state slot (which only appears in the pattern's pstream class) by setting the :state key to t.

DOCUMENTATION is a docstring describing the pattern. We recommend providing at least one example, and a \"See also\" section to refer to similar pattern classes.

PRE-INIT is an expression which will be inserted into the pattern creation function prior to initialization of the instance. Typically you'd use this for inserting `assert' statements, for example."
  (let ((slots (mapcar #'alexandria:ensure-list slots))
        (name-pstream (intern (concatenate 'string (symbol-name name) "-PSTREAM") 'cl-patterns))
        (super-pstream (if (eq 'pattern (car superclasses))
                           'pstream
                           (intern (concatenate 'string (symbol-name (car superclasses)) "-PSTREAM") 'cl-patterns))))
    (labels ((desugar-slot (slot)
               "Convert a slot into something appropriate for defclass to handle."
               (let ((name (car slot))
                     (rest (cdr slot)))
                 (setf rest (alexandria:remove-from-plist rest :default))
                 (setf rest (alexandria:remove-from-plist rest :state))
                 (when (not (position :initarg (keys rest)))
                   (alexandria:appendf rest (list :initarg (alexandria:make-keyword name))))
                 (append (list name) rest)))
             (optional-slot-p (slot)
               "Whether the slot is optional or not. A slot is optional if a default is provided."
               (position :default (keys (cdr slot))))
             (state-slot-p (slot)
               "Whether the slot is a state slot or not. State slots will not be included as arguments to the pattern creation function."
               (position :state (keys (cdr slot))))
             (function-lambda-list (slots)
               "Generate the lambda list for the pattern's creation function."
               (let ((optional-used nil))
                 (loop :for slot :in slots
                    :append (when (not (state-slot-p slot))
                              (if (optional-slot-p slot)
                                  (prog1
                                      (append (if (not optional-used)
                                                  (list '&optional)
                                                  (list))
                                              (list (list (car slot) (getf (cdr slot) :default))))
                                    (setf optional-used t))
                                  (list (car slot))))))))
      `(progn
         (defclass ,name ,superclasses
           ,(mapcar #'desugar-slot (remove-if #'state-slot-p slots))
           ,@(when documentation
               `((:documentation ,documentation))))
         (defclass ,name-pstream (,name ,super-pstream)
           ,(mapcar #'desugar-slot (remove-if-not #'state-slot-p slots))
           (:documentation ,(format nil "pstream class for ~a." name)))
         (defun ,name ,(function-lambda-list slots)
           ,documentation
           ,pre-init
           (set-parents
            (make-instance ',name
                           ,@(mapcan (lambda (i) (list (alexandria:make-keyword (car i)) (car i)))
                                     (remove-if #'state-slot-p slots)))))
         (export '(,name ,name-pstream))))))

(defparameter *max-pattern-yield-length* 64
  "The maximum amount of events or values that will be used by patterns like pshift, etc, in order to prevent hangs caused by infinite-length patterns.")

;;; pattern

(defclass pattern ()
  ((remaining :initarg :remaining :initform nil)
   (quant :initarg :quant :initform 1 :accessor quant)
   (parent :initarg :parent :initform nil :accessor parent)
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
   (next-time :initarg :next-time :initform 0) ;; FIX: is this needed?
   (history :initarg :history :initform (list))
   (nodes :initarg :nodes :initform (list)) ;; FIX: move this to the 'task' class?
   )
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
  (let ((slots (remove-if (lambda (x) (eq x 'parent)) (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern))))))
    (apply #'make-instance
                      (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM") 'cl-patterns)
                      (loop :for slot :in slots
                         :collect (alexandria:make-keyword slot)
                         :collect (as-pstream (slot-value pattern slot))))))

(defmethod as-pstream :around ((pattern pattern))
  (let ((pstream (call-next-method)))
    (setf (slot-value pstream 'remaining) (slot-value pattern 'remaining))
    (set-parents pstream)
    pstream))

(defmethod pstream-nth (n (pstream pstream)) ;; FIX: should this automatically "peek" at future values when the pstream hasn't advanced to the specified index yet? probably not...
  (nth-wrap n (slot-value pstream 'history)))

(defun parent-pbind (pattern)
  "Get the containing pbind of PATTERN, or NIL if there isn't one."
  (let ((par (parent pattern)))
    (loop :until (or (null par) (typep par 'pbind))
       :do (setf par (slot-value par 'parent)))
    par))

(defmethod beats-elapsed ((pattern pstream)) ;; FIX: get this from parent pattern if possible.
  )

;;; pbind

(defclass pbind (pattern)
  ((pairs :initarg :pairs :initform (list)))
  (:documentation "Please refer to the `pbind' documentation."))

(defun pbind (&rest pairs)
  "pbind yields events determined by its PAIRS, which are a list of keys and values. Each key corresponds to a key in the resulting events, and each value is treated as a pattern that is evaluated for each step of the pattern to generate the value for its key.

Example: (next-n (pbind :foo (pseq '(1 2 3)) :bar :hello) 4) ;=>
((EVENT :FOO 1 :BAR :HELLO) (EVENT :FOO 2 :BAR :HELLO)
 (EVENT :FOO 3 :BAR :HELLO) NIL)

See also: `pmono'"
  (assert (evenp (length pairs)) (pairs))
  (let* ((res-pairs nil)
         (pattern (make-instance 'pbind)))
    (loop :for (key value) :on pairs :by #'cddr
       :do
       (progn
         (when (typep value 'pattern)
           (setf (slot-value value 'parent) pattern))
         (if (position key *pbind-special-init-keys*)
             (let ((result (funcall (getf *pbind-special-init-keys* key) value pattern)))
               (when result
                 (setf res-pairs (append res-pairs result))))
             (alexandria:appendf res-pairs (list key value)))))
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

(setf (documentation 'pbind 'type) (documentation 'pbind 'function))

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

(define-pbind-special-post-key pdurstutter
  (pdurstutter value pattern))

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
  "pmono defines a mono instrument event pstream. It's effectively the same as `pbind' with its :type key set to :mono.

See also: `pbind'"
  (assert (evenp (length pairs)) (pairs))
  (apply #'pbind
         :instrument instrument
         :type :mono
         pairs))

;;; pseq

(defpattern pseq (pattern)
  (list
   (repeats :default 1)
   (current-repeats-remaining :state t))
  "pseq yields values from LIST in the same order they were provided, repeating the whole list REPEATS times.

Example: (next-n (pseq '(5 6 7) 2) 7) ;=> (5 6 7 5 6 7 NIL)

See also: `pser'")

(defmethod as-pstream ((pattern pseq))
  (with-slots (repeats list) pattern
    (let ((r (next repeats)))
      (make-instance 'pseq-pstream
                     :list (next list)
                     :repeats r
                     :current-repeats-remaining r))))

(defmethod next ((pattern pseq-pstream))
  (with-slots (number list) pattern
    (prog1
        (when (remainingp pattern 'current-repeats-remaining)
          (nth-wrap number list))
      (when (= (1- (length list)) (mod number (length list)))
        (decf-remaining pattern 'current-repeats-remaining)))))

;;; pser

(defpattern pser (pattern)
  (list
   (length :default 1)
   (current-repeats-remaining :state t))
  "pser yields values from LIST in the same order they were provided, returning a total of LENGTH values.

Example: (next-n (pser '(5 6 7) 2) 3) ;=> (5 6 NIL)

See also: `pseq'")

(defmethod as-pstream ((pattern pser))
  (with-slots (list length) pattern
    (let ((r (next length)))
      (make-instance 'pser-pstream
                     :list (next list)
                     :length r
                     :current-repeats-remaining r))))

(defmethod next ((pattern pser-pstream))
  (with-slots (number list) pattern
    (when (remainingp pattern 'current-repeats-remaining)
      (decf-remaining pattern 'current-repeats-remaining)
      (nth (mod number (length list))
           list))))

;;; pk

(defpattern pk (pattern)
  (key
   (default :default 1))
  "pk returns the value of KEY in the current *event* context, returning DEFAULT if that value is nil.

Example: (next-n (pbind :foo (pseries) :bar (pk :foo)) 3) ;=> ")

(defmethod next ((pattern pk-pstream))
  (with-slots (key default) pattern
    (or (get-event-value *event* key)
        default)))

;;; prand

(defpattern prand (pattern)
  (list
   (length :default :inf)
   (current-repeats-remaining :state t))
  "prand returns a random value from LIST, returning a total of LENGTH values.

Example: (next-n (prand '(1 2 3) 5) 6) ;=> (3 2 2 1 1 NIL)")

(defmethod as-pstream ((pattern prand))
  (with-slots (list length) pattern
    (let ((r (next length)))
      (make-instance 'prand-pstream
                     :list list
                     :length r
                     :current-repeats-remaining r))))

(defmethod next ((pattern prand-pstream))
  (when (remainingp pattern 'current-repeats-remaining)
    (decf-remaining pattern 'current-repeats-remaining)
    (alexandria:random-elt (slot-value pattern 'list))))

;;; pxrand

(defpattern pxrand (pattern)
  (list
   (length :default :inf)
   (last-result :state t :initform nil)
   (current-repeats-remaining :state t))
  "pxrand returns a random value from LIST that is not equal to the last result, returning a total of LENGTH values."
  (assert (> (length (remove-duplicates list)) 1) (list)))

(defmethod as-pstream ((pattern pxrand))
  (with-slots (list length) pattern
    (let ((r (next length)))
      (make-instance 'pxrand-pstream
                     :list list
                     :length r
                     :current-repeats-remaining r))))

(defmethod next ((pattern pxrand-pstream))
  (with-slots (list last-result) pattern
    (when (remainingp pattern 'current-repeats-remaining)
      (decf-remaining pattern 'current-repeats-remaining)
      (let ((res (alexandria:random-elt list)))
        (loop :while (eql res last-result)
           :do (setf res (alexandria:random-elt list)))
        (setf last-result res)
        res))))

;;; pwrand

(defpattern pwrand (pattern)
  (list
   (weights :default (make-list (length list) :initial-element 1))
   (length :default :inf)
   (current-repeats-remaining :state t))
  "pwrand returns a random element from LIST weighted by respective values from WEIGHTS, for a total of LENGTH values.

See also: `prand', `pxrand', `pwxrand'")

(defmethod as-pstream ((pattern pwrand))
  (with-slots (list weights length) pattern
    (let ((r (next length)))
      (make-instance 'pwrand-pstream
                     :list list
                     :weights weights
                     :length r
                     :current-repeats-remaining r))))

(defmethod next ((pattern pwrand-pstream))
  (with-slots (list weights) pattern
    (when (remainingp pattern 'current-repeats-remaining)
      (decf-remaining pattern 'current-repeats-remaining)
      (let* ((cweights (cumulative-list (normalized-sum weights)))
             (num (random 1.0)))
        (nth (index-of-greater-than num cweights) list)))))

;;; pfunc
;; NOTE: This implementation doesn't provide the event as an argument to the function like the SuperCollider implementation does.
;; Instead, access the event using the special variable *event*.

(defpattern pfunc (pattern)
  (func)
  "pfunc returns the result of the provided function FUNC."
  (assert (typep func 'function) (func)))

(defmethod next ((pattern pfunc-pstream))
  (funcall (slot-value pattern 'func)))

;;; pr

(defpattern pr (pattern)
  (pattern
   (repeats :default :inf)
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t :initform nil))
  "pr yields a value from PATTERN REPEATS times before moving on to the next value from PATTERN.

See also: `pstutter', `pdurstutter', `parp'")

(defmethod next ((pattern pr-pstream))
  (with-slots (current-value current-repeats-remaining repeats) pattern
    (when (or (null current-value)
              (not (remainingp pattern 'current-repeats-remaining)))
      (setf current-value (next (slot-value pattern 'pattern)))
      (setf current-repeats-remaining (if (typep repeats 'function)
                                          (if (= (length (cadr (function-lambda-expression repeats))) 0) ;; FIX - might be a better way to do this.
                                              (funcall repeats)
                                              (funcall repeats current-value))
                                          (next repeats))))
    (when current-repeats-remaining
      (decf-remaining pattern 'current-repeats-remaining)
      current-value)))

;;; pdef
;; NOTE: the pattern in a pdef will loop when played on a clock.
;; this can be prevented by doing (setf (slot-value YOUR-PDEF 'loop-p) nil)
;; if the pattern in a pdef is redefined, it switches between the current and next loops by default.
;; not fully implemented yet (FIX): if you want the pdef to switch at a time other than between loops, set the 'quant or 'condition slots.
;; if you want the pdef to stop after its current loop, set it to nil like so: (pdef KEY nil)

;; (pdef-ref KEY) returns the pdef's plist which holds the pattern, pstream, task, etc.
;; (pdef-ref-get KEY :task) returns the task associated with (pdef KEY).
;; (pdef-ref-set KEY :pattern PAT) sets the pattern for (pdef KEY) to PAT.

;; FIX: need 'reset' method

(defun pdef-ref-get (pdef-key key)
  "Get PDEF-KEY's KEY value from its plist."
  (getf (pdef-ref pdef-key) (alexandria:make-keyword key)))

(defun pdef-ref-set (pdef-key key pattern)
  "Set PDEF-KEY's KEY in its plist to PATTERN."
  (pdef-ref pdef-key (plist-set (pdef-ref pdef-key) (alexandria:make-keyword key) pattern)))

(defpattern pdef (pattern)
  ((key :reader pdef-key)
   (current-pstream :state t)
   (loop-p :initform t :accessor loop-p))
  "pdef names a pattern PATTERN by KEY.")

(create-global-dictionary pdef)

(defun pdef (key &optional (pattern nil pattern-supplied-p))
  "pdef names a pattern PATTERN by KEY."
  (when (or (not (null pattern))
            pattern-supplied-p)
    (pdef-ref-set key :pattern pattern))
  (make-instance 'pdef
                 :key key))

(defmethod pdef-pattern ((object pdef))
  (pdef-ref-get (pdef-key object) :pattern))

(defmethod quant ((object pdef))
  (quant (pdef-pattern object)))

(defmethod quant ((object null))
  1)

(defmethod as-pstream ((pattern pdef))
  (with-slots (key) pattern
    (make-instance 'pdef-pstream
                   :key key
                   :current-pstream (as-pstream (pdef-pattern pattern)))))

(defmethod next ((pattern pdef-pstream))
  (next (slot-value pattern 'current-pstream)))

;;; plazy

(defpattern plazy (pattern)
  (func
   (current-pstream :state t :initform nil))
  "plazy funcalls FUNC which should return a pattern, which is then yielded from until its end, at which point FUNC is re-evaluated to generate the next pattern.")

(defmethod next ((pattern plazy-pstream))
  (with-slots (current-pstream func) pattern
    (if (null current-pstream)
        (progn
          (setf current-pstream (as-pstream (funcall func)))
          (next current-pstream))
        (next current-pstream))))

;;; plazyn

(defpattern plazyn (pattern)
  (func
   (repeats :default :inf)
   (current-pstream :state t :initform nil)
   (current-repeats-remaining :state t :initform nil))
  "plazyn funcalls FUNC which should return a pattern, which is then yielded from until its end, at which point FUNC is re-evaluated to generate the next pattern. The pattern is generated a total of REPEATS times.")

(defmethod next ((pattern plazyn-pstream))
  (with-slots (func repeats current-pstream current-repeats-remaining) pattern
    (labels ((maybe-funcall ()
               (when (remainingp pattern 'current-repeats-remaining)
                 (setf current-pstream (as-pstream (funcall func)))
                 (decf-remaining pattern 'current-repeats-remaining))))
      (when (null current-repeats-remaining)
        (setf current-repeats-remaining (next repeats)))
      (when (null current-pstream)
        (maybe-funcall))
      (let ((nv (next current-pstream)))
        (if (null nv)
            (progn
              (maybe-funcall)
              (next current-pstream))
            nv)))))

;;; pcycles
;; inspired by tidalcycles
;; FIX: remove this and write it as regular functions instead?

(defpattern pcycles (pattern) ;; FIX: add REPEATS slot
  (list
   (parsed-list :state t))
  "pcycles yields values from LIST as events whose dur is (/ 1 list-length) and whose value is the original value in the list. This process recurses into sublists, subdividing their durs equally among the sublist's contents to be a fraction of what their dur originally would be. The total dur yielded by pcycles is always equal to 1. pcycles repeats the whole LIST once.")

(defun pcycles-parse-list (list)
  (labels ((recurse (list dur)
             (loop :for i :in list
                :collect (if (consp i)
                             (recurse i (* dur (/ 1 (length i))))
                             (event :value i :dur dur)))))
    (alexandria:flatten (recurse list (/ 1 (length list))))))

(defmethod as-pstream ((pattern pcycles)) ;; FIX: maybe make pcycles parse in the 'next' method instead of at construction time?
  (with-slots (list) pattern
    (make-instance 'pcycles-pstream
                   :list list
                   :parsed-list (pcycles-parse-list list))))

(defmethod next ((pattern pcycles-pstream))
  (with-slots (number parsed-list) pattern
    (nth number parsed-list)))

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
  (pattern
   (repeats :default :inf)
   (current-pstream :state t :initform nil))
  "pn embeds the full PATTERN into the pstream REPEATS times.")

(defmethod as-pstream ((pattern pn)) ;; need this so that PATTERN won't be automatically converted to a pstream when the pn is.
  (with-slots (repeats remaining) pattern
    (make-instance 'pn-pstream
                   :remaining remaining
                   :pattern (slot-value pattern 'pattern)
                   :repeats repeats
                   :current-pstream (as-pstream (slot-value pattern 'pattern)))))

(defmethod next ((pattern pn-pstream))
  (with-slots (current-pstream) pattern
    (let ((nv (next current-pstream)))
      (if (not (typep current-pstream 'pattern))
          (prog1
              (when (remainingp pattern 'repeats)
                nv)
            (decf-remaining pattern 'repeats))
          (when (remainingp pattern 'repeats)
            (if (null nv)
                (progn
                  (setf current-pstream (as-pstream (slot-value pattern 'pattern)))
                  (setf nv (next current-pstream))
                  (decf-remaining pattern 'repeats)
                  (when (remainingp pattern 'repeats)
                    nv))
                nv))))))

;;; pshuf

(defpattern pshuf (pattern)
  (list
   (repeats :default 1)
   (shuffled-list :state t :initform nil)
   (current-repeats-remaining :state t :initform nil))
  "pshuf shuffles LIST, then yields each item from the shuffled list, repeating the list REPEATS times.")

(defmethod as-pstream ((pattern pshuf))
  (with-slots (remaining list repeats) pattern
    (let ((list (typecase list
                  (pattern (next-upto-n list))
                  (function (funcall list))
                  (list list))))
      (make-instance 'pshuf-pstream
                     :list list
                     :repeats repeats
                     :shuffled-list (alexandria:shuffle (alexandria:copy-sequence 'list list))
                     :current-repeats-remaining (next repeats)))))

(defmethod next ((pattern pshuf-pstream))
  (with-slots (number shuffled-list) pattern
    (when (and (= 0 (mod number (length shuffled-list)))
               (plusp number))
      (decf-remaining pattern 'current-repeats-remaining))
    (when (remainingp pattern 'current-repeats-remaining)
      (nth (mod number (length shuffled-list))
           shuffled-list))))

;;; pwhite

(defpattern pwhite (pattern)
  ((lo :default 0)
   (hi :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t :initform nil))
  "pwhite yields LENGTH random numbers between LO and HI.")

(defmethod as-pstream ((pattern pwhite))
  (with-slots (lo hi length) pattern
    (make-instance 'pwhite-pstream
                   :lo (as-pstream lo)
                   :hi (as-pstream hi)
                   :length length
                   :current-repeats-remaining (next length))))

(defmethod next ((pattern pwhite-pstream))
  (when (remainingp pattern 'current-repeats-remaining)
    (decf-remaining pattern 'current-repeats-remaining)
    (alexandria:when-let ((nlo (next (slot-value pattern 'lo)))
                          (nhi (next (slot-value pattern 'hi))))
      (random-range nlo nhi))))

;;; pbrown

(defpattern pbrown (pattern)
  ((lo :default 0)
   (hi :default 1)
   (step :default 0.125)
   (length :default :inf)
   (current-repeats-remaining :state t :initform nil)
   (current-value :state t :initform nil))
  "pbrown implements brownian motion, yielding LENGTH values between LO and HI, each value a maximum of STEP away from the previous value.")

(defmethod as-pstream ((pattern pbrown))
  (with-slots (lo hi step length) pattern
    (make-instance 'pbrown-pstream
                   :lo (as-pstream lo)
                   :hi (as-pstream hi)
                   :step (as-pstream step)
                   :length length
                   :current-repeats-remaining (next length)
                   :current-value (random-range lo hi))))

(defmethod next ((pattern pbrown-pstream))
  (when (remainingp pattern 'current-repeats-remaining)
    (decf-remaining pattern 'current-repeats-remaining)
    (with-slots (lo hi step current-value) pattern
      (alexandria:when-let ((nlo (next lo))
                            (nhi (next hi))
                            (nstep (next step)))
        (incf current-value (random-range (* -1 nstep) nstep))
        (setf current-value (alexandria:clamp current-value nlo nhi))))))

;;; pseries

(defpattern pseries (pattern)
  ((start :default 0)
   (step :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t :initform nil)
   (current-value :state t :initform nil))
  "pseries yields START, and then each subsequent value is the previous value plus STEP, for a total of LENGTH values yielded.")

(defmethod as-pstream ((pattern pseries))
  (with-slots (start step length) pattern
    (make-instance 'pseries-pstream
                   :start start
                   :step (as-pstream step)
                   :length length
                   :current-repeats-remaining (next length)
                   :current-value (next start))))

(defmethod next ((pattern pseries-pstream))
  (with-slots (step current-value) pattern
    (when (and (remainingp pattern 'current-repeats-remaining)
               current-value)
      (decf-remaining pattern 'current-repeats-remaining)
      (let ((nxt (next step)))
        (prog1
            current-value
          (if (numberp nxt)
              (incf current-value nxt)
              (setf current-value nil)))))))

;;; pgeom

(defpattern pgeom (pattern)
  ((start :default 1)
   (grow :default 2)
   (length :default :inf)
   (current-repeats-remaining :state t :initform nil)
   (current-value :state t :initform nil))
  "pgeom yields START, and then each subsequent value is the previous value times GROW, for a total of LENGTH values yielded.")

(defmethod as-pstream ((pattern pgeom))
  (with-slots (start grow length) pattern
    (make-instance 'pgeom-pstream
                   :start start
                   :grow (as-pstream grow)
                   :length length
                   :current-repeats-remaining (next length)
                   :current-value (next start))))

(defmethod next ((pattern pgeom-pstream))
  (with-slots (grow current-value) pattern
    (when (remainingp pattern 'current-repeats-remaining)
      (decf-remaining pattern 'current-repeats-remaining)
      (prog1
          current-value
        (setf current-value (* current-value (next grow)))))))

;;; ptrace

(defpattern ptrace (pattern)
  (pattern
   (key :default nil)
   (stream :default t)
   (prefix :default ""))
  "ptrace prints to STREAM the PREFIX and then the value of KEY for each event yielded by PATTERN, or the whole event or value if KEY is not provided. ptrace yields everything from the source PATTERN unaffected.")

(defmethod next ((pattern ptrace-pstream))
  (with-slots (key stream prefix) pattern
    (let* ((n (next (slot-value pattern 'pattern)))
           (result (if (and (not (null key))
                            (not (null n)))
                       (get-event-value n key)
                       n)))
      (format stream "~a~a~%" prefix result)
      result)))

;;; ppatlace

(defpattern ppatlace (pattern)
  (list
   (repeats :default 1)
   (current-repeats-remaining :state t :initform nil))
  "ppatlace yields each value from LIST in sequence. If the value is a pattern, one value is yielded from that pattern before moving on to the next item in LIST. The second time around the LIST, the second value yielded from each pattern in LIST will be yielded instead. If one of the patterns embedded in LIST ends sooner than the others, it is simply removed and the ppatlace continues to yield from the rest of the LIST. The entire LIST is yielded through a total of REPEATS times.")

(defmethod as-pstream ((pattern ppatlace))
  (with-slots (repeats list) pattern
    (make-instance 'ppatlace-pstream
                   :list (mapcar #'as-pstream list)
                   :repeats repeats
                   :current-repeats-remaining (next repeats))))

(defmethod next ((pattern ppatlace-pstream))
  (with-slots (number list) pattern
    (when (remainingp pattern 'current-repeats-remaining)
      (when (= (1- (length list)) (mod number (length list)))
        (decf-remaining pattern 'current-repeats-remaining))
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
  (operator
   (patterns :initarg :patterns))
  "pnary yields the result of applying OPERATOR to each value yielded by each pattern in PATTERNS.")

(defun pnary (operator &rest patterns)
  "pnary yields the result of applying OPERATOR to each value yielded by each pattern in PATTERNS."
  (make-instance 'pnary
                 :operator operator
                 :patterns patterns))

(defmethod as-pstream ((pattern pnary))
  (with-slots (operator patterns) pattern
    (make-instance 'pnary-pstream
                   :operator operator
                   :patterns (mapcar #'as-pstream patterns))))

(defmethod next ((pattern pnary-pstream))
  (let ((nexts (mapcar #'next (slot-value pattern 'patterns))))
    (unless (position nil nexts)
      (apply (slot-value pattern 'operator) nexts))))

;;; pslide

(defpattern pslide (pattern)
  (list
   (repeats :default 1)
   (len :default 3)
   (step :default 1)
   (start :default 0)
   (wrap-at-end :default t)
   (current-repeats-remaining :state t :initform nil)
   (current-repeats :state t :initform nil)
   (remaining-current-segment :state t :initform nil)
   (current-value :state t :initform nil))
  "pslide ") ;; FIX

(defmethod as-pstream ((pattern pslide))
  (with-slots (list repeats len step start wrap-at-end) pattern
    (make-instance 'pslide-pstream
                   :list list
                   :repeats (as-pstream repeats)
                   :len (as-pstream len)
                   :step (as-pstream step)
                   :start start
                   :wrap-at-end wrap-at-end
                   :current-repeats-remaining repeats
                   :current-repeats 0
                   :remaining-current-segment len
                   :current-value start)))

(defmethod next ((pattern pslide-pstream)) 
  (with-slots (list repeats len step start wrap-at-end current-repeats-remaining current-repeats remaining-current-segment current-value) pattern
    (labels ((get-next ()
               (if (and (not wrap-at-end)
                        (minusp current-value))
                   nil
                   (funcall (if wrap-at-end #'nth-wrap #'nth) current-value list))))
      (when (remainingp pattern 'current-repeats-remaining) 
        (if (remainingp pattern 'remaining-current-segment)
            (prog1
                (get-next)
              (decf-remaining pattern 'remaining-current-segment)
              (incf current-value))
            (progn
              (decf-remaining pattern 'current-repeats-remaining)
              (setf remaining-current-segment (next len))
              (incf current-repeats)
              (setf current-value (+ start (* step current-repeats)))
              (next pattern)))))))

;;; phistory
;; basically uses pstream-nth to refer to history

;; (pscratch (pseries 0 1) (pseq (list 0 (pseq (list 1 1 1 -3) :inf))))

(defpattern phistory (pattern)
  (pattern
   step-pattern)
  "phistory refers back to PATTERN's history, yielding the value at the index provided by STEP-PATTERN. Note that PATTERN is still advanced once per event, and if STEP-PATTERN yields a number pointing to an event in PATTERN that hasn't been yielded yet (i.e. if PATTERN has only advanced once but STEP-PATTERN yields 3 for its output), it will return nil.

See also: `pfuture'")

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
  (pattern
   step-pattern)
  "pfuture gets the first *max-pattern-yield-length* outputs from PATTERN, and then uses STEP-PATTERN to index that history.

See also: `phistory'")

;; (defun pfuture (pattern step-pattern)
;;   (make-instance 'pfuture
;;                  :pattern pattern
;;                  :step-pattern step-pattern))

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
  (pattern
   step-pattern
   (current-index :state t :initform 0))) ;; FIX: docstring needed

(defmethod as-pstream ((pattern pscratch))
  (make-instance 'pscratch-pstream
                 :pattern (as-pstream (slot-value pattern 'pattern))
                 :step-pattern (as-pstream (slot-value pattern 'step-pattern))))

(defmethod next ((pattern pscratch-pstream))
  (with-slots (current-index (src-pattern pattern) step-pattern) pattern
    (incf current-index (next step-pattern))
    (loop :while (>= current-index (length (slot-value src-pattern 'history)))
       :do
       (next src-pattern))
    (pstream-nth current-index src-pattern)))

;;; pif

(defpattern pif (pattern)
  (condition
   true
   false)
  "pif acts as an if statement for patterns. CONDITION is evaluated for each step, and if it's non-nil, the value of TRUE will be yielded, otherwise the value of FALSE will be. Note that TRUE and FALSE can be patterns, and if they are, they are only advanced in their respective cases, not for every step. Also note that pif will continue to advance even if CONDITION yields nil; pif only yields nil if TRUE or FALSE do.

Example: (next-n (pif (pseq '(t t nil nil nil)) (pseq '(1 2)) (pseq '(3 nil 4))) 5) ;=> (1 2 3 NIL 4)")

(defmethod next ((pattern pif-pstream))
  (with-slots (condition true false) pattern
    (if (next condition)
        (next true)
        (next false))))

;;; ptracker (FIX)

(defpattern ptracker (pattern)
  (head
   tracks)) ;; FIX: docstring needed

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
  (pattern
   arpeggiator
   (current-pattern-event :state t :initform nil)
   (current-arpeggiator-stream :state t :initform nil))
  "parp is an \"arpeggiator\"; each event yielded by PATTERN is bound to *event* and then the entirety of the ARPEGGIATOR pattern is yielded.

Example: (next-n (parp (pbind :foo (pseq '(1 2 3))) (pbind :bar (pseq '(4 5 6)))) 9) ;=>
((EVENT :FOO 1 :BAR 4) (EVENT :FOO 1 :BAR 5) (EVENT :FOO 1 :BAR 6)
 (EVENT :FOO 2 :BAR 4) (EVENT :FOO 2 :BAR 5) (EVENT :FOO 2 :BAR 6)
 (EVENT :FOO 3 :BAR 4) (EVENT :FOO 3 :BAR 5) (EVENT :FOO 3 :BAR 6))")

(defmethod as-pstream ((pattern parp))
  (let ((pstr (as-pstream (slot-value pattern 'pattern)))
        (arp (slot-value pattern 'arpeggiator)))
    (make-instance 'parp-pstream
                   :pattern pstr
                   :arpeggiator arp
                   :current-pattern-event (next pstr)
                   :current-arpeggiator-stream (as-pstream arp))))

(defmethod next ((pattern parp-pstream))
  (with-slots (arpeggiator current-pattern-event current-arpeggiator-stream) pattern
    (when (not (null current-pattern-event))
      (let ((nxt (let ((*event* current-pattern-event))
                   (next current-arpeggiator-stream))))
        (if (null nxt)
            (progn
              (setf current-pattern-event (next (slot-value pattern 'pattern)))
              (setf current-arpeggiator-stream (as-pstream arpeggiator))
              (next pattern))
            (combine-events current-pattern-event nxt))))))

;;; pfin

(defpattern pfin (pattern)
  (count
   pattern)
  "pfin yields up to COUNT outputs from PATTERN.

Example: (next-n (pfin 3 (pseq '(1 2 3) :inf)) 5) ;=> (1 2 3 NIL NIL)")

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
  (dur
   pattern
   (tolerance :default 0.001)
   (current-elapsed :state t :initform 0))
  "pfindur yields events from PATTERN until their total dur is within TOLERANCE of DUR, or greater than DUR.

Example: (next-n (pfindur 2 (pbind :dur 1 :foo (pseries))) 3) ;=> ((EVENT :DUR 1 :FOO 0) (EVENT :DUR 1 :FOO 1) NIL)")

(defmethod as-pstream ((pattern pfindur))
  (make-instance 'pfindur-pstream
                 :dur (next (slot-value pattern 'dur))
                 :pattern (as-pstream (slot-value pattern 'pattern))
                 :tolerance (next (slot-value pattern 'tolerance))))

(defmethod next ((pattern pfindur-pstream))
  (with-slots (dur tolerance current-elapsed) pattern
    (alexandria:when-let ((n-event (next (slot-value pattern 'pattern))))
      (when (< (round-up current-elapsed tolerance) dur)
        (prog1
            (if (> (round-up (+ (delta n-event) current-elapsed) tolerance) dur)
                (combine-events n-event (event :delta (- dur current-elapsed)))
                n-event)
          (incf current-elapsed (delta n-event)))))))

;;; pstutter

(defpattern pstutter (pattern)
  (n
   pattern
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t :initform 0))
  "pstutter yields each output from PATTERN N times before moving on to the next output from PATTERN.

Example: (next-n (pstutter (pseq '(3 2 1 0 2)) (pseries)) 9) ;=> (0 0 0 1 1 2 4 4 NIL)

See also: `pr', `pdurstutter'")

(defmethod next ((pattern pstutter-pstream))
  (with-slots (n current-value current-repeats-remaining) pattern
    (loop :while (and (not (null current-repeats-remaining))
                      (= 0 current-repeats-remaining))
       :do
       (setf current-value (next (slot-value pattern 'pattern)))
       (setf current-repeats-remaining (next n)))
    (when (not (null current-repeats-remaining))
      (decf-remaining pattern 'current-repeats-remaining)
      current-value)))

;;; pdurstutter

(defpattern pdurstutter (pattern)
  (n
   pattern
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t :initform 0))
  "pdurstutter yields each output from PATTERN N times, dividing it by N. If the output from PATTERN is an event, its dur is divided; if it's a number, the number itself is divided instead of being yielded directly.

Example: (next-n (pdurstutter (pseq '(3 2 1 0 2)) (pseq '(1 2 3 4 5))) 9) ;=> (1/3 1/3 1/3 1 1 3 5/2 5/2 NIL)

Example: (next-n (pdurstutter (pseq '(3 2 1 0 2)) (pbind :dur (pseq '(1 2 3 4 5)))) 9) ;=>
((EVENT :DUR 1/3) (EVENT :DUR 1/3) (EVENT :DUR 1/3) (EVENT :DUR 1)
 (EVENT :DUR 1) (EVENT :DUR 3) (EVENT :DUR 5/2) (EVENT :DUR 5/2) NIL)

See also: `pr', `pstutter'")

(defmethod next ((pattern pdurstutter-pstream))
  (with-slots (n current-value current-repeats-remaining) pattern
    (loop :while (and (not (null current-repeats-remaining))
                      (= 0 current-repeats-remaining))
       :do
       (setf current-repeats-remaining (next n))
       (let ((e (next (slot-value pattern 'pattern))))
         (when (and (not (null current-repeats-remaining))
                    (not (eq 0 current-repeats-remaining)))
           (setf current-value (ctypecase e
                                 (event (combine-events e (event :dur (/ (get-event-value e :dur) current-repeats-remaining))))
                                 (number (/ e current-repeats-remaining))
                                 (null nil))))))
    (when (not (null current-repeats-remaining))
      (decf-remaining pattern 'current-repeats-remaining)
      current-value)))

;;; pbeats (FIX)

(defpattern pbeats (pattern)
  ((start-beat :state t :initform 0))
  "pbeats yields the number of beats elapsed since its embedding in the parent stream.")

(defmethod as-pstream ((pattern pbeats))
  (make-instance 'pbeats-pstream
                 :start-beat (if (boundp '*clock*)
                                 (slot-value *clock* 'beats)
                                 0)))

(defmethod next ((pattern pbeats-pstream))
  (- (if (boundp '*clock*)
         (slot-value *clock* 'beats)
         0)
     (slot-value pattern 'start-beat))
  (slot-value pattern 'beats))

;;; ptime (FIX)

(defpattern ptime (pattern)
  ((start-time :state t :initform 0))
  "ptime yields the amount of time in seconds since its embedding in the parent stream.")

(defmethod as-pstream ((pattern ptime))
  (make-instance 'ptime-pstream
                 :start-time (get-internal-real-time)))

(defmethod next ((pattern ptime-pstream))
  (/ (- (get-internal-real-time)
        (slot-value pattern 'start-time))
     1000))

;;; psinosc

(defpattern psinosc (pattern)
  ((freq :default 1)
   (phase :default 0)
   (mul :default 1)
   (add :default 0)
   (last-beat-tracked :state t :initform nil)
   (current-phase :state t :initform 0)))

(defun psinosc (&optional (freq 1) (phase 0) (mul 1) (add 0))
  (make-instance 'psinosc
                 :freq freq
                 :phase phase
                 :mul mul
                 :add add))

(defmethod as-pstream ((pattern psinosc))
  (make-instance 'psinosc-pstream
                 :freq (slot-value pattern 'freq)
                 :phase (slot-value pattern 'phase)
                 :mul (slot-value pattern 'mul)
                 :add (slot-value pattern 'add)))

(defmethod next ((pattern psinosc-pstream))
  (with-slots (freq phase mul add last-beat-tracked current-phase) pattern
    (progn
      (when last-beat-tracked
        (incf current-phase (- (slot-value *clock* 'beats) last-beat-tracked)))
      (prog1
          (+ (next add) (* (next mul) (sin (+ current-phase phase))))
        (setf last-beat-tracked (slot-value *clock* 'beats))))))
