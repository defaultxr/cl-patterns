(in-package :cl-patterns)

;; NOTES:
;; FIX: make fork method, which copies the pstream and continues it from the same position in the copy and the original.
;; FIX: take each pattern's name out of its docstring, and make sure the first sentence in each docstring is a good concise description of the functionality.
;; FIX: use &key instead of &optional for defpattern

;;; pattern glue

(defun make-default-event ()
  (or *event* (event)))

(defun set-parents (pattern)
  "Loop through PATTERN's slots and set the \"parent\" slot of any patterns to this pattern."
  (labels ((set-parent (list parent)
             "Recurse through LIST, setting the parent of any pattern found to PARENT."
             (cond ((listp list)
                    (mapc (lambda (x) (set-parent x parent)) list))
                   ((typep list 'pattern)
                    (setf (slot-value list 'parent) parent)))))
    (loop :for slot :in (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern)))
       :unless (eql slot 'parent) ;; FIX: add tests for this!
       :do
         (when (slot-boundp pattern slot)
           (set-parent (slot-value pattern slot) pattern)))
    pattern))

(defmacro defpattern (name superclasses slots &optional documentation creation-function) ;; FIX: should warn if `set-parents' is not called in the creation-function.
  "Define a pattern. This macro automatically generates the pattern's class, its pstream class, and the function to create an instance of the pattern, and makes them external in the cl-patterns package.

NAME is the name of the pattern. Typically a word or two that describes its function, prefixed with p.

SUPERCLASSES is a list of superclasses of the pattern. Most patterns just subclass the 'pattern' class.

SLOTS is a list of slots that the pattern and pstreams derived from it have. Each slot can either be just a symbol, or a slot definition a la `defclass'. You can provide a default for the slot with the :default key, and you can set a slot as a state slot (which only appears in the pattern's pstream class) by setting the :state key to t.

DOCUMENTATION is a docstring describing the pattern. We recommend providing at least one example, and a \"See also\" section to refer to similar pattern classes.

CREATION-FUNCTION is an expression which will be inserted into the pattern creation function prior to initialization of the instance. Typically you'd use this for inserting `assert' statements, for example."
  (let* ((superclasses (or superclasses (list 'pattern)))
         (slots (mapcar #'alexandria:ensure-list slots))
         (name-pstream (intern (concatenate 'string (symbol-name name) "-PSTREAM") 'cl-patterns))
         (super-pstream (if (eql 'pattern (car superclasses))
                            'pstream
                            (intern (concatenate 'string (symbol-name (car superclasses)) "-PSTREAM") 'cl-patterns))))
    (labels ((desugar-slot (slot)
               "Convert a slot into something appropriate for defclass to handle."
               (let ((name (car slot))
                     (rest (cdr slot)))
                 (setf rest (alexandria:remove-from-plist rest :default :state))
                 (unless (position :initarg (keys rest))
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
                    :append (unless (state-slot-p slot)
                              (if (optional-slot-p slot)
                                  (prog1
                                      (append (if (not optional-used)
                                                  (list '&optional)
                                                  (list))
                                              (list (list (car slot) (getf (cdr slot) :default))))
                                    (setf optional-used t))
                                  (list (car slot)))))))
             (make-defun (pre-init)
               `(defun ,name ,(function-lambda-list slots)
                  ,documentation
                  ,@(when pre-init (list pre-init))
                  (set-parents
                   (make-instance ',name
                                  ,@(mapcan (lambda (i) (list (alexandria:make-keyword (car i)) (car i)))
                                            (remove-if #'state-slot-p slots))))))
             (add-doc-to-defun (sexp)
               (if (and (listp sexp)
                        (position (car sexp) (list 'defun 'defmacro))
                        (not (stringp (nth 3 sexp))))
                   (append (subseq sexp 0 3) (list documentation) (subseq sexp 3))
                   sexp)))
      `(progn
         (defclass ,name ,superclasses
           ,(mapcar #'desugar-slot (remove-if #'state-slot-p slots))
           ,@(when documentation
               `((:documentation ,documentation))))
         (defclass ,name-pstream (,name ,super-pstream)
           ,(mapcar #'desugar-slot (remove-if-not #'state-slot-p slots))
           (:documentation ,(format nil "pstream for `~a'." (string-downcase (symbol-name name)))))
         ,(let* ((gen-func-p (or (null creation-function)
                                 (and (listp creation-function)
                                      (position (car creation-function) (list 'assert)))))
                 (pre-init (when gen-func-p
                             creation-function)))
            (if gen-func-p
                (make-defun pre-init)
                (add-doc-to-defun creation-function)))
         (export '(,name ,name-pstream))))))

(defparameter *max-pattern-yield-length* 256
  "The default maximum number of events or values that will be used by functions like `next-n' or patterns like `pshift', in order to prevent hangs caused by infinite-length patterns.")

;;; pattern

(defclass pattern ()
  ((quant :initarg :quant :initform (list 1) :reader quant :documentation "A list of numbers representing when the pattern's pstream can start playing. The list takes the form (QUANT &OPTIONAL PHASE TIMING-OFFSET). For example, a quant of (4) means it can start on any beat on the clock that is divisible by 4. A quant of (4 2) means the pstream can start 2 beats after any beat divisible by 4. And a quant of (4 0 1) means that the pstream can start 1 second after any beat that is divisible by 4.")
   (parent :initarg :parent :initform nil :documentation "When a pattern is embedded in another pattern, the embedded pattern's parent slot points to the pattern it is embedded in.")
   (loop-p :initarg :loop-p :initform nil :accessor loop-p :documentation "Whether or not the pattern should loop when played.")
   (cleanup-functions :initarg :cleanup-functions :initform (list) :documentation "A list of functions that are run when the pattern ends or is stopped.")
   (pstream-count :initform 0 :reader pstream-count :documentation "The number of pstreams that have been made of this pattern."))
  (:documentation "Abstract pattern superclass."))

(defun all-patterns ()
  "Get a list of all defined patterns."
  (remove-if (lambda (s) (eql s 'pstream))
             (mapcar #'class-name (closer-mop:class-direct-subclasses (find-class 'pattern)))))

(defmethod (setf quant) (value (pattern pattern))
  (setf (slot-value pattern 'quant) (alexandria:ensure-list value)))

(defgeneric peek (pattern)
  (:documentation "\"Peek\" at the next value of a pstream, without advancing its current position.

See also: `next', `peek-n'"))

(defun peek-n (pstream n)
  "Peek at the next N results of a pstream, without advancing it forward in the process.

See also: `peek', `peek-upto-n'"
  (loop :repeat n
     :collect (peek pstream)))

(defun peek-upto-n (pstream &optional (n *max-pattern-yield-length*))
  "Peek at up to the next N results of a pstream, without advancing it forward in the process.

See also: `peek', `peek-n'"
  (loop :repeat n ;; FIX: just copy this function from next-upto-n (so :inf will work too)
     :for val = (peek pstream)
     :until (null val)
     :collect val))

(defgeneric next (pattern)
  (:documentation "Get the next value of a pstream, function, or other object, advancing the pstream forward in the process.

See also: `peek', `next-n', `next-upto-n'")
  (:method-combination pattern))

(defmethod next ((pattern t))
  pattern)

(defmethod next ((pattern pattern))
  (next (as-pstream pattern)))

(defmethod next ((pattern function))
  (funcall pattern))

(defun next-n (pattern n)
  "Get the next N results of a pattern stream, function, or other object, advancing the pattern stream forward N times in the process.

See also: `next', `next-upto-n'"
  (assert (integerp n) (n) "next-n's N argument must be an integer (getting infinity results from a pstream is not supported).")
  (let ((pstream (as-pstream pattern)))
    (loop :repeat n
       :collect (next pstream))))

(defun next-upto-n (pattern &optional (n *max-pattern-yield-length*))
  "Get a list of up to N results from PATTERN. If PATTERN ends after less than N values, then all of its results will be returned.

See also: `next', `next-upto-n'"
  (assert (numberp n) (n) "next-upto-n's N argument must be a number.")
  (let ((pstream (as-pstream pattern)))
    (loop
       :for number :from 0 :upto n
       :for val = (next pstream)
       :while (and (not (null val))
                   (or (eql :inf n)
                       (< number n)))
       :append (list val))))

(defgeneric events-in-range (pstream min max)
  (:documentation "Get all the events from PSTREAM whose start beat are MIN or greater, and less than MAX.

See also: `events-after-p'"))

(defmethod events-in-range ((pattern pattern) min max)
  (events-in-range (as-pstream pattern) min max))

;;; pstream

(defclass pstream (pattern)
  ((number :initform 0 :documentation "The number of outputs yielded from the pstream.") ;; FIX: maybe remove this and just use (length (slot-value pstream 'history)) instead?
   (pattern-stack :initform (list) :documentation "The stack of pattern pstreams embedded in this pstream.")
   (history :initform (list) :documentation "The history of outputs yielded by the pstream.") ;; FIX: should this be turned into an array instead?
   (beat :initform 0 :reader beat :documentation "The number of beats that have elapsed since the start of the pstream.")
   (start-beat :initarg :start-beat :initform nil :documentation "The beat number of the parent pstream when this pstream started.") ;; FIX: remove?
   (pstream-offset :initform 0 :documentation "The current offset in the pstream's history that `next' should read from. For example, if `peek' is used on the pstream once, this would be -1.")
   (pstream-count :initarg :pstream-count :reader pstream-count :documentation "How many times previously a pstream was made of this pstream's parent. For example, if it was the first time `as-pstream' was called on the pattern, this will be 0."))
  (:documentation "\"Pattern stream\". Keeps track of the current state of a pattern in process of yielding its outputs."))

(defmethod events-in-range ((pstream pstream) min max)
  (loop :while (and (<= (beat pstream) max)
                    (not (ended-p pstream)))
     :do (let ((next (next pstream)))
           (unless (typep next '(or null event))
             (error "events-in-range can only be used on event streams."))))
  (unless (typep (pstream-elt-future pstream 0) '(or null event))
    (error "events-in-range can only be used on event streams."))
  (loop :for i :in (slot-value pstream 'history)
     :if (and (not (null i))
              (>= (beat i) min)
              (< (beat i) max))
     :collect i
     :if (or (null i)
             (>= (beat i) max))
     :do (loop-finish)))

(defgeneric events-after-p (pstream beat)
  (:documentation "Whether or not there are events in PSTREAM after BEAT.

See also: `events-in-range'"))

(defmethod events-after-p ((pstream pstream) beat)
  (let ((last-beat 0))
    (loop
       :for idx :from 0
       :for event = (pstream-elt-future pstream idx)
       :for ebeat = (beat event)
       :if (null event)
       :do (return-from events-after-p nil)
       :if (< ebeat last-beat) ;; if :beat goes backwards, the pstream is considered to be ended.
       :do (progn
             (warn ":beat key decreased - pstream returning NIL.")
             (return-from events-after-p nil))
       :if (>= ebeat beat)
       :return t
       :else
       :do (setf last-beat ebeat))))

(defgeneric last-output (pstream)
  (:documentation "Returns the last output yielded by PSTREAM.

Example:

;; (defparameter *pstr* (as-pstream (pseq '(1 2 3) 1)))
;; (next *pstr*) ;; => 1
;; (last-output *pstr*) ;; => 1

See also: `ended-p'"))

(defmethod last-output ((pstream pstream))
  (with-slots (number pstream-offset) pstream
    (let ((idx (+ number pstream-offset)))
      (when (plusp idx)
        (pstream-elt pstream (- idx (if (ended-p pstream) 2 1)))))))

(defgeneric ended-p (pstream)
  (:documentation "Returns t if PSTREAM has no more outputs, or nil if outputs remain to be yielded.

Example:

;; (defparameter *pstr* (as-pstream (pseq '(1 2) 1)))
;; (next *pstr*) ;; => 1
;; (ended-p *pstr*) ;; => NIL
;; (next *pstr*) ;; => 2
;; (ended-p *pstr*) ;; => NIL
;; (next *pstr*) ;; => NIL
;; (ended-p *pstr*) ;; => T

See also: `last-output'"))

(defmethod ended-p ((pstream pstream))
  (with-slots (number pstream-offset) pstream
    (and (not (= 0 (+ number pstream-offset)))
         (null (pstream-elt pstream -1)))))

(defun value-remaining-p (value)
  "Returns true if VALUE represents that a pstream has outputs \"remaining\"; i.e. VALUE is a symbol (i.e. :inf), or a number greater than 0."
  (typecase value
    (null nil)
    (symbol (eql value :inf))
    (number (plusp value))
    (otherwise nil)))

(defun remaining-p (pattern &optional (repeats-key 'repeats) (remaining-key 'current-repeats-remaining))
  "Returns true if PATTERN's REMAINING-KEY slot value represents outputs \"remaining\" (see `value-remaining-p'). If PATTERN's REMAINING-KEY slot is unbound or 0, and REPEATS-KEY is not nil, then it is automatically set to the `next' of PATTERN's REPEATS-KEY slot. Then if that new value is 0 or nil, remaining-p returns nil. Otherwise, :reset is returned as a generalized true value and to indicate that `next' was called on PATTERN's REPEATS-KEY slot."
  (labels ((set-next ()
             (setf (slot-value pattern remaining-key) (next (slot-value pattern repeats-key)))
             (when (value-remaining-p (slot-value pattern remaining-key))
               :reset)))
    (if (not (slot-boundp pattern remaining-key))
        (set-next)
        (let ((rem-key (slot-value pattern remaining-key)))
          (typecase rem-key
            (null nil)
            (symbol (eql rem-key :inf))
            (number (if (plusp rem-key)
                        t
                        (set-next))) ;; if it's already set to 0, it was decf'd to 0 in the pattern, so we get the next one. if the next is 0, THEN we return nil.
            (otherwise nil))))))

(defun decf-remaining (pattern &optional (key 'current-repeats-remaining))
  "Decrease PATTERN's KEY value."
  (when (numberp (slot-value pattern key))
    (decf (slot-value pattern key))))

(defmethod peek ((pstream pstream))
  (with-slots (pstream-offset) pstream
    (let ((c-offset pstream-offset))
      (setf pstream-offset 0)
      (let ((nxt (next pstream)))
        (prog1
            nxt
          (when (typep nxt 'event)
            (decf (slot-value pstream 'beat) (event-value nxt :delta)))
          (setf pstream-offset (1- c-offset)))))))

(defmethod next :around ((pattern pstream))
  (labels ((pattern-embed (pattern embed)
             (assert (typep pattern 'pstream) (pattern))
             (push (as-pstream embed) (slot-value pattern 'pattern-stack)))
           (get-value-from-stack (pattern)
             (if (null (slot-value pattern 'pattern-stack))
                 (prog1
                     (get-value pattern)
                   (incf (slot-value pattern 'number)))
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
    (with-slots (pstream-offset) pattern
      (if (minusp pstream-offset)
          (prog1
              (pstream-elt pattern pstream-offset)
            (incf pstream-offset))
          (let ((result (get-value-from-stack pattern)))
            (when (and (typep result 'event)
                       (null (beat result))
                       (null (slot-value pattern 'parent)))
              (setf (beat result) (beat pattern)))
            (alexandria:appendf (slot-value pattern 'history) (list result))
            (when (typep result 'event)
              (incf (slot-value pattern 'beat) (event-value result :delta)))
            result)))))

(defgeneric as-pstream (thing)
  (:documentation "Return THING as a pstream object.

See also: `pattern-as-pstream'"))

(defun pattern-as-pstream (thing)
  "Like `as-pstream', but only converts THING to a pstream if it is a pattern."
  (if (typep thing 'pattern)
      (as-pstream thing)
      thing))

(defclass t-pstream (pstream)
  ((value :initarg :value :initform nil :documentation "The value that is yielded by the t-pstream."))
  (:documentation "Pattern stream object that yields its value only once."))

(defun make-t-pstream (value)
  (make-instance 't-pstream
                 :value value))

(defmethod print-object ((pstream t-pstream) stream)
  (format stream "#<~s ~s>" 't-pstream (slot-value pstream 'value)))

(defmethod as-pstream ((value t))
  (make-t-pstream value))

(defmethod as-pstream ((symbol symbol))
  (if (keywordp symbol)
      (or (when (pdef-ref symbol)
            (as-pstream (pdef symbol)))
          (alexandria:when-let ((event-value (event-value *event* symbol)))
            event-value)
          (make-t-pstream symbol))
      (make-t-pstream symbol)))

(defmethod next ((pattern t-pstream))
  (when (= 0 (slot-value pattern 'number))
    (let ((value (slot-value pattern 'value)))
      (if (functionp value)
          (funcall value)
          value))))

(defmethod as-pstream ((pattern pattern))
  (let ((slots (remove-if (lambda (x) (eql x 'parent)) (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern))))))
    (apply #'make-instance
           (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM") 'cl-patterns)
           (loop :for slot :in slots
              :collect (alexandria:make-keyword slot)
              :collect (pattern-as-pstream (slot-value pattern slot))))))

(defmethod as-pstream :around ((pattern pattern))
  (let ((pstream (call-next-method)))
    (setf (slot-value pstream 'pstream-count) (slot-value pattern 'pstream-count))
    (incf (slot-value pattern 'pstream-count))
    (set-parents pstream)
    pstream))

(defmethod as-pstream :around ((pstream pstream)) ;; prevent pstreams from being "re-converted" to pstreams
  pstream)

(define-condition pstream-out-of-range () ((index :initarg :index :reader pstream-elt-index))
  (:report (lambda (condition stream)
             (format stream "The pstream has insufficient history to index to element ~d." (pstream-elt-index condition)))))

(defun pstream-nth (n pstream)
  "Obsolete alias for `pstream-elt'."
  (warn "~s is deprecated; use ~s instead." 'pstream-nth 'pstream-elt)
  (pstream-elt pstream n))

(defun pstream-elt (pstream n)
  "Return the Nth element from PSTREAM's history. Does not automatically advance PSTREAM if N is out of range.

Example:

;; (let ((pstream (as-pstream (pseq '(1 2 3)))))
;;   (next-upto-n pstream)
;;   (pstream-elt pstream 2))
;;
;; => 3

See also: `pstream-elt-future', `phistory'"
  (assert (integerp n) (n))
  (unless (typep pstream 'pstream)
    (return-from pstream-elt (pstream-elt (as-pstream pstream) n)))
  (with-slots (history) pstream
    (if (>= n (length history))
        (error 'pstream-out-of-range :index n)
        (elt-wrap history n))))

(defun pstream-nth-future (n pstream)
  "Obsolete alias for `pstream-elt-future'."
  (warn "~s is deprecated; use ~s instead." 'pstream-nth-future 'pstream-elt-future)
  (pstream-elt-future pstream n))

(defun pstream-elt-future (pstream n)
  "Return the Nth element from PSTREAM's history, automatically advancing PSTREAM as necessary if the Nth element has not yet occurred.

When N is negative, NILs at the end of PSTREAM's history are not included in indexing, but NIL may be returned if the negative index points to a position prior to the first element in history. Be careful when using negative numbers for N, since infinite-length patterns will cause this function to never return.

Note that if the Nth element has not yet occurred, this function will advance the pstream, thus affecting what will be returned when `next' is called on the pstream. However, this behavior may change in the future.

Example:

;; (pstream-elt-future (as-pstream (pbind :foo (pseq '(1 2 3) 1))) -1)
;;
;; => (EVENT :FOO 3)

See also: `pstream-elt'"
  (assert (integerp n) (n))
  (unless (typep pstream 'pstream)
    (return-from pstream-elt-future (pstream-elt-future (as-pstream pstream) n)))
  (with-slots (history) pstream
    (flet ((should-advance ()
             (if (>= n 0)
                 (>= n (length history))
                 (not (position nil history)))))
      (loop :while (should-advance)
         :do (next pstream)) ;; FIX: use `peek' instead once `peek' is implemented.
      (if (>= n 0)
          (nth n history)
          (let ((sub-history (subseq history 0 (position nil history))))
            (nth (+ n (length sub-history)) sub-history))))))

(defgeneric parent-pattern (pattern)
  (:documentation "Get the containing pattern of PATTERN, or NIL if there isn't one.

See also: `parent-pbind'"))

(defmethod parent-pattern ((pattern pattern))
  (slot-value pattern 'parent))

(defgeneric parent-pbind (pattern)
  (:documentation "Get the containing pbind of PATTERN, or NIL if there isn't one.

See also: `parent-pattern'"))

(defmethod parent-pbind ((pattern pattern))
  (let ((par (parent-pattern pattern)))
    (loop :until (or (null par) (typep par 'pbind))
       :do (setf par (slot-value par 'parent)))
    par))

;;; pbind

(defparameter *pbind-special-init-keys* (list)
  "The list of special keys for pbind that alters it during its initialization.

See also: `define-pbind-special-init-key'")

(defparameter *pbind-special-wrap-keys* (list)
  "The list of special keys for pbind that causes the pbind to be replaced by another pattern during its initialization.

See also: `define-pbind-special-wrap-key'")

(defparameter *pbind-special-process-keys* (list)
  "The list of special keys for pbind that alter the outputs of the pbind.

See also: `define-pbind-special-process-key'")

(defclass pbind (pattern)
  ((pairs :initarg :pairs :initform (list) :documentation "The pattern pairs of the pbind; a plist mapping its keys to their values."))
  (:documentation "Please refer to the `pbind' documentation."))

(defun pbind (&rest pairs)
  "pbind yields events determined by its PAIRS, which are a list of keys and values. Each key corresponds to a key in the resulting events, and each value is treated as a pattern that is evaluated for each step of the pattern to generate the value for its key.

Example:

;; (next-n (pbind :foo (pseq '(1 2 3)) :bar :hello) 4)
;;
;; => ((EVENT :FOO 1 :BAR :HELLO) (EVENT :FOO 2 :BAR :HELLO) (EVENT :FOO 3 :BAR :HELLO) NIL)

See also: `pmono', `pb'"
  (assert (evenp (length pairs)) (pairs))
  (when (> (count :pdef (keys pairs)) 1)
    (warn "More than one :pdef key detected in pbind."))
  (let* ((res-pairs (list))
         (pattern-chain (list))
         (pattern (make-instance 'pbind)))
    (loop :for (key value) :on pairs :by #'cddr
       :do
         (progn
           (when (typep value 'pattern)
             (setf (slot-value value 'parent) pattern))
           (cond ((position key *pbind-special-init-keys*)
                  (alexandria:when-let ((result (funcall (getf *pbind-special-init-keys* key) value pattern)))
                    (alexandria:appendf res-pairs result)))
                 ((position key *pbind-special-wrap-keys*)
                  (unless (null res-pairs)
                    (setf (slot-value pattern 'pairs) res-pairs)
                    (setf res-pairs (list)))
                  (unless (null pattern-chain)
                    (setf pattern (apply #'pchain (append pattern-chain (list pattern))))
                    (setf pattern-chain (list)))
                  (setf pattern (funcall (getf *pbind-special-wrap-keys* key) value pattern)))
                 (t
                  (unless (typep pattern 'pbind)
                    (alexandria:appendf pattern-chain (list pattern))
                    (setf pattern (make-instance 'pbind)))
                  (alexandria:appendf res-pairs (list key (if (and (eql key :embed)
                                                                   (typep value 'symbol))
                                                              (pdef value)
                                                              value)))))))
    (unless (null res-pairs)
      (setf (slot-value pattern 'pairs) res-pairs))
    (alexandria:appendf pattern-chain (list pattern))
    (unless (alexandria:length= 1 pattern-chain)
      (setf pattern (apply #'pchain pattern-chain)))
    ;; process :quant key. ;; FIX: should this be applied to the pdef, or the pattern itself?
    (alexandria:when-let ((quant (getf pairs :quant)))
      (setf (quant pattern)
            (if (functionp quant)
                (funcall quant)
                quant)))
    ;; process :pdef key.
    (alexandria:when-let ((pdef-name (getf pairs :pdef)))
      (pdef pdef-name pattern))
    pattern))

(setf (documentation 'pbind 'type) (documentation 'pbind 'function))

(defmethod print-object ((pbind pbind) stream)
  (format stream "#<~s~{ ~s ~s~}>" 'pbind (slot-value pbind 'pairs)))

(defmacro pb (key &body pairs) ;; FIX: should automatically convert +, *, -, /, etc to their equivalent patterns.
  "pb is a convenience macro, wrapping the functionality of `pbind' and `pdef'. KEY is the name of the pattern (same as pbind's :pdef key or `pdef' itself), and PAIRS is the same as in regular pbind. If PAIRS is only one element, pb operates like `pdef', otherwise it operates like `pbind'.

See also: `pbind', `pdef'"
  (if (alexandria:length= 1 pairs)
      `(pdef ,key ,@pairs)
      `(pbind :pdef ,key ,@pairs)))

(defclass pbind-pstream (pbind pstream)
  ())

(defmethod print-object ((pbind pbind-pstream) stream) ;; FIX: use print-unreadable-object (here and anywhere else print-object is implemented like this)
  (format stream "#<~s~{ ~s ~s~}>" 'pbind-pstream (slot-value pbind 'pairs)))

(defun as-pstream-pairs (pairs)
  (let ((results (list)))
    (loop :for (key val) :on pairs :by #'cddr
       :do (alexandria:appendf results (list key (pattern-as-pstream val))))
    results))

(defmethod as-pstream ((pattern pbind))
  (let ((slots (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern)))))
    (apply #'make-instance
           (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM") 'cl-patterns)
           (loop :for slot :in slots
              :collect (alexandria:make-keyword slot)
              :if (eql :pairs (alexandria:make-keyword slot))
              :collect (as-pstream-pairs (slot-value pattern 'pairs))
              :else
              :collect (slot-value pattern slot)))))

(defmacro define-pbind-special-init-key (key &body body)
  "Define a special key for pbind that alters the pbind during its initialization, either by embedding a plist into its pattern-pairs or in another way. These functions are called once, when the pbind is created, and must return a plist if the key should embed values into the pbind pairs, or NIL if it should not."
  (let ((keyname (alexandria:make-keyword key)))
    `(setf (getf *pbind-special-init-keys* ,keyname)
           (lambda (value pattern)
             (declare (ignorable value pattern))
             ,@body))))

;; (define-pbind-special-init-key inst ;; FIX: this should be part of event so it will affect the event as well. maybe just rename to 'synth'?
;;   (list :instrument value))

(defmacro define-pbind-special-wrap-key (key &body body)
  "Define a special key for pbind that replaces the pbind with another pattern during the pbind's initialization. Each encapsulation key is run once on the pbind after it has been initialized, altering the type of pattern returned if the return value of the function is non-NIL."
  (let ((keyname (alexandria:make-keyword key)))
    `(setf (getf *pbind-special-wrap-keys* ,keyname)
           (lambda (value pattern)
             (declare (ignorable value pattern))
             ,@body))))

(define-pbind-special-wrap-key parp
  (parp pattern value))

(define-pbind-special-wrap-key pfin
  (pfin pattern value))

(define-pbind-special-wrap-key pfindur
  (pfindur pattern value))

(define-pbind-special-wrap-key psync
  (if (listp value)
      (destructuring-bind (quant &optional maxdur) value
        (psync pattern quant maxdur))
      (psync pattern value value)))

(define-pbind-special-wrap-key pdurstutter
  (pdurstutter pattern value))

(define-pbind-special-wrap-key pr
  (pr pattern value))

(define-pbind-special-wrap-key ptrace
  (if value
      (if (eql t value)
          (ptrace pattern)
          (ptrace pattern value))
      pattern))

(define-pbind-special-wrap-key pmeta
  (if (eql t value)
      (pmeta pattern)
      pattern))

(defmacro define-pbind-special-process-key (key &body body)
  "Define a special key for pbind that alters the pattern in a nonstandard way. These functions are called for each event created by the pbind and must return a list or event if the key should embed values into the event stream, or NIL if it should not."
  (let ((keyname (alexandria:make-keyword key)))
    `(setf (getf *pbind-special-process-keys* ,keyname)
           (lambda (value)
             ,@body))))

(define-pbind-special-process-key embed
  value)

(defmethod next ((pattern pbind-pstream))
  (labels ((pbind-accumulator (pairs)
             (let ((cadr (cadr pairs)))
               (when (and (typep cadr 'pstream)
                          (null (slot-value cadr 'start-beat)))
                 (setf (slot-value cadr 'start-beat) (beat pattern)))
               (let ((next-cadr (next cadr)))
                 (unless (null next-cadr)
                   (if (position (car pairs) (keys *pbind-special-process-keys*))
                       (setf *event* (combine-events *event*
                                                     (funcall (getf *pbind-special-process-keys* (car pairs)) next-cadr)))
                       (setf (event-value *event* (alexandria:ensure-symbol (car pairs) 'cl-patterns)) next-cadr))
                   (if (not (null (cddr pairs)))
                       (pbind-accumulator (cddr pairs))
                       *event*))))))
    (let ((*event* (make-default-event)))
      (setf (slot-value *event* '%beat) (beat pattern))
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
   (repeats :default :inf)
   (offset :default 0)
   (current-repeats-remaining :state t))
  "pseq yields values from LIST in the same order they were provided, repeating the whole list REPEATS times. OFFSET is the offset to index into the list.

Example:

;; (next-n (pseq '(5 6 7) 2) 7)
;;
;; => (5 6 7 5 6 7 NIL)
;;
;; (next-upto-n (pseq '(5 6 7) 2 1))
;;
;; => (6 7 5 6 7 5)

See also: `pser'")

(defmethod as-pstream ((pattern pseq))
  (with-slots (repeats list offset) pattern
    (make-instance 'pseq-pstream
                   :list (next list)
                   :repeats (as-pstream repeats)
                   :offset (pattern-as-pstream offset))))

(defmethod next ((pseq pseq-pstream))
  (with-slots (number list offset) pseq
    (when (and (plusp number)
               (= 0 (mod number (length list))))
      (decf-remaining pseq 'current-repeats-remaining))
    (alexandria:when-let ((off (next offset)))
      (when (and (remaining-p pseq)
                 list)
        (elt-wrap list (+ off number))))))

;;; pser

(defpattern pser (pattern)
  (list
   (length :default :inf)
   (offset :default 0)
   (current-repeats-remaining :state t)
   (current-index :state t))
  "pser yields values from LIST in the same order they were provided, returning a total of LENGTH values.

Example:

;; (next-n (pser '(5 6 7) 2) 3)
;;
;; => (5 6 NIL)

See also: `pseq'")

(defmethod as-pstream ((pser pser))
  (with-slots (list length offset) pser
    (make-instance 'pser-pstream
                   :list (next list)
                   :length (as-pstream length)
                   :offset (pattern-as-pstream offset))))

(defmethod next ((pser pser-pstream))
  (with-slots (list offset current-index) pser
    (alexandria:when-let ((remaining (remaining-p pser 'length))
                          (off (next offset)))
      (decf-remaining pser 'current-repeats-remaining)
      (when (eql :reset remaining)
        (setf current-index 0))
      (prog1
          (elt-wrap list (+ off current-index))
        (incf current-index)))))

;;; pk

(defpattern pk (pattern)
  (key
   (default :default 1))
  "pk returns the value of KEY in the current *event* context, returning DEFAULT if that value is nil.

Example:

;; (next-n (pbind :foo (pseq '(1 2 3)) :bar (pk :foo)) 3)
;;
;; => ((EVENT :FOO 1 :BAR 1) (EVENT :FOO 2 :BAR 2) (EVENT :FOO 3 :BAR 3))

See also: `event-value', `*event*'")

(defmethod as-pstream ((pattern pk))
  (with-slots (key default) pattern
    (make-instance 'pk-pstream
                   :key key
                   :default default)))

(defmethod next ((pk pk-pstream))
  (with-slots (key default) pk
    (or (event-value *event* key)
        (if (string= :number key)
            (slot-value pk 'number)
            default))))

;;; prand

(defpattern prand (pattern)
  (list
   (length :default :inf)
   (current-repeats-remaining :state t))
  "prand returns a random value from LIST, returning a total of LENGTH values.

Example:

;; (next-n (prand '(1 2 3) 5) 6)
;;
;; => (3 2 2 1 1 NIL)

See also: `pxrand', `pwrand', `pwxrand'")

(defmethod as-pstream ((pattern prand))
  (with-slots (list length) pattern
    (make-instance 'prand-pstream
                   :list (next list)
                   :length (as-pstream length))))

(defmethod next ((pattern prand-pstream))
  (when (remaining-p pattern 'length)
    (decf-remaining pattern 'current-repeats-remaining)
    (alexandria:random-elt (slot-value pattern 'list))))

;;; pxrand

(defpattern pxrand (pattern)
  (list
   (length :default :inf)
   (last-result :state t :initform nil)
   (current-repeats-remaining :state t))
  "pxrand returns a random value from LIST that is not equal to the last result, returning a total of LENGTH values.

See also: `prand', `pwrand', `pwxrand'"
  (assert (> (length (remove-duplicates list)) 1) (list)))

(defmethod as-pstream ((pattern pxrand))
  (with-slots (list length) pattern
    (make-instance 'pxrand-pstream
                   :list (next list)
                   :length (as-pstream length))))

(defmethod next ((pattern pxrand-pstream))
  (with-slots (list last-result) pattern
    (when (remaining-p pattern 'length)
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
    (make-instance 'pwrand-pstream
                   :list (next list)
                   :weights (next weights)
                   :length (as-pstream length))))

(defmethod next ((pattern pwrand-pstream))
  (with-slots (list weights) pattern
    (when (remaining-p pattern 'length)
      (decf-remaining pattern 'current-repeats-remaining)
      (let* ((cweights (cumulative-list (normalized-sum weights)))
             (num (random 1.0)))
        (nth (index-of-greater-than num cweights) list)))))

;;; pwxrand

(defpattern pwxrand (pattern)
  (list
   (weights :default (make-list (length list) :initial-element 1))
   (length :default :inf)
   (current-repeats-remaining :state t))
  "pwxrand returns a random element from LIST weighted by respective values from WEIGHTS, for a total of LENGTH values, never repeating the same value twice in a row."
  (assert (> (length (remove-duplicates list)) 1) (list)))

(defmethod as-pstream ((pattern pwxrand))
  (with-slots (list weights length) pattern
    (make-instance 'pwxrand-pstream
                   :list list
                   :weights weights
                   :length (as-pstream length))))

(defmethod next ((pattern pwxrand-pstream))
  (with-slots (list weights) pattern
    (when (remaining-p pattern 'length)
      (decf-remaining pattern 'current-repeats-remaining)
      (labels ((get-next ()
                 (let* ((cweights (cumulative-list (normalized-sum weights)))
                        (num (random 1.0))
                        (res (nth (index-of-greater-than num cweights) list)))
                   (unless (null (slot-value pattern 'history))
                     (loop :while (eql res (pstream-elt pattern -1))
                        :do (setf res (get-next))))
                   res)))
        (get-next)))))

;;; pfunc
;; NOTE: This implementation doesn't provide the event as an argument to the function like the SuperCollider implementation does.
;; Instead, access the event using the special variable *event*.

(defpattern pfunc (pattern)
  (func
   (length :default :inf)
   (current-repeats-remaining :state t))
  "pfunc evaluates the function FUNC each time its next output is requested, and yields the result of that function. It does this a maximum of LENGTH times. Note that the current event of the parent pattern is still accessible via the `*event*' special variable.

Example:

;; (next-upto-n (pfunc (lambda () (random 10)) 4))
;; => ((5 2 8 9))

See also: `pf', `pnary'"
  (assert (typep func 'function) (func)))

(defmethod as-pstream ((pattern pfunc))
  (with-slots (func length) pattern
    (make-instance 'pfunc-pstream
                   :func func
                   :length (as-pstream length))))

(defmethod next ((pattern pfunc-pstream))
  (when (remaining-p pattern 'length)
    (decf-remaining pattern 'current-repeats-remaining)
    (funcall (slot-value pattern 'func))))

;;; pf

(defmacro pf (&body body)
  "Convenience macro for `pfunc' that automatically wraps BODY in a lambda."
  `(pfunc (lambda () ,@body)))

;;; pr

(defpattern pr (pattern)
  (pattern
   (repeats :default :inf)
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t))
  "pr yields a value from PATTERN REPEATS times before moving on to the next value from PATTERN.

See also: `pstutter', `pdurstutter', `parp'")

(defmethod as-pstream ((pattern pr))
  (with-slots (pattern repeats) pattern
    (make-instance 'pr-pstream
                   :pattern (as-pstream pattern)
                   :repeats (pattern-as-pstream repeats))))

(defmethod next ((pr pr-pstream))
  (with-slots (pattern repeats current-value current-repeats-remaining) pr
    (loop :while (or (not (slot-boundp pr 'current-repeats-remaining))
                     (and (not (null current-repeats-remaining))
                          (not (null current-value))
                          (not (value-remaining-p current-repeats-remaining))))
       :do
         (setf current-value (next pattern))
         (when current-value
           (setf current-repeats-remaining
                 (let ((*event* (if (typep current-value 'event)
                                    (if (null *event*)
                                        current-value
                                        (combine-events *event* current-value))
                                    *event*)))
                   (if (typep repeats 'function)
                       (let ((fle (function-lambda-expression repeats)))
                         (if fle
                             (if (alexandria:length= 0 (cadr fle))
                                 (funcall repeats)
                                 (funcall repeats current-value))
                             (handler-case
                                 (funcall repeats current-value)
                               #+sbcl (sb-int:simple-program-error (e) ;; FIX: need to add stuff for other implementations or generalize it somehow.
                                        (declare (ignore e))
                                        (funcall repeats)))))
                       (next repeats))))))
    (when (value-remaining-p current-repeats-remaining)
      (decf-remaining pr 'current-repeats-remaining)
      current-value)))

;;; pdef
;; FIX: need to implement 'reset' method, and 'condition slot (for switching source patterns based on a condition rather than a quant time).

;; (pdef-ref KEY) returns the pdef's plist which holds the pattern, pstream, task, etc.
;; (pdef-ref-get KEY :task) returns the task associated with (pdef KEY).
;; (pdef-ref-set KEY :pattern PAT) sets the pattern for (pdef KEY) to PAT.

(defun pdef-ref-get (pdef-key key)
  "Get PDEF-KEY's KEY value from its plist."
  (getf (pdef-ref pdef-key) (alexandria:make-keyword key)))

(defun pdef-ref-set (pdef-key key pattern)
  "Set PDEF-KEY's KEY in its plist to PATTERN."
  (pdef-ref pdef-key (plist-set (pdef-ref pdef-key) (alexandria:make-keyword key) pattern)))

(defpattern pdef (pattern)
  ((key :reader pdef-key)
   (current-pstream :state t)
   (loop-p :initform t))
  "pdef defines a named pattern, with KEY being the name of the pattern and PATTERN the pattern itself. Named patterns are stored by name in a global dictionary and can be referred back to by calling `pdef' without supplying PATTERN. The global dictionary also keeps track of the pdef's pstream when `play' is called on it. If a pdef is redefined while it is currently being played, the changes won't be audible until either PATTERN ends, or the pdef's `quant' time is reached. Note that, unlike bare patterns, pdefs loop by default when played (`loop-p')."
  (defun pdef (key &optional (pattern nil pattern-supplied-p))
    (when pattern-supplied-p
      (pdef-ref-set key :pattern pattern))
    (make-instance 'pdef
                   :key key)))

(create-global-dictionary pdef)

(defun all-pdefs ()
  "Get a list of all pdefs."
  (keys *pdef-dictionary*))

(defmethod pdef-pattern ((object pdef))
  (pdef-ref-get (pdef-key object) :pattern))

(defmethod quant ((pdef pdef))
  (quant (pdef-pattern pdef)))

(defmethod as-pstream ((pdef pdef))
  (with-slots (key) pdef
    (if (null (pdef-ref key))
        (error "No pdef with the key ~s defined." key)
        (make-instance 'pdef-pstream
                       :key key
                       :current-pstream (as-pstream (pdef-pattern pdef))))))

(defmethod next ((pattern pdef-pstream))
  (next (slot-value pattern 'current-pstream)))

;;; plazy

(defpattern plazy (pattern)
  (func
   (current-pstream :state t :initform nil))
  "plazy funcalls FUNC which should return a pattern, which is then yielded from until its end, at which point FUNC is re-evaluated to generate the next pattern.")

(defmethod as-pstream ((plazy plazy))
  (with-slots (func) plazy
    (make-instance 'plazy-pstream
                   :func func)))

(defmethod next ((pattern plazy-pstream))
  (with-slots (current-pstream func) pattern
    (let ((nv (next current-pstream)))
      (loop :repeat 5
         :while (or (null current-pstream)
                    (null nv))
         :do
           (setf current-pstream (as-pstream (funcall func)))
           (setf nv (next current-pstream)))
      nv)))

;;; plazyn

(defpattern plazyn (pattern)
  (func
   (repeats :default :inf)
   (current-pstream :state t :initform nil)
   (current-repeats-remaining :state t :initform nil))
  "plazyn funcalls FUNC which should return a pattern, which is then yielded from until its end, at which point FUNC is re-evaluated to generate the next pattern. The pattern is generated a total of REPEATS times.")

(defmethod as-pstream ((pattern plazyn))
  (with-slots (func repeats) pattern
    (make-instance 'plazyn-pstream
                   :func func
                   :repeats (as-pstream repeats))))

(defmethod next ((pattern plazyn-pstream))
  (with-slots (func repeats current-pstream current-repeats-remaining) pattern
    (labels ((maybe-funcall ()
               (when (remaining-p pattern)
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

;;; pshift
;; shift a pattern N forward or backward, wrapping around

(defun pshift (pattern shift &optional (max-yield *max-pattern-yield-length*)) ;; FIX: don't use pseq internally, and make it possible for 'shift' to be a pattern
  (pseq (alexandria:rotate (next-upto-n pattern max-yield) shift)))

;;; pn

(defpattern pn (pattern)
  (pattern
   (repeats :default :inf)
   (current-repeats-remaining :state t)
   (current-pstream :state t :initform nil))
  "pn embeds the full PATTERN into the pstream REPEATS times.")

(defmethod as-pstream ((pn pn)) ;; need this so that PATTERN won't be automatically converted to a pstream when the pn is.
  (with-slots (pattern repeats) pn
    (make-instance 'pn-pstream
                   :pattern pattern
                   :repeats (as-pstream repeats))))

(defmethod next ((pn pn-pstream))
  (with-slots (pattern current-pstream) pn
    (alexandria:when-let ((rem (remaining-p pn)))
      (when (eql :reset rem)
        (setf current-pstream (as-pstream pattern)))
      (let ((nv (next current-pstream)))
        (loop :while (and (null nv) rem) :do
             (decf-remaining pn 'current-repeats-remaining)
             (setf rem (remaining-p pn))
             (setf current-pstream (as-pstream pattern))
             (setf nv (next current-pstream)))
        (when rem
          nv)))))

;;; pshuf

(defpattern pshuf (pattern)
  (list
   (repeats :default :inf)
   (shuffled-list :state t)
   (current-repeats-remaining :state t))
  "pshuf shuffles LIST, then yields each item from the shuffled list, repeating the list REPEATS times.

Example:

;; (next-upto-n (pshuf '(1 2 3) 2))
;;
;; => (3 1 2 3 1 2)

See also: `prand'")

(defmethod as-pstream ((pattern pshuf))
  (with-slots (list repeats) pattern
    (let ((list (typecase list
                  (pattern (next-upto-n list))
                  (function (funcall list))
                  (list list))))
      (make-instance 'pshuf-pstream
                     :list (next list)
                     :repeats (as-pstream repeats)))))

(defmethod next ((pattern pshuf-pstream))
  (with-slots (list number shuffled-list) pattern
    (when (and (= 0 (mod number (length list)))
               (plusp number))
      (decf-remaining pattern 'current-repeats-remaining))
    (alexandria:when-let ((rem (remaining-p pattern)))
      (when (eql :reset rem)
        (setf shuffled-list (alexandria:shuffle (copy-list list)))) ;; alexandria:shuffle destructively modifies the list, so we use copy-list in case the user provided a quoted list as input.
      (nth (mod number (length shuffled-list))
           shuffled-list))))

;;; pwhite

(defpattern pwhite (pattern)
  ((lo :default 0)
   (hi :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t))
  "pwhite yields LENGTH random numbers between LO and HI.")

(defmethod as-pstream ((pattern pwhite))
  (with-slots (lo hi length) pattern
    (make-instance 'pwhite-pstream
                   :lo (pattern-as-pstream lo)
                   :hi (pattern-as-pstream hi)
                   :length (as-pstream length))))

(defmethod next ((pattern pwhite-pstream))
  (with-slots (lo hi) pattern
    (when (remaining-p pattern 'length)
      (decf-remaining pattern 'current-repeats-remaining)
      (alexandria:when-let ((nlo (next lo))
                            (nhi (next hi)))
        (random-range nlo nhi)))))

;;; pbrown

(defpattern pbrown (pattern)
  ((lo :default 0)
   (hi :default 1)
   (step :default 0.125)
   (length :default :inf)
   (current-repeats-remaining :state t)
   (current-value :state t :initform nil))
  "pbrown implements brownian motion, yielding LENGTH values between LO and HI, each value a maximum of STEP away from the previous value.")

(defmethod as-pstream ((pattern pbrown))
  (with-slots (lo hi step length) pattern
    (make-instance 'pbrown-pstream
                   :lo (pattern-as-pstream lo)
                   :hi (pattern-as-pstream hi)
                   :step (pattern-as-pstream step)
                   :length (as-pstream length))))

(defmethod next ((pattern pbrown-pstream))
  (when (remaining-p pattern 'length)
    (decf-remaining pattern 'current-repeats-remaining)
    (with-slots (lo hi step current-value) pattern
      (alexandria:when-let ((nlo (next lo))
                            (nhi (next hi))
                            (nstep (next step)))
        (when (null current-value)
          (setf current-value (random-range nlo nhi)))
        (incf current-value (random-range (* -1 nstep) nstep))
        (setf current-value (alexandria:clamp current-value nlo nhi))))))

;;; pexprand

(defpattern pexprand (pattern)
  ((lo :default 0.0001)
   (hi :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t))
  "pexprand yields LENGTH exponentially-distributed random numbers between LO and HI.")

(defmethod as-pstream ((pattern pexprand))
  (with-slots (lo hi length) pattern
    (make-instance 'pexprand-pstream
                   :lo (pattern-as-pstream lo)
                   :hi (pattern-as-pstream hi)
                   :length (as-pstream length))))

(defmethod next ((pattern pexprand-pstream))
  (with-slots (lo hi) pattern
    (when (remaining-p pattern 'length)
      (decf-remaining pattern 'current-repeats-remaining)
      (alexandria:when-let ((nlo (next lo))
                            (nhi (next hi)))
        (exponential-random-range nlo nhi)))))

;;; pseries

(defpattern pseries (pattern)
  ((start :default 0)
   (step :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t)
   (current-value :state t))
  "pseries yields START, and then each subsequent value is the previous value plus STEP, for a total of LENGTH values yielded.")

(defmethod as-pstream ((pattern pseries))
  (with-slots (start step length) pattern
    (make-instance 'pseries-pstream
                   :start (pattern-as-pstream start)
                   :step (pattern-as-pstream step)
                   :length (as-pstream length))))

(defmethod next ((pattern pseries-pstream))
  (with-slots (start step current-value) pattern
    (unless (slot-boundp pattern 'current-value)
      (setf current-value (next start)))
    (when (and (remaining-p pattern 'length)
               current-value)
      (decf-remaining pattern 'current-repeats-remaining)
      (let ((nxt (next step)))
        (prog1
            current-value
          (if (numberp nxt)
              (incf current-value nxt) ;; FIX: current-value should be CURRENT value, not the next one! also write tests for this!
              (setf current-value nil)))))))

;;; pgeom

(defpattern pgeom (pattern)
  ((start :default 1)
   (grow :default 2)
   (length :default :inf)
   (current-repeats-remaining :state t)
   (current-value :state t))
  "pgeom yields START, and then each subsequent value is the previous value times GROW, for a total of LENGTH values yielded.")

(defmethod as-pstream ((pattern pgeom))
  (with-slots (start grow length) pattern
    (make-instance 'pgeom-pstream
                   :start (pattern-as-pstream start)
                   :grow (pattern-as-pstream grow)
                   :length (as-pstream length))))

(defmethod next ((pattern pgeom-pstream))
  (with-slots (start grow current-value) pattern
    (unless (slot-boundp pattern 'current-value)
      (setf current-value (next start)))
    (when (remaining-p pattern 'length)
      (decf-remaining pattern 'current-repeats-remaining)
      (if (= 0 (slot-value pattern 'number))
          current-value
          (alexandria:when-let ((n (next grow)))
            (setf current-value (* current-value n)))))))

;;; ptrace

(defpattern ptrace (pattern)
  (pattern
   (key :default nil)
   (stream :default t)
   (prefix :default ""))
  "ptrace prints to STREAM the PREFIX and then the value of KEY for each event yielded by PATTERN, or the whole event or value if KEY is not provided. ptrace yields everything from the source PATTERN unaffected.")

(defmethod as-pstream ((ptrace ptrace))
  (with-slots (pattern key stream prefix) ptrace
    (make-instance 'ptrace-pstream
                   :pattern (as-pstream pattern)
                   :key (pattern-as-pstream key)
                   :stream stream
                   :prefix (pattern-as-pstream prefix))))

(defmethod next ((ptrace ptrace-pstream))
  (with-slots (pattern key stream prefix) ptrace
    (let* ((n (next pattern))
           (result (if (and (not (null key))
                            (not (null n)))
                       (event-value n key)
                       n)))
      (format stream "~a~a~%" prefix result)
      n)))

;;; ppatlace

(defpattern ppatlace (pattern)
  (list
   (repeats :default 1)
   (current-repeats-remaining :state t))
  "ppatlace yields each value from LIST in sequence. If the value is a pattern, one value is yielded from that pattern before moving on to the next item in LIST. The second time around the LIST, the second value yielded from each pattern in LIST will be yielded instead. If one of the patterns embedded in LIST ends sooner than the others, it is simply removed and the ppatlace continues to yield from the rest of the LIST. The entire LIST is yielded through a total of REPEATS times.")

(defmethod as-pstream ((pattern ppatlace))
  (with-slots (repeats list) pattern
    (make-instance 'ppatlace-pstream
                   :list (mapcar #'pattern-as-pstream list)
                   :repeats (as-pstream repeats))))

(defmethod next ((pattern ppatlace-pstream))
  (with-slots (number list) pattern
    (when (and (not (= number 0))
               (= 0 (mod number (length list))))
      (decf-remaining pattern 'current-repeats-remaining))
    (when (if (plusp number)
              (and (not (null (pstream-elt pattern -1)))
                   (remaining-p pattern))
              (remaining-p pattern))
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
  "pnary yields the result of applying OPERATOR to each value yielded by each pattern in PATTERNS.

See also: `pfunc'"
  (defun pnary (operator &rest patterns)
    (set-parents
     (make-instance 'pnary
                    :operator operator
                    :patterns patterns))))

(defmethod as-pstream ((pattern pnary))
  (with-slots (operator patterns) pattern
    (make-instance 'pnary-pstream
                   :operator (pattern-as-pstream operator)
                   :patterns (mapcar #'pattern-as-pstream patterns))))

(defmethod next ((pattern pnary-pstream))
  (with-slots (operator patterns) pattern
    (let ((op (if (typep operator 'pstream)
                  (next operator)
                  operator))
          (nexts (mapcar #'next patterns)))
      (unless (or (position nil nexts)
                  (null op))
        (apply op nexts)))))

;; show the arguments of the function being called in pnary
#+swank
(defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'pnary)) argument-forms)
  (swank::compute-enriched-decoded-arglist 'apply argument-forms))

(defun p+ (&rest numbers)
  "p+ adds NUMBERS, where NUMBERS can be any object that responds to the `next' method. This function is simply a shortcut for (apply #'pnary #'+ numbers)."
  (apply #'pnary #'+ numbers))

(defun p- (&rest numbers)
  "p- subtracts NUMBERS, where NUMBERS can be any object that responds to the `next' method. This function is simply a shortcut for (apply #'pnary #'- numbers)."
  (apply #'pnary #'- numbers))

(defun p* (&rest numbers)
  "p* multiplies NUMBERS, where NUMBERS can be any object that responds to the `next' method. This function is simply a shortcut for (apply #'pnary #'* numbers)."
  (apply #'pnary #'* numbers))

(defun p/ (&rest numbers)
  "p/ divides NUMBERS, where NUMBERS can be any object that responds to the `next' method. This function is simply a shortcut for (apply #'pnary #'/ numbers)."
  (apply #'pnary #'/ numbers))

;;; pslide

(defpattern pslide (pattern)
  (list
   (repeats :default 1)
   (len :default 3)
   (step :default 1)
   (start :default 0)
   (wrap-at-end :default t)
   (current-repeats-remaining :state t)
   (current-repeats :state t :initform nil)
   (remaining-current-segment :state t :initform nil)
   (current-value :state t :initform nil))
  "pslide slides across sections LIST. REPEATS is the total number of sections to output, LEN the length of the section. STEP the number to increment the start index by after each section, and START is the initial index that the first section starts from. WRAP-AT-END, when true, means that an index outside of the list will wrap around. When false, indexes outside of the list will return nils.

Example:

;; (next-upto-n (pslide (list 0 1 2 3 4 5 6) 3 3 2 1 t))
;;
;; => (1 2 3 3 4 5 5 6 0)

See also: `pscratch'")

(defmethod as-pstream ((pattern pslide))
  (with-slots (list repeats len step start wrap-at-end) pattern
    (make-instance 'pslide-pstream
                   :list (next list)
                   :repeats (pattern-as-pstream repeats)
                   :len (pattern-as-pstream len)
                   :step (pattern-as-pstream step)
                   :start (next start)
                   :wrap-at-end (next wrap-at-end)
                   :current-repeats 0
                   :remaining-current-segment len
                   :current-value start)))

(defmethod next ((pattern pslide-pstream))
  (with-slots (list repeats len step start wrap-at-end current-repeats-remaining current-repeats remaining-current-segment current-value) pattern
    (labels ((get-next ()
               (if (and (not wrap-at-end)
                        (minusp current-value))
                   nil
                   (funcall (if wrap-at-end #'elt-wrap #'elt) list current-value))))
      (unless (slot-boundp pattern 'current-repeats-remaining)
        (setf current-repeats-remaining (next repeats)))
      (when (value-remaining-p current-repeats-remaining)
        (if (value-remaining-p remaining-current-segment)
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

(defpattern phistory (pattern)
  (pattern
   step-pattern)
  "phistory refers back to PATTERN's history, yielding the value at the index provided by STEP-PATTERN. Note that PATTERN is still advanced once per event, and if STEP-PATTERN yields a number pointing to an event in PATTERN that hasn't been yielded yet (i.e. if PATTERN has only advanced once but STEP-PATTERN yields 3 for its output), it will return nil.

Example:

;; (next-n (phistory (pseries) (pseq '(0 2 1))) 3)
;;
;; => (0 NIL 1)

See also: `pfuture', `pscratch'")

(defmethod as-pstream ((phistory phistory))
  (with-slots (pattern step-pattern) phistory
    (make-instance 'phistory-pstream
                   :pattern (as-pstream pattern)
                   :step-pattern (as-pstream step-pattern))))

(defmethod next ((pstream phistory-pstream))
  (with-slots (pattern step-pattern) pstream
    (alexandria:when-let ((next-step (next step-pattern)))
      (next pattern)
      (handler-case (pstream-elt pattern next-step)
        (pstream-out-of-range ()
          nil)))))

;;; pfuture (FIX - remove)
;; like phistory, but immediately slurps the whole pstream and then allows you to index from its history instead of advancing the source pattern one at a time as normal
;; phistory is kept around because some (time-based) patterns may need to be advanced normally rather than all at once (though it might be kind of a bad idea to try to use phistory, pfuture, or pscratch on those kinds of patterns)
;; FIX: maybe deprecate this and just integrate its functionality into phistory somehow?
;; FIX: use `pstream-elt-future' to get the values for this instead of slurping the whole pstream right away.

(defpattern pfuture (phistory)
  (pattern
   step-pattern)
  "DEPRECATED - just use `phistory' instead.

pfuture gets the first *max-pattern-yield-length* outputs from PATTERN, and then uses STEP-PATTERN to index that history.

See also: `phistory', `pscratch'")

;; (defun pfuture (pattern step-pattern)
;;   (make-instance 'pfuture
;;                  :pattern pattern
;;                  :step-pattern step-pattern))

(defmethod as-pstream ((pfuture pfuture))
  (with-slots (pattern step-pattern) pfuture
    (let ((source-pstream (as-pstream pattern)))
      (next-upto-n source-pstream)
      (make-instance 'pfuture-pstream
                     :pattern source-pstream
                     :step-pattern (as-pstream step-pattern)))))

;;; pscratch
;;
;; NOTE: pscratch's mechanism seems to be slightly different:
;; supercollider:
;; > Pscratch(Pseries(0, 1), Pseq([1, 1, 1, -3], inf)).asStream.nextN(12)
;; [ 0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 0 ]
;;
;; lisp:
;; > (next-n (pscratch (pseries 0 1) (pseq (list 1 1 1 -3) :inf)) 12)
;; (1 2 3 0 1 2 3 0 1 2 3 0)
;; FIX: document this in sc-differences.org

(defpattern pscratch (pattern)
  (pattern
   step-pattern
   (current-index :state t :initform 0))
  "pscratch \"scratches\" across the values yielded by a pstream. PATTERN is the source pattern, and STEP-PATTERN determines the increment of the index into the pstream history.

Example:

;; (next-upto-n (pscratch (pseries) (pseq '(0 1 1 -1 2))))
;;
;; => (0 1 2 1 3)

See also: `phistory'")

(defmethod as-pstream ((pscratch pscratch))
  (with-slots (pattern step-pattern) pscratch
    (make-instance 'pscratch-pstream
                   :pattern (as-pstream pattern)
                   :step-pattern (pattern-as-pstream step-pattern))))

(defmethod next ((pscratch pscratch-pstream))
  (with-slots (pattern step-pattern current-index) pscratch
    (alexandria:when-let ((nxt (next step-pattern)))
      (incf current-index nxt) ;; FIX: should we clamp this so negative indexes don't happen?
      (pstream-elt-future pattern current-index))))

;;; pif

(defpattern pif (pattern)
  (condition
   true
   false)
  "pif acts as an if statement for patterns. CONDITION is evaluated for each step, and if it's non-nil, the value of TRUE will be yielded, otherwise the value of FALSE will be. Note that TRUE and FALSE can be patterns, and if they are, they are only advanced in their respective cases, not for every step. Also note that pif will continue to advance even if CONDITION yields nil; pif only yields nil if TRUE or FALSE do.

Example:

;; (next-n (pif (pseq '(t t nil nil nil)) (pseq '(1 2)) (pseq '(3 nil 4))) 5)
;;
;; => (1 2 3 NIL 4)")

(defmethod as-pstream ((pif pif))
  (with-slots (condition true false) pif
    (make-instance 'pif-pstream
                   :condition (pattern-as-pstream condition)
                   :true (pattern-as-pstream true)
                   :false (pattern-as-pstream false))))

(defmethod next ((pif pif-pstream))
  (with-slots (condition true false) pif
    (if (next condition)
        (next true)
        (next false))))

;;; parp

(defpattern parp (pattern)
  (pattern
   arpeggiator
   (current-pattern-event :state t :initform nil)
   (current-arpeggiator-stream :state t :initform nil))
  "parp is an \"arpeggiator\"; each event yielded by PATTERN is bound to *event* and then the entirety of the ARPEGGIATOR pattern is yielded.

Example:

;; (next-n (parp (pbind :foo (pseq '(1 2 3))) (pbind :bar (pseq '(4 5 6)))) 9)
;;
;; => ((EVENT :FOO 1 :BAR 4) (EVENT :FOO 1 :BAR 5) (EVENT :FOO 1 :BAR 6)
;;     (EVENT :FOO 2 :BAR 4) (EVENT :FOO 2 :BAR 5) (EVENT :FOO 2 :BAR 6)
;;     (EVENT :FOO 3 :BAR 4) (EVENT :FOO 3 :BAR 5) (EVENT :FOO 3 :BAR 6))

See also: `psym', `pmeta'")

(defmethod as-pstream ((parp parp))
  (with-slots (pattern arpeggiator) parp
    (let ((pstr (as-pstream pattern)))
      (make-instance 'parp-pstream
                     :pattern pstr
                     :arpeggiator arpeggiator
                     :current-pattern-event (next pstr)
                     :current-arpeggiator-stream (as-pstream arpeggiator)))))

(defmethod next ((parp parp-pstream))
  (with-slots (pattern arpeggiator current-pattern-event current-arpeggiator-stream) parp
    (unless (null current-pattern-event)
      (let ((nxt (let ((*event* (combine-events current-pattern-event)))
                   (next current-arpeggiator-stream))))
        (if (null nxt)
            (progn
              (setf current-pattern-event (next pattern)
                    current-arpeggiator-stream (as-pstream arpeggiator))
              (next parp))
            nxt)))))

;;; pfin

(defpattern pfin (pattern)
  (pattern
   count)
  "pfin yields up to COUNT outputs from PATTERN.

Example:

;; (next-n (pfin (pseq '(1 2 3) :inf) 3) 5)
;;
;; => (1 2 3 NIL NIL)

See also: `pfindur'")

(defmethod as-pstream ((pfin pfin))
  (with-slots (count pattern) pfin
    (make-instance 'pfin-pstream
                   :pattern (as-pstream pattern)
                   :count (next count))))

(defmethod next ((pfin pfin-pstream))
  (with-slots (pattern count number) pfin
    (when (< number count)
      (next pattern))))

;;; pfindur

(defpattern pfindur (pattern)
  (pattern
   dur
   (tolerance :default 0)
   (current-elapsed :state t :initform 0))
  "pfindur yields events from PATTERN until their total duration is within TOLERANCE of DUR, or greater than DUR. Any events that would end beyond DUR are cut short.

Example:

;; (next-n (pfindur (pbind :dur 1 :foo (pseries)) 2) 3)
;;
;; => ((EVENT :DUR 1 :FOO 0) (EVENT :DUR 1 :FOO 1) NIL)

See also: `pfin', `psync'")

(defmethod as-pstream ((pfindur pfindur))
  (with-slots (pattern dur tolerance) pfindur
    (make-instance 'pfindur-pstream
                   :pattern (as-pstream pattern)
                   :dur (next dur)
                   :tolerance (next tolerance))))

(defmethod next ((pfindur pfindur-pstream)) ;; FIX: make this affect the dur AND delta properly.
  (with-slots (pattern dur tolerance current-elapsed) pfindur
    (alexandria:when-let ((n-event (next pattern)))
      (when (or (eql :inf dur)
                (< (if (= 0 tolerance)
                       current-elapsed
                       (round-up current-elapsed tolerance))
                   dur))
        (let ((new-elapsed (+ (event-value n-event :delta) current-elapsed)))
          (prog1
              (if (and (not (eql :inf dur))
                       (> (if (= 0 tolerance)
                              new-elapsed
                              (round-up new-elapsed tolerance))
                          dur))
                  (combine-events n-event (event :dur (- dur current-elapsed)))
                  n-event)
            (incf current-elapsed (event-value n-event :delta))))))))

;;; psync

(defpattern psync (pattern)
  (pattern
   sync-quant
   (maxdur :default nil)
   (tolerance :default 0.001))
  "psync yields events from PATTERN until their total duration is within TOLERANCE of MAXDUR, cutting off any events that would extend past MAXDUR. If PATTERN ends before MAXDUR, a rest is added to the pstream to round its duration up to the nearest multiple of SYNC-QUANT.

Example:

;; (next-upto-n (psync (pbind :dur (pseq '(5) 1)) 4 16))
;;
;; => ((EVENT :DUR 5) (EVENT :TYPE :REST :DUR 3))
;;
;; (next-upto-n (psync (pbind :dur (pseq '(5) 5)) 4 16))
;;
;; => ((EVENT :DUR 5) (EVENT :DUR 5) (EVENT :DUR 5) (EVENT :DUR 5 :DELTA 1))

See also: `pfindur'")

(defmethod as-pstream ((psync psync))
  (with-slots (pattern sync-quant maxdur tolerance) psync
    (make-instance 'psync-pstream
                   :pattern (as-pstream pattern)
                   :sync-quant (next sync-quant)
                   :maxdur (next maxdur)
                   :tolerance (next tolerance))))

(defmethod next ((psync psync-pstream)) ;; FIX: implement tolerance
  (with-slots (pattern sync-quant maxdur tolerance history) psync
    (let* ((n-event (next pattern))
           (elapsed-dur (reduce #'+ (mapcar #'event-value (remove-if #'null history) (alexandria:circular-list :delta))))
           (delta (- (round-up elapsed-dur sync-quant) elapsed-dur)))
      (if (null n-event)
          (when (plusp delta)
            (event :type :rest :dur delta))
          (when (or (null maxdur)
                    (not (>= elapsed-dur maxdur)))
            (if (and (not (null maxdur))
                     (> (+ elapsed-dur (event-value n-event :dur)) maxdur))
                (combine-events n-event (event :dur (- maxdur elapsed-dur)))
                n-event))))))

;;; pstutter

(defpattern pstutter (pattern)
  (pattern
   n
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t :initform 0))
  "pstutter yields each output from PATTERN N times before moving on to the next output from PATTERN.

Example:

;; (next-n (pstutter (pseries) (pseq '(3 2 1 0 2) 1)) 9)
;;
;; => (0 0 0 1 1 2 4 4 NIL)

See also: `pr', `pdurstutter'")

(defmethod as-pstream ((pstutter pstutter))
  (with-slots (n pattern) pstutter
    (make-instance 'pstutter-pstream
                   :pattern (as-pstream pattern)
                   :n (pattern-as-pstream n))))

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
  (pattern
   n
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t :initform 0))
  "pdurstutter yields each output from PATTERN N times, dividing it by N. If the output from PATTERN is an event, its dur is divided; if it's a number, the number itself is divided instead of being yielded directly.

Example:

;; (next-n (pdurstutter (pseq '(1 2 3 4 5)) (pseq '(3 2 1 0 2))) 9)
;;
;; => (1/3 1/3 1/3 1 1 3 5/2 5/2 NIL)
;;
;; (next-n (pdurstutter (pbind :dur (pseq '(1 2 3 4 5))) (pseq '(3 2 1 0 2))) 9)
;;
;; => ((EVENT :DUR 1/3) (EVENT :DUR 1/3) (EVENT :DUR 1/3) (EVENT :DUR 1) (EVENT :DUR 1) (EVENT :DUR 3) (EVENT :DUR 5/2) (EVENT :DUR 5/2) NIL)

See also: `pr', `pstutter'")

(defmethod as-pstream ((pdurstutter pdurstutter))
  (with-slots (pattern n) pdurstutter
    (make-instance 'pdurstutter-pstream
                   :pattern (as-pstream pattern)
                   :n (pattern-as-pstream n))))

(defmethod next ((pattern pdurstutter-pstream))
  (with-slots (n current-value current-repeats-remaining) pattern
    (loop :while (and (not (null current-repeats-remaining))
                      (= 0 current-repeats-remaining))
       :do
         (setf current-repeats-remaining (next n))
         (let ((e (next (slot-value pattern 'pattern))))
           (when (and (not (null current-repeats-remaining))
                      (not (= 0 current-repeats-remaining)))
             (setf current-value (ctypecase e
                                   (event (combine-events e (event :dur (/ (event-value e :dur) current-repeats-remaining))))
                                   (number (/ e current-repeats-remaining))
                                   (null nil))))))
    (unless (null current-repeats-remaining)
      (decf-remaining pattern 'current-repeats-remaining)
      current-value)))

;;; pbeat

(defpattern pbeat (pattern)
  ()
  "pbeat yields the number of beats elapsed since its embedding in the pstream.

Example:

;; (next-n (pbind :dur (pseq '(1 2 3)) :foo (pbeat)) 3)
;;
;; => ((EVENT :DUR 1 :FOO 0) (EVENT :DUR 2 :FOO 1) (EVENT :DUR 3 :FOO 3))

See also: `beat', `prun'")

(defmethod next ((pbeat pbeat-pstream))
  (beat (parent-pbind pbeat)))

;;; ptime

(defpattern ptime (pattern)
  ((last-beat-checked :state t :initform nil)
   (tempo-at-beat :state t :initform nil)
   (elapsed-time :state t :initform 0))
  "ptime yields the number of seconds elapsed since its embedding in the pstream.

Note: May give inaccurate results if the clock's tempo changes occur more frequently than events in the parent pbind.

Example:

;; (setf (tempo *clock*) 1) ;; 60 BPM
;; (next-n (pbind :dur 1 :time (ptime)) 2)
;;
;; => ((EVENT :DUR 1 :TIME 0) (EVENT :DUR 1 :TIME 1.0))

See also: `pbeat', `prun', `beat'")

(defmethod next ((ptime ptime-pstream)) ;; FIX: take into account the previous tempo if it has been changed since the last-beat-checked.
  (with-slots (last-beat-checked tempo-at-beat elapsed-time) ptime
    (with-slots (tempo) *clock*
      (let ((beat (beat (parent-pbind ptime))))
        (prog1
            (incf elapsed-time (if (null last-beat-checked)
                                   0
                                   (dur-time (- beat last-beat-checked) tempo)))
          (setf last-beat-checked beat
                tempo-at-beat tempo))))))

;;; pindex
;; TODO: alternate version that only calls #'next on index-pat each time the pattern-as-pstream of list-pat has ended.

(defpattern pindex (pattern)
  (list-pat
   index-pat
   (wrap-p :default nil))
  "Use INDEX-PAT to index into the list returned by LIST-PAT. WRAP-P is whether indexes that are out of range will be wrapped (if t) or will simply return nil.

Example:

;; (next-n (pindex (list 99 98 97) (pseq (list 0 1 2 3))) 4)
;;
;; => (99 98 97 NIL)
;;
;; (next-upto-n (pindex (list 99 98 97) (pseries 0 1 6) t))
;;
;; => (99 98 97 99 98 97)

See also: `pwalk', `pswitch'")

(defmethod as-pstream ((pindex pindex))
  (with-slots (list-pat index-pat wrap-p) pindex
    (make-instance 'pindex-pstream
                   :list-pat (pattern-as-pstream list-pat)
                   :index-pat (pattern-as-pstream index-pat)
                   :wrap-p wrap-p)))

(defmethod next ((pindex pindex-pstream)) ;; FIX: make this work for envelopes as well (the index should not be normalized)
  (with-slots (list-pat index-pat wrap-p) pindex
    (let ((list (next list-pat))
          (idx (next index-pat)))
      (when idx
        (funcall (if wrap-p 'elt-wrap 'elt) list idx)))))

;;; prun

(defpattern prun (pattern) ;; FIX: make this work on event patterns too (make DUR a duration multiplier)
  (pattern
   (dur :default 1)
   (dur-history :state t))
  "Run PATTERN independently of when `next' is called on its pstream. Each output of PATTERN is treated as if it lasted DUR beats, during which time it will be continuously yielded by prun. If PATTERN is an event pattern, DUR acts as a duration multiplier instead.

Example:

;; (next-upto-n (pbind :foo (pseq '(1 2 3 4 5)) :bar (prun (pseq '(4 5 6 7 8)) (pseq '(1 2 0.5 0.5 1)))))
;;
;; => ((EVENT :FOO 1 :BAR 4) (EVENT :FOO 2 :BAR 5) (EVENT :FOO 3 :BAR 5) (EVENT :FOO 4 :BAR 6) (EVENT :FOO 5 :BAR 8))

See also: `beat', `pbeat'")

(defmethod as-pstream ((prun prun))
  (with-slots (pattern dur) prun
    (make-instance 'prun-pstream
                   :pattern (as-pstream pattern)
                   :dur (pattern-as-pstream dur)
                   :dur-history nil)))

(defmethod next ((prun prun-pstream))
  (with-slots (pattern dur dur-history) prun
    (flet ((ok ()
             (not (position nil dur-history))))
      (let ((beats (beat (parent-pbind prun)))
            (cdur (reduce #'+ dur-history)))
        (loop :while (and (ok) (<= cdur beats))
           :do
             (let ((nxt (next dur)))
               (alexandria:appendf dur-history (list nxt))
               (when nxt
                 (incf cdur nxt))))
        (when (ok)
          (let* ((clist (cumulative-list dur-history))
                 (idx (or (index-of-greater-than beats clist) 0)))
            (pstream-elt-future pattern idx)))))))

;;; psym

(defpattern psym (pattern)
  (pattern
   ;; (dict :default nil) ;; FIX: implement this
   (current-pstream :state t :initform nil))
  "Use a pattern of symbols to embed `pdef's. PATTERN is the source pattern that yields symbols naming the pdef to embed.

Example:

;; (pdef :foo (pseq '(1 2 3) 1))
;;
;; (pdef :bar (pseq '(4 5 6) 1))
;;
;; (next-upto-n (psym (pseq '(:foo :bar) 1)))
;;
;; => (1 2 3 4 5 6)

See also: `pdef', `ppar', `pmeta'")

(defmethod as-pstream ((psym psym))
  (with-slots (pattern) psym
    (make-instance 'psym-pstream
                   :pattern (as-pstream pattern))))

(defmethod next ((psym psym-pstream))
  (with-slots (pattern current-pstream) psym
    (let ((n (next current-pstream)))
      (if n
          n
          (let ((next-pdef (next pattern)))
            (unless (null next-pdef)
              (setf current-pstream (as-pstream (if (listp next-pdef)
                                                    (ppar (mapcar #'pdef next-pdef))
                                                    (pdef next-pdef))))
              (next psym)))))))

;;; pchain

(defpattern pchain (pattern)
  (patterns)
  "Combine multiple patterns into one event stream.

Example:

;; (next-n (pchain (pbind :foo (pseq '(1 2 3))) (pbind :bar (pseq '(7 8 9) 1))) 4)
;;
;; => ((EVENT :FOO 1 :BAR 7) (EVENT :FOO 2 :BAR 8) (EVENT :FOO 3 :BAR 9) NIL)

See also: `pbind''s :embed key"
  (defun pchain (&rest patterns)
    (set-parents
     (make-instance 'pchain
                    :patterns patterns))))

(defmethod as-pstream ((pchain pchain))
  (with-slots (patterns) pchain
    (make-instance 'pchain-pstream
                   :patterns (loop :for pattern :in patterns
                                :collect (as-pstream pattern)))))

(defmethod next ((pchain pchain-pstream))
  (with-slots (patterns) pchain
    (let ((c-event (make-default-event)))
      (loop :for pattern :in patterns
         :do (setf c-event (combine-events c-event (let ((*event* c-event))
                                                     (next pattern)))))
      c-event)))

;;; pdiff

(defpattern pdiff (pattern)
  (pattern)
  "Output the difference between successive outputs of PATTERN.

Example:

;; (next-n (pdiff (pseq '(3 1 4 3) 1)) 4)
;;
;; => (-2 3 -1 NIL)

See also: `pdelta'")

(defmethod next ((pdiff pdiff-pstream))
  (with-slots (pattern) pdiff
    (when (null (slot-value pattern 'history))
      (next pattern))
    (alexandria:when-let ((last (pstream-elt pattern -1))
                          (next (next pattern)))
      (- next last))))

;;; pdelta

(defpattern pdelta (pattern)
  (pattern
   (cycle :default 4))
  "Output the difference between successive outputs of PATTERN, assuming PATTERN restarts every CYCLE outputs.

Unlike `pdiff', pdelta is written with its use as input for `pbind''s :delta key in mind. If PATTERN's successive values would result in a negative difference, pdelta instead wraps the delta around to the next multiple of CYCLE. This would allow you to, for example, supply the number of the beat that each event occurs on, rather than specifying the delta between each event. This is of course achievable using pbind's :beat key as well, however that method requires the pbind to peek at future values, which is not always desirable.

Example:

;; (next-n (pdelta (pseq '(0 1 2 3)) 4) 8)
;;
;; => (1 1 1 1 1 1 1 1)
;;
;; (next-n (pdelta (pseq '(0 1 2 5)) 4) 8)
;;
;; => (1 1 3 3 1 1 3 3)

See also: `pdiff', `pbind''s :beat key")

(defmethod as-pstream ((pdelta pdelta))
  (with-slots (pattern cycle) pdelta
    (make-instance 'pdelta-pstream
                   :pattern (as-pstream pattern)
                   :cycle (next cycle))))

(defmethod next ((pdelta pdelta-pstream))
  (with-slots (pattern cycle history) pdelta
    (when (null history)
      (next pattern))
    (alexandria:when-let ((lv (or (pstream-elt pattern -1) 0))
                          (cv (next pattern)))
      (- cv
         (- lv (round-up (- lv cv) cycle))))))

;;; pdrop

(defpattern pdrop (pattern)
  (pattern
   (n :default 0))
  "Drop the first N outputs from PATTERN and yield the rest. If N is negative, drop the last N outputs from PATTERN instead.

Example:

;; (next-n (pdrop (pseq '(1 2 3 4) 1) 2) 4)
;;
;; => (3 4 NIL NIL)

See also: `pshift'")

(defmethod as-pstream ((pdrop pdrop))
  (with-slots (pattern n) pdrop
    (make-instance 'pdrop-pstream
                   :pattern (as-pstream pattern)
                   :n n)))

(defmethod next ((pdrop pdrop-pstream))
  (with-slots (pattern n) pdrop
    (if (minusp n)
        (loop :while (<= n (slot-value pattern 'pstream-offset))
           :do (when (null (peek pattern))
                 (return-from next nil)))
        (when (null (slot-value pattern 'history))
          (loop :repeat n
             :do (next pattern))))
    (next pattern)))

;;; ppar

(defpattern ppar (pattern)
  (list
   (pstreams :state t :initform nil))
  "Combine multiple event patterns into one pstream with all events in temporal order. LIST is the list of patterns, or a pattern yielding lists of patterns. The ppar ends when all of the patterns in LIST end.

Example:

;; (next-upto-n (ppar (list (pbind :dur (pn 1/2 4))
;;                          (pbind :dur (pn 2/3 4)))))
;;
;; => ((EVENT :DUR 1/2 :DELTA 0) (EVENT :DUR 2/3 :DELTA 1/2)
;;     (EVENT :DUR 1/2 :DELTA 1/6) (EVENT :DUR 2/3 :DELTA 1/3)
;;     (EVENT :DUR 1/2 :DELTA 1/3) (EVENT :DUR 2/3 :DELTA 1/6)
;;     (EVENT :DUR 1/2 :DELTA 1/2))

See also: `psym'")

(defmethod next ((ppar ppar-pstream))
  (with-slots (list pstreams history) ppar
    (labels ((next-in-pstreams ()
               (alexandria:when-let ((res (remove-if (lambda (pstream)
                                                       (and (not (null (slot-value pstream 'history)))
                                                            (null (pstream-elt pstream -1))))
                                                     pstreams)))
                 (most-x res #'< #'beat)))
             (maybe-reset-pstreams ()
               (when (null (remove-if #'null pstreams))
                 (let ((next-list (next list)))
                   (when (null next-list)
                     (return-from maybe-reset-pstreams nil))
                   (setf pstreams (mapcar #'as-pstream next-list))))))
      (when (null history)
        (maybe-reset-pstreams))
      (let ((nxt (next (next-in-pstreams))))
        (if (null nxt)
            (let ((nip (next-in-pstreams)))
              (when nip
                (let ((beppar (beat ppar))
                      (benip (beat nip)))
                  (if (<= beppar benip)
                      (event :type :rest :delta (- benip beppar))
                      (next ppar)))))
            (combine-events nxt (event :delta (- (beat (next-in-pstreams)) (beat ppar)))))))))

;;; pmeta

(defpattern pmeta (pattern) ;; FIX: add example
  (pattern
   (current-pstream :state t))
  "Meta-control patterns using the events output by PATTERN. In other words, instead of triggering synths directly, the events output by PATTERN are used to embed patterns into the pmeta's pstream.

The following keys are supported:

- :pattern or :instrument - name of the source pattern for this \"step\".
- :sustain - limit the duration of the embedded pattern (defaults to :inf, which causes the pattern to play to its end).

The following keys are planned for future implementation:

- :stretch - multiply the duration of each of the source pattern's events.
- :ts or :fit - timestretch a pattern so its total duration is the number specified, a la `pts'.
- :start or :end - adjust the start or end points of the source pattern (i.e. to skip the first half, set :start to 0.5).
- :start-beat or :end-beat - adjust the start or end points of the source pattern in number of beats (i.e. to end the pattern 2 beats early, set :end-beat to -2).
- :start-nth or :end-nth - adjust the start or end points of the source pattern by skipping the first or last N events.
- :filter or :remove-if - skip all events from the source pattern that return nil when applied to the specified function or pattern.
- :mapcar or :nary - process each event from the source pattern with a function or another pattern.
- :inject - inject key/value pairs from the output of this value into the source pattern.

See doc/special-keys.org for more information on these keys.

Example:

See also: `psym', `parp'")

(defmethod next ((pmeta pmeta-pstream))
  (with-slots (pattern current-pstream) pmeta
    (labels ((make-pstream (event)
               (let ((pattern (or (event-value event :pattern)
                                  (event-value event :instrument)))
                     (dur (multiple-value-bind (num from) (event-value event :dur) ;; FIX: should this account for :inf ?
                            (unless (eql t from)
                              num))))
                 (when pattern
                   (let* ((pattern (typecase pattern
                                     (pattern pattern)
                                     (symbol (pdef pattern))
                                     (list (ppar (mapcar 'pdef pattern)))))
                          (pattern (if dur
                                       (psync pattern dur dur)
                                       pattern))
                          (pstream (as-pstream pattern)))
                     (setf (slot-value pstream 'parent) pmeta)
                     pstream)))))
      (when (or (not (slot-boundp pmeta 'current-pstream))
                (null current-pstream))
        (setf current-pstream (make-pstream (next pattern))))
      (when current-pstream
        (let ((nxt (next current-pstream)))
          (if (null nxt)
              (progn
                (setf current-pstream nil)
                (next pmeta))
              nxt))))))

;;; pts

(defpattern pts (pattern)
  (pattern
   (dur :default 4))
  "Timestretch PATTERN so its total duration is DUR. Note that only the first `*max-pattern-yield-length*' events from PATTERN are considered, and that they are calculated immediately at pstream creation time rather than lazily as the pstream yields.

Example:

;; (next-upto-n (pts (pbind :dur (pn 1 4)) 5))
;;
;; => ((EVENT :DUR 5/4) (EVENT :DUR 5/4) (EVENT :DUR 5/4) (EVENT :DUR 5/4))

See also: `pfindur', `psync'")

(defmethod as-pstream ((pts pts))
  (with-slots (pattern dur) pts
    (let ((pstr (as-pstream pattern)))
      (next-upto-n pstr)
      (make-instance 'pts-pstream
                     :pattern pstr
                     :dur (next dur)))))

(defmethod next ((pts pts-pstream))
  (with-slots (pattern dur number) pts
    (let ((mul (/ dur (beat pattern)))
          (ev (pstream-elt pattern number)))
      (when ev
        (combine-events ev (event :dur (* mul (event-value ev :dur))))))))

;;; pwalk

(defpattern pwalk (pattern)
  (list
   step-pattern
   (direction-pattern :default 1)
   (start-pos :default 0)
   (current-index :state t)
   (current-direction :default 1 :state t))
  "\"Walk\" over the values in LIST by using the accumulated value of the outputs of STEP-PATTERN as the index. At the beginning of the pwalk and each time the start or end of the list is passed, the output of DIRECTION-PATTERN is taken and used as the multiplier for values from STEP-PATTERN. START-POS is the index in LIST for pwalk to take its first output from.

Example:

;; ;; using (pseq (list 1 -1)) as the DIRECTION-PATTERN causes the pwalk's output to \"ping-pong\":
;; (next-n (pwalk (list 0 1 2 3) (pseq (list 1)) (pseq (list 1 -1))) 10)
;;
;; => (0 1 2 3 2 1 0 1 2 3)

See also: `pindex', `pbrown'") ;; FIX: also `paccum' when it's completed

(defmethod as-pstream ((pwalk pwalk))
  (with-slots (list step-pattern direction-pattern start-pos) pwalk
    (make-instance 'pwalk-pstream
                   :list (next list)
                   :step-pattern (pattern-as-pstream step-pattern)
                   :direction-pattern (pattern-as-pstream direction-pattern)
                   :start-pos (pattern-as-pstream start-pos))))

(defmethod next ((pwalk pwalk-pstream))
  (with-slots (list step-pattern direction-pattern start-pos current-index current-direction) pwalk
    (unless (slot-boundp pwalk 'current-direction)
      (setf current-direction (next direction-pattern)))
    (if (slot-boundp pwalk 'current-index)
        (let ((nsp (next step-pattern)))
          (when (and nsp current-direction)
            (let ((next-index (+ current-index (* nsp current-direction))))
              (if (or (minusp next-index) ;; if we're out of the list bounds, check the direction pattern...
                      (>= next-index (length list)))
                  (setf current-direction (next direction-pattern)))
              (setf current-index (mod (+ current-index (* nsp current-direction)) (length list))))))
        (progn
          (next list) ;; FIX?
          (setf current-index (next start-pos))))
    (when (and current-index list)
      (elt-wrap list current-index))))

