(in-package #:cl-patterns)

;;;; patterns.lisp - basic pattern functionality (`defpattern', etc) and a variety of basic patterns implemented with it.

;;; pattern glue

(defun make-default-event ()
  "Get `*event*' if it's not nil, or get a fresh empty event."
  (or *event* (event)))

(defparameter *patterns* (list)
  "List of the names of all defined pattern types.")

(defmacro defpattern (name superclasses slots &key documentation defun)
  "Define a pattern. This macro automatically generates the pattern's class, its pstream class, and the function to create an instance of the pattern, and makes them external in the cl-patterns package.

NAME is the name of the pattern. Typically a word or two that describes its function, prefixed with p.

SUPERCLASSES is a list of superclasses of the pattern. Most patterns just subclass the 'pattern' class.

SLOTS is a list of slots that the pattern and pstreams derived from it have. Each slot can either be just a symbol, or a slot definition a la `defclass'. You can provide a default for the slot with the :default key, and you can set a slot as a state slot (which only appears in the pattern's pstream class) by setting the :state key to t.

DOCUMENTATION is a docstring describing the pattern. We recommend providing at least one example, and a \"See also\" section to refer to similar pattern classes.

DEFUN can either be a full defun form for the pattern, or an expression which will be inserted into the pattern creation function prior to initialization of the instance. Typically you'd use this for inserting `assert' statements, for example."
  (let* ((superclasses (or superclasses (list 'pattern)))
         (slots (mapcar #'ensure-list slots))
         (name-pstream (symbolicate name '-pstream))
         (super-pstream (if (eql 'pattern (car superclasses))
                            'pstream
                            (symbolicate (car superclasses) '-pstream))))
    (labels ((desugar-slot (slot)
               "Convert a slot into something appropriate for defclass to handle."
               (destructuring-bind (name . rest) slot
                 (append (list name)
                         (remove-from-plist rest :default :state)
                         (unless (position :initarg (keys rest))
                           (list :initarg (make-keyword name))))))
             (optional-slot-p (slot)
               "Whether the slot is optional or not. A slot is optional if a default is provided."
               (position :default (keys (cdr slot))))
             (state-slot-p (slot)
               "Whether the slot is a pstream state slot or not. Pstream state slots only appear as slots for the pattern's pstream class and not for the pattern itself."
               (position :state (keys (cdr slot))))
             (function-lambda-list (slots)
               "Generate the lambda list for the pattern's creation function."
               (let (optional-used)
                 (mappend (fn (unless (state-slot-p _)
                                (if (optional-slot-p _)
                                    (prog1
                                        (append (unless optional-used
                                                  (list '&optional))
                                                (list (list (car _) (getf (cdr _) :default))))
                                      (setf optional-used t))
                                    (list (car _)))))
                          slots)))
             (make-defun (pre-init)
               `(defun ,name ,(function-lambda-list slots)
                  ,documentation
                  ,@(when pre-init (list pre-init))
                  (make-instance ',name
                                 ,@(mappend (fn (unless (state-slot-p _)
                                                  (list (make-keyword (car _)) (car _))))
                                            slots))))
             (add-doc-to-defun (sexp)
               (if (and (listp sexp)
                        (position (car sexp) (list 'defun 'defmacro))
                        (not (stringp (fourth sexp))))
                   (append (subseq sexp 0 3) (list documentation) (subseq sexp 3))
                   sexp)))
      `(progn
         (defclass ,name ,superclasses
           ,(mapcar #'desugar-slot (remove-if #'state-slot-p slots))
           ,@(when documentation
               `((:documentation ,documentation))))
         (defmethod print-object ((,name ,name) stream)
           (print-unreadable-object (,name stream :type t)
             (format stream "~{~s~#[~:; ~]~}"
                     (mapcar (lambda (slot) (slot-value ,name slot))
                             ',(mapcar #'car (remove-if (lambda (slot)
                                                          (or (state-slot-p slot)
                                                              ;; FIX: don't show arguments that are set to the defaults?
                                                              ))
                                                        slots))))))
         (defclass ,name-pstream (,super-pstream ,name)
           ,(mapcar #'desugar-slot (remove-if-not #'state-slot-p slots))
           (:documentation ,(format nil "pstream for `~a'." (string-downcase (symbol-name name)))))
         ,(let* ((gen-func-p (or (null defun)
                                 (and (listp defun)
                                      (position (car defun) (list 'assert 'check-type)))))
                 (pre-init (when gen-func-p
                             defun)))
            (if gen-func-p
                (make-defun pre-init)
                (add-doc-to-defun defun)))
         (pushnew ',name *patterns*)))))

(defvar *max-pattern-yield-length* 256
  "The default maximum number of events or values that will be used by functions like `next-n' or patterns like `protate', in order to prevent hangs caused by infinite-length patterns.")

;;; pattern

(defgeneric pstream-count (pattern)
  (:documentation "The number of pstreams that have been made of this pattern."))

(defclass pattern ()
  ((play-quant :initarg :play-quant :documentation "A list of numbers representing when the pattern's pstream can start playing. See `play-quant' and `quant'.")
   (end-quant :initarg :end-quant :accessor end-quant :type list :documentation "A list of numbers representing when a pattern can end playing and when a `pdef' can be swapped out for a new definition. See `end-quant' and `quant'.")
   (end-condition :initarg :end-condition :initform nil :accessor end-condition :type (or null function) :documentation "Nil or a function that is called by the clock with the pattern as its argument to determine whether the pattern should end or swap to a new definition.")
   (parent :initarg :parent :initform nil :documentation "When a pattern is embedded in another pattern, the embedded pattern's parent slot points to the pattern it is embedded in.")
   (loop-p :initarg :loop-p :documentation "Whether or not the pattern should loop when played.")
   (cleanup :initarg :cleanup :initform (list) :documentation "A list of functions that are run when the pattern ends or is stopped.")
   (pstream-count :initform 0 :accessor pstream-count :documentation "The number of pstreams that have been made of this pattern.")
   (metadata :initarg :metadata :initform (make-hash-table) :type hash-table :documentation "Hash table of additional data associated with the pattern, accessible with the `pattern-metadata' function."))
  (:documentation "Abstract pattern superclass."))

(defun set-parents (pattern)
  "Loop through PATTERN's slots and set the \"parent\" slot of any patterns to this pattern."
  (labels ((set-parent (list parent)
             "Recurse through LIST, setting the parent of any pattern found to PARENT."
             (typecase list
               (list
                (mapc (lambda (x) (set-parent x parent)) list))
               (pattern
                (setf (slot-value list 'parent) parent)))))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern))) pattern)
      (when (and (not (eql slot 'parent))
                 (slot-boundp pattern slot))
        (set-parent (slot-value pattern slot) pattern)))))

(defmethod initialize-instance :after ((pattern pattern) &key)
  (set-parents pattern))

(defun pattern-p (object)
  "True if OBJECT is a pattern.

See also: `pattern', `defpattern'"
  (typep object 'pattern))

(defun all-patterns ()
  "Get a list of the names of all defined pattern classes.

See also: `all-pdefs'"
  *patterns*)

(defmethod play-quant ((pattern pattern))
  (if (slot-boundp pattern 'play-quant)
      (slot-value pattern 'play-quant)
      (list 1)))

(defmethod (setf play-quant) (value (pattern pattern))
  (setf (slot-value pattern 'play-quant) (ensure-list value)))

(defmethod end-quant ((pattern pattern))
  (when (slot-boundp pattern 'end-quant)
    (slot-value pattern 'end-quant)))

(defmethod (setf end-quant) (value (pattern pattern))
  (setf (slot-value pattern 'end-quant) (ensure-list value)))

(defmethod play ((pattern pattern))
  (clock-add (as-pstream pattern) *clock*))

(defmethod launch ((pattern pattern))
  (play pattern))

(defmethod playing-p ((pattern pattern) &optional (clock *clock*))
  (when clock
    (find pattern (clock-tasks clock)
          :key (fn (slot-value _ 'item)))))

(defmethod loop-p ((pattern pattern))
  (when (slot-boundp pattern 'loop-p)
    (slot-value pattern 'loop-p)))

(defmethod (setf loop-p) (value (pattern pattern))
  (setf (slot-value pattern 'loop-p) value))

(defmethod dur ((pattern pattern))
  (reduce #'+ (next-upto-n pattern) :key #'dur))

(defun pattern-parent (pattern &key (num 1) (accumulate nil) (class 'pattern))
  "Get the NUM-th containing pattern of PATTERN, or nil if there isn't one. If CLASS is specified, only consider patterns of that class.

See also: `pattern-children'"
  (check-type num (integer 0))
  (let ((i 0)
        res)
    (until (or (>= i num)
               (null pattern))
      (setf pattern (slot-value pattern 'parent))
      (when (typep pattern class)
        (incf i)
        (when accumulate
          (appendf res pattern))))
    (if accumulate
        res
        pattern)))

(uiop:with-deprecation (:warning)
  (defun parent-pattern (pattern)
    "Deprecated alias for `pattern-parent'."
    (pattern-parent pattern))

  (defun parent-pbind (pattern)
    "Deprecated forward to `pattern-parent'. Use (pattern-parent PATTERN :class 'pbind) instead."
    (pattern-parent pattern :class 'pbind)))

(defun pattern-children (pattern &key (num 1) (accumulate nil) (class 'pattern))
  "Get a list of all the direct child patterns of PATTERN, including any in slots or lists.

See also: `pattern-parent'"
  (let ((cur (list pattern))
        res)
    (dotimes (n num res)
      (setf cur (remove-if-not (lambda (pattern) (typep pattern class))
                               (mapcan #'%pattern-children cur)))
      (if accumulate
          (appendf res cur)
          (setf res cur)))))

(defmethod %pattern-children ((object t))
  nil)

(defmethod %pattern-children ((pattern pattern))
  (mapcan (lambda (slot)
            (copy-list (ensure-list (slot-value pattern (closer-mop:slot-definition-name slot)))))
          (closer-mop:class-direct-slots (class-of pattern))))

(defgeneric pattern-metadata (pattern &optional key)
  (:documentation "Get the value of PATTERN's metadata for KEY. Returns true as a second value if the metadata had an entry for KEY, or nil if it did not."))

(defmethod pattern-metadata ((pattern pattern) &optional key)
  (with-slots (metadata) pattern
    (if key
        (gethash key metadata)
        metadata)))

(defun (setf pattern-metadata) (value pattern key)
  (setf (gethash key (slot-value pattern 'metadata)) value))

(defgeneric peek (pattern)
  (:documentation "\"Peek\" at the next value of a pstream, without advancing its current position.

See also: `next', `peek-n', `peek-upto-n'"))

(defun peek-n (pstream n)
  "Peek at the next N results of a pstream, without advancing it forward in the process.

See also: `peek', `peek-upto-n', `next', `next-n'"
  (check-type n (integer 0))
  (unless (pstream-p pstream)
    (return-from peek-n (peek-n (as-pstream pstream) n)))
  (with-slots (number future-number) pstream
    (loop :for i :from 0 :below n
          :collect (pstream-elt-future pstream (+ number (- future-number) i)))))

(defun peek-upto-n (pstream &optional (n *max-pattern-yield-length*))
  "Peek at up to the next N results of a pstream, without advancing it forward in the process.

See also: `peek', `peek-n', `next', `next-upto-n'"
  (check-type n (integer 0))
  (unless (pstream-p pstream)
    (return-from peek-upto-n (peek-upto-n (as-pstream pstream) n)))
  (with-slots (number future-number) pstream
    (loop :for i :from 0 :below n
          :for res := (pstream-elt-future pstream (+ number (- future-number) i))
          :until (eop-p res)
          :collect res)))

(defgeneric next (pattern)
  (:documentation "Get the next value of a pstream, function, or other object, advancing the pstream forward in the process.

See also: `next-n', `next-upto-n', `peek'")
  (:method-combination pattern))

(defmethod next ((object t))
  object)

(defmethod next ((pattern pattern))
  (next (as-pstream pattern)))

(defmethod next ((function function))
  (funcall function))

(defun next-n (pstream &optional (n *max-pattern-yield-length*))
  "Get the next N outputs of a pstream, function, or other object, advancing the pstream forward N times in the process.

See also: `next', `next-upto-n', `peek', `peek-n'"
  (check-type n (integer 0))
  (let ((pstream (pattern-as-pstream pstream)))
    (loop :repeat n
          :collect (next pstream))))

(defun next-upto-n (pstream &optional (n *max-pattern-yield-length*))
  "Get a list of up to N results from PSTREAM, not including the end of pattern.

See also: `next', `next-n', `peek', `peek-upto-n'"
  (check-type n (integer 0))
  (let ((pstream (pattern-as-pstream pstream)))
    (loop
      :for number :from 0 :upto n
      :while (< number n)
      :for val := (next pstream)
      :if (eop-p val)
        :do (loop-finish)
      :else
        :collect val)))

(defgeneric bsubseq (object start-beat &optional end-beat)
  (:documentation "\"Beat subseq\" - get a list of all events from OBJECT whose `beat' is START-BEAT or above, and below END-BEAT.

See also: `events-in-range'"))

(defgeneric events-in-range (pstream min max)
  (:documentation "Get all the events from PSTREAM whose start beat are MIN or greater, and less than MAX."))

(defmethod events-in-range ((pattern pattern) min max)
  (events-in-range (as-pstream pattern) min max))

;;; pstream

(defclass pstream (pattern #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((number :initform 0 :documentation "The number of outputs yielded from this pstream and any sub-pstreams that have ended.") ;; FIX: rename to this-index ?
   (pattern-stack :initform (list) :documentation "The stack of pattern pstreams embedded in this pstream.")
   (source :initarg :source :documentation "The source object (i.e. pattern) that this pstream was created from.")
   (pstream-count :initarg :pstream-count :accessor pstream-count :type integer :documentation "How many times a pstream was made of this pstream's source prior to this pstream. For example, if it was the first time `as-pstream' was called on the pattern, this will be 0.")
   (beat :initform 0 :reader beat :type number :documentation "The number of beats that have elapsed since the start of the pstream.")
   (history :type vector :documentation "The history of outputs yielded by the pstream.")
   (history-number :initform 0 :documentation "The number of items in this pstream's history. Differs from the number slot in that all outputs are immediately included in its count.")
   (start-beat :initarg :start-beat :initform nil :documentation "The beat number of the parent pstream when this pstream started.")
   (future-number :initform 0 :documentation "The number of peeks into the future that have been made in the pstream. For example, if `peek' is used once, this would be 1. If `next' is called after that, future-number decreases back to 0.")
   (future-beat :initform 0 :documentation "The current beat including all future outputs (the `beat' slot does not include peeked outputs)."))
  (:documentation "\"Pattern stream\". Keeps track of the current state of a pattern in process of yielding its outputs."))

(defmethod initialize-instance :before ((pstream pstream) &key)
  (with-slots (history) pstream
    (setf history (make-array *max-pattern-yield-length* :initial-element nil))))

(defmethod initialize-instance :after ((pstream pstream) &key)
  (set-parents pstream))

(defmethod print-object ((pstream pstream) stream)
  (with-slots (number) pstream
    (print-unreadable-object (pstream stream :type t)
      (format stream "~s ~s" :number number))))

(defun pstream-p (object)
  "True if OBJECT is a pstream.

See also: `pstream', `as-pstream'"
  (typep object 'pstream))

(defmethod loop-p ((pstream pstream))
  (if (slot-boundp pstream 'loop-p)
      (slot-value pstream 'loop-p)
      (loop-p (slot-value pstream 'source))))

(defmethod ended-p ((pstream pstream))
  (with-slots (number future-number) pstream
    (and (not (= 0 (- number future-number)))
         (eop-p (pstream-elt pstream -1)))))

(defmethod events-in-range ((pstream pstream) min max)
  (while (and (<= (beat pstream) max)
              (not (ended-p pstream)))
    (let ((next (next pstream)))
      (unless (typep next '(or null event))
        (error "events-in-range can only be used on event streams."))))
  (loop :for i :across (slot-value pstream 'history)
        :if (and i
                 (>= (beat i) min)
                 (< (beat i) max))
          :collect i
        :if (or (eop-p i)
                (>= (beat i) max))
          :do (loop-finish)))

(defgeneric last-output (pstream)
  (:documentation "Returns the last output yielded by PSTREAM.

Example:

;; (defparameter *pstr* (as-pstream (pseq '(1 2 3) 1)))
;; (next *pstr*) ;=> 1
;; (last-output *pstr*) ;=> 1

See also: `ended-p'"))

(defmethod last-output ((pstream pstream))
  (with-slots (number future-number) pstream
    (let ((idx (- number future-number)))
      (when (plusp idx)
        (pstream-elt pstream (- idx (if (ended-p pstream) 2 1)))))))

(defun value-remaining-p (value)
  "True if VALUE represents that a pstream has outputs \"remaining\"; i.e. VALUE is a symbol (i.e. :inf), or a number greater than 0.

See also: `remaining-p', `decf-remaining'"
  (typecase value
    (null nil)
    (symbol (eql value :inf))
    (number (plusp value))
    (otherwise nil)))

(defun remaining-p (pattern &optional (repeats-key 'repeats) (remaining-key 'current-repeats-remaining))
  "True if PATTERN's REMAINING-KEY slot value represents outputs \"remaining\" (see `value-remaining-p'). If PATTERN's REMAINING-KEY slot is unbound or 0, and REPEATS-KEY is not nil, then it is automatically set to the `next' of PATTERN's REPEATS-KEY slot. Then if that new value is 0 or nil, remaining-p returns nil. Otherwise, :reset is returned as a generalized true value and to indicate that `next' was called on PATTERN's REPEATS-KEY slot.

See also: `value-remaining-p', `decf-remaining'"
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
  "Decrease PATTERN's KEY value.

See also: `remaining-p'"
  (when (numberp (slot-value pattern key))
    (decf (slot-value pattern key))))

(defmethod peek ((pstream pstream))
  (with-slots (number future-number) pstream
    (pstream-elt-future pstream (- number future-number))))

(defmethod peek ((pattern pattern))
  (next (as-pstream pattern)))

(defmethod next ((pstream pstream))
  ;; fallback method; patterns should override their pstream subclasses with their own behaviors
  nil)

(defmethod next :around ((pstream pstream))
  (labels ((get-value-from-stack (pattern)
             (with-slots (number pattern-stack) pattern
               (if pattern-stack
                   (let* ((popped (pop pattern-stack))
                          (nv (next popped)))
                     (if (eop-p nv)
                         (get-value-from-stack pattern)
                         (progn
                           (push popped pattern-stack)
                           nv)))
                   (prog1
                       (let ((res (call-next-method)))
                         (typecase res
                           (pattern
                            (if (typep pattern '(or function t-pstream))
                                res
                                (progn ;; if `next' returns a pattern, we push it to the pattern stack as a pstream
                                  (let ((pstr (as-pstream res)))
                                    (setf (slot-value pstr 'start-beat) (beat pattern))
                                    (push pstr pattern-stack))
                                  (get-value-from-stack pattern))))
                           (t res)))
                     (incf number))))))
    (with-slots (number history history-number future-number) pstream
      (let ((result (if (plusp future-number)
                        (let ((result (elt history (- number future-number))))
                          (decf future-number)
                          (when (event-p result)
                            (incf (slot-value pstream 'beat) (event-value result :delta)))
                          result)
                        (let ((result (restart-case
                                          (get-value-from-stack pstream)
                                        (yield-output (&optional (value 1))
                                          :report (lambda (s) (format s "Yield an alternate output for ~S." pstream))
                                          :interactive (lambda ()
                                                         (format *query-io* "~&Enter a form to yield: ")
                                                         (finish-output *query-io*)
                                                         (list (eval (read *query-io*))))
                                          value))))
                          (when (event-p result)
                            (setf result (copy-event result))
                            (when (and (null (raw-event-value result :beat))
                                       (null (slot-value pstream 'parent)))
                              (setf (beat result) (slot-value pstream 'future-beat)))
                            (incf (slot-value pstream 'beat) (event-value result :delta))
                            (incf (slot-value pstream 'future-beat) (event-value result :delta)))
                          (setf (elt history (mod history-number (length (slot-value pstream 'history)))) result)
                          (incf history-number)
                          result))))
        (unless (pattern-parent pstream)
          (dolist (proc *post-pattern-output-processors*)
            (setf result (funcall proc result pstream))))
        result))))

(defvar *post-pattern-output-processors* (list 'remap-instrument-to-parameters)
  "List of functions that are applied as the last step of pattern output generation. Each output yielded by an \"outermost\" pattern (i.e. one without a `pattern-parent') will be processed (along with the pstream as a second argument) through each function in this list, allowing for arbitrary transformations of the generated outputs. The return value of each function is used as the input to the next function, and the return value of the last function is used as the output yielded by the pattern.

This can be used, for example, to implement mappings from friendly instrument names to the full parameters needed to specify the instrument in question for backends such as MIDI which require it; in fact this feature is already implemented more conveniently with the setf-able `instrument-mapping' function.

See also: `*instrument-map*', `remap-instrument-to-parameters'")

(defvar *instrument-map* (make-hash-table :test #'equal)
  "Hash table mapping instrument names (as symbols) to arbitrary parameter lists. Used by `remap-instrument-to-parameters' as part of post-pattern output processing. Any events whose :instrument is not found in this table will not be affected.

See also: `remap-instrument-to-parameters'")

(defun remap-instrument-to-parameters (output &optional pstream)
  "Remap OUTPUT's instrument key to arbitrary parameters specified in `*instrument-map*'. If OUTPUT is not an event or the instrument is not found in the map, it is passed through unchanged.

See also: `instrument-mapping', `*instrument-map*', `*post-pattern-output-processors*'"
  (declare (ignore pstream))
  (unless (event-p output)
    (return-from remap-instrument-to-parameters output))
  (when-let ((mapping (gethash (event-value output :instrument) *instrument-map*)))
    (etypecase mapping
      (symbol
       (setf (event-value output :instrument) mapping))
      (list
       (doplist (key value mapping)
         (setf (event-value output key) value)))))
  output)

(defun instrument-mapping (instrument)
  "Get a mapping from INSTRUMENT (an instrument name as a string or symbol) to a plist of parameters which should be set in the event by `remap-instrument-to-parameters'.

See also: `remap-instrument-to-parameters', `*instrument-map*'"
  (gethash instrument *instrument-map*))

(defun (setf instrument-mapping) (value instrument)
  "Set a mapping from INSTRUMENT (an instrument name as a string or symbol) to a plist of parameters which will be set in the event by `remap-instrument-to-parameters'. Setting an instrument to nil with this function removes it from the map.

See also: `instrument-mapping', `remap-instrument-to-parameters', `*instrument-map*'"
  (assert (or (typep value '(or symbol number))
              (and (listp value)
                   (evenp (list-length value))))
          (value)
          "~s's VALUE argument must be a symbol, a number, or a plist; got ~s instead" 'instrument-mapping value)
  (if value
      (setf (gethash instrument *instrument-map*) value)
      (remhash instrument *instrument-map*)))

(defgeneric as-pstream (thing)
  (:documentation "Return THING as a pstream object.

See also: `pattern-as-pstream'"))

(defun pattern-as-pstream (thing)
  "Like `as-pstream', but only converts THING to a pstream if it is a pattern."
  (if (typep thing 'pattern)
      (as-pstream thing)
      thing))

(defclass t-pstream (pstream)
  ((value :initarg :value :initform nil :documentation "The value that is yielded by the t-pstream.")
   (length :initarg :length :initform 1 :documentation "The number of times to yield the value."))
  (:documentation "Pattern stream object that by default yields its value only once."))

(defun t-pstream (value &optional (length 1))
  "Make a t-pstream object with the value VALUE."
  (check-type length (or (integer 0) (eql :inf)))
  (make-instance 't-pstream
                 :value value
                 :length length))

(defmethod print-object ((t-pstream t-pstream) stream)
  (with-slots (value length) t-pstream
    (print-unreadable-object (t-pstream stream :type t)
      (format stream "~s ~s" value length))))

(defmethod as-pstream ((value t))
  (t-pstream value))

(defmethod next ((t-pstream t-pstream))
  (with-slots (value length number) t-pstream
    (when (and (not (eql :inf length))
               (>= number length))
      (return-from next eop))
    (if (functionp value)
        (funcall value)
        value)))

(defmethod as-pstream ((pattern pattern))
  (let* ((class (class-of pattern))
         (name (class-name class))
         (slots (remove 'parent (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class)))))
    (apply #'make-instance
           (intern (concat name '-pstream) (symbol-package name))
           (mapcan (fn (when (slot-boundp pattern _)
                         (list (make-keyword _)
                               (pattern-as-pstream (slot-value pattern _)))))
                   slots))))

(defmethod as-pstream :around ((object t))
  (let ((pstream (call-next-method)))
    (with-slots (pstream-count source history) pstream
      (setf pstream-count (if (slot-exists-p object 'pstream-count)
                              (slot-value object 'pstream-count)
                              0)
            source object))
    (when (slot-exists-p object 'pstream-count)
      (incf (slot-value object 'pstream-count)))
    pstream))

(defmethod as-pstream ((pstream pstream)) ;; prevent pstreams from being "re-converted" to pstreams
  pstream)

(define-condition pstream-out-of-range ()
  ((index :initarg :index :reader pstream-elt-index))
  (:report (lambda (condition stream)
             (format stream "The index ~d falls outside the scope of the pstream's history." (pstream-elt-index condition)))))

(defun pstream-elt-index-to-history-index (pstream index)
  "Given INDEX, an absolute index into PSTREAM's history, return the actual index into the current recorded history of the pstream.

See also: `pstream-history-advance-by'"
  (check-type index (integer 0))
  (with-slots (history) pstream
    (mod index (length history))))

(defun pstream-elt (pstream n)
  "Get the Nth item in PSTREAM's history. For negative N, get the -Nth most recent item.

Example:

;; (let ((pstream (as-pstream (pseq '(1 2 3)))))
;;   (next pstream) ;=> 1
;;   (pstream-elt pstream 0) ;=> 1 ;; first item in the pstream's history
;;   (next pstream) ;=> 2
;;   (pstream-elt pstream 1) ;=> 2 ;; second item in the pstream's history
;;   (pstream-elt pstream -1)) ;=> 2 ;; most recent item in the pstream's history

See also: `pstream-elt-future', `phistory'"
  (check-type n integer)
  (unless (pstream-p pstream)
    (return-from pstream-elt (pstream-elt (as-pstream pstream) n)))
  (with-slots (history history-number) pstream
    (let ((real-index (if (minusp n)
                          (+ history-number n)
                          n)))
      (if (and (>= real-index (max 0 (- history-number (length history))))
               (< real-index history-number))
          (elt history (pstream-elt-index-to-history-index pstream real-index))
          (error 'pstream-out-of-range :index n)))))

(defun pstream-history-advance-by (pstream index) ;; FIX: add tests for this
  "Convert a history index (i.e. a positive number provided to `pstream-elt-future') to the amount that the history must be advanced by.

If the provided index is before the earliest item in history, the result will be a negative number denoting how far beyond the earliest history the index is.
If the provided index is within the current history, the result will be zero.
If the provided index is in the future, the result will be a positive number denoting how far in the future it is.

See also: `pstream-elt-index-to-history-index'"
  (check-type index (integer 0))
  (with-slots (history history-number) pstream
    (let ((history-length (length history)))
      (if (< index (- history-number history-length))
          (- history-number history-length)
          (if (>= index history-number)
              (- index (1- history-number))
              0)))))

(defun pstream-elt-future (pstream n)
  "Get the element N away from the most recent in PSTREAM's history. Unlike `pstream-elt', this function will automatically peek into the future for any positive N.

Example:

;; (let ((pstream (as-pstream (pseq '(1 2 3)))))
;;   (pstream-elt-future pstream 0) ;=> 1
;;   (next pstream) ;=> 1
;;   (pstream-elt-future pstream 1) ;=> 2
;;   (next pstream)) ;=> 2

See also: `pstream-elt', `phistory'"
  (check-type n integer)
  (unless (pstream-p pstream)
    (return-from pstream-elt-future (pstream-elt-future (as-pstream pstream) n)))
  (when (minusp n)
    (return-from pstream-elt-future (pstream-elt pstream n)))
  (with-slots (history history-number future-number) pstream
    (let ((advance-by (pstream-history-advance-by pstream n)))
      (when (or (minusp advance-by)
                (> (+ future-number advance-by) (length history)))
        ;; the future and history are recorded to the same array.
        ;; since the array is of finite size, requesting more from the future than history is able to hold would result in the oldest elements of the future being overwritten with the newest, thus severing the timeline...
        (error 'pstream-out-of-range :index n))
      (let ((prev-future-number future-number))
        (setf future-number 0) ;; temporarily set it to 0 so the `next' method runs normally
        (loop :repeat advance-by
              :for next := (next pstream)
              :if (event-p next)
                :do (decf (slot-value pstream 'beat) (event-value next :delta)))
        (setf future-number (+ prev-future-number advance-by))))
    (let ((real-index (pstream-elt-index-to-history-index pstream n)))
      (elt history real-index))))

;;; pbind

(defvar *pbind-special-init-keys* (list)
  "The list of special keys for pbind that alters it during its initialization.

See also: `define-pbind-special-init-key'")

(defvar *pbind-special-wrap-keys* (list)
  "The list of special keys for pbind that causes the pbind to be replaced by another pattern during its initialization.

See also: `define-pbind-special-wrap-key'")

(defvar *pbind-special-process-keys* (list)
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
;; ;=> ((EVENT :FOO 1 :BAR :HELLO) (EVENT :FOO 2 :BAR :HELLO) (EVENT :FOO 3 :BAR :HELLO) EOP)

See also: `pmono', `pb'"
  (assert (evenp (length pairs)) (pairs) "~s's PAIRS argument must be a list of key/value pairs." 'pbind)
  (when (> (count :pdef (keys pairs)) 1)
    (warn "More than one :pdef key detected in pbind."))
  (let* ((res-pairs (list))
         (pattern-chain (list))
         (pattern (make-instance 'pbind)))
    (doplist (key value pairs)
      (when (pattern-p value)
        (setf (slot-value value 'parent) pattern))
      (cond ((position key *pbind-special-init-keys*)
             (when-let ((result (funcall (getf *pbind-special-init-keys* key) value pattern)))
               (appendf res-pairs result)))
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
               (appendf pattern-chain (list pattern))
               (setf pattern (make-instance 'pbind)))
             (appendf res-pairs (list key (if (and (eql key :embed)
                                                   (typep value 'symbol))
                                              (pdef value)
                                              value))))))
    (unless (null res-pairs)
      (setf (slot-value pattern 'pairs) res-pairs))
    (appendf pattern-chain (list pattern))
    (unless (length= 1 pattern-chain)
      (setf pattern (apply #'pchain pattern-chain)))
    ;; process quant keys.
    (doplist (k v pairs)
      (when (member k (list :quant :play-quant :end-quant))
        (funcall (fdefinition (list 'setf (ensure-symbol k 'cl-patterns))) (next v) pattern)))
    ;; process :pdef key.
    (when-let ((pdef-name (getf pairs :pdef)))
      (pdef pdef-name pattern))
    pattern))

(pushnew 'pbind *patterns*)

(setf (documentation 'pbind 'type) (documentation 'pbind 'function))

(defmethod print-object ((pbind pbind) stream)
  (format stream "(~s~{ ~s ~s~})" 'pbind (slot-value pbind 'pairs)))

(defmethod %pattern-children ((pbind pbind))
  (mapcan (lambda (slot)
            (let ((slot-name (closer-mop:slot-definition-name slot)))
              (copy-list (ensure-list
                          (if (eql slot-name 'pairs)
                              (loop :for (k v) :on (slot-value pbind slot-name) :by #'cddr :collect v)
                              (slot-value pbind slot-name))))))
          (closer-mop:class-direct-slots (find-class 'pbind))))

(defmethod keys ((pbind pbind))
  (keys (slot-value pbind 'pairs)))

(defvar *pattern-function-translations* (list)
  "The list of names of functions and the forms they will be translated to in `pb' and other pattern macros.

See also: `define-pattern-function-translation'")

(defmacro define-pattern-function-translation (function pattern)
  "Define a translation from FUNCTION to PATTERN in `pb'."
  `(setf (getf *pattern-function-translations* ',function) ',pattern))

(define-pattern-function-translation + p+)

(define-pattern-function-translation - p-)

(define-pattern-function-translation * p*)

(define-pattern-function-translation / p/)

(define-pattern-function-translation round (pnary 'round))

(defun pattern-translate-sexp (sexp)
  "Translate SEXP to the equivalent pattern as per `*pattern-function-translations*', or pass it through unchanged if there is no translation.

See also: `pb-translate-body-functions'"
  (typecase sexp
    (null sexp)
    (atom sexp)
    (list (let* ((first (car sexp))
                 (rest (cdr sexp))
                 (translated-p (getf *pattern-function-translations* first))
                 (head (list (if (find-if (fn (typep _ '(or pattern list))) rest)
                                 (or translated-p first)
                                 first))))
            `(,@head ,@(if translated-p
                           (mapcar #'pattern-translate-sexp rest)
                           rest))))))

(defun pb-translate-body-functions (body)
  "Translate functions in BODY to their equivalent pattern as per `*pattern-function-translations*'.

See also: `pattern-translate-sexp'"
  (loop :for (k v) :on body :by #'cddr
        :collect k
        :collect (pattern-translate-sexp v)))

;; FIX: allow keys to be lists, in which case results are destructured, i.e. (pb :blah (list :foo :bar) (pcycles (a 1!4))) results in four (EVENT :FOO 1 :DUR 1/4)
(defmacro pb (name &body pairs)
  "pb is a convenience macro, wrapping the functionality of `pbind' and `pdef' while also providing additional syntax sugar. NAME is the name of the pattern (same as pbind's :pdef key or `pdef' itself), and PAIRS is the same as in regular pbind. If PAIRS is only one element, pb operates like `pdef', otherwise it operates like `pbind'.

The expressions in PAIRS are also automatically translated to equivalent patterns if applicable; for example:

;; (pb :foo :bar (+ (pseries) (pseq (list -1 0 1))))

...is the same as:

;; (pb :foo :bar (p+ (pseries) (pseq (list -1 0 1))))

See also: `pbind', `pdef'"
  (if (length= 1 pairs)
      `(pdef ,name ,@(pb-translate-body-functions pairs))
      `(pdef ,name (pbind ,@(pb-translate-body-functions pairs)))))

(pushnew 'pb *patterns*)

(defclass pbind-pstream (pbind pstream)
  ()
  (:documentation "pstream for `pbind'"))

(defmethod print-object ((pbind pbind-pstream) stream)
  (print-unreadable-object (pbind stream :type t)
    (format stream "~{~s ~s~^ ~}" (slot-value pbind 'pairs))))

(defmethod as-pstream ((pbind pbind))
  (let ((name (class-name (class-of pbind)))
        (slots (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pbind)))))
    (apply #'make-instance
           (intern (concat name "-PSTREAM") (symbol-package name))
           (loop :for slot :in slots
                 :for slot-kw := (make-keyword slot)
                 :for bound := (slot-boundp pbind slot)
                 :if bound
                   :collect slot-kw
                 :if (eql :pairs slot-kw)
                   :collect (mapcar 'pattern-as-pstream (slot-value pbind 'pairs))
                 :if (and bound (not (eql :pairs slot-kw)))
                   :collect (slot-value pbind slot)))))

(defmacro define-pbind-special-init-key (key &body body)
  "Define a special key for pbind that alters the pbind during its initialization, either by embedding a plist into its pattern-pairs or in another way. These functions are called once, when the pbind is created, and must return a plist if the key should embed values into the pbind pairs, or NIL if it should not."
  `(setf (getf *pbind-special-init-keys* ,(make-keyword key))
         (lambda (value pattern)
           (declare (ignorable value pattern))
           ,@body)))

;; (define-pbind-special-init-key inst ;; FIX: this should be part of event so it will affect the event as well. maybe just rename to 'synth'?
;;   (list :instrument value))

(define-pbind-special-init-key loop-p
  (setf (loop-p pattern) value)
  nil)

(defmacro define-pbind-special-wrap-key (key &body body)
  "Define a special key for pbind that replaces the pbind with another pattern during the pbind's initialization. Each encapsulation key is run once on the pbind after it has been initialized, altering the type of pattern returned if the return value of the function is non-NIL."
  `(setf (getf *pbind-special-wrap-keys* ,(make-keyword key))
         (lambda (value pattern)
           (declare (ignorable value pattern))
           ,@body)))

(define-pbind-special-wrap-key parp
  (parp pattern value))

(define-pbind-special-wrap-key pfin
  (pfin pattern value))

(define-pbind-special-wrap-key pfindur
  (pfindur pattern value))

(define-pbind-special-wrap-key psync
  (destructuring-bind (quant &optional maxdur) (ensure-list value)
    (psync pattern quant (or maxdur quant))))

(define-pbind-special-wrap-key pdurstutter
  (pdurstutter pattern value))

(define-pbind-special-wrap-key pr
  (pr pattern value))

(define-pbind-special-wrap-key pn
  (pn pattern value))

(define-pbind-special-wrap-key ptrace
  (if value
      (if (eql t value)
          (ptrace pattern)
          (pchain pattern
                  (pbind :- (ptrace value))))
      pattern))

(define-pbind-special-wrap-key pmeta
  (if (eql t value)
      (pmeta pattern)
      pattern))

(define-pbind-special-wrap-key pchain ;; basically the same as the :embed key, but we have it anyway for convenience.
  (pchain pattern value))

(define-pbind-special-wrap-key pparchain
  (pparchain pattern value))

(defmacro define-pbind-special-process-key (key &body body)
  "Define a special key for pbind that alters the pattern in a nonstandard way. These functions are called for each event created by the pbind and must return an event if the key should embed values into the event stream, or `eop' if the pstream should end."
  `(setf (getf *pbind-special-process-keys* ,(make-keyword key))
         (lambda (value)
           ,@body)))

(define-pbind-special-process-key embed
  value)

(defmethod next ((pbind pbind-pstream))
  (labels ((accumulator (pairs)
             (let ((key (car pairs))
                   (val (cadr pairs)))
               (when (and (pstream-p val)
                          (null (slot-value val 'start-beat)))
                 (setf (slot-value val 'start-beat) (beat pbind)))
               (let ((next-val (next val)))
                 (when (eop-p next-val)
                   (return-from accumulator eop))
                 (if (position key (keys *pbind-special-process-keys*))
                     (setf *event* (combine-events *event*
                                                   (funcall (getf *pbind-special-process-keys* key) next-val)))
                     (setf (event-value *event* key) next-val))
                 (if-let ((cddr (cddr pairs)))
                   (accumulator cddr)
                   *event*)))))
    (let ((*event* (make-default-event)))
      (when (eop-p *event*)
        (return-from next eop))
      (setf (slot-value *event* '%beat) (+ (or (slot-value pbind 'start-beat) 0) (beat pbind)))
      (if-let ((pairs (slot-value pbind 'pairs)))
        (accumulator pairs)
        *event*))))

(defmethod as-pstream ((pbind pbind-pstream))
  pbind)

;;; prest

(defclass prest ()
  ((value :initarg :value :initform 1))
  (:documentation "An object representing a rest. When set as a value in an event, the event's :type becomes :rest and the prest's value slot is used as the actual value for the event key instead."))

(defun prest (&optional (value 1))
  "Make a prest object, which, when used in a `pbind' or similar event pattern, turns the current event into a rest and yields VALUE for the key's value.

Note that this is not a pattern; it is just a regular function that returns a prest object.

Example:

;; (next-upto-n (pbind :degree (pseq (list 0 1 (prest 2) 3) 1)))
;; ;=> ((EVENT :DEGREE 0) (EVENT :DEGREE 1) (EVENT :TYPE :REST :DEGREE 2) (EVENT :DEGREE 3))

See also: `pbind', `pbind''s :type key"
  (make-instance 'prest :value value))

(defmethod rest-p ((prest prest))
  t)

;;; pmono

(defun pmono (instrument &rest pairs)
  "pmono defines a mono instrument event pstream. It's effectively the same as `pbind' with its :type key set to :mono.

See also: `pbind'"
  (assert (evenp (length pairs)) (pairs) "~s's PAIRS argument must be a list of key/value pairs." 'pmono)
  (apply #'pbind
         :instrument instrument
         :type :mono
         pairs))

(pushnew 'pmono *patterns*)

;;; pseq

(defpattern pseq (pattern)
  (list
   (repeats :default :inf)
   (offset :default 0)
   (current-repeats-remaining :state t))
  :documentation "Sequentially yield items from LIST, repeating the whole list REPEATS times. OFFSET is the offset to index into the list.

Example:

;; (next-n (pseq '(5 6 7) 2) 7)
;; ;=> (5 6 7 5 6 7 EOP)
;;
;; (next-upto-n (pseq '(5 6 7) 2 1))
;; ;=> (6 7 5 6 7 5)

See also: `pser', `eseq'")

(defmethod as-pstream ((pseq pseq))
  (with-slots (repeats list offset) pseq
    (make-instance 'pseq-pstream
                   :list (next list)
                   :repeats (as-pstream repeats)
                   :offset (pattern-as-pstream offset))))

(defmethod next ((pseq pseq-pstream))
  (with-slots (number list offset) pseq
    (when (and (plusp number)
               (zerop (mod number (length list))))
      (decf-remaining pseq))
    (let ((off (next offset)))
      (if (and (not (eop-p off))
               (remaining-p pseq)
               list)
          (elt-wrap list (+ off number))
          eop))))

;;; pser

(defpattern pser (pattern)
  (list
   (length :default :inf)
   (offset :default 0)
   (current-repeats-remaining :state t)
   (current-index :state t))
  :documentation "Sequentially yield values from LIST, yielding a total of LENGTH values.

Example:

;; (next-n (pser '(5 6 7) 2) 3)
;;
;; ;=> (5 6 EOP)

See also: `pseq'")

(defmethod as-pstream ((pser pser))
  (with-slots (list length offset) pser
    (make-instance 'pser-pstream
                   :list (next list)
                   :length (as-pstream length)
                   :offset (pattern-as-pstream offset))))

(defmethod next ((pser pser-pstream))
  (with-slots (list offset current-index) pser
    (let ((remaining (remaining-p pser 'length))
          (off (next offset)))
      (when (or (not remaining)
                (eop-p off))
        (return-from next eop))
      (decf-remaining pser)
      (when (eql :reset remaining)
        (setf current-index 0))
      (prog1
          (elt-wrap list (+ off current-index))
        (incf current-index)))))

;;; pk

(defpattern pk (pattern)
  (key
   (default :default 1))
  :documentation "Yield the value of KEY in the current `*event*' context, returning DEFAULT if that value is nil.

Example:

;; (next-upto-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :foo)))
;; ;=> ((EVENT :FOO 1 :BAR 1) (EVENT :FOO 2 :BAR 2) (EVENT :FOO 3 :BAR 3))

See also: `pbind', `event-value', `*event*'")

(defmethod as-pstream ((pk pk))
  (with-slots (key default) pk
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
  :documentation "Yield random values from LIST.

Example:

;; (next-n (prand '(1 2 3) 5) 6)
;; ;=> (3 2 2 1 1 EOP)

See also: `pxrand', `pwrand', `pwxrand'")

(defmethod as-pstream ((prand prand))
  (with-slots (list length) prand
    (make-instance 'prand-pstream
                   :list (pattern-as-pstream list)
                   :length (as-pstream length))))

(defmethod next ((prand prand-pstream))
  (unless (remaining-p prand 'length)
    (return-from next eop))
  (decf-remaining prand)
  (random-elt (next (slot-value prand 'list))))

;;; pxrand

(defpattern pxrand (pattern)
  (list
   (length :default :inf)
   (last-result :state t)
   (current-repeats-remaining :state t))
  :documentation "Yield random values from LIST, never repeating equal values twice in a row.

Example:

;; (next-upto-n (pxrand '(1 2 3) 4))
;; ;=> (3 1 2 1)

See also: `prand', `pwrand', `pwxrand'"
  :defun (assert (or (not (listp list))
                     (position-if-not (lambda (i) (eql i (car list))) list))
                 (list)
                 "~s's LIST argument must have at least two non-eql elements." 'pxrand))

(defmethod as-pstream ((pxrand pxrand))
  (with-slots (list length) pxrand
    (make-instance 'pxrand-pstream
                   :list (pattern-as-pstream list)
                   :length (as-pstream length))))

(defmethod next ((pxrand pxrand-pstream))
  (with-slots (list last-result) pxrand
    (unless (remaining-p pxrand 'length)
      (return-from next eop))
    (decf-remaining pxrand)
    (let ((clist (next list)))
      (setf last-result (loop :for res := (random-elt clist)
                              :if (or (not (slot-boundp pxrand 'last-result))
                                      (not (eql res last-result)))
                                :return res)))))

;;; pwrand

(defpattern pwrand (pattern)
  (list
   (weights :default :equal)
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Yield random elements from LIST weighted by respective values from WEIGHTS.

Example:

;; (next-upto-n (pwrand '(1 2 3) '(7 5 3) 10))
;; ;=> (1 1 2 2 2 1 2 1 1 3)

See also: `prand', `pxrand', `pwxrand'")

(defmethod as-pstream ((pwrand pwrand))
  (with-slots (list weights length) pwrand
    (make-instance 'pwrand-pstream
                   :list (pattern-as-pstream list)
                   :weights (pattern-as-pstream weights)
                   :length (as-pstream length))))

(defmethod next ((pwrand pwrand-pstream))
  (with-slots (list weights) pwrand
    (unless (remaining-p pwrand 'length)
      (return-from next eop))
    (decf-remaining pwrand)
    (let* ((clist (next list))
           (cweights (cumulative-list (if (eql weights :equal)
                                          (let ((len (length clist)))
                                            (make-list len :initial-element (/ 1 len)))
                                          (normalized-sum (mapcar #'next (next weights))))))
           (num (random 1.0)))
      (nth (index-of-greater-than num cweights) clist))))

;;; pwxrand

(defpattern pwxrand (pattern)
  (list
   (weights :default :equal)
   (length :default :inf)
   (last-result :state t)
   (current-repeats-remaining :state t))
  :documentation "Yield random elements from LIST weighted by respective values from WEIGHTS, never repeating equivalent values twice in a row. This is effectively `pxrand' and `pwrand' combined.

Example:

;; (next-upto-n (pwxrand '(1 2 3) '(7 5 3) 10))
;; ;=> (1 2 1 2 1 3 1 2 1 2)

See also: `prand', `pxrand', `pwrand'"
  ;; FIX: maybe also take the weights into account to see that it doesn't get stuck?
  :defun (assert (or (not (listp list))
                     (position-if-not (lambda (i) (eql i (car list))) list))
                 (list)
                 "~s's input list must have at least two non-eql elements" 'pwxrand))

(defmethod as-pstream ((pwxrand pwxrand))
  (with-slots (list weights length) pwxrand
    (make-instance 'pwxrand-pstream
                   :list (pattern-as-pstream list)
                   :weights (pattern-as-pstream weights)
                   :length (as-pstream length))))

(defmethod next ((pwxrand pwxrand-pstream))
  (with-slots (list weights last-result) pwxrand
    (unless (remaining-p pwxrand 'length)
      (return-from next eop))
    (decf-remaining pwxrand)
    (let* ((clist (next list))
           (cweights (cumulative-list (if (eql weights :equal)
                                          (let ((len (length clist)))
                                            (make-list len :initial-element (/ 1 len)))
                                          (normalized-sum (mapcar #'next (next weights)))))))
      (setf last-result (loop :for res := (nth (index-of-greater-than (random 1.0) cweights) clist)
                              :if (or (not (slot-boundp pwxrand 'last-result))
                                      (not (eql res last-result)))
                                :return res)))))

;;; pfunc

(defpattern pfunc (pattern)
  (func
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Yield the result of evaluating FUNC. Note that the current event of the parent pattern is still accessible via the `*event*' special variable.

Example:

;; (next-upto-n (pfunc (lambda () (random 10)) 4))
;; ;=> ((5 2 8 9))
;;
;; (next-upto-n (pbind :foo (pwhite 0 10 4) :bar (pfunc (lambda () (if (> (event-value *event* :foo) 5) :greater :lesser)))))
;; ;=> ((EVENT :FOO 0 :BAR :LESSER) (EVENT :FOO 6 :BAR :GREATER)
;;      (EVENT :FOO 7 :BAR :GREATER) (EVENT :FOO 8 :BAR :GREATER))

See also: `pf', `pnary', `plazy', `pif'"
  :defun (check-type func function))

(defmethod as-pstream ((pfunc pfunc))
  (with-slots (func length) pfunc
    (make-instance 'pfunc-pstream
                   :func func
                   :length (as-pstream length))))

(defmethod next ((pfunc pfunc-pstream))
  (unless (remaining-p pfunc 'length)
    (return-from next eop))
  (decf-remaining pfunc)
  (funcall (slot-value pfunc 'func)))

;;; pf

(defmacro pf (&body body)
  "Convenience macro for `pfunc' that automatically wraps BODY in a lambda."
  `(pfunc (lambda () ,@body)))

(pushnew 'pf *patterns*)

;;; pr

(defpattern pr (pattern)
  (pattern
   (repeats :default :inf)
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t))
  :documentation "Repeat each value from PATTERN REPEATS times. If REPEATS is 0, the value is skipped.

Example:

;; (next-upto-n (pr (pseries) (pseq '(1 3 0 2) 1)))
;; ;=> (0 1 1 1 3 3)

See also: `pdurstutter', `pn', `pdrop', `parp'")

(defmethod as-pstream ((pr pr))
  (with-slots (pattern repeats) pr
    (make-instance 'pr-pstream
                   :pattern (as-pstream pattern)
                   :repeats (pattern-as-pstream repeats))))

(defmethod next ((pr pr-pstream))
  (with-slots (pattern repeats current-value current-repeats-remaining) pr
    (while (or (not (slot-boundp pr 'current-repeats-remaining))
               (and current-repeats-remaining
                    current-value
                    (not (value-remaining-p current-repeats-remaining))))
      (setf current-value (next pattern))
      (when (eop-p current-value)
        (return-from next eop))
      (setf current-repeats-remaining
            (let ((*event* (if (event-p current-value)
                               (if *event*
                                   (combine-events *event* current-value)
                                   current-value)
                               *event*)))
              (if (typep repeats 'function)
                  (let ((arglist (function-arglist repeats)))
                    (if (null arglist)
                        (funcall repeats)
                        (funcall repeats current-value)))
                  (next repeats)))))
    (when (value-remaining-p current-repeats-remaining)
      (decf-remaining pr)
      current-value)))

;;; plazy

(defpattern plazy (pattern)
  (func
   (repeats :default :inf)
   (current-pstream :state t :initform nil)
   (current-repeats-remaining :state t :initform nil))
  :documentation "Evaluates FUNC to generate a pattern, which is then yielded from until its end, at which point FUNC is evaluated again to generate the next pattern. The pattern is generated a total of REPEATS times.

Example:

;; (next-n (plazy (lambda () (if (= 0 (random 2)) (pseq '(1 2 3) 1) (pseq '(9 8 7) 1)))) 6)
;; ;=> (9 8 7 1 2 3)

See also: `pfunc'")

(defmethod as-pstream ((plazy plazy))
  (with-slots (func repeats) plazy
    (make-instance 'plazy-pstream
                   :func func
                   :repeats (as-pstream repeats))))

(defmethod next ((plazy plazy-pstream))
  (with-slots (func repeats current-pstream current-repeats-remaining) plazy
    (labels ((set-current-pstream ()
               (setf current-pstream (as-pstream (funcall func)))
               (decf-remaining plazy)))
      (when (null current-repeats-remaining)
        (setf current-repeats-remaining (next repeats)))
      (when (null current-pstream)
        (set-current-pstream))
      (unless (remaining-p plazy)
        (return-from next eop))
      (let ((nv (next current-pstream)))
        (if (eop-p nv)
            (progn
              (set-current-pstream)
              (next current-pstream))
            nv)))))

;;; protate

(defpattern protate (pattern)
  (pattern
   (shift :default 0))
  :documentation "Rotate PATTERN N outputs forward or backward, wrapping the shifted items to the other side, a la `alexandria:rotate'.

Example:

;; (next-upto-n (protate (pseq '(1 2 3 4 5) 1) 2))
;; ;=> (4 5 1 2 3)

See also: `pdrop', `phistory', `pscratch'")

(defmethod as-pstream ((protate protate))
  (with-slots (pattern shift) protate
    (make-instance 'protate-pstream
                   :pattern (pattern-as-pstream pattern)
                   :shift (pattern-as-pstream shift))))

(defmethod next ((protate protate-pstream))
  (with-slots (pattern shift number) protate
    (when (zerop number)
      (next-upto-n pattern))
    (let ((actual-index (- number (next shift)))
          (hn (slot-value pattern 'history-number)))
      (if (>= number (1- hn))
          eop
          (pstream-elt pattern (mod actual-index (1- hn)))))))

;;; pn

(defpattern pn (pattern)
  (pattern
   (repeats :default :inf)
   (current-repeats-remaining :state t)
   (current-pstream :state t :initform nil))
  :documentation "Embed the full PATTERN into the pstream REPEATS times.

Example:

;; (next-upto-n (pn (pwhite 0 5 1) 5))
;; ;=> (2 4 2 1 0)

See also: `pr'")

(defmethod as-pstream ((pn pn)) ;; need this so that PATTERN won't be automatically converted to a pstream when the pn is.
  (with-slots (pattern repeats) pn
    (make-instance 'pn-pstream
                   :pattern pattern
                   :repeats (as-pstream repeats))))

(defmethod next ((pn pn-pstream))
  (with-slots (pattern current-pstream) pn
    (let ((rem (remaining-p pn)))
      (when (eql :reset rem)
        (setf current-pstream (as-pstream pattern)))
      (let ((nv (next current-pstream)))
        (while (and (eop-p nv) rem)
          (decf-remaining pn)
          (setf rem (remaining-p pn))
          (setf current-pstream (as-pstream pattern))
          (setf nv (next current-pstream)))
        (if rem
            nv
            eop)))))

;;; pshuf

(defpattern pshuf (pattern)
  (list
   (repeats :default :inf)
   (shuffled-list :state t)
   (current-repeats-remaining :state t))
  :documentation "Shuffle LIST, then yield each item from the shuffled list, repeating it REPEATS times.

Example:

;; (next-upto-n (pshuf '(1 2 3) 2))
;;
;; ;=> (3 1 2 3 1 2)

See also: `prand'")

(defmethod as-pstream ((pshuf pshuf))
  (with-slots (list repeats) pshuf
    (let ((list (typecase list
                  (pattern (next-upto-n list))
                  (function (funcall list))
                  (list list))))
      (make-instance 'pshuf-pstream
                     :list (next list)
                     :repeats (as-pstream repeats)))))

(defmethod next ((pshuf pshuf-pstream))
  (with-slots (list number shuffled-list) pshuf
    (when (and (zerop (mod number (length list)))
               (plusp number))
      (decf-remaining pshuf))
    (let ((rem (remaining-p pshuf)))
      (unless rem
        (return-from next eop))
      (when (eql :reset rem)
        (setf shuffled-list (shuffle (copy-list list)))) ;; alexandria:shuffle destructively modifies the list, so we use copy-list in case the user provided a quoted list as input.
      (nth (mod number (length shuffled-list))
           shuffled-list))))

;;; pwhite

(defpattern pwhite (pattern)
  ((lo :default 0)
   (hi :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Linearly-distributed random numbers between LO and HI, inclusive.

Example:

;; (next-upto-n (pwhite 0 10 16))
;; ;=> (7 2 4 5 7 10 4 8 10 2 3 5 9 2 5 4)

See also: `pexprand', `pbrown', `pgauss', `prand'"
  ;; if only one argument is provided, we use it as the "hi" value
  :defun (defun pwhite (&optional (lo 0 lo-provided-p) (hi 1 hi-provided-p) (length :inf))
           (make-instance 'pwhite
                          :lo (if hi-provided-p
                                  lo
                                  0)
                          :hi (if hi-provided-p
                                  hi
                                  (if lo-provided-p
                                      lo
                                      1))
                          :length length)))

(defmethod as-pstream ((pwhite pwhite))
  (with-slots (lo hi length) pwhite
    (make-instance 'pwhite-pstream
                   :lo (pattern-as-pstream lo)
                   :hi (pattern-as-pstream hi)
                   :length (as-pstream length))))

(defmethod next ((pwhite pwhite-pstream))
  (with-slots (lo hi) pwhite
    (unless (remaining-p pwhite 'length)
      (return-from next eop))
    (decf-remaining pwhite)
    (let ((nlo (next lo))
          (nhi (next hi)))
      (when (or (eop-p nlo)
                (eop-p nhi))
        (return-from next eop))
      (random-range nlo nhi))))

;;; pbrown

(defpattern pbrown (pattern)
  ((lo :default 0.0)
   (hi :default 1.0)
   (step :default 0.125)
   (length :default :inf)
   (current-repeats-remaining :state t)
   (current-value :state t :initform nil))
  :documentation "Brownian motion within a range; each output randomly a maximum of STEP away from the previous. LO and HI define the lower and upper bounds of the range. STEP defaults to 1 if LO and HI are integers.

Example:

;; (next-upto-n (pbrown 0 10 1 10))
;; ;=> (2 3 3 3 4 3 4 5 6 5)

See also: `pwhite', `pexprand', `pgauss'"
  ;; if only one argument is provided, we use it as the "hi" value
  :defun (defun pbrown (&optional (lo 0.0 lo-provided-p) (hi 1.0 hi-provided-p) (step 0.125 step-provided-p) (length :inf))
           (let ((lo (if hi-provided-p
                         lo
                         (if (integerp lo) 0 0.0)))
                 (hi (if hi-provided-p
                         hi
                         (if lo-provided-p
                             lo
                             1))))
             (make-instance 'pbrown
                            :lo lo
                            :hi hi
                            :step (if step-provided-p
                                      step
                                      (if (and (integerp lo)
                                               (integerp hi))
                                          1
                                          0.125))
                            :length length))))

(defmethod as-pstream ((pbrown pbrown))
  (with-slots (lo hi step length) pbrown
    (make-instance 'pbrown-pstream
                   :lo (pattern-as-pstream lo)
                   :hi (pattern-as-pstream hi)
                   :step (pattern-as-pstream step)
                   :length (as-pstream length))))

(defmethod next ((pbrown pbrown-pstream))
  (unless (remaining-p pbrown 'length)
    (return-from next eop))
  (decf-remaining pbrown)
  (with-slots (lo hi step current-value) pbrown
    (let ((nlo (next lo))
          (nhi (next hi))
          (nstep (next step)))
      (when (member eop (list nlo nhi nstep))
        (return-from next eop))
      (unless current-value
        (setf current-value (random-range nlo nhi)))
      (incf current-value (random-range (* -1 nstep) nstep))
      (setf current-value (clamp current-value nlo nhi)))))

;;; pexprand
;; FIX: integer inputs should result in integer outputs

(defpattern pexprand (pattern)
  ((lo :default 0.0001)
   (hi :default 1.0)
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Exponentially-distributed random numbers between LO and HI. Note that LO and HI cannot be 0, and that LO and HI must have the same sign or else complex numbers will be output.

Example:

;; (next-upto-n (pexprand 1.0 8.0 4))
;; ;=> (1.0420843091865208d0 1.9340168112124456d0 2.173209129035095d0 4.501371557329618d0)

See also: `pwhite', `pbrown', `pgauss', `prand'"
  :defun (defun pexprand (&optional (lo 0.0001) (hi 1.0) (length :inf))
           (assert (and (numberp lo)
                        (not (zerop lo)))
                   (lo)
                   "~s's LO argument must be a nonzero number; got ~s" 'pexprand lo)
           (assert (and (numberp hi)
                        (not (zerop hi)))
                   (hi)
                   "~s's HI argument must be a nonzero number; got ~s" 'pexprand hi)
           (make-instance 'pexprand
                          :lo lo
                          :hi hi
                          :length length)))

(defmethod as-pstream ((pexprand pexprand))
  (with-slots (lo hi length) pexprand
    (make-instance 'pexprand-pstream
                   :lo (pattern-as-pstream lo)
                   :hi (pattern-as-pstream hi)
                   :length (as-pstream length))))

(defmethod next ((pexprand pexprand-pstream))
  (with-slots (lo hi) pexprand
    (unless (remaining-p pexprand 'length)
      (return-from next eop))
    (decf-remaining pexprand)
    (let ((nlo (next lo))
          (nhi (next hi)))
      (when (or (eop-p nlo)
                (eop-p nhi))
        (return-from next eop))
      (exponential-random-range nlo nhi))))

;;; pgauss

(defpattern pgauss (pattern)
  ((mean :default 0.0)
   (deviation :default 1.0)
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Random numbers distributed along a normal (gaussian) curve. MEAN is the \"center\" of the distribution, DEVIATION is the standard deviation (i.e. the higher the value, the further the outputs are spread from MEAN).

Example:

;; (next-n (pgauss) 4)
;; ;=> (0.08918811646370092d0 0.1745957067161632d0 0.7954678768273173d0 -1.2215823449671597d0)

See also: `pwhite', `pexprand', `pbrown'")

(defmethod as-pstream ((pgauss pgauss))
  (with-slots (mean deviation length) pgauss
    (make-instance 'pgauss-pstream
                   :mean (pattern-as-pstream mean)
                   :deviation (pattern-as-pstream deviation)
                   :length (as-pstream length))))

(defmethod next ((pgauss pgauss-pstream))
  (with-slots (mean deviation) pgauss
    (unless (remaining-p pgauss 'length)
      (return-from next eop))
    (decf-remaining pgauss)
    (let ((nmean (next mean))
          (ndev (next deviation)))
      (when (or (eop-p nmean)
                (eop-p ndev))
        (return-from next eop))
      (random-gauss nmean ndev))))

;;; pseries

(defpattern pseries (pattern)
  ((start :default 0)
   (step :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t)
   (current-value :state t))
  :documentation "Yield START, then generate subsequent outputs by adding STEP, for a total of LENGTH outputs.

Example:

;; (next-upto-n (pseries 1 2 4))
;; ;=> (1 3 5 7)

See also: `pseries*', `pgeom', `paccum'")

(defmethod as-pstream ((pseries pseries))
  (with-slots (start step length) pseries
    (make-instance 'pseries-pstream
                   :start (pattern-as-pstream start)
                   :step (pattern-as-pstream step)
                   :length (as-pstream length))))

(defmethod next ((pseries pseries-pstream))
  (with-slots (start step current-value) pseries
    (unless (slot-boundp pseries 'current-value)
      (setf current-value (next start)))
    (unless (and (remaining-p pseries 'length)
                 current-value)
      (return-from next eop))
    (decf-remaining pseries)
    (let ((nxt (next step)))
      (prog1
          current-value
        (if (numberp nxt)
            (incf current-value nxt) ;; FIX: current-value should be CURRENT value, not the next one! also write tests for this!
            (setf current-value eop))))))

;;; pseries*

(defun pseries* (&optional (start 0) (end 1) length)
  "Syntax sugar to generate a `pseries' whose values go from START to END linearly over LENGTH steps. If LENGTH is not provided, it is calculated such that the step will be 1. Note that LENGTH cannot be infinite since delta calculation requires dividing by it.

Based on the Pseries extension from the ddwPatterns SuperCollider library.

Example:

;; (pseries* 0 10 16)
;; ;=> (pseries 0 2/3 16)
;;
;; (next-upto-n *)
;; ;=> (0 2/3 4/3 2 8/3 10/3 4 14/3 16/3 6 20/3 22/3 8 26/3 28/3 10)

See also: `pseries', `pgeom', `pgeom*'"
  (check-type length (or null (integer 2) pattern))
  (let ((length (or length
                    (max 2 (round (1+ (abs (- end start))))))))
    (pseries start (/ (- end start) (1- length)) length)))

(pushnew 'pseries* *patterns*)

;;; pgeom

(defpattern pgeom (pattern)
  ((start :default 1)
   (grow :default 2)
   (length :default :inf)
   (current-repeats-remaining :state t)
   (current-value :state t))
  :documentation "Yield START, then generate subsequent outputs by multiplying by GROW, for a total of LENGTH outputs.

Example:

;; (next-upto-n (pgeom 1 2 4))
;; ;=> (1 2 4 8)

See also: `pseries', `paccum'")

(defmethod as-pstream ((pgeom pgeom))
  (with-slots (start grow length) pgeom
    (make-instance 'pgeom-pstream
                   :start (pattern-as-pstream start)
                   :grow (pattern-as-pstream grow)
                   :length (as-pstream length))))

(defmethod next ((pgeom pgeom-pstream))
  (with-slots (start grow current-value) pgeom
    (unless (slot-boundp pgeom 'current-value)
      (setf current-value (next start)))
    (unless (remaining-p pgeom 'length)
      (return-from next eop))
    (decf-remaining pgeom)
    (if (zerop (slot-value pgeom 'number))
        current-value
        (let ((n (next grow)))
          (if (eop-p n)
              eop
              (setf current-value (* current-value n)))))))

;;; pgeom*

(defun pgeom* (&optional (start 0.01) (end 1) (length 16))
  "Syntax sugar to generate a `pgeom' whose values go from START to END exponentially over LENGTH steps. LENGTH cannot be infinite since delta calculation requires dividing by it.

Based on the Pgeom extension from the ddwPatterns SuperCollider library.

Example:

;; (pgeom* 1 100 8)
;; ;=> (pgeom 1 1.9306977 8)
;;
;; (next-upto-n *)
;; ;=> (1 1.9306977 3.7275934 7.196856 13.894953 26.826954 51.79474 99.999985)
;; ;; Note that due to floating point rounding errors the last output may not always be exactly END.

See also: `pgeom', `pseries', `pseries*'"
  (check-type length (or (integer 2) pattern))
  (pgeom start (expt (/ end start) (/ 1 (1- length))) length))

(pushnew 'pgeom* *patterns*)

;;; ptrace

(defpattern ptrace (pattern)
  ((trace :default t)
   (prefix :default nil)
   (stream :default t))
  :documentation "Print the PREFIX and each output of TRACE to STREAM. If TRACE is t, print `*event*'. If TRACE is a different symbol, print the value of that symbol in `*event*'. If TRACE is a pattern, ptrace yields its output unaffected. Otherwise, it yields t.

See also: `debug-recent-events'")

(defmethod as-pstream ((ptrace ptrace))
  (with-slots (trace prefix stream) ptrace
    (make-instance 'ptrace-pstream
                   :trace (pattern-as-pstream trace)
                   :prefix (pattern-as-pstream prefix)
                   :stream (pattern-as-pstream stream))))

(defmethod next ((ptrace ptrace-pstream))
  (with-slots (trace prefix stream) ptrace
    (let ((prefix (next prefix))
          (stream (next stream)))
      (if (eql trace t)
          (progn
            (format stream "~&~@[~a ~]~s~%" prefix *event*)
            t)
          (typecase trace
            ((or list symbol)
             (progn
               (format stream "~&~@[~a ~]~{~{~s: ~s~}~#[~:; ~]~}~%" prefix
                       (mapcar (lambda (symbol)
                                 (list symbol (event-value *event* symbol)))
                               (ensure-list trace)))
               t))
            (pattern
             (let ((res (next trace)))
               (format stream "~&~@[~a ~]~s~%" prefix res)
               res)))))))

;;; place

(defpattern place (pattern)
  (list
   (repeats :default :inf)
   (current-repeat :state t)
   (current-repeats-remaining :state t))
  :documentation "Yield each value from LIST in sequence. If the value is a list, the first element of that list is yielded. The second time that sub-list is encountered, its second element will be yielded; the third time, the third element, and so on. REPEATS controls the number of times LIST is repeated.

Example:

;; (next-upto-n (place (list 1 2 (list 3 4 5)) 3))
;; ;=> (1 2 3 1 2 4 1 2 5)

See also: `ppatlace'")

(defmethod as-pstream ((place place))
  (with-slots (list repeats) place
    (make-instance 'place-pstream
                   :list (next list)
                   :repeats (as-pstream repeats)
                   :current-repeat 0)))

(defmethod next ((place place-pstream))
  (with-slots (number list current-repeat) place
    (when (and (not (= number 0))
               (= 0 (mod number (length list))))
      (incf current-repeat)
      (decf-remaining place))
    (unless (if (plusp number)
                (and (not (ended-p place))
                     (remaining-p place))
                (remaining-p place))
      (return-from next eop))
    (let* ((mod (mod number (length list)))
           (result (next (nth mod list))))
      (if (listp result)
          (elt-wrap result current-repeat)
          result))))

;;; ppatlace

(defpattern ppatlace (pattern)
  (list
   (repeats :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Yield each value from LIST in sequence, or one output from each pattern in LIST per cycle of the list. If one of the patterns embedded in LIST ends sooner than the others, it is simply removed and the ppatlace continues to yield from the rest of the LIST. The entire LIST is yielded through a total of REPEATS times.

Example:

;; (next-upto-n (ppatlace (list (pseq (list 1 2 3) 1)
;;                              (pseq (list 4 5 6) 2))))
;; ;=> (1 4 2 5 3 6 4 5 6)

See also: `place'")

(defmethod as-pstream ((ppatlace ppatlace))
  (with-slots (repeats list) ppatlace
    (make-instance 'ppatlace-pstream
                   :list (mapcar #'pattern-as-pstream list)
                   :repeats (as-pstream repeats))))

(defmethod next ((ppatlace ppatlace-pstream))
  (with-slots (number list) ppatlace
    (when (and (not (zerop number))
               (not (zerop (length list)))
               (zerop (mod number (length list))))
      (decf-remaining ppatlace))
    (when (or (not list)
              (and (plusp number)
                   (ended-p ppatlace))
              (not (remaining-p ppatlace)))
      (return-from next eop))
    (let ((result (next (elt-wrap list number))))
      (unless (eop-p result)
        (return-from next result))
      (setf list (remove-if #'ended-p list))
      (next ppatlace))))

;;; pnary

(defpattern pnary (pattern)
  (operator
   (patterns :initarg :patterns))
  :documentation "Yield the result of applying OPERATOR to each value yielded by each pattern in PATTERNS.

Example:

;; (next-upto-n (pnary (pseq (list '+ '- '* '/) 2) 2 2))
;; ;=> (4 0 4 1)

See also: `pfunc', `p+', `p-', `p*', `p/'"
  :defun (defun pnary (operator &rest patterns)
           (make-instance 'pnary
                          :operator operator
                          :patterns patterns)))

(defmethod as-pstream ((pnary pnary))
  (with-slots (operator patterns) pnary
    (make-instance 'pnary-pstream
                   :operator (pattern-as-pstream operator)
                   :patterns (mapcar #'pattern-as-pstream patterns))))

(defmethod next ((pnary pnary-pstream))
  (with-slots (operator patterns) pnary
    (let ((op (if (pstream-p operator)
                  (next operator)
                  operator))
          (nexts (mapcar #'next patterns)))
      (when (or (position eop nexts)
                (eop-p op))
        (return-from next eop))
      (restart-case
          (handler-bind
              ((type-error
                 (lambda (c)
                   (when-let ((restart (find-restart 'retry-with-prest-values c)))
                     (invoke-restart restart)))))
            (apply #'multi-channel-funcall op nexts))
        (retry-with-prest-values ()
          :test (lambda (c)
                  (and (typep c 'type-error)
                       (eql 'number (type-error-expected-type c))
                       (typep (type-error-datum c) 'prest)))
          (labels ((replace-prests (list)
                     (mapcar (lambda (item)
                               (typecase item
                                 (list (replace-prests item))
                                 (prest (slot-value item 'value))
                                 (t item)))
                             list)))
            (apply #'multi-channel-funcall op (replace-prests nexts))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-pattern-for-function (function)
    "Generate Lisp of a wrapper function named pFUNCTION whose definition is (pnary FUNCTION ...).

See also: `pnary'"
    (let* ((pat-sym (intern (concat 'p function) 'cl-patterns))
           (argslist (function-arglist function))
           (func-name (string-downcase function))
           (full-func-name (if (eql (find-package 'cl-patterns) (symbol-package function))
                               func-name
                               (concat (string-downcase (package-name (symbol-package function))) ":" func-name)))
           (parsed (multiple-value-list (parse-ordinary-lambda-list argslist)))
           (has-rest (third parsed))
           (args (append (first parsed) (mapcar #'car (second parsed)) (ensure-list (third parsed)))))
      `(progn
         (defun ,pat-sym ,argslist
           ,(concat "Syntax sugar for (pnary #'" func-name " ...).

See also: `pnary', `" full-func-name "'")
           (,(if has-rest
                 'apply
                 'funcall)
            #'pnary #',function ,@args))
         (pushnew ',pat-sym *patterns*)))))

#.`(progn ,@(mapcar #'make-pattern-for-function '(+ - * / > >= < <= = /= eql wrap)))

;;; prerange

(defpattern prerange (pattern)
  (input
   from-range
   to-range)
  :documentation "Remap INPUT from one range, specified by FROM-RANGE, to another range, specified by TO-RANGE.

Note that this is effectively a convenience wrapper over `pnary' and `rerange'; thus you should see `rerange' for more information.

See also: `rerange', `pnary'")

(defmethod as-pstream ((prerange prerange))
  (with-slots (input from-range to-range) prerange
    (make-instance 'prerange-pstream
                   :input (pattern-as-pstream input)
                   :from-range (pattern-as-pstream from-range)
                   :to-range (pattern-as-pstream to-range))))

(defmethod next ((prerange prerange-pstream))
  (with-slots (input from-range to-range) prerange
    (let ((input (next input))
          (from-range (next from-range))
          (to-range (next to-range)))
      (when (member eop (list input from-range to-range))
        (return-from next eop))
      (rerange input from-range to-range))))

;;; pslide

(defpattern pslide (pattern)
  (list
   (repeats :default :inf)
   (len :default 3)
   (step :default 1)
   (start :default 0)
   (wrap-at-end :default t)
   (current-repeats-remaining :state t)
   (current-repeats :state t :initform nil)
   (remaining-current-segment :state t :initform nil)
   (current-value :state t :initform nil)
   (current-list-length :state t :initform nil))
  :documentation "\"Slide\" across sections of LIST. REPEATS is the total number of sections to output, LEN the length of the section. STEP is the number to increment the start index by after each section, and START is the initial index into LIST that the first section starts from. WRAP-AT-END, when true, means that an index outside of the list will wrap around. When false, indexes outside of the list result in nil.

Example:

;; (next-upto-n (pslide (list 0 1 2 3 4 5 6) 3 3 2 1 t))
;; ;=> (1 2 3 3 4 5 5 6 0)

See also: `pscratch'")

(defmethod as-pstream ((pslide pslide))
  (with-slots (list repeats len step start wrap-at-end) pslide
    (make-instance 'pslide-pstream
                   :list (next list)
                   :repeats (pattern-as-pstream repeats)
                   :len (pattern-as-pstream len)
                   :step (pattern-as-pstream step)
                   :start (pattern-as-pstream start)
                   :wrap-at-end (next wrap-at-end)
                   :current-repeats 0
                   :remaining-current-segment len)))

(defmethod next ((pslide pslide-pstream))
  (with-slots (list repeats len step start wrap-at-end current-repeats-remaining current-repeats remaining-current-segment current-value current-list-length) pslide
    (unless current-value
      (setf current-value (next start)))
    (unless current-list-length
      (setf current-list-length (length list)))
    (labels ((get-next ()
               (if (and (not wrap-at-end)
                        (or (minusp current-value)
                            (>= current-value current-list-length)))
                   eop
                   (elt-wrap list current-value))))
      (unless (slot-boundp pslide 'current-repeats-remaining)
        (setf current-repeats-remaining (next repeats)))
      (unless (value-remaining-p current-repeats-remaining)
        (return-from next eop))
      (if (value-remaining-p remaining-current-segment)
          (prog1
              (get-next)
            (decf-remaining pslide 'remaining-current-segment)
            (incf current-value))
          (progn
            (decf-remaining pslide)
            (setf remaining-current-segment (next len))
            (incf current-repeats)
            (setf current-value (+ (next start) (* (next step) current-repeats)))
            (next pslide))))))

;;; phistory

(defpattern phistory (pattern)
  (pattern
   step-pattern)
  :documentation "Refer back to PATTERN's history, yielding the output at the index provided by STEP-PATTERN.

Note that PATTERN is still advanced once per event, and if STEP-PATTERN yields a number pointing to an event in PATTERN that hasn't been yielded yet (i.e. if PATTERN has only advanced once but STEP-PATTERN yields 3 for its output), phistory yields nil.

Example:

;; (next-n (phistory (pseries) (pseq '(0 2 1))) 3)
;; ;=> (0 NIL 1)

See also: `pscratch'")

(defmethod as-pstream ((phistory phistory))
  (with-slots (pattern step-pattern) phistory
    (make-instance 'phistory-pstream
                   :pattern (as-pstream pattern)
                   :step-pattern (pattern-as-pstream step-pattern))))

(defmethod next ((phistory phistory-pstream))
  (with-slots (pattern step-pattern) phistory
    (let ((next-step (next step-pattern)))
      (if (eop-p next-step)
          eop
          (progn
            (next pattern)
            (handler-case (pstream-elt pattern next-step)
              (pstream-out-of-range ()
                nil)))))))

;;; pscratch
;;
;; NOTE: pscratch's mechanism seems to be slightly different:
;; supercollider:
;; > Pscratch(Pseries(0, 1), Pseq([1, 1, 1, -3], inf)).asStream.nextN(12)
;; [ 0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 0 ]
;;
;; lisp:
;; > (next-n (pscratch (pseries 0 1) (pseq (list 1 1 1 -3) :inf)) 12)
;; (0 1 2 3 0 1 2 3 0 1 2 3)
;; FIX: document this in sc-differences.org

(defpattern pscratch (pattern)
  (pattern
   step-pattern
   (current-index :state t :initform 0))
  :documentation "\"Scratches\" across the values yielded by a pstream, similar in concept to how a DJ might scratch a record, altering the normal flow of playback. PATTERN is the source pattern, and STEP-PATTERN determines the increment of the index into the pstream history.

Based on the pattern originally from the ddwPatterns SuperCollider library.

Example:

;; (next-upto-n (pscratch (pseries) (pseq '(0 1 1 -1 2) 2)))
;; ;=> (0 0 1 2 1 3 3 4 5 4)

See also: `phistory'")

(defmethod as-pstream ((pscratch pscratch))
  (with-slots (pattern step-pattern) pscratch
    (make-instance 'pscratch-pstream
                   :pattern (as-pstream pattern)
                   :step-pattern (pattern-as-pstream step-pattern))))

(defmethod next ((pscratch pscratch-pstream))
  (with-slots (pattern step-pattern current-index) pscratch
    (let ((nxt (next step-pattern)))
      (when (eop-p nxt)
        (return-from next eop))
      (prog1
          (pstream-elt-future pattern current-index)
        (setf current-index (max (+ current-index nxt) 0))))))

;;; pif

(defpattern pif (pattern)
  ((test)
   (true)
   (false :default nil))
  :documentation "\"If\" expression for patterns. TEST is evaluated for each step, and if it's non-nil, the value of TRUE will be yielded, otherwise the value of FALSE will be. Note that TRUE and FALSE can be patterns, and if they are, they are only advanced in their respective cases, not for every step.

Example:

;; (next-n (pif (pseq '(t t nil nil nil)) (pseq '(1 2 3)) (pseq '(9 8 7))) 8)
;; ;=> (1 2 9 8 7 3 1 9)

See also: `plazy', `pfunc'")

(defmethod next ((pif pif-pstream))
  (with-slots (test true false) pif
    (let ((nxt (next test)))
      (if (eop-p nxt)
          eop
          (if nxt
              (next true)
              (next false))))))

;;; parp

(defpattern parp (pattern)
  (pattern
   arpeggiator
   (current-pattern-event :state t :initform nil)
   (current-arpeggiator-stream :state t :initform nil))
  :documentation "Arpeggiator pattern; each event yielded by PATTERN is bound to `*event*' and then the entirety of the ARPEGGIATOR pattern is yielded.

Example:

;; (next-upto-n (parp (pbind :foo (pseq '(1 2 3) 1))
;;                    (pbind :bar (pseq '(4 5 6) 1))))
;; ;=> ((EVENT :FOO 1 :BAR 4) (EVENT :FOO 1 :BAR 5) (EVENT :FOO 1 :BAR 6)
;;      (EVENT :FOO 2 :BAR 4) (EVENT :FOO 2 :BAR 5) (EVENT :FOO 2 :BAR 6)
;;      (EVENT :FOO 3 :BAR 4) (EVENT :FOO 3 :BAR 5) (EVENT :FOO 3 :BAR 6))

See also: `psym', `pmeta', `pr'")

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
    (when (eop-p current-pattern-event)
      (return-from next eop))
    (let ((nxt (let ((*event* (combine-events (make-default-event)
                                              current-pattern-event)))
                 (next current-arpeggiator-stream))))
      (unless (eop-p nxt)
        (return-from next nxt))
      (setf current-pattern-event (next pattern)
            current-arpeggiator-stream (as-pstream arpeggiator))
      (next parp))))

;;; pfin

(defpattern pfin (pattern)
  (pattern
   count)
  :documentation "Yield up to COUNT outputs from PATTERN.

Example:

;; (next-n (pfin (pseq '(1 2 3) :inf) 3) 5)
;; ;=> (1 2 3 NIL NIL)

See also: `pfindur'")

(defmethod as-pstream ((pfin pfin))
  (with-slots (count pattern) pfin
    (make-instance 'pfin-pstream
                   :pattern (as-pstream pattern)
                   :count (next count)))) ;; FIX: should be able to use as a gate pattern

(defmethod next ((pfin pfin-pstream))
  (with-slots (pattern count number) pfin
    (unless (< number count)
      (return-from next eop))
    (next pattern)))

;;; pfindur

(defpattern pfindur (pattern)
  (pattern
   dur
   (tolerance :default 0)
   (current-dur :state t)
   (elapsed-dur :state t :initform 0))
  :documentation "Yield events from PATTERN until their total duration is within TOLERANCE of DUR, or greater than DUR. Any events that would end beyond DUR are cut short. If PATTERN outputs numbers, their total sum is limited instead.

Example:

;; (next-n (pfindur (pbind :dur 1 :foo (pseries)) 2) 3)
;; ;=> ((EVENT :DUR 1 :FOO 0) (EVENT :DUR 1 :FOO 1) EOP)
;;
;; (next-upto-n (pfindur (pwhite 0 4) 16))
;; ;=> (1 3 0 1 2 2 1 3 0 1 2)
;; (reduce #'+ *)
;; ;=> 16

See also: `pfin', `psync'")

(defmethod as-pstream ((pfindur pfindur))
  (with-slots (pattern dur tolerance) pfindur
    (make-instance 'pfindur-pstream
                   :pattern (pattern-as-pstream pattern)
                   :dur (as-pstream dur)
                   :tolerance (next tolerance))))

(defmethod next ((pfindur pfindur-pstream))
  (labels ((get-delta (ev)
             (etypecase ev
               (event
                (event-value ev :delta))
               (list
                (reduce #'max (mapcar #'get-delta ev)))
               (number
                ev))))
    (with-slots (pattern dur tolerance current-dur elapsed-dur) pfindur
      (let ((n-event (next pattern)))
        (when (eop-p n-event)
          (return-from next eop))
        (unless (slot-boundp pfindur 'current-dur)
          (setf current-dur (next dur)))
        (when (eop-p current-dur)
          (return-from next eop))
        (let ((res (if (or (eql :inf current-dur)
                           (>= current-dur (+ elapsed-dur (get-delta n-event))))
                       n-event
                       (let ((tdur (- current-dur elapsed-dur)))
                         (when (>= tolerance tdur)
                           (return-from next eop))
                         (if (event-p n-event)
                             (combine-events n-event (event :dur tdur))
                             tdur)))))
          (incf elapsed-dur (get-delta res))
          res)))))

;;; psync

(defpattern psync (pattern)
  (pattern
   sync-quant
   (maxdur :default nil)
   (tolerance :default 0.001)
   (elapsed-dur :state t :initform 0))
  :documentation "Yield events from PATTERN until their total duration is within TOLERANCE of MAXDUR, cutting off any events that would extend past MAXDUR. If PATTERN ends before MAXDUR, a rest is added to the pstream to round its duration up to the nearest multiple of SYNC-QUANT.

Example:

;; (next-upto-n (psync (pbind :dur (pseq '(5) 1)) 4 16))
;;
;; ;=> ((EVENT :DUR 5) (EVENT :TYPE :REST :DUR 3))
;;
;; (next-upto-n (psync (pbind :dur (pseq '(5) 5)) 4 16))
;;
;; ;=> ((EVENT :DUR 5) (EVENT :DUR 5) (EVENT :DUR 5) (EVENT :DUR 5 :DELTA 1))

See also: `pfindur'")

(defmethod as-pstream ((psync psync))
  (with-slots (pattern sync-quant maxdur tolerance) psync
    (make-instance 'psync-pstream
                   :pattern (as-pstream pattern)
                   :sync-quant (next sync-quant)
                   :maxdur (next maxdur)
                   :tolerance (next tolerance))))

(defmethod next ((psync psync-pstream))
  (with-slots (pattern sync-quant maxdur tolerance elapsed-dur) psync
    (when (and maxdur
               (>= elapsed-dur (- maxdur tolerance)))
      (return-from next eop))
    (let* ((nbfq (next-beat-for-quant sync-quant elapsed-dur))
           (n-event (next pattern))
           (n-event (if (eop-p n-event)
                        (let ((diff (- nbfq elapsed-dur)))
                          (if (plusp diff)
                              (event :type :rest :dur diff)
                              (return-from next eop)))
                        n-event))
           (n-event (if maxdur
                        (combine-events n-event (event :dur (min (event-value n-event :dur)
                                                                 (- maxdur elapsed-dur))))
                        n-event)))
      (incf elapsed-dur (event-value n-event :dur))
      n-event)))

;;; pdurstutter
;; FIX: make a version where events skipped with 0 are turned to rests instead (to keep the correct dur)

(defpattern pdurstutter (pattern)
  (pattern
   n
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t :initform 0))
  :documentation "Yield each output from PATTERN N times, dividing it by N. If the output from PATTERN is an event, its dur is divided; if it's a number, the number itself is divided instead of being yielded directly.

Example:

;; (next-n (pdurstutter (pseq '(1 2 3 4 5)) (pseq '(3 2 1 0 2))) 9)
;; ;=> (1/3 1/3 1/3 1 1 3 5/2 5/2 NIL)
;;
;; (next-n (pdurstutter (pbind :dur (pseq '(1 2 3 4 5)))
;;                      (pseq '(3 2 1 0 2)))
;;         9)
;; ;=> ((EVENT :DUR 1/3) (EVENT :DUR 1/3) (EVENT :DUR 1/3) (EVENT :DUR 1) (EVENT :DUR 1) (EVENT :DUR 3) (EVENT :DUR 5/2) (EVENT :DUR 5/2) NIL)

See also: `pr'")

(defmethod as-pstream ((pdurstutter pdurstutter))
  (with-slots (pattern n) pdurstutter
    (make-instance 'pdurstutter-pstream
                   :pattern (as-pstream pattern)
                   :n (pattern-as-pstream n))))

(defmethod next ((pdurstutter pdurstutter-pstream))
  (with-slots (pattern n current-value current-repeats-remaining) pdurstutter
    (while (and current-repeats-remaining
                (zerop current-repeats-remaining))
      (setf current-repeats-remaining (next n))
      (let ((e (next pattern)))
        (when (eop-p current-repeats-remaining)
          (return-from next eop))
        (when (and current-repeats-remaining
                   (not (zerop current-repeats-remaining)))
          (setf current-value (if (eop-p e)
                                  eop
                                  (ctypecase e
                                    (event (combine-events e (event :dur (/ (event-value e :dur) current-repeats-remaining))))
                                    (number (/ e current-repeats-remaining))))))))
    (when current-repeats-remaining
      (decf-remaining pdurstutter)
      current-value)))

;;; pbeat

(defpattern pbeat (pattern)
  ()
  :documentation "Yield the number of beats elapsed since the pbeat was embedded in the pstream.

Example:

;; (next-n (pbind :dur (pseq '(1 2 3)) :foo (pbeat)) 3)
;; ;=> ((EVENT :DUR 1 :FOO 0) (EVENT :DUR 2 :FOO 1) (EVENT :DUR 3 :FOO 3))

See also: `pbeat*', `beat', `prun'")

(defmethod next ((pbeat pbeat-pstream))
  (beat (pattern-parent pbeat :class 'pbind)))

;;; pbeat*

(defpattern pbeat* (pattern)
  ((task :state t :initform nil))
  :documentation "Yield the number of beats on the `*clock*' of the current pattern. In other words, pbeat* is clock-synced, unlike `pbeat', which is pattern-synced.

Example:

;; (next-n (pbind :dur (pseq '(1 2 3)) :foo (pbeat*)) 3)
;; ;=> ((EVENT :DUR 1 :FOO 200) (EVENT :DUR 2 :FOO 201) (EVENT :DUR 3 :FOO 203))

See also: `pbeat', `beat', `prun'")

(defmethod next ((pbeat* pbeat*-pstream))
  (beat *clock*))

;;; ptime

(defpattern ptime (pattern)
  ((last-beat-checked :state t :initform nil)
   (tempo-at-beat :state t :initform nil)
   (elapsed-time :state t :initform 0))
  :documentation "Yield the number of seconds elapsed since ptime was embedded in the pstream.

Note: May give inaccurate results if the clock's tempo changes occur more frequently than events in the parent pbind.

Example:

;; (setf (tempo *clock*) 1) ;; 60 BPM
;; (next-n (pbind :dur 1 :time (ptime)) 2)
;; ;=> ((EVENT :DUR 1 :TIME 0) (EVENT :DUR 1 :TIME 1.0))

See also: `pbeat', `prun', `beat'")

(defmethod next ((ptime ptime-pstream)) ;; FIX: take into account the previous tempo if it has been changed since the last-beat-checked.
  (with-slots (last-beat-checked tempo-at-beat elapsed-time) ptime
    (with-slots (tempo) *clock*
      (let ((beat (beat (pattern-parent ptime :class 'pbind))))
        (prog1
            (incf elapsed-time (if (null last-beat-checked)
                                   0
                                   (dur-time (- beat last-beat-checked) tempo)))
          (setf last-beat-checked beat
                tempo-at-beat tempo))))))

;;; pindex
;; TODO: alternate version that only calls #'next on index-pat each time the pattern-as-pstream of list-pat has ended.
;; TODO: pindex1 that only embeds 1 element from subpatterns, a la SuperCollider's Pswitch1.

(defpattern pindex (pattern)
  (list-pat
   index-pat
   (wrap-p :default nil))
  :documentation "Use INDEX-PAT to index into the list returned by LIST-PAT. WRAP-P is whether indexes that are out of range will be wrapped (if t) or will simply return nil.

Example:

;; (next-n (pindex (list 99 98 97) (pseq (list 0 1 2 3))) 4)
;;
;; ;=> (99 98 97 NIL)
;;
;; (next-upto-n (pindex (list 99 98 97) (pseries 0 1 6) t))
;;
;; ;=> (99 98 97 99 98 97)

See also: `pwalk'")

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
      (when (or (eop-p idx)
                (eop-p list))
        (return-from next eop))
      (funcall (if wrap-p 'nth-wrap 'nth) idx list))))

;;; prun

(defpattern prun (pattern)
  (pattern
   (dur :default 1)
   (current-dur :state t))
  :documentation "Run PATTERN \"independently\" of its parent, holding each value for DUR beats. Each of PATTERN's outputs is treated as if it lasted DUR beats, being continuously yielded during that time before moving on to the next output.

Example:

;; (next-upto-n (pbind :foo (pseq '(1 2 3 4 5))
;;                     :bar (prun (pseq '(4 5 6 7 8))
;;                                (pseq '(1 2 0.5 0.5 1)))))
;; ;=> ((EVENT :FOO 1 :BAR 4)
;;      (EVENT :FOO 2 :BAR 5)
;;      (EVENT :FOO 3 :BAR 5)
;;      (EVENT :FOO 4 :BAR 6)
;;      (EVENT :FOO 5 :BAR 8))

See also: `beat', `pbeat'")

(defmethod as-pstream ((prun prun))
  (with-slots (pattern dur) prun
    (unless (pattern-parent prun :class 'pbind)
      (error "~s cannot be used outside of a pbind" prun))
    (make-instance 'prun-pstream
                   :pattern (as-pstream pattern)
                   :dur (pattern-as-pstream dur)
                   :current-dur 0)))

(defmethod next ((prun prun-pstream))
  (with-slots (pattern dur current-dur dur-history number) prun
    (let ((beats (beat (pattern-parent prun :class 'pbind))))
      (flet ((next-dur ()
               (let ((nxt (next dur)))
                 (unless (eop-p nxt)
                   (next pattern)
                   (incf current-dur nxt)))))
        (when (zerop number)
          (next-dur))
        (while (and (or (not (pstream-p dur))
                        (not (ended-p dur)))
                    (<= current-dur beats))
          (next-dur))))
    (last-output pattern)))

;;; psym

(defpattern psym (pattern)
  (pattern
   (current-pstream :state t :initform eop))
  :documentation "Use a pattern of symbols to embed `pdef's. PATTERN is the source pattern that yields symbols naming the pdef to embed.

Example:

;; (pdef :foo (pseq '(1 2 3) 1))
;;
;; (pdef :bar (pseq '(4 5 6) 1))
;;
;; (next-upto-n (psym (pseq '(:foo :bar) 1)))
;;
;; ;=> (1 2 3 4 5 6)

See also: `pdef', `ppar', `pmeta'")

(defmethod as-pstream ((psym psym))
  (with-slots (pattern) psym
    (make-instance 'psym-pstream
                   :pattern (as-pstream pattern))))

(defmethod next ((psym psym-pstream))
  (labels ((maybe-pdef (x)
             (if-let ((pdef (and (symbolp x)
                                 (pdef-pattern x))))
               pdef
               x)))
    (with-slots (pattern current-pstream) psym
      (let ((n (next current-pstream)))
        (if (eop-p n)
            (let ((next-pdef (next pattern)))
              (when (eop-p next-pdef)
                (return-from next eop))
              (setf current-pstream (as-pstream (if (listp next-pdef)
                                                    (ppar (mapcar #'maybe-pdef next-pdef))
                                                    (maybe-pdef next-pdef))))
              (next psym))
            n)))))

;;; pchain

(defpattern pchain (pattern)
  (patterns)
  :documentation "Combine multiple patterns into one event stream.

Example:

;; (next-n (pchain (pbind :foo (pseq '(1 2 3))) (pbind :bar (pseq '(7 8 9) 1))) 4)
;;
;; ;=> ((EVENT :FOO 1 :BAR 7) (EVENT :FOO 2 :BAR 8) (EVENT :FOO 3 :BAR 9) NIL)

See also: `pbind''s :embed key"
  :defun (defun pchain (&rest patterns)
           (make-instance 'pchain
                          :patterns patterns)))

(defmethod as-pstream ((pchain pchain))
  (with-slots (patterns) pchain
    (make-instance 'pchain-pstream
                   :patterns (mapcar #'pattern-as-pstream patterns))))

(defmethod next ((pchain pchain-pstream))
  (with-slots (patterns) pchain
    (let ((c-event (make-default-event)))
      (dolist (pattern patterns c-event)
        (setf c-event (combine-events c-event
                                      (let ((*event* c-event))
                                        (next pattern))))))))

;;; pdiff

(defpattern pdiff (pattern)
  (pattern)
  :documentation "Output the difference between successive outputs of PATTERN.

Example:

;; (next-n (pdiff (pseq '(3 1 4 3) 1)) 4)
;;
;; ;=> (-2 3 -1 NIL)

See also: `pdelta'")

(defmethod next ((pdiff pdiff-pstream))
  (with-slots (pattern) pdiff
    (when (zerop (slot-value pattern 'history-number))
      (next pattern))
    (let ((last (pstream-elt pattern -1))
          (next (next pattern)))
      (when (or (eop-p last)
                (eop-p next))
        (return-from next eop))
      (- next last))))

;;; pdelta

(defpattern pdelta (pattern)
  (pattern
   (cycle :default 4))
  :documentation "Output the difference between successive outputs of PATTERN, assuming PATTERN restarts every CYCLE outputs.

Unlike `pdiff', pdelta is written with its use as input for `pbind''s :delta key in mind. If PATTERN's successive values would result in a negative difference, pdelta instead wraps the delta around to the next multiple of CYCLE. This would allow you to, for example, supply the number of the beat that each event occurs on, rather than specifying the delta between each event. This is of course achievable using pbind's :beat key as well, however that method requires the pbind to peek at future values, which is not always desirable.

Based on the pattern originally from the ddwPatterns SuperCollider library.

Example:

;; (next-n (pdelta (pseq '(0 1 2 3)) 4) 8)
;;
;; ;=> (1 1 1 1 1 1 1 1)
;;
;; (next-n (pdelta (pseq '(0 1 2 5)) 4) 8)
;;
;; ;=> (1 1 3 3 1 1 3 3)

See also: `pdiff', `pbind''s :beat key")

(defmethod as-pstream ((pdelta pdelta))
  (with-slots (pattern cycle) pdelta
    (make-instance 'pdelta-pstream
                   :pattern (as-pstream pattern)
                   :cycle (next cycle))))

(defmethod next ((pdelta pdelta-pstream))
  (with-slots (pattern cycle history-number) pdelta
    (when (zerop history-number)
      (next pattern))
    (let ((lv (or (pstream-elt pattern -1) 0))
          (cv (next pattern)))
      (when (or (eop-p lv)
                (eop-p cv))
        (return-from next eop))
      (- cv
         (- lv (ceiling-by (- lv cv) cycle))))))

;;; pdrop

(defpattern pdrop (pattern)
  (pattern
   (n :default 0))
  :documentation "Drop the first N outputs from PATTERN and yield the rest. If N is negative, drop the last N outputs from PATTERN instead.

Example:

;; (next-n (pdrop (pseq '(1 2 3 4) 1) 2) 4)
;;
;; ;=> (3 4 NIL NIL)

See also: `protate'")

(defmethod as-pstream ((pdrop pdrop))
  ;; FIX: check that n is not bigger or smaller than history allows
  (with-slots (pattern n) pdrop
    (make-instance 'pdrop-pstream
                   :pattern (as-pstream pattern)
                   :n n)))

(defmethod next ((pdrop pdrop-pstream))
  (with-slots (pattern n number) pdrop
    (if (minusp n)
        (when (eop-p (pstream-elt-future pattern (- number n)))
          (return-from next eop))
        (when (zerop (slot-value pattern 'history-number))
          (dotimes (i n)
            (next pattern))))
    (next pattern)))

;;; ppar

(defpattern ppar (pattern)
  (patterns
   (pstreams :state t :initform nil))
  :documentation "Combine multiple event patterns into one pstream with all events in temporal order. PATTERNS is the list of patterns, or a pattern yielding lists of patterns. The ppar ends when all of the patterns in PATTERNS end.

Example:

;; (next-upto-n (ppar (list (pbind :dur (pn 1/2 4))
;;                          (pbind :dur (pn 2/3 4)))))
;;
;; ;=> ((EVENT :DUR 1/2 :DELTA 0) (EVENT :DUR 2/3 :DELTA 1/2)
;;      (EVENT :DUR 1/2 :DELTA 1/6) (EVENT :DUR 2/3 :DELTA 1/3)
;;      (EVENT :DUR 1/2 :DELTA 1/3) (EVENT :DUR 2/3 :DELTA 1/6)
;;      (EVENT :DUR 1/2 :DELTA 1/2))

See also: `psym'")

(defmethod next ((ppar ppar-pstream))
  (with-slots (patterns pstreams history-number) ppar
    (labels ((next-in-pstreams ()
               (if-let ((res (remove-if (lambda (pstream)
                                          (and (not (zerop (slot-value pstream 'history-number)))
                                               (ended-p pstream)))
                                        pstreams)))
                 (most #'< res :key #'beat)
                 eop))
             (maybe-reset-pstreams ()
               (unless (remove eop pstreams)
                 (let ((next-list (next patterns)))
                   (when (eop-p next-list)
                     (return-from maybe-reset-pstreams nil))
                   (setf pstreams (mapcar #'as-pstream next-list))))))
      (when (zerop history-number)
        (maybe-reset-pstreams))
      (let ((nxt (next (next-in-pstreams))))
        (if (eop-p nxt)
            (let ((nip (next-in-pstreams)))
              (if (eop-p nip)
                  eop
                  (let ((beppar (beat ppar))
                        (benip (beat nip)))
                    (if (< beppar benip)
                        (event :type :rest :delta (- benip beppar))
                        (next ppar)))))
            (combine-events nxt (event :delta (- (beat (next-in-pstreams)) (beat ppar)))))))))

;;; pmeta
;; FIX: `pk' doesn't work in pmeta.

(defpattern pmeta (pattern)
  (pattern
   (current-pstream :state t :initform nil))
  :documentation "Meta-control patterns using the events output by PATTERN. In other words, instead of triggering synths directly, the events output by PATTERN are used to embed patterns into the pmeta's pstream.

The following keys are supported:

- :pattern or :instrument - name of the source pattern for this \"step\".
- :dur - set the duration of the embedded pattern (defaults to :inf, which causes the pattern to play to its end).
- :findur - limit the duration of the embedded pattern.
- :sync - sync the duration of the embedded pattern to a multiple of the provided value, similar to `psync'
- :stretch - multiply the duration of each of the source pattern's events.
- :ts or :fit - timestretch a pattern so its total duration is the number specified, a la `pts'.
- :r or :repeat - repeat each event the number of times returned by the function when the event is applied to it, similar to `pr'.
- :inject - inject key/value pairs from the output of this value into the source pattern.
- :step-inject or :sinject - inject one output from this value per step.

The following keys are planned for future implementation:

- :start or :end - adjust the start or end points of the source pattern (i.e. to skip the first half, set :start to 0.5).
- :start-beat or :end-beat - adjust the start or end points of the source pattern in number of beats (i.e. to end the pattern 2 beats early, set :end-beat to -2).
- :start-nth or :end-nth - adjust the start or end points of the source pattern by skipping the first or last N events.
- :filter or :remove-if-not - skip all events from the source pattern that return nil when applied to the specified function or pattern.
- :mapcar or :nary - process each event from the source pattern with a function or another pattern.

See doc/special-keys.org for more information on these keys.

Example:

;; ;; define a few patterns...
;; (pdef :foo (pbind :x (pseq '(1 2 3) 1) :dur 1))
;; (pdef :bar (pbind :y (pseries) :dur (pwhite 0.1 1.0 3)))
;;
;; ;; use pmeta to play pattern foo and then pattern bar, ensuring each are 2 beats long in total by way of pmeta's sync key, which works similarly to `psync'
;; (next-upto-n (pmeta :pattern (pseq (list :foo :bar) 1) :sync 2))
;; ;=> ((EVENT :X 1 :DUR 1) (EVENT :X 2 :DUR 1) ;; from (pdef :foo)
;;      (EVENT :Y 0 :DUR 0.76958686) (EVENT :Y 1 :DUR 0.94850993) (EVENT :Y 2 :DUR 0.28190327)) ;; from (pdef :bar)

See also: `psym', `parp', `pdef', `pbind'"
  :defun (defun pmeta (&rest pairs)
           (make-instance 'pmeta :pattern (if (length= 1 pairs)
                                              (car pairs)
                                              pairs))))

(defmethod as-pstream ((pmeta pmeta))
  (with-slots (pattern) pmeta
    (make-instance 'pmeta-pstream
                   :pattern (if (listp pattern)
                                (loop :for (key value) :on pattern :by #'cddr
                                      :if (member key (list :inject))
                                        :append (list key value)
                                      :else
                                        :append (list key (pattern-as-pstream value)))
                                (as-pstream pattern)))))

(defmethod next ((pmeta pmeta-pstream))
  (with-slots (pattern current-pstream) pmeta
    (labels ((make-pstream (plist &optional res-pattern)
               (unless plist
                 (return-from make-pstream nil))
               (destructuring-bind (key value &rest rest) plist
                 (when (eop-p value)
                   (return-from make-pstream eop))
                 (case key
                   ((:pattern :instrument)
                    (setf res-pattern (etypecase value
                                        (symbol
                                         (pdef value))
                                        (pattern
                                         value)
                                        (list
                                         (ppar value)))))
                   ((:embed)
                    (let* ((nv (next value))
                           (nvp (typecase nv
                                  (list nv)
                                  (event (event-plist nv)))))
                      (dolist (i (reverse nvp))
                        (push i rest))))
                   ((:dur)
                    (unless (eql value :inf)
                      (setf res-pattern (psync res-pattern value value))))
                   ((:findur)
                    (unless (eql value :inf)
                      (setf res-pattern (pfindur res-pattern value))))
                   ((:sync)
                    (unless (eql value :inf)
                      (setf res-pattern (apply #'psync res-pattern (ensure-list value)))))
                   ((:stretch)
                    (setf res-pattern (pchain res-pattern (pbind :dur (p* value (pk :dur))))))
                   ((:fit :ts)
                    (setf res-pattern (pts res-pattern value)))
                   ((:r :repeat)
                    (setf res-pattern (pr res-pattern value)))
                   ((:inject)
                    (setf res-pattern (if res-pattern
                                          (pchain res-pattern value)
                                          value)))
                   ((:step-inject :sinject)
                    (setf res-pattern (if res-pattern
                                          (pchain res-pattern (pn (next value)))
                                          (pn (next value))))))
                 (if rest
                     (make-pstream rest res-pattern)
                     (let ((pstream (as-pstream res-pattern)))
                       (setf (slot-value pstream 'parent) pmeta)
                       pstream)))))
      (unless current-pstream
        (setf current-pstream (make-pstream (if (listp pattern)
                                                (loop :for (key value) :on pattern :by #'cddr
                                                      :append (list key (next value)))
                                                (let ((nxt (next pattern)))
                                                  (typecase nxt
                                                    (event (event-plist nxt))
                                                    (symbol (if (eop-p nxt)
                                                                (list :none eop)
                                                                (list :pattern nxt)))))))))
      (when (eop-p current-pstream)
        (return-from next eop))
      (let ((nxt (next current-pstream)))
        (if (eop-p nxt)
            (progn
              (setf current-pstream nil)
              (next pmeta))
            nxt)))))

;;; pts

(defpattern pts (pattern)
  (pattern
   (dur :default 4)
   (pattern-outputs :state t))
  :documentation "Timestretch PATTERN so its total duration is DUR. Note that only the first `*max-pattern-yield-length*' events from PATTERN are considered, and that they are calculated immediately at pstream creation time rather than lazily as the pstream yields.

Example:

;; (next-upto-n (pts (pbind :dur (pn 1 4)) 5))
;;
;; ;=> ((EVENT :DUR 5/4) (EVENT :DUR 5/4) (EVENT :DUR 5/4) (EVENT :DUR 5/4))

See also: `pfindur', `psync'")

(defmethod as-pstream ((pts pts))
  (with-slots (pattern dur) pts
    (let* ((pstr (as-pstream pattern))
           (res (next-upto-n pstr)))
      (make-instance 'pts-pstream
                     :pattern pstr
                     :dur (next dur)
                     :pattern-outputs (coerce res 'vector)))))

(defmethod next ((pts pts-pstream))
  (with-slots (pattern dur pattern-outputs number) pts
    (when (>= number (length pattern-outputs))
      (return-from next eop))
    (let ((mul (/ dur (beat pattern)))
          (ev (elt pattern-outputs number)))
      (combine-events ev (event :dur (* mul (event-value ev :dur)))))))

;;; pwalk

(defpattern pwalk (pattern)
  (list
   step-pattern
   (direction-pattern :default 1)
   (start-pos :default 0)
   (current-index :state t)
   (current-direction :default 1 :state t))
  :documentation "\"Walk\" over the values in LIST by using the accumulated value of the outputs of STEP-PATTERN as the index. At the beginning of the pwalk and each time the start or end of the list is passed, the output of DIRECTION-PATTERN is taken and used as the multiplier for values from STEP-PATTERN. START-POS is the index in LIST for pwalk to take its first output from.

Example:

;; ;; using (pseq (list 1 -1)) as the DIRECTION-PATTERN causes the pwalk's output to \"ping-pong\":
;; (next-n (pwalk (list 0 1 2 3) (pseq (list 1)) (pseq (list 1 -1))) 10)
;;
;; ;=> (0 1 2 3 2 1 0 1 2 3)

See also: `pindex', `pbrown', `paccum'")

(defmethod as-pstream ((pwalk pwalk))
  (with-slots (list step-pattern direction-pattern start-pos) pwalk
    (make-instance 'pwalk-pstream
                   :list (next list)
                   :step-pattern (pattern-as-pstream step-pattern)
                   :direction-pattern (pattern-as-pstream direction-pattern)
                   :start-pos (pattern-as-pstream start-pos))))

(defmethod next ((pwalk pwalk-pstream))
  (with-slots (list step-pattern direction-pattern start-pos current-index current-direction number) pwalk
    (when (zerop number)
      (setf current-index (next start-pos))
      (setf current-direction (next direction-pattern))
      (return-from next (nth current-index list)))
    (let ((nsp (next step-pattern)))
      (when (or (eop-p nsp) (eop-p current-index) (eop-p current-direction))
        (return-from next eop))
      (labels ((next-index ()
                 (+ current-index (* nsp current-direction))))
        (let ((next-index (next-index)))
          (when (or (minusp next-index)
                    (>= next-index (length list)))
            (setf current-direction (next direction-pattern)))
          (when (eop-p current-direction)
            (return-from next eop))
          (setf current-index (mod (next-index) (length list))))))
    (elt-wrap list current-index)))

;;; pparchain

(defpattern pparchain (pchain)
  (patterns)
  :documentation "Combine multiple patterns into several event streams. The event yielded by the first pattern will be used as the input event to the second pattern, and so on. The events yielded by each pattern will be collected into a list and yielded by the pparchain. This pattern is effectively `ppar' and `pchain' combined.

Example:

;; (next-upto-n (pparchain (pbind :foo (pseries 0 1 3)) (pbind :baz (p+ (pk :foo) 1) :foo (p+ (pk :foo) 3))))
;; ;=> (((EVENT :FOO 0) (EVENT :FOO 3 :BAZ 1))
;;      ((EVENT :FOO 1) (EVENT :FOO 4 :BAZ 2))
;;      ((EVENT :FOO 2) (EVENT :FOO 5 :BAZ 3)))

See also: `ppc', `ppar', `pchain', `pbind''s :embed key"
  :defun (defun pparchain (&rest patterns)
           (make-instance 'pparchain
                          :patterns patterns)))

(defmethod as-pstream ((pparchain pparchain))
  (with-slots (patterns) pparchain
    (make-instance 'pparchain-pstream
                   :patterns (mapcar #'as-pstream patterns))))

(defmethod next ((pparchain pparchain-pstream))
  (with-slots (patterns) pparchain
    (let ((c-event (make-default-event)))
      (loop :for pattern :in patterns
            :do (setf c-event (combine-events c-event (let ((*event* (copy-event c-event)))
                                                        (next pattern))))
            :if (eop-p c-event)
              :return eop
            :else
              :collect c-event))))

;;; ppc

(defmacro ppc (&body pairs)
  "Syntax sugar for `pparchain' that automatically splits PAIRS by :- symbols.

Example:

;; (ppc :foo (pseq (list 1 2 3) 1)
;;      :-
;;      :bar (p+ (pk :foo) 2))
;; ;=> (((EVENT :FOO 1) (EVENT :FOO 1 :BAR 3))
;;      ((EVENT :FOO 2) (EVENT :FOO 2 :BAR 4))
;;      ((EVENT :FOO 3) (EVENT :FOO 3 :BAR 5)))

See also: `pparchain'"
  (labels ((ppc-split (pairs)
             (let ((pos (position :- pairs)))
               (if pos
                   (list (subseq pairs 0 pos)
                         (ppc-split (subseq pairs (1+ pos))))
                   pairs))))
    `(pparchain ,@(loop :for i :in (ppc-split pairs)
                        :collect (cons 'pbind i)))))

(pushnew 'ppc *patterns*)

;;; pclump

(defpattern pclump (pattern)
  (pattern
   (n :default 1))
  :documentation "Group outputs of the source pattern into lists of up to N items each.

Example:

;; (next-upto-n (pclump (pseries 0 1 5) 2))
;; ;=> ((0 1) (2 3) (4))

See also: `paclump'")

(defmethod next ((pclump pclump-pstream))
  (with-slots (pattern n) pclump
    (let ((next (next n)))
      (when (eop-p next)
        (return-from next eop))
      (or (next-upto-n pattern next)
          eop))))

;;; paclump

(defpattern paclump (pattern)
  (pattern)
  :documentation "Automatically group outputs of the source pattern into lists of up to N items each. Unlike `pclump', clump size is automatically set to the length of the longest list in the values of `*event*', or 1 if there are no lists.

Example:

;; (next-upto-n (pbind :foo (pseq '((1) (1 2) (1 2 3)) 1) :bar (paclump (pseries))))
;; ;=> ((EVENT :FOO (1) :BAR (0)) (EVENT :FOO (1 2) :BAR (1 2)) (EVENT :FOO (1 2 3) :BAR (3 4 5)))

See also: `pclump'")

(defmethod next ((paclump paclump-pstream))
  (with-slots (pattern) paclump
    (unless *event*
      (return-from next eop))
    (next-upto-n pattern (reduce #'max (mapcar (fn (length (ensure-list (e _))))
                                               (keys *event*))))))

;;; paccum
;; https://pcm.peabody.jhu.edu/~gwright3/stdmp2/docs/SuperCollider_Book/code/Ch%2020%20dewdrop%20and%20chucklib/dewdrop_lib/ddwPatterns/Help/Paccum.html

(defpattern paccum (pattern)
  ((operator :default #'+)
   (start :default 0)
   (step :default 1)
   (length :default :inf)
   (lo :default nil)
   (hi :default nil)
   (bound-by :default nil)
   (current-value :state t)
   (current-repeats-remaining :state t))
  :documentation "Numeric accumulator. Each output and STEP is used as the input for OPERATOR to generate the next output. When LO, HI, and BOUND-BY are provided, outputs that fall outside the range LO..HI are wrapped back inside with the BOUND-BY function; the value is provided as its first argument, and LO and HI are provided as its second and third.

Based on the pattern originally from the ddwPatterns SuperCollider library.

Example:

;; (next-upto-n (paccum #'+ 0 1) 5) ;; same as (pseries 0 1)
;; ;=> (0 1 2 3 4)

;; (next-upto-n (paccum #'+ 0 1 :inf :lo 0 :hi 3 :bound-by #'wrap) 9) ;; same as above, wrapping between 0 and 3.
;; ;=> (0 1 2 0 1 2 0 1 2)

See also: `pseries', `pgeom', `pwalk'"
  :defun
  (defun paccum (&optional (operator #'+) (start 0) (step 1) (length :inf) &key lo hi bound-by)
    (make-instance 'paccum
                   :operator operator
                   :start start
                   :step step
                   :length length
                   :lo lo
                   :hi hi
                   :bound-by bound-by)))

(defmethod as-pstream ((paccum paccum))
  (with-slots (operator start step length lo hi bound-by) paccum
    (make-instance 'paccum-pstream
                   :operator (pattern-as-pstream operator)
                   :start (pattern-as-pstream start)
                   :step (pattern-as-pstream step)
                   :length (as-pstream length)
                   :lo (pattern-as-pstream lo)
                   :hi (pattern-as-pstream hi)
                   :bound-by (pattern-as-pstream bound-by))))

(defmethod next ((paccum paccum-pstream))
  (with-slots (operator start step length lo hi bound-by current-value) paccum
    (unless (remaining-p paccum 'length)
      (return-from next eop))
    (decf-remaining paccum)
    (setf current-value (if (slot-boundp paccum 'current-value)
                            (when-let ((res (funcall (if (pstream-p operator)
                                                         (next operator)
                                                         operator)
                                                     current-value
                                                     (next step))))
                              (if bound-by
                                  (when-let ((func (if (pstream-p bound-by)
                                                       (next bound-by)
                                                       bound-by))
                                             (lo (next lo))
                                             (hi (next hi)))
                                    (funcall func res lo hi))
                                  res))
                            (next start)))))

;;; ps
;; https://pustota.basislager.org/_/sc-help/Help/Classes/PS.html
;; https://pustota.basislager.org/_/sc-help/Help/Tutorials/PSx_stream_patterns.html

(defpattern ps (pattern)
  (pattern
   pstream)
  :documentation "Preserve pstream state across subsequent calls to `as-pstream'. To reset the pstream, simply re-evaluate the ps definition.

Based on the pattern originally from the miSCellaneous SuperCollider library.

Example:

;; (defparameter *pst* (ps (pseries)))
;;
;; (next-upto-n *pst* 4)
;; ;=> (0 1 2 3)
;;
;; (next-upto-n *pst* 4)
;; ;=> (4 5 6 7)
;;
;; (defparameter *pst* (ps (pseries)))
;;
;; (next-upto-n *pst* 4)
;; ;=> (0 1 2 3)

See also: `prs', `pdef'"
  :defun (defun ps (pattern)
           (make-instance 'ps
                          :pattern pattern)))

(defmethod as-pstream ((ps ps))
  (with-slots (pattern pstream) ps
    (make-instance 'ps-pstream
                   :pattern pattern
                   :pstream (if (slot-boundp ps 'pstream)
                                pstream
                                (let ((pstr (as-pstream pattern)))
                                  (setf pstream pstr)
                                  pstr)))))

(defmethod next ((ps-pstream ps-pstream))
  (with-slots (pstream) ps-pstream
    (next pstream)))

;;; prs

(defun prs (pattern &optional (repeats :inf))
  "Syntax sugar for (pr (ps PATTERN) REPEATS). Useful, for example, to ensure that each cycle of a pattern only gets one value from the `ps'.

See also: `pr', `ps'"
  (pr (ps pattern) repeats))

(pushnew 'prs *patterns*)

;;; ipstream

(defpattern ipstream (pattern)
  ((patterns :default (list))
   (end-when-empty :default nil)
   (granularity :default 1/5)
   (lock :state t :initform (bt:make-recursive-lock "ipstream patterns slot lock")))
  :documentation "Insertable pstream; a pstream that can be changed while it's running by inserting new patterns at a specified beat.")

(defmethod as-pstream ((ipstream ipstream))
  (with-slots (patterns end-when-empty granularity) ipstream
    (let ((pstr (make-instance 'ipstream-pstream
                               :patterns (list)
                               :end-when-empty end-when-empty
                               :granularity granularity
                               :lock (bt:make-recursive-lock "ipstream patterns slot lock"))))
      (dolist (pat patterns pstr)
        (etypecase pat
          (pattern
           (ipstream-insert pstr pat 0))
          (list
           (destructuring-bind (beat &rest patterns) pat
             (dolist (pat patterns)
               (ipstream-insert pstr pat beat)))))))))

(defmethod next ((ipstream ipstream-pstream))
  (with-slots (patterns end-when-empty granularity lock) ipstream
    (labels ((actual-beat (pstream)
               (+ (slot-value pstream 'start-beat) (beat pstream)))
             (sorted-pstrs ()
               (sort patterns #'< :key #'actual-beat)))
      (bt:with-recursive-lock-held (lock)
        (if patterns
            (let* ((next-pstr (car (sorted-pstrs)))
                   (ev (if (<= (actual-beat next-pstr)
                               (beat ipstream))
                           (let ((nxt (next next-pstr)))
                             (if (eop-p nxt)
                                 (progn
                                   (deletef patterns next-pstr)
                                   (next ipstream))
                                 nxt))
                           (event :type :rest :delta granularity)))
                   (next-pstr (car (sorted-pstrs))))
              (combine-events ev (event :delta (if next-pstr
                                                   (min granularity
                                                        (- (actual-beat next-pstr)
                                                           (beat ipstream)))
                                                   granularity))))
            (if end-when-empty
                eop
                (event :type :rest :delta granularity)))))))

(defgeneric ipstream-insert (ipstream pattern &optional start-beat)
  (:documentation "Insert PATTERN into IPSTREAM at START-BEAT. START-BEAT defaults to the ipstream's current beat."))

(defmethod ipstream-insert ((ipstream ipstream-pstream) pattern &optional start-beat)
  (with-slots (patterns lock) ipstream
    (let ((pstr (as-pstream pattern)))
      (setf (slot-value pstr 'start-beat) (or start-beat (beat ipstream)))
      (bt:with-lock-held (lock)
        (push pstr patterns)))))

(export 'ipstream-insert)

;;; pfilter

(defpattern pfilter (pattern)
  (pattern
   (predicate :initform 'identity))
  :documentation "Skip elements of a source pattern that PREDICATE returns false for. If PREDICATE is not a function, skip items that are `eql' to it.

Example:

;; (next-n (pfilter (pseq (list 1 2 3))
;;                  2)
;;         6)
;; ;=> (1 3 1 3 1 3)

;; (next-n (pfilter (pseries 0 1) 'evenp)
;;         6)
;; ;=> (1 3 5 7 9 11)

See also: `pfilter-out', `pr', `pdurstutter'")

(defmethod next ((pfilter pfilter-pstream))
  (with-slots (pattern predicate) pfilter
    (let ((func (if (function-designator-p predicate)
                    predicate
                    (lambda (input) (eql input predicate)))))
      (loop :for res := (next pattern)
            :if (or (eop-p res)
                    (funcall func res))
              :return res))))

;;; pfilter-out

(defun pfilter-out (pattern predicate)
  "Skip elements of a source pattern that PREDICATE returns true for. If PREDICATE is not a function, skip items that are `eql' to it.

See also: `pfilter'"
  (pfilter pattern (if (function-designator-p predicate)
                       (lambda (x) (not (funcall predicate x)))
                       (lambda (x) (not (eql predicate x))))))

(pushnew 'pfilter-out *patterns*)
