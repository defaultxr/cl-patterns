(in-package #:cl-patterns)

;; NOTES:
;; FIX: take each pattern's name out of its docstring, and make sure the first sentence in each docstring is a good concise description of the functionality.
;; FIX: don't append stuff to the end of lists; https://stackoverflow.com/questions/6439972/what-is-the-cons-to-add-an-item-to-the-end-of-the-list

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

(defparameter *patterns* (list)
  "List of the names of all defined pattern types.")

(defmacro defpattern (name superclasses slots &key documentation defun) ;; FIX: should warn if `set-parents' is not called in the creation-function.
  "Define a pattern. This macro automatically generates the pattern's class, its pstream class, and the function to create an instance of the pattern, and makes them external in the cl-patterns package.

NAME is the name of the pattern. Typically a word or two that describes its function, prefixed with p.

SUPERCLASSES is a list of superclasses of the pattern. Most patterns just subclass the 'pattern' class.

SLOTS is a list of slots that the pattern and pstreams derived from it have. Each slot can either be just a symbol, or a slot definition a la `defclass'. You can provide a default for the slot with the :default key, and you can set a slot as a state slot (which only appears in the pattern's pstream class) by setting the :state key to t.

DOCUMENTATION is a docstring describing the pattern. We recommend providing at least one example, and a \"See also\" section to refer to similar pattern classes.

DEFUN can either be a full defun form for the pattern, or an expression which will be inserted into the pattern creation function prior to initialization of the instance. Typically you'd use this for inserting `assert' statements, for example."
  (let* ((superclasses (or superclasses (list 'pattern)))
         (slots (mapcar #'ensure-list slots))
         (name-pstream (intern (concatenate 'string (symbol-name name) "-PSTREAM") 'cl-patterns))
         (super-pstream (if (eql 'pattern (car superclasses))
                            'pstream
                            (intern (concatenate 'string (symbol-name (car superclasses)) "-PSTREAM") 'cl-patterns))))
    (labels ((desugar-slot (slot)
               "Convert a slot into something appropriate for defclass to handle."
               (let ((name (car slot))
                     (rest (cdr slot)))
                 (setf rest (remove-from-plist rest :default :state))
                 (unless (position :initarg (keys rest))
                   (appendf rest (list :initarg (make-keyword name))))
                 (append (list name) rest)))
             (optional-slot-p (slot)
               "Whether the slot is optional or not. A slot is optional if a default is provided."
               (position :default (keys (cdr slot))))
             (state-slot-p (slot)
               "Whether the slot is a pstream state slot or not. Pstream state slots only appear as slots for the pattern's pstream class and not for the pattern itself."
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
                                  ,@(mapcan (lambda (i) (list (make-keyword (car i)) (car i)))
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
         ,(unless defun ;; FIX: does this work properly for all patterns?
            `(defmethod print-object ((,name ,name) stream)
               (format stream "#<~s~{ ~s~}>" ',name
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
                                      (position (car defun) (list 'assert)))))
                 (pre-init (when gen-func-p
                             defun)))
            (if gen-func-p
                (make-defun pre-init)
                (add-doc-to-defun defun)))
         (pushnew ',name *patterns*)
         (export '(,name ,name-pstream))))))

(defparameter *max-pattern-yield-length* 256
  "The default maximum number of events or values that will be used by functions like `next-n' or patterns like `pshift', in order to prevent hangs caused by infinite-length patterns.")

;;; pattern

(defgeneric pstream-count (pattern)
  (:documentation "The number of pstreams that have been made of this pattern."))

(defclass pattern ()
  ((quant :initarg :quant :documentation "A list of numbers representing when the pattern's pstream can start playing. The list takes the form (QUANT &OPTIONAL PHASE TIMING-OFFSET). For example, a quant of (4) means it can start on any beat on the clock that is divisible by 4. A quant of (4 2) means the pstream can start 2 beats after any beat divisible by 4. And a quant of (4 0 1) means that the pstream can start 1 second after any beat that is divisible by 4.")
   (parent :initarg :parent :initform nil :documentation "When a pattern is embedded in another pattern, the embedded pattern's parent slot points to the pattern it is embedded in.")
   (loop-p :initarg :loop-p :documentation "Whether or not the pattern should loop when played.")
   (cleanup-functions :initarg :cleanup-functions :initform (list) :documentation "A list of functions that are run when the pattern ends or is stopped.")
   (pstream-count :initform 0 :reader pstream-count :documentation "The number of pstreams that have been made of this pattern.")
   (metadata :initarg :metadata :initform (make-hash-table) :type hash-table :documentation "Hash table of additional data associated with the pattern, accessible with the `pattern-metadata' function."))
  (:documentation "Abstract pattern superclass."))

(defun pattern-p (object)
  "Return true if OBJECT is a pattern, and NIL otherwise."
  (typep object 'pattern))

(defun all-patterns ()
  "Get a list of all defined patterns.

See also: `all-pdefs'"
  *patterns*)

(defmethod quant ((pattern pattern))
  (if (slot-boundp pattern 'quant)
      (slot-value pattern 'quant)
      (list 1)))

(defmethod (setf quant) (value (pattern pattern))
  (setf (slot-value pattern 'quant) (ensure-list value)))

(defmethod loop-p ((pattern pattern))
  (if (slot-boundp pattern 'loop-p)
      (slot-value pattern 'loop-p)
      nil))

(defmethod (setf loop-p) (value (pattern pattern))
  (setf (slot-value pattern 'loop-p) value))

(defgeneric peek (pattern)
  (:documentation "\"Peek\" at the next value of a pstream, without advancing its current position.

See also: `next', `peek-n', `peek-upto-n'"))

(defun peek-n (pstream n)
  "Peek at the next N results of a pstream, without advancing it forward in the process.

See also: `peek', `peek-upto-n', `next', `next-n'"
  (assert (integerp n) (n) "peek-n's N argument must be an integer.")
  (unless (pstream-p pstream)
    (return-from peek-n (peek-n (as-pstream pstream) n)))
  (with-slots (number future-number) pstream
    (loop :for i :from 0 :below n
          :collect (pstream-elt-future pstream (+ number (- future-number) i)))))

(defun peek-upto-n (pstream &optional (n *max-pattern-yield-length*))
  "Peek at up to the next N results of a pstream, without advancing it forward in the process.

See also: `peek', `peek-n', `next', `next-upto-n'"
  (assert (integerp n) (n) "peek-upto-n's N argument must be an integer.")
  (unless (pstream-p pstream)
    (return-from peek-upto-n (peek-upto-n (as-pstream pstream) n)))
  (with-slots (number future-number) pstream
    (loop :for i :from 0 :below n
          :for res := (pstream-elt-future pstream (+ number (- future-number) i))
          :until (null res)
          :collect res)))

(defgeneric next (pattern)
  (:documentation "Get the next value of a pstream, function, or other object, advancing the pstream forward in the process.

See also: `next-n', `next-upto-n', `peek'")
  (:method-combination pattern))

(defmethod next ((pattern t))
  pattern)

(defmethod next ((pattern pattern))
  (next (as-pstream pattern)))

(defmethod next ((pattern function))
  (funcall pattern))

(defun next-n (pattern n)
  "Get the next N results of a pattern stream, function, or other object, advancing the pattern stream forward N times in the process.

See also: `next', `next-upto-n', `peek', `peek-n'"
  (assert (integerp n) (n) "next-n's N argument must be an integer.")
  (let ((pstream (as-pstream pattern)))
    (loop :repeat n
          :collect (next pstream))))

(defun next-upto-n (pattern &optional (n *max-pattern-yield-length*))
  "Get a list of up to N results from PATTERN. If PATTERN ends after less than N values, then all of its results will be returned.

See also: `next', `next-n', `peek', `peek-upto-n'"
  (assert (integerp n) (n) "next-upto-n's N argument must be an integer.")
  (let ((pstream (as-pstream pattern)))
    (loop
      :for number :from 0 :upto n
      :while (< number n)
      :for val := (next pstream)
      :if (null val)
        :do (loop-finish)
      :else
        :collect val)))


(defgeneric events-in-range (pstream min max)
  (:documentation "Get all the events from PSTREAM whose start beat are MIN or greater, and less than MAX."))

(defmethod events-in-range ((pattern pattern) min max)
  (events-in-range (as-pstream pattern) min max))

(defgeneric pattern-metadata (pattern &optional key)
  (:documentation "Get the value of PATTERN's metadata for KEY. Returns true as a second value if the metadata had an entry for KEY, or nil if it did not."))

(defmethod pattern-metadata ((pattern pattern) &optional key)
  (if key
      (gethash key (slot-value pattern 'metadata))
      (slot-value pattern 'metadata)))

(defun (setf pattern-metadata) (value pattern key)
  (setf (gethash key (slot-value pattern 'metadata)) value))

;;; pstream

(defclass pstream (pattern #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((number :initform 0 :documentation "The number of outputs yielded from this pstream and any sub-pstreams that have ended.") ;; FIX: rename to this-index ?
   (pattern-stack :initform (list) :documentation "The stack of pattern pstreams embedded in this pstream.")
   (source :initarg :source :documentation "The source object (i.e. pattern) that this pstream was created from.")
   (pstream-count :initarg :pstream-count :reader pstream-count :type integer :documentation "How many times a pstream was made of this pstream's source prior to this pstream. For example, if it was the first time `as-pstream' was called on the pattern, this will be 0.")
   (beat :initform 0 :reader beat :type number :documentation "The number of beats that have elapsed since the start of the pstream.")
   (history :type vector :documentation "The history of outputs yielded by the pstream.")
   (history-number :initform 0 :documentation "The number of items in this pstream's history. Differs from the number slot in that all outputs are immediately included in its count.")
   (start-beat :initarg :start-beat :initform nil :documentation "The beat number of the parent pstream when this pstream started.")
   (future-number :initform 0 :documentation "The number of peeks into the future that have been made in the pstream. For example, if `peek' is used once, this would be 1. If `next' is called after that, future-number decreases back to 0."))
  (:documentation "\"Pattern stream\". Keeps track of the current state of a pattern in process of yielding its outputs."))

(defmethod print-object ((pstream pstream) stream)
  (with-slots (number) pstream
    (print-unreadable-object (pstream stream :type t)
      (format stream "~s ~s" :number number))))

(defun pstream-p (object)
  "Return true if OBJECT is a pstream, and NIL otherwise."
  (typep object 'pstream))

(defmethod events-in-range ((pstream pstream) min max)
  (loop :while (and (<= (beat pstream) max)
                    (not (ended-p pstream)))
        :do (let ((next (next pstream)))
              (unless (typep next '(or null event))
                (error "events-in-range can only be used on event streams."))))
  (loop :for i :across (slot-value pstream 'history)
        :if (and i
                 (>= (beat i) min)
                 (< (beat i) max))
          :collect i
        :if (or (null i)
                (>= (beat i) max))
          :do (loop-finish)))

(defgeneric last-output (pstream)
  (:documentation "Returns the last output yielded by PSTREAM.

Example:

;; (defparameter *pstr* (as-pstream (pseq '(1 2 3) 1)))
;; (next *pstr*) ;; => 1
;; (last-output *pstr*) ;; => 1

See also: `ended-p'"))

(defmethod last-output ((pstream pstream))
  (with-slots (number future-number) pstream
    (let ((idx (- number future-number)))
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
  (with-slots (number future-number) pstream
    (and (not (= 0 (- number future-number)))
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
  (with-slots (number future-number) pstream
    (pstream-elt-future pstream (- number future-number))))

(defmethod peek ((pattern pattern))
  (next (as-pstream pattern)))

(defmethod next :around ((pstream pstream))
  (labels ((get-value-from-stack (pattern)
             (if (null (slot-value pattern 'pattern-stack))
                 (prog1
                     (let ((res (call-next-method)))
                       (typecase res
                         (pattern
                          ;; if `next' returns a pattern, we push it to the pattern stack as a pstream
                          (let ((pstr (as-pstream res)))
                            (setf (slot-value pstr 'start-beat) (beat pattern))
                            (push pstr (slot-value pattern 'pattern-stack)))
                          (get-value-from-stack pattern))
                         (t res)))
                   (incf (slot-value pattern 'number)))
                 (let* ((popped (pop (slot-value pattern 'pattern-stack)))
                        (nv (next popped)))
                   (if (null nv)
                       (get-value-from-stack pattern)
                       (progn
                         (push popped (slot-value pattern 'pattern-stack))
                         nv))))))
    (with-slots (number history history-number future-number) pstream
      (if (plusp future-number)
          (prog1
              (elt history (- number future-number))
            (decf future-number))
          (let ((result (get-value-from-stack pstream)))
            (when (event-p result)
              (setf result (copy-event result))
              (when (and (null (raw-event-value result :beat))
                         (null (slot-value pstream 'parent)))
                (setf (beat result) (beat pstream)))
              (incf (slot-value pstream 'beat) (event-value result :delta)))
            (setf (elt history (mod history-number (length (slot-value pstream 'history)))) result)
            (incf history-number)
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

(defun t-pstream (value)
  "Make a t-pstream object with the value VALUE."
  (make-instance 't-pstream :value value))

(defmethod print-object ((pstream t-pstream) stream)
  (print-unreadable-object (pstream stream :type t) (prin1 (slot-value pstream 'value) stream)))

(defmethod as-pstream ((value t))
  (t-pstream value))

(defmethod next ((pattern t-pstream))
  (when (= 0 (slot-value pattern 'number))
    (let ((value (slot-value pattern 'value)))
      (if (functionp value)
          (funcall value)
          value))))

(defmethod as-pstream ((pattern pattern))
  (let ((slots (remove 'parent (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pattern))))))
    (apply #'make-instance
           (intern (concatenate 'string (symbol-name (class-name (class-of pattern))) "-PSTREAM") 'cl-patterns)
           (loop :for slot :in slots
                 :if (slot-boundp pattern slot)
                   :append (list (make-keyword slot)
                                 (pattern-as-pstream (slot-value pattern slot)))))))

(defmethod as-pstream :around ((pattern pattern))
  (let ((pstream (call-next-method)))
    (incf (slot-value pattern 'pstream-count))
    pstream))

(defmethod as-pstream :around ((object t))
  (let ((pstream (call-next-method)))
    (with-slots (pstream-count source history) pstream
      (setf pstream-count (if (slot-exists-p object 'pstream-count)
                              (slot-value object 'pstream-count)
                              0)
            source object
            history (make-array *max-pattern-yield-length* :initial-element nil)))
    (set-parents pstream)
    pstream))

(defmethod as-pstream :around ((pstream pstream)) ;; prevent pstreams from being "re-converted" to pstreams
  pstream)

(define-condition pstream-out-of-range () ((index :initarg :index :reader pstream-elt-index))
  (:report (lambda (condition stream)
             (format stream "The index ~d falls outside the scope of the pstream's history." (pstream-elt-index condition)))))

(defun pstream-elt-index-to-history-index (pstream index)
  "Given INDEX, an absolute index into PSTREAM's history, return the actual index into the current recorded history of the pstream.

See also: `pstream-history-advance-by'"
  (assert (>= index 0) (index))
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
  (assert (integerp n) (n))
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
  (assert (>= index 0) (index))
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
  (assert (integerp n) (n))
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
        ;; (error "Peeking further would render the future inaccessible to the present")
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
  (assert (evenp (length pairs)) (pairs) "pbind's input must be provided as a list of key/value pairs.")
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
    ;; process :quant key.
    (when-let ((quant (getf pairs :quant)))
      (setf (quant pattern)
            (if (functionp quant)
                (funcall quant)
                quant)))
    ;; process :pdef key.
    (when-let ((pdef-name (getf pairs :pdef)))
      (pdef pdef-name pattern))
    pattern))

(setf (documentation 'pbind 'type) (documentation 'pbind 'function))

(defmethod print-object ((pbind pbind) stream)
  (format stream "(~s~{ ~s ~s~})" 'pbind (slot-value pbind 'pairs)))

(defmacro pb (key &body pairs) ;; FIX: should automatically convert +, *, -, /, etc to their equivalent patterns.
  "pb is a convenience macro, wrapping the functionality of `pbind' and `pdef'. KEY is the name of the pattern (same as pbind's :pdef key or `pdef' itself), and PAIRS is the same as in regular pbind. If PAIRS is only one element, pb operates like `pdef', otherwise it operates like `pbind'.

See also: `pbind', `pdef'"
  (if (length= 1 pairs)
      `(pdef ,key ,@pairs)
      `(pbind :pdef ,key ,@pairs)))

(defclass pbind-pstream (pbind pstream)
  ())

(defmethod print-object ((pbind pbind-pstream) stream)
  (print-unreadable-object (pbind stream :type t)
    (format stream "~{~s ~s~^ ~}" (slot-value pbind 'pairs))))

(defmethod as-pstream ((pbind pbind))
  (let ((slots (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of pbind)))))
    (apply #'make-instance
           (intern (concatenate 'string (symbol-name (class-name (class-of pbind))) "-PSTREAM") 'cl-patterns)
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
  (let ((keyname (make-keyword key)))
    `(setf (getf *pbind-special-init-keys* ,keyname)
           (lambda (value pattern)
             (declare (ignorable value pattern))
             ,@body))))

;; (define-pbind-special-init-key inst ;; FIX: this should be part of event so it will affect the event as well. maybe just rename to 'synth'?
;;   (list :instrument value))

(define-pbind-special-init-key loop-p
  (setf (loop-p pattern) value))

(defmacro define-pbind-special-wrap-key (key &body body)
  "Define a special key for pbind that replaces the pbind with another pattern during the pbind's initialization. Each encapsulation key is run once on the pbind after it has been initialized, altering the type of pattern returned if the return value of the function is non-NIL."
  (let ((keyname (make-keyword key)))
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
  (destructuring-bind (quant &optional maxdur) (ensure-list value)
    (psync pattern quant (or maxdur quant))))

(define-pbind-special-wrap-key pdurstutter
  (pdurstutter pattern value))

(define-pbind-special-wrap-key pr
  (pr pattern value))

(define-pbind-special-wrap-key ptrace
  (if value
      (if (eql t value)
          (ptrace pattern)
          (etypecase value
            (pattern (ptrace value))
            (symbol (ptrace pattern value))))
      pattern))

(define-pbind-special-wrap-key pmeta
  (if (eql t value)
      (pmeta pattern)
      pattern))

(defmacro define-pbind-special-process-key (key &body body)
  "Define a special key for pbind that alters the pattern in a nonstandard way. These functions are called for each event created by the pbind and must return an event if the key should embed values into the event stream, or NIL if the pstream should end."
  (let ((keyname (make-keyword key)))
    `(setf (getf *pbind-special-process-keys* ,keyname)
           (lambda (value)
             ,@body))))

(define-pbind-special-process-key embed
  value)

(defmethod next ((pbind pbind-pstream))
  (labels ((accumulator (pairs)
             (let ((key (car pairs))
                   (val (cadr pairs)))
               (when (and (pstream-p val)
                          (null (slot-value val 'start-beat)))
                 (setf (slot-value val 'start-beat) (beat pbind)))
               (when-let ((next-val (next val)))
                 (if (position key (keys *pbind-special-process-keys*))
                     (setf *event* (combine-events *event*
                                                   (funcall (getf *pbind-special-process-keys* key) next-val)))
                     (setf (event-value *event* key) next-val))
                 (if-let ((cddr (cddr pairs)))
                   (accumulator cddr)
                   *event*)))))
    (let ((*event* (make-default-event)))
      (setf (slot-value *event* '%beat) (+ (or (slot-value pbind 'start-beat) 0) (beat pbind)))
      (if-let ((pairs (slot-value pbind 'pairs)))
        (accumulator pairs)
        *event*))))

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
  :documentation "Sequentially yields items from LIST, repeating the whole list REPEATS times. OFFSET is the offset to index into the list.

Example:

;; (next-n (pseq '(5 6 7) 2) 7)
;; ;; => (5 6 7 5 6 7 NIL)
;;
;; (next-upto-n (pseq '(5 6 7) 2 1))
;; ;; => (6 7 5 6 7 5)

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
    (when-let ((off (next offset)))
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
  :documentation "Sequentially yields values from LIST, returning a total of LENGTH values.

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
    (when-let ((remaining (remaining-p pser 'length))
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
  :documentation "Gets the value of KEY in the current `*event*' context, returning DEFAULT if that value is nil.

Example:

;; (next-upto-n (pbind :foo (pseq '(1 2 3) 1) :bar (pk :foo)))
;; ;; => ((EVENT :FOO 1 :BAR 1) (EVENT :FOO 2 :BAR 2) (EVENT :FOO 3 :BAR 3))

See also: `pbind', `event-value', `*event*'")

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
  :documentation "Returns random values from LIST.

Example:

;; (next-n (prand '(1 2 3) 5) 6)
;; ;; => (3 2 2 1 1 NIL)

See also: `pxrand', `pwrand', `pwxrand'")

(defmethod as-pstream ((pattern prand))
  (with-slots (list length) pattern
    (make-instance 'prand-pstream
                   :list (next list)
                   :length (as-pstream length))))

(defmethod next ((pattern prand-pstream))
  (when (remaining-p pattern 'length)
    (decf-remaining pattern 'current-repeats-remaining)
    (random-elt (slot-value pattern 'list))))

;;; pxrand

(defpattern pxrand (pattern)
  (list
   (length :default :inf)
   (last-result :state t)
   (current-repeats-remaining :state t))
  :documentation "Returns random values from LIST, never repeating equal values twice in a row.

Example:

;; (next-upto-n (pxrand '(1 2 3) 4))
;; ;; => (3 1 2 1)

See also: `prand', `pwrand', `pwxrand'"
  :defun (assert (position-if-not (lambda (i) (eql i (car list))) list) (list)
                 "pxrand's input list must have at least two non-eql elements."))

(defmethod as-pstream ((pxrand pxrand))
  (with-slots (list length) pxrand
    (make-instance 'pxrand-pstream
                   :list (next list)
                   :length (as-pstream length))))

(defmethod next ((pxrand pxrand-pstream))
  (with-slots (list last-result) pxrand
    (when (remaining-p pxrand 'length)
      (decf-remaining pxrand 'current-repeats-remaining)
      (setf last-result (loop :for res := (random-elt list)
                              :if (or (not (slot-boundp pxrand 'last-result))
                                      (not (eql res last-result)))
                                :return res)))))

;;; pwrand

(defpattern pwrand (pattern)
  (list
   (weights :default (make-list (length list) :initial-element 1))
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Returns random elements from LIST weighted by respective values from WEIGHTS.

Example:

;; (next-upto-n (pwrand '(1 2 3) '(7 5 3) 10))
;; ;; => (1 1 2 2 2 1 2 1 1 3)

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
   (last-result :state t)
   (current-repeats-remaining :state t))
  :documentation "Returns random elements from LIST weighted by respective values from WEIGHTS, never repeating equivalent values twice in a row. This is effectively `pxrand' and `pwrand' combined.

Example:

;; (next-upto-n (pwxrand '(1 2 3) '(7 5 3) 10))
;; ;; => (1 2 1 2 1 3 1 2 1 2)

See also: `prand', `pxrand', `pwrand'"
  ;; FIX: maybe also take the weights into account to see that it doesn't get stuck?
  :defun (assert (position-if-not (lambda (i) (eql i (car list))) list) (list)
                 "pwxrand's input list must have at least two non-eql elements."))

(defmethod as-pstream ((pattern pwxrand))
  (with-slots (list weights length) pattern
    (make-instance 'pwxrand-pstream
                   :list list
                   :weights (pattern-as-pstream weights)
                   :length (as-pstream length))))

(defmethod next ((pwxrand pwxrand-pstream))
  (with-slots (list weights last-result) pwxrand
    (when (remaining-p pwxrand 'length)
      (decf-remaining pwxrand 'current-repeats-remaining)
      (let ((cweights (cumulative-list (normalized-sum (next weights)))))
        (setf last-result (loop :for res := (nth (index-of-greater-than (random 1.0) cweights) list)
                                :if (or (not (slot-boundp pwxrand 'last-result))
                                        (not (eql res last-result)))
                                  :return res))))))

;;; pfunc

(defpattern pfunc (pattern)
  (func
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Yields the result of evaluating FUNC. Note that the current event of the parent pattern is still accessible via the `*event*' special variable.

Example:

;; (next-upto-n (pfunc (lambda () (random 10)) 4))
;; ;; => ((5 2 8 9))
;;
;; (next-upto-n (pbind :foo (pwhite 0 10 4) :bar (pfunc (lambda () (if (> (event-value *event* :foo) 5) :greater :lesser)))))
;; ;; => ((EVENT :FOO 0 :BAR :LESSER) (EVENT :FOO 6 :BAR :GREATER)
;; ;;     (EVENT :FOO 7 :BAR :GREATER) (EVENT :FOO 8 :BAR :GREATER))

See also: `pf', `pnary'"
  :defun (assert (typep func 'function) (func)))

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
  :documentation "Repeats each value from PATTERN REPEATS times. If REPEATS is 0, the value is skipped.

Example:

;; (next-upto-n (pr (pseries) (pseq '(1 3 0 2) 1)))
;; ;; => (0 1 1 1 3 3)

See also: `pdurstutter', `pn', `pdrop', `parp'")

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
          :do (setf current-value (next pattern))
              (when current-value
                (setf current-repeats-remaining
                      (let ((*event* (if (event-p current-value)
                                         (if (null *event*)
                                             current-value
                                             (combine-events *event* current-value))
                                         *event*)))
                        (if (typep repeats 'function)
                            (let ((fle (function-lambda-expression repeats)))
                              (if fle
                                  (if (length= 0 (cadr fle))
                                      (funcall repeats)
                                      (funcall repeats current-value))
                                  (handler-case
                                      (funcall repeats current-value) ;; FIX: just provide the current-value as a key in *event*
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
  (getf (pdef-ref pdef-key) (make-keyword key)))

(defun pdef-ref-set (pdef-key key pattern)
  "Set PDEF-KEY's KEY in its plist to PATTERN."
  (pdef-ref pdef-key (plist-set (pdef-ref pdef-key) (make-keyword key) pattern)))

(defpattern pdef (pattern)
  ((key :reader pdef-key)
   (current-pstream :state t))
  :documentation "Defines a named pattern, with KEY being the name of the pattern and PATTERN the pattern itself. Named patterns are stored by name in a global dictionary and can be referred back to by calling `pdef' without supplying PATTERN. The global dictionary also keeps track of the pdef's pstream when `play' is called on it. If a pdef is redefined while it is currently being played, the changes won't be audible until either PATTERN ends, or the pdef's `quant' time is reached. Note that, unlike bare patterns, pdefs loop by default when played (`loop-p').

Example:

;; (pdef :foo (pbind :degree (pseries 0 1 4)))
;;
;; (play (pdef :foo))
;; ;; redefine the pdef's pattern... note that the redefinition doesn't become audible until the current loop finishes playing:
;; (pdef :foo (pbind :degree (pseries 4 -1 4)))

See also: `find-pdef', `all-pdefs', `pb', `pmeta', `ps'"
  :defun (defun pdef (key &optional (pattern nil pattern-supplied-p))
           (when pattern-supplied-p
             (pdef-ref-set key :pattern pattern))
           (make-instance 'pdef
                          :key key)))

(defun find-pdef (key)
  "Get the pdef with the provided name, or nil if one does not exist.

See also: `pdef', `all-pdefs'"
  (etypecase key
    (pattern
     key)
    (symbol
     (when (pdef-ref key)
       (pdef key)))))

(defmethod print-object ((pdef pdef) stream)
  (with-slots (key) pdef
    (format stream "(~s ~s)" 'pdef key)))

(defmethod print-object ((pdef pdef-pstream) stream)
  (with-slots (key) pdef
    (print-unreadable-object (pdef stream :type t)
      (format stream "~s" key))))

(create-global-dictionary pdef)

(defun all-pdefs ()
  "Get a list of all pdefs.

See also: `all-patterns'"
  (keys *pdef-dictionary*))

(defun ensure-pdef (object)
  "Attempt to ensure OBJECT is a pdef."
  (etypecase object
    (pdef object)
    (symbol (pdef object))
    (string (ensure-pdef (make-keyword object)))))

(defun pdef-pattern (pdef)
  "Get the pattern that PDEF points to."
  (pdef-ref-get (pdef-key (ensure-pdef pdef)) :pattern))

(defmethod quant ((pdef pdef))
  (if (slot-boundp pdef 'quant)
      (slot-value pdef 'quant)
      (quant (pdef-pattern pdef))))

(defmethod loop-p ((pdef pdef))
  (if (slot-boundp pdef 'loop-p)
      (slot-value pdef 'loop-p)
      (let ((pattern (pdef-pattern pdef)))
        (if (slot-boundp pattern 'loop-p)
            (slot-value pattern 'loop-p)
            t))))

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
   (repeats :default :inf)
   (current-pstream :state t :initform nil)
   (current-repeats-remaining :state t :initform nil))
  :documentation "Evaluates FUNC to generate a pattern, which is then yielded from until its end, at which point FUNC is evaluated again to generate the next pattern. The pattern is generated a total of REPEATS times.

Example:

;; (next-n (plazy (lambda () (if (= 0 (random 2)) (pseq '(1 2 3) 1) (pseq '(9 8 7) 1)))) 6)
;; ;; => (9 8 7 1 2 3)

See also: `pfunc'")

(defmethod as-pstream ((pattern plazy))
  (with-slots (func repeats) pattern
    (make-instance 'plazy-pstream
                   :func func
                   :repeats (as-pstream repeats))))

(defmethod next ((pattern plazy-pstream))
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

(defun pshift (pattern shift &optional (max-yield *max-pattern-yield-length*)) ;; FIX: don't use pseq internally, and make it possible for 'shift' to be a pattern
  (pseq (alexandria:rotate (next-upto-n pattern max-yield) shift)))

;;; pn

(defpattern pn (pattern)
  (pattern
   (repeats :default :inf)
   (current-repeats-remaining :state t)
   (current-pstream :state t :initform nil))
  :documentation "pn embeds the full PATTERN into the pstream REPEATS times.")

(defmethod as-pstream ((pn pn)) ;; need this so that PATTERN won't be automatically converted to a pstream when the pn is.
  (with-slots (pattern repeats) pn
    (make-instance 'pn-pstream
                   :pattern pattern
                   :repeats (as-pstream repeats))))

(defmethod next ((pn pn-pstream))
  (with-slots (pattern current-pstream) pn
    (when-let ((rem (remaining-p pn)))
      (when (eql :reset rem)
        (setf current-pstream (as-pstream pattern)))
      (let ((nv (next current-pstream)))
        (loop :while (and (null nv) rem)
              :do (decf-remaining pn 'current-repeats-remaining)
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
  :documentation "pshuf shuffles LIST, then yields each item from the shuffled list, repeating the list REPEATS times.

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
    (when-let ((rem (remaining-p pattern)))
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
;; => (7 2 4 5 7 10 4 8 10 2 3 5 9 2 5 4)

See also: `pexprand', `pbrown', `pgauss', `prand'")

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
      (when-let ((nlo (next lo))
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
  :documentation "Brownian motion within a range; each output a maximum of STEP away from the previous. LO and HI define the lower and upper bounds of the range.

Example:

;; (next-upto-n (pbrown 0 10 1 10))
;; => (2 3 3 3 4 3 4 5 6 5)

See also: `pwhite', `pexprand', `pgauss'")

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
      (when-let ((nlo (next lo))
                            (nhi (next hi))
                            (nstep (next step)))
        (when (null current-value)
          (setf current-value (random-range nlo nhi)))
        (incf current-value (random-range (* -1 nstep) nstep))
        (setf current-value (clamp current-value nlo nhi))))))

;;; pexprand
;; FIX: should integer inputs result in integer outputs?
;; FIX: assert against 0 as an input

(defpattern pexprand (pattern)
  ((lo :default 0.0001)
   (hi :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Exponentially-distributed random numbers between LO and HI. Note that LO and HI cannot be 0, and that LO and HI must have the same sign or else complex numbers will be output.

Example:

;; (next-upto-n (pexprand 1.0 8.0 4))
;; => (1.0420843091865208d0 1.9340168112124456d0 2.173209129035095d0 4.501371557329618d0)

See also: `pwhite', `pbrown', `pgauss', `prand'")

(defmethod as-pstream ((pexprand pexprand))
  (with-slots (lo hi length) pexprand
    (make-instance 'pexprand-pstream
                   :lo (pattern-as-pstream lo)
                   :hi (pattern-as-pstream hi)
                   :length (as-pstream length))))

(defmethod next ((pexprand pexprand-pstream))
  (with-slots (lo hi) pexprand
    (when (remaining-p pexprand 'length)
      (decf-remaining pexprand 'current-repeats-remaining)
      (when-let ((nlo (next lo))
                            (nhi (next hi)))
        (exponential-random-range nlo nhi)))))

;;; pgauss

(defpattern pgauss (pattern)
  ((mean :default 0.0)
   (deviation :default 1.0)
   (length :default :inf)
   (current-repeats-remaining :state t))
  :documentation "Random numbers distributed along a normal (gaussian) curve. MEAN is the \"center\" of the distribution, DEVIATION is the standard deviation (i.e. the higher the value, the further the outputs are spread from MEAN).

Example:

;; (next-n (pgauss) 4)
;; => (0.08918811646370092d0 0.1745957067161632d0 0.7954678768273173d0 -1.2215823449671597d0)

See also: `pwhite', `pexprand', `pbrown'")

(defmethod as-pstream ((pgauss pgauss))
  (with-slots (mean deviation length) pgauss
    (make-instance 'pgauss-pstream
                   :mean (pattern-as-pstream mean)
                   :deviation (pattern-as-pstream deviation)
                   :length (as-pstream length))))

(defmethod next ((pgauss pgauss-pstream))
  (with-slots (mean deviation) pgauss
    (when (remaining-p pgauss 'length)
      (decf-remaining pgauss 'current-repeats-remaining)
      (when-let ((nmean (next mean))
                 (ndev (next deviation)))
        (random-gauss nmean ndev)))))

;;; pseries

(defpattern pseries (pattern)
  ((start :default 0)
   (step :default 1)
   (length :default :inf)
   (current-repeats-remaining :state t)
   (current-value :state t))
  :documentation "Yields START and then generates subsequent values by adding STEP, for a total of LENGTH values yielded.

Example:

;; (next-upto-n (pseries 1 2 4))
;; ;; => (1 3 5 7)

See also: `pseries*', `pgeom', `paccum'")

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

;;; pseries*

(defun pseries* (&optional (start 0) (end 1) length)
  "Syntax sugar to generate a `pseries' whose values go from START to END linearly over LENGTH steps. If LENGTH is not provided, it is calculated such that the step will be 1. Note that LENGTH cannot be infinite since delta calculation requires dividing by it.

Based on the Pseries extension from the ddwPatterns SuperCollider library.

Example:

;; (pseries* 0 10 16)
;; ;; => (pseries 0 2/3 16)
;;
;; (next-upto-n *)
;; ;; => (0 2/3 4/3 2 8/3 10/3 4 14/3 16/3 6 20/3 22/3 8 26/3 28/3 10)

See also: `pseries', `pgeom', `pgeom*'"
  (assert (or (null length)
              (and (integerp length) (> length 1))) ;; FIX: it should be possible to provide length as a pattern; same for pgeom*
          (length)
          "LENGTH must be an integer greater than 1 (~s provided)." length)
  (let ((length (or length
                    (max 2 (round (1+ (abs (- end start))))))))
    (pseries start (/ (- end start) (1- length)) length)))

;;; pgeom

(defpattern pgeom (pattern)
  ((start :default 1)
   (grow :default 2)
   (length :default :inf)
   (current-repeats-remaining :state t)
   (current-value :state t))
  :documentation "Yields START, and then generates subsequent values by multiplying by GROW, for a total of LENGTH values yielded.

Example:

;; (next-upto-n (pgeom 1 2 4))
;; ;; => (1 2 4 8)

See also: `pseries', `paccum'")

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
          (when-let ((n (next grow)))
            (setf current-value (* current-value n)))))))

;;; pgeom*

(defun pgeom* (&optional (start 0) (end 1) (length 16))
  "Syntax sugar to generate a `pgeom' whose values go from START to END exponentially over LENGTH steps. LENGTH cannot be infinite since delta calculation requires dividing by it.

Based on the Pgeom extension from the ddwPatterns SuperCollider library.

Example:

;; (pgeom* 1 100 8)
;; ;; => (pgeom 1 1.9306977 8)
;;
;; (next-upto-n *)
;; ;; => (1 1.9306977 3.7275934 7.196856 13.894953 26.826954 51.79474 99.999985)
;; ;; Note that due to floating point rounding errors the last output may not always be exactly END.

See also: `pgeom', `pseries', `pseries*'"
  (assert (and (integerp length) (> length 1)) (length) "LENGTH must be an integer greater than 1 (~s provided)." length)
  (pgeom start (expt (/ end start) (/ 1 (1- length))) length))

;;; ptrace

(defpattern ptrace (pattern)
  (pattern
   (key :default nil)
   (stream :default t)
   (prefix :default ""))
  :documentation "ptrace prints to STREAM the PREFIX and then the value of KEY for each event yielded by PATTERN, or the whole event or value if KEY is not provided. ptrace yields everything from the source PATTERN unaffected.")

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
           (result (if (null key)
                       n
                       (event-value n key))))
      (format stream "~a~a~%" prefix result)
      n)))

;;; place

(defpattern place (pattern)
  (list
   (repeats :default :inf)
   (current-repeat :state t)
   (current-repeats-remaining :state t))
  :documentation "place yields each value from LIST in sequence. If the value is a list, the first element of that list is yielded. The second time that sub-list is encountered, its second element will be yielded; the third time, the third element, and so on. REPEATS controls the number of times LIST is repeated.

Example:

;; (next-upto-n (place (list 1 2 (list 3 4 5)) 3))
;; ;; => (1 2 3 1 2 4 1 2 5)

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
      (decf-remaining place 'current-repeats-remaining))
    (when (if (plusp number)
              (and (not (null (pstream-elt place -1)))
                   (remaining-p place))
              (remaining-p place))
      (let* ((mod (mod number (length list)))
             (result (next (nth mod list))))
        (if (listp result)
            (elt-wrap result current-repeat)
            result)))))

;;; ppatlace

(defpattern ppatlace (pattern)
  (list
   (repeats :default :inf)
   (current-repeats-remaining :state t))
  :documentation "ppatlace yields each value from LIST in sequence. If the value is a pattern, one value is yielded from that pattern before moving on to the next item in LIST. The second time around the LIST, the second value yielded from each pattern in LIST will be yielded instead. If one of the patterns embedded in LIST ends sooner than the others, it is simply removed and the ppatlace continues to yield from the rest of the LIST. The entire LIST is yielded through a total of REPEATS times.

See also: `place'")

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
  :documentation "pnary yields the result of applying OPERATOR to each value yielded by each pattern in PATTERNS.

See also: `pfunc'"
  :defun (defun pnary (operator &rest patterns)
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
    (let ((op (if (pstream-p operator)
                  (next operator)
                  operator))
          (nexts (mapcar #'next patterns)))
      (unless (or (position nil nexts)
                  (null op))
        (apply #'multi-channel-funcall op nexts)))))

(defun p+ (&rest numbers)
  "Add NUMBERS, where NUMBERS can be any object that responds to the `next' method. This function is simply a shortcut for (apply #'pnary #'+ numbers)."
  (apply #'pnary #'+ numbers))

(defun p- (&rest numbers)
  "Subtract NUMBERS, where NUMBERS can be any object that responds to the `next' method. This function is simply a shortcut for (apply #'pnary #'- numbers)."
  (apply #'pnary #'- numbers))

(defun p* (&rest numbers)
  "Multiply NUMBERS, where NUMBERS can be any object that responds to the `next' method. This function is simply a shortcut for (apply #'pnary #'* numbers)."
  (apply #'pnary #'* numbers))

(defun p/ (&rest numbers)
  "Divide NUMBERS, where NUMBERS can be any object that responds to the `next' method. This function is simply a shortcut for (apply #'pnary #'/ numbers)."
  (apply #'pnary #'/ numbers))

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
    (when-let ((input (next input))
               (from-range (next from-range))
               (to-range (next to-range)))
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
   (current-value :state t :initform nil))
  :documentation "Slides across sections of LIST. REPEATS is the total number of sections to output, LEN the length of the section. STEP is the number to increment the start index by after each section, and START is the initial index into LIST that the first section starts from. WRAP-AT-END, when true, means that an index outside of the list will wrap around. When false, indexes outside of the list result in nil.

Example:

;; (next-upto-n (pslide (list 0 1 2 3 4 5 6) 3 3 2 1 t))
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
                   (if wrap-at-end
                       (elt-wrap list current-value)
                       (nth current-value list)))))
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
              (setf current-value (+ start (* (next step) current-repeats)))
              (next pattern)))))))

;;; phistory

(defpattern phistory (pattern)
  (pattern
   step-pattern)
  :documentation "phistory refers back to PATTERN's history, yielding the value at the index provided by STEP-PATTERN. Note that PATTERN is still advanced once per event, and if STEP-PATTERN yields a number pointing to an event in PATTERN that hasn't been yielded yet (i.e. if PATTERN has only advanced once but STEP-PATTERN yields 3 for its output), it will return nil.

Example:

;; (next-n (phistory (pseries) (pseq '(0 2 1))) 3)
;; ;; => (0 NIL 1)

See also: `pscratch'")

(defmethod as-pstream ((phistory phistory))
  (with-slots (pattern step-pattern) phistory
    (make-instance 'phistory-pstream
                   :pattern (as-pstream pattern)
                   :step-pattern (as-pstream step-pattern))))

(defmethod next ((pstream phistory-pstream))
  (with-slots (pattern step-pattern) pstream
    (when-let ((next-step (next step-pattern)))
      (next pattern)
      (handler-case (pstream-elt pattern next-step)
        (pstream-out-of-range ()
          nil)))))

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
;; ;; => (0 0 1 2 1 3 3 4 5 4)

See also: `phistory'")

(defmethod as-pstream ((pscratch pscratch))
  (with-slots (pattern step-pattern) pscratch
    (make-instance 'pscratch-pstream
                   :pattern (as-pstream pattern)
                   :step-pattern (pattern-as-pstream step-pattern))))

(defmethod next ((pscratch pscratch-pstream))
  (with-slots (pattern step-pattern current-index) pscratch
    (when-let ((nxt (next step-pattern)))
      (prog1
          (pstream-elt-future pattern current-index)
        (setf current-index (max (+ current-index nxt) 0))))))

;;; pif

(defpattern pif (pattern)
  (test true false)
  :documentation "pif acts as an if statement for patterns. TEST is evaluated for each step, and if it's non-nil, the value of TRUE will be yielded, otherwise the value of FALSE will be. Note that TRUE and FALSE can be patterns, and if they are, they are only advanced in their respective cases, not for every step. Also note that pif will continue to advance even if TEST yields nil; pif only yields nil if TRUE or FALSE do.

Example:

;; (next-n (pif (pseq '(t t nil nil nil)) (pseq '(1 2)) (pseq '(3 nil 4))) 5)
;; => (1 2 3 NIL 4)")

(defmethod as-pstream ((pif pif))
  (with-slots (test true false) pif
    (make-instance 'pif-pstream
                   :test (pattern-as-pstream test)
                   :true (pattern-as-pstream true)
                   :false (pattern-as-pstream false))))

(defmethod next ((pif pif-pstream))
  (with-slots (test true false) pif
    (if (next test)
        (next true)
        (next false))))

;;; parp

(defpattern parp (pattern)
  (pattern
   arpeggiator
   (current-pattern-event :state t :initform nil)
   (current-arpeggiator-stream :state t :initform nil))
  :documentation "parp is an \"arpeggiator\"; each event yielded by PATTERN is bound to *event* and then the entirety of the ARPEGGIATOR pattern is yielded.

Example:

;; (next-n (parp (pbind :foo (pseq '(1 2 3))) (pbind :bar (pseq '(4 5 6) 1))) 9)
;; ;; => ((EVENT :FOO 1 :BAR 4) (EVENT :FOO 1 :BAR 5) (EVENT :FOO 1 :BAR 6)
;; ;;     (EVENT :FOO 2 :BAR 4) (EVENT :FOO 2 :BAR 5) (EVENT :FOO 2 :BAR 6)
;; ;;     (EVENT :FOO 3 :BAR 4) (EVENT :FOO 3 :BAR 5) (EVENT :FOO 3 :BAR 6))

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
  :documentation "pfin yields up to COUNT outputs from PATTERN.

Example:

;; (next-n (pfin (pseq '(1 2 3) :inf) 3) 5)
;; ;; => (1 2 3 NIL NIL)

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
   (current-dur :state t)
   (elapsed-dur :state t :initform 0))
  :documentation "pfindur yields events from PATTERN until their total duration is within TOLERANCE of DUR, or greater than DUR. Any events that would end beyond DUR are cut short. If PATTERN outputs numbers, their total sum is limited instead.

Example:

;; (next-n (pfindur (pbind :dur 1 :foo (pseries)) 2) 3)
;; => ((EVENT :DUR 1 :FOO 0) (EVENT :DUR 1 :FOO 1) NIL)
;;
;; (next-upto-n (pfindur (pwhite 0 4) 16))
;; => (1 3 0 1 2 2 1 3 0 1 2)
;; (reduce #'+ *)
;; => 16

See also: `pfin', `psync'")

(defmethod as-pstream ((pfindur pfindur))
  (with-slots (pattern dur tolerance) pfindur
    (make-instance 'pfindur-pstream
                   :pattern (pattern-as-pstream pattern)
                   :dur (as-pstream dur)
                   :tolerance (next tolerance))))

(defmethod next ((pfindur pfindur-pstream))
  (flet ((get-delta (ev)
           (if (event-p ev)
               (event-value ev :delta)
               ev)))
    (with-slots (pattern dur tolerance current-dur elapsed-dur) pfindur
      (when-let ((n-event (next pattern)))
        (unless (slot-boundp pfindur 'current-dur)
          (setf current-dur (next dur)))
        (when current-dur
          (if (eql :inf current-dur)
              n-event
              (let ((new-elapsed (+ (get-delta n-event) elapsed-dur)))
                (prog1
                    (if (> (if (= 0 tolerance)
                               new-elapsed
                               (round-by-direction new-elapsed tolerance))
                           current-dur)
                        (let ((tdur (- current-dur elapsed-dur)))
                          (when (plusp tdur)
                            (if (event-p n-event)
                                (combine-events n-event (event :dur tdur))
                                tdur)))
                        n-event)
                  (incf elapsed-dur (get-delta n-event))))))))))

;;; psync

(defpattern psync (pattern)
  (pattern
   sync-quant
   (maxdur :default nil)
   (tolerance :default 0.001)
   (elapsed-dur :state t :initform 0))
  :documentation "psync yields events from PATTERN until their total duration is within TOLERANCE of MAXDUR, cutting off any events that would extend past MAXDUR. If PATTERN ends before MAXDUR, a rest is added to the pstream to round its duration up to the nearest multiple of SYNC-QUANT.

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
  (with-slots (pattern sync-quant maxdur tolerance elapsed-dur) psync
    (let ((n-event (next pattern))
          (delta (- (round-by-direction elapsed-dur sync-quant) elapsed-dur)))
      (when-let ((res-event (if (null n-event)
                                (when (plusp delta)
                                  (event :type :rest :dur delta))
                                (when (or (null maxdur)
                                          (not (>= elapsed-dur maxdur)))
                                  (if (and (not (null maxdur))
                                           (> (+ elapsed-dur (event-value n-event :dur)) maxdur))
                                      (combine-events n-event (event :dur (- maxdur elapsed-dur)))
                                      n-event)))))
        (incf elapsed-dur (event-value res-event :dur))
        res-event))))

;;; pdurstutter
;; FIX: make a version where events skipped with 0 are turned to rests instead (to keep the correct dur)

(defpattern pdurstutter (pattern)
  (pattern
   n
   (current-value :state t :initform nil)
   (current-repeats-remaining :state t :initform 0))
  :documentation "pdurstutter yields each output from PATTERN N times, dividing it by N. If the output from PATTERN is an event, its dur is divided; if it's a number, the number itself is divided instead of being yielded directly.

Example:

;; (next-n (pdurstutter (pseq '(1 2 3 4 5)) (pseq '(3 2 1 0 2))) 9)
;; => (1/3 1/3 1/3 1 1 3 5/2 5/2 NIL)
;;
;; (next-n (pdurstutter (pbind :dur (pseq '(1 2 3 4 5)))
;;                      (pseq '(3 2 1 0 2)))
;;         9)
;; => ((EVENT :DUR 1/3) (EVENT :DUR 1/3) (EVENT :DUR 1/3) (EVENT :DUR 1) (EVENT :DUR 1) (EVENT :DUR 3) (EVENT :DUR 5/2) (EVENT :DUR 5/2) NIL)

See also: `pr'")

(defmethod as-pstream ((pdurstutter pdurstutter))
  (with-slots (pattern n) pdurstutter
    (make-instance 'pdurstutter-pstream
                   :pattern (as-pstream pattern)
                   :n (pattern-as-pstream n))))

(defmethod next ((pattern pdurstutter-pstream))
  (with-slots (n current-value current-repeats-remaining) pattern
    (loop :while (and (not (null current-repeats-remaining))
                      (= 0 current-repeats-remaining))
          :do (setf current-repeats-remaining (next n))
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
  :documentation "pbeat yields the number of beats elapsed since its embedding in the pstream.

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
  :documentation "ptime yields the number of seconds elapsed since its embedding in the pstream.

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
  :documentation "Use INDEX-PAT to index into the list returned by LIST-PAT. WRAP-P is whether indexes that are out of range will be wrapped (if t) or will simply return nil.

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
        (funcall (if wrap-p 'nth-wrap 'nth) idx list)))))

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
    (unless (parent-pbind prun)
      (error "~s cannot be used outside of a pbind" prun))
    (make-instance 'prun-pstream
                   :pattern (as-pstream pattern)
                   :dur (pattern-as-pstream dur)
                   :current-dur 0)))

(defmethod next ((prun prun-pstream))
  (with-slots (pattern dur current-dur dur-history number) prun
    (let ((beats (beat (parent-pbind prun))))
      (flet ((next-dur ()
               (when-let ((nxt (next dur)))
                 (next pattern)
                 (incf current-dur nxt))))
        (when (= number 0)
          (next-dur))
        (loop :while (and (or (not (pstream-p dur))
                              (not (ended-p dur)))
                          (<= current-dur beats))
              :do (next-dur))))
    (pstream-elt pattern -1)))

;;; psym

(defpattern psym (pattern)
  (pattern
   (current-pstream :state t :initform nil))
  :documentation "Use a pattern of symbols to embed `pdef's. PATTERN is the source pattern that yields symbols naming the pdef to embed.

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
  (labels ((maybe-pdef (x)
             (if-let ((pdef (and (symbolp x)
                                 (pdef-ref-get x :pattern))))
               pdef
               x)))
    (with-slots (pattern current-pstream) psym
      (let ((n (next current-pstream)))
        (if n
            n
            (let ((next-pdef (next pattern)))
              (unless (null next-pdef)
                (setf current-pstream (as-pstream (if (listp next-pdef)
                                                      (ppar (mapcar #'maybe-pdef next-pdef))
                                                      (maybe-pdef next-pdef))))
                (next psym))))))))

;;; pchain

(defpattern pchain (pattern)
  (patterns)
  :documentation "Combine multiple patterns into one event stream.

Example:

;; (next-n (pchain (pbind :foo (pseq '(1 2 3))) (pbind :bar (pseq '(7 8 9) 1))) 4)
;;
;; => ((EVENT :FOO 1 :BAR 7) (EVENT :FOO 2 :BAR 8) (EVENT :FOO 3 :BAR 9) NIL)

See also: `pbind''s :embed key"
  :defun (defun pchain (&rest patterns)
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
  :documentation "Output the difference between successive outputs of PATTERN.

Example:

;; (next-n (pdiff (pseq '(3 1 4 3) 1)) 4)
;;
;; => (-2 3 -1 NIL)

See also: `pdelta'")

(defmethod next ((pdiff pdiff-pstream))
  (with-slots (pattern) pdiff
    (when (zerop (slot-value pattern 'history-number))
      (next pattern))
    (when-let ((last (pstream-elt pattern -1))
               (next (next pattern)))
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
  (with-slots (pattern cycle history-number) pdelta
    (when (zerop history-number)
      (next pattern))
    (when-let ((lv (or (pstream-elt pattern -1) 0))
               (cv (next pattern)))
      (- cv
         (- lv (round-by-direction (- lv cv) cycle))))))

;;; pdrop

(defpattern pdrop (pattern)
  (pattern
   (n :default 0))
  :documentation "Drop the first N outputs from PATTERN and yield the rest. If N is negative, drop the last N outputs from PATTERN instead.

Example:

;; (next-n (pdrop (pseq '(1 2 3 4) 1) 2) 4)
;;
;; => (3 4 NIL NIL)

See also: `pshift'")

(defmethod as-pstream ((pdrop pdrop))
  ;; FIX: check that n is not bigger or smaller than history allows
  (with-slots (pattern n) pdrop
    (make-instance 'pdrop-pstream
                   :pattern (as-pstream pattern)
                   :n n)))

(defmethod next ((pdrop pdrop-pstream))
  (with-slots (pattern n number) pdrop
    (if (minusp n)
        (unless (pstream-elt-future pattern (- number n))
          (return-from next nil))
        (when (zerop (slot-value pattern 'history-number))
          (dotimes (i n)
            (next pattern))))
    (next pattern)))

;;; ppar

(defpattern ppar (pattern)
  (list
   (pstreams :state t :initform nil))
  :documentation "Combine multiple event patterns into one pstream with all events in temporal order. LIST is the list of patterns, or a pattern yielding lists of patterns. The ppar ends when all of the patterns in LIST end.

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
  (with-slots (list pstreams history-number) ppar
    (labels ((next-in-pstreams ()
               (when-let ((res (remove-if (lambda (pstream)
                                            (and (not (zerop (slot-value pstream 'history-number)))
                                                 (null (pstream-elt pstream -1))))
                                          pstreams)))
                 (most-x res #'< #'beat)))
             (maybe-reset-pstreams ()
               (when (null (remove-if #'null pstreams))
                 (let ((next-list (next list)))
                   (when (null next-list)
                     (return-from maybe-reset-pstreams nil))
                   (setf pstreams (mapcar #'as-pstream next-list))))))
      (when (zerop history-number)
        (maybe-reset-pstreams))
      (let ((nxt (next (next-in-pstreams))))
        (if (null nxt)
            (let ((nip (next-in-pstreams)))
              (when nip
                (let ((beppar (beat ppar))
                      (benip (beat nip)))
                  (if (< beppar benip)
                      (event :type :rest :delta (- benip beppar))
                      (next ppar)))))
            (combine-events nxt (event :delta (- (beat (next-in-pstreams)) (beat ppar)))))))))

;;; pmeta

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

;; (pdef :foo (pbind :x (pseq '(1 2 3) 1) :dur 1))
;; (pdef :bar (pbind :y (pseries) :dur (pn 1/2 3)))
;; (pmeta (pbind :pattern (pseq (list :foo :bar) 1) :dur 2))
;; => ((EVENT :X 1 :DUR 1) (EVENT :X 2 :DUR 1) ;; from (pdef :foo)
;;     (EVENT :Y 0 :DUR 1/2) (EVENT :Y 1 :DUR 1/2) (EVENT :Y 2 :DUR 1/2) (EVENT :TYPE :REST :DUR 1/2)) ;; from (pdef :bar)

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
    (labels ((make-pstream (plist)
               (unless plist
                 (return-from make-pstream))
               (let (res-pattern)
                 (doplist (key value plist)
                   (when (null value)
                     (return-from make-pstream nil))
                   (case key
                     ((:pattern :instrument)
                      (setf res-pattern (etypecase value
                                          (symbol
                                           (pdef value))
                                          (pattern
                                           value))))
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
                      (setf res-pattern (pchain res-pattern (as-pstream value))))
                     ((:step-inject :sinject)
                      (setf res-pattern (pchain res-pattern (pn (next value)))))))
                 (let ((pstream (as-pstream res-pattern)))
                   (setf (slot-value pstream 'parent) pmeta)
                   pstream))))
      (unless current-pstream
        (setf current-pstream (make-pstream (if (listp pattern)
                                                (loop :for (key value) :on pattern :by #'cddr
                                                      :if (pstream-p value)
                                                        :append (list key (next value))
                                                      :else
                                                        :append (list key value))
                                                (event-plist (next pattern))))))
      (when current-pstream
        (if-let ((nxt (next current-pstream)))
          nxt
          (progn
            (setf current-pstream nil)
            (next pmeta)))))))

;;; pts

(defpattern pts (pattern)
  (pattern
   (dur :default 4))
  :documentation "Timestretch PATTERN so its total duration is DUR. Note that only the first `*max-pattern-yield-length*' events from PATTERN are considered, and that they are calculated immediately at pstream creation time rather than lazily as the pstream yields.

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
  :documentation "\"Walk\" over the values in LIST by using the accumulated value of the outputs of STEP-PATTERN as the index. At the beginning of the pwalk and each time the start or end of the list is passed, the output of DIRECTION-PATTERN is taken and used as the multiplier for values from STEP-PATTERN. START-POS is the index in LIST for pwalk to take its first output from.

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

;;; pparchain

(defpattern pparchain (pchain)
  (patterns)
  :documentation "Combine multiple patterns into several event streams. The event yielded by the first pattern will be used as the input event to the second pattern, and so on. The events yielded by each pattern will be collected into a list and yielded by the pparchain. This pattern is effectively `ppar' and `pchain' combined.

Example:

;; (next-upto-n (pparchain (pbind :foo (pseries 0 1 3)) (pbind :baz (p+ (pk :foo) 1) :foo (p+ (pk :foo) 3))))
;; => (((EVENT :FOO 0) (EVENT :FOO 3 :BAZ 1))
;;     ((EVENT :FOO 1) (EVENT :FOO 4 :BAZ 2))
;;     ((EVENT :FOO 2) (EVENT :FOO 5 :BAZ 3)))

See also: `ppc', `ppar', `pchain', `pbind''s :embed key"
  :defun (defun pparchain (&rest patterns)
           (set-parents
            (make-instance 'pparchain
                           :patterns patterns))))

(defmethod as-pstream ((pparchain pparchain))
  (with-slots (patterns) pparchain
    (make-instance 'pparchain-pstream
                   :patterns (loop :for pattern :in patterns
                                   :collect (as-pstream pattern)))))

(defmethod next ((pparchain pparchain-pstream))
  (with-slots (patterns) pparchain
    (let ((c-event (make-default-event)))
      (loop :for pattern :in patterns
            :do (setf c-event (combine-events c-event (let ((*event* (copy-event c-event)))
                                                        (next pattern))))
            :if (null c-event)
              :return nil
            :else
              :collect c-event))))

;;; ppc

(defmacro ppc (&body pairs)
  "Syntax sugar for `pparchain' that automatically splits PAIRS by :- symbols.

Example:

;; (ppc :foo (pseq (list 1 2 3) 1)
;;      :-
;;      :bar (p+ (pk :foo) 2))
;; => (((EVENT :FOO 1) (EVENT :FOO 1 :BAR 3))
;;     ((EVENT :FOO 2) (EVENT :FOO 2 :BAR 4))
;;     ((EVENT :FOO 3) (EVENT :FOO 3 :BAR 5)))

See also: `pparchain'"
  (labels ((ppc-split (pairs)
             (let ((pos (position :- pairs)))
               (if pos
                   (list (subseq pairs 0 pos)
                         (ppc-split (subseq pairs (1+ pos))))
                   pairs))))
    `(pparchain ,@(loop :for i :in (ppc-split pairs)
                        :collect (cons 'pbind i)))))

;;; pclump

(defpattern pclump (pattern)
  (pattern
   (n :default 1))
  :documentation "Group outputs of the source pattern into lists of up to N items each.

Example:

;; (next-upto-n (pclump (pseries 0 1 5) 2))
;; => ((0 1) (2 3) (4))

See also: `paclump'")

(defmethod next ((pclump pclump-pstream))
  (with-slots (pattern n) pclump
    (when-let ((next (next n)))
      (next-upto-n pattern next))))

;;; paclump

(defpattern paclump (pattern)
  (pattern)
  :documentation "Automatically group outputs of the source pattern into lists of up to N items each. Unlike `pclump', clump size is automatically set to the length of the longest list in the values of `*event*', or 1 if there are no lists.

Example:

;; (next-upto-n (pbind :foo (pseq '((1) (1 2) (1 2 3)) 1) :bar (paclump (pseries))))
;; => ((EVENT :FOO (1) :BAR (0)) (EVENT :FOO (1 2) :BAR (1 2)) (EVENT :FOO (1 2 3) :BAR (3 4 5)))

See also: `pclump'")

(defmethod next ((paclump paclump-pstream))
  (with-slots (pattern) paclump
    (when *event*
      (let ((max (loop :for key :in (keys *event*)
                       :maximizing (length (ensure-list (event-value *event* key))))))
        (next-upto-n pattern max)))))

;;; ps

(defpattern ps (pattern)
  (pattern
   pstream)
  :documentation "Defines a pattern whose pstream will be preserved to subsequent calls to `as-pstream'. To reset the pstream, simply re-evaluate the ps definition.

Based on the pattern originally from the miSCellaneous SuperCollider library.

Example:

;; (defparameter pst (ps (pseries)))
;;
;; (next-upto-n pst 4)
;; => (0 1 2 3)
;;
;; (next-upto-n pst 4)
;; => (4 5 6 7)
;;
;; (defparameter pst (ps (pseries)))
;;
;; (next-upto-n pst 4)
;; => (0 1 2 3)

See also: `pdef'"
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
