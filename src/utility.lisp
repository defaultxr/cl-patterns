(in-package #:cl-patterns)

;;; customizable settings

(defvar *cl-patterns-temporary-directory*
  (merge-pathnames "cl-patterns/" (uiop:temporary-directory))
  "The default directory to store `render'ed files in.")

;;; special variables

(defvar *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defvar *clock* nil
  "The default clock to run tasks on.")

(defparameter eop nil
  "End Of Pattern; the symbol yielded by patterns after their last output.")

;;; string stuff

(defun string-replace (string old new)
  "Find the first instance of OLD in STRING and replace it with NEW. Return the new string, or if OLD was not found, return STRING unchanged. Returns the position that OLD was found as a second value, or nil if it was not found."
  (let ((pos (search old string)))
    (values
     (if pos
         (concatenate 'string (subseq string 0 pos) new (subseq string (+ pos (length old))))
         string)
     pos)))

;;; list stuff

(uiop:with-deprecation (:style-warning)
  (defun gete (list key)
    "Get a list of the value of KEY for each event in LIST."
    (mapcar (fn (event-value _ key)) list)))

(defun normalized-sum (list)
  "Return a copy of LIST normalized so all of its numbers summed together equal 1."
  (mapcar (lambda (x) (/ x (apply #'+ list))) list))

(defun cumulative-list (list)
  "Return a copy of LIST where the elements previous are added to the current one.

Example:

;; (cumulative-list (list 1 2 3 4))
;; => (1 3 6 10)"
  (loop :for element :in list
     :for index :from 0
     :collect (apply #'+ element (subseq list 0 index))))

(defun index-of-greater-than (n list)
  "Get the index of the first element of LIST greater than N."
  (position-if (lambda (num) (> num n)) list))

(defgeneric last-dur (object)
  (:documentation "Get the beat position of the ending of the last event in the ESEQ."))

(defmethod last-dur ((list list))
  (if (car list)
      (reduce #'max list :key (lambda (ev) (+ (beat ev) (event-value ev :dur))))
      0))

(defun mapcar-longest (function &rest lists)
  "Like `mapcar', but the resulting list is the length of the longest input list instead of the shortest. Indexes into shorter lists are wrapped. Additional return values from the last call are passed through as additional values from this function.

Example:

;; (mapcar-longest #'+ (list 1) (list 2 3 4))
;; => (3 4 5)

See also: `multi-channel-funcall'"
  (let (more-values)
    (apply #'values
           (loop
             :for i :from 0 :below (reduce #'max (mapcar #'length lists))
             :for res := (multiple-value-list
                          (apply function
                                 (mapcar
                                  (lambda (list)
                                    (elt-wrap list i))
                                  lists)))
             :collect (car res)
             :do (setf more-values (cdr res)))
           more-values)))

(defun multi-channel-funcall (function &rest args)
  "Call FUNCTION on the provided arguments. If one or more of the arguments is a list, funcall for each element of the list(s). The length of the resulting list will be the same as the longest input list.

Example:

;; (multi-channel-funcall #'+ 1 (list 1 2 3))
;; => (2 3 4)

See also: `mapcar-longest', `split-event-by-lists'"
  (if-let ((has-list (position-if #'listp args)))
    (apply #'mapcar-longest function (mapcar #'ensure-list args))
    (apply #'funcall function args)))

(defun plist-set (plist key value) ;; doesn't actually setf the place; only returns an altered plist.
  "Return a new copy of PLIST, but with its KEY set to VALUE. If VALUE is nil, return a copy without KEY."
  (if (null value)
      (remove-from-plist plist key)
      (if (getf plist key)
          (progn
            (setf (getf plist key) value)
            plist)
          (append plist (list key value)))))

(defun seq (&key start end limit step (default :mean)) ;; FIX: deprecate and move to mutility
  "Generate a sequence of numbers as a list.

START is the start of the range, END is the end. LIMIT is a hard limit on the number of results in the sequence. STEP is the interval between each number in the sequence.

When STEP is omitted and LIMIT is provided, the step is automatically calculated by dividing the range between LIMIT steps.

If LIMIT is 1, DEFAULT is used to find the value. DEFAULT can be :START, :END, :MEAN, or another value. :START and :END mean the returned value is the value of those arguments. :MEAN means the mean value of START and END is used. If another value is provided, it is used as the default instead.

See also: `seq-range'"
  (cond ((and limit step)
         (loop :for i :from start :upto end :by step :repeat limit
               :collect i))
        ((and limit (null step))
         (if (= 1 limit)
             (case default
               (:mean (/ (+ start end) 2))
               (:start start)
               (:end end)
               (t default))
             (loop :for i :from start :upto end :by (/ (- end start) (1- limit))
                   :collect i)))
        ((and step (null limit))
         (loop :for i :from start :upto end :by step
               :collect i))
        ((and (null step) (null limit))
         (loop :repeat (1+ (abs (- end start)))
               :with i := start
               :collect i
               :do (incf i (signum (- end start)))))))

(defun seq-range (num &optional stop step) ;; FIX: deprecate and move to mutility
  "Conveniently generate a sequence of numbers as a list. This function is based off Python's range() function, and thus has three ways of being called:

With one argument NUM, generate a range from 0 to (1- NUM):

;; (seq-range 4) ; => (0 1 2 3)

With two arguments NUM and STOP, generate a range from NUM to (1- STOP):

;; (seq-range 2 4) ; => (2 3)

With three arguments NUM, STOP, and STEP, generate a range from NUM to (1- STOP), each step increasing by STEP:

;; (seq-range 2 8 2) ; => (2 4 6)

See also: `seq'"
  (cond ((null stop)
         (seq :start 0 :end (1- num)))
        ((null step)
         (seq :start num :end (1- stop)))
        (t
         (seq :start num :end (1- stop) :step step))))

(defun pyramid (sequence &optional (pattern-type 1))
  "Return a new list whose elements have been reordered via one of 10 \"counting\" algorithms. This is based on and tested against SuperCollider's Array.pyramid method."
  (check-type sequence sequence)
  (check-type pattern-type (integer 1 10))
  (let ((seq-length (length sequence))
        (k 0))
    (case pattern-type
      (1
       (let ((res (make-list (/ (+ (* seq-length seq-length) seq-length) 2))))
         (dotimes (i (1+ seq-length) res)
           (loop :for j :below i :do
             (setf (elt res k) (elt sequence j))
             (incf k)))))
      (2
       (let ((res (make-list (/ (+ (* seq-length seq-length) seq-length) 2))))
         (dotimes (i seq-length res)
           (loop :for j :from (- seq-length 1 i) :below seq-length :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))))
      (3
       (let ((res (make-list (/ (+ (* seq-length seq-length) seq-length) 2))))
         (dotimes (i seq-length res)
           (loop :for j :below (- seq-length i) :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))))
      (4
       (let ((res (make-list (/ (+ (* seq-length seq-length) seq-length) 2))))
         (dotimes (i seq-length res)
           (loop :for j :from i :below seq-length :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))))
      (5
       (let ((res (make-list (* seq-length seq-length))))
         (dotimes (i seq-length)
           (loop :for j :below i :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))
         (dotimes (i seq-length res)
           (loop :for j :below (- seq-length i) :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))))
      (6
       (let ((res (make-list (* seq-length seq-length))))
         (dotimes (i seq-length)
           (loop :for j :from (- seq-length 1 i) :below seq-length :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))
         (dotimes (i seq-length res)
           (loop :for j :from (1+ i) :below seq-length :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))))
      (7
       (let ((res (make-list (+ (* seq-length seq-length) seq-length -1))))
         (dotimes (i seq-length)
           (loop :for j :below (- seq-length i) :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))
         (dotimes (i (1- seq-length) res)
           (loop :for j :upto (1+ i) :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))))
      (8
       (let ((res (make-list (+ (* seq-length seq-length) seq-length -1))))
         (dotimes (i seq-length)
           (loop :for j :from i :below seq-length :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))
         (dotimes (i (1- seq-length) res)
           (loop :for j :from (- seq-length 1 (1+ i)) :upto (1- seq-length) :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))))
      (9
       (let ((res (make-list (* seq-length seq-length))))
         (dotimes (i seq-length)
           (loop :for j :upto i :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))
         (dotimes (i seq-length res)
           (loop :for j :from (1+ i) :upto (1- seq-length) :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))))
      (10
       (let ((res (make-list (* seq-length seq-length))))
         (dotimes (i seq-length)
           (loop :for j :from (- seq-length 1 i) :upto (1- seq-length) :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k)))
         (dotimes (i seq-length res)
           (loop :for j :upto (- seq-length 2 i) :do
             (setf (elt res k) (elt-wrap sequence j))
             (incf k))))))))

;;; math stuff

(defun near (number &optional (range 1) (of 0))
  "Test whether NUMBER is within RANGE (bipolar) of OF.

Examples:

;; (near 4 1 5) ;; => t
;; (near 4 1) ;; => nil
;; (near 0.5) ;; => t
;; (near 0.5 0.6 1) ;; => t

See also: `alexandria:clamp', `wrap'"
  (<= (abs (- number of))
      range))

(defun next-beat-for-quant (&optional (quant 1) (beat (beat *clock*)) (direction 1))
  "Get the next valid beat for QUANT after BEAT. If DIRECTION is negative, finds the previous valid beat for QUANT.

See also: `quant'"
  (destructuring-bind (quant &optional (phase 0) (offset 0)) (ensure-list quant)
    (declare (ignore offset))
    (let ((direction (if (minusp direction) -1 1)))
      (labels ((find-next (quant phase cb try)
                 (let ((res (+ phase
                               (+ (* direction try)
                                  (funcall (if (minusp direction) #'floor-by #'ceiling-by) beat quant)))))
                   (if (funcall (if (plusp direction) #'>= #'<=) res cb)
                       res
                       (find-next quant phase cb (1+ try))))))
        (if (= 0 quant)
            beat
            (find-next quant phase beat 0))))))

;;; range stuff

(defun from-range (input map)
  "Unmap INPUT from the range specified by MAP to the range [0..1].

See also: `to-range', `rerange'"
  (destructuring-bind (&optional (min 0) (max 1) (warp :linear)) map
    (case warp
      ((:lin :linear 0) (/ (- input min) (- max min)))
      ((:exp :exponential 1) (/ (log (/ input min))
                                (log (/ max min))))
      ((:cos :cosine) (error "not done yet!"))
      (t (error "not done yet!")) ;; curve (other)
      )))

(defun to-range (input map)
  "Map INPUT from the range [0..1] to the range specified by MAP.

See also: `from-range', `rerange'"
  (destructuring-bind (&optional (min 0) (max 1) (warp :linear)) map
    (case warp
      ((:ste :step :stp) (if (zerop input) min max))
      ((:hol :hold :hld) (if (< input 1) min max))
      ((:lin :linear 0) (+ min (* input (- max min))))
      ((:exp :exponential 1) (* min (expt (/ max min) input)))
      (t (error "not done yet!")))))

(defun rerange (input from-range to-range)
  "Remap INPUT from one range, specified by FROM-RANGE, to another range, specified by TO-RANGE.

Example:

;; (rerange 64 (list 0 127) (list 0 1)) ;; map from [0..127] to [0..1]
;; => 0.503937

See also: `to-range', `from-range', `prerange'"
  (to-range (from-range input from-range) to-range))

;;; find-object-by-id

(defvar *dictionary-lookup-functions* (list 'find-pdef)
  "List of functions that can be used to look up the object that a symbol can name. Each function should return the object in question if it exists, or nil (or throw an error) if it doesn't.

Functions like `play', `end', `launch', and `stop' will check symbols against each of these dictionaries in order and will apply themselves to the object from the first dictionary with a matching key. 

Example:

;; *dictionary-lookup-functions*
;; => (CL-PATTERNS::FIND-PDEF BDEF:BDEF)
;; (play :foo) ;; will (play (pdef :foo)) if that pdef exists, or (play (bdef :foo)) if the bdef exists. If neither exists, it will throw an error.

See also: `play', `launch', `end', `stop'")

(defun find-object-by-id (id &key default)
  "Find an object identified by ID using `*dictionary-lookup-functions*'. Returns DEFAULT if no object was found. If DEFAULT is a symbol with name \"error\" then throw an error.

See also: `find-pdef'"
  (dolist (func *dictionary-lookup-functions* (if (string= 'error default)
                                                  (error "No object found with ID ~s" id)
                                                  default))
    (when-let ((res (ignore-errors (funcall func id))))
      (return-from find-object-by-id res))))

;;; generics

(defgeneric tempo (object)
  (:documentation "Get the tempo of OBJECT in beats per second. If OBJECT is a number, set the tempo of `*clock*' to that number."))

(defmethod tempo ((symbol symbol))
  (tempo (find-object-by-id symbol)))

(defgeneric beat (object)
  (:documentation "Get the beat that OBJECT occurs on, relative to its context's start. i.e. for an event, the beat is relative to the start of its source pattern, while for a pstream or clock object, the beat is the number of beats that have passed since its start."))

(defmethod beat ((null null))
  nil)

(defgeneric quant (object)
  (:documentation "The quant of OBJECT; a list representing when OBJECT is allowed to begin playing (`play-quant'), end playing (`end-quant'), or when a `pdef' is allowed to swap to its new definition (`end-quant'). `quant' will return the value of `play-quant', but sets both `play-quant' and `end-quant' when it is setf.

A quant value takes the form (divisor phase offset) where all provided elements are numbers. Only the first element is required.

- \"divisor\" is the divisor to quantize the clock to. The next time (mod (beats *clock*) divisor) is 0 is when OBJECT will start playing.
- \"phase\" is the number of beats to add on to the position determined by \"divisor\".
- \"offset\" is the number of seconds to add on to the position determined by \"divisor\" and \"phase\".

For example, a quant of (4) means it can start on any clock beat that is divisible by 4 (0, 4, 8, etc). A quant of (4 2) means the pstream can start 2 beats after any beat divisible by 4 (2, 6, 10, etc). And a quant of (4 0 1) means that the pstream can start 1 second after any beat that is divisible by 4.

See also: `play-quant', `end-quant', `next-beat-for-quant', `beat', `play'"))

(defmethod quant ((object t))
  (play-quant object))

(defmethod (setf quant) (value (object t))
  (let ((value (ensure-list value)))
    (setf (play-quant object) value
          (end-quant object) value)))

(defgeneric play-quant (object)
  (:documentation "The play-quant of OBJECT; a list representing when OBJECT is allowed to begin playing. Defaults to (1).

See `quant' for more information on quants and a description of acceptable values.

See also: `quant', `end-quant', `next-beat-for-quant', `beat', `play'"))

(defgeneric end-quant (object)
  (:documentation "The end-quant of OBJECT; a list representing when OBJECT is allowed to end playing or when a `pdef' is allowed to swap to a new definition if it has been redefined. Note that if `end-quant' is not set (the default), the pattern can only end or swap when the pattern itself ends (i.e. when it yields nil).

See `quant' for more information on quants and a description of acceptable values.

See also: `quant', `play-quant', `next-beat-for-quant', `beat', `end', `pdef'"))

(defgeneric rest-p (object)
  (:documentation "Whether or not something is a rest or a rest-representing object (i.e. :rest, :r, or a rest event)."))

(defmethod rest-p ((symbol symbol))
  (and (find symbol (list :rest :r 'rest 'r))
       t))

(defmethod rest-p ((this t))
  nil)

(defgeneric play (object)
  (:documentation "Play an object (typically an event or pattern).

See also: `launch', `stop'"))

(defmethod play ((symbol symbol))
  (when-let ((res (find-object-by-id symbol)))
    (play res)))

(defmethod play ((list list))
  (mapcar #'play list))

(defgeneric launch (object)
  (:documentation "Play a new copy of OBJECT on the clock. Unlike `play', calling this method on a `pdef' will always start a new copy of its pattern instead of the pdef itself.

See also: `play'"))

(defmethod launch ((object t)) ;; forward to `play' if a more specific method hasn't been defined for a class.
  (play object))

(defmethod launch ((symbol symbol))
  (when-let ((res (find-object-by-id symbol)))
    (launch res)))

(defmethod launch ((list list))
  (mapcar #'launch list))

(defgeneric stop (object)
  (:documentation "Immediately stop a playing object (typically a pattern, pdef, task, or node). If OBJECT is T, stop all playing patterns and nodes.

See also: `end', `play'"))

(defmethod stop ((object (eql t))) ;; (stop t) to stop all playing pdefs and nodes.
  (stop (clock-tasks))
  (dolist (backend (enabled-backends))
    (backend-panic backend)))

(defmethod stop ((symbol symbol))
  (when-let ((res (find-object-by-id symbol)))
    (stop res)))

(defmethod stop ((list list))
  (mapcar #'stop list))

(defmethod stop ((null null))
  nil)

(defgeneric end (object)
  (:documentation "End a task; it will stop when its current loop completes."))

(defmethod end ((object t)) ;; forward to `stop' if a more specific method hasn't been defined for a class.
  (stop object))

(defmethod end ((symbol symbol))
  (when-let ((res (find-object-by-id symbol)))
    (end res)))

(defmethod end ((list list))
  (mapcar #'end list))

(defgeneric playing-p (object &optional clock)
  (:documentation "Whether OBJECT is playing.

See also: `play-or-stop', `play-or-end', `playing-pdefs', `playing-nodes'"))

(defmethod playing-p ((symbol symbol) &optional (clock *clock*))
  (when-let ((res (find-object-by-id symbol)))
    (playing-p res clock)))

(defmethod playing-p ((list list) &optional (clock *clock*))
  (mapcar (fn _ (playing-p _ clock)) list))

(defgeneric loop-p (object)
  (:documentation "Whether or not OBJECT should play again after it ends."))

(defgeneric ended-p (pstream)
  (:documentation "Returns t if PSTREAM has no more outputs, or nil if outputs remain to be yielded.

Example:

;; (defparameter *pstr* (as-pstream (pseq '(1 2) 1)))
;; (next *pstr*) ;=> 1
;; (ended-p *pstr*) ;=> NIL
;; (next *pstr*) ;=> 2
;; (ended-p *pstr*) ;=> NIL
;; (next *pstr*) ;=> NIL
;; (ended-p *pstr*) ;=> T

See also: `eop-p', `last-output'"))

(defun play-or-stop (object)
  "`play' an object, or `stop' it if it is already playing. Returns the task if the object will start playing, or NIL if it will stop."
  (if (playing-p object)
      (progn
        (stop object)
        nil)
      (play object)))

(defun play-or-end (object)
  "`play' an object, or `end' it if it's already playing. Returns the task if the object will start playing, or NIL if it will end."
  (if (playing-p object)
      (progn
        (end object)
        nil)
      (play object)))

(defun all-instruments (&optional backend)
  "Get a list of the names of all instruments defined for BACKEND, or all enabled backends if none specified.

See also: `playing-nodes', `all-patterns', `all-pdefs', `enabled-backends'"
  (loop :for backend :in (or (ensure-list backend) (enabled-backends))
        :append (backend-all-instruments backend)))

(defun playing-nodes (&optional backend)
  "Get a list of all nodes on BACKEND that are currently playing. Without BACKEND, get all playing nodes on all backends.

See also: `all-instruments', `playing-pdefs', `playing-p'"
  (if backend
      (backend-all-nodes backend)
      (apply #'append (mapcar #'playing-nodes (enabled-backends)))))

;;; render

(defgeneric render (object output &key tempo max-pattern-yield-length max-output-duration &allow-other-keys)
  (:documentation "Render a pattern or other object as audio or other format. OUTPUT is what the pattern should be rendered as. It accepts the following values:

- A string - Output file name (file format is determined by the file extension).
- :buffer - Render to a buffer in the relevant backend (determined by parameters of OBJECT, i.e. instrument or backend keys of events).
- :bdef - Render to a buffer handled by `bdef:bdef' if the bdef library is loaded. Falls back to :buffer if bdef is not loaded.
- :file - Render to a file in the `*cl-patterns-temporary-directory*'.
- :score - Render as a SuperCollider score in memory. Only works if the cl-patterns/supercollider/score system is loaded. Can also be rendered to a file if a .osc filename is provided and :supercollider is provided for BACKEND.
- :pstream - Make a pstream from the pattern and grab outputs to it. Effectively defers to `next-upto-n'.
- :eseq - Make an `eseq' from the pattern. Effectively defers to `as-eseq'.
- Any backend name - Render as a buffer in that backend.

The following additional keyword arguments are also supported, depending on the output type:

- BACKEND - The name of the backend to use to render. Defaults to the first enabled backend.
- TEMPO - The tempo of the result in beats per second. Defaults to `*clock*''s current tempo.
- MAX-PATTERN-YIELD-LENGTH - Maximum number of outputs to grab from the source pattern. Must be an integer (cannot be :inf). See also: `*max-pattern-yield-length*'.
- MAX-OUTPUT-DURATION - The maximum duration of the output in seconds. Defaults to infinite, in which case the pattern is limited by MAX-PATTERN-YIELD-LENGTH.

See also: `as-eseq'"))

(defmethod render (object (output (eql :pstream)) &key max-pattern-yield-length)
  (next-upto-n object (or max-pattern-yield-length *max-pattern-yield-length*)))

(defmethod render (object (output pathname) &rest args &key &allow-other-keys)
  (apply #'render object (namestring output) args))

(defun find-backend-supporting-render (render-type)
  "Get the output and backend names of the first enabled backend that supports RENDER-TYPE (i.e. :buffer, :file, :score, etc), or nil if none support it.

See also: `render'"
  (let ((backends (enabled-backends)))
    (dolist (backend backends)
      (let ((sym (my-intern (concat backend "-" render-type) :keyword)))
        (when (find-method #'render nil (list t (list 'eql sym)) nil)
          (return-from find-backend-supporting-render (values sym backend)))))))

(defmacro make-default-render-method (type)
  "Generate a default `render' method."
  `(defmethod render (object (output (eql ,type)) &rest args &key &allow-other-keys)
     (let ((backend (getf args :backend)))
       (if backend
           (apply #'render object output args)
           (if-let ((backend (find-backend-supporting-render ,type)))
             (apply #'render object backend args)
             (error "No enabled backend supports rendering as ~s." ,type))))))

(defmacro make-default-render-methods ()
  "Generate the default `render' methods for :buffer, :file, :score, etc."
  `(progn
     ,@(loop :for type :in (list :buffer :file :score)
             :collect `(make-default-render-method ,type))))

(make-default-render-methods)

;;; macros / MOP stuff

(define-method-combination pattern ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  "Method combination type for patterns; specifically, the `next' function. Similar to the standard CLOS method combination, except that :around methods are called in reverse order, from the least specific to the most specific."
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary)))))
      (if around
          (let ((around (reverse around)))
            `(call-method ,(first around)
                          (,@(rest around)
                           (make-method ,form))))
          form))))

;; conditionally load swank extensions if swank is available
;; using conditional compilation with #+swank fails if cl-patterns is compiled with swank and then loaded without -- see issue #7.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (featurep :swank)
    (load (asdf:system-relative-pathname :cl-patterns "src/extensions/swank.lisp"))))

;; conditionally load slynk extensions if slynk is available
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (featurep :slynk)
    (load (asdf:system-relative-pathname :cl-patterns "src/extensions/slynk.lisp"))))
