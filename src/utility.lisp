;;;; utility.lisp - various utility functions that are either fundamental to other functionality, or just don't belong anywhere else.

(in-package #:cl-patterns)

;;; special variables

(defvar *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defvar *clock* nil
  "The default clock to run tasks on.")

(defparameter eop 'eop
  "End Of Pattern; the symbol yielded by patterns after their last output.")

;;; symbol stuff

(defun pattern-pstream-class-name (pattern)
  "Get the name of the pstream class for PATTERN."
  (let ((pattern-name (etypecase pattern
                        (class (class-name pattern))
                        (standard-object (class-name (class-of pattern)))
                        (null (error "~S is not a pattern class name" pattern))
                        (symbol pattern))))
    (intern (concat (symbol-name pattern-name) "-PSTREAM")
            (symbol-package pattern))))

;;; string stuff

(defun string-replace (string old new)
  "Find the first instance of OLD in STRING and replace it with NEW. Return the new string, or if OLD was not found, return STRING unchanged. Returns the position that OLD was found as a second value, or nil if it was not found."
  (let ((pos (search old string)))
    (values
     (if pos
         (concatenate 'string (subseq string 0 pos) new (subseq string (+ pos (length old))))
         string)
     pos)))

(defun note-name-and-octave (note)
  "Given a note name, return a list consisting of its note number and its octave (defaulting to 4 if it's not specified).

Examples:

;; (note-name-and-octave :c4) ;=> (:C 4)
;; (note-name-and-octave :a#7) ;=> (:A# 7)
;; (note-name-and-octave :c-1) ;=> (:C -1)
;; (note-name-and-octave :d) ;=> (:D 4)

See also: `note-midinote', `chromatic-index-note'"
  (let* ((str (string note))
         (note (remove-if-not (lambda (i)
                                (or (alpha-char-p i)
                                    (char= i #\#)))
                              str))
         (octave (remove-if-not (fn (or (char= #\- _)
                                        (digit-char-p _)))
                                str)))
    (list (if (emptyp note) :c (make-keyword (string-upcase note)))
          (if (emptyp octave) 4 (parse-integer octave)))))

;;; list stuff

(defun normalized-sum (list &optional (sum 1))
  "Get a copy of LIST \"normalized\" so all of its numbers summed together equal SUM.

Examples:

;; (normalized-sum (list 1 1 1 1)) ;=> (1/4 1/4 1/4 1/4)
;; (normalized-sum (list 1 2 3) 2) ;=> (1/3 2/3 1)

See also: `cumulative-list'"
  (let ((div-by (/ (apply #'+ list) sum)))
    (mapcar (fn (/ _ div-by)) list)))

(defun cumulative-list (list)
  "Get a fresh version of LIST where each element is the sum of it and the previous elements.

Example:

;; (cumulative-list (list 1 2 3 4)) ;=> (1 3 6 10)

See also: `normalized-sum'"
  (let ((cur 0))
    (mapcar (fn (incf cur _)) list)))

(defun index-of-greater-than (n list)
  "Get the index of the first element of LIST greater than N."
  (position-if (lambda (num) (> num n)) list))

(defgeneric last-dur (object)
  (:documentation "Get the beat position of the ending of the last event in the ESEQ."))

(defmethod last-dur ((list list))
  (if list
      (reduce #'max list
              :key (fn (+ (beat _)
                          (event-value _ :dur))))
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

(defun plist-set (plist key value)
  "Return a new copy of PLIST, but with its KEY set to VALUE."
  (if (getf plist key)
      (progn
        (setf (getf plist key) value)
        plist)
      (append plist (list key value))))

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

(defun find-buffer-symbol (sequence)
  "Get the name and index of the symbol in SEQUENCE that represents the buffer argument. This is typically either :buffer or :bufnum."
  (loop :for idx :from 0
        :for i :being :the :elements :of sequence
        :if (member i '(:buffer :bufnum) :test #'string-equal)
          :do (return-from find-buffer-symbol (values i idx))))

;;; math stuff

(defun near-p (number &optional (range 1) (of 0))
  "Test whether NUMBER is within RANGE (bipolar) of OF.

Examples:

;; (near-p 4 1 5) ; => t
;; (near-p 4 1) ; => nil
;; (near-p 0.5) ; => t
;; (near-p 0.5 0.6 1) ; => t

See also: `alexandria:clamp', `wrap', `nearest'"
  (<= (abs (- number of))
      range))

(defun nearest (input list)
  "Get the element in LIST nearest to INPUT.

See also: `near-p'"
  (reduce (lambda (a b)
            (if (> (abs (- input a))
                   (abs (- input b)))
                b
                a))
          list))

(defun transpose (freq &optional (semitones 0) (octaves 0))
  "Transpose FREQ the specified number of SEMITONES and OCTAVES."
  (let ((semi (+ semitones (* octaves 12.0d0))))
    (* freq (expt 2 (/ semi 12.0d0)))))

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
      (t (error "not done yet!")) ; curve (other)
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

;; (rerange 64 (list 0 127) (list 0 1)) ; map from [0..127] to [0..1]
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
;; (play :foo) ; will (play (pdef :foo)) if that pdef exists, or (play (bdef :foo)) if the bdef exists. If neither exists, it will throw an error.

See also: `play', `launch', `end', `stop'")

(defun find-object-by-id (id &key default)
  "Find an object identified by ID using `*dictionary-lookup-functions*'. Returns DEFAULT if no object was found, or signals an error if DEFAULT is the symbol :error.

See also: `find-pdef'"
  (dolist (func *dictionary-lookup-functions* (if (eql :error default)
                                                  (error "No object found with ID ~S" id)
                                                  default))
    (when-let ((res (ignore-errors (funcall func id))))
      (return-from find-object-by-id res))))

;;; generics

(defgeneric tempo (object)
  (:documentation "Get the tempo of OBJECT in beats per second. If OBJECT is a number, set the tempo of `*clock*' to that number.

See also: `beat'"))

(defmethod tempo ((symbol symbol))
  (tempo (find-object-by-id symbol)))

(defgeneric beat (object)
  (:documentation "Get the beat that OBJECT occurs on, relative to its context's start. For example, for an `event', the beat is relative to the start of its source pattern, while for a `pstream' or clock object, the beat is the number of beats that have passed since its start.

See also: `tempo'"))

(defmethod beat ((null null))
  nil)

(defmethod beat ((eop (eql eop)))
  eop)

(defgeneric quant (object)
  (:documentation "The quant of OBJECT; a list representing when OBJECT is allowed to begin playing (`play-quant'), end playing (`end-quant'), or when a `pdef' is allowed to swap to its new definition (`end-quant'). `quant' will return the value of `play-quant' as its first value and `end-quant' as its second, and sets both `play-quant' and `end-quant' when it is setf.

A quant value takes the form (divisor phase offset) where all provided elements are numbers. Only the first element is required.

- \"divisor\" is the divisor to quantize the clock to. The next time (mod (beat *clock*) divisor) is 0 is when OBJECT will start playing.
- \"phase\" is the number of beats to add on to the position determined by \"divisor\".
- \"offset\" is the number of seconds to add on to the position determined by \"divisor\" and \"phase\".

For example, a quant of (4) means it can start on any clock beat that is divisible by 4 (0, 4, 8, etc). A quant of (4 2) means the pstream can start 2 beats after any beat divisible by 4 (2, 6, 10, etc). And a quant of (4 0 1) means that the pstream can start 1 second after any beat that is divisible by 4.

See also: `play-quant', `end-quant', `next-beat-for-quant', `beat', `play'"))

(defmethod quant ((object t))
  (values (play-quant object)
          (end-quant object)))

(defmethod (setf quant) (value (object t))
  (let ((value (ensure-list value)))
    (setf (play-quant object) value
          (end-quant object) value)))

(defgeneric play-quant (object)
  (:documentation "The play-quant of OBJECT; a list representing when OBJECT is allowed to begin playing. Defaults to (1).

See `quant' for more information on quants and a description of acceptable values.

See also: `quant', `end-quant', `next-beat-for-quant', `beat', `play'"))

(defgeneric end-quant (object)
  (:documentation "The end-quant of OBJECT; a list representing when OBJECT is allowed to end playing or when a `pdef' is allowed to swap to a new definition if it has been redefined. Note that if `end-quant' is not set (the default), the pattern can only end or swap when the pattern itself ends (i.e. when it yields `eop').

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

(defmethod launch ((object t)) ; forward to `play' if a more specific method hasn't been defined for a class.
  (play object))

(defmethod launch ((symbol symbol))
  (when-let ((res (find-object-by-id symbol)))
    (launch res)))

(defmethod launch ((list list))
  (mapcar #'launch list))

(defgeneric stop (object)
  (:documentation "Immediately stop a playing object (typically a pattern, pdef, task, or node). If OBJECT is T, stop all playing patterns and nodes.

See also: `end', `play'"))

(defmethod stop ((object (eql t))) ; (stop t) to stop all playing pdefs and nodes.
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

(defmethod end ((object t)) ; forward to `stop' if a more specific method hasn't been defined for a class.
  (stop object))

(defmethod end ((symbol symbol))
  (when-let ((res (find-object-by-id symbol)))
    (end res)))

(defmethod end ((list list))
  (mapcar #'end list))

(defgeneric eop-p (object)
  (:documentation "True if OBJECT indicates the end of a pstream's outputs.

See also: `ended-p'"))

(defmethod eop-p ((object t))
  nil)

(defmethod eop-p ((symbol symbol))
  (eql eop symbol))

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

(defun play-solo (object &key (stop-type 'end) (clock *clock*))
  "End or stop all tasks on CLOCK and play OBJECT (or simply let it continue playing if it's already playing).

See also: `play', `end', `stop', `play-swap'"
  (let ((object-name (etypecase object
                       (string-designator object)
                       (pdef (pdef-name object))))
        (stop-type (switch (stop-type :test 'string=)
                     ('end 'end)
                     ('stop 'stop))))
    (mapc stop-type (remove-if (fn (eql (pdef-name (task-item _))
                                        object-name))
                               (clock-tasks clock)))
    (unless (playing-p object)
      (play object))))

(defun play-swap (play end &key (stop-type 'end) (clock *clock*))
  "Play PLAY, and end (or stop) END.

See also: `play', `end', `stop', `play-solo'"
  (let ((stop-type (switch (stop-type :test 'string=)
                     ('end 'end)
                     ('stop 'stop)))
        (*clock* clock))
    (play play)
    (funcall stop-type end)))

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
