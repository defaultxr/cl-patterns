(in-package :cl-patterns)

;;; special variables

(defparameter *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defvar *clock* nil
  "The default clock to run tasks on.")

;;; glue

(defun gete (list key)
  "Get a list of the value of KEY for each event in LIST."
  (mapcar (lambda (event)
            (unless (null event)
              (event-value event key)))
          list))

(defun string-keyword (string)
  "Return STRING as a keyword, with all non-alphanumeric characters removed."
  (make-keyword (string-upcase (remove-if-not (lambda (letter)
                                                (or (digit-char-p letter)
                                                    (alpha-char-p letter)))
                                              string))))

;;; list stuff

(defun elt-wrap (list n)
  "Return the Nth value of LIST, wrapping around if the value is bigger or smaller than the list length."
  (elt list (mod n (length list))))

(defun normalized-sum (list)
  "Return a copy of LIST normalized so all of its numbers summed together equal 1."
  (mapcar (lambda (x) (/ x (apply #'+ list))) list))

(defun cumulative-list (list)
  "Return a copy of LIST where the elements previous are added to the current one.

Example: (cumulative-list (list 1 2 3 4)) => (1 3 6 10)"
  (loop :for element :in list
     :for index :from 0
     :collect (apply #'+ element (subseq list 0 index))))

(defun index-of-greater-than (n list)
  "Get the index of the first element of LIST greater than N."
  (position-if (lambda (num) (> num n)) list))

(defun flatten-1 (list)
  "Like `alexandria:flatten', but only flattens one layer."
  (apply #'append (mapcar #'ensure-list list)))

(defun most-x (list predicate key) ;; from https://stackoverflow.com/questions/30273802/how-would-i-get-the-min-max-of-a-list-using-a-key
  "Get the most PREDICATE item in LIST by comparing whether PREDICATE is true for the values returned by KEY applied to each element of LIST.

Example:

;; get the smallest item in the list:
;; (most-x (list 1 2 3) '< 'identity)"
  (when list
    (let* ((m0 (first list))
           (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
              (when (funcall predicate e1 m1)
                (psetf m0 e0 m1 e1)))
            list)
      m0)))

(defun plist-set (plist key value) ;; doesn't actually setf the place; only returns an altered plist.
  "Return a new copy of PLIST, but with its KEY set to VALUE. If VALUE is nil, return a copy without KEY."
  (if (null value)
      (remove-from-plist plist key)
      (if (getf plist key)
          (progn
            (setf (getf plist key) value)
            plist)
          (append plist (list key value)))))

(defgeneric keys (item)
  (:documentation "Get the keys of ITEM, whether it be a plist, event, etc."))

(defmethod keys ((item null))
  nil)

(defmethod keys ((item cons))
  (labels ((accum (list)
             (cons (car list)
                   (when (cddr list)
                     (accum (cddr list))))))
    (accum item)))

(defmethod keys ((item hash-table))
  (hash-table-keys item))

;;; math stuff

(defun sign (number)
  "Get an integer representing the sign of a number."
  (cond ((plusp number) 1)
        ((minusp number) -1)
        ((zerop number) 0)))

(defun wrap (number bottom top)
  "Wraps a number between BOTTOM and TOP, similar to `cl:mod'."
  (+ (mod (- number bottom) (- top bottom)) bottom))

(defun round-by (number &optional (by 1))
  "Round NUMBER by BY."
  (* (round (/ number by)) by))

(defun round-by-direction (number &optional (by 1))
  "Round NUMBER by BY. With positive BY, round up; with negative, round down."
  (if (= 0 (mod number by))
      number
      (let* ((positive (plusp by))
             (diff (cadr (multiple-value-list (funcall (if positive #'floor #'ceiling) number (abs by))))))
        (funcall (if positive #'+ #'-) number (funcall (if positive #'- #'+) (abs by) diff)))))

(defun random-coin (&optional (probability 0.5))
  "Randomly return true with a probability of PROBABILITY/1."
  (<= (random 1.0) probability))

(defun random-range (low &optional high)
  "Return a random number between LOW and HIGH, inclusive. If HIGH is not provided, act the same as (random LOW).

See also: `exponential-random-range', `gauss'"
  (if high
      (let ((rval (- high low)))
        (+ low
           (random (if (integerp rval)
                       (1+ rval)
                       rval))))
      (random low)))

(defun random-range.new (low &optional high) ;; version 2, with support for ratios - FIX
  "Return a random number between LOW and HIGH, inclusive. If HIGH is not provided, act the same as (random LOW)."
  (flet ((rnd (number)
           (if (typep number 'ratio)
               (/ (random (1+ (numerator number))) (denominator number))
               (random number))))
    (if high
        (let ((rval (- high low)))
          (+ low
             (rnd (if (integerp rval)
                      (1+ rval)
                      rval))))
        (rnd low))))

(defun exponential-random-range (low high) ;; adapted from supercollider/include/plugin_interface/SC_RGen.h
  "Generate a random number between LOW and HIGH, with exponential distribution.

See also: `random-range', `gauss'"
  (* low
     (exp (* (log (/ high
                     low))
             (random 1d0)))))

(defun gauss (mean standard-deviation)
  "Generate a random number from a normal (Gaussian) distribution.

See also: `random-range', `exponential-random-range'"
  (+ (* (sqrt (* -2 (log (random 1.0))))
        (sin (random (* 2 pi)))
        standard-deviation)
     mean))

(defun seq (&key start end limit step (default :mean))
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
            :with i = start
            :collect i
            :do (incf i (sign (- end start)))))))

(defun seq-range (num &optional stop step)
  "Conveniently generate a sequence of numbers as a list. This function is based off Python's range() function, and thus has three ways of being called:

With one argument NUM, generate a range from 0 to (1- NUM):

;; (seq-range 4) ;=> (0 1 2 3)

With two arguments NUM and STOP, generate a range from NUM to (1- STOP):

;; (seq-range 2 4) ;=> (2 3)

With three arguments NUM, STOP, and STEP, generate a range from NUM to (1- STOP), each step increasing by STEP:

;; (seq-range 2 8 2) ;=> (2 4 6)

See also: `seq'"
  (cond ((null stop)
         (seq :start 0 :end (1- num)))
        ((null step)
         (seq :start num :end (1- stop)))
        (t
         (seq :start num :end (1- stop) :step step))))

(defun next-beat-for-quant (quant beat &optional (direction 1))
  "Get the next valid beat for QUANT after BEAT. If DIRECTION is negative, finds the previous valid beat for QUANT."
  (destructuring-bind (quant &optional (phase 0) (offset 0)) (ensure-list quant)
    (declare (ignore offset))
    (let ((sign (sign direction)))
      (labels ((find-next (quant phase cb try)
                 (let ((res (+ phase
                               (+ (* sign try)
                                  (round-by-direction beat (* sign quant))))))
                   (if (funcall (if (plusp direction) #'>= #'<=) res cb)
                       res
                       (find-next quant phase cb (1+ try))))))
        (if (= 0 quant)
            beat
            (find-next quant phase beat 0))))))

;;; generics

(defgeneric tempo (object)
  (:documentation "Get the tempo of OBJECT in beats per second. If OBJECT is a number, set the tempo of `*clock*' to that number."))

(defgeneric beat (object)
  (:documentation "Get the beat that OBJECT occurs on, relative to its context's start. i.e. for an event, the beat is relative to the start of its source pattern, while for a pstream or clock object, the beat is the number of beats that have passed since its start."))

(defmethod beat ((null null))
  nil)

(defgeneric quant (object)
  (:documentation "Get the quant of OBJECT; a list representing when OBJECT is allowed to begin playing.

A quant takes the form (divisor phase offset) where all provided elements are numbers. Only the first element is required.

- \"divisor\" is the divisor to quantize the clock to. The next time (mod (beats *clock*) divisor) is 0 is when OBJECT will start playing.
- \"phase\" is the number of beats to add on to the position determined by \"divisor\".
- \"offset\" is the number of seconds to add on to the position determined by \"divisor\" and \"phase\".

See also: `next-beat-for-quant', `beat', `play'"))

(defgeneric play (item)
  (:documentation "Play an item (typically an event or pattern) according to the current `*event-output-function*'.

See also: `launch', `stop'"))

(defgeneric launch (item)
  (:documentation "Play a new copy of ITEM on the clock. Unlike `play', calling this method on a `pdef' will always start a new copy of its pattern instead of the pdef itself.

See also: `play'"))

(defgeneric stop (item)
  (:documentation "Immediately stop a playing item (typically a playing task or pdef).

See also: `end', `play'"))

(defgeneric end (item)
  (:documentation "End a task; it will stop when its current loop completes."))

(defgeneric playing-p (item &optional clock)
  (:documentation "Whether ITEM is playing.

See also: `play-or-stop', `play-or-end', `playing-pdefs'"))

(defgeneric loop-p (item)
  (:documentation "Whether or not ITEM should play again after it ends."))

(defun play-or-stop (item)
  "`play' an item, or `stop' it if it is already playing. Returns the task if the item will start playing, or NIL if it will stop."
  (if (playing-p item)
      (progn
        (stop item)
        nil)
      (play item)))

(defun play-or-end (item)
  "`play' an item, or `end' it if it's already playing. Returns the task if the item will start playing, or NIL if it will end."
  (if (playing-p item)
      (progn
        (end item)
        nil)
      (play item)))

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
  "Unmap INPUT from FROM-RANGE and re-map it to the range TO-RANGE.

See also: `to-range', `from-range'"
  (to-range (from-range input from-range) to-range))

;;; MIDI stuff

(defun midi-truncate-clamp (number &optional (max 127))
  "Truncate NUMBER and clamp it to the range 0..MAX (default 127)."
  (declare (number number))
  (clamp (truncate number) 0 max))

(defun bipolar-1-to-midi (number)
  "Convert the range -1..1 to 0..127."
  (clamp (ceiling (* 63.5 (1+ number))) 0 127))

(defun unipolar-1-to-midi (number)
  "Convert the range 0..1 to 0..127."
  (clamp (round (* 127 number)) 0 127))

(defun frequency-to-midi (frequency)
  "Convert FREQUENCY to a MIDI note number (rounding to ensure it's an integer).

Note that this function is meant for use with the MIDI backend; for frequency-to-midinote conversion without rounding, see `freq-midinote' instead."
  (round (freq-midinote frequency)))

;;; macros / MOP stuff

(defmacro create-global-dictionary (name) ;; FIX: remove/refactor this?
  (let* ((name-name (symbol-name name))
         (dict-symbol (intern (string-upcase (concatenate 'string "*" name-name "-dictionary*")))))
    `(progn
       (defvar ,dict-symbol (make-hash-table)
         ,(concatenate 'string "The global " name-name " dictionary."))
       (defun ,(intern (string-upcase (concatenate 'string name-name "-ref"))) (key &optional (value nil value-provided-p))
         ,(concatenate 'string "Retrieve a value from the global " name-name " dictionary, or set it if VALUE is provided.")
         (let ((key (make-keyword key)))
           (if value-provided-p
               (if (null value)
                   (remhash key ,dict-symbol)
                   (setf (gethash key ,dict-symbol) value))
               (gethash key ,dict-symbol)))))))

(define-method-combination pattern () ;; same as standard, but :around methods are called in reverse order, from least to most specific.
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
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
