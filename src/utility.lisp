(in-package :cl-patterns)

;;; special variables

(defparameter *event* nil
  "The event special variable. Can be referenced inside a pattern's code.")

(defvar *clock* nil
  "The default clock to run tasks on.")

;;; scale/tuning/chord structs
;; (they're here instead of in scales.lisp to avoid compiler warnings)

(defstruct scale name notes tuning)

(defstruct tuning name tuning octave-ratio)

(defstruct chord name scale indexes)

;;; glue

(defun gete (list key)
  "Get a list of the value of KEY for each event in LIST."
  (mapcar (lambda (event)
            (unless (null event)
              (event-value event key)))
          list))

(defun string-keyword (string)
  "Converts a string to a decent-looking keyword."
  (alexandria:make-keyword (string-upcase (remove-if-not (lambda (letter)
                                                           (or (digit-char-p letter)
                                                               (alpha-char-p letter)))
                                                         string))))

;;; list stuff

(defun nth-wrap (n list)
  "Return the Nth value of LIST, wrapping around if the value is bigger or smaller than the list length."
  (nth (mod n (length list)) list))

(defun normalized-sum (list)
  "Return a copy of LIST normalized so all of its numbers summed together equal 1."
  (mapcar (lambda (x) (/ x (reduce #'+ list))) list))

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
  (apply #'append (mapcar #'alexandria:ensure-list list)))

(defun most-x (list predicate key) ;; from https://stackoverflow.com/questions/30273802/how-would-i-get-the-min-max-of-a-list-using-a-key
  "Get the most PREDICATE item in LIST by comparing the value returned by KEY."
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
      (alexandria:remove-from-plist plist key)
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
  (alexandria:hash-table-keys item))

;;; math stuff

(defun sign (number)
  "Get an integer representing the sign of a number."
  (cond ((plusp number) 1)
        ((minusp number) -1)
        ((zerop number) 0)))

(defun wrap (number bottom top)
  "Wraps a number between BOTTOM and TOP, similar to `cl:mod'."
  (+ (mod (- number bottom) (- top bottom)) bottom))

(defun round-up (number &optional (divisor 1))
  "Round NUMBER up to the next multiple of DIVISOR if it isn't already a multiple of it."
  (if (= 0 (mod number divisor))
      number
      (let ((diff (multiple-value-bind (res div) (floor number divisor) (declare (ignore res)) div)))
        (+ number (- divisor diff)))))

(defun random-range (low &optional high)
  "Return a random number between LOW and HIGH, inclusive. If HIGH is not provided, act the same as (random LOW).

See also: `exponential-random-range'"
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

See also: `random-range'"
  (* low
     (exp (* (log (/ high
                     low))
             (random 1.0)))))

(defun seq (&key start end limit step)
  "Generate a sequence of numbers as a list.

START is the start of the range, END is the end. LIMIT is a hard limit on the number of results in the sequence. STEP is the interval between each number in the sequence.

When STEP is omitted and LIMIT is provided, the step is automatically calculated by dividing the range between LIMIT steps.

See also: `seq-range'"
  (cond ((and limit step)
         (loop :for i :from start :upto end :by step :repeat limit
            :collect i))
        ((and limit (null step))
         (loop :for i :from start :upto end :by (/ (- end start) (1- limit))
            :collect i))
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

;;; macros / MOP stuff

(defmacro create-global-dictionary (name) ;; FIX: remove/refactor this?
  (let* ((name-name (symbol-name name))
         (dict-symbol (intern (string-upcase (concatenate 'string "*" name-name "-dictionary*")))))
    `(progn
       (defparameter ,dict-symbol (list)
         ,(concatenate 'string "The global " name-name " dictionary."))
       (defun ,(intern (string-upcase (concatenate 'string name-name "-ref"))) (key &optional value)
         ,(concatenate 'string "Retrieve a value from the global " name-name " dictionary, or set it if VALUE is provided.")
         (let ((key (alexandria:make-keyword key)))
           (if (null value)
               (getf ,dict-symbol key)
               (setf (getf ,dict-symbol key) value)))))))

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
