(in-package :cl-patterns)

;; NOTES: use alexandria:clamp instead of clip

;; glue

(defun gete (list key)
  "Get a list of the value of KEY for each element in LIST."
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

;; list stuff

(defun nth-wrap (n list)
  "Return the Nth value of LIST, wrapping around if the value is bigger or smaller than the list length.x"
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

(defgeneric keys (item)
  (:documentation "Get the keys of ITEM, whether it be a plist, event, etc."))

(defmethod keys ((item cons))
  (labels ((accum (list)
             (cons (car list)
                   (when (cddr list)
                     (accum (cddr list))))))
    (accum item)))

(defmethod keys ((item null))
  nil)

;; math stuff

(defun wrap (number bottom top)
  "Wraps a number between BOTTOM and TOP, similar to `mod'."
  (+ (mod (- number bottom) (- top bottom)) bottom))

(defun round-up (number &optional (tolerance 1))
  "Round NUMBER up to the next multiple of TOLERANCE if it isn't already a multiple of it."
  (if (= 0 (mod number tolerance))
      number
      (+ number (* (if (plusp number) 1 -1) (rem number tolerance)))))

(defun random-range (low &optional high)
  "Return a random number between LOW and HIGH, inclusive. If HIGH is not provided, act the same as (random LOW)."
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

(defun exponential-random (lo hi)
  "Generate a random number between LO and HI, with exponential distribution."
  ;; taken from supercollider/include/plugin_interface/SC_RGen.h
  (* lo
     (exp (* (log (/ hi
                     lo))
             (random 1.0)))))

;; macros / MOP stuff

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

