;;;; cycles.lisp - divide the time of each "cycle" evenly among elements of an input list, a la TidalCycles.

;;; TODO:
;; FIX: remove this and write it as regular functions instead?
;; FIX: ensure that events in MAP are "executed"; i.e. :map (list 'foo (event :value (lambda () (random 200)))) should replace 'foo in the input list with events whose :value are random numbers between 0 and 199.

(in-package #:cl-patterns)

(defpattern pcycles (pattern)
  (list
   (map :initform nil)
   (key :initform :value)
   (dur :initform 1)
   (repeats :initform *default-pattern-repeats*)
   (parsed-list :state t)
   (current-repeats-remaining :state t))
  :documentation "Divide DUR between the elements in LIST. Child lists are recursively divided by the dur of their step. LIST can also be a string, in which case it is split into a list, each character converted into an integer or symbol.

pcycles also supports the following keyword arguments:

- MAP - Plist mapping symbols to the event to yield for that symbol. The symbols - (dash), _ (underscore), and . (period) are mapped to rest events by default.
- KEY - The event key that should hold the value of that step. Can be convenient if you're :embed-ing pcycles into another pattern. Defaults to :value.
- DUR - The total duration in beats of one cycle of LIST.
- REPEATS - The number of times that LIST should be repeated before the pattern ends.

Examples:

;; (next-upto-n (pcycles '(1 - 2 - ) :repeats 1)) ; 4 items in LIST, so each output will be 1/4 beats long.
;; ;=> ((EVENT :VALUE 1 :DUR 1/4)
;; ;    (EVENT :TYPE :REST :DUR 1/4)
;; ;    (EVENT :VALUE 2 :DUR 1/4)
;; ;    (EVENT :TYPE :REST :DUR 1/4))

;; (next-upto-n (pcycles '(1 - (2 3)) :repeats 1)) ; 3 items in LIST, so each will fit into 1/3 beats.
;; ;=> ((EVENT :VALUE 1 :DUR 1/3)
;; ;    (EVENT :TYPE :REST :DUR 1/3)
;; ;    (EVENT :VALUE 2 :DUR 1/6) ; since 2 and 3 are a sublist, they will each be 1/2 of 1/3 beats long, thus 1/6.
;; ;    (EVENT :VALUE 3 :DUR 1/6))

;; (next-upto-n (pcycles '(foo 2 3) :map (list :foo (event :midinote 60)) :repeats 1)) ; map the symbol \"FOO\" to (event :midinote 60)
;; ;=> ((EVENT :MIDINOTE 60 :DUR 1/3) ; note that the mapped event still contains the correct :dur.
;; ;    (EVENT :VALUE 2 :DUR 1/3)
;; ;    (EVENT :VALUE 3 :DUR 1/3))

;; (next-upto-n (pcycles '(1 2) :key :bar :repeats 1)) ; the value of each item will be used as the value of the :key key -- in this case, :bar.
;; ;=> ((EVENT :BAR 1 :DUR 1/2)
;; ;    (EVENT :BAR 2 :DUR 1/2))

;; (next-upto-n (pcycles '(1 2) :dur 4 :repeats 1)) ; set the total duration of each cycle of LIST to 4 beats.
;; ;=> ((EVENT :VALUE 1 :DUR 2) ; 4 beats divided by 2 events = each event is 2 beats in length.
;; ;    (EVENT :VALUE 2 :DUR 2))

;; (next-upto-n (pcycles '(1 2) :repeats 2)) ; repeat the list 2 times before ending the pattern.
;; ;=> ((EVENT :VALUE 1 :DUR 1/2) ; 2 items in list and 2 repeats results in 4 total outputs.
;; ;    (EVENT :VALUE 2 :DUR 1/2)
;; ;    (EVENT :VALUE 1 :DUR 1/2)
;; ;    (EVENT :VALUE 2 :DUR 1/2))

pcycles is based on and inspired by the TidalCycles live coding environment (see https://tidalcycles.org/ ), however most TidalCycles notation is not supported.

See also: `cycles', `ptrack'"
  :defun (defun pcycles (list &key map (key :value) (dur 1) (repeats *default-pattern-repeats*))
           (let ((args (list :map map :key key :dur dur :repeats repeats)))
             (etypecase list
               (string
                (apply #'pcycles (mapcar #'upcase-intern (coerce list 'list)) args))
               ((or list pattern)
                (apply #'make-instance 'pcycles :list list args))))))

(defmethod print-object ((pcycles pcycles) stream)
  (with-slots (list map key dur repeats) pcycles
    (format stream "(~S ~S~@[ :MAP ~S~]~@[ :KEY ~S~]~@[ :DUR ~S~]~@[ :REPEATS ~S~])"
            'pcycles list
            map
            (unless (eql :value key) key)
            (unless (= 1 dur) dur)
            (unless (eql :inf repeats) repeats))))

(defun pcycles-parse-list (list &key map (dur 1) (key :value))
  (let ((map (concatenate 'list map (list :- (event :type :rest) :_ (event :type :rest) :. (event :type :rest)))))
    (labels ((recurse (list subdur)
               (mapcar (fn (if (consp _)
                               (recurse _ (* subdur (/ 1 (length _))))
                               (combine-events (or (doplist (k v map)
                                                     (when (funcall
                                                            (if (typep _ 'string-designator) #'string= #'eql)
                                                            k _)
                                                       (return v)))
                                                   (event key _))
                                               (event :dur (* dur subdur)))))
                       list)))
      (flatten (recurse list (/ 1 (length list)))))))

(defmethod as-pstream ((pcycles pcycles)) ; FIX: maybe make pcycles parse in the 'next' method instead of at construction time?
  (with-slots (list map key dur repeats) pcycles
    (make-instance 'pcycles-pstream
                   :list list
                   :map (pattern-as-pstream map)
                   :key key
                   :dur (pattern-as-pstream dur)
                   :repeats (as-pstream repeats))))

(defmethod next ((pcycles pcycles-pstream))
  (with-slots (number list dur map key parsed-list) pcycles
    (unless (slot-boundp pcycles 'parsed-list)
      (setf parsed-list (pcycles-parse-list list :map map :dur dur :key key)))
    (when (and (plusp number)
               (zerop (mod number (length parsed-list))))
      (decf-remaining pcycles))
    (if (remaining-p pcycles)
        (elt-wrap parsed-list number)
        eop)))

(defun cycles-parse (list)
  (labels ((modifier-symbol-p (symbol)
             (and (symbolp symbol)
                  (position (aref (symbol-name symbol) 0) (list "*" "/") :test #'string=)))
           (parse-modifier-symbol (symbol)
             (let ((str (symbol-name symbol)))
               (list (make-keyword (aref str 0)) (read-from-string (subseq str 1)))))
           (modifier-symbol-dup (symbol)
             (let ((parsed (parse-modifier-symbol symbol)))
               (case (car parsed)
                 (:* (cadr parsed))
                 (:/ 1))))
           (length-for-dur (list)
             (apply #'+
                    (length (remove-if #'modifier-symbol-p list))
                    (* -1 (length (remove-if-not #'modifier-symbol-p list)))
                    (mapcar (lambda (x)
                              (let ((parsed (parse-modifier-symbol x)))
                                (case (car parsed)
                                  (:* 1)
                                  (:/ (cadr parsed)))))
                            (remove-if-not #'modifier-symbol-p list))))
           (dur-from-modifier (dur &optional modifier)
             (* dur (if modifier
                        (let ((parsed (parse-modifier-symbol modifier)))
                          (case (car parsed)
                            (:* (/ 1 (cadr parsed)))
                            (:/ (cadr parsed))
                            (t 1)))
                        1)))
           (recurse (list dur)
             (let ((num (length-for-dur list)))
               (loop :for i :in list
                     :for idx :from 0
                     :for nxt := (nth (1+ idx) list)
                     :if (consp i)
                       :append (flatten-1 (make-list (if (modifier-symbol-p nxt)
                                                         (modifier-symbol-dup nxt)
                                                         1)
                                                     :initial-element (recurse i (dur-from-modifier (/ dur num) (when (modifier-symbol-p nxt) nxt)))))
                     :else
                       :unless (modifier-symbol-p i)
                         :append (make-list (if (modifier-symbol-p nxt)
                                                (modifier-symbol-dup nxt)
                                                1)
                                            :initial-element (list (dur-from-modifier (/ dur num)
                                                                                      (when (modifier-symbol-p nxt)
                                                                                        nxt))
                                                                   i))))))
    (recurse list 1)))

(defmacro cycles (spec map &rest list)
  "Convenience macro to specify a rhythm or melody using symbols. Outputs a list of events with :type, :dur, and another parameter which is specifiable. Usually used to :embed into a pattern.

SPEC is a symbol representing the type of output you're going to send. It can be midinote, freq, note, instrument, or buffer/bufnum. It can also be a list, which provides the spec as the first element, and keywords (such as :dur for the total dur) for the rest.

MAP is a plist specifying the mapping from symbols to SPEC's parameter. It can be blank if you're just making a melody. By default, the map includes keys that map - and _ to rests.

LIST is the actual pattern to generate."
  (destructuring-bind (key &key (dur 1)) (ensure-list spec)
    (let ((key (make-keyword key))
          (map (concatenate 'list map (list :- (event :type :rest) :_ (event :type :rest)))))
      (labels ((translate-symbol (symbol)
                 (or (getf map symbol)
                     symbol)))
        `(pseq (list ,@(mapcar (lambda (i)
                                 `(event :dur ,(* dur (car i)) ,@(if (eql '- (cadr i))
                                                                     (list :type :rest)
                                                                     (list key (translate-symbol (cadr i))))))
                               (cycles-parse list)))
               1)))))

(export '(cycles))
