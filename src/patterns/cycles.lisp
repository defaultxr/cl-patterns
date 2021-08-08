(in-package #:cl-patterns)

;;; pcycles
;; inspired by tidalcycles
;; FIX: remove this and write it as regular functions instead?

(defpattern pcycles (pattern)
  (list
   (map :default nil :initform nil)
   (key :default :value :initform :value)
   (dur :default 1 :initform 1)
   (repeats :default :inf :initform :inf)
   (parsed-list :state t)
   (current-repeats-remaining :state t))
  :documentation "pcycles yields values from LIST as events whose dur is (/ 1 list-length) and whose value is the original value in the list. This process recurses into sublists, subdividing their durs equally among the sublist's contents to be a fraction of what their dur originally would be. The total dur yielded by pcycles is always equal to 1. pcycles repeats the whole LIST once."
  :defun (defun pcycles (list &key map (key :value) (dur 1) (repeats :inf))
           (let ((args (list :map map :key key :dur dur :repeats repeats)))
             (etypecase list
               (string
                (apply #'pcycles (mapcar #'my-intern (coerce list 'list)) args))
               ((or list pattern)
                (apply #'make-instance 'pcycles :list list args))))))

(defmethod print-object ((pcycles pcycles) stream)
  (with-slots (list map key dur repeats) pcycles
    (format stream "(~s ~s~@[ :MAP ~s~]~@[ :KEY ~s~]~@[ :DUR ~s~]~@[ :REPEATS ~s~])"
            'pcycles list
            map
            (unless (eql :value key) key)
            (unless (= 1 dur) dur)
            (unless (eql :inf repeats) repeats))))

(defun pcycles-parse-list (list &key map (dur 1) (key :value))
  (let ((map (concatenate 'list map (list :- (event :type :rest) :_ (event :type :rest)))))
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

(defmethod as-pstream ((pcycles pcycles)) ;; FIX: maybe make pcycles parse in the 'next' method instead of at construction time?
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
