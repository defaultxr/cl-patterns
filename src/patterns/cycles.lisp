(in-package #:cl-patterns)

;;; pcycles
;; inspired by tidalcycles
;; FIX: remove this and write it as regular functions instead?

(defpattern pcycles (pattern) ;; FIX: add REPEATS slot
  (list
   (parsed-list :state t))
  "pcycles yields values from LIST as events whose dur is (/ 1 list-length) and whose value is the original value in the list. This process recurses into sublists, subdividing their durs equally among the sublist's contents to be a fraction of what their dur originally would be. The total dur yielded by pcycles is always equal to 1. pcycles repeats the whole LIST once.")

(defun pcycles-parse-list (list)
  (labels ((recurse (list dur)
             (loop :for i :in list
                :collect (if (consp i)
                             (recurse i (* dur (/ 1 (length i))))
                             (event :value i :dur dur)))))
    (alexandria:flatten (recurse list (/ 1 (length list))))))

(defmethod as-pstream ((pattern pcycles)) ;; FIX: maybe make pcycles parse in the 'next' method instead of at construction time?
  (with-slots (list) pattern
    (make-instance 'pcycles-pstream
                   :list list
                   :parsed-list (pcycles-parse-list list))))

(defmethod next ((pattern pcycles-pstream))
  (with-slots (number parsed-list) pattern
    (nth number parsed-list)))

(defun cycles-parse (list)
  (labels ((modifier-symbol-p (symbol)
             (and (symbolp symbol)
                  (position (aref (symbol-name symbol) 0) (list "*" "/") :test #'string=)))
           (parse-modifier-symbol (symbol)
             (let ((str (symbol-name symbol)))
               (list (alexandria:make-keyword (aref str 0)) (read-from-string (subseq str 1)))))
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
                  :for nxt = (nth (1+ idx) list)
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
                                     :initial-element (list (dur-from-modifier (/ dur num) (when (modifier-symbol-p nxt) nxt)) i))))))
    (recurse list 1)))

(defmacro cycles (spec map &rest list)
  "Convenience macro to specify a rhythm or melody using symbols. Outputs a list of events with :type, :dur, and another parameter which is specifiable. Usually used to :inject into a pattern.

SPEC is a symbol representing the type of output you're going to send. It can be midinote, freq, note, instrument, or bufnum. It can also be a list, which provides the spec as the first element, and keywords (such as :dur for the total dur) for the rest.

MAP is a plist specifying the mapping from symbols to SPEC's parameter. It can be blank if you're just making a melody.

LIST is the actual pattern to generate."
  (destructuring-bind (key &key (dur 1)) spec
    (let ((key (alexandria:make-keyword key)))
      (labels ((translate-symbol (symbol)
                 (or (getf map symbol)
                     symbol)))
        `(pseq (list ,@(loop :for i :in (cycles-parse list)
                          :collect `(event :dur ,(* dur (car i)) ,@(if (eq '- (cadr i))
                                                                       (list :type :rest)
                                                                       (list key (translate-symbol (cadr i)))))))
               1)))))

(export '(cycles))
