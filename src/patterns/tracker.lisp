(in-package #:cl-patterns)

;;; ptracker

(defpattern ptracker (pattern)
  (header
   rows
   (repeats :state t)
   (current-row :state t)
   (current-repeats-remaining :state t))
  :documentation "Defines an event pattern via a tracker-inspired notation. HEADER is a plist mapping the pattern's \"columns\" (keys) to their default values or to patterns that are used to generate the key's values for each output. ROWS is a list of lists, each sublist specifying the values for a step. Values can be specified on their own (in which case they're matched in order to the columns specified in the header) or by using Lisp's traditional key/value notation.

By default, the pattern repeats infinitely, however it can be limited by providing a :repeats argument in the header specifying the number of repeats.

Example:

;; (ptracker (list :freq 440 :dur 1/4)
;;           '((100) ;; play a note with a frequency of 100
;;             :r ;; rest for 1/4 beat
;;             (:midinote 70 :dur 1/2) ;; play a note at midinote 70 for 1/2 beat
;;             (-) ;; continue previous note for an additional 1/4 beat
;;             (600 3/4) ;; play a note with a frequency of 600 for 3/4 beat
;;             :r ;; rest for 1/4 beat
;;             (750 :foo 3) ;; play a note at frequency 750, dur 1/4, and additional :foo key with value 3
;;             ))

See also: `pt', `pcycles', `pbind'"
  :defun (assert (evenp (length header)) (header)))

(defmethod as-pstream ((ptracker ptracker))
  (with-slots (header rows current-row) ptracker
    (let ((repeats (or (getf header :repeats) :inf))
          (header (mapcar #'pattern-as-pstream (remove-from-plist header :repeats))))
      (make-instance 'ptracker-pstream
                     :header header
                     :rows (mapcar
                            (lambda (row)
                              (typecase row
                                (list
                                 (mapcar #'pattern-as-pstream row))
                                (t row)))
                            rows)
                     :repeats (as-pstream repeats)
                     :current-row 0
                     :current-repeats-remaining repeats))))

(defmethod next ((ptracker ptracker-pstream))
  (with-slots (header rows current-row current-repeats-remaining) ptracker
    (when (>= current-row (length rows))
      (setf current-row 0)
      (decf-remaining ptracker)
      (unless (value-remaining-p current-repeats-remaining)
        (return-from next nil)))
    (let* ((c-header (mapcar #'next header))
           (row (nth current-row rows))
           (row (etypecase row
                  (event (event-plist row))
                  (list row)
                  (atom (list row))))
           (inc-by 1))
      (unless (position nil c-header)
        (labels ((parse (index &optional keys-p)
                   (when-let ((cur (nth index row)))
                     (if keys-p
                         (list* cur
                                (next (nth (1+ index) row))
                                (parse (+ 2 index) t))
                         (if (keywordp cur)
                             (parse index t)
                             (list* (nth index (keys c-header))
                                    (next cur)
                                    (parse (1+ index))))))))
          ;; generate the output event for this step
          (let ((ev
                 (if (and (listp row)
                          (null (cdr row))
                          (rest-p (car row)))
                     (combine-events
                      (apply #'event c-header)
                      (event :type :rest))
                     (let ((res (parse 0)))
                       (apply #'event
                              (append
                               (flatten-1 (mapcar (lambda (x)
                                                    (list x (getf c-header x)))
                                                  (set-difference (keys c-header) (keys res))))
                               res))))))
            ;; check for and handle continuation lines (dashes) after this line, increasing the dur of the event for each.
            (let ((next-num (1+ current-row))
                  (extra-dur 0))
              (loop :while (let ((next (car (ensure-list (nth next-num rows)))))
                             (and (symbolp next)
                                  (string= :- next)))
                 :do
                   (incf next-num)
                   (incf inc-by)
                   (incf extra-dur (or (getf (mapcar #'next header) :dur) 1)))
              (when (plusp extra-dur)
                (incf (event-value ev :dur) extra-dur)))
            (incf current-row inc-by)
            ev))))))

(defmacro pt (header &rest rows)
  "Syntax sugar for `ptracker'. Avoids the need to quote or use `list'.

Example:

;; (pt (:foo 1 :bar (pseries) :dur 1/4)
;;     (2) ;; (:foo 2 :bar 0 :dur 1/4)
;;     () ;; (:foo 1 :bar 1 :dur 1/4)
;;     (9 :bar 90) ;; (:foo 9 :bar 90 :dur 1/2)
;;     (-) ;; extends the previous note by 1/4 beat
;;     :r ;; (:foo 1 :bar 4 :dur 1/4 :type :rest)
;;     )

See also: `ptracker'"
  (flet ((ensure-dash-quoted (e)
           (if (and (symbolp e)
                    (string= "-" e))
               '(quote -)
               e)))
    (let ((rows (mapcar (lambda (row)
                          (if (listp row)
                              (if (and (symbolp (car row))
                                       (string-equal "LAMBDA" (symbol-name (car row))))
                                  row
                                  `(list ,@(mapcar #'ensure-dash-quoted row)))
                              (ensure-dash-quoted row)))
                        rows)))
      `(ptracker (list ,@header) (list ,@rows)))))

(defun tracker-shorthand (stream char subchar)
  "Reader macro for `ptracker' preprocessing."
  (declare (ignore char subchar))
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\; (lambda (stream ignore)
                               (funcall (get-macro-character #\; nil) stream ignore)
                               (values :+ptracker-shorthand-separator-symbol+)))
    (set-macro-character #\newline (lambda (xx yy)
                                     (declare (ignore xx yy))
                                     (values :+ptracker-shorthand-separator-symbol+)))
    (let* ((input (read-preserving-whitespace stream nil nil))
           (rows (remove-if #'null (split-sequence input :+ptracker-shorthand-separator-symbol+))))
      `(list ,@(mapcar (lambda (row)
                         (append (list 'list)
                                 (mapcar (lambda (atom)
                                           (if (and (symbolp atom)
                                                    (string= "-" (symbol-name atom)))
                                               :-
                                               atom))
                                         row)))
                       rows)))))

(defun t-short (stream char subchar)
  (declare (ignore char subchar))
  (loop (read-char))
  (list 'quote (read-preserving-whitespace stream)))

(defun string-reader (stream char)
  (declare (ignore char))
  (let ((this (let ((*readtable* (copy-readtable)))
                (setf (readtable-case *readtable*) :preserve)
                (read stream t nil t))))
    (etypecase this
      (string this)
      (symbol (symbol-name this)))))

(named-readtables:defreadtable :cl-patterns
  (:merge :standard)
  (:dispatch-macro-char #\# #\T #'cl-patterns::tracker-shorthand))
