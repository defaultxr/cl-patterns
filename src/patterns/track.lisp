;;;; track.lisp - tracker-inspired pattern class and associated functionality.

(in-package #:cl-patterns)

;;; ptrack

(defpattern ptrack (pattern)
  ((header :initform nil)
   (rows :initform (make-list 16))
   (repeats :initform :inf)
   (current-row :state t)
   (current-repeats-remaining :state t))
  :documentation "Defines an event pattern via a tracker-inspired notation. HEADER is a plist mapping the pattern's \"columns\" (keys) to their default values or to patterns that are used to generate the key's values for each output. ROWS is a list of lists, each sublist specifying the values for a step. Values can be specified on their own (in which case they're matched in order to the columns specified in the header) or by using Lisp's traditional key/value notation.

By default, the pattern repeats infinitely, however it can be limited by providing a :repeats argument in the header specifying the number of repeats.

Example:

;; (ptrack (list :freq 440 :dur 1/4)
;;         '((100) ;; play a note with a frequency of 100
;;           :r ;; rest for 1/4 beat
;;           (:midinote 70 :dur 1/2) ;; play a note at midinote 70 for 1/2 beat
;;           (-) ;; continue previous note for an additional 1/4 beat
;;           (600 3/4) ;; play a note with a frequency of 600 for 3/4 beat
;;           :r ;; rest for 1/4 beat
;;           (750 :foo 3) ;; play a note at frequency 750, dur 1/4, and additional :foo key with value 3
;;           ))

See also: `pt', `pcycles', `pbind'"
  :defun (defun ptrack (header rows &key (repeats :inf))
           (assert (evenp (length header)) (header))
           (make-instance 'ptrack
                          :header header
                          :rows rows
                          :repeats repeats)))

(defmethod as-pstream ((ptrack ptrack))
  (with-slots (header rows repeats current-row) ptrack
    (let ((header (mapcar #'pattern-as-pstream header)))
      (make-instance 'ptrack-pstream
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

(defmethod next ((ptrack ptrack-pstream))
  (with-slots (header rows current-row current-repeats-remaining) ptrack
    (when (>= current-row (length rows))
      (setf current-row 0)
      (decf-remaining ptrack))
    (unless (value-remaining-p current-repeats-remaining)
      (return-from next eop))
    (let* ((c-header (mapcar #'next header))
           (row (nth current-row rows))
           (row (etypecase row
                  (event (event-plist row))
                  (list row)
                  (atom (list row))))
           (inc-by 1))
      (when (position eop c-header)
        (return-from next eop))
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
        (let ((ev (if (and (listp row)
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
            (while (let ((next (car (ensure-list (nth next-num rows)))))
                     (and (symbolp next)
                          (string= :- next)))
              (incf next-num)
              (incf inc-by)
              (incf extra-dur (or (getf (mapcar #'next header) :dur) 1)))
            (when (plusp extra-dur)
              (incf (event-value ev :dur) extra-dur)))
          (incf current-row inc-by)
          ev)))))

(defun ptrack-does-not-exist (ptrack type name if-does-not-exist)
  (etypecase if-does-not-exist
    ((member :error) (error "No ~A at ~S exists in ~S." (string-downcase type) name ptrack))
    (null nil)))

(defgeneric ptrack-rows (ptrack &key if-does-not-exist)
  (:documentation "Get the list of rows from PTRACK."))

(defmethod ptrack-rows (ptrack &key if-does-not-exist)
  (slot-value ptrack 'rows))

(defgeneric ptrack-row (ptrack row &key if-does-not-exist)
  (:documentation "Get the plist for ROW from PTRACK."))

(defmethod ptrack-row ((ptrack ptrack) row &key (if-does-not-exist :error))
  (typecase row
    ((member :header :head :h nil)
     (slot-value ptrack 'cl-patterns::header))
    ((integer 0)
     (let ((rows (slot-value ptrack 'cl-patterns::rows)))
       (elt rows row))) ; FIX: check if exists
    (t
     (ptrack-does-not-exist ptrack 'row row if-does-not-exist))))

(defmethod (setf ptrack-row) (value (ptrack ptrack) row &key)
  (etypecase row
    ((member :header :head :h nil)
     (setf (slot-value ptrack 'cl-patterns::header) value))
    ((integer 0)
     (setf (nth row (slot-value ptrack 'cl-patterns::rows)) value))))

(defgeneric ptrack-cell (ptrack row key &key if-does-not-exist) ; FIX: implement if-does-not-exist
  (:documentation "Get the value of a cell from ptrack (i.e. the value at that location if specified, or the generated value if not specified). Can also be used on a ptrack-pstream to get the generated values from it. Returns three values:

- the value of the cell
- the name of the key that the specified KEY was derived from, or nil if it was not found
- t if the output was taken from the row itself, or nil if it was derived from the header

See also: `ptrack', `ptrack-row'"))

(defmethod ptrack-cell ((ptrack ptrack) row key &key if-does-not-exist)
  (multiple-value-bind (res derived-from)
      (event-value (apply #'event (ptrack-row ptrack row :if-does-not-exist if-does-not-exist))
                   key)
    (if (eql derived-from t)
        (let ((res (lastcar (next-n ptrack (1+ row)))))
          (values-list (append (multiple-value-list (event-value res key))
                               (list nil))))
        (values res derived-from t))))

(defmethod ptrack-cell ((ptrack ptrack-pstream) row key &key if-does-not-exist)
  (etypecase row
    (symbol
     (ptrack-cell ptrack nil row))
    (integer
     (if-let ((event (elt ptrack row)))
       (event-value event key)
       (values nil nil nil)))))

(defgeneric (setf ptrack-cell) (value ptrack row key &key))

(defmethod (setf ptrack-cell) (value ptrack row key &key)
  (let ((row-plist (ptrack-row ptrack row)))
    (setf (getf row-plist key) value
          (ptrack-row ptrack row) row-plist)))

(uiop:with-deprecation (:warning)
  (defun ptracker (&rest args)
    "Deprecated alias for `ptrack'."
    (apply #'ptrack args)))
(export 'ptracker)

(defmacro pt (header &rest rows)
  "Syntax sugar for `ptrack'. Avoids the need to quote or use `list'.

Example:

;; (pt (:foo 1 :bar (pseries) :dur 1/4)
;;     (2) ;; (:foo 2 :bar 0 :dur 1/4)
;;     () ;; (:foo 1 :bar 1 :dur 1/4)
;;     (9 :bar 90) ;; (:foo 9 :bar 90 :dur 1/2)
;;     (-) ;; extends the previous note by 1/4 beat
;;     :r ;; (:foo 1 :bar 4 :dur 1/4 :type :rest)
;;     )

See also: `ptrack'"
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
      `(ptrack (list ,@header) (list ,@rows)))))

(defun track-shorthand (stream char subchar)
  "Reader macro for `ptrack' preprocessing."
  (declare (ignore char subchar))
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\; (lambda (stream ignore)
                               (funcall (get-macro-character #\; nil) stream ignore)
                               (values :+ptrack-shorthand-separator-symbol+)))
    (set-macro-character #\newline (lambda (xx yy)
                                     (declare (ignore xx yy))
                                     (values :+ptrack-shorthand-separator-symbol+)))
    (let* ((input (read-preserving-whitespace stream nil nil))
           (rows (remove nil (sequence-split input :+ptrack-shorthand-separator-symbol+))))
      `(list ,@(mapcar (lambda (row)
                         (list* 'list
                                (mapcar (lambda (atom)
                                          (if (and (symbolp atom)
                                                   (string= "-" atom))
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
  (:dispatch-macro-char #\# #\T #'cl-patterns::track-shorthand))
