(in-package #:cl-patterns)

;;; ptracker (FIX)
;; FIX: allow each row to have keyword arguments
;; FIX: make as-pstream method so any functions in the rows will be executed each time.
;; FIX: use https://github.com/Shinmera/trivial-indent to ensure all forms are correctly indented?

(defpattern ptracker (pattern)
    (header
     rows
     (current-row :state t))
  :documentation "Creates an event pattern via tracker-inspired notation. HEADER is an argument list mapping the pattern's \"columns\" (keys) to their default values (or to patterns that are used to generate the key's values). ROWS is a list of lists, each sublist specifying the values for that step. Values can be specified on their own (in which case they're matched in order to the columns specified in the header) or by using Lisp's traditional key/value notation.

Example:

;; (ptracker (list :rate 1 :start 0 :bufnum buf-1 :instrument :spt :dur 1/4)
;;           '((1) ;; play at rate 1
;;             (-) ;; no event (continue previous note for this 1/4 beat)
;;             (-) ;; same
;;             (-) ;; same
;;             (0.5 0.5) ;; play at rate 0.5 and with start point 0.5
;;             (-) ;; continue previous note
;;             (:bufnum buf-2) ;; trigger an event with a different buffer
;;             (-) ;; continue previous note
;;             (-) ;; same
;;             ))

See also: `pbind'"
  :defun (assert (evenp (length header)) (header)))

(defmacro pt (header &rest rows)
  "Syntax sugar for `ptracker'"
  `(ptracker (list ,@header) ,@rows))

;; (defun ptracker (header rows)
;;   (assert (evenp (length header)) (header))
;;   (let* ((h-ev (apply #'event header))
;;          (h-keys (keys h-ev))
;;          (result (list)))
;;     (loop :for row :in rows :do
;;          (let ((r-ev (cond ((equal row (list :-))
;;                             (when (= 0 (length result))
;;                               (event :type :rest)))
;;                            ((position (car row) (list :r :rest))
;;                             (event :type :rest))
;;                            (t
;;                             (progn
;;                               (apply #'event (loop
;;                                                 :for e :in row
;;                                                 :for i :from 0
;;                                                 :append (list (nth i h-keys) e))))))))
;;            (if r-ev
;;                (appendf result (list (combine-events h-ev r-ev)))
;;                (incf (event-value (car (last result)) :dur) (dur h-ev)))))
;;     (pseq result 1)))

(defmethod as-pstream ((ptracker ptracker))
  (with-slots (header rows current-row) ptracker
    (make-instance 'ptracker-pstream
                   :header header
                   :rows rows
                   :current-row 0)))

(defmethod next ((ptracker ptracker-pstream))
  (with-slots (header rows current-row) ptracker
    (labels ((row-keys (row)
               (let ((h-keys (keys header)))
                 (append h-keys (keys (ignore-errors (subseq row (length h-keys))))))))
      (let* ((h-keys (keys header))
             (row (nth current-row rows))
             (n-row (nth (1+ current-row) rows))
             (row-keys (row-keys row)))
        (prog1
            (when row
              (apply 'event
                     (append
                      (loop
                         :for i :in (subseq row 0 (length h-keys))
                         :for idx :from 0
                         :append (list (nth idx h-keys)
                                       i))
                      (ignore-errors (subseq row (length h-keys))))))
          (incf current-row))))))

;; (pdef :foo
;;       (ptracker (list :beat 0 :midinote 69)
;;                 #T(
;;                    0 68
;;                    1 87
;;                    2 32
;;                    3 56)))

(defun tracker-shorthand (stream char subchar)
  "Reader macro for `ptracker' preprocessing. "
  (declare (ignore char subchar))
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\; (lambda (stream ignore)
                               (funcall (get-macro-character #\; nil) stream ignore)
                               (values :+ptracker-shorthand-separator-symbol+)))
    (set-macro-character #\newline (lambda (xx yy)
                                     (declare (ignore xx yy))
                                     (values :+ptracker-shorthand-separator-symbol+)))
    (let* ((input (read-preserving-whitespace stream nil nil))
           (rows (remove-if #'null (split-sequence:split-sequence :+ptracker-shorthand-separator-symbol+ input))))
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
