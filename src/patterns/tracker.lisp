(in-package #:cl-patterns)

;;; ptracker (FIX)
;; FIX: allow each row to have keyword arguments
;; FIX: make as-pstream method so any functions in the rows will be executed each time.

(defpattern ptracker (pattern)
  (header
   rows)
  "ptracker " ;; FIX: docstring needed
  (defun ptracker (header rows)
    (assert (evenp (length header)) (header))
    (let* ((h-ev (apply #'event header))
           (h-keys (keys h-ev))
           (result (list)))
      (loop :for row :in rows :do
         (let ((r-ev (cond ((equal row (list :-))
                            (when (= 0 (length result))
                              (event :type :rest)))
                           ((position (car row) (list :r :rest))
                            (event :type :rest))
                           (t
                            (progn
                              (apply #'event (loop
                                                :for e :in row
                                                :for i :from 0
                                                :append (list (nth i h-keys) e))))))))
           (if r-ev
               (alexandria:appendf result (list (combine-events h-ev r-ev)))
               (incf (event-value (car (last result)) :dur) (dur h-ev)))))
      (pseq result 1))))

;; (defmethod next ((pattern ptracker-pstream))
;;   )

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
    (let ((rows (remove-if #'null (split-sequence:split-sequence :+ptracker-shorthand-separator-symbol+ (read-preserving-whitespace stream nil nil)))))
      `(list ,@(mapcar (lambda (row)
                         (append (list 'list)
                                 (mapcar (lambda (atom)
                                           (if (and (symbolp atom)
                                                    (string= "-" (symbol-name atom)))
                                               :-
                                               atom))
                                         row)))
                       rows)))))

(set-dispatch-macro-character #\# #\T #'tracker-shorthand)

