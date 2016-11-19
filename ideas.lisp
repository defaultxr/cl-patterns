;; FIX: doesn't work with negative numbers yet
(defgeneric tsubseq (sequence start &optional end)
  (:documentation "Return a copy of a subsequence of SEQUENCE starting with the first element after START, ending with the last element before END.
START and END can be negative, in which case the elements are taken from the end.
Note that this function only takes the start of each event into account, so it's possible that events may play beyond the end of the sequence. See also: `tsubseq*', which will cut events short if they go beyond END."))

(defmethod tsubseq ((sequence pattern) start &optional (end 16)) ;; helper function
  (tsubseq (as-pstream sequence) start end))

(defmethod tsubseq ((sequence pstream) start &optional (end 16)) ;; helper function, gets the shortest list, then passes to the tsubseq list method
  (let ((cdur 0)
        (list (list)))
    (loop
       :for ev = (next sequence)
       :while (and ev end (<= cdur end))
       :do
       (progn
         (setf list (append list (list ev)))
         (incf cdur (dur ev))))
    (tsubseq list start end)))

(defmethod tsubseq ((sequence list) start &optional end) ;; this function does the heavy lifting.
  (let ((cdur 0)
        (list (list)))
    (loop
       :for ev :in sequence
       :do
       (progn
         (if (null end)
             (when (>= cdur start)
               (setf list (append list (list ev))))
             (when (and (>= cdur start)
                        (< cdur end))
               (setf list (append list (list ev)))))
         (incf cdur (dur ev))))
    list))

(defun tsubseq-rel (sequence start &optional (dur 16))
  "Return a subsequence of total duration DUR, starting with the first event after START."
  (tsubseq sequence start (+ start dur)))

(defgeneric tsubseq* (sequence start &optional end)
  (:documentation "Return a copy of a subsequence of SEQUENCE starting with the first element playing at START (cut if necessary), ending with the last element before END.
START and END can be negative, in which case the elements are taken from the end.
Because it cuts off the start and end of notes that are outside of the range specified by START and END, this function will always return a list whose total duration is exactly END minus START. If necessary it will also pad the beginning and end of the returned list with rests.
See also: `tsubseq', which will not cut events short or insert rests."))

(defmethod tsubseq* ((sequence pattern) start &optional (end 16)) ;; helper function
  (tsubseq* (as-pstream sequence) start end))

(defmethod tsubseq* ((sequence pstream) start &optional (end 16)) ;; helper function, gets the shortest list, then passes to the tsubseq list method
  (let ((cdur 0)
        (list (list)))
    (loop
       :for ev = (next sequence)
       :while (and ev end (<= cdur end))
       :do
       (progn
         (setf list (append list (list ev)))
         (incf cdur (dur ev))))
    ;; (tsubseq* list start end)
    list))

(defmethod tsubseq* ((sequence list) start &optional end) ;; this function does the heavy lifting.
  (let ((cdur 0)
        (list (list))
        ;; (sdur (reduce #'+ (mapcar #'dur sequence)))
        )
    (loop
       :for ev :in sequence
       :do
       (let ((ev-g-end (+ cdur (sustain ev))))
         (if (null end)
             (when (> ev-g-end start)
               (when (< cdur start)
                 (setf (sustain ev) (- ev-g-end start)))
               (setf list (append list (list ev))))
             (when (and (>= cdur start)
                        (< cdur end))
               (setf list (append list (list ev)))))
         (incf cdur (dur ev))))
    list))

(defun tsubseq*-rel (sequence start &optional (dur 16))
  (tsubseq* sequence start (+ start dur)))

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
                                  (lambda (stream char1 char2)
                                      (apply fn
                                             (read-delimited-list right stream t))))))

(defdelim #\[ #\] (&rest args)
  `(list ,@args))

