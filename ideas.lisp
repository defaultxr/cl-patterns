(defun tsubseq (sequence start &optional end)
  "Return a copy of a subsequence of SEQUENCE starting with the first element after START, ending with the last element before END.
START and END can be negative, in which case the elements are taken from the end." ;; FIX this description i was tired when i wrote it
  )

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

