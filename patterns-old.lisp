;; (defmethod next :around ((pattern pattern)) ;; OLD
;;   (labels ((get-value (pattern)
;;              (incf (slot-value pattern 'number))
;;              (let ((res (funcall (slot-value pattern 'stream))))
;;                (typecase res
;;                  (pattern (next res))
;;                  (t res)))))
;;     (if (null (slot-value pattern 'remaining))
;;         (get-value pattern)
;;         (when (> (slot-value pattern 'remaining) 0)
;;           (decf (slot-value pattern 'remaining))
;;           (get-value pattern)))))

;; (defun pseq (list &optional repeats) ;; OLD
;;   (make-instance 'pseq
;;                  :list list
;;                  :remaining (when repeats (* repeats (length list)))
;;                  :stream
;;                  (let ((list-2 list))
;;                    (lambda ()
;;                      (let ((res (car list-2)))
;;                        (setf list-2 (append (cdr list-2) (list (car list-2))))
;;                        res)))))

