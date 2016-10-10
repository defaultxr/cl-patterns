;; (ql:quickload '(:series))

;; (defun pseq (list &optional (repeats 1))
;;   "Pseq"
;;   (let ((current -1))
;;     (lambda ()
;;       (setf current (+ current 1))
;;       (nth current list))))

;; (defclass pattern ()
;;   (;; (length :initform 1)
;;    (length :accessor pattern-length :initarg :length :initform 1 :type (or null number))
;;    (series :accessor series :initarg :series :initform (series:scan (list)))))

;; (defun pseq (list &optional (repeats 1))
;;   (make-instance 'pattern
;;                  :length (* (length list) repeats)
;;                  :series (series:scan list)))

;; (defgeneric as-series (item))

;; (defmethod as-series ((item pattern))
;;   (series item))

;; (defun next-in-pattern (pattern)
;;   "Gets the next value in a pattern."
;;   (funcall pattern))

;; (setf foo (scan-fn 'number
;;                    (lambda () (alexandria:random-elt (list 1 2 3)))
;;                    (lambda (x) (+ 2 x))
;;                    (lambda (x) (< 15 x))))

