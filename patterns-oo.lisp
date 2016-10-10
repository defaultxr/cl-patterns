;; OO-style patterns

(in-package :cl-patterns)

(defgeneric play (item))

(defmethod play ((item cons))
  (output "It's a cons, yo."))

(defclass pattern ()
  ((stream :initarg :stream :accessor :stream)
   (remaining :initarg :remaining :accessor :remaining :initform nil)
   (number :accessor :number :initform 0))
  (:documentation "Abstract pattern superclass."))

(defgeneric next (pattern))

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

(defmethod next :around ((pattern pattern))
  (labels ((get-value (pattern)
             (let ((res (call-next-method)))
               (typecase res
                 (pattern (next res))
                 (t res)))))
    (if (null (slot-value pattern 'remaining))
        (get-value pattern)
        (when (> (slot-value pattern 'remaining) 0)
          (decf (slot-value pattern 'remaining))
          (get-value pattern)))))

(defmethod next :after ((pattern pattern))
  (incf (slot-value pattern 'number)))

(defmethod next ((pattern function))
  (funcall pattern))

(defmethod next ((pattern t))
  pattern)

(defgeneric next-n (pattern n))

(defmethod next-n (pattern (n number))
  (loop
     :for i from 0 below n
     :collect (next pattern)))

(defclass pbind (pattern)
  ((pairs :initarg :pairs :accessor :pairs :initform (list))))

(defun pbind (&rest pairs) ;; FIX
  (make-instance 'pbind
                 :pairs pairs
                 :stream
                 (labels ((pbind-accumulator (pairs chash)
                            (setf (getf chash (as-keyword (car pairs)))
                                  (cadr pairs))
                            (if (not (null (cddr pairs)))
                                (pbind-accumulator (cddr pairs) chash)
                                chash)))
                   (let ((next pattern)) (as-stream (pbind-accumulator pairs (list)))
                     (lambda ()
                       (multiple-value-bind (cur nxt) (get-result next)
                         (setf next nxt)
                         cur))))))

(defclass pseq (pattern)
  ((list :initarg :list :accessor :list)))

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

(defun pseq (list &optional repeats)
  (make-instance 'pseq
                 :list list
                 :remaining (when repeats (* repeats (length list)))))

(defmethod next ((pattern pseq))
  (nth (wrap (slot-value pattern 'number) 0 (1- (length (slot-value pattern 'list))))
       (slot-value pattern 'list)))

(defclass pk (pattern)
  ((key :initarg :key :accessor :key)
   (default :initarg :default :accessor :default :initform 1)))

(defun pk (key &optional (default 1))
  (make-instance 'pk
                 :key key
                 :default default
                 :stream
                 (lambda ()
                   (or (getf *event* key) default))))
