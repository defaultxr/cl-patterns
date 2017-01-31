;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.

(in-package :cl-patterns)

(defclass clock ()
  ((beats :initform 0)
   (tempo :initform 1 :initarg :tempo :accessor tempo)
   (granularity :initform 1/10 :initarg :granularity)
   (tasks :initform nil)
   ;; (tasks-lock :initform (bt:make-lock))
   ))

(defclass task ()
  ((gensym :initform (gensym))
   (clock :initform *clock* :initarg :clock)
   (item :initform nil :initarg :item)
   ;; (dead-p :initform nil) ;; FIX?
   ))

(defun make-clock (&optional (tempo 1) tasks)
  "Create a clock. The clock automatically starts running in a new thread."
  (let ((clock (make-instance 'clock :tempo tempo)))
    (mapc (lambda (task) (clock-add task clock)) tasks)
    (bt:make-thread (lambda () (tick clock)) :name "cl-patterns clock")
    clock))

(defmethod print-object ((clock clock) stream)
  (format stream "#<CLOCK :tempo ~f :beats ~f>" (slot-value clock 'tempo) (slot-value clock 'beats)))

(defun clock-add (item &optional (clock *clock*))
  (bt:with-lock-held ((slot-value clock 'tasks-lock))
    (let ((task (make-instance 'task :clock clock :item item)))
      (setf (slot-value clock 'tasks) (append (slot-value clock 'tasks) (list task)))
      task)))

(defun clock-remove (item &optional (clock *clock*))
  (bt:with-lock-held ((slot-value clock 'tasks-lock))
    (setf (slot-value clock 'tasks)
          (remove-if
           (lambda (task)
             (eq (slot-value task 'gensym) (slot-value item 'gensym)))
           (slot-value clock 'tasks)))))

(defun clock-clear-tasks (&optional (clock *clock*))
  (bt:with-lock-held ((slot-value clock 'tasks-lock))
    (setf (slot-value clock 'tasks) nil)))

(defun clock-process-task (task) ;; if the task is done with, we return it so it can be removed from the clock, else we return nil so nothing happens.
  (let ((nv (next (slot-value task 'item))))
    (if (null nv)
        task
        (progn
          (play nv)
          nil))))

(defun task-should-run-now-p (task clock) ;; FIX
  (let ((item (slot-value task 'item)))
    (if (eq 'pbind-pstream (type-of item))
        (if (= 0 (slot-value item 'number)) ;; if the pstream hasn't started yet, it starts when the :quant determines it.
            (or (= 0 (slot-value item 'quant))
                (< (mod (slot-value clock 'beats) (slot-value item 'quant)) (slot-value clock 'granularity)))
            t ;; FIX
            )
        t)))

(defun tick (clock)
  (loop :do
     (format t "clock: ~a; tasks:~{ ~a~}~%" clock (slot-value clock 'tasks))
     ;; FIX: use acquire-lock and wait-p on the clock's tasks list.
     (let ((results (bt:with-lock-held ((slot-value clock 'tasks-lock))
                      (mapcar #'clock-process-task (remove-if-not
                                                    (lambda (task) (task-should-run-now-p task clock))
                                                    (slot-value clock 'tasks)))))) ;; FIX: this does consing, may be slow...
       ;; remove dead tasks
       (mapc (lambda (task)
               (when (not (null task)) ;; if it's nil it means it's not done with yet so we do nothing.
                 (clock-remove task clock)))
             results))
     (sleep (dur-time (slot-value clock 'granularity) (slot-value clock 'tempo))) ;; FIX: account for possible lag.
     ;; FIX: do we need to acquire a lock on the clock's tempo as well?
     (incf (slot-value clock 'beats) (slot-value clock 'granularity))))

(defmethod play ((item pbind))
  (clock-add (as-pstream item)))

(defmethod stop ((item task))
  (clock-remove item))

;; (defparameter *clock* (make-clock 1))
