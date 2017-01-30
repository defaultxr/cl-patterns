;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.

(in-package :cl-patterns)

(defclass clock ()
  ((beats :initform 0 :accessor :beats)
   (tempo :initform 1 :initarg :tempo :accessor :tempo)
   (granularity :initform 0.1 :initarg :granularity :accessor :granularity)
   (tasks :initform nil :accessor :tasks)))

(defclass task ()
  ((gensym :initform (gensym) :accessor :gensym)
   (clock :initform *clock* :initarg :clock :accessor :clock)
   (item :initform nil :initarg :item :accessor :item)
   ;; (dead-p :initform nil :accessor :dead-p) ;; FIX?
   ))

(defun make-clock (&optional (tempo 1) tasks)
  "Create a clock. The clock automatically starts running in a new thread."
  (let ((clock (make-instance 'clock :tempo tempo)))
    (mapc (lambda (task) (clock-add task clock)) tasks)
    (bt:make-thread (lambda () (tick clock)) "cl-patterns clock")
    clock))

(defun clock-add (item &optional (clock *clock*))
  (bt:with-lock-held (slot-value clock 'tasks) ;; FIX: use acquire-lock with wait-p enabled instead to block until item is scheduled.
    (let ((task (make-instance 'task :clock clock :item item)))
      (setf (slot-value clock 'tasks) (append (slot-value clock 'tasks) task))
      task)))

(defun clock-remove (item &optional (clock *clock*))
  (bt:with-lock-held (slot-value clock 'tasks) ;; FIX: use acquire-lock with wait-p instead
    (setf (slot-value clock 'tasks)
          (remove-if
           (lambda (task)
             (eq (slot-value task 'gensym) (slot-value item 'gensym)))
           (slot-value clock 'tasks)))))

(defparameter *clock* (make-clock 1))

(defun clock-process-task (task) ;; if the task is done with, we return it so it can be removed from the clock, else we return nil so nothing happens.
  (let ((nv (next (slot-value task 'item))))
    (if (null nv)
        task
        (progn
          (play nv)
          nil))))

(defun task-prior-to-current-beat-p (task) ;; FIX
  nil)

(defun tick (clock)
  (loop :do
     (format t "tick: ~a; tasks: ~{~a~}" clock (slot-value clock 'tasks))
     ;; FIX: use acquire-lock and wait-p on the clock's tasks list.
     (let ((results (mapcar #'clock-process-task (remove-if-not #'task-prior-to-current-beat-p (slot-value clock 'tasks)))))
       ;; remove dead tasks
       (mapc (lambda (task)
               (when (not (null result)) ;; if it's nil it means it's not done with yet so we do nothing.
                 (clock-remove task clock)))
             results))
     (sleep (dur-time (slot-value clock 'granularity) (slot-value clock 'tempo))) ;; FIX: account for possible lag.
     ;; FIX: do we need to acquire a lock on the clock's tempo as well?
     (incf (slot-value clock 'beats) (slot-value clock 'granularity))))
