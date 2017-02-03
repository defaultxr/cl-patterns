;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.

(in-package :cl-patterns)

(defclass clock ()
  ((beats :initform 0)
   (tempo :initform 1 :initarg :tempo :accessor tempo)
   (granularity :initform 1/10 :initarg :granularity)
   (tasks :initform nil)
   (tasks-lock :initform (bt:make-lock))))

(defclass task ()
  ((gensym :initform (gensym))
   (clock :initform *clock* :initarg :clock)
   (item :initform nil :initarg :item)
   (start-time :initform nil :initarg :start-time)
   (next-time :initform nil :initarg :next-time)
   ;; (dead-p :initform nil) ;; FIX?
   ))

(defparameter *clock* nil)

(defun make-clock (&optional (tempo 1) tasks)
  "Create a clock. The clock automatically starts running in a new thread."
  (let ((clock (make-instance 'clock :tempo tempo)))
    (mapc (lambda (task) (clock-add task clock)) tasks)
    (bt:make-thread (lambda () (tick clock)) :name "cl-patterns clock")
    clock))

(defmethod print-object ((clock clock) stream)
  (format stream "#<CLOCK :tempo ~f :beats ~f>" (slot-value clock 'tempo) (slot-value clock 'beats)))

(defun clock-add (item &optional (clock *clock*))
  (with-slots (tasks tasks-lock) clock
    (bt:with-lock-held (tasks-lock)
      (let ((task (make-instance 'task :clock clock :item item)))
        (setf tasks (append tasks (list task)))
        task))))

(defun clock-remove (item &optional (clock *clock*))
  (with-slots (tasks tasks-lock) clock
    (bt:with-lock-held (tasks-lock)
      (setf tasks
            (remove-if
             (lambda (task)
               (eq (slot-value task 'gensym) (slot-value item 'gensym)))
             tasks)))))

(defun clock-clear-tasks (&optional (clock *clock*))
  (with-slots (tasks tasks-lock) clock
    (bt:with-lock-held (tasks-lock)
      (setf tasks nil))))

(defun clock-process-task (task clock)
  (let ((item (slot-value task 'item)))
    (when (= 0 (slot-value item 'number)) ;; pstream hasn't started yet.
      (setf (slot-value task 'start-time) (slot-value clock 'beats)
            (slot-value task 'next-time) (slot-value clock 'beats))))
  (let ((nv (next (slot-value task 'item)))
        (nt (or (slot-value task 'next-time) (slot-value clock 'beats))))
    (if (null nv)
        task ;; if the task is done with, we return it so it can be removed from the clock, else we return nil so nothing happens.
        (progn
          (incf (slot-value task 'next-time) (delta nv))
          (let ((time-at (- nt (slot-value clock 'beats))))
            (format t "~a~%~a next-time: ~f diff: ~f~%" nv clock nt time-at)
            (sc:at (+ (sc:now) (dur-time time-at (tempo clock)))
              (play nv)))
          (when (< (slot-value task 'next-time) (+ (slot-value clock 'beats) (slot-value clock 'granularity)))
            (clock-process-task task clock))))))

(defun task-should-run-now-p (task clock)
  (let ((item (slot-value task 'item)))
    (if (eq 'pbind-pstream (type-of item))
        (if (null (slot-value task 'next-time)) ;; (= 0 (slot-value item 'number)) ;; if the pstream hasn't started yet...
            (or (= 0 (slot-value item 'quant)) ;; ...it starts when the :quant determines it.
                (< (mod (slot-value clock 'beats) (slot-value item 'quant)) (slot-value clock 'granularity)))
            (< (slot-value task 'next-time) (+ (slot-value clock 'beats) (slot-value clock 'granularity))))
        t)))

(defun tick (clock)
  (with-slots (tasks tasks-lock granularity tempo beats) clock
    (loop :do
       ;; (when (= 0 (mod (slot-value clock 'beats) 1.0))
       ;;   (format t "clock: ~a; tasks:~{ ~a~}~%" clock tasks))
       ;; FIX: use acquire-lock and wait-p on the clock's tasks list.
       (let ((now (sc:now))
             (results (bt:with-lock-held (tasks-lock)
                        (mapcar (lambda (task) (clock-process-task task clock))
                                (remove-if-not
                                 (lambda (task) (task-should-run-now-p task clock))
                                 tasks))))) ;; FIX: this does consing, may be slow...
         ;; remove dead tasks
         (mapc (lambda (task)
                 (when (not (null task)) ;; if it's nil it means it's not done with yet so we do nothing.
                   (clock-remove task clock)))
               results)
         (sleep (- (dur-time granularity tempo) (- (sc:now) now))))
       ;; FIX: do we need to acquire a lock on the clock's tempo as well?
       (incf beats granularity))))

(defmethod play ((item pbind))
  (clock-add (as-pstream item) *clock*))

(defmethod stop ((item task))
  (clock-remove item))

;; (defparameter *clock* (make-clock 0.9))
