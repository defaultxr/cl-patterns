;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.

(in-package :cl-patterns)

(defclass clock ()
  ((beats :initform 0)
   (tempo :initform 1 :initarg :tempo :accessor tempo)
   (granularity :initform 1/10 :initarg :granularity)
   (tasks :initform nil)
   (tasks-lock :initform (bt:make-lock))
   (unix-time-at-tempo :initform (unix-time-byulparan)) ;; the unix time that the tempo was last set at.
   (when-tempo-changed :initform 0) ;; what 'beats' was when tempo was last changed.
   ))

(defun unix-time-byulparan () ;; FIX: pull request this to byulparan/scheduler??
  "Get the current unix time in the same format as byulparan/scheduler."
  (let ((now (local-time:now)))
    (+ (local-time:timestamp-to-unix now) (* (local-time:nsec-of now) 1.0d-9))))

(defclass task ()
  ((gensym :initform (gensym))
   (clock :initform *clock* :initarg :clock)
   (item :initform nil :initarg :item)
   (start-time :initform nil :initarg :start-time)
   (next-time :initform nil :initarg :next-time)))

(defparameter *clock* nil)

(defun absolute-beats-to-unix-time (beats clock)
  "Convert a clock's number of beats to a unix time. Number of beats is only guaranteed to be accurate if it's greater than the clock's when-tempo-changed slot."
  (+ (slot-value clock 'unix-time-at-tempo) (dur-time (- beats (slot-value clock 'when-tempo-changed)) (tempo clock))))

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
  (when (null (slot-value task 'next-time)) ;; pstream hasn't started yet.
    (setf (slot-value task 'start-time) (slot-value clock 'beats)
          (slot-value task 'next-time) (slot-value clock 'beats)))
  (let ((nv (next (slot-value task 'item)))
        (nt (slot-value task 'next-time)))
    (if (null nv)
        task ;; if the task is done with, we return it so it can be removed from the clock, else we return nil so nothing happens.
        (progn
          (incf (slot-value task 'next-time) (delta nv))
          (play (combine-events nv (event :unix-time-at-start (absolute-beats-to-unix-time nt clock))))
          (when (< (slot-value task 'next-time) (+ (slot-value clock 'beats) (slot-value clock 'granularity)))
            (clock-process-task task clock))))))

(defun task-should-run-now-p (task clock)
  (let ((item (slot-value task 'item)))
    (if (typep item 'pstream)
        (if (null (slot-value task 'next-time)) ;; if the pstream hasn't started yet...
            (or (= 0 (slot-value item 'quant)) ;; ...it starts when the :quant determines it.
                (< (mod (slot-value clock 'beats) (slot-value item 'quant)) (slot-value clock 'granularity)))
            (< (slot-value task 'next-time) (+ (slot-value clock 'beats) (slot-value clock 'granularity))))
        t)))

(defun tick (clock)
  (with-slots (tasks tasks-lock granularity tempo beats) clock
    (loop :do
       (let ((results (bt:with-lock-held (tasks-lock)
                        (mapcar (lambda (task) (clock-process-task task clock))
                                (remove-if-not
                                 (lambda (task) (task-should-run-now-p task clock))
                                 tasks))))) ;; FIX: this does consing, may be slow...
         ;; remove dead tasks
         (mapc (lambda (task)
                 (when (not (null task)) ;; if it's nil it means it's not done with yet so we do nothing.
                   (clock-remove task clock)))
               results)
         (sleep (max 0 (- (absolute-beats-to-unix-time (+ beats granularity) clock) (unix-time-byulparan)))))
       ;; FIX: do we need to acquire a lock on the clock's tempo as well?
       (incf beats granularity))))

(defmethod play ((item pattern))
  (clock-add (as-pstream item) *clock*))

(defmethod stop ((item task))
  (clock-remove item))
