;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.
;; This clock uses the local-time system to calculate the exact time each event should occur. This calculated time is then passed to the relevant backend. Thus there should be no jitter from cl-patterns, in theory.
;; The reason we have a clock at all is so that patterns can be changed while they're playing. When a pattern is played, its events are not all generated immediately; they're generated approximately `*latency*' seconds before they're supposed to be heard.

;; FIX: don't play "expired" events... i.e. if the event should have already ended by the time we get around to playing it, just don't play it. this way, if the user takes too long to select a restart when an error occurs on the clock, the events won't be played all at once.
;; FIX: print a warning after the clock has finished "catching up"... or perhaps print a warning each time the sleep amount is 0?
;; FIX: make the clock's "beats" slot always completely accurate, so it can be referenced from within patterns or the like.

(in-package :cl-patterns)

(defvar *performance-mode* nil
  "Whether \"performance mode\" is enabled. In performance mode, all errors signaled within a task will be caught and the task will be removed automatically, to prevent other tasks on the clock from pausing. The task and its stack trace are saved to `*performance-errors*'.")

(defparameter *performance-errors* (list)
  "A list of tasks' errors caught by the clock while `*performance-mode*' is enabled. Each entry in the list is a plist containing the task, the error, and the stack trace.")

(defparameter *performance-errors-lock* (bt:make-lock)
  "The lock on `*performance-errors*' to make it thread-safe.")

(defgeneric tempo (object)
  (:documentation "Get the tempo of OBJECT in beats per second. If OBJECT is a number, set the tempo of `*clock*' to that number."))

(defclass clock ()
  ((beats :initform 0 :documentation "The number of beats that have elapsed since the creation of the clock.")
   (tempo :initarg :tempo :initform 1 :reader tempo :documentation "The tempo of the clock, in beats per second.")
   (granularity :initarg :granularity :initform 1/10 :documentation "How often the clock \"wakes up\" to process events in each beat. You generally will not need to adjust this.")
   (tasks :initform nil :documentation "The list of tasks that are running on the clock.")
   (tasks-lock :initform (bt:make-lock) :documentation "The lock on the tasks to make the clock thread-safe.")
   (timestamp-at-tempo :initform (local-time:now) :documentation "The local-time timestamp when the tempo was last changed.")
   (beat-at-tempo :initform 0 :documentation "The number of beats on the clock when the tempo was last changed.")
   (thread :documentation "The bordeaux-threads thread that is running the clock.")))

(defmethod (setf tempo) (value (item clock))
  (clock-add (event :type :tempo-change :tempo value) item))

(defclass task ()
  ((gensym :initform (gensym))
   (clock :initarg :clock :initform *clock*)
   (item :initarg :item :initform nil)
   (start-time :initarg :start-time :initform nil)
   (next-time :initarg :next-time :initform nil)
   (nodes :initarg :nodes :initform (list))))

(defun absolute-beats-to-timestamp (beats clock)
  "Convert a clock's number of beats to a timestamp. The result is only guaranteed to be accurate if it's greater than the clock's beat-at-tempo slot."
  (with-slots (timestamp-at-tempo beat-at-tempo) clock
    (local-time:timestamp+ timestamp-at-tempo (truncate (* (dur-time (- beats beat-at-tempo) (tempo clock)) 1000000000)) :nsec)))

(defun make-clock (&optional (tempo 1) tasks)
  "Create a clock. The clock automatically starts running in a new thread."
  (let ((clock (make-instance 'clock :tempo tempo)))
    (mapc (lambda (task) (clock-add task clock)) tasks)
    (setf (slot-value clock 'thread) (bt:make-thread (lambda () (clock-loop clock)) :name "cl-patterns clock"))
    clock))

(defmethod print-object ((clock clock) stream)
  (with-slots (tempo beats) clock
    (format stream "#<~s :tempo ~f :beats ~f>" 'clock tempo beats)))

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
               (let ((nodes (slot-value task 'nodes))
                     (eq (eq (slot-value task 'gensym) (slot-value item 'gensym))))
                 (when (and nodes eq)
                   (let ((backend (car (last (remove-if #'null (mapcar #'which-backend-for-event (slot-value (slot-value task 'item) 'history)))))))
                     (if backend
                         (map nil (getf (getf *backends* backend) :release) nodes)
                         (error "No backends found for the events from task ~a; unable to release ~d nodes!" task (length nodes)))))
                 eq))
             tasks)))))

(defun clock-clear-tasks (&optional (clock *clock*))
  "Remove all tasks from CLOCK."
  (with-slots (tasks tasks-lock) clock
    (bt:with-lock-held (tasks-lock)
      (setf tasks nil))))

(defun clock-process-task (task clock) ;; FIX: only remove patterns if they've returned NIL as their first output 4 times in a row.
  "Process TASK on CLOCK. This function processes meta-events like tempo changes, etc, and everything else is sent to to `*event-output-function*'."
  (with-slots (beats tempo granularity timestamp-at-tempo beat-at-tempo) clock
    (when (null (slot-value task 'next-time)) ;; pstream hasn't started yet.
      (setf (slot-value task 'start-time) beats
            (slot-value task 'next-time) beats))
    (restart-case
        (let* ((item (slot-value task 'item))
               (nv (next item)))
          (when (and (null nv) (loop-p item)) ;; auto-reschedule patterns that should loop.
            (setf (slot-value task 'item) (as-pstream (pdef (slot-value item 'key))))
            (setf nv (next (slot-value task 'item)))
            (setf item (slot-value task 'item)))
          (unless (null nv)
            (let ((type (event-value nv :type)))
              ;; actually "play" the event
              (case type
                (:tempo-change
                 (if (and (numberp (event-value nv :tempo))
                          (plusp (event-value nv :tempo)))
                     (setf timestamp-at-tempo (absolute-beats-to-timestamp beats clock)
                           tempo (event-value nv :tempo)
                           beat-at-tempo beats)
                     (warn "Tempo change event ~a has invalid :tempo parameter; ignoring." nv)))
                (:rest
                 nil)
                (otherwise
                 (let* ((event (combine-events nv (event :timestamp-at-start (absolute-beats-to-timestamp (slot-value task 'next-time) clock))))
                        (backend (which-backend-for-event event)))
                   (if backend
                       (funcall (getf (getf *backends* backend) :play) event task)
                       (warn "No backend claims to handle ~s; skipping." nv)))))
              ;; play the next event from this task if it occurs before the clock's next "wakeup time"
              (unless (position type '(:tempo-change))
                (incf (slot-value task 'next-time) (event-value nv :delta))
                (when (< (slot-value task 'next-time) (+ beats granularity))
                  (clock-process-task task clock)))))
          ;; if the task is done with or isn't a pstream, we return it so it can be removed from the clock, else we return nil so nothing happens
          (when (or (null nv) (not (typep item 'pstream)))
            task))
      (skip-event ()
        :report "Skip this event, preserving the task on the clock so it can be run again."
        nil)
      (remove-task ()
        :report "Remove this task from the clock."
        task))))

(defun clock-safe-process-task (task clock)
  "Like `clock-process-task', process TASK on CLOCK, but automatically remove the task from the clock if it has any errors, recording the error and stack to `*performance-errors*'."
  (handler-bind
      ((error (lambda (e)
                (bt:with-lock-held (*performance-errors-lock*)
                  (warn "Task ~s had error ~s; removed from clock, with state recorded as index ~s in ~s" task e (length *performance-errors*) '*performance-errors*.)
                  (alexandria:appendf *performance-errors* (list (list :task task :error e :stack (dissect:stack))))
                  (invoke-restart 'remove-task)))))
    (clock-process-task task clock)))

(defun clock-tasks-names (&optional (clock *clock*))
  "Get a list of names of pdefs currently scheduled on CLOCK."
  (mapcar
   (lambda (task) (slot-value (slot-value task 'item) 'key))
   (slot-value clock 'tasks)))

(defun task-should-run-now-p (task clock)
  "Determine whether TASK should be run now according to CLOCK and TASK's quant, etc."
  (destructuring-bind (quant &optional (phase 0) (offset 0)) (alexandria:ensure-list (quant (slot-value task 'item)))
    (declare (ignore offset))
    (if (typep (slot-value task 'item) 'pstream)
        (if (null (slot-value task 'next-time)) ;; if the pstream has already started, return t
            (or (= 0 quant) ;; a quant of 0 means run immediately without waiting for a specific beat.
                (< (abs (- (mod (slot-value clock 'beats) quant) phase))
                   (slot-value clock 'granularity)))
            (< (slot-value task 'next-time) (+ (slot-value clock 'beats) (slot-value clock 'granularity))))
        t)))

(defun clock-loop (clock)
  (loop (tick clock)))

(defun tick (clock)
  (with-slots (tasks tasks-lock granularity tempo beats) clock
    (let ((results (bt:with-lock-held (tasks-lock) ;; FIX: if a tempo-change occurs at the same time as a regular event, make sure the tempo-change is processed first
                     (mapcar (lambda (task)
                               (if *performance-mode*
                                   (clock-safe-process-task task clock)
                                   (clock-process-task task clock)))
                             (remove-if-not
                              (lambda (task) (task-should-run-now-p task clock))
                              tasks)))))
      ;; remove dead tasks
      (mapc (lambda (task)
              (when (not (null task)) ;; if it's nil it means it's not done with yet so we do nothing.
                (clock-remove task clock)))
            results)
      (sleep (max 0 (- (local-time:timestamp-difference (absolute-beats-to-timestamp (+ beats granularity) clock)
                                                        (local-time:now))
                       (/ *latency* 2)))))
    (incf beats granularity)))

(defgeneric play (item)
  (:documentation "Play an item (typically an event or pattern) according to the current *event-output-function*."))

(defgeneric stop (item)
  (:documentation "Stop an item (typically a playing task or pdef) according to the current *event-output-function*."))

(defgeneric play-or-stop (item)
  (:documentation "Play an item or stop it if it is already playing. Returns T if the item will start playing, returns NIL if it will stop playing."))

(defmethod play ((item event))
  (clock-add item))

(defmethod play ((pattern pdef)) ;; prevent pdef from playing twice if it's already playing on the clock. you can do (play (pdef-ref-get KEY :pattern)) to bypass this and play it again anyway. (FIX: make a fork method?)
  (with-slots (key) pattern
    (unless (position (pdef-ref-get key :task)
                      (slot-value *clock* 'tasks))
      (let ((task (call-next-method)))
        (when (typep task 'task)
          (pdef-ref-set key :task task)
          task)))))

(defmethod stop ((pattern pdef))
  (with-slots (key) pattern
    (when (pdef-ref-get key :task)
      (stop (pdef-ref-get key :task)))
    (pdef-ref-set key :pstream (as-pstream (pdef-ref-get key :pattern)))
    (pdef-ref-set key :task nil)))

;; FIX: in the future, make these two methods work for proxies & other stuff as well...
(defmethod play ((pattern symbol)) ;; we assume they meant the pdef with that symbol as name.
  (play (pdef pattern)))

(defmethod stop ((pattern symbol)) ;; we assume they meant the pdef with that symbol as name.
  (stop (pdef pattern)))

(defmethod play ((item pattern))
  (clock-add (as-pstream item) *clock*))

(defmethod stop ((item task))
  (clock-remove item))

(defmethod play-or-stop ((item pattern)) ;; if it's a regular pattern, we can't tell if it should stop because it has no 'key'... so we always play it.
  (play item)
  t)

(defmethod play-or-stop ((item pdef))
  (play-or-stop (slot-value item 'key)))

(defmethod play-or-stop ((item symbol))
  (let ((playing (position item (clock-tasks-names))))
    (if playing
        (progn (stop item) nil)
        (progn (play item) t))))

(defun pdefs-playing (&optional (clock *clock*))
  "Get a list of the names of all pdefs playing on CLOCK."
  (loop :for i :in (slot-value clock 'tasks)
     :collect (slot-value (slot-value i 'item) 'key)))
