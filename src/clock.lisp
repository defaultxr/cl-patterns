;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.
;; This clock uses the local-time system to calculate the exact time each event should occur. This calculated time is then passed to the relevant backend. Thus there should be no jitter from cl-patterns, in theory.
;; The reason we have a clock at all is so that patterns can be changed while they're playing. When a pattern is played, its events are not all generated immediately; they're generated approximately `*latency*' seconds before they're supposed to be heard.

;; FIX: don't play "expired" events... i.e. if the event should have already ended by the time we get around to playing it, just don't play it. this way, if the user takes too long to select a restart when an error occurs on the clock, the events won't be played all at once.
;; FIX: print a warning after the clock has finished "catching up"... or perhaps print a warning each time the sleep amount is 0?
;; FIX: make the clock's "beat" slot always completely accurate, so it can be referenced from within patterns or the like... or maybe just make the #'beat function default to the current context?
;; FIX: trigger warning or error if something is played when the clock is stopped (or when the clock stops?)

(in-package :cl-patterns)

(defclass clock ()
  ((beat :initform 0 :documentation "The number of beats that have elapsed since the creation of the clock.")
   (tempo :initarg :tempo :initform 1 :reader tempo :documentation "The tempo of the clock, in beats per second.")
   (tasks :initform nil :documentation "The list of tasks that are running on the clock.")
   (tasks-lock :initform (bt:make-lock) :documentation "The lock on the tasks to make the clock thread-safe.")
   (timestamp-at-tempo :initform (local-time:now) :documentation "The local-time timestamp when the tempo was last changed.")
   (beat-at-tempo :initform 0 :documentation "The number of beats on the clock when the tempo was last changed."))
  (:documentation "A musical time-based clock defining a tempo and pulse that its tasks synchronize to."))

(defmethod beat ((clock clock))
  (slot-value clock 'beat))

(defmethod (setf tempo) (value (item clock))
  (clock-add (event :type :tempo-change :tempo value) item))

(defmethod tempo ((number number))
  ;; convenience method to quickly set the tempo of the default clock in bpm
  (setf (tempo *clock*) number))

(defclass task ()
  ((item :initarg :item :initform nil :documentation "The actual playing item that the task refers to.")
   (loop-p :initarg :loop-p :documentation "Whether the task should loop. If left unbound, the task's item's loop-p slot is referred to instead.")
   (start-beat :initarg :start-beat :initform nil :documentation "The beat of the clock when the task started.")
   (clock :initarg :clock :documentation "The clock that the task is running on."))
  (:documentation "An item scheduled to be run on the clock."))

(defun absolute-beats-to-timestamp (beats clock)
  "Convert a clock's number of beats to a timestamp. The result is only guaranteed to be accurate if it's greater than the clock's beat-at-tempo slot."
  (with-slots (timestamp-at-tempo beat-at-tempo) clock
    (local-time:timestamp+ timestamp-at-tempo (truncate (* (dur-time (- beats beat-at-tempo) (tempo clock)) 1000000000)) :nsec)))

(defmethod loop-p ((task task))
  (if (slot-boundp task 'loop-p)
      (slot-value task 'loop-p)
      (slot-value (slot-value task 'item) 'loop-p)))

(defun make-clock (&optional (tempo 1) tasks)
  "Create a clock.

To start the clock, run `clock-loop' on a new thread, like so:

;; (bt:make-thread (lambda () (clock-loop *clock*)) :name \"cl-patterns clock-loop\")

Alternatively, you can call `clock-process' manually to process N beats on the clock."
  (let ((clock (make-instance 'clock :tempo tempo)))
    (dolist (task tasks)
      (clock-add task clock))
    clock))

(defmethod print-object ((clock clock) stream)
  (with-slots (tempo beat) clock
    (format stream "#<~s :tempo ~s :beat ~f>" 'clock tempo beat)))

(defun clock-add (item &optional (clock *clock*))
  "Add ITEM to CLOCK's tasks."
  (with-slots (tasks tasks-lock) clock
    (bt:with-lock-held (tasks-lock)
      (let ((task (make-instance 'task :item item :clock clock :start-beat (next-beat-for-quant (quant item) (slot-value clock 'beat)))))
        (setf tasks (append tasks (list task)))
        task))))

(defun clock-remove (task &optional (clock *clock*))
  "Remove TASK from CLOCK's tasks."
  (with-slots (tasks tasks-lock) clock
    (bt:with-lock-held (tasks-lock)
      (setf tasks
            (remove-if
             (lambda (ctask)
               (alexandria:when-let ((eq (eql ctask task)))
                 (dolist (backend *enabled-backends*)
                   (backend-task-removed task backend))
                 eq))
             tasks)))))

(defun clock-clear-tasks (&optional (clock *clock*))
  "Remove all tasks from CLOCK."
  (dolist (task (slot-value clock 'tasks))
    (clock-remove task clock)))

(defun clock-process (clock beats)
  "Process any of CLOCK's tasks that occur in the next BEATS beats."
  (let* ((sbeat (slot-value clock 'beat))
         (ebeat (+ sbeat beats)))
    (labels ((clock-process-task (task &optional (times 0))
               (with-slots (item start-beat) task
                 (if (or (typep item 'event)
                         (events-after-p item (- sbeat start-beat)))
                     ;; FIX: need to make sure tempo-change events are processed first
                     (progn
                       (dolist (ev (if (typep item 'event)
                                       (alexandria:ensure-list item)
                                       (events-in-range item (- sbeat start-beat) (- ebeat start-beat))))
                         (let* ((beat (+ start-beat (or (event-value ev :beat) 0)))
                                (ts (absolute-beats-to-timestamp beat clock))
                                (event (combine-events ev (event :timestamp-at-start ts))))
                           (case (event-value event :type)
                             (:tempo-change
                              (with-slots (timestamp-at-tempo tempo beat-at-tempo) clock
                                (if (and (numberp (event-value event :tempo))
                                         (plusp (event-value event :tempo)))
                                    (setf timestamp-at-tempo ts
                                          tempo (event-value event :tempo)
                                          beat-at-tempo beat)
                                    (warn "Tempo change event ~a has invalid :tempo parameter; ignoring." event)))
                              nil)
                             (:rest
                              nil)
                             (otherwise
                              (dolist (backend (enabled-backends))
                                (when (backend-plays-event-p event backend)
                                  (backend-play-event event task backend)))))))
                       (if (typep item 'event)
                           nil
                           task))
                     (unless (typep item 'event)
                       (if (loop-p task)
                           (let ((last-output (last-output item)))
                             (setf item (as-pstream (pdef (slot-value item 'key)))) ;; FIX: should this just call a "reset" method or the like?
                             ;; FIX: add loop-quant for specifying a quant for when the loop is allowed to start playing next
                             (setf start-beat (+ start-beat
                                                 (or (beat last-output) 0)
                                                 (or (dur last-output) 0)))
                             (if (>= times 32)
                                 (progn
                                   (warn "Task ~s yielded NIL 32 times in a row; removing from clock to avoid locking into an infinite loop." task)
                                   nil)
                                 (clock-process-task task (1+ times))))
                           nil))))))
      (dolist (task (slot-value clock 'tasks))
        (restart-case
            (unless (clock-process-task task)
              (clock-remove task clock))
          (skip-event ()
            :report "Skip this event, preserving the task on the clock so it can be run again."
            nil)
          (remove-task ()
            :report "Remove this task from the clock."
            (clock-remove task clock)))))
    (incf (slot-value clock 'beat) beats)))

;;; basic clock-loop convenience functionality

(defvar *performance-mode* nil
  "Set to true to enable \"performance mode\". In performance mode, all errors signaled within a task will be caught and a restart invoked automatically to prevent other tasks on the clock from pausing. REMOVE-TASK is the default restart, but another can be specified by setting this variable to its name. When an error occurs in performance mode, a warning is printed, and the task and its stack trace are saved to `*performance-errors*'.")

(defparameter *performance-errors* (list)
  "A list of tasks' errors caught by the clock while `*performance-mode*' is enabled. Each entry in the list is a plist containing the task, the error, and the stack trace.")

(defparameter *performance-errors-lock* (bt:make-lock)
  "The lock on `*performance-errors*' to make it thread-safe.")

(defun clock-loop (clock &key (granularity *latency*)) ;; FIX: make an option to automatically skip "expired" events (i.e. if the clock has to pause due to a condition, then upon resuming, it will skip events that were missed instead of playing them all at once)
  "Convenience method for processing a clock's tasks in a loop.

To run the clock in a new thread, you can do something like this:

;; (bt:make-thread (lambda () (clock-loop *clock*)) :name \"cl-patterns clock-loop\")

See also: `clock-process'"
  (loop
     (if *performance-mode*
         (handler-bind
             ((error (lambda (e)
                       (bt:with-lock-held (*performance-errors-lock*)
                         (let ((restart (if (member *performance-mode* (list 'remove-task 'skip-event))
                                            *performance-mode*
                                            'remove-task)))
                           (warn "Task had error ~s; invoked ~s restart, with state recorded as index ~s in ~s." e restart (length *performance-errors*) '*performance-errors*)
                           (alexandria:appendf *performance-errors* (list (list :error e :stack (dissect:stack))))
                           (invoke-restart restart))))))
           (clock-process clock granularity))
         (clock-process clock granularity))
     (sleep (max 0
                 (- (local-time:timestamp-difference
                     (absolute-beats-to-timestamp (slot-value clock 'beat) clock)
                     (local-time:now))
                    (/ *latency* 2))))))

;;; play/stop/end methods

(defmethod play ((event event))
  (clock-add event))

(defmethod play ((pattern pattern))
  (clock-add (as-pstream pattern) *clock*))

(defmethod play ((pdef pdef)) ;; prevent pdef from playing twice if it's already playing on the clock. you can do (play (pdef-ref-get KEY :pattern)) to bypass this and play it again anyway. (FIX: make a fork method?)
  (with-slots (key) pdef
    (unless (position (pdef-ref-get key :task)
                      (slot-value *clock* 'tasks))
      (let ((task (call-next-method)))
        (when (typep task 'task)
          (pdef-ref-set key :task task)
          task)))))

(defmethod play ((symbol symbol)) ;; FIX: possibly make this work for proxies & other stuff as well
  (play (pdef symbol)))

(defmethod stop ((pdef pdef))
  (with-slots (key) pdef
    (when (pdef-ref-get key :task)
      (stop (pdef-ref-get key :task)))
    (pdef-ref-set key :pstream (as-pstream (pdef-ref-get key :pattern)))
    (pdef-ref-set key :task nil)))

(defmethod stop ((symbol symbol)) ;; we assume they meant the pdef with that symbol as name.
  (stop (pdef symbol)))

(defmethod stop ((task task))
  (clock-remove task))

(defmethod end ((item pdef))
  (with-slots (key) item
    (if (pdef-ref-get key :task)
        (end (pdef-ref-get key :task))
        (warn "pdef ~a has no connected task; try the `stop' method instead." key))))

(defmethod end ((item symbol))
  (end (pdef item)))

(defmethod end ((item task))
  (setf (slot-value item 'loop-p) nil))

(defun pdefs-playing (&optional (clock *clock*))
  "Get a list of the names of all pdefs playing on CLOCK."
  (loop :for i :in (slot-value clock 'tasks)
     :collect (slot-value (slot-value i 'item) 'key)))

(defmethod playing-p ((task task) &optional (clock *clock*))
  (position task (slot-value clock 'tasks)))

(defmethod playing-p ((pdef pdef) &optional (clock *clock*))
  (position pdef (slot-value clock 'tasks)))

(defmethod playing-p ((key symbol) &optional (clock *clock*))
  (position key (pdefs-playing clock)))
