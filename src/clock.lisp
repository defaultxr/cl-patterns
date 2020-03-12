;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.
;; This clock uses the local-time system to calculate the exact time each event should occur. This calculated time is then passed to the relevant backend. Thus there should be no jitter from cl-patterns, in theory.
;; The reason we have a clock at all is so that patterns can be changed while they're playing. When a pattern is played, its events are not all generated immediately; they're generated approximately `*latency*' seconds before they're supposed to be heard.

;; FIX: don't play "expired" events... i.e. if the event should have already ended by the time we get around to playing it, just don't play it. this way, if the user takes too long to select a restart when an error occurs on the clock, the events won't be played all at once.
;; FIX: print a warning after the clock has finished "catching up"... or perhaps print a warning each time the sleep amount is 0?
;; FIX: make the clock's "beat" slot always completely accurate, so it can be referenced from within patterns or the like... or maybe just make the #'beat function default to the current context?

(in-package #:cl-patterns)

(defclass clock ()
  ((beat :initform 0 :documentation "The number of beats that have elapsed since the creation of the clock.")
   (tempo :initarg :tempo :initform 1 :reader tempo :type number :documentation "The tempo of the clock, in beats per second.")
   (tasks :initform nil :documentation "The list of tasks that are running on the clock.")
   (tasks-lock :initform (bt:make-recursive-lock) :documentation "The lock on the tasks to make the clock thread-safe.")
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
  ((item :initarg :item :initform nil :documentation "The actual playing item that the task refers to. Typically this is a pstream or similar.")
   (loop-p :initarg :loop-p :documentation "Whether the task should loop. If left unbound, the task's item's loop-p slot is referred to instead.")
   (start-beat :initarg :start-beat :initform nil :documentation "The beat of the clock when the task started.")
   (clock :initarg :clock :documentation "The clock that the task is running on.")
   (backend-resources :initarg :backend-resources :initform nil :documentation "Resources associated with this task that should be freed by it, i.e. nodes it triggered, buffers it loaded, etc."))
  (:documentation "An item scheduled to be run on the clock."))

(defmethod print-object ((task task) stream)
  (with-slots (item) task
    (print-unreadable-object (task stream :type t)
      (format stream ":ITEM ~s" item))))

(defun absolute-beats-to-timestamp (beats clock)
  "Convert a clock's number of beats to a timestamp. The result is only guaranteed to be accurate if it's greater than the clock's beat-at-tempo slot."
  (with-slots (timestamp-at-tempo beat-at-tempo) clock
    (local-time:timestamp+ timestamp-at-tempo (truncate (* (dur-time (- beats beat-at-tempo) (tempo clock)) 1000000000)) :nsec)))

(defmethod loop-p ((task task))
  (if (slot-boundp task 'loop-p)
      (slot-value task 'loop-p)
      (loop-p (slot-value task 'item))))

(defun task-pattern (task)
  "Attempt to get the pattern that TASK is playing. Returns nil if the pattern couldn't be found.

See also: `pattern-tasks', `clock-tasks'"
  (with-slots (item) task
    (when (slot-boundp item 'source)
      (slot-value item 'source))))

(defun pattern-tasks (pattern &optional (clock *clock*))
  "Attempt to get the tasks that are playing PATTERN.

See also: `task-pattern', `clock-tasks'"
  (remove-if-not (lambda (i)
                   (let ((item (slot-value i 'item)))
                     (or (eq pattern item)
                         (let ((source (slot-value item 'source)))
                           (or (eq pattern source)
                               (when (typep source 'pdef)
                                 (or (and (typep pattern 'pdef)
                                          (eql (pdef-key source) (pdef-key pattern)))
                                     (eq pattern (pdef-pattern source)))))))))
                 (slot-value clock 'tasks)))

(defun playing-pdefs (&optional (clock *clock*))
  "Get a list of the names of all pdefs playing on CLOCK."
  (loop :for task :in (clock-tasks clock)
     :for item = (slot-value task 'item)
     :if (ignore-errors (slot-boundp item 'key))
     :collect (slot-value item 'key)))

(defun make-clock (&optional (tempo 1))
  "Create a clock with a tempo of TEMPO in beats per second (Hz).

To make and start the clock, run `clock-loop' on a new thread, like so:

;; (bt:make-thread (lambda () (clock-loop *clock*)) :name \"cl-patterns clock-loop\")

Alternatively, you can call `clock-process' manually to process N beats on the clock.

See also: `clock-loop', `clock-process'"
  (make-instance 'clock :tempo tempo))

(defmethod print-object ((clock clock) stream)
  (with-slots (tempo beat) clock
    (format stream "#<~s :tempo ~s :beat ~f>" 'clock tempo beat)))

(defun clock-add (item &optional (clock *clock*))
  "Add ITEM to CLOCK's tasks. Generally you don't need to use this directly and would use `play' instead.

See also: `clock-remove', `play'"
  (when (null clock)
    (error "cl-patterns clock is NIL; perhaps try (defparameter *clock* (make-clock)) or (start-clock-loop)"))
  (with-slots (tasks tasks-lock) clock
    (bt:with-recursive-lock-held (tasks-lock)
      (let ((task (make-instance 'task :item item :clock clock :start-beat (next-beat-for-quant (quant item) (slot-value clock 'beat)))))
        (setf tasks (append tasks (list task)))
        task))))

(defun clock-remove (task &optional (clock *clock*)) ;; FIX: can we just add "remove" events to the clock so that removals are automatically synchronized/happen at the right time?
  "Remove TASK from CLOCK's tasks. Generally you don't need to use this directly and would use `stop' or `end' instead.

See also: `clock-add', `stop', `end'"
  (with-slots (tasks tasks-lock) clock
    (bt:with-recursive-lock-held (tasks-lock)
      (setf tasks
            (remove-if
             (lambda (ctask)
               (when-let ((eq (eq ctask task)))
                 (dolist (backend (enabled-backends))
                   (backend-task-removed task backend))
                 eq))
             tasks)))))

(defun clock-tasks (&optional (clock *clock*))
  "Get a list of all tasks running on CLOCK.

See also: `pattern-tasks'"
  (slot-value clock 'tasks))

(defun (setf clock-tasks) (value &optional (clock *clock*))
  (let* ((current (clock-tasks clock))
         (removed (set-difference current value))
         (added (set-difference value current)))
    (dolist (task removed)
      (clock-remove task clock))
    (dolist (task added)
      (clock-add task clock))))

(defun clock-clear-tasks (&optional (clock *clock*))
  "Remove all tasks from CLOCK.

See also: `clock-tasks'"
  (dolist (task (clock-tasks clock))
    (clock-remove task clock)))

(defun clock-process (clock beats)
  "Process any of CLOCK's tasks that occur in the next BEATS beats.

See also: `clock-loop', `clock-tasks', `make-clock'"
  (let* ((sbeat (slot-value clock 'beat))
         (ebeat (+ sbeat beats)))
    (labels ((clock-process-task (task &optional (times 0))
               (with-slots (item start-beat) task
                 (if (or (typep item 'event)
                         (events-after-p item (- sbeat start-beat)))
                     ;; FIX: need to make sure tempo-change events are processed first
                     (progn
                       (dolist (event (if (typep item 'event)
                                          (list item)
                                          (events-in-range item (- sbeat start-beat) (- ebeat start-beat))))
                         (let ((event (event-with-raw-timing event task)))
                           (dolist (event (split-event-by-lists event))
                             (case (event-value event :type)
                               (:tempo-change
                                (with-slots (timestamp-at-tempo tempo beat-at-tempo) clock
                                  (if (and (numberp (event-value event :tempo))
                                           (plusp (event-value event :tempo)))
                                      (setf timestamp-at-tempo (raw-event-value event :timestamp-at-start)
                                            tempo (event-value event :tempo)
                                            beat-at-tempo (raw-event-value event :beat-at-start))
                                      (warn "Tempo change event ~a has invalid :tempo parameter; ignoring." event)))
                                (dolist (backend (backends-for-event event))
                                  (backend-play-event event task backend))
                                nil)
                               (:rest
                                nil)
                               (otherwise
                                (dolist (backend (backends-for-event event))
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
      (bt:with-recursive-lock-held ((slot-value clock 'tasks-lock))
        (dolist (task (slot-value clock 'tasks))
          (restart-case
              (unless (clock-process-task task)
                (clock-remove task clock))
            (skip-event ()
              :report "Skip this event, preserving the task on the clock so it can be run again."
              nil)
            (remove-task ()
              :report "Remove this task from the clock."
              (clock-remove task clock))))))
    (setf (slot-value clock 'beat) ebeat)))

;;; basic clock-loop convenience functionality

(defvar *performance-mode* nil
  "Set to true to enable \"performance mode\". In performance mode, all errors signaled within a task will be caught and a restart invoked automatically to prevent other tasks on the clock from pausing. REMOVE-TASK is the default restart, but another can be specified by setting this variable to its name. When an error occurs in performance mode, a warning is printed, and the task and its stack trace are saved to `*performance-errors*'.")

(defparameter *performance-errors* (list)
  "A list of tasks' errors caught by the clock while `*performance-mode*' is enabled. Each entry in the list is a plist containing the task, the error, and the stack trace.")

(defparameter *performance-errors-lock* (bt:make-lock)
  "The lock on `*performance-errors*' to make it thread-safe.")

(defun clock-loop (clock &key (granularity *latency*)) ;; FIX: make an option to automatically skip "expired" events (i.e. if the clock has to pause due to a condition, then upon resuming, it will skip events that were missed instead of playing them all at once)
  "Convenience method for processing a clock's tasks in a loop.

To run the clock in a new thread, you can call `start-clock-loop'.

See also: `start-clock-loop', `clock-process'"
  (unwind-protect
       (loop
          (if *performance-mode*
              (handler-bind
                  ((error (lambda (e)
                            (bt:with-lock-held (*performance-errors-lock*)
                              (let ((restart (if (member *performance-mode* (list 'remove-task 'skip-event))
                                                 *performance-mode*
                                                 'remove-task)))
                                (warn "Task had error ~s; invoked ~s restart, with state recorded as index ~s in ~s." e restart (length *performance-errors*) '*performance-errors*)
                                (appendf *performance-errors* (list (list :error e :stack (dissect:stack))))
                                (invoke-restart restart))))))
                (clock-process clock granularity))
              (clock-process clock granularity))
          (sleep (max 0
                      (- (local-time:timestamp-difference
                          (absolute-beats-to-timestamp (slot-value clock 'beat) clock)
                          (local-time:now))
                         (/ *latency* 2)))))
    (warn "The clock loop has stopped! You will likely need to create a new clock with (start-clock-loop) in order to play patterns again.")))

(defun start-clock-loop (&key tempo force)
  "Convenience method to make a clock and start its loop in a new thread.

With FORCE, make a new clock and thread even if one already appears to be running.

See also: `clock-loop'"
  (if (or (null *clock*)
          (null (find "cl-patterns clock-loop" (bt:all-threads) :key #'bt:thread-name :test #'string-equal))
          force)
      (progn
        (setf *clock* (make-clock (or tempo 1)))
        (bt:make-thread (lambda () (clock-loop *clock*)) :name "cl-patterns clock-loop"))
      (warn "A clock appears to be running already; doing nothing. Set ~s's ~s argument to true to force the creation of a new clock and thread regardless." 'start-clock-loop 'force))
  *clock*)

;;; play/stop/end methods

(defmethod play ((event event))
  (clock-add event))

(defmethod play ((pattern pattern))
  (clock-add (as-pstream pattern) *clock*))

(defmethod play ((pdef pdef))
  (with-slots (key) pdef
    ;; if there is already a task playing this pdef, we do nothing.
    ;; you can use `launch' instead to force launching a second instance of the pattern.
    (unless (position (pdef-ref-get key :task) (clock-tasks *clock*))
      (let ((task (call-next-method)))
        (when (typep task 'task)
          (pdef-ref-set key :task task)
          task)))))

(defmethod play ((symbol symbol))
  (when-let ((res (lookup-object-for-symbol symbol)))
    (play res)))

(defmethod play ((list list))
  (mapcar 'play list))

(defmethod launch ((item t)) ;; forward to `play' if a more specific method hasn't been defined for a class.
  (play item))

(defmethod launch ((event event))
  (play event))

(defmethod launch ((pattern pattern))
  (play pattern))

(defmethod launch ((pdef pdef))
  (with-slots (key) pdef
    (play (pdef-ref-get key :pattern))))

(defmethod launch ((symbol symbol))
  (launch (pdef symbol)))

(defmethod launch ((list list))
  (mapcar 'launch list))

(defmethod stop ((pdef pdef))
  (with-slots (key) pdef
    (when (pdef-ref-get key :task)
      (stop (pdef-ref-get key :task)))
    (pdef-ref-set key :pstream (as-pstream (pdef-ref-get key :pattern)))
    (pdef-ref-set key :task nil)))

(defmethod stop ((symbol symbol))
  (stop (pdef symbol)))

(defmethod stop ((task task))
  (clock-remove task))

(defmethod stop ((list list))
  (mapcar 'stop list))

(defmethod end ((item t)) ;; forward to `stop' if a more specific method hasn't been defined for a class.
  (stop item))

(defmethod end ((item pdef))
  (with-slots (key) item
    (if (pdef-ref-get key :task)
        (end (pdef-ref-get key :task))
        (warn "pdef ~a has no connected task; try the `stop' method instead." key))))

(defmethod end ((item symbol))
  (end (pdef item)))

(defmethod end ((item task))
  (setf (slot-value item 'loop-p) nil))

(defmethod end ((list list))
  (mapcar 'end list))

(defmethod playing-p ((task task) &optional (clock *clock*))
  (position task (clock-tasks clock)))

(defmethod playing-p ((pdef pdef) &optional (clock *clock*))
  (position pdef (clock-tasks clock)))

(defmethod playing-p ((key symbol) &optional (clock *clock*))
  (position key (playing-pdefs clock)))

(defmethod playing-p ((list list) &optional (clock *clock*))
  (mapcar (lambda (item) (playing-p item clock)) list))
