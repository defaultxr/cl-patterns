;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.
;; This clock uses the local-time system to calculate the exact time each event should occur. This calculated time is then passed to the relevant backend. Thus there should be no jitter from cl-patterns, in theory.
;; The reason we have a clock at all is so that patterns can be changed while they're playing. When a pattern is played, its events are not all generated immediately; they're generated approximately `*latency*' seconds before they're supposed to be heard.

;; FIX: investigate syncing to internal time or time of day instead of local-time? https://github.com/jamieforth/osc/blob/79d25ca4e0a4a04135b6bc56231c6b9bb058f1d4/osc.lisp#L279

(in-package #:cl-patterns)

(defclass clock ()
  ((beat :initform 0 :accessor beat :type number :documentation "The number of beats that have elapsed since the creation of the clock.")
   (tempo :initarg :tempo :initform 1 :reader tempo :type number :documentation "The tempo of the clock, in beats per second.")
   (tasks :initform nil :documentation "The list of tasks that are running on the clock.")
   (tasks-lock :initform (bt:make-recursive-lock) :documentation "The lock on the tasks to make the clock thread-safe.")
   (timestamp-at-tempo :initform (local-time:now) :documentation "The local-time timestamp when the tempo was last changed.")
   (beat-at-tempo :initform 0 :documentation "The number of beats on the clock when the tempo was last changed.")
   (play-expired-events :initarg :play-expired-events :initform nil :documentation "If T, always play events, even if their scheduled time has already passed. If NIL, silently skip these events. If :WARN, print a warning for each event skipped."))
  (:documentation "A musical time-based clock defining a tempo and pulse that its tasks synchronize to."))

(defmethod real-beat ((clock clock))
  "Get the \"real beat\" of the clock; i.e. compute what the beat number should actually be at this instant in time (whereas the beat slot for the clock is quantized to the clock's granularity).

Note that this function will likely be removed in the future with improvements to the clock, so you should expect to eventually have to update code depending on it.

See also: `beat'"
  (with-slots (tempo timestamp-at-tempo beat-at-tempo) clock
    (+ beat-at-tempo (* tempo (local-time:timestamp-difference (local-time:now) timestamp-at-tempo)))))

(defmethod (setf tempo) (value (item clock))
  (clock-add (as-pstream (event :type :tempo :tempo value)) item))

(defmethod tempo ((number number))
  ;; convenience method to quickly set the tempo of the default clock
  ;; i.e. (tempo 110/60) sets *clock*'s tempo to 110 BPM
  (setf (tempo *clock*) number))

(defmethod tempo ((symbol symbol))
  (tempo (lookup-object-for-symbol symbol)))

(defclass task ()
  ((item :initarg :item :initform nil :accessor task-item :documentation "The actual playing item that the task refers to. Typically this is a pstream or similar.")
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

(defun event-with-raw-timing (event task)
  "Get an event like EVENT but with the :BEAT-AT-START and :TIMESTAMP-AT-START keys added for backends."
  (with-slots (item clock start-beat) task
    (let* ((tempo (tempo clock))
           (play-quant (play-quant event))
           (e-beat (beat event))
           (beat (+ (if (event-p item)
                        (if e-beat
                            e-beat
                            (next-beat-for-quant play-quant (beat *clock*)))
                        (+ start-beat e-beat))
                    (time-dur (or (raw-event-value event :latency) *latency*)
                              tempo)
                    (time-dur (or (raw-event-value event :timing-offset) 0)
                              tempo)
                    (if (> (length play-quant) 2)
                        (nth 2 play-quant)
                        0))))
      (combine-events event
                      (event :beat-at-start beat
                             :timestamp-at-start (absolute-beats-to-timestamp beat clock))))))

(defmethod beat ((task task))
  (beat (task-item task)))

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
  "Attempt to get the tasks on CLOCK that are playing PATTERN.

See also: `task-pattern', `clock-tasks'"
  (let ((pattern (if (symbolp pattern)
                     (pdef pattern)
                     pattern)))
    (remove-if-not (lambda (i)
                     (with-slots (item) i
                       (or (eq pattern item)
                           (with-slots (source) item
                             (or (eq pattern source)
                                 (when-let ((source-key (ignore-errors (pdef-key source))))
                                   (eql source-key (pdef-key pattern)))
                                 (when (typep source 'pdef)
                                   (eq pattern (pdef-pattern source))))))))
                   (slot-value clock 'tasks))))

(defun playing-pdefs (&optional (clock *clock*))
  "Get a list of the names of all pdefs playing on CLOCK.

See also: `playing-nodes', `playing-p'"
  (loop :for task :in (clock-tasks clock)
        :for item := (slot-value task 'item)
        :if (ignore-errors (slot-boundp item 'key))
          :collect (slot-value item 'key)))

(defun playing-nodes (&optional backend)
  "Get a list of all nodes on BACKEND that are currently playing. Without BACKEND, get all playing nodes on all backends.

See also: `playing-pdefs', `playing-p'"
  (if backend
      (backend-all-nodes backend)
      (apply #'append (mapcar #'playing-nodes (enabled-backends)))))

(defun make-clock (&optional (tempo 1) &key play-expired-events)
  "Create a clock with a tempo of TEMPO in beats per second (Hz).

To make and start the clock, run `clock-loop' on a new thread, like so:

;; (bt:make-thread (lambda () (clock-loop *clock*)) :name \"cl-patterns clock-loop\")

Alternatively, you can call `clock-process' manually to process N beats on the clock.

See also: `clock-loop', `clock-process'"
  (make-instance 'clock :tempo tempo
                        :play-expired-events play-expired-events))

(defmethod print-object ((clock clock) stream)
  (with-slots (tempo beat) clock
    (format stream "#<~s :tempo ~s :beat ~f>" 'clock tempo beat)))

(defun clock-add (item &optional (clock *clock*))
  "Add ITEM (usually a `pstream') to CLOCK's tasks. Generally you don't need to use this directly and would use `play' instead.

See also: `clock-remove', `play'"
  (when (null clock)
    (error "cl-patterns clock is NIL; perhaps try (start-clock-loop) or (defparameter *clock* (make-clock))"))
  (with-slots (tasks tasks-lock) clock
    (bt:with-recursive-lock-held (tasks-lock)
      (let ((task (make-instance 'task :item item :clock clock :start-beat (next-beat-for-quant (play-quant item) (slot-value clock 'beat)))))
        (push task tasks)
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

(defgeneric clock-process-event (clock task event type)
  (:documentation "Process EVENT on CLOCK. TASK is the associated task, and TYPE is the event type."))

(defmethod clock-process-event (clock task event (type (eql :tempo)))
  (with-slots (timestamp-at-tempo tempo beat-at-tempo) clock
    (if (and (numberp (event-value event :tempo))
             (plusp (event-value event :tempo)))
        (let* ((backends (event-backends event))
               (timestamps (loop :for backend :in backends
                                 :collect (list (car (backend-timestamps-for-event event task backend)) backend))))
          (setf timestamp-at-tempo (raw-event-value event :timestamp-at-start)
                tempo (event-value event :tempo)
                beat-at-tempo (raw-event-value event :beat-at-start))
          (dolist (timestamp timestamps)
            (apply 'backend-tempo-change-at clock timestamp)))
        (warn "Tempo change event ~a has invalid :tempo parameter; ignoring." event))))

(defmethod clock-process-event (clock task event (type (eql :rest)))
  nil)

(defmethod clock-process-event (clock task event type)
  (dolist (backend (event-backends event))
    (backend-play-event event task backend)))

(defun can-swap-now-p (pstream)
  "Whether PSTREAM can swap to its new definition, based on `end-quant', `end-condition', and `ended-p'."
  (or (when (plusp (slot-value pstream 'number))
        (when-let ((end-quant (end-quant pstream))
                   (beat (beat *clock*)))
          ;; (break "eq ~s b ~s nbfq ~s eq ~s" end-quant beat (next-beat-for-quant end-quant beat) (= beat (next-beat-for-quant end-quant beat)))
          (= beat (next-beat-for-quant end-quant beat))))
      (when (end-condition pstream)
        (funcall (end-condition pstream) pstream))
      (ended-p pstream)))

(defun clock-process (clock beats)
  "Process tasks on CLOCK for the next BEATS beats.

See also: `clock-loop', `clock-tasks', `make-clock'"
  (bt:with-recursive-lock-held ((slot-value clock 'tasks-lock))
    (let ((*clock* clock)
          (clock-start-beat (beat clock))
          (clock-end-beat (+ (beat clock) beats))
          (retries 0)
          (prev-task nil))
      (loop
        :until (or (>= (beat *clock*) clock-end-beat)
                   (null (clock-tasks clock)))
        :for tasks := (remove-if-not (lambda (task)
                                       (< (+ (slot-value task 'start-beat) (beat task)) clock-end-beat))
                                     (clock-tasks clock))
        :for task := (most #'< tasks :key (lambda (task) (+ (slot-value task 'start-beat) (beat task))))
        :for item := (and task (task-item task))
        :do (setf retries (if (eq task prev-task) (1+ retries) 0)
                  prev-task task)
        :if (null item)
          :do (loop-finish)
        :if (>= retries 32)
          :do (warn "Task ~s yielded NIL 32 times in a row; removing from clock to avoid locking into an infinite loop." task)
              (clock-remove task)
        :else
          :if (or (and (typep item 'pdef-pstream)
                       (not (eq (slot-value item 'pattern) (pdef-pattern (pdef-key item))))
                       (can-swap-now-p item))
                  (and (can-swap-now-p item)
                       (not (loop-p task)))
                  (ended-p item))
            :do (if (not (loop-p task))
                    (clock-remove task)
                    (let ((prev-item-beat (beat item))
                          (prev-start-beat (slot-value task 'start-beat)))
                      (setf (task-item task) (as-pstream (slot-value item 'source))
                            (slot-value task 'start-beat) (+ prev-start-beat prev-item-beat))))
        :else
          :do (restart-case
                  (progn
                    (setf (beat clock) (+ (slot-value task 'start-beat) (beat task)))
                    (when-let ((event (next item)))
                      (dolist (event (split-event-by-lists (event-with-raw-timing event task)))
                        (if (or (local-time:timestamp>= (event-value event :timestamp-at-start) (local-time:now))
                                (eql t (slot-value clock 'play-expired-events)))
                            (clock-process-event clock task event (event-value event :type))
                            (when (eql :warn (slot-value clock 'play-expired-events))
                              (warn "Clock skipped expired event ~s from task ~s" event task))))))
                (skip-event ()
                  :report "Skip this event, preserving the task on the clock so it can be run again."
                  nil)
                (remove-task ()
                  :report "Remove this task from the clock."
                  (clock-remove task clock))))
      (setf (beat clock) clock-end-beat))))

;;; basic clock-loop convenience functionality

(defvar *performance-mode* nil
  "Set to true to enable \"performance mode\". In performance mode, all errors signaled within a task will be caught and a restart invoked automatically to prevent other tasks on the clock from pausing. REMOVE-TASK is the default restart, but another can be specified by setting this variable to its name. When an error occurs in performance mode, a warning is printed, and the task and its stack trace are saved to `*performance-errors*'.")

(defparameter *performance-errors* (list)
  "A list of tasks' errors caught by the clock while `*performance-mode*' is enabled. Each entry in the list is a plist containing the task, the error, and the stack trace.")

(defparameter *performance-errors-lock* (bt:make-lock)
  "The lock on `*performance-errors*' to make it thread-safe.")

(defun clock-loop (clock &key (granularity *latency*))
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
                               (warn "Task had error ~s; invoked ~s restart, with state recorded ~s." e restart '*performance-errors*)
                               (push (list (list :error e :stack (dissect:stack))) *performance-errors*)
                               (invoke-restart restart))))))
               (clock-process clock granularity))
             (clock-process clock granularity))
         (sleep (max 0
                     (- (local-time:timestamp-difference
                         (absolute-beats-to-timestamp (slot-value clock 'beat) clock)
                         (local-time:now))
                        (/ *latency* 2)))))
    (warn "The clock loop has stopped! You will likely need to create a new clock with (start-clock-loop) in order to play patterns again.")))

(defun start-clock-loop (&key tempo force play-expired-events)
  "Convenience method to make a clock and start its loop in a new thread.

With FORCE, make a new clock and thread even if one already appears to be running.

See also: `clock-loop'"
  (if (or (null *clock*)
          (null (find "cl-patterns clock-loop" (bt:all-threads) :key #'bt:thread-name :test #'string-equal))
          force)
      (progn
        (setf *clock* (make-clock (or tempo 1) :play-expired-events play-expired-events))
        (bt:make-thread (lambda () (clock-loop *clock*)) :name "cl-patterns clock-loop"))
      (warn "A clock appears to be running already; doing nothing. Set ~s's ~s argument to true to force the creation of a new clock and thread regardless." 'start-clock-loop 'force))
  *clock*)

;;; play/stop/end methods

(defmethod play ((event event))
  (clock-add (as-pstream event) *clock*))

(defmethod play ((pattern pattern))
  (clock-add (as-pstream pattern) *clock*))

(defmethod play ((pdef pdef))
  ;; if there is already a task playing this pdef, we do nothing.
  ;; you can use `launch' instead to force launching a second instance of the pattern.
  (unless (position (pdef-task pdef) (clock-tasks *clock*))
    (let ((task (call-next-method)))
      (when (typep task 'task)
        (setf (pdef-task pdef) task)
        task))))

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
  (play (pdef-pattern pdef)))

(defmethod launch ((symbol symbol))
  (launch (pdef symbol)))

(defmethod launch ((list list))
  (mapcar 'launch list))

(defmethod stop ((pdef pdef))
  (when-let ((task (pdef-task pdef)))
    (stop task))
  (setf (pdef-pstream pdef) (as-pstream (pdef-pattern pdef))
        (pdef-task pdef) nil))

(defmethod stop ((symbol symbol))
  (stop (pdef symbol)))

(defmethod stop ((task task))
  (clock-remove task))

(defmethod stop ((list list))
  (mapcar 'stop list))

(defmethod stop ((item null))
  nil)

(defmethod stop ((item (eql t))) ;; (stop t) to stop all playing pdefs and nodes.
  (stop (playing-pdefs))
  (dolist (backend (enabled-backends))
    (backend-panic backend)))

(defmethod end ((item t)) ;; forward to `stop' if a more specific method hasn't been defined for a class.
  (stop item))

(defmethod end ((pdef pdef))
  (if (pdef-task pdef)
      (end (pdef-task pdef))
      (warn "~s has no connected task; try the `stop' method instead." pdef)))

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
