;;;; clock.lisp - clocks, tasks, and other functionality related to playing patterns in sync.
;;; This clock uses the local-time system to calculate the exact time each event should occur. This calculated time is then passed to the relevant backend. Thus there should be no jitter from cl-patterns, in theory.
;;; The reason we have a clock at all is so that patterns can be changed while they're playing. When a pattern is played, its events are not all generated immediately; they're generated approximately N seconds before they're supposed to be heard, where N is the value of the clock's LATENCY slot.
;;; For efficiency, the clock processes LATENCY seconds worth of events at a time and sends them with timestamps to the relevant backend (see backend.lisp), which then converts the event to whatever format the backend server understands.
;;;
;;; FIX:
;;; - investigate syncing to internal time or time of day instead of local-time? https://github.com/jamieforth/osc/blob/79d25ca4e0a4a04135b6bc56231c6b9bb058f1d4/osc.lisp#L279

(in-package #:cl-patterns)

;;; task

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass task ()
    ((item :initarg :item :initform nil :accessor task-item :documentation "The actual playing item that the task refers to. Typically this is a pstream or similar.")
     (loop-p :initarg :loop-p :documentation "Whether the task should loop. If left unbound, the task's item's loop-p slot is referred to instead.")
     (start-beat :initarg :start-beat :initform nil :type (or null number) :documentation "The beat of the clock when the task started.")
     (clock :initarg :clock :accessor task-clock :type clock :documentation "The clock that the task is running on.")
     (backend-resources :initarg :backend-resources :initform nil :accessor task-backend-resources :type list :documentation "Resources associated with this task that should be stopped when the task is stopped, i.e. nodes it triggered, etc."))
    (:documentation "An item scheduled to be run on the clock.")))

(eval-when (:compile-toplevel :load-toplevel :execute) ; needed for the following let form.
  (closer-mop:ensure-finalized (find-class 'task)))

(let ((sym 'task-backend-resources))
  (setf (documentation sym 'function) (documentation (find-class-slot 'task :accessor sym) t)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (item) task
      (format stream "~S ~S" :item item))))

(defmethod beat ((task task))
  (beat (task-item task)))

(defmethod stop ((task task))
  (clock-remove task))

(defmethod end ((item task))
  (setf (slot-value item 'loop-p) nil))

(defmethod playing-p ((task task) &optional (clock *clock*))
  (when clock
    (position task (clock-tasks clock))))

(defmethod loop-p ((task task))
  (if (slot-boundp task 'loop-p)
      (slot-value task 'loop-p)
      (loop-p (slot-value task 'item))))

(defun event-with-raw-timing (event task)
  "Get an event like EVENT but with the :BEAT-AT-START and :TIMESTAMP-AT-START keys added for backends."
  (with-slots (item clock start-beat) task
    (let* ((tempo (tempo clock))
           (play-quant (play-quant event))
           (e-beat (beat event))
           (beat (+ (if (event-p item)
                        (if (eop-p e-beat)
                            (next-beat-for-quant play-quant (beat *clock*))
                            e-beat)
                        (+ start-beat e-beat))
                    (duration-dur (or (raw-event-value event :latency) (clock-latency (task-clock task)))
                                  tempo)
                    (duration-dur (or (raw-event-value event :timing-offset) 0)
                                  tempo)
                    (if (> (length play-quant) 2)
                        (nth 2 play-quant)
                        0))))
      (combine-events event
                      (event :beat-at-start beat
                             :timestamp-at-start (absolute-beats-to-timestamp beat clock))))))

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
    (remove-if-not (fn (with-slots (item) _
                         (or (eq pattern item)
                             (with-slots (source) item
                               (or (eq pattern source)
                                   (when-let ((source-key (ignore-errors (pdef-name source))))
                                     (eql source-key (pdef-name pattern)))
                                   (when (typep source 'pdef)
                                     (eq pattern (pdef-pattern source))))))))
                   (slot-value clock 'tasks))))

;;; clock

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass clock ()
    ((beat :initform 0 :accessor beat :type number :documentation "The number of beats that have elapsed since the creation of the clock.")
     (tempo :initarg :tempo :initform 1 :reader tempo :type number :documentation "The tempo of the clock, in beats per second.")
     (latency :initarg :latency :initform 1/10 :type number :documentation "The default latency for events played on the clock; the number of seconds added onto the event's scheduled time, in order to allow the backend to process it without being \"late\".")
     (play-expired-events :initarg :play-expired-events :initform nil :documentation "If T, always play events, even if their scheduled time has already passed. If NIL, silently skip these events. If :WARN, print a warning for each event skipped.")
     (condition-handler :initarg :condition-handler :initform nil :type symbol :documentation "The restart to invoke when a condition occurs during task processing. If nil, the clock will not attempt to handle any conditions. If non-nil, all conditions signaled within a task will be caught and the specified restart will be invoked automatically to prevent the clock from pausing. Caught conditions will be printed as a warning and recorded with data generated by `caught-conditions-extra-data-function' in the clock's `caught-conditions' slot.")
     (caught-conditions :initform nil :type list :documentation "A list of conditions caught by the clock while processing tasks when its condition-handler is non-nil. Each entry in the list is a plist containing the the condition and any extra data generated by `caught-conditions-extra-data-function'.")
     (caught-conditions-lock :initform (bt:make-lock) :documentation "The lock on the caught-conditions slot to ensure it is thread-safe.")
     (caught-conditions-extra-data-function :initarg :caught-conditions-extra-data-function :initform nil :accessor clock-caught-conditions-extra-data-function :type (or null function) :documentation "A function of one argument (the clock) that generates extra data for the `caught-conditions' when a condition occurs during the clock's task processing.")
     (tasks :initform nil :type list :documentation "The list of tasks that are running on the clock. Use `play', `stop', etc to play and stop patterns (the \"friendly\" way to add or remove them from the clock), or `clock-add' and `clock-remove' to manually remove them directly.")
     (tasks-lock :initform (bt:make-recursive-lock) :documentation "The lock on the tasks to make the clock thread-safe.")
     (timestamp-at-tempo :initform (local-time:now) :documentation "The local-time timestamp when the tempo was last changed.")
     (beat-at-tempo :initform 0 :documentation "The number of beats on the clock when the tempo was last changed."))
    (:documentation "A musical time-based clock defining a tempo and pulse that its tasks synchronize to.")))

(eval-when (:compile-toplevel :load-toplevel :execute) ; needed for the following let form.
  (closer-mop:ensure-finalized (find-class 'clock)))

(let ((sym 'clock-caught-conditions-extra-data-function))
  (setf (documentation sym 'function) (documentation (find-class-slot 'clock :accessor sym) t)))

(defmethod print-object ((clock clock) stream)
  (print-unreadable-object (clock stream :type t)
    (with-slots (tempo beat) clock
      (format stream "~S ~S (~S BPM) ~S ~F" :tempo tempo (* tempo 60) :beat beat))))

(defun make-clock (&optional (tempo 1) &key (latency 1/10) play-expired-events condition-handler)
  "Create a clock with a tempo of TEMPO in beats per second (Hz).

Use `start-clock-loop' to start the clock so that it begins processing its tasks in a new thread, or call `clock-loop' to start the loop in the current thread. Alternatively, the clock can be advanced manually an arbitrary number of beats at a time using `clock-process'.

See also: `clock-process', `clock-loop', `start-clock-loop'"
  (make-instance 'clock :tempo tempo
                        :latency latency
                        :play-expired-events play-expired-events
                        :condition-handler condition-handler))

(defmethod real-beat ((clock clock))
  "Get the \"real beat\" of the clock; i.e. compute what the beat number should actually be at this instant in time (whereas the beat slot for the clock is quantized to the clock's granularity).

Note that this function will likely be removed in the future with improvements to the clock, so you should expect to eventually have to update code depending on it.

See also: `beat'"
  (with-slots (tempo timestamp-at-tempo beat-at-tempo) clock
    (+ beat-at-tempo (* tempo (local-time:timestamp-difference (local-time:now) timestamp-at-tempo)))))

(defun absolute-beats-to-timestamp (beats &optional (clock *clock*)) ; FIX: should this be moved to conversions.lisp?
  "Convert a clock's number of beats to a timestamp. The result is only guaranteed to be accurate if it's greater than the clock's beat-at-tempo slot."
  (with-slots (timestamp-at-tempo beat-at-tempo) clock
    (local-time:timestamp+ timestamp-at-tempo (truncate (* (dur-duration (- beats beat-at-tempo) (tempo clock)) 1000000000)) :nsec)))

(defmethod (setf tempo) (value (clock clock))
  ;; FIX: the "remove previous events" functionality should be a standard part of `clock-add' for :tempo events; it should remove any existing events with the same `beat'.
  (dolist (task (clock-tasks clock)) ; this tempo change event obsoletes any existing ones, so we remove them
    (with-slots (item) task
      (let ((event (pattern-source item)))
        (when (and (event-p event)
                   (eql (event-value event :type) :tempo)
                   (null (beat event))) ; as long as they don't have a beat specified
          (clock-remove task clock)))))
  (clock-add (as-pstream (event :type :tempo :tempo value)) clock))

(defmethod tempo ((number number))
  ;; convenience method to quickly set the tempo of the default clock.
  (setf (tempo *clock*) number))

(defun clock-latency (&optional (clock *clock*))
  "The default latency for events played on the clock; the number of seconds added onto the event's scheduled time, in order to allow the backend to process it without being \"late\"."
  (slot-value clock 'latency))

(defun (setf clock-latency) (value &optional (clock *clock*))
  (setf (slot-value clock 'latency) value))

(defun clock-add (item &optional (clock *clock*)) ; FIX: take event :beat into account
  "Add ITEM (usually a `pstream') to CLOCK's tasks. Generally you don't need to use this directly and would use `play' instead.

See also: `clock-remove', `play'"
  (unless clock
    (restart-case
        (error "~S is nil; use ~S to start a clock loop on a new thread" 'clock 'start-clock-loop)
      (start-clock ()
        :report (lambda (stream) (format stream "Start the clock loop with ~S" '(start-clock-loop)))
        (start-clock-loop))))
  (with-slots (tasks tasks-lock) clock
    (bt:with-recursive-lock-held (tasks-lock)
      (let ((task (make-instance 'task :item item
                                       :clock clock
                                       :start-beat (next-beat-for-quant (play-quant item) (slot-value clock 'beat)))))
        (push task tasks)
        task))))

(defun clock-remove (task &optional (clock *clock*))
  "Remove TASK from CLOCK's tasks. Generally you don't need to use this directly and would use `stop' or `end' instead.

See also: `clock-add', `stop', `end'"
  (with-slots (tasks tasks-lock) clock
    (bt:with-recursive-lock-held (tasks-lock)
      (setf tasks (remove-if (lambda (ctask)
                               (when-let ((eq (eq ctask task)))
                                 (dolist (backend (all-backends :enabled-p t) eq)
                                   (backend-task-removed backend task))))
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

(defun clock-process-event (clock task event type)
  "Process EVENT on CLOCK. TASK is the associated task, and TYPE is the event type."
  (when (eql type :tempo)
    (with-slots (timestamp-at-tempo tempo beat-at-tempo) clock
      (unless (and (numberp (event-value event :tempo))
                   (plusp (event-value event :tempo)))
        (warn "Tempo change event ~S has invalid :tempo parameter; ignoring." event)
        (return-from clock-process-event))
      (setf timestamp-at-tempo (raw-event-value event :timestamp-at-start)
            tempo (event-value event :tempo)
            beat-at-tempo (raw-event-value event :beat-at-start))
      (dolist (backend (event-backends event))
        (backend-tempo-change-at backend clock (backend-timestamps-for-event backend event task)))))
  (dolist (backend (event-backends event))
    (restart-case
        (backend-play-event backend event task)
      (backend-skip-event ()
        :report (lambda (stream)
                  (format stream "Skip this event on ~S." backend))
        nil)
      (backend-disable ()
        :report (lambda (stream)
                  (format stream "Disable backend ~S." backend))
        (setf (backend-enabled-p backend) nil)))))

(defun can-swap-now-p (pstream &optional (beat (beat *clock*)))
  "Whether PSTREAM can swap to its new definition, based on `end-quant', `end-condition', `ended-p', and BEAT (the current beat of the clock)."
  (or (when (and beat
                 (plusp (slot-value pstream 'number)))
        (when-let ((end-quant (end-quant pstream)))
          (= beat (next-beat-for-quant end-quant beat))))
      (when (end-condition pstream)
        (funcall (end-condition pstream) pstream))
      (ended-p pstream)))

(defun clock-process (clock beats)
  "Process tasks on CLOCK for the next BEATS beats.

See also: `clock-loop', `clock-tasks', `make-clock'"
  (bt:with-recursive-lock-held ((slot-value clock 'tasks-lock))
    (loop
      :with *clock* := clock
      :with clock-end-beat := (+ (beat clock) beats)
      :with retries := 0
      :with prev-task := nil
      :until (or (>= (beat *clock*) clock-end-beat)
                 (null (clock-tasks clock)))
      :for tasks := (remove-if-not (lambda (task)
                                     (< (+ (slot-value task 'start-beat) (beat task)) clock-end-beat))
                                   (clock-tasks clock))
      :for task := (most #'< tasks :key (lambda (task) (+ (slot-value task 'start-beat) (beat task))))
      :for item := (and task (task-item task))
      :do (setf retries (if (eq task prev-task) (1+ retries) 0)
                prev-task task)
      :if (or (null item) (eop-p item))
        :do (loop-finish)
      :if (>= retries 32)
        :do (warn "Task ~S yielded NIL 32 times in a row; removing from clock to avoid locking into an infinite loop." task)
            (clock-remove task)
      :else
        :if (and (can-swap-now-p item (beat clock))
                 (or (and (typep item 'pdef-pstream)
                          (not (eq (slot-value item 'pattern) (pdef-pattern (pdef-name item)))))
                     (not (loop-p task))
                     (ended-p item)))
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
                  (let ((event (next item)))
                    (unless (eop-p event)
                      (dolist (event (split-event-by-lists event))
                        (let ((event (event-with-raw-timing event task)))
                          (if (or (local-time:timestamp>= (event-value event :timestamp-at-start) (local-time:now))
                                  (eql t (slot-value clock 'play-expired-events)))
                              (clock-process-event clock task event (event-value event :type))
                              (when (eql :warn (slot-value clock 'play-expired-events))
                                (warn "Clock skipped expired event ~S from task ~S." event task))))))))
              (skip-event ()
                :report "Skip this event, preserving the task on the clock so it can be run again."
                nil)
              (restart-task ()
                :report (lambda (stream) (format stream "Remove the task from the clock and re-add its item to play at its next play-quant ~S." (play-quant (task-item task))))
                (clock-remove task clock)
                (let ((*clock* clock))
                  (play (task-item task))))
              (remove-task ()
                :report "Remove this task from the clock."
                (clock-remove task clock)))
      :finally (setf (beat clock) clock-end-beat))))

(defun clock-condition-handler (&optional (clock *clock*))
  "The restart to invoke when a condition occurs during task processing. If nil, the clock will not attempt to handle any conditions. If non-nil, all conditions signaled within a task will be caught and the specified restart will be invoked automatically to prevent the clock from pausing. Caught conditions will be printed as a warning and recorded in the clock's caught-conditions slot with the condition and any extra data generated by the function in the clock's `caught-conditions-extra-data-function' slot.

See also: `clock-caught-conditions'"
  (slot-value clock 'condition-handler))

(defun (setf clock-condition-handler) (value &optional (clock *clock*))
  (setf (slot-value clock 'condition-handler) value))

(defun clock-caught-conditions (&optional (clock *clock*))
  "A list of conditions caught by the clock while processing tasks when its condition-handler is non-nil. Each entry in the list is a plist containing the the condition and additional data generated by the `caught-conditions-extra-data-function' slot's function.

See also: `clock-condition-handler'"
  (slot-value clock 'caught-conditions))

(defun (setf clock-caught-conditions) (value &optional (clock *clock*))
  (setf (slot-value clock 'caught-conditions) value))

;;; basic clock-loop convenience functionality

(defun clock-loop (clock &key (granularity (clock-latency clock)))
  "Convenience method for processing a clock's tasks in a loop.

To run the clock in a new thread, you can call `start-clock-loop'.

See also: `start-clock-loop', `clock-process'"
  (unwind-protect
       (loop
         (if (clock-condition-handler clock)
             (handler-bind
                 ((error
                    (lambda (e)
                      (bt:with-lock-held ((slot-value clock 'caught-conditions-lock))
                        (let ((restart (if (member (clock-condition-handler clock) (list 'remove-task 'skip-event))
                                           (clock-condition-handler clock)
                                           'remove-task)))
                          (warn "Task had condition ~S; invoked ~S restart and pushed the condition to ~S's ~S slot." e restart clock 'caught-conditions)
                          (push (list* :condition e
                                       (when-let ((function (caught-conditions-extra-data-function clock)))
                                         (funcall function clock)))
                                (slot-value clock 'caught-conditions))
                          (invoke-restart restart))))))
               (clock-process clock granularity))
             (clock-process clock granularity))
         (sleep (max 0
                     (- (local-time:timestamp-difference
                         (absolute-beats-to-timestamp (beat clock) clock)
                         (local-time:now))
                        (/ granularity 2)))))
    (warn "The clock loop has stopped! You will likely need to create a new clock with (start-clock-loop) in order to play patterns again.")))

(defun start-clock-loop (&rest clock-initargs &key (tempo 1) force &allow-other-keys)
  "Convenience method to make a clock and start its loop in a new thread if one isn't already started.

With FORCE, make a new clock and thread even if one already appears to be running.

See also: `clock-loop', `*clock*'"
  (if (or force
          (null *clock*)
          (null (find "cl-patterns clock-loop" (bt:all-threads) :key #'bt:thread-name :test #'string-equal)))
      (progn
        (setf *clock* (apply #'make-clock tempo :allow-other-keys t clock-initargs))
        (bt:make-thread (lambda () (clock-loop *clock*)) :name "cl-patterns clock-loop"))
      (warn "A clock appears to be running already; doing nothing. Provide :force t as arguments to force the creation of a new clock and thread regardless."))
  *clock*)
