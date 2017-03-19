;;; clock.lisp - keep playing patterns in sync by running each clock in its own thread and all patterns on a clock.

(in-package :cl-patterns)

(defclass clock ()
  ((beats :initform 0 :documentation "The number of beats that have elapsed since the creation of the clock.")
   (tempo :initform 1 :initarg :tempo :accessor tempo :documentation "The tempo of the clock, in beats per second.")
   (granularity :initform 1/10 :initarg :granularity :documentation "How often the clock \"wakes up\" to process events in each beat. Shouldn't need to be adjusted.")
   (tasks :initform nil :documentation "The list of tasks that are running on the clock.")
   (tasks-lock :initform (bt:make-lock) :documentation "The lock on the tasks to make the clock thread-safe.")
   (unix-time-at-tempo :initform (unix-time-byulparan) :documentation "The Unix time when the tempo was last changed.")
   (when-tempo-changed :initform 0 :documentation "The number of 'beats' on the clock when the tempo was last changed.")))

(defmethod (setf tempo) (value (item clock))
  (clock-add (event :type :tempo-change :tempo value) item))

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
  "Process TASK on CLOCK. Processes meta-events like tempo changes, etc. Everything else is sent to to *EVENT-OUTPUT-FUNCTION*."
  (when (null (slot-value task 'next-time)) ;; pstream hasn't started yet.
    (setf (slot-value task 'start-time) (slot-value clock 'beats)
          (slot-value task 'next-time) (slot-value clock 'beats)))
  (let* ((item (slot-value task 'item))
         (nv (next item)))
    (when (not (null nv))
      (if (eq (get-event-value nv :type) :tempo-change)
          (if (numberp (get-event-value nv :tempo))
              (setf (slot-value clock 'tempo) (get-event-value nv :tempo))
              (warn "Tempo change event ~a has invalid :tempo parameter; ignoring." nv))
          (progn
            (incf (slot-value task 'next-time) (delta nv))
            (funcall *event-output-function* (combine-events nv (event :unix-time-at-start (absolute-beats-to-unix-time (slot-value task 'next-time) clock))))
            (when (< (slot-value task 'next-time) (+ (slot-value clock 'beats) (slot-value clock 'granularity)))
              (clock-process-task task clock)))))
    (when (or (null nv) (not (typep item 'pstream))) ;; if the task is done with or isn't a pstream, we return it so it can be removed from the clock, else we return nil so nothing happens.
      task)))

(defun clock-tasks-names (&optional (clock *clock*))
  "Get a list of names of pdefs currently scheduled on CLOCK."
  (mapcar
   (lambda (task) (slot-value (slot-value task 'item) 'key))
   (slot-value clock 'tasks)))

(defun task-should-run-now-p (task clock)
  "Determine whether TASK should be run now according to CLOCK and TASK's quant, etc."
  (let* ((item (slot-value task 'item))
         (quant (quant item)))
    (if (typep item 'pstream)
        (if (null (slot-value task 'next-time)) ;; if the pstream hasn't started yet...
            (or (= 0 quant) ;; ...it starts when the :quant determines it.
                (< (mod (slot-value clock 'beats) quant) (slot-value clock 'granularity)))
            (< (slot-value task 'next-time) (+ (slot-value clock 'beats) (slot-value clock 'granularity))))
        t)))

(defun tick (clock)
  (with-slots (tasks tasks-lock granularity tempo beats) clock
    (loop :do
       (let ((results (bt:with-lock-held (tasks-lock)
                        (mapcar (lambda (task)
                                  (clock-process-task task clock))
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

(defgeneric play (item)
  (:documentation "Play an item (typically an event or pattern) according to the current *event-output-function*."))

(defgeneric stop (item)
  (:documentation "Stop an item (typically a playing task or pdef) according to the current *event-output-function*."))

(defgeneric play-or-stop (item)
  (:documentation "Play an item or stop it if it is already playing. Returns T if the item will start playing, returns NIL if it will stop playing."))

(defmethod play ((item event))
  ;; (funcall *event-output-function* item)
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
