;;;; backend.lisp - general functionality to implement cl-patterns sound server backends.
;;; specific backends are defined in files in the src/backends/ directory.

(in-package #:cl-patterns)

;;; helpers
;; FIX: move these?

(defun task-nodes (task &optional backend)
  "Get the list of nodes for TASK for the specified backend. If BACKEND is nil, get all of the resources for TASK regardless of backend."
  (with-slots (backend-resources) task
    (if backend
        (remove-if-not (curry #'backend-node-p backend) backend-resources)
        backend-resources)))

(defun (setf task-nodes) (value task &optional backend)
  "Set the list of nodes for TASK for the specified backend. If BACKEND is nil, set the full backend-resources slot for the task."
  (with-slots (backend-resources) task
    (setf backend-resources (if backend
                                (append (ensure-list value)
                                        (remove-if (curry #'backend-node-p backend) backend-resources))
                                (ensure-list value)))))

(defun event-backends (event)
  "Get a list of backends that EVENT might be playable on, either via the event's :backend key or via the `enabled-backends'."
  (or (mapcar #'find-backend (ensure-list (or (event-value event :backend)
                                              (event-value event :backends))))
      (enabled-backends)))

;;; backend management

(defclass backend ()
  ((name :initarg :name :accessor backend-name :type string-designator :documentation "The name of the backend instance.")
   (enabled-p :initarg :enabled-p :initform t :accessor backend-enabled-p :type boolean :documentation "Whether this backend instance is currently enabled. Events being played will only be sent to enabled and running backends.")
   (started-p :initarg :started-p :initform nil :accessor backend-started-p :type boolean :documentation "Whether the backend is current enabled and running.")
   (input-processors :initarg :input-processors :initform nil :accessor backend-input-processors :type list :documentation "List of functions that process incoming events. Similar to `*post-pattern-output-processors*' but per-backend.") ; FIX: implement
   (metadata :initarg :metadata :initform nil :accessor backend-metadata :type list :documentation "Additional metadata associated with the backend instance."))
  (:documentation "Abstract superclass for backends."))

(defun all-backend-types ()
  "Get a list of names of all defined backend types. A backend type is any class that inherits from `backend'.

See also: `all-backends', `enabled-backends'"
  (mapcar #'class-name (subclasses-of 'backend)))

(defvar *backends* nil
  "List of loaded and active cl-patterns backends. This variable generally shouldn't need to be modified by the user; instead register backends by loading the relevant cl-patterns sub-system, then use `make-backend' and/or `backend-start' to make and activate a backend.

See also: `all-backends', `all-backend-types', `make-backend', `backend-start', `backend-stop'")

(defun all-backends (&key (enabled-p nil enabled-p-provided-p) (started-p nil started-p-provided-p))
  "Get a list of all backends made with `make-backend' or `backend-start'. ENABLED-P and STARTED-P can be used to limit the results only to backends with matching status.

See also: `all-backend-types', `enabled-backends'"
  (loop :for backend :in *backends*
        :if (and (if enabled-p-provided-p
                     (eql enabled-p (backend-enabled-p backend))
                     t)
                 (if started-p-provided-p
                     (eql started-p (backend-started-p backend))
                     t))
          :collect backend))

(defun backend-p (object)
  "Test whether OBJECT is a `backend'.

See also: `find-backend', `all-backends'"
  (typep object 'backend))

(defun find-backend (backend &key enabled-p)
  "Find a registered backend whose name or type matches BACKEND. With ENABLED-P, only search through currently enabled backends.

See also: `all-backends', `enabled-backends'"
  (when (typep backend 'backend)
    (return-from find-backend backend))
  (find-if (fn (or (string-equal backend (backend-name _))
                   (string-equal backend (class-name (class-of _)))))
           (if enabled-p
               (enabled-backends)
               (all-backends))))

(defgeneric make-backend (backend &rest rest &key &allow-other-keys)
  (:documentation "Make a backend of the specified type.

See also: `backend-start'"))

(defmethod make-backend (backend &rest rest &key &allow-other-keys)
  (if-let ((found-backend (find backend (all-backend-types) :test #'string-equal)))
    (apply #'make-instance found-backend rest)
    (apply #'no-applicable-method #'make-backend backend rest)))

(defmethod initialize-instance :after ((backend backend) &key &allow-other-keys)
  (pushnew backend *backends*))

(defgeneric backend-start (backend &rest args &key &allow-other-keys)
  (:documentation "Start BACKEND's server so it is ready to receive events. If BACKEND is the name of a backend rather than a `backend' instance, first make an instance of the backend as if `make-backend' was called, then call `backend-start' on that.

See also: `backend-stop', `backend-enabled-p', `make-backend'"))

(defmethod backend-start ((name symbol) &rest args &key &allow-other-keys)
  (let ((backend (apply #'make-backend name :allow-other-keys t args)))
    (apply #'backend-start backend :allow-other-keys t args)))

(defmethod backend-start ((backend backend) &key &allow-other-keys)
  nil)

(defmethod backend-start :after ((backend backend) &key &allow-other-keys)
  (setf (backend-started-p backend) t))

(defgeneric backend-stop (backend)
  (:documentation "Stop BACKEND's server if it is running.

See also: `backend-start', `backend-enabled-p'"))

(defmethod backend-stop ((backend symbol))
  (dolist (c-backend (all-backends))
    (when (string-equal backend (backend-name c-backend))
      (backend-stop c-backend))))

(defmethod backend-stop :after ((backend backend))
  (setf (backend-started-p backend) nil))

;; (defgeneric backend-handles-event-p (backend event) ; FIX: is this needed?
;;   (:documentation "True if BACKEND is currently available to handle EVENT."))

;; (defmethod backend-handles-event-p (backend event)
;;   )

(defun enabled-backends () ; FIX: remove this?
  "Get a list of all enabled backends.

See also: `all-backends', `backend-enabled-p'"
  (remove-if-not #'backend-enabled-p (all-backends)))

(defgeneric backend-responds-p (backend event)
  (:documentation "True if BACKEND should respond to EVENT."))

(defmethod backend-responds-p (backend event)
  t)

(defmethod backend-responds-p (event (backend backend))
  (backend-responds-p backend event))

(defgeneric backend-play-event (backend event task)
  (:documentation "Play ITEM on the sound server specified by BACKEND. TASK is the task that triggered ITEM to be played. Typically a backend should not need to define a specialized method for this generic if it already provides methods for the following:

- `backend-instrument-controls'
- `backend-node-p'
- `backend-timestamps-for-event'
- `backend-proxys-node'
- `backend-control-node-at'

It's suggested to define methods for `backend-convert-object' if the backend requires objects to be converted to another representation before being used as an instrument's parameters. Additionally, methods for `play', `launch', `stop', and `end' may be convenient to define for the backend's node class.

See also: `backend-task-removed'"))

;; FIX: move all of the node stuff out of backend? or is it possible that some backends would know voice allocation better than we do?

(defmethod backend-play-event (backend event task)
  (let ((type (event-value event :type))
        (instrument (instrument event)))
    (case type
      (:rest
       nil)
      (:tempo
       (backend-tempo-change-at backend
                                (task-clock task)
                                (car (backend-timestamps-for-event backend event task))))
      (t
       (when (and (not (position type (list :rest :tempo)))
                  (or (backend-instrument-controls backend instrument)
                      (backend-node-p backend instrument)))
         (let ((time (backend-timestamps-for-event backend event task))
               (params (backend-instrument-args-list backend instrument event)))
           (if (or (eql type :mono)
                   (backend-node-p backend instrument))
               (let ((node (backend-control-node-at backend
                                                    (first time)
                                                    (let ((nodes (task-nodes task backend)))
                                                      (cond (nodes
                                                             (car nodes))
                                                            ((backend-node-p backend instrument)
                                                             (or (backend-proxys-node backend instrument)
                                                                 instrument))
                                                            (t instrument)))
                                                    params)))
                 (unless (or (backend-node-p backend instrument)
                             (not (backend-instrument-has-gate-p backend instrument)))
                   (if (< (legato event) 1)
                       (progn
                         (backend-control-node-at backend (second time) node (list :gate 0))
                         (setf (task-nodes task backend) nil))
                       (setf (task-nodes task backend) (list node)))))
               (let ((node (backend-control-node-at backend (first time) instrument params)))
                 ;; FIX: should add NODE to the task's backend-resources slot, then free it when it stops
                 (when (backend-instrument-has-gate-p backend instrument)
                   (backend-control-node-at backend (second time) node (list :gate 0)))))))))))

(defgeneric backend-tempo-change-at (backend clock timestamp)
  (:documentation "Set the backend's tempo to NEW-TEMPO at the timestamp provided."))

(defmethod backend-tempo-change-at (backend clock timestamp)
  nil)

(defgeneric backend-task-added (backend task) ; FIX: when created, backend objects should be sent all current tasks
  (:documentation "Called when TASK is added to the clock so BACKEND can prepare any related state.

See also: `backend-task-removed'"))

(defmethod backend-task-added (backend task)
  nil)

(defgeneric backend-task-removed (backend task)
  (:documentation "Called when TASK is removed from the clock so BACKEND can free any associated nodes. Typically a backend shouldn't need to define a method for this generic if it already defines methods for the events listed in the docstring for `backend-play-event'.

See also: `backend-play-event'"))

(defmethod backend-task-removed (backend task) ; FIX: this code should probably be run by the clock rather than a backend method since it should apply to all backends (maybe?)
  (let ((item (slot-value task 'item))
        (nodes (task-nodes task backend)))
    (if (event-p item)
        (mapc #'stop nodes) ; FIX: this doesn't work because the preview synth doesn't have a gate argument, and non-gated synths aren't kept in task's backend-resources slot.
        (let ((last-output (last-output item)))
          (dolist (node nodes)
            (backend-control-node-at backend
                                     (cadr
                                      (backend-timestamps-for-event
                                       backend
                                       (event-with-raw-timing (combine-events last-output (event :legato 1))
                                                              task)
                                       task))
                                     node
                                     (list :gate 0)))))
    (when-let ((cleanup (and (slot-exists-p item 'cleanup)
                             (slot-value item 'cleanup))))
      (mapc 'funcall cleanup)))
  (setf (task-nodes task backend) nil))

(defgeneric backend-instrument-controls (backend instrument)
  (:documentation "Get the list of names of controls for INSTRUMENT in BACKEND."))

(defun backend-instrument-has-gate-p (backend instrument)
  "Whether or not SYNTH in BACKEND has a gate control (i.e. whether it needs to be manually released or if it acts as a \"one-shot\")."
  (position :gate (backend-instrument-controls backend instrument) :test 'string-equal))

(defgeneric backend-instrument-args-list (backend instrument event)
  (:documentation "Generate a plist of parameters for INSTRUMENT based off of its controls, taking values from EVENT. Unlike `event-plist', this function doesn't include event keys that aren't also one of the synth's arguments."))

(defmethod backend-instrument-args-list (backend instrument event)
  (if-let ((controls (backend-instrument-controls backend instrument)))
    (let ((instrument-params (remove-if (lambda (arg) ; for parameters unspecified by the event, we fall back to the instrument's defaults, NOT the event's...
                                          (unless (string-equal arg :sustain) ; ...with the exception of sustain, which the instrument should always get.
                                            (multiple-value-bind (value key) (event-value event arg)
                                              (declare (ignore value))
                                              (eql key t))))
                                        (append controls (list :group :to :id))))) ; FIX: this is for the supercollider backend; genericize this
      ;; get the value of each of the instrument's arguments from the event...
      (loop :for param :in instrument-params
            :for sparam := (make-keyword (string-upcase param))
            :for val := (backend-convert-object backend (event-value event sparam) sparam)
            :if (or (eql :gate sparam)
                    val)
              :append (list (if (eql :group sparam) ; :group is an alias for :to
                                :to
                                sparam)
                            (if (eql :gate sparam) 1 val))))
    (copy-list (event-plist event)))) ; if we don't have data for the instrument, all we can do is return the plist for the event and hope for the best.

(defgeneric backend-all-instruments (backend)
  (:documentation "Get a list of the names of all defined synths (synthdefs, dsps, etc) for the specified backend.

See also: `backend-all-nodes'"))

(defmethod backend-all-instruments (backend)
  nil)

(defmethod backend-all-instruments ((backend symbol))
  (backend-all-instruments (find-backend backend)))

(defgeneric backend-all-nodes (backend)
  (:documentation "Get a list of all active nodes for the specified backend.

See also: `backend-all-instruments', `backend-node-p', `backend-panic'"))

(defmethod backend-all-nodes (backend)
  nil)

(defgeneric backend-node-p (backend object)
  (:documentation "True if OBJECT is a node for the specified backend.

See also: `backend-all-nodes'"))

(defgeneric backend-panic (backend)
  (:documentation "\"Panic\" the backend, i.e. stop all of its nodes immediately.

See also: `backend-all-nodes'"))

(defmethod backend-panic (backend)
  (stop (backend-all-nodes backend)))

(defgeneric backend-timestamps-for-event (backend event task)
  (:documentation "Get a list containing timestamps for the start and end of EVENT's note, in the format that BACKEND expects for its scheduling function.

See also: `backend-control-node-at'"))

(defgeneric backend-proxys-node (backend id)
  (:documentation "Get the current node object on BACKEND for the proxy with the specified ID."))

(defgeneric backend-control-node-at (backend time node params)
  (:documentation "At TIME, set NODE's parameters to PARAMS. If NODE is an instrument name, launch a node with those parameters at the specified time instead. This function should return the node object."))

(defgeneric backend-convert-object (backend object key)
  (:documentation "Convert OBJECT to a value that BACKEND can understand. For example, the SuperCollider backend requires that any `cl-collider::buffer' objects are converted to their bufnum."))

(defmethod backend-convert-object (backend object key)
  (declare (ignore key))
  object)

(defmethod backend-convert-object (backend (symbol symbol) key)
  (declare (ignore key backend))
  (when symbol
    (find-object-by-id symbol)))
