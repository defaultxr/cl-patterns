(in-package #:cl-patterns)

;;; helpers

(defvar *dictionary-lookup-functions* (list 'find-pdef)
  "List of functions that can be used to look up the object that a symbol can name. Each function should return the object in question if it exists, or nil (or throw an error) if it doesn't.

Functions like `play', `end', `launch', and `stop' will check symbols against each of these dictionaries in order and will apply themselves to the object from the first dictionary with a matching key. 

Example:

;; *dictionary-lookup-functions*
;; => (CL-PATTERNS::FIND-PDEF BDEF:BDEF)
;; (play :foo) ;; will (play (pdef :foo)) if that pdef exists, or (play (bdef :foo)) if the bdef exists. If neither exists, it will throw an error.

See also: `play', `launch', `end', `stop'")

(defun lookup-object-for-symbol (symbol)
  "Look up the object named by SYMBOL using `*dictionary-lookup-functions*'. Returns nil if no object was found."
  (loop :for func :in *dictionary-lookup-functions*
     :for res = (ignore-errors (funcall func symbol))
     :when res
     :return res))

(defun task-nodes (task backend)
  "Get the list of nodes for TASK for the specified backend. If BACKEND is nil, get all of the resources for TASK regardless of backend."
  (if backend
      (remove-if-not (rcurry 'backend-node-p backend) (slot-value task 'backend-resources))
      (slot-value task 'backend-resources)))

(defun (setf task-nodes) (value task backend)
  "Set the list of nodes for TASK for the specified backend. If BACKEND is nil, set the full backend-resources slot for the task."
  (if backend
      (setf (slot-value task 'backend-resources) (append (ensure-list value)
                                                         (remove-if (rcurry 'backend-node-p backend)
                                                                    (slot-value task 'backend-resources))))
      (setf (slot-value task 'backend-resources) (ensure-list value))))

(defun event-backends (event)
  "Get a list of backends that EVENT should be played on, either via the event's :backend key or via the `enabled-backends'."
  (or (ensure-list (or (event-value event :backends)
                       (event-value event :backend)))
      (enabled-backends)))

;;; backend management

(defvar *enabled-backends* nil
  "List of registered and enabled backends for cl-patterns. Any event that is played is tested against each of these backends' respond-p functions in order. The event is then played on the first backend to return true.")

(defvar *backends* nil
  "List of registered backends for cl-patterns. This holds all the information about each backend and should not be modified by the user; change the `*enabled-backends*' variable to set which backends are enabled.")

(defun register-backend (name)
  "Register a cl-patterns backend."
  (pushnew (make-keyword name) *backends*))

(defun all-backends ()
  "Get a list of all registered backends.

See also: `enabled-backends'"
  (keys *backends*))

(defun enabled-backends ()
  "Get a list of all enabled backends.

See also: `all-backends', `enable-backend'"
  *enabled-backends*)

(defun enable-backend (name)
  "Enable a registered backend.

See also: `disable-backend', `start-backend', `enabled-backends'"
  (let ((name (make-keyword name)))
    (assert (position name *backends*) (name) "No backend named ~s registered." name)
    (pushnew name *enabled-backends*)))

(defun disable-backend (name)
  "Disable a registered backend and stop it if it is running.

See also: `enable-backend', `stop-backend'"
  (let ((name (make-keyword name)))
    (setf *enabled-backends*
          (delete name *enabled-backends*))
    (stop-backend name)))

;;; generics

(defgeneric start-backend (backend &key &allow-other-keys)
  (:documentation "Start a backend. Some backends support additional options which can be provided as keyword arguments.

See also: `stop-backend', `enable-backend'"))

(defmethod start-backend :before (backend &key &allow-other-keys)
  (enable-backend backend))

(defgeneric stop-backend (backend)
  (:documentation "Stop a backend. This is automatically called when `disable-backend' is run.

See also: `start-backend', `disable-backend'"))

(defgeneric backend-play-event (event task backend)
  (:documentation "Play ITEM on the sound server specified by BACKEND. TASK is the task that triggered ITEM to be played. Typically a backend should not need to define a specialized method for this generic if it already provides methods for the following:

- `backend-instrument-controls'
- `backend-node-p'
- `backend-timestamps-for-event'
- `backend-proxys-node'
- `backend-control-node-at'

It's suggested to define methods for `backend-convert-object' if the backend requires objects to be converted to another representation before being used as an instrument's parameters. Additionally, methods for `play', `launch', `stop', and `end' may be convenient to define for the backend's node class.

See also: `backend-task-removed'"))

(defgeneric backend-tempo-change-at (clock timestamp backend)
  (:documentation "Set the backend's tempo to NEW-TEMPO at the timestamp provided."))

(defmethod backend-tempo-change-at (clock timestamp (backend t))
  nil)

(defmethod backend-play-event (item task backend)
  (let ((type (event-value item :type))
        (instrument (instrument item)))
    (case type
      (:rest
       nil)
      (:tempo-change
       (backend-tempo-change-at (slot-value task 'clock)
                                (car (backend-timestamps-for-event item task backend))
                                backend))
      (t
       (when (and (not (position type (list :rest :tempo-change)))
                  (or (backend-instrument-controls instrument backend)
                      (backend-node-p instrument backend)))
         (let ((time (backend-timestamps-for-event item task backend))
               (params (backend-instrument-args-list instrument item backend)))
           (if (or (eql type :mono)
                   (backend-node-p instrument backend))
               (let ((node (backend-control-node-at (car time)
                                                    (let ((nodes (task-nodes task backend)))
                                                      (cond (nodes
                                                             (car nodes))
                                                            ((backend-node-p instrument backend)
                                                             (or (backend-proxys-node instrument backend)
                                                                 instrument))
                                                            (t instrument)))
                                                    params
                                                    backend)))
                 (unless (or (backend-node-p instrument backend)
                             (not (backend-instrument-has-gate-p instrument backend)))
                   (if (< (legato item) 1)
                       (progn
                         (backend-control-node-at (cadr time)
                                                  node
                                                  (list :gate 0)
                                                  backend)
                         (setf (task-nodes task backend) nil))
                       (setf (task-nodes task backend) (list node)))))
               (let ((node (backend-control-node-at (car time)
                                                    instrument
                                                    params
                                                    backend)))
                 ;; FIX: should add NODE to the task's backend-resources slot, then free it when it stops
                 (when (backend-instrument-has-gate-p instrument backend)
                   (backend-control-node-at (cadr time)
                                            node
                                            (list :gate 0)
                                            backend))))))))))

(defgeneric backend-task-removed (task backend)
  (:documentation "Called when TASK is removed from the clock so BACKEND can free any associated nodes. Typically a backend shouldn't need to define a method for this generic if it already defines methods for the events listed in the docstring for `backend-play-event'.

See also: `backend-play-event'"))

(defmethod backend-task-removed (task backend)
  (let ((item (slot-value task 'item))
        (nodes (task-nodes task backend)))
    (if (event-p item)
        (mapc 'stop nodes) ;; FIX: this doesn't work because the preview synth doesn't have a gate argument, and non-gated synths aren't kept in task's backend-resources slot.
        (let ((last-output (last-output item)))
          (dolist (node nodes)
            (backend-control-node-at (cadr
                                      (backend-timestamps-for-event
                                       (event-with-raw-timing (combine-events last-output (event :legato 1))
                                                              task)
                                       task
                                       backend))
                                     node
                                     (list :gate 0)
                                     backend)))))
  (setf (task-nodes task backend) nil))

(defgeneric backend-instrument-controls (instrument backend)
  (:documentation "Get the list of names of controls for INSTRUMENT in BACKEND."))

(defun backend-instrument-has-gate-p (instrument backend)
  "Whether or not SYNTH in BACKEND has a gate control (i.e. whether it needs to be manually released or if it acts as a \"one-shot\")."
  (position :gate (backend-instrument-controls instrument backend) :test 'string-equal))

(defgeneric backend-instrument-args-list (instrument event backend)
  (:documentation "Generate a plist of parameters for INSTRUMENT based off of its controls, taking values from EVENT. Unlike `event-plist', this function doesn't include event keys that aren't also one of the synth's arguments."))

(defmethod backend-instrument-args-list (instrument event backend)
  (if-let ((controls (backend-instrument-controls instrument backend)))
    (let ((instrument-params (remove-if (lambda (arg) ;; for parameters unspecified by the event, we fall back to the instrument's defaults, NOT the event's...
                                          (unless (string= (symbol-name arg) "SUSTAIN") ;; ...with the exception of sustain, which the instrument should always get.
                                            (multiple-value-bind (value key) (event-value event arg)
                                              (declare (ignore value))
                                              (eql key t))))
                                        (append controls (list :group :to :id))))) ;; FIX: this is for the supercollider backend; genericize this
      ;; get the value of each of the instrument's arguments from the event...
      (loop :for param :in instrument-params
         :for sparam = (make-keyword param)
         :for val = (backend-convert-object (event-value event sparam) sparam backend)
         :if (or (eql :gate sparam)
                 (not (null val)))
         :append (list (if (eql :group sparam) ;; :group is an alias for :to
                           :to
                           sparam)
                       (if (eql :gate sparam) 1 val))))
    (copy-list (event-plist event)))) ;; if we don't have data for the instrument, all we can do is return the plist for the event and hope for the best.

(defgeneric backend-node-p (object backend)
  (:documentation "True if OBJECT is a node for the specified backend."))

(defgeneric backend-timestamps-for-event (event task backend)
  (:documentation "Get a list containing timestamps for the start and end of EVENT's note, in the format that BACKEND expects for its scheduling function.

See also: `backend-control-node-at'"))

(defgeneric backend-proxys-node (id backend)
  (:documentation "Get the current node object on BACKEND for the proxy with the specified ID."))

(defgeneric backend-control-node-at (time node params backend)
  (:documentation "At TIME, set NODE's parameters to PARAMS. If NODE is an instrument name, launch a node with those parameters at the specified time instead. This function should return the node object."))

(defgeneric backend-convert-object (object key backend)
  (:documentation "Convert OBJECT to a value that BACKEND can understand. For example, the SuperCollider backend requires that any `cl-collider::buffer' objects are converted to their bufnum."))

(defmethod backend-convert-object (object key backend)
  (declare (ignore key))
  object)
