(in-package #:cl-patterns)

;;; helpers

(defgeneric synth-controls (synth backend)
  (:documentation "Get the list of names of controls for SYNTH in BACKEND."))

(defun has-gate-p (synth backend)
  "Whether or not SYNTH in BACKEND has a gate control (i.e. whether it needs to be manually released or if it acts as a \"one-shot\")."
  (position :gate (synth-controls synth backend) :test 'string-equal))

(defun make-node-map ()
  "Make a hash-table to hold task->node mappings for a backend."
  (make-hash-table :test 'eq))

(defun task-nodes (task map)
  "Get the list of nodes for TASK from MAP."
  (gethash task map))

(defun (setf task-nodes) (value task map)
  "Set the list of nodes for TASK in MAP. If VALUE is nil, delete the reference to TASK in MAP."
  (if (null value)
      (remhash task map)
      (setf (gethash task map) value)))

(defun backends-for-event (event)
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
  "Get a list of all registered backends."
  (keys *backends*))

(defun enabled-backends ()
  "Get a list of all enabled backends."
  *enabled-backends*)

(defun enable-backend (name)
  "Enable a registered backend."
  (let ((name (make-keyword name)))
    (assert (position name *backends*) (name) "No backend named ~s registered." name)
    (if (member name *enabled-backends*)
        (warn "Backend ~a already enabled; doing nothing." name)
        (pushnew name *enabled-backends*))))

(defun disable-backend (name)
  "Disable a registered backend."
  (let ((name (make-keyword name)))
    (setf *enabled-backends*
          (delete name *enabled-backends*))))

;;; generics

(defgeneric start-backend (backend) ;; FIX
  (:documentation "Start a backend. By default, this is automatically called when `enable-backend' is run."))

(defgeneric stop-backend (backend) ;; FIX
  (:documentation "Stop a backend. This is automatically called when `disable-backend' is run."))

(defgeneric backend-play-event (event task backend)
  (:documentation "Play EVENT using BACKEND."))

(defgeneric backend-task-removed (task backend)
  (:documentation "Called when TASK is removed from the clock so BACKEND can free any associated nodes."))
