(in-package #:cl-patterns)

(defvar *enabled-backends* nil
  "List of registered and enabled backends for cl-patterns. Any event that is played is tested against each of these backends' respond-p functions in order. The event is then played on the first backend to return true.")

(defvar *backends* nil
  "Plist of registered backends for cl-patterns. This holds all the information about each backend and should not be modified by the user; change the `*enabled-backends*' variable to set which backends are enabled.")

(defun register-backend (name)
  "Register a cl-patterns backend."
  (pushnew (alexandria:make-keyword name) *backends*))

(defun all-backends ()
  "Get a list of all registered backends."
  (keys *backends*))

(defun enabled-backends ()
  "Get a list of all enabled backends."
  *enabled-backends*)

(defun enable-backend (name &optional (start t))
  "Enable a registered backend. With START, start the backend server."
  (let ((name (alexandria:make-keyword name)))
    (assert (position name *backends*) (name) "No backend named ~s registered." name)
    (if (member name *enabled-backends*)
        (warn "Backend ~a already enabled; doing nothing." name)
        (progn
          (when start
            (start-backend name))
          (pushnew name *enabled-backends*)))))

(defun disable-backend (name &optional (stop t))
  "Disable a registered backend."
  (let ((name (alexandria:make-keyword name)))
    (when stop
      (stop-backend name))
    (setf *enabled-backends*
          (delete name *enabled-backends*))))

;;; generics

(defgeneric start-backend (backend) ;; FIX
  (:documentation "Start a backend. By default, this is automatically called when `enable-backend' is run."))

(defgeneric stop-backend (backend) ;; FIX
  (:documentation "Stop a backend. This is automatically called when `disable-backend' is run."))

(defgeneric backend-plays-event-p (event backend)
  (:documentation "Method returning true when BACKEND should play EVENT."))

(defgeneric backend-play-event (event task backend)
  (:documentation "Play EVENT using BACKEND."))

(defgeneric backend-task-removed (task backend)
  (:documentation "Called when TASK is removed from the clock so BACKEND can free any associated nodes."))
