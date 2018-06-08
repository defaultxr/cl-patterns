(in-package #:cl-patterns)

(defparameter *enabled-backends* nil
  "List of registered and enabled backends for cl-patterns. Any event that is played is tested against each of these backends' respond-p functions in order. The event is then played on the first backend to return true.")

(defparameter *backends* nil
  "Plist of registered backends for cl-patterns. This holds all the information about each backend and should not be modified by the user; change the `*enabled-backends*' variable to set which backends are enabled.")

(defun register-backend (name &key respond-p play release release-at)
  "Register a cl-patterns backend."
  (let ((name (alexandria:make-keyword name)))
    (setf *backends*
          (plist-set *backends* name (list :name name :respond-p respond-p :play play :release release :release-at release-at)))))

(defun all-backends ()
  "Get a list of all registered backends."
  (keys *backends*))

(defun enabled-backends ()
  "Get a list of all enabled backends."
  *enabled-backends*)

(defun enable-backend (name)
  "Enable a registered backend."
  (assert (position (alexandria:make-keyword name)
                    (keys *backends*))
          (name) "No backend named ~s registered." name)
  (pushnew (alexandria:make-keyword name) *enabled-backends*))

(defun disable-backend (name)
  "Disable a registered backend."
  (setf *enabled-backends*
        (delete (alexandria:make-keyword name) *enabled-backends*)))

(defun which-backend-for-event (event)
  "Return the name of the first backend in `*enabled-backends*' that will handle EVENT."
  (loop :for i :in *enabled-backends*
     :if (funcall (getf (getf *backends* i) :respond-p) event)
     :return i))

(defgeneric convert-object (object)
  (:documentation "Convert objects in events to a format that the backend will accept."))

(defmethod convert-object ((object t))
  object)
