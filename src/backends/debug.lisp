;;;; debug.lisp - the debug backend for cl-patterns; simply prints incoming events for debugging purposes.

(in-package #:cl-patterns)

(defclass debug-backend (backend)
  ((name :initform "debug")
   (print-stream :initarg :print-stream :initform nil :accessor debug-backend-print-stream :documentation "The stream to print debugging output to, or nil if no printing should be done.")
   (recent-events :initform (list) :writer (setf debug-backend-recent-events) :type list :documentation "List of events received by the backend, with the most recent events first.")))

(defmethod backend-start ((backend debug-backend) &key)
  (format (debug-backend-print-stream backend) "~&Starting debug backend (i.e. doing nothing).~%"))

(defmethod backend-stop ((backend debug-backend))
  (format (debug-backend-print-stream backend) "~&Stopping debug backend (i.e. doing nothing).~%"))

(defun debug-backend-recent-events (&optional (backend (find-backend 'debug-backend)) (n 10))
  "Get the N most recent events recorded to the debug backend (most recent first). You will of course need to enable the debug backend first in order to record events with it.

See also: `ptrace'"
  (loop :for i :in (slot-value backend 'recent-events)
        :repeat n
        :collect i))

(defun debug-backend-clear-recent-events (&optional (backend (find-backend 'debug-backend)))
  "Clear the log of events captured by a `debug-backend'.

See also: `debug-backend-recent-events'"
  (setf (slot-value backend 'recent-events) (list)))

(defmethod backend-play-event ((backend debug-backend) event task)
  (declare (ignore task))
  (format (debug-backend-print-stream backend) "~&Debug: playing event: ~S~%" event)
  (push event (debug-backend-recent-events backend)))

(defmethod backend-task-removed ((backend debug-backend) task)
  (format (debug-backend-print-stream backend) "~&Debug: task removed: ~S~%" task))

(export '(debug-backend-recent-events debug-backend-clear-recent-events debug-recent-events debug-clear-events *debug-print-events*))

;;; deprecated

(define-symbol-macro *debug-backend-events* (deprecated-debug-backend-events))

(defun deprecated-debug-backend-events ()
  "Deprecated alias for `(debug-backend-recent-events (find-backend 'debug-backend))'."
  (warn "Using ~S is deprecated; please use ~S instead." '*debug-backend-events* 'debug-backend-recent-events)
  (debug-backend-recent-events (find-backend 'debug-backend)))

(uiop:with-deprecation (:error)
  (defun debug-recent-events (&optional (n 10))
    "Deprecated alias for `debug-backend-recent-events'."
    (debug-backend-recent-events (find-backend 'debug-backend) n))

  (defun debug-clear-events ()
    "Deprecated alias for `debug-backend-clear-recent-events'."
    (debug-backend-clear-recent-events (find-backend 'debug-backend))))

(define-symbol-macro *debug-print-events* (deprecated-debug-print-events))

(setf (documentation '*debug-print-events* 'variable) "Deprecated variable; use (debug-backend-print-stream (find-backend 'debug-backend)) instead.")

(defun deprecated-debug-print-events ()
  "Deprecated alias for (debug-backend-print-stream (find-backend 'debug-backend))."
  (warn "Using ~S is deprecated; please use ~S instead." '*debug-print-events* 'debug-backend-print-stream)
  (debug-backend-print-stream (find-backend 'debug-backend)))

(defun (setf deprecated-debug-print-events) (value)
  "Deprecated alias for (setf (debug-backend-print-stream (find-backend 'debug-backend)) ...)."
  (warn "Using ~S is deprecated; please use ~S instead." '*debug-print-events* 'debug-backend-print-stream)
  (setf (debug-backend-print-stream (find-backend 'debug-backend)) value))
