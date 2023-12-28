;;;; debug.lisp - the debug backend for cl-patterns.
;;; simply prints and stores incoming events for debugging purposes.

(in-package #:cl-patterns)

(defclass debug-backend (backend)
  ((name :initform "debug")
   (print-stream :initarg :print-stream :initform nil :accessor debug-backend-print-stream :documentation "The stream to print debugging output to, or nil if no printing should be done.")
   (events :initform nil :accessor debug-backend-events :type list :documentation "List of events received by the backend, with the most recent events first."))
  (:documentation "Pseudo-backend for capturing generated events."))

(setf (documentation 'debug-backend-print-stream 'function) "The stream to print debugging output to, or nil if no printing should be done."
      (documentation 'debug-backend-events 'function) "List of events received by the backend, with the most recent events first.")

(defmethod backend-start ((backend debug-backend) &key)
  (format (debug-backend-print-stream backend) "~&Starting debug backend (i.e. doing nothing).~%"))

(defmethod backend-stop ((backend debug-backend))
  (format (debug-backend-print-stream backend) "~&Stopping debug backend (i.e. doing nothing).~%"))

(defun (setf debug-backend-recent-events) (value &optional (backend (find-backend 'debug-backend)))
  (setf (debug-backend-events backend) value))

(defun debug-backend-recent-events (&optional (backend (find-backend 'debug-backend)) (n 10))
  "Get the N most recent events recorded to the debug backend (most recent first). You will of course need to enable the debug backend first in order to record events with it.

See also: `ptrace'"
  (loop :repeat n
        :for i :in (debug-backend-events backend)
        :collect i))

(defun debug-backend-clear-recent-events (&optional (backend (find-backend 'debug-backend)))
  "Clear the log of events captured by a `debug-backend'.

See also: `debug-backend-recent-events'"
  (setf (debug-backend-events backend) nil))

(defmethod backend-play-event ((backend debug-backend) event task)
  (declare (ignore task))
  (format (debug-backend-print-stream backend) "~&Debug: playing event: ~S~%" event)
  (push event (debug-backend-events backend)))

(defmethod backend-task-removed ((backend debug-backend) task)
  (format (debug-backend-print-stream backend) "~&Debug: task removed: ~S~%" task))

(defmethod backend-timestamps-for-event ((backend debug-backend) event task)
  nil)

(export '(debug-backend debug-backend-print-stream debug-backend-events debug-backend-recent-events debug-backend-clear-recent-events))
