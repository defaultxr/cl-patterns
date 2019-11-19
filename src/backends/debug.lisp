;;; debug.lisp - the debug backend for cl-patterns.
;; this is just used to print incoming backend events, for debugging purposes.

(in-package #:cl-patterns)

(defmethod start-backend ((backend (eql :debug)))
  (format t "~&Starting debug backend (i.e. doing nothing).~%"))

(defmethod stop-backend ((backend (eql :debug)))
  (format t "~&Stopping debug backend (i.e. doing nothing).~%"))

(defmethod backend-plays-event-p (event (backend (eql :debug)))
  t)

(defparameter *debug-backend-events* (list)
  "A list of all events received by the debug backend, with the most recent events first.")

(defun debug-recent-events (&optional (n 10))
  "Get the N most recent events that the debug backend received."
  (loop :for i :in *debug-backend-events*
     :repeat n
     :collect i))

(defun debug-clear-events ()
  "Clear the log of events captured by the cl-patterns debug backend."
  (setf *debug-backend-events* (list)))

(defparameter *debug-print-events* t
  "If T, the debug backend will print events it plays; if NIL, it will only record them to `*debug-backend-events*'.")

(export '(debug-recent-events debug-clear-events *debug-print-events*))

(defmethod backend-play-event (item task (backend (eql :debug)))
  (declare (ignore task))
  (when *debug-print-events*
    (format t "~&Debug: playing event: ~s~%" item))
  (push item *debug-backend-events*))

(defmethod backend-task-removed (task (backend (eql :debug)))
  (format t "~&Debug: task removed: ~s~%" task))

(register-backend :debug)
