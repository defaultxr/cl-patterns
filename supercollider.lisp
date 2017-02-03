(in-package :cl-patterns)

(defgeneric play-sc (item))

(defmethod play-sc ((item t))
  (unless (eq (get-event-value item :type) :rest)
    (let ((params (copy-list (event-plist item))))
      (remf params :instrument)
      (sc:synth (instrument item) params))))

(setf *event-output-function* 'play-sc)
