(in-package :cl-patterns)

(require :sc)

(defgeneric play-sc (item))

(defmethod play-sc ((item t))
  (let ((params (event-plist item)))
    (remf params :instrument)
    (sc:synth (instrument item) params)))

