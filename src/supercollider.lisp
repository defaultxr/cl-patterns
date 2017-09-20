(in-package :cl-patterns)

;; FIX: `at' and `release' currently have to be defined here since clock.lisp uses them.

(defmacro at (time &body body)
  `(sc:at ,time ,@body))

(defun release (node)
  (sc:release node))

(defun local-time-cl-collider (time)
  "Convert a local-time timestamp into the timestamp format used by cl-collider."
  (+ (local-time:timestamp-to-unix time) (* (local-time:timestamp-microsecond time) 1.0d-6)))

(defun get-synth-args-list (synth)
  "Return the argument list for a synth defined with `defsynth*'."
  (or (mapcar #'car (getf sc::*synthdef-metadata* synth))
      (mapcar #'alexandria:make-keyword
              (remove-if (lambda (symbol)
                           (or
                            (position symbol (list '&key 'sc::end-f))
                            (consp symbol)))
                         (swank-backend:arglist (fdefinition (alexandria:ensure-symbol synth))))))) ;; FIX: remove this swank dependency

(defun play-sc (item &optional task) ;; FIX: move the params stuff into its own function
  (unless (eq (get-event-value item :type) :rest)
    (let* ((inst (instrument item))
           (synth-params (remove-if (lambda (arg) ;; for parameters unspecified by the event, we fall back to the synth's defaults, NOT the event's.
                                      (multiple-value-bind (value key) (get-event-value item arg)
                                        (declare (ignore value))
                                        (eq key t)))
                                    (get-synth-args-list inst)))
           (quant (alexandria:ensure-list (quant item)))
           (offset (if (> (length quant) 2)
                       (nth 2 quant)
                       0))
           (time (+ (or (raw-get-event-value item :latency) *latency*)
                    (or (raw-get-event-value item :unix-time-at-start) (sc:now))
                    offset))
           (params (if synth-params ;; make the parameters list for sc:synth.
                       (apply #'append
                              (remove-if #'null
                                         (loop :for sparam :in synth-params
                                            :collect (let ((val (get-event-value item sparam)))
                                                       (when val
                                                         (list sparam (if (eq :gate sparam)
                                                                          1
                                                                          val)))))))
                       (copy-list (event-plist item))))
           (params (mapcar (lambda (list-item) ;; replace buffers, buses, etc in the parameters list with their numbers.
                             (typecase list-item
                               (sc::buffer (sc:bufnum list-item))
                               (sc::bus (sc:busnum list-item))
                               (otherwise list-item)))
                           params)))
      (if (and (eq (get-event-value item :type) :mono)
               (not (null task))) ;; if the user calls #'play manually, then we always start a note instead of checking if a node already exists.
          (let ((node (at time
                        (if (and (not (null (slot-value task 'nodes))))
                            (apply #'sc:ctrl (car (slot-value task 'nodes)) params)
                            (sc:synth inst params)))))
            (if (< (legato item) 1)
                (at (+ time (dur-time (sustain item)))
                  (setf (slot-value task 'nodes) (list))
                  (release node))
                (setf (slot-value task 'nodes) (list node))))
          (let ((node (at time
                        (sc:synth inst params))))
            (when (sc:has-gate-p node)
              (at (+ time (dur-time (sustain item)))
                (release node))))))))

(setf *event-output-function* 'play-sc)
