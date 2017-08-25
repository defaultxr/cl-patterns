(in-package :cl-patterns)

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
           (synth-params (get-synth-args-list inst))
           ;; (synth-params (remove-if-not (lambda (arg) (position arg (keys item))) (get-synth-args-list inst))) ;; FIX: not a good solution (i.e. what if midinote is provided by the event but the synth's key is freq? it will not be seen...)
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
           (params (mapcar (lambda (list-item) ;; replace buffers in the parameters list with their buffer numbers.
                             (if (typep list-item 'sc::buffer)
                                 (sc:bufnum list-item)
                                 list-item))
                           params)))
      (if (and (eq (get-event-value item :type) :mono)
               (not (null task)))
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
