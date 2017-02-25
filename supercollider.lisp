(in-package :cl-patterns)

(defgeneric play-sc (item))

(defmethod play-sc ((item t)) ;; TODO: move the params stuff into its own function
  (unless (eq (get-event-value item :type) :rest)
    (let* ((inst (instrument item))
           (synth-params (getf sc::*synthdef-metadata* inst))
           (time (+ (or (raw-get-event-value item :latency) *latency*) (or (raw-get-event-value item :unix-time-at-start) (sc:now))))
           (params (if synth-params ;; make the parameters list for sc:synth.
                       (alexandria:flatten
                        (remove-if #'null
                                   (loop :for sparam :in (mapcar #'car synth-params)
                                      :collect (let ((val (get-event-value item sparam)))
                                                 (if val
                                                     (cons sparam (if (eq :gate sparam)
                                                                      1
                                                                      val))
                                                     nil)))))
                       (copy-list (event-plist item))))
           (params (mapcar (lambda (list-item) ;; replace buffers in the parameters list with their buffer numbers.
                             (if (typep list-item 'sc::buffer)
                                 (sc:bufnum list-item)
                                 list-item))
                           params)))
      (let ((node (sc:at time
                    (sc:synth inst params))))
        (when (sc:has-gate-p node)
          (sc:at (+ time (dur-time (sustain item)))
            (sc:release node)))))))

(setf *event-output-function* 'play-sc)
