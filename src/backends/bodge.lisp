;;;; bodge.lisp - the bodge/trivial-gamekit backend for cl-patterns.
;;; Note that your game has to be running first!

(in-package #:cl-patterns)

;;; helper functions

(defun gamekit-get-resource (resource)
  (ignore-errors (gamekit::resource-by-id resource)))

(defun bodge-set-sound-parameter (sound &key (pitch 1) (gain 1) (looped-p nil))
  (when-let ((source (gamekit-get-resource sound)))
    (setf (ge.snd:audio-gain source) gain)
    (setf (ge.snd:audio-pitch source) pitch)
    (setf (ge.snd:audio-looped-p source) looped-p)
    source))

(defun bodge-play-sound (sound &key (pitch 1) (gain 1) (looped-p nil))
  (when-let ((source (bodge-set-sound-parameter sound :pitch pitch :gain gain :looped-p looped-p)))
    (ge.snd:play-audio source)
    source))

;;; backend functions

(defun is-bodge-event-p (event)
  (let ((inst (event-value event :instrument)))
    (or (typep inst 'ge.snd::audio-source)
        (gamekit-get-resource inst))))

(defun play-bodge (item &optional task)
  "Play ITEM via trivial-gamekit/cl-bodge. TASK is an internal parameter used when this function is called from the clock."
  (declare (ignore task))
  (when (eql (event-value item :type) :rest)
    (return-from play-bodge))
  (let* ((inst (instrument item))
         (quant (quant item))
         (offset (if (> (length quant) 2)
                     (nth 2 quant)
                     0))
         (time (local-time:timestamp+ (or (raw-event-value item :timestamp-at-start) (local-time:now))
                                      (+ (or (raw-event-value item :latency) (slot-value (task-clock task) 'latency)) offset)
                                      :sec))
         (rate (or (event-value item :rate)
                   (multiple-value-bind (freq from) (event-value item :freq)
                     (unless (eql t from)
                       (freq-rate freq)))
                   1))
         (amp (or (event-value item :amp) 0.5))
         (looped-p (event-value item :looped-p))
         ;; (func (case (event-value item :type)
         ;;         (:set 'bodge-set-sound-parameter)
         ;;         (:note 'bodge-play-sound)
         ;;         (otherwise 'bodge-play-sound)))
         )
    (when (gamekit-get-resource inst)
      (bt:make-thread (lambda ()
                        (sleep (local-time:timestamp-difference time (local-time:now)))
                        (bodge-play-sound inst :gain amp :pitch rate :looped-p looped-p))
                      :name "cl-patterns bodge sound trigger"))
    ;; (if (or (and (eql (event-value item :type) :mono)
    ;;              (not (null task))) ; if the user calls #'play manually, then we always start a note instead of checking if a node already exists.
    ;;         (typep inst 'sc::node))
    ;;     (let ((node (sc:at time
    ;;                   (cond ((not (null (slot-value task 'nodes)))
    ;;                          (apply #'sc:ctrl (car (slot-value task 'nodes)) params))
    ;;                         ((typep inst 'sc::node)
    ;;                          ;; redefining a proxy changes its Node's ID.
    ;;                          ;; thus if the user redefines a proxy, the Node object previously provided to the pbind will become inaccurate.
    ;;                          ;; thus we look up the proxy in the node-proxy-table to ensure we always have the correct ID.
    ;;                          (let ((node (or (get-proxys-node-id inst)
    ;;                                          inst)))
    ;;                            (apply #'sc:ctrl node params)))
    ;;                         (t
    ;;                          (apply #'sc:synth inst params))))))
    ;;       (unless (or (typep inst 'sc::node)
    ;;                   (not (backend-instrument-has-gate-p inst :bodge)))
    ;;         (if (< (legato item) 1)
    ;;             (sc:at (+ time (dur-time (sustain item)))
    ;;               (setf (slot-value task 'nodes) (list))
    ;;               (release-sc node))
    ;;             (setf (slot-value task 'nodes) (list node)))))
    ;;     (let ((node (sc:at time
    ;;                   (apply #'sc:synth inst params))))
    ;;       (when (backend-instrument-has-gate-p node :bodge)
    ;;         (sc:at (+ time (dur-time (sustain item)))
    ;;           (release-sc node)))))
    ))

(defun release-bodge (source)
  (ge.snd:stop-audio (if (symbolp source)
                         (gamekit-get-resource source)
                         source)))

(defun release-bodge-at (time source)
  (return-from release-bodge-at nil)
  (scheduler:sched-add nil time (lambda () (release-bodge source))))

(defun timestamp-to-scheduler (timestamp)
  "Convert a local-time timestamp to the format used by scheduler."
  (+ (local-time:timestamp-to-unix timestamp) (* (local-time:nsec-of timestamp) 1.0d-9)))
