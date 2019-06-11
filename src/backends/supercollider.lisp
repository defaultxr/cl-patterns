;;; supercollider.lisp - the SuperCollider/cl-collider backend for cl-patterns.
;; FIX: changing :instrument in pmono causes the old one to stay playing.
;; FIX: multichannel expansion breaks :mono

(in-package :cl-patterns)

;;; helper functions

(defun timestamp-to-cl-collider (timestamp)
  "Convert a local-time timestamp to the format used by cl-collider."
  (+ (local-time:timestamp-to-unix timestamp) (* (local-time:nsec-of timestamp) 1.0d-9)))

(defmethod synth-controls (synth (backend (eql :supercollider)))
  (mapcar #'car (sc::get-synthdef-metadata synth :controls)))

(defun supercollider-make-synth-args-list (instrument event)
  "Generate a plist of parameters for a synth based off of the synth's arguments. Unlike `event-plist', this function doesn't include event keys that aren't also one of the synth's arguments."
  (when (not (sc::get-synthdef-metadata instrument)) ;; if we don't have data for the synth, simply return a plist for the event and hope for the best.
    (return-from supercollider-make-synth-args-list (copy-list (event-plist event))))
  (let ((synth-params (remove-if (lambda (arg) ;; for parameters unspecified by the event, we fall back to the synth's defaults, NOT the event's...
                                   (unless (string= (symbol-name arg) "SUSTAIN") ;; ...with the exception of sustain, which the synth should always get.
                                     (multiple-value-bind (value key) (event-value event arg)
                                       (declare (ignore value))
                                       (eql key t))))
                                 (append (synth-controls instrument :supercollider)
                                         (list :group :to :id)))))
    ;; get the value of each of the synth's arguments from the event...
    (loop :for param :in synth-params
       :for sparam = (alexandria:make-keyword param)
       :for val = (supercollider-convert-object (event-value event sparam) sparam)
       :if (or (eql :gate sparam)
               (not (null val)))
       :append (list (if (eql :group sparam) ;; :group is an alias for :to
                         :to
                         sparam)
                     (if (eql :gate sparam) 1 val)))))

(defun get-proxys-node-id (name)
  "Get the current node ID of the proxy NAME, or NIL if it doesn't exist in cl-collider's node-proxy-table."
  (if (typep name 'sc::node)
      (get-proxys-node-id (alexandria:make-keyword (string-upcase (slot-value name 'sc::name))))
      (alexandria:when-let ((val (gethash name (sc::node-proxy-table sc::*s*))))
        (sc::id val))))

(defgeneric supercollider-convert-object (object key)
  (:documentation "Method used to convert objects in events to values the SuperCollider server can understand. For example, any `cl-collider::buffer' objects are converted to their bufnum."))

(defmethod supercollider-convert-object ((object t) key)
  (declare (ignore key))
  object)

(defmethod supercollider-convert-object ((object sc::buffer) key)
  (declare (ignore key))
  (sc:bufnum object))

(defmethod supercollider-convert-object ((object sc::bus) key)
  (declare (ignore key))
  (sc:busnum object))

(defmethod supercollider-convert-object ((object sc::node) key)
  (let ((bus (if (eql key :out)
                 (sc:get-synthdef-metadata object :input-bus)
                 (or (sc:get-synthdef-metadata object :output-bus)
                     (sc:get-synthdef-metadata object :input-bus)))))
    (if bus
        (sc:busnum bus)
        object)))

(defmethod supercollider-convert-object ((object sc::group) key)
  (declare (ignore key))
  (sc::id object))

(defvar *supercollider-node-map* (make-node-map)
  "Hash mapping cl-patterns tasks to SuperCollider nodes.")

;;; backend functions

(defmethod start-backend ((backend (eql :supercollider)))
  (setf sc:*s* (sc:make-external-server "localhost" :port 4444))
  (sc:server-boot sc:*s*))

(defmethod stop-backend ((backend (eql :supercollider)))
  (sc:server-quit sc:*s*))

(defmethod backend-plays-event-p (event (backend (eql :supercollider)))
  (let ((inst (event-value event :instrument)))
    (or (gethash inst sc::*synthdef-metadata*)
        (typep inst 'sc::node))))

(defmethod backend-play-event (item task (backend (eql :supercollider)))
  "Play ITEM on the SuperCollider sound server. TASK is an internal parameter used when this function is called from the clock."
  (unless (eql (event-value item :type) :rest)
    (let* ((inst (instrument item))
           (quant (alexandria:ensure-list (quant item)))
           (offset (if (> (length quant) 2)
                       (nth 2 quant)
                       0))
           (time (+ (or (raw-event-value item :latency) *latency*)
                    (or (timestamp-to-cl-collider (raw-event-value item :timestamp-at-start)) (sc:now))
                    offset))
           (params (supercollider-make-synth-args-list inst item)))
      (if (or (eql (event-value item :type) :mono)
              (typep inst 'sc::node))
          (let ((node (sc:at time
                        (let ((nodes (task-nodes task *supercollider-node-map*)))
                          (cond ((not (null nodes))
                                 (apply #'sc:ctrl (car nodes) params))
                                ((typep inst 'sc::node)
                                 ;; redefining a proxy changes its Node's ID.
                                 ;; thus if the user redefines a proxy, the Node object previously provided to the pbind will become inaccurate.
                                 ;; thus we look up the proxy in the node-proxy-table to ensure we always have the correct ID.
                                 (let ((node (or (get-proxys-node-id inst)
                                                 inst)))
                                   (apply #'sc:ctrl node params)))
                                (t
                                 (apply #'sc:synth inst params)))))))
            (unless (or (typep inst 'sc::node)
                        (not (has-gate-p inst :supercollider)))
              (if (< (legato item) 1)
                  (sc:at (+ time (dur-time (sustain item)))
                    (setf (task-nodes task *supercollider-node-map*) nil)
                    (sc:release node))
                  (setf (task-nodes task *supercollider-node-map*) (list node)))))
          (let ((node (sc:at time
                        (apply #'sc:synth inst params))))
            (when (has-gate-p node :supercollider)
              (sc:at (+ time (dur-time (sustain item)))
                (sc:release node))))))))

(defmethod backend-task-removed (task (backend (eql :supercollider)))
  (let ((item (slot-value task 'item)))
    (unless (typep item 'event)
      (let ((last-output (last-output item)))
        (dolist (node (task-nodes task *supercollider-node-map*))
          (sc:at (timestamp-to-cl-collider
                  (absolute-beats-to-timestamp (+ (slot-value task 'start-beat) (beat last-output) (sustain last-output))
                                               (slot-value task 'clock)))
            (sc:release node))))))
  (setf (task-nodes task *supercollider-node-map*) nil))

;;; convenience methods

(defmethod play ((object sc::node))
  t)

(defmethod stop ((object sc::node))
  (sc:stop object))

(defmethod end ((object sc::node))
  (sc:release object))

(defmethod playing-p ((node sc::node) &optional (server sc:*s*))
  (when (position (sc::node-watcher server) (sc::id node))
    t))

(register-backend :supercollider)

;; (enable-backend :supercollider)
