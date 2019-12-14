;;; supercollider.lisp - the SuperCollider/cl-collider backend for cl-patterns.
;; FIX: changing :instrument in pmono causes the old one to stay playing.
;; FIX: multichannel expansion breaks :mono

(in-package :cl-patterns)

;;; global settings

(defvar *cl-collider-buffer-preview-synth* :sp
  "The name of the synth to use to `play' a buffer.")

;;; helper functions

(defun timestamp-to-cl-collider (timestamp)
  "Convert a local-time timestamp to the format used by cl-collider."
  (+ (local-time:timestamp-to-unix timestamp) (* (local-time:nsec-of timestamp) 1.0d-9)))

(defmethod synth-controls (synth (backend (eql :supercollider)))
  (mapcar #'car (cl-collider:synthdef-metadata synth :controls)))

(defun supercollider-make-synth-args-list (instrument event)
  "Generate a plist of parameters for a synth based off of the synth's arguments. Unlike `event-plist', this function doesn't include event keys that aren't also one of the synth's arguments."
  (when (not (cl-collider:synthdef-metadata instrument)) ;; if we don't have data for the synth, simply return a plist for the event and hope for the best.
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
       :for sparam = (make-keyword param)
       :for val = (supercollider-convert-object (event-value event sparam) sparam)
       :if (or (eql :gate sparam)
               (not (null val)))
       :append (list (if (eql :group sparam) ;; :group is an alias for :to
                         :to
                         sparam)
                     (if (eql :gate sparam) 1 val)))))

(defun cl-collider-proxy (name)
  "Get the object representing the `cl-collider:proxy' with the given name."
  (gethash name (cl-collider::node-proxy-table cl-collider::*s*)))

(defun get-proxys-node-id (name)
  "Get the current node ID of the proxy NAME, or NIL if it doesn't exist in cl-collider's node-proxy-table."
  (if (typep name 'cl-collider::node)
      (get-proxys-node-id (make-keyword (string-upcase (slot-value name 'cl-collider::name))))
      (when-let ((val (cl-collider-proxy name)))
        (cl-collider::id val))))

(defgeneric supercollider-convert-object (object key)
  (:documentation "Method used to convert objects in events to values the SuperCollider server can understand. For example, any `cl-collider::buffer' objects are converted to their bufnum."))

(defmethod supercollider-convert-object ((object t) key)
  (declare (ignore key))
  object)

(defmethod supercollider-convert-object ((object cl-collider::buffer) key)
  (declare (ignore key))
  (cl-collider:bufnum object))

(defmethod supercollider-convert-object ((object cl-collider::bus) key)
  (declare (ignore key))
  (cl-collider:busnum object))

(defmethod supercollider-convert-object ((object cl-collider::node) key)
  (let ((bus (if (eql key :out)
                 (cl-collider:synthdef-metadata object :input-bus)
                 (or (cl-collider:synthdef-metadata object :output-bus)
                     (cl-collider:synthdef-metadata object :input-bus)))))
    (if bus
        (cl-collider:busnum bus)
        object)))

(defmethod supercollider-convert-object ((object cl-collider::group) key)
  (declare (ignore key))
  (cl-collider::id object))

(defvar *supercollider-node-map* (make-node-map)
  "Hash mapping cl-patterns tasks to SuperCollider nodes.")

;;; backend functions

(defmethod start-backend ((backend (eql :supercollider)))
  (setf cl-collider:*s* (cl-collider:make-external-server "localhost" :port 4444))
  (cl-collider:server-boot cl-collider:*s*))

(defmethod stop-backend ((backend (eql :supercollider)))
  (cl-collider:server-quit cl-collider:*s*))

(defmethod backend-play-event (item task (backend (eql :supercollider)))
  "Play ITEM on the SuperCollider sound server. TASK is an internal parameter used when this function is called from the clock."
  (let ((type (event-value item :type))
        (inst (instrument item)))
    (when (and (not (position type (list :rest :tempo-change))) ;; FIX: make sure other backends ignore tempo-change too.
               (or (cl-collider:synthdef-metadata inst)
                   (typep inst 'cl-collider::node)))
      (let* ((quant (quant item))
             (offset (if (> (length quant) 2)
                         (nth 2 quant)
                         0))
             (time (+ (or (raw-event-value item :latency) *latency*)
                      (or (timestamp-to-cl-collider (raw-event-value item :timestamp-at-start)) (cl-collider:now))
                      (time-dur (or (raw-event-value item :timing-offset) 0) (tempo *clock*))
                      offset))
             (params (supercollider-make-synth-args-list inst item)))
        (if (or (eql type :mono)
                (typep inst 'cl-collider::node))
            (let ((node (cl-collider:at time
                          (let ((nodes (task-nodes task *supercollider-node-map*)))
                            (cond ((not (null nodes))
                                   (apply #'cl-collider:ctrl (car nodes) params))
                                  ((typep inst 'cl-collider::node)
                                   ;; redefining a proxy changes its Node's ID.
                                   ;; thus if the user redefines a proxy, the Node object previously provided to the pbind will become inaccurate.
                                   ;; thus we look up the proxy in the node-proxy-table to ensure we always have the correct ID.
                                   (let ((node (or (get-proxys-node-id inst)
                                                   inst)))
                                     (apply #'cl-collider:ctrl node params)))
                                  (t
                                   (apply #'cl-collider:synth inst params)))))))
              (unless (or (typep inst 'cl-collider::node)
                          (not (has-gate-p inst :supercollider)))
                (if (< (legato item) 1)
                    (cl-collider:at (+ time (dur-time (sustain item)))
                      (setf (task-nodes task *supercollider-node-map*) nil)
                      (cl-collider:release node))
                    (setf (task-nodes task *supercollider-node-map*) (list node)))))
            (let ((node (cl-collider:at time
                          (apply #'cl-collider:synth inst params))))
              (when (has-gate-p node :supercollider)
                (cl-collider:at (+ time (dur-time (sustain item)))
                  (cl-collider:release node)))))))))

(defmethod backend-task-removed (task (backend (eql :supercollider)))
  (let ((item (slot-value task 'item)))
    (unless (typep item 'event)
      (let ((last-output (last-output item)))
        (dolist (node (task-nodes task *supercollider-node-map*))
          (cl-collider:at (timestamp-to-cl-collider
                           (absolute-beats-to-timestamp (+ (slot-value task 'start-beat) (beat last-output) (sustain last-output))
                                                        (slot-value task 'clock)))
            (cl-collider:release node))))))
  (setf (task-nodes task *supercollider-node-map*) nil))

;;; convenience methods

(defmethod play ((object cl-collider::node))
  t)

(defmethod stop ((object cl-collider::node))
  (cl-collider:free object))

(defmethod end ((object cl-collider::node))
  (cl-collider:release object))

(defmethod playing-p ((node cl-collider::node) &optional (server cl-collider:*s*))
  (when (position (cl-collider::id node) (cl-collider::node-watcher server))
    t))

(defmethod play ((buffer cl-collider::buffer))
  (play (event :instrument *cl-collider-buffer-preview-synth*
               :bufnum (cl-collider:bufnum buffer) ;; FIX: send as :buffer or :bufnum key, depending on what the synthdef actually has.
               :quant 0
               :latency 0)))

(register-backend :supercollider)

;; (enable-backend :supercollider)
