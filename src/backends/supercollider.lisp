;;; supercollider.lisp - the SuperCollider/cl-collider backend for cl-patterns.
;; FIX: changing :instrument in pmono causes the old one to stay playing.

(in-package :cl-patterns)

;;; helper functions

(defun get-synthdef-control-names (name)
  (mapcar #'car (sc::get-synthdef-metadata name :controls)))

(defgeneric has-gate-p (item))

(defmethod has-gate-p ((item t))
  (position :gate (get-synthdef-control-names item) :test #'string-equal))

(defmethod has-gate-p ((item sc::node))
  (has-gate-p (sc::name item)))

(defmethod has-gate-p ((item string))
  (has-gate-p (alexandria:make-keyword (string-upcase item))))

(defun generate-plist-for-synth (instrument event)
  "Generate a plist of parameters for a synth based off of the synth's arguments. Unlike `event-plist', this function doesn't include event keys that aren't also one of the synth's arguments."
  (when (not (sc::get-synthdef-metadata instrument)) ;; if we don't have data for the synth, simply return a plist for the event and hope for the best.
    (return-from generate-plist-for-synth (copy-list (event-plist event))))
  (let ((synth-params (remove-if (lambda (arg) ;; for parameters unspecified by the event, we fall back to the synth's defaults, NOT the event's...
                                   (unless (string= (symbol-name arg) "SUSTAIN") ;; ...with the exception of sustain, which the synth should always get.
                                     (multiple-value-bind (value key) (event-value event arg)
                                       (declare (ignore value))
                                       (eq key t))))
                                 (get-synthdef-control-names instrument))))
    (loop :for sparam :in synth-params
       :for val = (event-value event sparam)
       :if (or (eq :gate sparam)
               (not (null val)))
       :append (list (alexandria:make-keyword sparam) (if (eq :gate sparam) 1 val)))))

(defmethod convert-object ((object sc::buffer))
  (sc:bufnum object))

(defmethod convert-object ((object sc::bus))
  (sc:busnum object))

(defun get-proxys-node-id (name)
  "Get the current node ID of the proxy NAME, or NIL if it doesn't exist in cl-collider's node-proxy-table."
  (if (typep name 'sc::node)
      (get-proxys-node-id (alexandria:make-keyword (string-upcase (slot-value name 'sc::name))))
      (slot-value (gethash name (sc::node-proxy-table sc::*s*))
                  'sc::id)))

;;; backend functions

(defun is-sc-event-p (event)
  (let ((inst (event-value event :instrument)))
    (or (gethash inst sc::*synthdef-metadata*)
        (typep inst 'sc::node))))

(defun play-sc (item &optional task)
  "Play ITEM on the SuperCollider sound server. TASK is an internal parameter used when this function is called from the clock."
  (unless (eq (event-value item :type) :rest)
    (let* ((inst (instrument item))
           (quant (alexandria:ensure-list (quant item)))
           (offset (if (> (length quant) 2)
                       (nth 2 quant)
                       0))
           (time (+ (or (raw-event-value item :latency) *latency*)
                    (or (timestamp-to-cl-collider (raw-event-value item :timestamp-at-start)) (sc:now))
                    offset))
           (params (loop :for (key value) :on (generate-plist-for-synth inst item) :by #'cddr
                      :append (list key (convert-object value)))))
      (if (or (and (eq (event-value item :type) :mono)
                   (not (null task))) ;; if the user calls #'play manually, then we always start a note instead of checking if a node already exists.
              (typep inst 'sc::node))
          (let ((node (sc:at time
                        (cond ((not (null (slot-value task 'nodes)))
                               (apply #'sc:ctrl (car (slot-value task 'nodes)) params))
                              ((typep inst 'sc::node)
                               ;; redefining a proxy changes its Node's ID.
                               ;; thus if the user redefines a proxy, the Node object previously provided to the pbind will become inaccurate.
                               ;; thus we look up the proxy in the node-proxy-table to ensure we always have the correct ID.
                               (let ((node (or (get-proxys-node-id inst)
                                               inst)))
                                 (apply #'sc:ctrl node params)))
                              (t
                               (apply #'sc:synth inst params))))))
            (unless (or (typep inst 'sc::node)
                        (not (has-gate-p inst)))
              (if (< (legato item) 1)
                  (sc:at (+ time (dur-time (sustain item)))
                    (setf (slot-value task 'nodes) (list))
                    (release-sc node))
                  (setf (slot-value task 'nodes) (list node)))))
          (let ((node (sc:at time
                        (apply #'sc:synth inst params))))
            (when (has-gate-p node)
              (sc:at (+ time (dur-time (sustain item)))
                (release-sc node))))))))

(defun release-sc (node)
  (sc:release node))

(defun release-sc-at (time node)
  (sc:at time
    (release-sc node)))

(defun timestamp-to-cl-collider (timestamp)
  "Convert a local-time timestamp to the format used by cl-collider."
  (+ (local-time:timestamp-to-unix timestamp) (* (local-time:nsec-of timestamp) 1.0d-9)))

(defmethod release ((object sc::node))
  (sc::release object))

(register-backend :cl-collider
                  :respond-p 'is-sc-event-p
                  :play 'play-sc
                  :release 'release-sc
                  :release-at 'release-sc-at)

(enable-backend :cl-collider)
