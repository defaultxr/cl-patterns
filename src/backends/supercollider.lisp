;;;; supercollider.lisp - the SuperCollider/cl-collider backend for cl-patterns.
;;; FIX:
;;; - changing :instrument in pmono causes the old one to stay playing.
;;; - multichannel expansion breaks :mono

(in-package #:cl-patterns)

(defclass supercollider (backend)
  ((name :initform "cl-collider")
   (server :initarg :server :accessor backend-server :type cl-collider::server :documentation "The `cl-collider::server' object for the backend.")
   (buffer-preview-synth :initarg :buffer-preview-synth :initform :spt :accessor backend-buffer-preview-synth :type symbol :documentation "The name of the synth to use to preview buffers.")
   (num-ugens :initform 0 :accessor backend-num-ugens :type (integer 0) :documentation "The last-reported number of UGens running on the server.")
   (num-synths :initform 0 :accessor backend-num-synths :type (integer 0) :documentation "The last-reported number of Synths running on the server.")
   (num-groups :initform 0 :accessor backend-num-groups :type (integer 0) :documentation "The last-reported number of Groups active on the server.")
   (num-definitions :initform 0 :accessor backend-num-definitions :type (integer 0) :documentation "The last-reported number of SynthDefs known by the server.")
   (last-status-reply-time :initform nil :accessor backend-last-status-reply-time :type (or null integer) :documentation "The internal-real-time when we last got a status update from the backend.")
   (status-request-interval :initform 7/10 :accessor backend-status-request-interval :type (real 0) :documentation "The time in seconds to sleep between each status request to the backend.")
   (status-requester-thread :initform nil :accessor backend-status-requester-thread :type (or null bt:thread) :documentation "The thread periodically requesting status updates from the backend, or nil if it is not currently running."))
  (:documentation "cl-patterns SuperCollider backend via the cl-collider system."))

(defmethod initialize-instance :after ((backend supercollider) &rest args &key (host "127.0.0.1") (port 4444) just-connect-p &allow-other-keys)
  (let ((server-options (apply #'cl-collider:make-server-options :allow-other-keys t args)))
    (setf (backend-server backend) (cl-collider:make-external-server (backend-name backend)
                                                                     :server-options server-options
                                                                     :host host
                                                                     :port port
                                                                     :just-connect-p just-connect-p))
    backend))

(defmethod backend-start ((backend supercollider) &rest rest &key (set-cl-collider-default-p t) force-boot-p &allow-other-keys)
  (declare (ignore rest))
  (let ((server (backend-server backend)))
    (when (and set-cl-collider-default-p
               (null cl-collider:*s*))
      (setf cl-collider:*s* server))
    (if (and (cl-collider:boot-p server)
             (not force-boot-p))
        (warn "Server ~S for backend ~S appears to be already booted; use ~S argument to force." server backend :force-boot-p)
        (cl-collider:server-boot server))))

(defmethod backend-stop ((backend supercollider))
  (cl-collider:server-quit (backend-server backend)))

(defmethod backend-start-status-tracker ((backend supercollider))
  (cl-collider:add-reply-responder
   "/status.reply"
   (lambda (_ ugens synths groups definitions average-cpu peak-cpu nominal-sample-rate actual-sample-rate)
     (declare (ignore _ average-cpu peak-cpu actual-sample-rate))
     (setf (backend-num-ugens backend) ugens
           (backend-num-synths backend) synths
           (backend-num-groups backend) groups
           (backend-num-definitions backend) definitions
           (cl-collider::server-options-hardware-samplerate (cl-collider:server-options (backend-server backend))) nominal-sample-rate
           (backend-last-status-reply-time backend) (get-internal-real-time))))
  (with-slots (status-requester-thread) backend
    (unless (and status-requester-thread
                 (bt:thread-alive-p status-requester-thread))
      (setf status-requester-thread (bt:make-thread (lambda ()
                                                      (sleep 1)
                                                      (while (and (backend-status-requester-thread backend)
                                                                  (cl-collider:boot-p (backend-server backend)))
                                                        (cl-collider:server-status (backend-server backend))
                                                        (sleep (backend-status-request-interval backend)))
                                                      (setf (backend-status-requester-thread backend) nil))
                                                    :name "SuperCollider server status requester")))))

;; (defmethod backend-status ((backend supercollider) &optional ()))

(defmethod backend-instrument-controls ((backend supercollider) instrument)
  (mapcar #'car (cl-collider:synthdef-metadata instrument :controls)))

(defmethod backend-all-instruments ((backend supercollider))
  (keys cl-collider::*synthdef-metadata*))

(defmethod backend-all-nodes ((backend supercollider))
  ;; cl-collider doesn't store the node objects themselves, so this method creates its own node objects.
  ;; unfortunately, that means that the objects returned by this function won't have all information, such as the 'name slot.
  (let ((server (backend-server backend)))
    (loop :for id :in (cl-collider::node-watcher server)
          :unless (member id (list 0 1))
            :collect (make-instance 'cl-collider::node :id id :server server :name nil))))

(defmethod backend-node-p ((backend supercollider) object)
  (typep object 'cl-collider::node))

(defmethod backend-panic ((backend supercollider))
  (cl-collider:group-free-all (backend-server backend)))

(defmethod backend-timestamps-for-event ((backend supercollider) event task)
  (let ((time (if-let ((timestamp (raw-event-value event :timestamp-at-start)))
                (+ (local-time:timestamp-to-unix timestamp) (* (local-time:nsec-of timestamp) 1.0d-9))
                (cl-collider:now))))
    (list time (+ time (dur-time (sustain event))))))

(defmethod backend-proxys-node ((backend supercollider) id)
  (let ((proxy-table (cl-collider::node-proxy-table (backend-server backend))))
    (etypecase id
      (symbol
       (gethash id proxy-table))
      (string
       (find id (hash-table-values proxy-table)
             :key #'cl-collider::name
             :test #'string-equal))
      (integer
       (find id (hash-table-values proxy-table)
             :key #'cl-collider::id
             :test #'=))
      (cl-collider::node
       ;; even if we're provided a node object, we still look it up in cl-collider's node-proxy-table.
       ;; this is because if the node was a proxy, it may have been redefined, which would cause its ID to change
       ;; thus causing the ID stored in the node object to be invalid.
       (backend-proxys-node backend (cl-collider::name id))))))

(defmethod backend-control-node-at ((backend supercollider) time (node symbol) params)
  (cl-collider:at time
    (apply #'cl-collider:synth node params)))

(defmethod backend-control-node-at ((backend supercollider) time (node cl-collider::node) params)
  (cl-collider:at time
    (apply #'cl-collider:ctrl node params)))

(defmethod backend-convert-object ((backend supercollider) (object cl-collider::buffer) key)
  (declare (ignore key))
  (cl-collider:bufnum object))

(defmethod backend-convert-object ((backend supercollider) (object cl-collider::bus) key)
  (declare (ignore key))
  (cl-collider:busnum object))

(defmethod backend-convert-object ((backend supercollider) (object cl-collider::node) key)
  (let ((bus (if (eql key :out)
                 (cl-collider:synthdef-metadata object :input-bus)
                 (or (cl-collider:synthdef-metadata object :output-bus)
                     (cl-collider:synthdef-metadata object :input-bus)))))
    (if bus
        (cl-collider:busnum bus)
        object)))

(defmethod backend-convert-object ((backend supercollider) (object cl-collider::group) key)
  (declare (ignore key))
  (cl-collider::id object))

;;; pseudo-ugens

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'cl-collider/cl-patterns-extensions)
    (make-package 'cl-collider/cl-patterns-extensions
                  :use (list 'cl 'cl-collider))))

(defmacro define-cl-collider-pseugen (name lambda-list &body body)
  "Define a pseudo-ugen for a function, which can then be used in cl-collider synthdefs."
  (let ((sc-name (alexandria:ensure-symbol (concat name '~) :cl-collider/cl-patterns-extensions)))
    `(progn
       (defun ,sc-name ,lambda-list
         ,@(cl-collider::convert-code body))
       (setf (cl-collider::synthdef-equivalent-function ',name) ',sc-name))))

(defmacro make-cl-collider-conversion-pseugen (function)
  "Convert FUNCTION, a function from `*conversions*', into a cl-collider pseudo-ugen using `define-cl-collider-pseugen'."
  (let* ((val (gethash function *conversions*))
         (lambda-list (getf val :lambda-list)))
    (multiple-value-bind (body declarations docstring) (parse-body (getf val :body) :documentation t)
      `(define-cl-collider-pseugen ,function ,lambda-list
         ,@(when docstring (list docstring))
         ,@declarations
         (block ,function
           ,@body)))))

;; convert all of the functions in `*conversions*'
#.(loop :for function :in (keys *conversions*)
        :collect `(make-cl-collider-conversion-pseugen ,function) :into res
        :finally (return `(progn ,@res)))

;;; convenience methods

(defun cl-collider-proxy (id) ; FIX: remove this and just make `find-object-by-id' call `backend-proxys-node' for each enabled/registered backend?
  "Get the object representing the `cl-collider:proxy' with the given name."
  (backend-proxys-node id :supercollider))

(unless (member 'cl-collider-proxy *dictionary-lookup-functions* :test #'eql)
  (appendf *dictionary-lookup-functions* (list 'cl-collider-proxy)))

(defmethod play ((object cl-collider::node))
  t)

(defmethod stop ((object cl-collider::node))
  (cl-collider:free object))

(defmethod end ((object cl-collider::node))
  (cl-collider:release object))

(defmethod playing-p ((node cl-collider::node) &optional (clock *clock*))
  (declare (ignore clock))
  (find (cl-collider::id node) (cl-collider::node-watcher (or (cl-collider::server node)
                                                              cl-collider:*s*))))

(defmethod play ((buffer cl-collider::buffer))
  (let* ((backend (find-backend 'supercollider))
         (synth (backend-buffer-preview-synth backend))
         (synthdef-controls (mapcar #'car (cl-collider:synthdef-metadata synth :controls))))
    (play (event :backend 'supercollider ; backend ; FIX: should support using the backend object itself rather than just the symbol
                 ;; :type :play ; to avoid automatically stopping it ; FIX: implement this note type
                 :instrument synth
                 ;; FIX: should we "need" `make-keyword' here? shouldn't the control names should just be in the correct package already?
                 (make-keyword (find-buffer-symbol synthdef-controls)) buffer
                 :dur 16
                 :quant 0))))

(defmethod render ((buffer cl-collider::buffer) (path string) &rest args &key &allow-other-keys)
  (let (o-args)
    (doplist (k v args)
      (when (position k (list :server :frames :start-frame :format :sample-format :leave-open-p :complete-handler))
        (push v o-args)
        (push (if (eql k :sample-format)
                  :format
                  k)
              o-args)))
    (apply #'cl-collider:buffer-write buffer path o-args)))

(defmethod cl-collider:sr ((supercollider supercollider))
  (cl-collider::server-options-hardware-samplerate (cl-collider::server-options (backend-server supercollider))))

(export '(supercollider))
