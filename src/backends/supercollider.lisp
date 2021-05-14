;;; supercollider.lisp - the SuperCollider/cl-collider backend for cl-patterns.
;; FIX: changing :instrument in pmono causes the old one to stay playing.
;; FIX: multichannel expansion breaks :mono

(in-package #:cl-patterns)

;;; global settings

(defvar *cl-collider-buffer-preview-synth* :spt
  "The name of the synth to use to `play' a buffer.")

;;; backend functions

(defmethod start-backend ((backend (eql :supercollider)) &rest rest &key (name "cl-collider") (host "localhost") (port 4444) just-connect-p &allow-other-keys)
  (setf cl-collider:*s* (cl-collider:make-external-server
                         name
                         :server-options (apply #'cl-collider:make-server-options
                                                (remove-from-plist rest :name :host :port :just-connect-p))
                         :host host
                         :port port
                         :just-connect-p just-connect-p))
  (cl-collider:server-boot cl-collider:*s*))

(defmethod stop-backend ((backend (eql :supercollider)))
  (cl-collider:server-quit cl-collider:*s*))

(defmethod backend-instrument-controls (instrument (backend (eql :supercollider)))
  (mapcar #'car (cl-collider:synthdef-metadata instrument :controls)))

(defmethod backend-all-instruments ((backend (eql :supercollider)))
  (keys cl-collider::*synthdef-metadata*))

(defmethod backend-all-nodes ((backend (eql :supercollider)))
  ;; cl-collider doesn't store the node objects themselves, so this method creates its own node objects.
  ;; unfortunately, that means that the objects returned by this function won't have all information, such as the 'name slot.
  (loop :for id :in (cl-collider::node-watcher cl-collider:*s*)
        :unless (member id (list 0 1))
          :collect (make-instance 'cl-collider::node :id id :server cl-collider:*s* :name nil)))

(defmethod backend-node-p (object (backend (eql :supercollider)))
  (typep object 'cl-collider::node))

(defmethod backend-panic ((backend (eql :supercollider)))
  (cl-collider:group-free-all cl-collider:*s*))

(defmethod backend-timestamps-for-event (event task (backend (eql :supercollider)))
  (let ((time (if-let ((timestamp (raw-event-value event :timestamp-at-start)))
                (+ (local-time:timestamp-to-unix timestamp) (* (local-time:nsec-of timestamp) 1.0d-9))
                (cl-collider:now))))
    (list time (+ time (dur-time (sustain event))))))

(defmethod backend-proxys-node (id (backend (eql :supercollider)))
  (let ((proxy-table (cl-collider::node-proxy-table cl-collider::*s*)))
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
       (backend-proxys-node (cl-collider::name id) :supercollider)))))

(defmethod backend-control-node-at (time (node symbol) params (backend (eql :supercollider)))
  (cl-collider:at time
    (apply #'cl-collider:synth node params)))

(defmethod backend-control-node-at (time (node cl-collider::node) params (backend (eql :supercollider)))
  (cl-collider:at time
    (apply #'cl-collider:ctrl node params)))

(defmethod backend-convert-object ((object cl-collider::buffer) key (backend (eql :supercollider)))
  (declare (ignore key))
  (cl-collider:bufnum object))

(defmethod backend-convert-object ((object cl-collider::bus) key (backend (eql :supercollider)))
  (declare (ignore key))
  (cl-collider:busnum object))

(defmethod backend-convert-object ((object cl-collider::node) key (backend (eql :supercollider)))
  (let ((bus (if (eql key :out)
                 (cl-collider:synthdef-metadata object :input-bus)
                 (or (cl-collider:synthdef-metadata object :output-bus)
                     (cl-collider:synthdef-metadata object :input-bus)))))
    (if bus
        (cl-collider:busnum bus)
        object)))

(defmethod backend-convert-object ((object cl-collider::group) key (backend (eql :supercollider)))
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
         (lambda-list (getf val :lambda-list))
         (body (getf val :body)))
    `(define-cl-collider-pseugen ,function ,lambda-list
       ,@body)))

;; convert all of the functions in `*conversions*'
#.(loop :for function :in (keys *conversions*)
        :collect `(make-cl-collider-conversion-pseugen ,function) :into res
        :finally (return `(progn ,@res)))

;;; convenience methods

(defun cl-collider-proxy (id) ;; FIX: remove this and just make `find-object-by-id' call `backend-proxys-node' for each enabled/registered backend?
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
  (find (cl-collider::id node) (cl-collider::node-watcher cl-collider:*s*)))

(defmethod play ((buffer cl-collider::buffer))
  (let* ((synth *cl-collider-buffer-preview-synth*)
         (synthdef-controls (mapcar #'car (cl-collider:synthdef-metadata synth :controls))))
    (play (event :backend :supercollider
                 ;; :type :play ;; to avoid automatically stopping it ;; FIX: implement this note type
                 :instrument synth
                 (find-if (lambda (x) ;; buffer or bufnum argument
                            (position x (list 'buffer 'bufnum) :test #'string-equal))
                          synthdef-controls)
                 (cl-collider:bufnum buffer) ;; get the actual buffer number
                 :dur 32
                 :quant 0))))

(register-backend :supercollider)

;; (enable-backend :supercollider)
