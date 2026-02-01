;;;; incudine.lisp - the Incudine backend for cl-patterns.
;;; http://incudine.sourceforge.net

(in-package #:cl-patterns)

(defclass incudine (backend)
  ((name :initform "Incudine")
   (buffer-preview-synth :initarg :buffer-preview-synth :initform :spt :accessor backend-buffer-preview-synth :type symbol :documentation "The name of the synth to use to preview buffers.")
   (start-timestamp :initform nil :accessor backend-start-timestamp :type (or null local-time:timestamp) :documentation "The local-time timestamp when Incudine was started.")
   (start-samples :initform 0 :accessor backend-start-samples :type (integer 0) :documentation "The result of `incudine:now' when `backend-start-timestamp' was generated."))
  (:documentation "cl-patterns Incudine backend."))

;; (defmethod make-backend ((backend (eql 'incudine)) &rest rest &key &allow-other-keys)
;;   (apply #'make-instance 'incudine rest))

;;; helper functions

(defun timestamp-incudine-samples (timestamp &optional (backend (find-backend 'incudine)))
  "Convert a `local-time:timestamp' to an Incudine sample number.

See also: `incudine-samples-timestamp'"
  (* (local-time:timestamp-difference timestamp (backend-start-timestamp backend))
     (incudine:rt-sample-rate)))

(defun incudine-samples-timestamp (samples &optional (backend (find-backend 'incudine)))
  "Convert an Incudine sample number to a `local-time:timestamp'.

See also: `timestamp-incudine-samples'"
  (let* ((start-samples (backend-start-samples backend))
         (diff (- samples start-samples)))
    (when (minusp diff)
      (warn "incudine-samples-timestamp: SAMPLES (~S) occurs before backend-start-samples (~S); result may be inaccurate." samples start-samples))
    (local-time:timestamp+ (backend-start-timestamp backend) (/ diff (incudine:rt-sample-rate)) :sec)))

(defvar *incudine-start-timestamp* nil
  "The local-time timestamp when Incudine was started.")

(defvar *incudine-start-samples* 0
  "The result of `incudine:now' when `*incudine-start-timestamp*' was generated.")

(defun timestamp-to-incudine (timestamp) ; FIX: deprecated; remove
  "Convert a local-time timestamp to an Incudine sample number."
  (* (local-time:timestamp-difference timestamp *incudine-start-timestamp*)
     (incudine:rt-sample-rate)))

(defun incudine-dsp-name (instrument)
  "Get the name of an Incudine DSP."
  (etypecase instrument
    (symbol (find instrument (incudine.vug:all-dsp-names) :test #'string-equal))
    (incudine:node (slot-value instrument 'incudine::name))))

;; (defmethod synth-controls ((backend incudine) synth)
;;   (incudine.vug::dsp-properties-arguments (incudine.vug::get-dsp-properties (incudine-dsp-name synth))))

;; (defun incudine-make-dsp-args-list (event &optional use-kw)
;;   "Given EVENT, return a list that can be applied to the DSP's creation function. With USE-KW, generate a plist.

;; NOTE: This function may produce incorrect results if you fail to provide a required parameter for a DSP."
;;   (loop :for ctl :in (backend-instrument-controls 'incudine (incudine-dsp-name (instrument event)))
;;      :for kwctl = (make-keyword ctl)
;;      :for res = (multiple-value-list (event-value event kwctl))
;;      :for val = (if (eql :gate kwctl)
;;                     1
;;                     (elt res 0))
;;      :for has = (elt res 1)
;;      :if (and (null has)
;;               (not (eql :gate kwctl)))
;;      :do (setf use-kw t)
;;      :append (when val
;;                (if use-kw
;;                    (list kwctl val)
;;                    (list val)))))

(defun incudine-note (dsp-name args-list end-time)
  (apply (fdefinition dsp-name) args-list)
  (when (backend-instrument-has-gate-p 'incudine dsp-name)
    (let ((node (slot-value (incudine:node 0) 'last)))
      (incudine:at end-time #'incudine:set-control node :gate 0))))

;;; backend functions

(defmethod backend-start ((backend incudine) &key)
  (unless *clock*
    (warn "cl-patterns' ~S is nil; starting the clock on your behalf with ~S: ~S."
          '*clock* 'start-clock-loop (start-clock-loop)))
  (incudine:rt-start)
  (setf *incudine-start-timestamp* (local-time:now)
        *incudine-start-samples* (incudine:now)))

(defmethod backend-stop ((backend incudine))
  (incudine:rt-stop)
  backend)

(defmethod backend-tempo-change-at ((backend incudine) clock timestamp)
  nil)

(defmethod backend-convert-object ((backend incudine) (object incudine:buffer) key)
  (declare (ignore key))
  object)

(defmethod backend-instrument-controls ((backend incudine) instrument)
  (incudine.vug::dsp-properties-arguments (incudine.vug::get-dsp-properties (incudine-dsp-name instrument))))

(defmethod backend-all-instruments ((backend incudine))
  (incudine.vug:all-dsp-names))

(defmethod backend-all-nodes ((backend incudine))
  ;; FIX; perhaps we can parse the output of (let ((stream (make-string-output-stream))) (incudine:dump 0 stream) stream) ?
  )

(defmethod backend-node-p ((backend incudine) object)
  (incudine:node-p object))

(defmethod backend-panic ((backend incudine))
  (incudine:node-free-all))

(defmethod backend-timestamps-for-event ((backend incudine) event task)
  (when (and (not *incudine-start-timestamp*)
             (not (eql (incudine:rt-status) :started)))
    (error "cl-patterns cannot generate a timestamp for Incudine as Incudine is not running. Try (backend-start 'incudine) if you want to use Incudine, or disable its backend"))
  (let ((timestamp (or (raw-event-value event :timestamp-at-start)
                       (local-time:now))))
    (mapcar #'timestamp-to-incudine (list timestamp (local-time:timestamp+ timestamp (dur-duration (sustain event)) :sec)))))

(defmethod backend-event-timestamps ((backend incudine) event task)
  (when (and (not (backend-start-timestamp backend))
             (not (eql (incudine:rt-status) :started)))
    (error "cl-patterns cannot generate a timestamp for Incudine as Incudine is not running. Try (backend-start 'incudine) if you want to use Incudine, or disable its backend"))
  (let ((timestamp (or (raw-event-value event :timestamp-at-start)
                       (local-time:now))))
    (mapcar (rcurry #'timestamp-incudine-samples backend)
            (list timestamp (local-time:timestamp+ timestamp (dur-duration (sustain event)) :sec)))))

(defmethod backend-proxys-node ((backend incudine) id)
  ;; FIX?
  )

(defmethod backend-control-node-at ((backend incudine) time (node incudine:node) params)
  (apply #'incudine:at
         time
         #'incudine:set-controls
         node
         params)
  node)

;; FIX: maybe use incudine.vug::with-reserved-node for this?
(defmethod backend-control-node-at ((backend incudine) time (node symbol) params)
  (if-let ((dsp (find node (incudine.vug:all-dsp-names) :test #'string-equal))) ; FIX: can we just assume it will be in the incudine.scratch package?
    (let ((id (or (getf params :id)
                  (incf incudine::*last-node-id*)))) ; can we reserve a node ID with `incudine.vug::with-reserved-node' or the like?
      (apply #'incudine:at time (fdefinition dsp) :id id params)
      (incudine:node id)) ; this will show :ID NIL but it does actually point to the correct node.
    (error "Unable to find an Incudine DSP with name ~S" node)))

(defmethod play ((node incudine:node))
  t)

(defmethod stop ((node incudine:node))
  (incudine:free node))

(defmethod end ((node incudine:node))
  (incudine:set-control node :gate 0))

(defmethod play ((buffer incudine:buffer))
  (let* ((backend (find-backend 'incudine))
         (dsp (backend-buffer-preview-synth backend))
         (dsp-controls (backend-instrument-controls 'incudine dsp)))
    (play (event :backend 'incudine
                 ;; :type :play ; to avoid automatically stopping it ; FIX: implement this note type
                 :instrument dsp
                 (find-buffer-symbol dsp-controls) buffer
                 :dur 16
                 :quant 0
                 :latency 0))))

(export '(incudine))
