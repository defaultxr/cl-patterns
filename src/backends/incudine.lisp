(in-package :cl-patterns)

;;; helper functions

(defparameter *incudine-start-timestamp* nil
  "The local-time timestamp when Incudine was started.")

(defparameter *incudine-start-samples* 0
  "The result of `incudine:now' when `*incudine-start-timestamp*' was generated.")

(defun timestamp-to-incudine (timestamp)
  "Convert a local-time timestamp to an Incudine sample number."
  (unless *incudine-start-timestamp*
    (if (eql (incudine:rt-status) :started)
        (setf *incudine-start-timestamp* (local-time:now)
              *incudine-start-samples* (incudine:now))
        (error "cl-patterns cannot generate a timestamp for Incudine as Incudine is not running. Try (start-backend :incudine) if you want to use Incudine, or disable its backend.")))
  (* (local-time:timestamp-difference timestamp *incudine-start-timestamp*)
     (incudine:rt-sample-rate)))

(defun incudine-dsp-name (instrument)
  (etypecase instrument
    (symbol (find instrument (incudine.vug:all-dsp-names) :test #'string-equal))
    (incudine:node (slot-value instrument 'incudine::name))))

;; (defmethod synth-controls (synth (backend (eql :incudine)))
;;   (incudine.vug::dsp-properties-arguments (incudine.vug::get-dsp-properties (incudine-dsp-name synth))))

;; (defun incudine-make-dsp-args-list (event &optional use-kw)
;;   "Given EVENT, return a list that can be applied to the DSP's creation function. With USE-KW, generate a plist.

;; NOTE: This function may produce incorrect results if you fail to provide a required parameter for a DSP."
;;   (loop :for ctl :in (backend-instrument-controls (incudine-dsp-name (instrument event)) :incudine)
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
  (when (backend-instrument-has-gate-p dsp-name :incudine)
    (let ((node (slot-value (incudine:node 0) 'last)))
      (incudine:at end-time #'incudine:set-control node :gate 0))))

;;; backend functions

(defmethod start-backend ((backend (eql :incudine)))
  (unless *clock*
    (warn "cl-patterns' *CLOCK* is nil; starting the clock on your behalf with START-CLOCK-LOOP: ~s."
          (start-clock-loop)))
  (incudine:rt-start)
  (setf *incudine-start-timestamp* (local-time:now)))

(defmethod stop-backend ((backend (eql :incudine)))
  (incudine:rt-stop))


(defmethod backend-instrument-controls (instrument (backend (eql :incudine)))
  (incudine.vug::dsp-properties-arguments (incudine.vug::get-dsp-properties (incudine-dsp-name instrument))))

(defmethod backend-node-p (object (backend (eql :incudine)))
  (incudine:node-p object))

(defmethod backend-timestamps-for-event (event task (backend (eql :incudine)))
  (let ((timestamp (or (raw-event-value event :timestamp-at-start)
                       (local-time:now))))
    (mapcar #'timestamp-to-incudine (list timestamp (local-time:timestamp+ timestamp (dur-time (sustain event)) :sec)))))

(defmethod backend-proxys-node (id (backend (eql :incudine)))
  ;; FIX?
  )

(defmethod backend-control-node-at (time (node incudine:node) params (backend (eql :incudine)))
  (apply #'incudine:at
         time
         #'incudine:set-controls
         node
         params)
  node)

(defmethod backend-control-node-at (time (node symbol) params (backend (eql :incudine)))
  (if-let ((dsp (find node (incudine.vug:all-dsp-names) :test #'string-equal))) ;; FIX: can we just assume it will be in the incudine.scratch package?
    (let ((id (or (getf params :id)
                  (incudine:next-node-id)))) ;; can we reserve a node ID with `incudine.vug::with-reserved-node' or the like?
      (apply #'incudine:at
             time
             (fdefinition dsp)
             (append (list :id id) params))
      (incudine:node id)) ;; this will show :ID NIL but it does actually point to the correct node.
    (error "Unable to find an Incudine DSP with name ~s." node)))

(defmethod play ((node incudine:node))
  t)

(defmethod stop ((node incudine:node))
  (incudine:free node))

(defmethod end ((node incudine:node))
  (incudine:set-control node :gate 0))

(register-backend :incudine)

;; (enable-backend :incudine)
