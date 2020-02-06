(in-package :cl-patterns)

;;; helper functions

(defparameter *incudine-tempo-start-beat* nil
  "The `*clock*' beat when the `*incudine-tempo*' tempo started.")

(defparameter *incudine-samples-at-tempo-change* 0
  "The number of samples in Incudine when the clock's tempo was changed.")

(defun beat-to-incudine (beat &optional (clock *clock*))
  "Convert a cl-patterns clock beat number into an Incudine sample number."
  (if *incudine-tempo-start-beat*
      (+ (* (- beat *incudine-tempo-start-beat*) (tempo clock) (incudine:rt-sample-rate))
         *incudine-samples-at-tempo-change*)
      (error "cl-patterns cannot sync to Incudine, as *incudine-tempo-start-beat* is still NIL. You may need to call (start-backend :incudine) first.")))

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

(defmethod start-backend ((backend (eql :incudine))) ;; FIX: allow a quant to be used to sync the clock to the newly-created incudine tempo?
  (unless *clock*
    (warn "cl-patterns' *CLOCK* is nil; starting the clock on your behalf with START-CLOCK-LOOP: ~s."
          (start-clock-loop)))
  (incudine:rt-start)
  (setf *incudine-tempo-start-beat* (beat *clock*)
        *incudine-tempo* (incudine:make-tempo (tempo *clock*) :bps))) ;; FIX: also need to handle tempo-change events since we're using an incudine tempo object

(defmethod stop-backend ((backend (eql :incudine)))
  (incudine:rt-stop))


(defmethod backend-instrument-controls (instrument (backend (eql :incudine)))
  (incudine.vug::dsp-properties-arguments (incudine.vug::get-dsp-properties (incudine-dsp-name instrument))))

(defmethod backend-node-p (object (backend (eql :incudine)))
  (incudine:node-p object))

(defmethod backend-timestamps-for-event (event task (backend (eql :incudine)))
  (let ((beat (or (raw-event-value event :beat-at-start) 0)))
    (mapcar (rcurry #'beat-to-incudine (slot-value task 'clock)) (list beat (+ beat (event-value event :sustain))))))

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
