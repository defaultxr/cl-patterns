(in-package :cl-patterns)

;;; helper functions

(defparameter *incudine-tempo-start-beat* nil
  "The `*clock*' beat when the `*incudine-tempo*' tempo started.")

(defparameter *incudine-tempo* nil
  "An Incudine tempo object started by cl-pattern's Incudine backend.")

(defun beat-to-incudine (beat)
  "Convert a cl-patterns clock beat number into the timestamp format used by Incudine."
  (if *incudine-tempo-start-beat*
      (eval (incudine::parse-time-unit "b" (- beat *incudine-tempo-start-beat*) *incudine-tempo* nil))
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

(defmethod backend-play-event.old (event task (backend (eql :incudine)))
  (let ((type (event-value event :type)))
    (unless (eql type :rest)
      (case type
        (:tempo-change
         (warn "Tempo change events are not yet supported for the Incudine backend..."))
        ((:note :mono)
         (let* ((instrument (instrument event))
                (dsp-name (incudine-dsp-name instrument)))
           (unless dsp-name
             (warn "No Incudine DSP with the name ~s defined - skipping event ~s." instrument event)
             (return-from backend-play-event.old nil))
           (let* ((quant (quant event))
                  (offset (if (> (length quant) 2)
                              (nth 2 quant)
                              0))
                  (beat (event-value event :beat-at-start))
                  (rt-sr (incudine:rt-sample-rate))
                  (time (+ (* (or (raw-event-value event :latency) *latency*)
                              rt-sr)
                           (if beat
                               (beat-to-incudine beat)
                               (incudine:now))
                           offset))
                  (end-time (+ time (* rt-sr (dur-time (event-value event :sustain) (tempo *clock*))))))
             (cond
               ((or (eql :mono type)
                    (incudine:node-p instrument))
                (let* ((nodes (task-nodes task *incudine-node-map*))
                       (should-ctrl (or nodes
                                        (incudine:node-p instrument)))
                       (node (if (incudine:node-p instrument)
                                 instrument
                                 (car nodes))))
                  (apply 'incudine:at time
                         (if should-ctrl
                             #'incudine:set-controls
                             (fdefinition dsp-name))
                         (if should-ctrl
                             (append (list node)
                                     (incudine-make-dsp-args-list event t))
                             (incudine-make-dsp-args-list event)))
                  (unless (or (incudine:node-p instrument)
                              (not (backend-instrument-has-gate-p instrument :incudine)))
                    (if (< (legato event) 1)
                        (progn
                          (setf (task-nodes task *incudine-node-map*) nil)
                          (incudine:at end-time #'incudine:set-control node :gate 0))
                        (setf (task-nodes task *incudine-node-map*) (list node))))))
               ((eql type :note)
                (incudine:at time #'incudine-note dsp-name (incudine-make-dsp-args-list event) end-time))))))))))

(defmethod backend-task-removed.old (task (backend (eql :incudine)))
  (let ((item (slot-value task 'item)))
    (unless (typep item 'event)
      (let ((last-output (last-output item))
            (rt-sr (incudine:rt-sample-rate)))
        (dolist (node (task-nodes task *incudine-node-map*))
          (incudine:at (+ (* (or (raw-event-value last-output :latency) *latency*) ;; FIX: include quant's offset here
                             rt-sr)
                          (beat-to-incudine (+ (slot-value task 'start-beat) (or (beat last-output) 0) (sustain last-output))))
                       #'incudine:set-control node :gate 0)))))
  (setf (task-nodes task *incudine-node-map*) nil))

(defmethod backend-instrument-controls (instrument (backend (eql :incudine)))
  (incudine.vug::dsp-properties-arguments (incudine.vug::get-dsp-properties (incudine-dsp-name instrument))))

(defmethod backend-node-p (object (backend (eql :incudine)))
  (incudine:node-p object))

(defmethod backend-timestamps-for-event (event task (backend (eql :incudine)))
  (let ((beat (or (raw-event-value event :beat-at-start) 0)))
    (mapcar #'beat-to-incudine (list beat (+ beat (event-value event :sustain))))))

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
