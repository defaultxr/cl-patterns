(in-package :cl-patterns)

;;; helper functions

(defparameter *incudine-tempo-start-beat* nil
  "The `*clock*' beat when the `*incudine-tempo*' tempo started.")

(defparameter *incudine-tempo* nil
  "An Incudine tempo object started by cl-pattern's Incudine backend.")

;; (defun timestamp-to-incudine (timestamp)
;;   "Convert a local-time timestamp into the timestamp format used by Incudine."
;;   (* (incudine:rt-sample-rate)
;;      (local-time:timestamp-difference timestamp *incudine-rt-start-time*)))

(defun beat-to-incudine (beat)
  "Convert a cl-patterns clock beat number into the timestamp format used by Incudine."
  (if *incudine-tempo-start-beat*
      (eval (incudine::parse-time-unit "b" (- beat *incudine-tempo-start-beat*) *incudine-tempo* nil))
      (error "cl-patterns cannot sync to Incudine, as *incudine-tempo-start-beat* is still NIL. You may need to call (start-backend :incudine) first.")))

(defun incudine-dsp-name (instrument) ;; FIX: make sure this works if INSTRUMENT is a node itself
  (ensure-symbol instrument :incudine.scratch))

(defmethod synth-controls (synth (backend (eql :incudine)))
  (incudine.vug::dsp-properties-arguments (incudine.vug::get-dsp-properties (incudine-dsp-name synth))))

(defun incudine-make-dsp-args-list (event &optional use-kw)
  "Given EVENT, return a list that can be applied to the DSP's creation function. With USE-KW, generate a plist.

NOTE: This function may produce incorrect results if you fail to provide a required parameter for a DSP."
  (loop :for ctl :in (synth-controls (incudine-dsp-name (instrument event)) :incudine)
     :for kwctl = (make-keyword ctl)
     :for res = (multiple-value-list (event-value event kwctl))
     :for val = (if (eql :gate kwctl)
                    1
                    (elt res 0))
     :for has = (elt res 1)
     :if (and (null has)
              (not (eql :gate kwctl)))
     :do (setf use-kw t)
     :append (when val
               (if use-kw
                   (list kwctl val)
                   (list val)))))

(defvar *incudine-node-map* (make-node-map)
  "Hash mapping cl-patterns tasks to SuperCollider nodes.")

;;; backend functions

(defmethod start-backend ((backend (eql :incudine))) ;; FIX: allow a quant to be used to sync the clock to the newly-created incudine tempo?
  (incudine:rt-start)
  (setf *incudine-tempo-start-beat* (beat *clock*)
        *incudine-tempo* (incudine:make-tempo (tempo *clock*) :bps))) ;; FIX: also need to handle tempo-change events since we're using an incudine tempo object

(defmethod stop-backend ((backend (eql :incudine)))
  (incudine:rt-stop))

(defmethod backend-plays-event-p (event (backend (eql :incudine)))
  (or (eql :tempo-change (event-value event :type))
      (and (position (incudine-dsp-name (event-value event :instrument))
                     (incudine::all-dsp-names))
           t)))

(defmethod backend-play-event (event task (backend (eql :incudine)))
  (unless (eql (event-value event :type) :rest)
    (let* ((instrument (instrument event))
           (dsp-name (incudine-dsp-name instrument)))
      (unless (position dsp-name (incudine::all-dsp-names))
        (warn "No Incudine DSP with the name ~s defined - skipping event ~s." instrument event)
        (return-from backend-play-event nil))
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
             (node-id (incudine:next-node-id)) ;; FIX: not thread-safe? also seems to cause multichannel expansion to fail...
             (ev-type (event-value event :type))
             (end-time (+ time (* rt-sr (dur-time (event-value event :sustain) (tempo *clock*))))))
        (cond
          ((or (eql :mono ev-type)
               (incudine:node-p instrument))
           (let* ((nodes (task-nodes task *incudine-node-map*))
                  (should-ctrl (or nodes
                                   (incudine:node-p instrument)))
                  (ctrl (if (incudine:node-p instrument)
                            instrument
                            (car nodes))))
             (apply 'incudine:at time
                    (if should-ctrl
                        #'incudine:set-controls
                        (fdefinition dsp-name))
                    (if should-ctrl
                        (append (list ctrl)
                                (incudine-make-dsp-args-list event t))
                        (append (incudine-make-dsp-args-list event) (list :id node-id))))
             (unless (or (incudine:node-p instrument)
                         (not (has-gate-p instrument :incudine)))
               (let ((node (if should-ctrl ctrl (incudine:node node-id))))
                 (if (< (legato event) 1)
                     (progn
                       (setf (task-nodes task *incudine-node-map*) nil)
                       (incudine:at end-time #'incudine:set-control node :gate 0))
                     (setf (task-nodes task *incudine-node-map*) (list node)))))))
          ((position ev-type (list :note nil))
           (apply 'incudine:at time (fdefinition dsp-name) (append (incudine-make-dsp-args-list event) (list :id node-id)))
           (when (has-gate-p dsp-name :incudine)
             (incudine:at end-time #'incudine:set-control (incudine:node node-id) :gate 0))))))))

(defmethod backend-task-removed (task (backend (eql :incudine)))
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

(defmethod play ((node incudine:node))
  t)

(defmethod end ((node incudine:node))
  (incudine:set-control node :gate 0))

(defmethod stop ((node incudine:node))
  (incudine:free node))

(register-backend :incudine)

;; (enable-backend :incudine)
