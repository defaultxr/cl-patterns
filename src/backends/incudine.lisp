(in-package :cl-patterns)

;;; helper functions

;; incudine's (now) function returns the number of samples elapsed.
;; we will need to keep track of the value of (now) when the clock is started or when the backend is launched.
(defun timestamp-to-incudine (timestamp)
  "Convert a local-time timestamp into the timestamp format used by Incudine."
  ;; FIX
  nil
  )

(defun get-dsp-args-list (dsp)
  "Return the argument list for an Incudine DSP."
  (mapcar #'alexandria:make-keyword
          (remove-if (lambda (symbol)
                       (or
                        (position symbol (list '&key 'incudine.vug::id 'incudine.vug::head 'incudine.vug::tail 'incudine.vug::before 'incudine.vug::after 'incudine.vug::action 'incudine.vug::stop-hook 'incudine.vug::free-hook 'incudine.vug::fade-time 'incudine.vug::fade-curve 'incudine.scratch::replace))))
                     (swank-backend:arglist (fdefinition (alexandria:ensure-symbol dsp 'incudine.scratch))) ;; FIX: this is the only thing we require swank for, so remove the swank dependency from the .asd file once this is removed.
                     )))

;;; backend functions

(defmethod start-backend ((backend (eql :incudine)))
  ;; FIX
  )

(defmethod stop-backend ((backend (eql :incudine)))
  ;; FIX
  )

(defmethod backend-plays-event-p (event (backend (eql :incudine)))
  t ;; FIX
  )

(defmethod backend-play-event (event task (backend (eql :incudine)))
  (unless (eq (event-value item :type) :rest)
    (let* ((instrument (instrument item))
           (incudine-dsp-name (intern (symbol-name instrument) 'incudine.scratch))
           (synth-params (get-dsp-args-list instrument))
           (quant (alexandria:ensure-list (quant item)))
           (offset (if (> (length quant) 2)
                       (nth 2 quant)
                       0))
           (time (+ (or (raw-event-value )))) ;; FIX
           (params (mapcar (lambda (param)
                             (if (eq :gate param)
                                 1
                                 (event-value item param)))
                           synth-params)))
      (if (position incudine-dsp-name (incudine::all-dsp-names))
          (if (and (eq (event-value item :type) :mono)
                   (not (null task))) ;; FIX: remove this line?
              (let* ((node-id (incudine:next-node-id)) ;; FIX: THIS IS NOT THREAD-SAFE! since no locking is done and the node ID and DSP instantiation are not done atomically, it's possible that two threads may get the same ID and launch two different DSPs with it. Not sure how to resolve this.
                     (params (append params (list :id node-id))))
                (incudine:at time (fdefinition incudine-dsp-name) params))
              ())
          (warn "No DSP with the name ~a defined - skipping." instrument)))))

(defmethod backend-task-removed (task (backend (eql :incudine)))
  ;; FIX
  )

(defmethod play ((node incudine:node)) ;; FIX
  t)

(defmethod end ((node incudine:node)) ;; FIX
  (incudine:set-control node :gate 0))

(defmethod stop ((node incudine:node)) ;; FIX
  (incudine:set-control node :gate 0))

(register-backend :incudine
                  :respond-p 'is-incudine-event-p
                  :play 'play-incudine
                  :release 'release-incudine
                  :release-at 'release-incudine-at)

(enable-backend :incudine)
