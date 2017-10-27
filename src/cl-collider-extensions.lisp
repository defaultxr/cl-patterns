(in-package :sc)

(defun synth (name &optional args)
  "Make a synth by name."
  (let* ((name-string (string-downcase (symbol-name name)))
         (next-id (get-next-id *s*))
         (to 1)
         (pos :head)
         (new-synth (make-instance 'node :server *s* :id next-id :name name-string :pos pos :to to))
         (args (mapcar
                (lambda (arg) (if (symbolp arg) (string-downcase (symbol-name arg)) arg)) ;; FIX: should get the actual case of the synth argument instead of just downcasing it
                args)))
    (message-distribute new-synth
                        (apply #'make-synth-msg *s* name-string next-id to pos args)
                        *s*)))

(defun modify-params (params)
  (let ((param-names (loop :for x :in params :collect (car x))))
    (loop :for x :in (list
                      (list (intern "SUSTAIN" *package*) 1)
                      (list (intern "TEMPO" *package*) 1)
                      (list (intern "AMP" *package*) 0.5)
                      (list (intern "PAN" *package*) 0)
                      (list (intern "OUT" *package*) 0))
       :do (when (not (position (car x) param-names))
             (setf params (append params (list x)))))
    params))

(defun modify-body (body)
  (append (list (list 'declare (list 'ignorable
                                     (intern "SUSTAIN" *package*)
                                     (intern "TEMPO" *package*)
                                     (intern "AMP" *package*)
                                     (intern "PAN" *package*))))
          body))

(defparameter *synthdef-metadata* nil
  "The metadata for each synth, such as its parameter list, etc.")

(defun get-synthdef-metadata (name)
  (getf *synthdef-metadata* (alexandria:make-keyword name)))

(defun save-synthdef-metadata (name params)
  (let ((name (alexandria:make-keyword name))
        (params (mapcar (lambda (param) (append (list (alexandria:make-keyword (car param))) (cdr param)))
                        params)))
    (setf (getf *synthdef-metadata* name) params)))

(defun get-synthdef-parameters (name)
  (get-synthdef-metadata name))

(defun get-synthdef-parameter-names (name)
  (mapcar #'car (get-synthdef-parameters name)))

(defgeneric has-gate-p (item))

(defmethod has-gate-p ((item t))
  (position :gate (get-synthdef-parameter-names item)))

(defmethod has-gate-p ((item sc::node))
  (has-gate-p (sc::name item)))

(defmethod has-gate-p ((item string))
  (has-gate-p (alexandria:make-keyword (string-upcase item))))

(defmacro defsynth* (name params &body body)
  "Like `sc:defsynth' but includes some extra features, such as:
- Storing extra metadata for the synthdef.
- Automatic inclusion of extra arguments to the definition: sustain, tempo, amp, pan, out"
  ;; - The ability to specify a ControlSpec for each parameter ;; FIX: not implemented yet
  ;; - Automatic inclusion of some ugens (i.e. out.ar, pan2.ar, etc) unless another is included ;; FIX: not implemented yet
  (let ((params (modify-params params))
        (body (modify-body body)))
    (save-synthdef-metadata name params)
    `(defsynth ,name ,params
       ,@body)))

(export '(synth release get-synthdef-metadata get-synthdef-parameters get-synthdef-parameter-names has-gate-p defsynth*) :sc)
