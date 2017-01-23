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

(defun release (node)
  (ctrl node :gate 0))

(export '(synth release) :sc)
