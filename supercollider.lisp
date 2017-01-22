(in-package :sc)

;; (defun make-synth-msg (rt-server name id to pos &rest args)
;;   (with-node (to target server)
;;     (assert (eql rt-server server) nil "target server != synth's server.(/= ~a ~a)" server rt-server)
;;     (apply #'list 9 name id (node-to-pos pos) target (mapcar #'floatfy args))))

;; (defun make-synth-msg-2 (rt-server name id to pos args)
;;   (with-node (to target server)
;;     (assert (eql rt-server server) nil "target server != synth's server.(/= ~a ~a)" server rt-server)
;;     (apply #'list 9 name id (node-to-pos pos) target (mapcar #'floatfy args))))

;; (defmacro defsynth (name params &body body)
;;   (alexandria:with-gensyms (synthdef)
;;     `(let* ((,synthdef (make-instance 'synthdef :name ,(string-downcase name)))
;;             (*synthdef* ,synthdef))
;;        (with-controls (,@(mapcar #!(if (symbolp %) (list % 0) %) (remove '&key params)))
;;          ,@(convert-code body)
;;          (build-synthdef ,synthdef)
;;          (when (and *s* (boot-p *s*))
;;            (ecase *synth-definition-mode*
;;              (:recv (recv-synthdef ,synthdef nil))
;;              (:load (load-synthdef ,synthdef nil)))
;;            (sync)
;;            (synth-funcall-definition ,name ,params))
;;          ,synthdef))))

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
    ;; (let* ((,next-id (get-next-id *s*))
    ;;        (,new-synth (make-instance 'node :server *s* :id ,next-id :name ,(string-downcase name) :pos pos :to to)))
    ;;   (prog1
    ;;       (message-distribute 
    ;;        ,new-synth
    ;;        (make-synth-msg *s* ,(string-downcase name) ,next-id to pos
    ;;                        ,@(alexandria:flatten (mapcar #!(list (string-downcase %) %)
    ;;                                                      (mapcar #!(if (listp %) (car %) %)
    ;;                                                              (remove '&key args)))))
    ;;        *s*)
    ;;     (when end-f
    ;;       (setf (gethash ,next-id (end-node-handler *s*)) end-f))))
    (message-distribute new-synth
                        (make-synth-msg *s* name-string next-id to pos args)
                        *s*)
    ;; (message-distribute new-synth
    ;;                     (apply #'make-synth-msg *s* name-string next-id to pos args)
    ;;                     *s*)
    ))

(defun release (node)
  (ctrl node :gate 0))

(export '(synth release) :sc)

(in-package :cl-patterns)

(defgeneric play-sc (item))

(defmethod play-sc ((item t))
  (unless (eq (get-event-value item :type) :rest)
    (let ((params (copy-list (event-plist item))))
      (remf params :instrument)
      (sc:at (+ 0.2 (sc:now)) ;; FIX
        (sc:synth (instrument item) params)))))

(setf *event-output-function* 'play-sc)
