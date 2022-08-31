;;;; pdef.lisp - pdef and associated functionality to define and reference "named patterns".
;;; FIX:
;;; - implement `reset' method and test to ensure end-condition works properly

(in-package #:cl-patterns)

(defgeneric pdef-name (pdef)
  (:documentation "The name (\"key\") of PDEF."))

(uiop:with-deprecation (:error)
  (defun pdef-key (pdef)
    "Deprecated alias for `pdef-name'."
    (pdef-name pdef))

  (defun (setf pdef-key) (value pdef)
    "Deprecated alias for `(setf pdef-name)'."
    (setf (pdef-name pdef) value)))

(defgeneric pdef-pattern (pdef)
  (:documentation "The pattern that PDEF points to."))

(defgeneric pdef-pstream (pdef)
  (:documentation "The currently-playing pstream of PDEF's pattern."))

(defgeneric pdef-task (pdef)
  (:documentation "The task that PDEF was last being played in."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpattern pdef (pattern)
    ((name :reader pdef-name :type string-designator)
     (pattern :accessor pdef-pattern)
     (pstream :initform nil :accessor pdef-pstream)
     (task :initform nil :accessor pdef-task)
     (current-pstream :state t))
    :documentation "Define a named pattern, with NAME being the name of the pattern and PATTERN the pattern itself. Named patterns are stored in a global dictionary and can be referred back to by calling `pdef' without supplying PATTERN. The global dictionary also keeps track of the pdef's pstream when `play' is called on it. If a pdef is redefined while it is being played, the changes won't be audible until either PATTERN ends, or the pdef's `end-quant' time is reached (if non-nil). Note that, unlike bare patterns, pdefs loop by default when played (`loop-p').

Example:

;; (pdef :foo (pbind :degree (pseries 0 1 4)))
;;
;; (play (pdef :foo))
;; ;; redefine the pdef's pattern... note that the redefinition doesn't become audible until the current loop finishes playing:
;; (pdef :foo (pbind :degree (pseries 4 -1 4)))

See also: `find-pdef', `all-pdefs', `pb', `pmeta', `ps'"
    :defun (defun pdef (name &optional (pattern nil pattern-supplied-p))
             (check-type name string-designator)
             (let ((pdef (ensure-gethash name *pdef-dictionary* (make-instance 'pdef :name name))))
               (when pattern-supplied-p
                 (setf (pdef-pattern pdef) pattern)
                 (unless (pattern-parent pattern)
                   (setf (slot-value pattern 'parent) pdef)))
               pdef))))

(define-dictionary pdef :define-class-functions t)

(defmethod print-object ((pdef pdef) stream)
  (with-slots (name) pdef
    (format stream "(~S ~S)" 'pdef name)))

(defmethod print-object ((pdef pdef-pstream) stream)
  (with-slots (name) pdef
    (print-unreadable-object (pdef stream :type t)
      (format stream "~S" name))))

(defun playing-pdefs (&optional (clock *clock*))
  "Get a list of the names of all pdefs playing on CLOCK.

See also: `all-pdefs', `playing-nodes', `playing-p'"
  (loop :for task :in (clock-tasks clock)
        :for item := (slot-value task 'item)
        :if (ignore-errors (pdef-name item))
          :collect (pdef-name item)))

(defmethod pdef-name ((pbind pbind))
  (getf (slot-value pbind 'pairs) :pdef))

(defmethod (setf pdef-name) (value (pdef pdef))
  (let ((prev-name (pdef-name pdef)))
    (remhash prev-name *pdef-dictionary*)
    (setf (slot-value pdef 'name) value
          (gethash value *pdef-dictionary*) pdef)))

(defmethod (setf pdef-name) (value (symbol symbol))
  (setf (pdef-name (pdef symbol)) value))

(defmethod (setf pdef-pstream) (value (symbol symbol))
  (setf (slot-value (find-pdef symbol) 'pstream) value))

(defmethod play-quant ((pdef pdef))
  (if (slot-boundp pdef 'play-quant)
      (slot-value pdef 'play-quant)
      (play-quant (pdef-pattern pdef))))

(defmethod end-quant ((pdef pdef))
  (if (slot-boundp pdef 'end-quant)
      (slot-value pdef 'end-quant)
      (end-quant (pdef-pattern pdef))))

(defmethod play ((pdef pdef))
  ;; if there is already a task playing this pdef, we do nothing.
  ;; you can use `launch' instead to force launching a second instance of the pattern.
  (unless (position (pdef-task pdef) (clock-tasks *clock*))
    (let ((task (call-next-method)))
      (when (typep task 'task)
        (setf (pdef-task pdef) task)
        task))))

(defmethod launch ((pdef pdef))
  (play (pdef-pattern pdef)))

(defmethod stop ((pdef pdef))
  (when-let ((task (pdef-task pdef)))
    (stop task))
  (setf (pdef-pstream pdef) (as-pstream (pdef-pattern pdef))
        (pdef-task pdef) nil))

(defmethod end ((pdef pdef))
  (if (pdef-task pdef)
      (end (pdef-task pdef))
      (warn "~S has no associated task; try ~S instead." pdef 'stop)))

(defmethod playing-p ((pdef pdef) &optional (clock *clock*))
  (when clock
    (find (pdef-name pdef) (clock-tasks clock)
          :key (fn (ignore-errors (pdef-name (slot-value _ 'item)))))))

(defmethod loop-p ((pdef pdef))
  (if (slot-boundp pdef 'loop-p)
      (slot-value pdef 'loop-p)
      (let ((pattern (pdef-pattern pdef)))
        (if (slot-boundp pattern 'loop-p)
            (slot-value pattern 'loop-p)
            t))))

(defmethod keys ((pdef pdef))
  (keys (pdef-pattern pdef)))

(defmethod as-pstream ((pdef pdef))
  (with-slots (name pattern) pdef
    (make-instance 'pdef-pstream
                   :name name
                   :pattern pattern
                   :current-pstream (as-pstream (pdef-pattern pdef)))))

(defmethod next ((pdef pdef-pstream))
  (next (slot-value pdef 'current-pstream)))

