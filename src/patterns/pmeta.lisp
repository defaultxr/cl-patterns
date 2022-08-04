;;;; pmeta.lisp - pmeta; meta-pattern for embedding and controlling other patterns.
;;; FIX:
;;; - `pk' doesn't work in pmeta.

(in-package #:cl-patterns)

(defpattern pmeta (pattern)
  (pattern
   (current-pstream :state t :initform nil))
  :documentation "Meta-control patterns using the events output by PATTERN. In other words, instead of triggering synths directly, the events output by PATTERN are used to embed patterns into the pmeta's pstream.

The following keys are supported:

- :pattern or :instrument - name of the source pattern for this \"step\".
- :dur - set the duration of the embedded pattern (defaults to :inf, which causes the pattern to play to its end).
- :findur - limit the duration of the embedded pattern.
- :sync - sync the duration of the embedded pattern to a multiple of the provided value, similar to `psync'
- :stretch - multiply the duration of each of the source pattern's events.
- :ts or :fit - timestretch a pattern so its total duration is the number specified, a la `pts'.
- :r or :repeat - repeat each event the number of times returned by the function when the event is applied to it, similar to `pr'.
- :inject - inject key/value pairs from the output of this value into the source pattern.
- :step-inject or :sinject - inject one output from this value per step.

The following keys are planned for future implementation:

- :start or :end - adjust the start or end points of the source pattern (i.e. to skip the first half, set :start to 0.5).
- :start-beat or :end-beat - adjust the start or end points of the source pattern in number of beats (i.e. to end the pattern 2 beats early, set :end-beat to -2).
- :start-nth or :end-nth - adjust the start or end points of the source pattern by skipping the first or last N events.
- :filter or :remove-if-not - skip all events from the source pattern that return nil when applied to the specified function or pattern.
- :mapcar or :nary - process each event from the source pattern with a function or another pattern.

See doc/special-keys.org for more information on these keys.

Example:

;; ;; define a few patterns...
;; (pdef :foo (pbind :x (pseq '(1 2 3) 1) :dur 1))
;; (pdef :bar (pbind :y (pseries) :dur (pwhite 0.1 1.0 3)))
;;
;; ;; use pmeta to play pattern foo and then pattern bar, ensuring each are 2 beats long in total by way of pmeta's sync key, which works similarly to `psync'
;; (next-upto-n (pmeta :pattern (pseq (list :foo :bar) 1) :sync 2))
;; ;=> ((EVENT :X 1 :DUR 1) (EVENT :X 2 :DUR 1) ; from (pdef :foo)
;;      (EVENT :Y 0 :DUR 0.76958686) (EVENT :Y 1 :DUR 0.94850993) (EVENT :Y 2 :DUR 0.28190327)) ; from (pdef :bar)

See also: `psym', `parp', `pdef', `pbind'"
  :defun (defun pmeta (&rest pairs)
           (make-instance 'pmeta :pattern (if (length= 1 pairs)
                                              (car pairs)
                                              pairs))))

(defmethod as-pstream ((pmeta pmeta))
  (with-slots (pattern) pmeta
    (make-instance 'pmeta-pstream
                   :pattern (if (listp pattern)
                                (loop :for (key value) :on pattern :by #'cddr
                                      :if (member key (list :inject))
                                        :append (list key value)
                                      :else
                                        :append (list key (pattern-as-pstream value)))
                                (as-pstream pattern)))))

(defgeneric process-pattern-key-value (pattern key value)
  (:documentation "Process a key/value pair for a pattern like `pbind' or `pmeta'."))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :pattern)) (value pattern))
  value)

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :pattern)) (value symbol))
  (pdef value))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :pattern)) (value list))
  (ppar value))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :findur)) value)
  (with-slots (current-pstream) pmeta
    (if (eql value :inf)
        current-pstream
        (pfindur current-pstream value))))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :sync)) value)
  (with-slots (current-pstream) pmeta
    (if (eql value :inf)
        current-pstream
        (apply #'psync current-pstream (ensure-list value)))))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :dur)) value)
  (process-pattern-key-value pmeta :sync (list value value)))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :stretch)) value)
  (with-slots (current-pstream) pmeta
    (pchain current-pstream (pbind :dur (p* value (pk :dur))))))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :ts)) value)
  (with-slots (current-pstream) pmeta
    (pts current-pstream value)))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :fit)) value)
  (process-pattern-key-value pmeta :ts value))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :r)) value)
  (with-slots (current-pstream) pmeta
    (pr current-pstream value)))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :repeat)) value)
  (process-pattern-key-value pmeta :r value))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :inject)) value)
  (with-slots (current-pstream) pmeta
    (if current-pstream
        (pchain current-pstream value)
        value)))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :sinject)) value)
  (with-slots (current-pstream) pmeta
    (if current-pstream
        (pchain current-pstream (pn (next value)))
        (pn (next value)))))

(defmethod process-pattern-key-value ((pmeta pmeta) (key (eql :step-inject)) value)
  (process-pattern-key-value pmeta :sinject value))

(defmethod next ((pmeta pmeta-pstream))
  (with-slots (pattern current-pstream) pmeta
    (labels ((make-pstream (plist)
               (unless plist
                 (return-from make-pstream nil))
               (destructuring-bind (key value &rest rest) plist
                 (when (eop-p value)
                   (return-from make-pstream eop))
                 (case key
                   ((:embed)
                    (let* ((nv (next value))
                           (nvp (typecase nv
                                  (list nv)
                                  (event (event-plist nv)))))
                      (dolist (i (reverse nvp))
                        (push i rest))))                   
                   (t
                    (setf current-pstream (process-pattern-key-value pmeta key value))))
                 (if rest
                     (make-pstream rest)
                     (let ((pstream (as-pstream current-pstream)))
                       (setf (slot-value pstream 'parent) pmeta)
                       pstream)))))
      (unless current-pstream
        (setf current-pstream (make-pstream (if (listp pattern)
                                                (loop :for (key value) :on pattern :by #'cddr
                                                      :append (list key (next value)))
                                                (let ((nxt (next pattern)))
                                                  (typecase nxt
                                                    (event (event-plist nxt))
                                                    (symbol (if (eop-p nxt)
                                                                (list :none eop)
                                                                (list :pattern nxt)))))))))
      (when (eop-p current-pstream)
        (return-from next eop))
      (let ((nxt (next current-pstream)))
        (if (eop-p nxt)
            (progn
              (setf current-pstream nil)
              (next pmeta))
            nxt)))))
