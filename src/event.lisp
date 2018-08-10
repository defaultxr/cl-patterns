(in-package :cl-patterns)

;; FIX: make versions of these generic functions that will work with supercollider ugens and put them in supercollider.lisp.
;; FIX: need to test weird scales/tunings to make sure they're converting correctly, etc.

;;; event glue

(defclass event ()
  ((event-plist :initarg :event-plist :initform (list) :reader event-plist :type 'list :documentation "The plist containing all of the event's keys and values."))
  (:documentation "Class representing a musical event."))

(defun event (&rest params)
  "Create an event, using the PARAMS as its keys/values."
  (assert (= 0 (mod (length params) 2)))
  (let ((ev (make-instance 'event)))
    (labels ((accumulator (pairs)
               (when (not (null (car pairs)))
                 (setf (event-value ev (alexandria:ensure-symbol (car pairs) 'cl-patterns)) (cadr pairs))
                 (accumulator (cddr pairs)))))
      (accumulator params)
      ev)))

(defvar *latency* 0.1
  "Default latency for events.")

(defparameter *event-special-slots* (list)
  "Plist mapping event special slots to their case lists.")

(defun event-value (event slot)
  "Get the value of SLOT in EVENT, running any necessary conversion functions.

Returns 2 values: the value of the slot, and the name of the slot the value was derived from (or t if the default value of the slot was used, or nil if no value or default was provided)."
  (when (null event)
    (return-from event-value (values nil nil)))
  (let* ((slot (alexandria:make-keyword slot))
         (cases (car (getf *event-special-slots* slot)))
         (cases (if (not (position slot (keys cases)))
                    (append (list slot (lambda (event) (raw-event-value event slot))) cases)
                    cases))
         (cases-keys (keys cases))
         (key (car (or (member-if (lambda (k) (position k (keys event))) cases-keys)
                       (member t cases-keys))))
         (func (getf cases key)))
    (values (if (null func)
                nil
                (funcall func event))
            key)))

(defun event-equal (event1 event2)
  "Test if EVENT1 and EVENT2 are equivalent."
  (and (equal (keys event1)
              (keys event2))
       (loop :for key :in (keys event1)
          :if (not (equal (event-value event1 key) (event-value event2 key)))
          :return nil
          :finally (return t))))

(defun every-event-equal (&rest lists)
  "Test if all the events in LISTS are equivalent. Similar to (every #'event-equal LIST-1 LIST-2...) except that it will fail if the lists are not the same length."
  (and (apply #'= (mapcar #'length lists))
       (apply #'every #'event-equal lists)))

(defun (setf event-value) (value event slot)
  "Set the value of SLOT to VALUE in EVENT, running any conversion functions that exist."
  (let* ((slot (alexandria:make-keyword slot))
         (cases (getf *event-special-slots* slot)))
    (when (cadr cases) ;; remove-keys
      (let ((keys (remove-if (lambda (c) (eq c t)) (keys (car cases)))))
        (loop :for i :in keys
           :do (remove-event-value event i))))
    (raw-set-event-value event slot value)
    value))

(defun raw-set-event-value (event slot value)
  "Set the value of SLOT to VALUE in EVENT without running any conversion functions."
  (with-slots (event-plist) event
    (setf event-plist (plist-set event-plist (alexandria:make-keyword slot) value))))

(defun set-event-value (event slot value) ;; kind of deprecated. prefer (setf (event-value EVENT SLOT) VALUE) instead.
  "Set the value of SLOT to VALUE in EVENT, running any conversion functions that exist.

This function is semi-deprecated; use `(setf event-value)' instead, i.e.:

;; (setf (event-value EVENT SLOT) VALUE)"
  (setf (event-value event slot) value))

(defun remove-event-value (event slot)
  "Removes SLOT from EVENT."
  (with-slots (event-plist) event
    (setf event-plist (alexandria:remove-from-plist event-plist (alexandria:make-keyword slot))))
  event)

(defun raw-event-value (event slot)
  "Get the value of SLOT in EVENT without running any conversion functions."
  (getf (slot-value event 'event-plist) (alexandria:make-keyword slot)))

(defun get-event-value (event &optional slot) ;; FIX: swap argument order, stop using this in cl-patterns
  "Get the value of SLOT in EVENT, running any necessary conversion functions.

The EVENT parameter can be omitted, in which case the SLOT slot is looked up from `*event*'.

Returns 2 values: the value of the slot, and the name of the slot the value was derived from (or t if the default value of the slot was used, or nil if no value or default was provided).

See also: `event-value'"
  (when (null slot) ;; if called like (get-event-value :foo) then assume *event* is the event and :foo is the slot.
    (assert (typep event 'symbol))
    (return-from get-event-value (get-event-value *event* event)))
  (event-value event slot))

(defun combine-events (&rest events)
  "Returns an event that inserts all the items in each event of EVENTS. Keys from the events listed first will be overwritten by later events.

See also: `split-event-by-lists'"
  (let ((result (loop :for ev :in events
                   :if (null ev)
                   :return nil
                   :append (loop :for key :in (keys ev)
                              :append (list key (event-value ev key))))))
    (when (and result
               (null (position-if #'null result)))
      (apply #'event result))))

(defun split-event-by-lists (event)
  "Split an event up by any lists in its values. Also known as multichannel expansion.

Example:

;; (split-event-with-lists (event :foo 1 :bar (list 1 2) :baz (list 3 4 5)))
;;
;; ;=> ((EVENT :FOO 1 :BAR 1 :BAZ 3)
;;      (EVENT :FOO 1 :BAR 2 :BAZ 4)
;;      (EVENT :FOO 1 :BAR 1 :BAZ 5))

See also: `combine-events'"
  (let ((length (reduce 'max
                        (mapcar
                         (lambda (x) (length (alexandria:ensure-list x)))
                         (loop :for key :in (keys event)
                            :collect (event-value event key))))))
    (if (= 1 length)
        (list event)
        (loop :for i :from 0 :below length
           :collect
             (apply 'event
                    (loop :for key :in (keys event)
                       :append (list key (nth-wrap i (alexandria:ensure-list (event-value event key))))))))))

(defun play-test (item &optional pstream)
  "Simply output information about the event that's being played. Useful for diagnostics when no audio output is available."
  (declare (ignore pstream))
  (format t "Playing ~s at ~f.~%" item (/ (get-internal-real-time) internal-time-units-per-second)))

(defmethod keys ((item event))
  (keys (slot-value item 'event-plist)))

(defmethod print-object ((item event) stream)
  (format stream "(~s~{ ~s ~s~})" 'event (event-plist item)))

(defmacro define-event-special-slot (name cases &key (remove-keys t) (define-methods nil))
  "Define a special slot with the key NAME for events (i.e. slots that take their values from other slots, or slots that have default values).

CASES is a plist of cases mapping event slot names to forms. When `event-value' is called on an event for the NAME slot, then the event is tested for the keys of CASES in the order they're listed. The associated form of the first key of CASES that exists in the event is evaluated to get the value of that call to `event-value'.

If no case is provided with a KEY that's the same as NAME, one is automatically added at the start with a form that just returns the value of the event's NAME slot. If a case is provided with t as its KEY, that case is always run when found. This allows you to set a default value for the slot if none of the other keys from CASES exist in the event.

REMOVE-KEYS is a list of keys to remove from the event when the NAME key is being set with `(setf event-value)'. If t (the default), all keys in CASES will be removed from the event.

DEFINE-METHODS, if true, will cause the macro to define methods for getting and setting the slot in an event.

Example:
;; (define-event-special-slot amp (:db (db-amp (raw-event-value event :db))
;;                                 t 0.5)
;;   :define-methods t)

This defines the amp slot for events. Since the :amp KEY is implied, it doesn't need to be specified in the CASES. Thus if the event already has an :amp key, its value will be used by default. If no :amp key exists in the event, then the :db FUNC is run if the :db key exists. If neither :amp nor :db exist in the event, then the t key is run, giving a default of 0.5.

Additionally, because :define-methods is true, we can also do the following:

;; (defparameter *foo* (event :amp 0.9))
;; (amp *foo*) ;=> 0.9
;; (setf (amp *foo*) 0.7)"
  ;; FIX: does not handle cases with multiple keys. (i.e. (((:foo :bar) 5)))
  (let ((kwname (alexandria:make-keyword name)))
    (unless (position kwname (keys cases))
      (setf cases (append (list kwname (list 'raw-event-value 'event kwname)) cases)))
    `(progn
       (setf *event-special-slots*
             (plist-set *event-special-slots* ,kwname (list
                                                       (list ,@(loop :for (key value) :on cases :by #'cddr
                                                                  :append (list
                                                                           (if (eq key t)
                                                                               key
                                                                               (alexandria:make-keyword key))
                                                                           `(lambda (event)
                                                                              (declare (ignorable event))
                                                                              ,value))))
                                                       (list ,@(alexandria:ensure-list remove-keys)))))
       ,(when define-methods
          `(progn
             (defmethod ,name ((event null)) nil)
             (defmethod ,name ((event event))
               (event-value event ,kwname))
             (defmethod (setf ,name) (value (event event))
               (setf (event-value event ,kwname) value)))))))

;;; instrument/group/out

(define-event-special-slot instrument (t :default)
  :define-methods t)

;; (defgeneric instrument (item))

;; (defmethod instrument ((item null)) nil)

;; (defmethod instrument ((item event))
;;   (event-value item :instrument))

(define-event-special-slot group (t 0))

(define-event-special-slot out (t 0))

;;; amp/pan

(define-event-special-slot amp (:db (db-amp (raw-event-value event :db))
                                    t 0.5)
  :define-methods t)

(define-event-special-slot db (:amp (amp-db (raw-event-value event :amp))
                                    t (amp-db 0.5))
  :define-methods t)

(define-event-special-slot pan (t 0)
  :define-methods t)

;;; dur/delta

(define-event-special-slot tempo (t (if (and (boundp '*clock*) (not (null *clock*)))
                                         (tempo *clock*)
                                         1)))

(define-event-special-slot delta (:dur (event-value event :dur)
                                       t (event-value event :dur))
  :remove-keys nil
  :define-methods t)

(define-event-special-slot dur (:delta (raw-event-value event :delta)
                                       t 1)
  :remove-keys nil
  :define-methods t)

;; sustain/legato

(define-event-special-slot sustain (t (* (event-value event :legato)
                                         (event-value event :dur)))
  :remove-keys (:legato)
  :define-methods t)

(define-event-special-slot legato (:sustain (* (raw-event-value event :sustain)
                                               (event-value event :dur))
                                            t 0.8)
  :remove-keys (:sustain)
  :define-methods t)

;;; timing-offset

(define-event-special-slot timing-offset (t 0))

;;; quant

(define-event-special-slot quant (t 1)
  :define-methods t)

;;; freq/midinote/degree/octave/root/scale

(define-event-special-slot freq (:midinote (midinote-freq (event-value event :midinote))
                                           :degree (degree-freq (event-value event :degree)
                                                                :root (event-value event :root)
                                                                :octave (event-value event :octave)
                                                                :scale (event-value event :scale))
                                           t 440)
  :remove-keys (:midinote :degree :root :octave)
  :define-methods t)

(define-event-special-slot midinote (:freq (freq-midinote (event-value event :freq))
                                           :degree (degree-midinote (event-value event :degree)
                                                                    :root (event-value event :root)
                                                                    :octave (event-value event :octave)
                                                                    :scale (event-value event :scale))
                                           t 69)
  :remove-keys (:freq :degree :root :octave)
  :define-methods t)

;; FIX: this can return NIL. i.e. (degree (event :midinote 0))
(define-event-special-slot degree (:freq (midinote-degree (freq-midinote (event-value event :freq))
                                                          :root (event-value event :root)
                                                          :octave (event-value event :octave)
                                                          :scale (event-value event :scale))
                                         :midinote (midinote-degree (event-value event :midinote)
                                                                    :root (event-value event :root)
                                                                    :octave (event-value event :octave)
                                                                    :scale (event-value event :scale))
                                         t 5)
  :remove-keys (:freq :midinote)
  :define-methods t)

(define-event-special-slot root (t 0) ;; FIX: can we derive this when :freq, :midinote, :degree, etc are available?
  :remove-keys (:freq :midinote)
  :define-methods t)

(define-event-special-slot octave (:freq (freq-octave (raw-event-value event :freq))
                                         :midinote (midinote-octave (raw-event-value event :midinote))
                                         t 5)
  :remove-keys (:freq :midinote)
  :define-methods t)

(define-event-special-slot scale (t :major)
  :remove-keys (:freq :midinote))

;;; remaining

(define-event-special-slot remaining (t :inf))
