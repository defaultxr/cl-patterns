(in-package :cl-patterns)

;; FIX: make versions of these generic functions that will work with supercollider ugens; put them in supercollider.lisp..
;; FIX: need to test weird scales/tunings to make sure they're converting correctly, etc.

;;; event glue

(defclass event ()
  ((other-params :initarg :other-params :initform (list) :type 'list))
  (:documentation "Class representing a musical event."))

(defun event (&rest params)
  "Create an event, using the PARAMS as its keys/values."
  (assert (= 0 (mod (length params) 2)))
  (let ((ev (make-instance 'event)))
    (labels ((accumulator (pairs)
               (when (not (null (car pairs)))
                 (set-event-value ev (alexandria:ensure-symbol (car pairs) 'cl-patterns) (cadr pairs))
                 (accumulator (cddr pairs)))))
      (accumulator params)
      ev)))

(defparameter *latency* 0.1
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
                    (append (list slot (lambda (event) (raw-get-event-value event slot))) cases)
                    cases))
         (cases-keys (keys cases))
         (key (car (or (member-if (lambda (k) (position k (keys event))) cases-keys)
                       (member t cases-keys))))
         (func (getf cases key)))
    (values (if (null func)
                nil
                (funcall func event))
            key)))

(defun (setf event-value) (value event slot) ;; FIX: this should be the main function and set-event-value should forward to it.
  (set-event-value event slot value))

;; (define-setf-expander )

(defun raw-set-event-value (event slot value)
  "Set the value of SLOT to VALUE in EVENT without running any conversion functions."
  (with-slots (other-params) event
    (setf other-params (plist-set other-params (alexandria:make-keyword slot) value))))

(defun set-event-value (event slot value)
  "Set the value of SLOT to VALUE in EVENT, running any conversion functions that exist."
  (let* ((slot (alexandria:make-keyword slot))
         (cases (getf *event-special-slots* slot)))
    (when (cadr cases) ;; remove-keys
      (let ((keys (remove-if (lambda (c) (eq c t)) (keys (car cases)))))
        (loop :for i :in keys
           :do (remove-event-value event i))))
    (raw-set-event-value event slot value)
    ;; (if (fboundp (list 'setf slot))
    ;;     (funcall (fdefinition (list 'setf slot)) value event)
    ;;     (raw-set-event-value event slot value))
    ))

(defun remove-event-value (event slot)
  "Removes SLOT from EVENT."
  (with-slots (other-params) event
    (setf other-params (alexandria:remove-from-plist other-params (alexandria:make-keyword slot)))))

(defun raw-get-event-value (event slot)
  "Get the value of SLOT in EVENT without running any conversion functions."
  (getf (slot-value event 'other-params) (alexandria:make-keyword slot)))

(defun get-event-value (event &optional slot) ;; FIX: swap argument order, stop using this in cl-patterns
  "Get the value of SLOT in EVENT, running any necessary conversion functions.

Returns 2 values: the value of the slot, and the name of the slot the value was derived from (or t if the default value of the slot was used, or nil if no value or default was provided)."
  (when (null slot) ;; if called like (get-event-value :foo) then assume *event* is the event and :foo is the slot.
    (assert (typep event 'symbol))
    (return-from get-event-value (get-event-value *event* event)))
  (event-value event slot))

(defun combine-events (&rest events)
  "Returns an event that inserts all the items in each event of EVENTS. Keys from the events listed first will be overwritten by later events."
  (let ((result (loop :for ev :in events
                   :if (null ev)
                   :return nil
                   :append (loop :for key :in (keys ev)
                              :append (list key (get-event-value ev key))))))
    (when (and result
               (null (position-if #'null result)))
      (apply #'event result))))

(defun play-test (item &optional pstream)
  "Simply output information about the event that's being played. Useful for diagnostics when no audio output is available."
  (declare (ignore pstream))
  (format t "Playing ~s at ~f.~%" item (/ (get-internal-real-time) internal-time-units-per-second)))

(defparameter *event-output-function* 'play-test
  "Which function to `play' an event with.")

(defmethod keys ((item event))
  (keys (slot-value item 'other-params)))

(defun plist-set (plist key value) ;; doesn't actually setf the place; only returns an altered plist.
  "Return a new copy of PLIST, but with its KEY set to VALUE. If VALUE is nil, return a copy without KEY."
  (if (null value)
      (alexandria:remove-from-plist plist key)
      (if (getf plist key)
          (progn
            (setf (getf plist key) value)
            plist)
          (append plist (list key value)))))

(defun event-plist (event)
  "Return EVENT as a plist."
  (slot-value event 'other-params))

(defmethod print-object ((item event) stream)
  (format stream "(~s~{ ~s ~s~})" 'event (event-plist item)))

(defmacro define-event-special-slot (name cases &key (remove-keys t))
  "Define a special slot with the key NAME for events (i.e. slots that take their values from other slots, or slots that have default values).

CASES is a plist of cases mapping event slot names to forms. When `get-event-value' is called on an event for the NAME slot, then the event is tested for the keys of CASES in the order they're listed. The associated form of the first key of CASES that exists in the event is evaluated to get the value of that call to `get-event-value'.

If no case is provided with a KEY that's the same as NAME, one is automatically added at the start with a form that just returns the value of the event's NAME slot. If a case is provided with t as its KEY, that case is always run when found. This allows you to set a default value for the slot if none of the other keys from CASES exist in the event.

REMOVE-KEYS is a list of keys to remove from the event when the NAME key is being set with `set-event-value'. If t (the default), all keys in CASES will be removed from the event.

Example:
(define-event-special-slot amp (:db (db-amp (raw-get-event-value event :db))
                                t 0.5))

This defines the amp slot for events. Since the :amp KEY is implied, it doesn't need to be specified in the CASES. Thus if the event already has an :amp key, its value will be used by default. If no :amp key exists in the event, then the :db FUNC is run if the :db key exists. If neither :amp nor :db exist in the event, then the t key is run, giving a default of 0.5."
  ;; FIX: does not handle cases with multiple keys. (i.e. (((:foo :bar) 5)))
  (let ((name (alexandria:make-keyword name)))
    (unless (position name (keys cases))
      (setf cases (append (list name (list 'raw-get-event-value 'event name)) cases)))
    `(setf *event-special-slots*
           (plist-set *event-special-slots* ,name (list
                                                   (list ,@(loop :for (key value) :on cases :by #'cddr
                                                              :append (list
                                                                       (if (eq key t)
                                                                           key
                                                                           (alexandria:make-keyword key))
                                                                       `(lambda (event)
                                                                          (declare (ignorable event))
                                                                          ,value))))
                                                   (list ,@(alexandria:ensure-list remove-keys)))))))

;;; instrument/group/out

(define-event-special-slot instrument (t :default))

(defgeneric instrument (item))

(defmethod instrument ((item null)) nil)

(defmethod instrument ((item event))
  (get-event-value item :instrument))

(define-event-special-slot group (t 0))

(define-event-special-slot out (t 0))

;;; amp/pan

(define-event-special-slot amp (:db (db-amp (raw-get-event-value event :db))
                                    t 0.5))

(define-event-special-slot db (:amp (amp-db (raw-get-event-value event :amp))
                                    t (amp-db 0.5)))

(define-event-special-slot pan (t 0))

;;; dur/delta

(define-event-special-slot tempo (t (if (and (boundp '*clock*) (not (null *clock*)))
                                         (tempo *clock*)
                                         1)))

(define-event-special-slot delta (:dur (get-event-value event :dur)
                                       t (get-event-value event :dur))
  :remove-keys nil)

(defgeneric delta (item))

(defmethod delta ((item event))
  (get-event-value item :delta))

;; (defgeneric (setf delta) (value item))

;; (defmethod (setf delta) (value (item event))
;;   (set-event-value item :delta value))

(define-event-special-slot dur (:delta (raw-get-event-value event :delta)
                                       t 1)
  :remove-keys nil)

(defgeneric dur (item))

(defmethod dur ((item event))
  (get-event-value item :dur))

;; (defgeneric (setf dur) (value item))

;; (defmethod (setf dur) (value (item event))
;;   (set-event-value item :dur value))

;; sustain/legato

(define-event-special-slot sustain (t (* (get-event-value event :legato)
                                         (get-event-value event :dur)))
  :remove-keys (:legato))

(defgeneric sustain (item))

(defmethod sustain ((item event))
  (get-event-value item :sustain))

;; (defgeneric (setf sustain) (value item))

;; (defmethod (setf sustain) (value (item event))
;;   (set-event-value item :sustain value))

(define-event-special-slot timing-offset (t 0))

(define-event-special-slot legato (:sustain (* (raw-get-event-value event :sustain)
                                               (get-event-value event :dur))
                                            t 0.8)
  :remove-keys (:sustain))

(defgeneric legato (item))

(defmethod legato ((item event))
  (get-event-value item :legato))

;; (defgeneric (setf legato) (value item))

;; (defmethod (setf legato) (value (item event))
;;   (set-event-value item :legato value))

;;; quant

(define-event-special-slot quant (t 1))

(defgeneric quant (item))

(defmethod quant ((item event))
  (get-event-value item :quant))

;;; freq/midinote/degree/octave/root/scale

(define-event-special-slot freq (:midinote (midinote-freq (get-event-value event :midinote))
                                           :degree (degree-freq (get-event-value event :degree)
                                                       (get-event-value event :root)
                                                       (get-event-value event :octave)
                                                       (get-event-value event :scale))
                                           t 440)
  :remove-keys (:midinote :degree :root :octave))

(defmethod freq ((item event))
  (get-event-value item :freq))

;; (defmethod (setf freq) (value (item event))
;;   (set-event-value item :freq value))

(define-event-special-slot midinote (:freq (freq-midinote (get-event-value event :freq))
                                           :degree (degree-midinote (get-event-value event :degree)
                                                                    (get-event-value event :root)
                                                                    (get-event-value event :octave)
                                                                    (get-event-value event :scale))
                                           t 69)
  :remove-keys (:freq :degree :root :octave))

(defmethod midinote ((item event))
  (get-event-value item :midinote))

;; (defmethod (setf midinote) (value (item event))
;;   (set-event-value item :midinote value))

;; FIX: this can return NIL. i.e. (degree (event :midinote 0))
(define-event-special-slot degree (:freq (midinote-degree (freq-midinote (get-event-value event :freq))
                                                          (get-event-value event :root) (get-event-value event :octave) (get-event-value event :scale))
                                         :midinote (midinote-degree (get-event-value event :midinote)
                                                                    (get-event-value event :root) (get-event-value event :octave) (get-event-value event :scale))
                                         t 5)
  :remove-keys (:freq :midinote))

(defmethod degree ((item event))
  (get-event-value item :degree))

;; (defmethod (setf degree) (value (item event))
;;   (set-event-value item :degree value))

(define-event-special-slot root (t 0) ;; FIX: can we derive this when :freq, :midinote, :degree, etc are available?
  :remove-keys (:freq :midinote))

;; (defmethod (setf root) (value (item event))
;;   (set-event-value item :root value))

(define-event-special-slot octave (:freq (freq-octave (raw-get-event-value event :freq))
                                         :midinote (midinote-octave (raw-get-event-value event :midinote))
                                         t 5)
  :remove-keys (:freq :midinote))

;; (defmethod (setf octave) (value (item event))
;;   (set-event-value item :octave value))

(define-event-special-slot scale (t :major)
  :remove-keys (:freq :midinote))

;;; remaining

(define-event-special-slot remaining (t :inf))
