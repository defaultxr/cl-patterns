(in-package #:cl-patterns)

;; FIX: make versions of these generic functions that will work with supercollider ugens and put them in supercollider.lisp.
;; FIX: need to test weird scales/tunings to make sure they're converting correctly, etc.
;; FIX: implement more keys and event types (see TODO.org)
;; FIX: (play (event :octave 2)) doesn't work properly (it still plays the default note)

;;; event glue

(defgeneric event-plist (event)
  (:documentation "The raw plist containing the key/value pairs of the event."))

(defclass event ()
  ((event-plist :initarg :event-plist :initform (list) :reader event-plist :type list :documentation "The plist containing all of the event's keys and values.")
   (%beat :initform nil :type (or null number) :documentation "The time in beats when this event occurred in the pstream. Generally you should use `beat' instead."))
  (:documentation "Class representing a musical event."))

(defmethod print-object ((item event) stream)
  (format stream "(~s~{ ~s ~s~})" 'event (event-plist item)))

(defun event (&rest params)
  "Create an event, using the PARAMS as its keys/values."
  (assert (= 0 (mod (length params) 2)))
  (let ((ev (make-instance 'event)))
    (labels ((accumulator (pairs)
               (when (not (null (car pairs)))
                 (setf (event-value ev (ensure-symbol (car pairs) 'cl-patterns)) (cadr pairs))
                 (accumulator (cddr pairs)))))
      (accumulator params)
      ev)))

(defvar *latency* 1/10
  "Default latency for events, in seconds.")

(defparameter *event-special-keys* (list)
  "Plist mapping event special keys to their case lists.")

(defun event-p (object)
  "Return true if OBJECT is an event, and NIL otherwise."
  (typep object 'event))

(defun event-value (event key)
  "Get the value of KEY in EVENT, running any necessary conversion functions.

Returns 2 values: the value of the key, and the name of the key the value was derived from (or t if the default value of the key was used, or nil if no value or default was provided).

See also: `e'"
  (when (null event)
    (return-from event-value (values nil nil)))
  (let* ((key (make-keyword key))
         (cases (car (getf *event-special-keys* key)))
         (cases (if (not (position key (keys cases))) ;; FIX: move this to when the special-key is defined instead?
                    (append (list key (lambda (event) (raw-event-value event key))) cases)
                    cases))
         (cases-keys (keys cases))
         (key (car (or (member-if (lambda (k) (position k (keys event))) cases-keys)
                       (member t cases-keys))))
         (func (getf cases key))
         (res (if (null func)
                  (list nil nil)
                  (multiple-value-list (funcall func event)))))
    (values (car res) (or (cadr res) key))))

(defun e (key)
  "Syntax sugar; like `event-value', but always gets the value from `*event*'.

See also: `event-value'"
  (event-value *event* key))

(defun event-equal (event1 event2)
  "Test if EVENT1 and EVENT2 are equivalent.

See also: `every-event-equal'"
  (let ((types (mapcar #'type-of (list event1 event2))))
    (cond ((set-equal types (list 'cons 'cons))
           (every-event-equal event1 event2))
          ((set-equal types (list 'event 'event))
           (let ((ev1-keys (keys event1)))
             (and (set-equal ev1-keys (keys event2))
                  (every (lambda (key)
                           (equal (event-value event1 key)
                                  (event-value event2 key)))
                         ev1-keys))))
          ((set-equal types (list 'cons 'event))
           (event-equal (ensure-list event1) (ensure-list event2)))
          (t (equal event1 event2)))))

(defun every-event-equal (&rest lists)
  "Test if all the events in LISTS are equivalent. Similar to (every #'event-equal LIST-1 LIST-2...) except that it will fail if the lists are not the same length.

See also: `events-differing-keys'"
  (and (apply #'= (mapcar #'length lists))
       (apply #'every #'event-equal lists)))

(defun events-differing-keys (&rest events)
  "Get a list of keys that differ between EVENTS.

See also: `every-event-equal', `events-lists-differing-keys'"
  (loop :for key :in (remove-duplicates (flatten (mapcar (lambda (event) (keys event)) events)))
        :unless (every (lambda (event) (equal (event-value (nth 0 events) key)
                                              (event-value event key)))
                       events)
          :collect key))

(defun events-lists-differing-keys (&rest lists) ;; FIX: what is this for? is it needed?
  "Get a list of the keys that differ between respective event in LISTS.

Example:

;; (events-lists-differing-keys (list (event :foo 1 :bar 2) (event :bar 3)        (event :foo 1 :bar 3))
;;                              (list (event :foo 1 :bar 2) (event :foo 1 :bar 3) (event :foo 2 :bar 3)))
;; => (NIL (:FOO) (:FOO))

See also: `every-event-equal'"
  (loop :for idx :from 0 :below (reduce #'max (mapcar #'length lists))
     :collect (apply #'events-differing-keys (mapcar (lambda (list) (nth idx list)) lists))))

(defun (setf event-value) (value event key)
  "Set the value of KEY to VALUE in EVENT, running any conversion functions that exist."
  (let* ((key (make-keyword key))
         (cases (getf *event-special-keys* key)))
    (when (cadr cases) ;; remove-keys
      (let ((keys (remove-if (lambda (c) (eql c t)) (keys (car cases)))))
        (loop :for i :in keys
           :do (remove-event-value event i))))
    (raw-set-event-value event key value)
    (when (eql key :beat)
      (setf (slot-value event '%beat) value))
    value))

(defun (setf e) (value key)
  (if (event-p *event*)
      (setf (event-value *event* key) value)
      (error "Can't setf (e ~s); *EVENT* is not currently set to an event." key)))

(defun raw-set-event-value (event key value)
  "Set the value of KEY to VALUE in EVENT without running any conversion functions."
  (with-slots (event-plist) event
    (setf event-plist (plist-set event-plist (make-keyword key) value))))

(defun remove-event-value (event key)
  "Removes KEY from EVENT."
  (with-slots (event-plist) event
    (setf event-plist (remove-from-plist event-plist (make-keyword key))))
  event)

(defun raw-event-value (event key)
  "Get the value of KEY in EVENT without running any conversion functions."
  (getf (slot-value event 'event-plist) (make-keyword key)))

(defun combine-events (&rest events)
  "Returns a new event that inserts all the items in each event of EVENTS. Keys from the events listed first will be overwritten by later events.

See also: `copy-event', `split-event-by-lists', `combine-events-via-lists'"
  (unless (position nil events)
    (let ((ne (event)))
      (dolist (ev events ne)
        (dolist (key (keys ev))
          (if-let ((val (event-value ev key)))
            (setf (event-value ne key) val)
            (return-from combine-events nil)))
        (unless (raw-event-value ne :beat)
          (when-let ((ibeat (slot-value ev '%beat)))
            (setf (slot-value ne '%beat) ibeat)))))))

(defun copy-event (event)
  "Return a new event that is a copy of EVENT.

See also: `combine-events'"
  (combine-events event))

(defun split-event-by-lists (event)
  "Split an event up by any lists in its values. Also known as multichannel expansion.

Example:

;; (split-event-with-lists (event :foo 1 :bar (list 1 2) :baz (list 3 4 5)))
;;
;; => ((EVENT :FOO 1 :BAR 1 :BAZ 3)
;;     (EVENT :FOO 1 :BAR 2 :BAZ 4)
;;     (EVENT :FOO 1 :BAR 1 :BAZ 5))

See also: `multi-channel-funcall', `combine-events-via-lists', `combine-events'"
  (let ((length (reduce 'max
                        (mapcar
                         (lambda (x) (length (ensure-list x)))
                         (loop :for key :in (keys event)
                               :collect (event-value event key))))))
    (if (= 1 length)
        (list event)
        (loop :for i :from 0 :below length
              :collect
              (apply 'event
                     (loop :for key :in (keys event)
                           :append (list key (elt-wrap (ensure-list (event-value event key)) i))))))))

(defun combine-events-via-lists (&rest events)
  "Combine EVENTS together to produce one event. Any keys that differ between the events will have be set to lists containing all the values from each event (unless the value is null). This is the opposite of `split-event-by-lists'.

Example:

;; (combine-events-via-lists (event :foo 1 :bar 2 :qux 4) (event :foo 1 :bar 3 :baz 5))
;; => (EVENT :FOO 1 :BAR (2 3) :QUX 4 :BAZ 5)

See also: `split-event-by-lists', `combine-events'"
  ;; FIX
  (let ((event (event))
        (listified nil))
    (dolist (ev events)
      (dolist (key (keys ev))
        (let ((cv (event-value event key))
              (nv (event-value ev key)))
          (if (null cv)
              (setf (event-value event key) nv)
              (when (not (eql cv nv))
                (setf (event-value event key) (append (if (position key listified)
                                                          cv
                                                          (progn
                                                            (push key listified)
                                                            (list cv)))
                                                      (list nv))))))))
    event))

(defmethod keys ((item event))
  (keys (slot-value item 'event-plist)))

(defmethod beat ((item event))
  (event-value item :beat))

(defmethod (setf beat) (value (event event))
  (setf (slot-value event '%beat) value)
  (when (member :beat (keys event))
    (setf (event-value event :beat) value)))


(defmethod rest-p ((event event))
  (eql :rest (event-value event :type)))

(defmacro define-event-special-key (name cases &key (remove-keys t) (define-methods nil) documentation)
  "Define a special key with the key NAME for events (i.e. keys that take their values from other keys, or keys that have default values).

CASES is an alist of cases mapping event key names to forms. When `event-value' is called on an event for the NAME key, then the event is tested for the keys of CASES in the order they're listed. The associated form of the first key of CASES that exists in the event is evaluated to get the value of that call to `event-value'.

If no case is provided with a key that's the same as NAME, one is automatically added at the start with a form that just returns the value of the event's NAME key. If a case is provided with t as its key, that case is always run when found. This allows you to set a default value for the key if none of the other keys from CASES exist in the event.

REMOVE-KEYS is a list of keys to remove from the event when the NAME key is being set with `(setf event-value)'. If t (the default), all keys in CASES will be removed from the event.

DEFINE-METHODS, if true, will cause the macro to define methods for getting and setting the key in an event.

DOCUMENTATION is the documentation string for the function.

Example:
;; (define-event-special-key amp ((:db (db-amp (raw-event-value event :db)))
;;                                (t 0.5))
;;   :define-methods t)

This defines the amp key for events. Since the :amp KEY is implied, it doesn't need to be specified in the CASES. Thus if the event already has an :amp key, its value will be used by default. If no :amp key exists in the event, then the :db FUNC is run if the :db key exists. If neither :amp nor :db exist in the event, then the t key is run, giving a default of 0.5.

Additionally, because :define-methods is true, we can also do the following:

;; (defparameter *foo* (event :amp 0.9))
;; (amp *foo*) ; => 0.9
;; (setf (amp *foo*) 0.7)"
  ;; FIX: does not handle cases with multiple keys. (i.e. (((:foo :bar) 5)))
  (let ((kwname (make-keyword name)))
    (unless (position kwname (mapcar #'car cases))
      (setf cases (cons (list kwname (list 'raw-event-value 'event kwname)) cases)))
    `(progn
       (setf *event-special-keys*
             (plist-set *event-special-keys* ,kwname (list
                                                      (list ,@(loop :for case :in cases
                                                                 :for key = (car case)
                                                                 :for value = (cadr case)
                                                                 :append (list
                                                                          (if (eql key t)
                                                                              key
                                                                              (make-keyword key))
                                                                          `(lambda (event)
                                                                             (declare (ignorable event))
                                                                             ,value))))
                                                      (list ,@(ensure-list remove-keys)))))
       ,(when define-methods
          `(progn
             ,(when documentation
                `(defgeneric ,name (object)
                   (:documentation ,documentation)))
             (defmethod ,name ((event null)) nil)
             (defmethod ,name ((event event))
               (event-value event ,kwname))
             (defmethod (setf ,name) (value (event event))
               (setf (event-value event ,kwname) value)))))))

;;; type

(define-event-special-key type ((t :note)))

;;; instrument/group/out

(define-event-special-key instrument ((t :default))
  :define-methods t
  :documentation "The instrument or synth to trigger.")

(define-event-special-key group ((t 1)))

(define-event-special-key out ((t 0)))

;;; amp/pan

(define-event-special-key amp ((:db (db-amp (raw-event-value event :db)))
                               (t 0.5))
  :define-methods t
  :documentation "Volume in amplitude, from 0 to 1.")

(define-event-special-key db ((:amp (amp-db (raw-event-value event :amp)))
                              (t (amp-db 0.5)))
  :define-methods t
  :documentation "Volume in decibels (dB).")

(define-event-special-key pan ((t 0))
  :define-methods t
  :documentation "Stereo panning, where -1 is fully left, 1 is fully right, and 0 is center.")

;;; dur/delta

(define-event-special-key tempo ((t (if (and (boundp '*clock*) (not (null *clock*)))
                                        (values (tempo *clock*) :tempo)
                                        1))))

(define-event-special-key beat ((t (or (raw-event-value event :beat)
                                       (slot-value event '%beat)))))

(define-event-special-key delta ((:dur (event-value event :dur))
                                 (t (event-value event :dur)))
  :remove-keys nil
  :define-methods t
  :documentation "The number of beats between the start of this event and the start of the next one.

See also: `dur', `sustain'")

(define-event-special-key dur ((t 1))
  :remove-keys nil
  :define-methods t
  :documentation "The total duration of the note, in beats.

See also: `delta', `legato'")

;; sustain/legato

(define-event-special-key sustain ((t (* (event-value event :legato)
                                         (event-value event :dur))))
  :remove-keys (:legato)
  :define-methods t
  :documentation "How long the note should be held, in beats.

See also: `legato', `delta'")

(define-event-special-key legato ((:sustain (* (raw-event-value event :sustain)
                                               (event-value event :dur)))
                                  (t 0.8))
  :remove-keys (:sustain)
  :define-methods t
  :documentation "How long the note should be held, in beats, as a factor of its total duration (`dur').

See also: `sustain', `dur'")

;;; timing

(define-event-special-key timing-offset ((t 0)))

(define-event-special-key quant ((:quant (ensure-list (raw-event-value event :quant)))
                                 (t (list 1)))
  :define-methods t)

;;; pitch

(define-event-special-key freq ((:note (note-freq (event-value event :note)))
                                (:midinote (midinote-freq (event-value event :midinote)))
                                (:degree (degree-freq (event-value event :degree)
                                                      :root (event-value event :root)
                                                      :octave (event-value event :octave)
                                                      :scale (event-value event :scale)))
                                (t 440))
  :remove-keys (:midinote :degree :root :octave)
  :define-methods t
  :documentation "Frequency of the note, in Hz.

See also: `rate', `midinote', `degree'")

(define-event-special-key note ((:freq (freq-note (event-value event :freq)))
                                (:midinote (midinote-note (event-value event :midinote)))
                                (:degree (degree-note (event-value event :degree)
                                                      (event-value event :scale)))
                                (t 0))
  :remove-keys (:freq :midinote :degree :root :octave)
  :documentation "Note number relative to the root.")

(define-event-special-key midinote ((:freq (freq-midinote (event-value event :freq)))
                                    (:note (note-midinote (event-value event :note)))
                                    (:degree (degree-midinote (event-value event :degree)
                                                              :root (event-value event :root)
                                                              :octave (event-value event :octave)
                                                              :scale (event-value event :scale)))
                                    (t 69))
  :remove-keys (:freq :note :degree :root :octave)
  :define-methods t
  :documentation "MIDI note number of the note (0-127).")

(define-event-special-key degree ((:freq (freq-degree (event-value event :freq)
                                                      :root (event-value event :root)
                                                      :octave (event-value event :octave)
                                                      :scale (event-value event :scale)))
                                  (:note (note-degree (event-value event :note)))
                                  (:midinote (midinote-degree (event-value event :midinote)
                                                              :root (event-value event :root)
                                                              :octave (event-value event :octave)
                                                              :scale (event-value event :scale)))
                                  (t 5))
  :remove-keys (:freq :note :midinote))

(define-event-special-key root ((t 0)) ;; FIX: can we derive this when :freq, :midinote, :degree, etc are available?
  :remove-keys (:freq :note :midinote))

(define-event-special-key octave ((:freq (freq-octave (raw-event-value event :freq)))
                                  (:midinote (midinote-octave (raw-event-value event :midinote)))
                                  (t 5))
  :remove-keys (:freq :note :midinote))

(define-event-special-key scale ((t :major))
  :remove-keys (:freq :note :midinote)
  :define-methods t)

(define-event-special-key base-freq ((:base-note (midinote-freq (event-value event :base-note)))
                                     (t 440)))

(define-event-special-key base-note ((:base-freq (freq-midinote (event-value event :base-freq)))
                                     (t 69)))

(define-event-special-key rate ((t (let ((res (multiple-value-list (event-value event :freq))))
                                     (values (freq-rate (car res) (event-value event :base-freq))
                                             (cadr res)))))
  ;; we don't do remove-keys because other frequency keys are not exact synonyms.
  ;; it's also conceivable that some instruments may use both :rate and :freq.
  :define-methods t
  :documentation "Playback rate, i.e. for playing back sound buffers. 1 is normal speed, 2 is twice as fast, 0.5 half as fast, etc.")
