(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;;; t/event.lisp - tests for `event' and related functionality.

;;; TODO:
;; FIX: add more

(test event
  "Test event functionality"
  (is (= 1
         (event-value (event :dur 0 :sustain 1) :sustain))
      "event returns the wrong sustain when sustain is provided and dur is 0")
  (is (= 0.8
         (event-value (event) :sustain))
      "event returns the wrong default value for sustain")
  (is (= 0.5
         (event-value (event :dur 0 :legato 0.5) :legato))
      "event returns the wrong legato when legato is provided and dur is 0")
  (is (= 0.8
         (event-value (event) :legato))
      "event returns the wrong default value for legato")
  (is (= 1
         (event-value (event) :dur))
      "event returns the wrong default value for dur")
  (is (eql :default
           (event-value (event) :instrument))
      "event returns the wrong default value for instrument")
  (is (= (amp-db 0.125)
         (event-value (event :amp 0.125) :db))
      "event incorrectly converts amp to db")
  (is (= (db-amp -7)
         (event-value (event :db -7) :amp))
      "event incorrectly converts db to amp")
  (is (eql :freq
           (cadr (multiple-value-list (event-value (event :freq 420) :midinote))))
      "event-value does not provide the key it derives its value from as the second return value")
  (is (eql :freq
           (cadr (multiple-value-list (event-value (event :freq 420) :rate))))
      "event-value does not provide the key it derives its value from as the second return value when called for :rate")
  (is-true (let ((*clock* (make-clock 9/7)))
             (equal (list 9/7 :tempo)
                    (multiple-value-list (event-value (event) :tempo))))
           "event-value doesn't provide :tempo when getting the tempo from *clock*"))

(test event-beat
  "Test the beat key for events"
  (is-true (= 5
              (slot-value (event :beat 5) 'cl-patterns::%beat))
           "event doesn't set the internal %beat slot correctly")
  (is-true (= 2
              (slot-value (combine-events
                           (event :beat 3)
                           (event :beat 2))
                          'cl-patterns::%beat))
           "combine-events doesn't set the internal %beat slot correctly")
  (is-true (= 94
              (let ((ev (event)))
                (setf (event-value ev :beat) 94)
                (slot-value ev 'cl-patterns::%beat)))
           "setting an event's :beat key incorrectly sets its %beat slot"))

(test event-equal
  "Test event-equal"
  (is-true (event-equal (event :dur 1) (event :dur 1))
           "event-equal doesn't return true for equivalent events")
  (is-false (event-equal (event :dur 1) (event :dur 1 :foo 2))
            "event-equal doesn't return false for events with differing keys")
  (is-true (event-equal (list (event :foo 1)) (event :foo 1))
           "event-equal doesn't consider an event to be equal to a list of the same event"))

(test every-event-equal
  "Test every-event-equal"
  (is-true (every-event-equal
            (list (event :freq 440))
            (list (event :freq 440)))
           "every-event-equal doesn't return true for two lists of equivalent events")
  (is-false (every-event-equal
             (list (event :dur 1))
             (list))
            "every-event-equal doesn't return false for two lists of different length"))

(test events-differing-keys
  "Test `events-differing-keys'"
  (is-true (equal (list :bar)
                  (events-differing-keys (event :foo 1 :bar 2) (event :foo 1 :bar 4) (event :foo 1 :bar 5)))
           "events-differing-keys doesn't return keys whose values differ")
  (is-true (equal (list :foo)
                  (events-differing-keys (event :foo 1 :bar 5) (event :foo 1 :bar 5) (event :bar 5)))
           "events-differing-keys doesn't return keys that are missing from some events"))

(test events-lists-differing-keys
  (is (equal (list nil nil (list :bar))
             (events-lists-differing-keys (list (event :foo 1 :bar 2) (event :foo 3 :bar 4) (event :foo 3 :bar 5))
                                          (list (event :foo 1 :bar 2) (event :foo 3 :bar 4) (event :foo 3 :bar 6))))
      "events-lists-differing-keys returns incorrect results"))

(test combine-events
  "Test combine-events"
  (is-true (event-equal
            (event :foo 1 :bar 2 :baz 3)
            (combine-events (event :foo 1) (event :bar 2 :baz 3)))
           "combine-events doesn't work correctly on two events")
  (is-true (event-equal
            (event :freq 450 :qux 69 :baz 3)
            (combine-events (event :freq 450) (event :qux 69) (event :baz 3)))
           "combine-events doesn't work correctly on three events")
  (is-true (event-equal
            (event :freq 200)
            (combine-events (event :freq 200) (event)))
           "combine-events doesn't work correctly for empty second event")
  (is-true (event-equal
            (event :qux 69)
            (combine-events (event) (event :qux 69)))
           "combine-events doesn't work correctly for empty first event")
  (is-true (eop-p (combine-events eop (event :qux 69)))
           "combine-events doesn't work correctly for nil first event")
  (is-true (eop-p (combine-events (event :foo 1) eop))
           "combine-events doesn't work correctly for nil second event")
  (is (event-equal (event)
                   (copy-event (event)))
      "copy-event doesn't copy an empty event")
  (is (eql 2
           (let ((ev1 (event))
                 (ev2 (event))
                 (ev3 (event)))
             (setf (slot-value ev1 'cl-patterns::%beat) 1
                   (slot-value ev2 'cl-patterns::%beat) 2)
             (slot-value (combine-events ev1 ev2 ev3) 'cl-patterns::%beat)))
      "combine-events doesn't propagate the %beat slot")
  (is (eql 2
           (let ((ev1 (event))
                 (ev2 (event :beat 2))
                 (ev3 (event)))
             (setf (slot-value ev1 'cl-patterns::%beat) 1
                   (slot-value ev3 'cl-patterns::%beat) 3)
             (beat (combine-events ev1 ev2 ev3))))
      "combine-events doesn't prioritize the :beat key over the %beat slot"))

(test split-event-by-lists
  "Test split-event-by-lists"
  (is-true (every-event-equal
            (list (event :foo 1 :bar 1 :baz 3)
                  (event :foo 1 :bar 2 :baz 4)
                  (event :foo 1 :bar 1 :baz 5))
            (split-event-by-lists (event :foo 1 :bar (list 1 2) :baz (list 3 4 5))))
           "split-event-by-lists returns incorrect results")
  (is-true (every-event-equal
            (list (event :foo 1 :bar 1 :baz 3)
                  (event :foo 1 :bar 2 :baz 4)
                  (event :foo 1 :bar 1 :baz 5))
            (split-event-by-lists (event :foo (list 1) :bar (list 1 2) :baz (list 3 4 5))))
           "split-event-by-lists returns incorrect results if one of the event values is a list of length 1")
  (is-true (equal (list 999)
                  (let ((event (event)))
                    (setf (beat event) 999)
                    (mapcar #'beat (split-event-by-lists event))))
           "split-event-by-lists doesn't carry over the %beat slot for empty events")
  (is-true (equal (list 999 999 999)
                  (let ((event (event :midinote (list 40 50 60))))
                    (setf (beat event) 999)
                    (mapcar #'beat (split-event-by-lists event))))
           "split-event-by-lists doesn't carry over the %beat slot for events with lists"))

(test combine-events-via-lists
  "Test combine-events-via-lists"
  (is-true (event-equal
            (event :foo 1 :bar (list 2 3) :qux 4 :baz 5)
            (combine-events-via-lists (event :foo 1 :bar 2 :qux 4) (event :foo 1 :bar 3 :baz 5)))
           "combine-events-via-lists returns incorrect results"))

