(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; event (FIX: add more)

(test event
  "Test event functionality"
  (is (=
       1
       (event-value (event :dur 0 :sustain 1) :sustain))
      "event returns the correct sustain when sustain is provided and dur is 0")
  (is (=
       0.8
       (event-value (event) :sustain))
      "event returns the correct default value for sustain")
  (is (=
       0.5
       (event-value (event :dur 0 :legato 0.5) :legato))
      "event returns the correct legato when legato is provided and dur is 0")
  (is (=
       0.8
       (event-value (event) :legato))
      "event returns the correct default value for legato")
  (is (=
       1
       (event-value (event) :dur))
      "event returns the correct default value for dur")
  (is (eql
       :default
       (event-value (event) :instrument))
      "event returns the correct default value for instrument")
  (is (= (amp-db 0.125)
         (event-value (event :amp 0.125) :db))
      "event correctly converts amp to db")
  (is (= (db-amp -7)
         (event-value (event :db -7) :amp))
      "event correctly converts db to amp"))

(test event-equal
  "Test event-equal"
  (is-true
   (event-equal (event :dur 1) (event :dur 1))
   "event-equal returns true for equivalent events")
  (is-false
   (event-equal (event :dur 1) (event :dur 1 :foo 2))
   "event-equal returns false for events that don't have the same keys"))

(test every-event-equal
  "Test every-event-equal"
  (is-true (every-event-equal
            (list (event :freq 440))
            (list (event :freq 440)))
           "every-event-equal returns true for two lists of equivalent events")
  (is-false (every-event-equal
             (list (event :dur 1))
             (list))
            "every-event-equal returns false for two lists of different length"))

(test events-differing-keys
  "Test `events-differing-keys'"
  (is-true (equal (list :bar)
                  (events-differing-keys (event :foo 1 :bar 2) (event :foo 1 :bar 4) (event :foo 1 :bar 5)))
           "events-differing-keys returns keys whose values differ")
  (is-true (equal (list :foo)
                  (events-differing-keys (event :foo 1 :bar 5) (event :foo 1 :bar 5) (event :bar 5)))
           "events-differing-keys returns keys that are not present in all events"))

(test events-lists-differing-keys
  (is (equal (list nil nil (list :bar))
             (events-lists-differing-keys (list (event :foo 1 :bar 2) (event :foo 3 :bar 4) (event :foo 3 :bar 5))
                                          (list (event :foo 1 :bar 2) (event :foo 3 :bar 4) (event :foo 3 :bar 6))))
      "events-lists-differing-keys lists NIL for events that don't differ, and a list of differing keys for events that do"))

(test combine-events
  "Test combine-events"
  (is-true (event-equal
            (event :foo 1 :bar 2 :baz 3)
            (combine-events (event :foo 1) (event :bar 2 :baz 3)))
           "event-equal works correctly on two events")
  (is-true (event-equal
            (event :freq 450 :qux 69 :baz 3)
            (combine-events (event :freq 450) (event :qux 69) (event :baz 3)))
           "event-equal works correctly on three events"))

(test split-event-by-lists
  "Test split-event-by-lists"
  (is-true
   (every-event-equal
    (list (event :foo 1 :bar 1 :baz 3)
          (event :foo 1 :bar 2 :baz 4)
          (event :foo 1 :bar 1 :baz 5))
    (split-event-by-lists (event :foo 1 :bar (list 1 2) :baz (list 3 4 5))))
   "split-event-by-lists correctly splits an event into a list of three events"))

(test combine-events-via-lists
  "Test combine-events-via-lists"
  (is-true
   (event-equal
    (event :foo 1 :bar (list 2 3) :qux 4 :baz 5)
    (combine-events-via-lists (event :foo 1 :bar 2 :qux 4) (event :foo 1 :bar 3 :baz 5)))
   "combine-events-via-lists returns correct results"))

