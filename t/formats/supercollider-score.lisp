;;;; t/supercollider-score.lisp - tests for the SuperCollider score functionality.

(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

(test as-supercollider-score
  "Test the `as-supercollider-score' function"
  (let* ((score (cl-patterns::as-supercollider-score (pn (event :instrument :default :dur 1 :legato 1/2) 4)
                                                     :tempo 1))
         (score-length (length score)))
    (is (= 11 score-length)
        "as-supercollider-score didn't generate the right length (got ~D, expected 11)" score-length)
    (is (every #'=
               (list 0 0 0 0.5 1 1.5 2 2.5 3 3.5 4)
               (mapcar #'car score))
        "as-supercollider-score generates incorrect timestamps")
    (is (every #'equal
               (list
                (list "/g_new" 1 0 0)
                (list "/d_load" "default")
                (list "/s_new" "default" 1000 0 1 "gate" 1 "freq" 440 "sustain" 0.5 "tempo" 1 "amp" 0.5 "pan" 0 "out" 0)
                (list "/n_set" 1000 "gate" 0)
                (list "/s_new" "default" 1001 0 1 "gate" 1 "freq" 440 "sustain" 0.5 "tempo" 1 "amp" 0.5 "pan" 0 "out" 0)
                (list "/n_set" 1001 "gate" 0)
                (list "/s_new" "default" 1002 0 1 "gate" 1 "freq" 440 "sustain" 0.5 "tempo" 1 "amp" 0.5 "pan" 0 "out" 0)
                (list "/n_set" 1002 "gate" 0)
                (list "/s_new" "default" 1003 0 1 "gate" 1 "freq" 440 "sustain" 0.5 "tempo" 1 "amp" 0.5 "pan" 0 "out" 0)
                (list "/n_set" 1003 "gate" 0)
                (list "/c_set" 0 0))
               (mapcar #'second score))
        "as-supercollider-score generates incorrect OSC messages"))
  (let* ((score (cl-patterns::as-supercollider-score (pn (event :instrument :default :dur 1 :legato 1/2) 4)
                                                     :tempo 2
                                                     :max-length 2))
         (score-length (length score)))
    (is (= 7 score-length)
        "as-supercollider-score with :max-length 2 didn't generate the right length (got ~D, expected 7)" score-length)
    (is (every #'=
               (list 0 0 0 0.5 1 1.5 4)
               (mapcar #'car score))
        "as-supercollider-score generates incorrect timestamps")))
