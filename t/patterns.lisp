(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;; patterns

(test embedding
  "Test embedding patterns in patterns"
  (is (equal
       (list 0 1 2 3 4 5 eop)
       (next-n (pseq (list 0 (pseq (list 1 (pseq (list 2 3) 1) 4) 1) 5) 1) 7))
      "Stacked pseqs do not give correct results")
  (is-true (equal (list 0 0 0 1 1 1 2 2)
                  (mapcar (fn (event-value _ :inner))
                          (next-n
                           (pbind :foo 3
                                  :embed (pr (pbind :inner (pseries))
                                             (pk :foo)))
                           8)))
           "embedded patterns can not access event values from their parent pattern"))

(test number-key
  "Test the number key in patterns and pstreams"
  (is (equal (list 0 1 2 3 4 5 6 7)
             (let ((pstr (as-pstream (pbind :foo 1))))
               (loop :for i :upto 7
                     :collect (slot-value pstr 'cl-patterns::number)
                     :do (next pstr))))
      "number slot in pstreams is wrong")
  (is (equal (list 0 1 2 3 4 5 6 7)
             (let ((pstr (as-pstream (pbind :foo (pk :number)))))
               (mapcar (lambda (e) (event-value e :foo)) (next-upto-n pstr 8))))
      "pstream's number is not accessible with pk"))


(test next
  "Test next and next-upto-n"
  (is-true (equal (list (list 0 1) (list 2 3) (list 4) nil)
                  (let ((pstr (as-pstream (pseries 0 1 5))))
                    (list (next-upto-n pstr 2)
                          (next-upto-n pstr 2)
                          (next-upto-n pstr 2)
                          (next-upto-n pstr 2))))
           "next-upto-n gives wrong results"))

(test pbind
  "Test pbind functionality"
  (is (equal (list :foo :bar :baz)
             (keys (next (pbind :foo 1 :bar 2 :baz (pseq (list 1 2 3) 1)))))
      "pbind's output events don't have the expected keys")
  (is (= 3
         (length (next-upto-n (pbind :foo 1 :bar 2 :baz (pseq (list 1 2 3) 1)))))
      "pbind doesn't yield the correct number of events")
  (let* ((num (random 200))
         (*max-pattern-yield-length* num))
    (is (= num
           (length (next-upto-n (pbind :foo 1 :bar 2))))
        "pbind doesn't yield the correct number of events by default for a *max-pattern-yield-length* of ~a"
        num))
  (is (every-event-equal (list (event) (event) (event))
                         (next-n (pbind) 3))
      "empty pbinds don't yield empty events"))

(test pb
  "Test pb functionality"
  (is-true (every-event-equal (list (event :bar 1)
                                    (event :bar 1/2)
                                    (event :bar 1/3)
                                    (event :bar 1/4))
                              (next-n (pb :foo :bar (/ 1 (pseries 1 1 4))) 4))
           "pb doesn't correctly translate /")
  (is-true (every-event-equal (list (event :bar 0)
                                    (event :bar 2))
                              (next-n (pb :foo :bar (pseq (list 0 (truncate (/ 5 2))) 1)) 2))
           "pb doesn't correctly avoid translating functions inside non-translatable functions"))

(test parent ;; FIX: make sure all patterns are given parents
  "Test whether patterns have the correct parent information"
  (is-true (let ((pb (pbind :foo (pseq (list 1 2 3)))))
             (eql (pattern-parent (getf (slot-value pb 'cl-patterns::pairs) :foo))
                  pb))
           "pbind subpatterns don't have correct parents for pseq")
  (is-true (let ((pb (pbind :foo (pfunc (lambda () (random 5))))))
             (eql (pattern-parent (getf (slot-value pb 'cl-patterns::pairs) :foo))
                  pb))
           "pbind subpatterns don't have correct parents for pfunc")
  (is-true (let* ((child (pn 1 4))
                  (parent (pbind :foo child)))
             (eq parent (pattern-parent child :class 'pbind)))
           "pattern-parent with :class 'pbind does not give correct results"))

(test children
  "Test whether children of a pattern can be accessed"
  (is-true (let* ((child (pn 1 4))
                  (parent (pbind :foo child)))
             (eq child (car (pattern-children parent))))
           "pattern-children does not find the sole child of a pbind")
  (is-true (let* ((child (pn 1 4))
                  (parent (pbind :foo child :bar 4)))
             (set-equal (list child 4)
                        (pattern-children parent :class t)))
           "pattern-children with :class t does not find all children of a pbind")
  (is-true (let* ((top (pseq (list 99 98 97) 1))
                  (child (pseq (list 1 2 top) 4))
                  (parent (pbind :foo child :bar 4)))
             (set-equal (list 1 2 top 4 0)
                        (pattern-children parent :num 2 :class t)))
           "pattern-children with :num 2 and :class t does not find all children of a pbind")
  (is-true (let* ((top (pseq (list 99 98 97) 1))
                  (child (pseq (list 1 2 top) 4))
                  (parent (pbind :foo child :bar 4)))
             (set-equal (list child 4 1 2 top 4 0 99 98 97 1 0)
                        (pattern-children parent :num 3 :accumulate t :class t)))
           "pattern-children with :num 3, :accumulate t, and :class t does not find all children of a pbind"))

(test beat-key
  "Test that the :beat key is correct"
  (is-true (equal (list 0 1/2 1 3/2)
                  (mapcar (fn (event-value _ :x))
                          (next-n (pbind :dur 1/2
                                         :x (pf (beat *event*)))
                                  4)))
           "*event*'s beat key is not correct in basic patterns")
  (is-true (every-event-equal
            (list (event :beat 0)
                  (event :beat 3))
            (next-upto-n (pseq (list (event :beat 0) (event :beat 3)) 1)))
           ":beat key is overwritten when events are yielded from pstreams")
  (is-true (every-event-equal
            (list
             (event :beat 3 :x 0)
             (event :beat 5 :x 1))
            (next-upto-n (pbind :beat 2
                                :embed (pbind :beat (p+ (pk :beat) (pseq (list 1 3) 1))
                                              :x (pseries)))))
           ":beat key is not accessible in embedded patterns")
  (is-true (equal (list 0 1/2 1 3/2 2 5/2 3 7/2)
                  (mapcar #'beat (next-upto-n (pseq (list (pbind :dur (pn 1/2 4))
                                                          (pbind :dur (pn 1/2 4)))
                                                    1))))
           ":beat key is not correct for sequential subpatterns")
  (is-true (let ((cl-patterns::*pdef-dictionary* (make-hash-table)))
             (pb :c1 :dur (pn 1 4))
             (pb :c2 :dur (pn 1 4))
             (equal (list 0 1 2 3 4 5 6 7)
                    (mapcar #'beat (next-upto-n (pseq (list (pdef :c1) (pdef :c2)) 1)))))
           ":beat key is not correct for sequential pdef subpatterns")
  (is-true (equal (mapcar #'beat (next-upto-n (pbind :dur (pn 1 1) :pdurstutter 3)))
                  (list 0 1/3 2/3))
           ":beat key of events from subpatterns is altered by the containing pattern next :around method"))

(test pstream-p
  "Test the `pstream-p' function"
  (is (pstream-p (as-pstream 2))
      "pstream-p returns incorrect results for pstreams")
  (is-false (pstream-p (pseq (list 1)))
            "pstream-p returns incorrect results for patterns"))

(test t-pstream
  "Test `t-pstream' functionality"
  (is (equal (list 3 3 3 eop eop)
             (next-n (t-pstream 3 3) 5))
      "t-pstream yields incorrect results"))

(test pstream-count
  "Test `pstream-count' functionality"
  (is-true (= 0 (pstream-count (as-pstream (pbind :foo 1))))
           "pstream-count doesn't return 0 for patterns not yet made into pstreams")
  (is-true (= 1 (let ((pb (pbind :foo 1)))
                  (as-pstream pb)
                  (pstream-count pb)))
           "pstream-count doesn't return 1 for a pattern that one pstream has been made of")
  (is-true (equal (iota 20)
                  (let* ((pb (pbind :foo 1))
                         (lst (loop :repeat 20 :collect (as-pstream pb))))
                    (mapcar #'pstream-count lst)))
           "pstream-count does not return the correct value for pstreams"))

(test post-pattern-output-processors
  "Test `*post-pattern-output-processors*' functionality"
  (defun increase-foo (event pstream)
    "Helper function for the post-pattern-output-processors test."
    (declare (ignore pstream))
    (when (nth-value 1 (event-value event :foo))
      (incf (event-value event :foo)))
    event)
  (let ((*post-pattern-output-processors* (list 'increase-foo)))
    (is-true (equal (list 1 2 3)
                    (mapcar (rcurry #'event-value :foo)
                            (next-n (pbind :foo (pseries)) 3)))
             "post-pattern-output-processors aren't working")
    (is-true (equal (list 1 2 3)
                    (mapcar (rcurry #'event-value :foo)
                            (next-n (pchain (pbind :foo (pseries))
                                            (pbind :bar (pseries)))
                                    3)))
             "post-pattern-output-processors are being applied incorrectly for nested patterns")))

(test instrument-mapping
  "Test `instrument-mapping' functionality"
  (let ((cl-patterns::*instrument-map* (make-hash-table :test #'equal)))
    (setf (instrument-mapping :foo) (list :bar 1 :baz 2)
          (instrument-mapping :bar) :qux)
    (is-true (every-event-equal
              (list (event :instrument :foo :bar 1 :baz 2)
                    (event :instrument :foo :test 99 :bar 1 :baz 2)
                    (event :instrument :qux)
                    (event :instrument :qux :test 99))
              (next-n (pseq (list (event :instrument :foo)
                                  (event :instrument :foo :test 99)
                                  (event :instrument :bar)
                                  (event :instrument :bar :test 99)))
                      4))
             "instrument-mapping does not map correctly")))

(test special-keys
  "Test pbind special keys"
  (is-true (every-event-equal
            (list (event :bar 1 :qux 69 :dur 1/3)
                  (event :bar 1 :qux 69 :dur 1/3)
                  (event :bar 1 :qux 69 :dur 1/3)
                  (event :bar 2 :qux 420 :dur 1/2)
                  (event :bar 2 :qux 420 :dur 1/2)
                  (event :bar 2 :qux 666 :dur 1)
                  (event :bar 3 :qux 69 :dur 1/3)
                  (event :bar 3 :qux 69 :dur 1/3)
                  (event :bar 3 :qux 69 :dur 1/3)
                  (event :bar 3 :qux 420 :dur 1/2)
                  (event :bar 3 :qux 420 :dur 1/2)
                  (event :bar 3 :qux 666 :dur 1))
            (next-upto-n (pbind :bar (pseq (list 1 2 3) 1)
                                :pr (pseq (list 1 2 3))
                                :qux (pseq (list 69 420 666))
                                :pdurstutter (pseq (list 3 2 1)))))
           "pbind's pr and/or pdurstutter special keys do not work correctly"))

(test special-wrap-keys ;; FIX: should work for all filter patterns
  "Test behavior of wrap keys"
  (is (every-event-equal
       (list (event :foo 0 :x 1) (event :foo 1 :x 2) (event :foo 1 :x 2) (event :foo 2 :x 3) (event :foo 2 :x 3) (event :foo 2 :x 3))
       (next-n (pr (pbind :foo (pseries) :x (pseq (list 1 2 3))) (pk :x)) 6))
      "pk in pr's repeats parameter can't access keys from pbind in pr's pattern parameter"))

(test t-pstream
  "Test functionality of non-patterns as pstreams"
  (is (= 1
         (length (next-upto-n (as-pstream 69))))
      "pstreams made from numbers are yielding the wrong number of outputs")
  (is (= 1
         (length (next-upto-n (as-pstream (lambda () (random 420))))))
      "pstreams made from functions are yielding the wrong number of outputs")
  (is (= 3
         (length (next-upto-n (pseq (list 1 2 3) 1))))
      "patterns don't yield the correct number of values when their parameters are values coerced to pstreams")
  (is (= 5
         (let ((*max-pattern-yield-length* 5))
           (length (next-upto-n (pfunc (lambda () (random 64)))))))
      "pfunc yields the wrong number of outputs when a function used as its input"))

(test remaining-p ;; FIX: test this for all patterns that use it.
  "Test the behavior of the `remaining-p' function"
  (is (equal (list 1 2 3)
             (next-upto-n (pseq (list 1 2 3) 1)))
      "pseq yields the wrong number of outputs")
  (is (= 64
         (let ((*max-pattern-yield-length* 64))
           (length (next-upto-n (pseq (list 1 2 3) :inf)))))
      "pseq yields the wrong number of results when `next-upto-n' is called with its REPEATS as :inf")
  (is (= 64
         (let ((*max-pattern-yield-length* 64))
           (length (next-upto-n (pseq (list 1 2 3) (pseq (list 1) :inf))))))
      "pseq yields the wrong number of results when its REPEATS is a pattern")
  (is (= 3
         (let ((*max-pattern-yield-length* 64))
           (length (next-upto-n (pseq (list 1 2 3) (pseq (list 1 0) 1))))))
      "pseq yields the wrong number of results when its REPEATS is a pattern"))

(test pstream-elt
  "Test the behavior of the `pstream-elt' function"
  (is (eop-p
       (let ((pstr (as-pstream (pseq '(1 2 3) 1))))
         (next-upto-n pstr)
         (pstream-elt pstr -1)))
      "pstream-elt -1 does not return eop for ended pstreams")
  (is (= 99
         (let ((pstr (as-pstream (pseq '(1 2 99) 1))))
           (next-n pstr 3)
           (pstream-elt pstr -1)))
      "pstream-elt -1 does not return the last item in the pstream")
  (is (= 1
         (let ((pstr (as-pstream (pseq '(1 2 99) 1))))
           (next-n pstr 3)
           (pstream-elt pstr 0)))
      "pstream-elt 0 does not return the first item in the pstream")
  (signals cl-patterns::pstream-out-of-range
    (pstream-elt (as-pstream (pseq '(1 2 99) 1)) 2)
    "pstream-elt doesn't signal a pstream-out-of-range error when called for an index not yet generated")
  (let* ((*max-pattern-yield-length* 4)
         (pstr (as-pstream (pseq (list 0 1 2 3)))))
    (dotimes (n 8)
      (signals cl-patterns::pstream-out-of-range
        (pstream-elt pstr n)
        "pstream-elt doesn't signal a pstream-out-of-range error when called for an index not yet generated")
      (next pstr)
      (is-true (pstream-elt pstr n)
               "pstream-elt doesn't return the value for a positive index")
      (is-true (pstream-elt pstr -1)
               "pstream-elt doesn't return the last output for an index of -1"))
    (signals cl-patterns::pstream-out-of-range
      (pstream-elt pstr -5)
      "pstream-elt doesn't signal a pstream-out-of-range error when called for a negative index older than the oldest")
    (is-true (pstream-elt pstr -4)
             "pstream-elt doesn't return the oldest output for the oldest negative index")))

(test pstream-elt-future
  "Test the behavior of the `pstream-elt-future' function"
  (let ((pstr (as-pstream (pseq (list 1 2 99) 1))))
    (is (= 1 (pstream-elt-future pstr 0))
        "pstream-elt-future doesn't return the first output")
    (next pstr)
    (is (= 1
           (pstream-elt-future pstr 0))
        "pstream-elt-future 0 doesn't return the first output if it has been generated")
    (is (= 2
           (pstream-elt-future pstr 1))
        "pstream-elt-future 1 doesn't return the next output from the pstream")
    (is (= 99
           (pstream-elt-future pstr 2))
        "pstream-elt-future 2 doesn't return the third output")
    (is (equal (list 1 1)
               (let ((pstr (as-pstream (pseq (list 1 2 99) 1))))
                 (list (pstream-elt-future pstr 0)
                       (next pstr))))
        "getting future outputs with pstream-elt-future affects outputs from `next'")
    (is-false (position nil (let ((pstr (as-pstream (pwhite 0 127))))
                              (loop :repeat 200 :collect (= (peek pstr) (next pstr)))))
              "peek and next do not return the same values")
    (is (= 0
           (let ((pstr (as-pstream (pbind :dur 1/3 :foo (pseq (list 1 2 3) 1)))))
             (peek pstr)
             (beat pstr)))
        "beat method is counting peeked outputs")
    (is (equal (list 1 2 eop)
               (peek-n (pseq (list 1 2) 1) 3))
        "peek-n does not return correct results")
    (is (equal (list 1 2)
               (peek-upto-n (pseq (list 1 2) 1) 3))
        "peek-upto-n does not return correct results")
    (let ((pstr (as-pstream (pbind :dur (pn 1 4)))))
      (is (equal (list 0 1 2 3)
                 (loop :for n :below 4
                       :collect (slot-value pstr 'cl-patterns::future-beat)
                       :do (pstream-elt-future pstr n)))
          "future-beat is not correct for peeked pstreams"))
    (let ((pstr (as-pstream (pbind :dur (pn 1 4)))))
      (is (equal (list 0 1 0 1 2 3 4)
                 (mapcar #'beat (append (peek-n pstr 2)
                                        (next-n pstr 4)
                                        (list pstr))))
          "event and pstream beats are broken by peeking"))))

(test last-output
  "Test `last-output' function"
  (let ((pstr (as-pstream (pbind :x (pseq (list 1 2) 1)))))
    (is-false (cl-patterns::last-output pstr)
              "last-output doesn't return nil on pstreams not yet started")
    (next pstr)
    (is (event-equal (event :x 1)
                     (cl-patterns::last-output pstr))
        "last-output doesn't return the first output of a pstream")
    (next pstr)
    (is (event-equal (event :x 2)
                     (cl-patterns::last-output pstr))
        "last-output doesn't return the second output of a pstream")
    (next pstr)
    (is (event-equal (event :x 2)
                     (cl-patterns::last-output pstr))
        "last-output doesn't return the second output of a pstream")))

(test prest
  "Test `prest'"
  (is-true (rest-p (prest))
           "`rest-p' is not true for prest")
  (is (equal (list nil t nil)
             (mapcar #'rest-p (next-upto-n
                               (pbind :dur (pseq (list 1 (prest 2) 1) 1)
                                      :dur (p+ (pk :dur) 1)))))
      "prest does not work properly as a :dur value")
  (is (equal (list 2 3 2)
             (mapcar #'dur (next-upto-n
                            (pbind :dur (pseq (list 1 (prest 2) 1) 1)
                                   :dur (p+ (pk :dur) 1)))))
      "prest's value is not used as the value of the key it is used in")
  (is (equal (list nil t nil)
             (mapcar #'rest-p (split-event-by-lists
                               (next (pbind :freq (list 1 (prest 2) 1)
                                            :freq (p+ (pk :freq) 1))))))
      "prest does not work properly with multichannel expansion"))

(test pseq
  "Test pseq"
  (is (null
       (next-upto-n (pseq (list 1 2 3) 0)))
      "pseq yields the wrong number of outputs when REPEATS is 0")
  (is (equal
       (list 1 2 3 1 2 3 eop eop)
       (next-n (pseq (list 1 2 3) 2) 8))
      "pseq yields wrong results when REPEATS is provided")
  (is (equal
       (list 1 2 3 1 2 3 eop)
       (next-n (pseq (lambda () (list 1 2 3)) 2) 7))
      "pseq yields incorrect results when LIST is a function")
  (is (string= "" ;; FIX: this should be done for other patterns as well.
               (let* ((s (make-string-output-stream))
                      (*standard-output* s))
                 (as-pstream (pseq (list 1 2 3) (lambda () (print 3))))
                 (get-output-stream-string s)))
      "pseq's REPEATS argument is evaluated before `next' is called")
  (is (equalp (vector 1 2 3 1 2 3 1 2 3 1 2 3 eop) ;; FIX: do this for other patterns as well.
              (let* ((foo 1)
                     (bar (as-pstream (pseq (list 1 2 3) (pfunc (lambda () foo))))))
                (next-n bar 10) ;=> (1 2 3 1 2 3 1 2 3 1)
                (setf foo 0)
                (next-n bar 3) ;=> (2 3 NIL)
                (subseq (slot-value bar 'cl-patterns::history) 0 13)))
      "pseq returns incorrect results when its REPEATS is used as a gate")
  (is (equal
       (list 6 7 5 6 7 5)
       (next-upto-n (pseq (list 5 6 7) 2 1)))
      "pseq's OFFSET argument doesn't work with an integer")
  (is (equal
       (list 6 5 6)
       (next-upto-n (pseq (list 5 6 7) 2 (pseq (list 1 2 -1) 1))))
      "pseq's OFFSET argument doesn't work with a pattern"))

(test pser
  "Test pser"
  (is (equal
       (list 1 2 3 eop eop eop)
       (next-n (pser (list 1 2 3) 3) 6))
      "pser yields the wrong number of outputs when its LENGTH is specified")
  (is (equal
       (list 1 2 3 1 2 1 1 2 3 1 2 1)
       (next-upto-n (pser (list 1 2 3) (pseq (list 3 2 1 3 2 1 0) 1))))
      "pser yields incorrect results when its LENGTH is a pattern")
  (is (equal
       (list 1 1 0 0 2 2)
       (next-upto-n (pser (list 0 1 2) :inf (pseq (list 1 0))) 6))
      "pser's OFFSET argument doesn't work correctly with a pattern")
  (is (equal
       (list 1 1 0 0 2 2)
       (next-upto-n (pser (list 0 1 2) :inf (pseq (list 1 0) 3))))
      "pser's OFFSET argument doesn't work correctly with a finite pattern"))

(test pk
  "Test pk"
  (is (equal
       (list 3)
       (mapcar (fn (event-value _ :bar))
               (next-n (pbind :foo (pseq (list 3) 1) :bar (pk :foo)) 1)))
      "pk yields incorrect results")
  (is (equal
       (list 1 2 3 nil)
       (mapcar (fn (event-value _ :bar))
               (next-n (pbind :foo (pseq (list 1 2 3) 1) :bar (pk :foo)) 4)))
      "pk yields incorrect results")
  (is (equal
       (list 2 2 2 nil)
       (mapcar (fn (event-value _ :bar))
               (next-n (pbind :foo (pseq (list 1 2 3) 1) :bar (pk :baz 2)) 4)))
      "pk yields incorrect results when a default is provided and its KEY is not in the source")
  (is (=
       3
       (let ((*event* (event :foo 3)))
         (event-value (next (pbind :bar (pk :foo))) :bar)))
      "*event* is not propagated to pbinds when it is bound, resulting in pk yielding incorrect results"))

(test prand
  "Test prand"
  (is (not (member nil (mapcar (lambda (x) (member x (list 1 2 3))) (next-upto-n (prand (list 1 2 3) :inf)))))
      "prand is yielding outputs not specified in its inputs")
  (is (= 3
         (length (next-upto-n (prand (list 1 2 3) 3))))
      "prand yields the correct number of outputs")
  (is-false
   (find-if-not (fn (member _ (list 1 2)))
                (next-upto-n (prand (pf (list 1 2)))))
   "prand returned incorrect results for LIST as a pattern"))

(test pxrand
  "Test pxrand"
  (is-true
   (block pxrand-test-1
     (let ((prev))
       (dolist (cur (next-n (pxrand (list 1 2)) 1000))
         (when (eql cur prev)
           (return-from pxrand-test-1 nil))
         (setf prev cur))
       t))
   "pxrand yielded the same item twice in a row")
  (signals simple-error (pxrand (list 1 1 1))
    "pxrand does not raise an error for insufficient differing elements in its input list")
  (is-false
   (find-if-not (fn (member _ (list 1 2)))
                (next-upto-n (pxrand (pf (list 1 2)))))
   "pxrand returned incorrect results for LIST as a pattern"))

(test pwrand
  "Test pwrand"
  (is-false
   (position 0 (next-n (pwrand (list 0 1) (list 0 1)) 1000))
   "pwrand yielded an item whose weight was 0")
  (is-false
   (find-if-not (fn (member _ (list 1 2)))
                (next-upto-n (pwrand (pf (list 1 2)))))
   "pwrand returned incorrect results for LIST as a pattern")
  (is-false
   (find 1 (mapcar #'freq
                   (next-upto-n (pbind :foo 0
                                       :freq (pwrand (list 1 0)
                                                     (list (pk :foo) 1))))))
   "pwrand returned incorrect results for WEIGHTS with a pattern"))

(test pwxrand
  "Test pwxrand"
  (is-true
   (block pwxrand-test-1
     (let ((prev))
       (dolist (cur (next-n (pwxrand (list 1 2)) 1000))
         (when (eql cur prev)
           (return-from pwxrand-test-1 nil))
         (setf prev cur))
       t))
   "pwxrand yielded the same item twice in a row")
  (is-false
   (position 0 (next-n (pwxrand (list 0 1 2) (list 0 1 1)) 1000))
   "pwxrand yielded an item whose weight was 0")
  (signals simple-error (pwxrand (list 1 1 1))
    "pwxrand does not raise an error for insufficient differing elements in its input list")
  (is-false
   (find-if-not (fn (member _ (list 1 2)))
                (next-upto-n (pwxrand (pf (list 1 2)))))
   "pwxrand returned incorrect results for LIST as a pattern")
  (is-false
   (find 1 (mapcar #'freq
                   (next-upto-n (pbind :foo 0
                                       :freq (pwxrand (list 1 0 -1)
                                                      (list (pk :foo) 1 1))))))
   "pwxrand returned incorrect results for WEIGHTS with a pattern"))

(test pfunc
  "Test pfunc"
  (is (= 9
         (length (next-upto-n (pfunc (lambda () (random 9))) 9)))
      "pfunc yields the wrong number of outputs")
  (is (= 4
         (next (pfunc (lambda () (+ 2 2)))))
      "pfunc yields incorrect results")
  (is (length= 4 (next-upto-n (pfunc (lambda () (random 10)) 4)))
      "pfunc yields the wrong number of results when LENGTH is provided."))

(test pr
  "Test pr"
  (is (equal (list 1 1 2 2 3 3 eop)
             (next-n (pr (pseq (list 1 2 3) 1) 2) 7))
      "pr yields incorrect outputs when its REPEATS is a number")
  (is (equal (list 1 1 2 2 2 3 3 eop)
             (next-n (pr (pseq (list 1 2 3) 1) (lambda (e) (if (= e 2) 3 2))) 8))
      "pr yields incorrect outputs when its REPEATS is a function")
  (is (equal (list 1 1 2 2 3 3 eop eop)
             (next-n (pr (pseq (list 1 2 3) 1) (lambda () 2)) 8))
      "pr yields incorrect outputs when its REPEATS is a function that doesn't accept arguments")
  (is (equal (list 3 3 3 3 3 3 3 3 3 3)
             (next-n (pr 3) 10))
      "pr yields incorrect outputs when its REPEATS is :inf")
  (is (equal (list 1 1 2 eop)
             (next-n (pr (pseq (list 1 2 3) 1) (pseq (list 2 1 0) 1)) 4))
      "pr does not skip elements when REPEATS is 0")
  (is (equal ;; FIX: make sure this works for all filter patterns (parp, etc)
       (list 1 2 2 3 3 3 1 2 2 3 3 3)
       (mapcar (fn (event-value _ :x))
               (next-upto-n
                (pr (pbind :x (pseq (list 1 2 3)))
                    (pk :x))
                12)))
      "pr's REPEATS parameter doesn't have access to the event generated by its source pattern"))

(test pdef
  (let ((cl-patterns::*pdef-dictionary* (make-hash-table)))
    (is (equal (list)
               (all-pdefs))
        "all-pdefs doesn't return an empty list when no pdefs are defined")
    (let* ((pat (pbind :quant 6/9))
           (pdef (pdef :x pat)))
      (is (equal (quant pat)
                 (quant pdef))
          "pdef's quant doesn't defer to its source pattern's quant when unspecified"))
    (let* ((pat (pbind :quant (list 3)))
           (pdef (pdef :x2 pat)))
      (is (progn
            (setf (quant pdef) (list 7))
            (and (equal (list 3)
                        (quant pat))
                 (equal (list 7)
                        (quant pdef))))
          "setting the quant of a pdef doesn't shadow the value of its source pattern"))
    (let ((length (length (all-pdefs))))
      (is (= 2
             length)
          "all-pdefs didn't return the correct number of elements in its result (all-pdefs result length: ~s; pdef-dictionary: ~s)"
          length
          cl-patterns::*pdef-dictionary*))))

(test plazy
  "Test plazy"
  (is (equal (list 1 2 3 1 2 3 1)
             (next-n (plazy (lambda () (pseq (list 1 2 3)))) 7))
      "plazy yields incorrect outputs")
  (is-false (next-upto-n (plazy (lambda () eop)))
            "plazy yields incorrect outputs when its function returns eop")
  (is-false (next-upto-n (plazy (lambda () (pseq (list 1 2 3))) 0))
            "plazy yields 0 outputs when REPEATS is 0")
  (is (length= 14
               (next-upto-n (plazy (lambda () (random 20)) 14)))
      "plazy yields the wrong number of outputs when REPEATS is provided"))

(test protate
  "Test protate"
  (is (equal (list 3 1 2)
             (next-upto-n (protate (pseq (list 1 2 3) 1) 1)))
      "protate yields incorrect outputs")
  (is (equal (list 4 5 1 2 3)
             (next-upto-n (protate (pseq (list 1 2 3 4 5) 1) 2)))
      "protate yields incorrect outputs")
  (is (equal (list 2 3 1)
             (next-upto-n (protate (pseq (list 1 2 3) 1) -1)))
      "protate yields incorrect outputs for negative shifts"))

(test pn
  "Test pn"
  (is (equal
       (list 1 eop eop)
       (next-n (pn 1 1) 3))
      "pn yields incorrect outputs when its source pattern is a value")
  (is (equal
       (list 3 3 3 eop)
       (next-n (pn 3 3) 4))
      "pn yields incorrect outputs when its source pattern is a value")
  (is (equal
       (list 1 2 3 1 2 3 1 2 3 eop eop eop)
       (next-n (pn (pseq (list 1 2 3) 1) 3) 12))
      "pn yields incorrect outputs when its source pattern is a pattern")
  (is (eop-p (next (pn (pseq (list 1 2 3) 0) 1)))
      "pn does not yield eop when its source pattern yields no outputs"))

(test pshuf
  "Test pshuf"
  (is (= 5
         (length (next-upto-n (pshuf (list 1 2 3 4 5) 1) 32)))
      "pshuf yields the wrong number of outputs when REPEATS is specified")
  (is (= 10
         (length (next-upto-n (pshuf (list 1 2 3 4 5) 2) 32)))
      "pshuf yields the wrong number of outputs when REPEATS is specified")
  (is (equal
       (list 1 2 3 4 5)
       (next-upto-n (pseq (list 1 2 3 4 5) 1))) ;; this list must be quoted and must be the same as one of the ones used in the pshuf test above.
      "pshuf destructively modifies its input list"))

(test pwhite
  "Test pwhite"
  (is (every #'integerp (next-upto-n (pwhite 0 1 :inf)))
      "pwhite yields outputs other than integers when its LO and HI are integers")
  (is (every #'floatp (next-upto-n (pwhite 0.0 1 :inf)))
      "pwhite yields outputs other than floats when its LO is a float")
  (is (every #'floatp (next-upto-n (pwhite 0 1.0 :inf)))
      "pwhite yields outputs other than floats when its HI is a float")
  (is (every (fn (and (>= _ -10)
                      (<= _ -1)))
             (next-upto-n (pwhite -10 -1 :inf)))
      "pwhite yields incorrect outputs")
  (let* ((len (random *max-pattern-yield-length*))
         (res (length (next-upto-n (pwhite 0 1 len)))))
    (is (= len res)
        "pwhite yields the wrong number of outputs (expected ~s, got ~s)" len res)))

(test pbrown
  "Test pbrown"
  (is (every #'integerp (next-upto-n (pbrown 0 1)))
      "pbrown yields outputs other than integers when its LO and HI are integers")
  (is (every #'floatp (next-upto-n (pbrown 0.0 1.0)))
      "pbrown yields outputs other than floats when its LO and HI are floats")
  (is (every (fn (and (>= _ -10)
                      (<= _ -1)))
             (next-upto-n (pbrown -10 -1)))
      "pbrown yields incorrect outputs")
  (is (every (fn (<= _ 0.125))
             (loop :for (one two) :on (next-upto-n (pbrown 0.0 1.0 0.125))
                   :if (and one two)
                     :collect (abs (- one two))))
      "pbrown's successive outputs are more than STEP away from each other")
  (let* ((len (random *max-pattern-yield-length*))
         (res (length (next-upto-n (pbrown 0 1 0.125 len)))))
    (is (= len res)
        "pbrown yields the wrong number of outputs (expected ~s, got ~s)" len res)))

(test pexprand
  "Test pexprand"
  ;; FIX
  )

(test pgauss
  "Test pgauss"
  (let* ((len (random *max-pattern-yield-length*))
         (res (length (next-upto-n (pgauss 8 8 len)))))
    (is (= len res)
        "pgauss yields the wrong number of outputs (expected ~s, got ~s)" len res)))

(test pseries
  "Test pseries"
  (is (equal (iota 64)
             (next-n (pseries 0 1 :inf) 64))
      "pseries yields incorrect outputs")
  (is (equal (list 0 1 1 0 -1 -2)
             (next-upto-n (pseries 0 (pseq (list 1 0 -1 -1 -1) 1) :inf)))
      "pseries yields incorrect outputs when its STEP is a pattern")
  (is (equal (list 4)
             (mapcar (fn (event-value _ :y))
                     (next-upto-n (pbind :f 4 :y (pseries (pk :f) 0 1)))))
      "pseries START can't access *event*"))

(test pseries*
  "Test pseries*"
  (for-all ((num (gen-integer :min 2 :max 128)))
    (is (length= num
                 (next-upto-n (pseries* 1 2 num)))
        "pseries* yields the wrong number of outputs"))
  (for-all ((num (gen-integer :min 0)))
    (is (= num (next (pseries* num 2 4)))
        "pseries* did not yield START as the first output (when provided with ~a as START)"
        num))
  (for-all ((end (gen-integer :min -40 :max 40))
            (len (gen-integer :min 2 :max 40)))
    (is (= end
           (lastcar (next-upto-n (pseries* 50 end len))))
        "pseries*'s last output is incorrect when END is ~a and LENGTH is ~a"
        end len)))

(test pgeom
  "Test pgeom"
  (is (equal (list 1 2 4 8 16 32 64 128)
             (next-n (pgeom 1 2 :inf) 8))
      "pgeom yields incorrect outputs")
  (is (equal (list 1 1 2 6 3.0 2.1)
             (next-upto-n (pgeom 1 (pseq (list 1 2 3 0.5 0.7) 1) :inf)))
      "pgeom yields incorrect outputs when its GROW is a pattern")
  (is (equal (list 4)
             (mapcar (fn (event-value _ :y))
                     (next-upto-n (pbind :f 4 :y (pgeom (pk :f) 1 1)))))
      "pgeom's START can't access *EVENT*"))

(test pgeom*
  "Test pgeom*"
  (for-all ((len (gen-integer :min 2 :max 128)))
    (is (length= len
                 (next-upto-n (pgeom* 1 2 len)))
        "pgeom* yields the wrong number of outputs when LENGTH is ~a"
        len))
  (for-all ((start (gen-integer)))
    (is (= start (next (pgeom* start 2 4)))
        "pgeom* did not yield START as the first output (when ~a provided as START)"
        start))
  (for-all ((end (gen-integer :min -40 :max 40))
            (len (gen-integer :min 2 :max 20)))
    (let ((res (lastcar (next-upto-n (pgeom* 50 end len)))))
      (is (> 1
             (abs (- end res)))
          "pgeom*'s last output is nowhere near END (it is ~a when END is ~a and LENGTH is ~a)"
          res end len))))

(test ptrace
  "Test ptrace"
  (let ((*standard-output* (make-string-output-stream)))
    (is (every-event-equal
         (list (event :foo 0) (event :foo 1) (event :foo 2))
         (next-upto-n (ptrace (pbind :foo (pseries))) 3))
        "ptrace doesn't correctly pass TRACE's output events"))
  (let ((s (make-string-output-stream)))
    (next-upto-n (ptrace (pseq (list 1 2 3) 1) "foo" s))
    (is (string= (format nil "foo 1~%foo 2~%foo 3~%foo ~s~%" eop)
                 (get-output-stream-string s))
        "ptrace doesn't print the correct trace output to its STREAM"))
  (let ((s (make-string-output-stream)))
    (next-upto-n (pbind :bar (pseries) :baz (pseries 4) :- (ptrace (list :bar :baz) "foo" s)) 4)
    (is (string= (format nil "foo :BAR: 0 :BAZ: 4~%foo :BAR: 1 :BAZ: 5~%foo :BAR: 2 :BAZ: 6~%foo :BAR: 3 :BAZ: 7~%")
                 (get-output-stream-string s))
        "ptrace doesn't print the correct trace output to its STREAM when tracing keys in *event*")))

(test place
  "Test place"
  (is (equal (list 1 2 3 1 2 4 1 2 5)
             (next-upto-n (place (list 1 2 (list 3 4 5)) 3)))
      "place yields incorrect outputs")
  (is (equal (list 1 2 3 1 2 4 1 2 5 1 2 3 1 2 4 1 2 5)
             (next-upto-n (place (list 1 2 (list 3 4 5)) :inf) 18))
      "place yields incorrect outputs"))

(test ppatlace
  "Test ppatlace"
  (is (equal (list 1 4 2 5 3 6 7 8 eop)
             (next-n (ppatlace (list (pseq (list 1 2 3) 1) (pseq (list 4 5 6 7 8) 1)) :inf) 9))
      "ppatlace yields incorrect outputs when its REPEATS is inf")
  (is (equal (list 1 4 2 5 eop eop eop eop eop)
             (next-n (ppatlace (list (pseq (list 1 2 3)) (pseq (list 4 5 6 7 8))) 2) 9))
      "ppatlace yields incorrect outputs when its REPEATS is a number"))

(test pnary
  "Test pnary"
  (is (equal (list 3 4 5)
             (next-upto-n (pnary #'+ (pseq (list 1 2 3) 1) 2)))
      "pnary yields incorrect outputs with pattern and number as arguments")
  (is (equal (list 4 5 6)
             (next-upto-n (pnary #'+ (pseq (list 1 2 3) 1) 2 1)))
      "pnary yields incorrect outputs with pattern and two numbers as arguments")
  (is (equal (list 3 0)
             (next-upto-n (pnary (pseq (list #'+ #'-)) 2 (pseq (list 1 2) 1))))
      "pnary yields incorrect outputs when its operator is a pattern")
  (is (equal (list 1 2 3)
             (next-n (pnary #'+ 0 (pseq (list 1 (prest 2) 3))) 3))
      "pnary does not handle prests correctly"))

(test prerange
  "Test prerange"
  (is (equal (list 10 20 30)
             (mapcar #'round ;; needed because of floating point rounding errors
                     (next-upto-n (prerange (pseq (list -1 -2 -3) 1)
                                            (list 0 -10)
                                            (list 0 100)))))
      "prerange yields incorrect outputs"))

(test pslide
  "Test pslide"
  (is (equal (list 1 2 3 2 3 4 3 4 5 4 5 1 5)
             (next-n (pslide (list 1 2 3 4 5) :inf 3 1 0) 13))
      "pslide yields incorrect outputs for :inf REPEATS, 3 LEN, 1 STEP, 0 START")
  (is (equal (list 1 2 3 2 3 4)
             (next-upto-n (pslide (list 1 2 3 4 5) 2 3 1 0)))
      "pslide yields incorrect outputs for 2 REPEATS, 3 LEN, 1 STEP, 0 START")
  (is (equal (list 1 2 3 2 3 4 eop eop eop eop eop eop eop)
             (next-n (pslide (list 1 2 3 4 5) 2 3 1 0) 13))
      "pslide yields incorrect outputs for 2 REPEATS, 3 LEN, 1 STEP, 0 START")
  (is (equal (list 1 2 3 2 3 4 3 4 5 4 5 eop 5)
             (next-n (pslide (list 1 2 3 4 5) :inf 3 1 0 nil) 13))
      "pslide yields incorrect outputs for :inf REPEATS, 3 LEN, 1 STEP, 0 START, nil WRAP-AT-END")
  (is (equal (list 1 2 3 5 1 2 4 5 1 3 4 5 2)
             (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 0) 13))
      "pslide yields incorrect outputs for :inf REPEATS, 3 LEN, -1 STEP, 0 START")
  (is (equal (list 1 2 3 eop 1 2 eop eop 1 eop eop eop eop)
             (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 0 nil) 13))
      "pslide yields incorrect outputs for :inf REPEATS, 3 LEN, -1 STEP, 0 START, nil WRAP-AT-END")
  (is (equal (list 2 3 4 1 2 3 5 1 2 4 5 1 3)
             (next-n (pslide (list 1 2 3 4 5) :inf 3 -1 1) 13))
      "pslide yields incorrect outputs for :inf REPEATS, 3 LEN, -1 STEP, 1 START")
  (is (equal (list 1 2 3 2 3 1 2 3 1 1 2 3)
             (next-upto-n (pslide (list 1 2 3) 4 3 (pseq (list 1 -1 2)))))
      "pslide yields incorrect outputs when STEP is a pattern")
  (is (equal (list 0 1 2 1 2 3 2 3 4 3)
             (next-upto-n (pslide (list 0 1 2 3 4 5 6 7 8 9 10) :inf 3 0 (pseries 0 1)) 10))
      "pslide yields incorrect outputs when START is a pattern"))

(test phistory
  "Test phistory"
  (is (equal (list 0 nil 1)
             (next-n (phistory (pseries) (pseq (list 0 2 1))) 3))
      "phistory yields incorrect outputs when outputs that haven't occurred yet are accessed"))

(test pscratch
  "Test pscratch"
  (is (equal (list 0 1 2 3 0 1 2 3 0 1 2 3)
             (next-n (pscratch (pseries 0 1) (pseq (list 1 1 1 -3) :inf)) 12))
      "pscratch yields incorrect outputs when using patterns as source and step")
  (is (equal (list 0 0 0 0)
             (next-n (pscratch (pseries 0 1) (pseq (list 0) :inf)) 4))
      "pscratch yields incorrect outputs when step stays 0")
  (is (equal (list 0 0 1 1 2)
             (next-n (pscratch (pseries 0 1) (pseq (list 0 1) :inf)) 5))
      "pscratch yields incorrect outputs for steps of 0 and 1"))

(test pif
  "Test pif"
  (is (equal (list 1 2 4 5 3 eop 6)
             (next-n (pif (pseq (list t t nil nil t t nil) 1)
                          (pseq (list 1 2 3) 1)
                          (pseq (list 4 5 6) 1))
                     7))
      "pif yields incorrect outputs")
  (is (equal (list 1 2 3 nil 4)
             (next-n (pif (pseq (list t t nil nil nil))
                          (pseq (list 1 2))
                          (pseq (list 3 nil 4)))
                     5))
      "pif yields incorrect outputs"))

(test parp
  "Test parp"
  (is-true (every-event-equal
            (list (event :foo 1 :bar 4)
                  (event :foo 1 :bar 5)
                  (event :foo 1 :bar 6)
                  (event :foo 2 :bar 4)
                  (event :foo 2 :bar 5)
                  (event :foo 2 :bar 6)
                  (event :foo 3 :bar 4)
                  (event :foo 3 :bar 5)
                  (event :foo 3 :bar 6))
            (next-n (parp (pbind :foo (pseq (list 1 2 3)))
                          (pbind :bar (pseq (list 4 5 6) 1)))
                    9))
           "parp yields incorrect outputs")
  (is-true (every-event-equal
            (list (event :freq 200 :xx 400)
                  (event :freq 200 :xx 200)
                  (event :freq 300 :xx 600)
                  (event :freq 300 :xx 300)
                  (event :freq 400 :xx 800)
                  (event :freq 400 :xx 400)
                  eop)
            (next-n (parp (pbind :freq (pseq (list 200 300 400) 1))
                          (pbind :xx (p* (pk :freq 99) (pseq (list 2 1) 1))))
                    7))
           "parp yields incorrect outputs when the arpeggiator pattern references values from the base pattern via pk")
  (is-true (every-event-equal
            (list (event :x 1 :y 3 :z 5)
                  (event :x 1 :y 3 :z 6)
                  (event :x 1 :y 4 :z 5)
                  (event :x 1 :y 4 :z 6)
                  (event :x 2 :y 3 :z 5)
                  (event :x 2 :y 3 :z 6)
                  (event :x 2 :y 4 :z 5)
                  (event :x 2 :y 4 :z 6))
            (next-upto-n (parp (pbind :x (pseq (list 1 2) 1))
                               (parp (pbind :y (pseq (list 3 4) 1))
                                     (pbind :z (pseq (list 5 6) 1))))))
           "triply-nested parp yields incorrect outputs"))

(test pfin
  "Test pfin"
  (is (= 3
         (length (next-upto-n (pfin (pseq (list 1 2 3) :inf) 3))))
      "pfin doesn't correctly limit its source pattern when COUNT is a number")
  (is (= 3
         (length (next-upto-n (pfin (pseq (list 1 2 3) :inf) (pseq (list 3))))))
      "pfin doesn't correctly limit its source pattern when COUNT is a pattern"))

(test pfindur
  "Test pfindur"
  (is (= 5
         (reduce #'+ (mapcar #'dur (next-upto-n (pfindur (pbind :dur (pwhite 0.0 1.0)) 5)))))
      "pfindur patterns don't have a correct total duration")
  (is (= 99
         (length (next-upto-n (pfindur (pbind :dur 5) :inf) 99)))
      "pfindur doesn't properly handle :inf as its DUR")
  (is-false (remove-if-not (lambda (n) (> n 4))
                           (let (list)
                             (dotimes (n 100 list)
                               (push (reduce #'+ (next-upto-n (pfindur (pwhite 0.1 2.0 4) 4))) list))))
            "pfindur doesn't limit value patterns")
  (is (= 2
         (length (next-upto-n (pfindur (pbind :dur 2) 5 1))))
      "pfindur's TOLERANCE argument doesn't work properly"))

(test psync
  "Test psync"
  (is-true (every-event-equal
            (list (event :dur 5) (event :type :rest :dur 3))
            (next-upto-n (psync (pbind :dur (pseq (list 5) 1)) 4)))
           "psync doesn't correctly quantize up to the next multiple of QUANT")
  (is-true (equal
            (list 5 5 5 1)
            (mapcar #'dur (next-upto-n (psync (pbind :dur (pseq (list 5) 5)) 4 16))))
           "psync fails to limit its source pattern to MAXDUR")
  (is-true (every-event-equal
            (list (event :dur 2) (event :dur 2))
            (next-upto-n (psync (pbind :dur 2) 5 5 1)))
           "psync's TOLERANCE argument doesn't work properly")
  (is-true (every-event-equal
            (list (event :dur 2) (event :dur 2) (event :dur 1))
            (next-upto-n (psync (pbind :dur 2) 5 5 0.5)))
           "psync with TOLERANCE doesn't cut off events after MAXDUR"))

(test pdurstutter
  "Test pdurstutter"
  (is (equal (list 2 3/2 3/2)
             (next-upto-n (pdurstutter (pseq (list 1 2 3) 1) (pseq (list 0 1 2) 1))))
      "pdurstutter yields incorrect outputs for value patterns")
  (is (every-event-equal
       (list (event :foo 1 :dur 1)
             (event :foo 2 :dur 1/2)
             (event :foo 2 :dur 1/2))
       (next-upto-n (pdurstutter (pbind :foo (pseries)) (pseq (list 0 1 2) 1))))
      "pdurstutter yields incorrect outputs for event patterns"))

(test pbeat
  "Test pbeat"
  (is (equalp (list 0.0 0.25 0.5 0.75 1.0 1.25)
              (let ((pstr (as-pstream (pbind :foo (pbeat) :dur 0.25))))
                (loop :for i :upto 5
                   :collect (event-value (next pstr) :foo))))
      "pbeat yields incorrect outputs")
  ;; FIX: test for peek
  )

(test pbeat*
  "Test pbeat*"
  (with-fixture with-debug-backend-and-clock ()
    (play (pbind :dur (pseq (list 1 1/2 1/4 1/4)) :x (pbeat*)))
    (clock-process *clock* 4)
    (let ((results (mapcar (fn (event-value _ :x))
                           (nreverse (debug-recent-events 8)))))
      (is (equal (list 0 1 3/2 7/4 2 3 7/2 15/4) results)
          "pbeat* doesn't correctly read the clock's beat; got ~s" results))))

(test ptime
  ;; FIX
  )

(test psinosc
  ;; FIX
  )

(test pindex
  "Test pindex"
  (is (equal
       (list 3 2 1 eop eop eop eop)
       (next-n (pindex (list 3 2 1 0) (pseq (list 0 1 2) 1)) 7))
      "pindex yields incorrect outputs")
  (is (equal (list 99 98 97 99 98 97 99 98 97)
             (next-n (pindex (list 99 98 97) (pseries 0 1) t) 9))
      "pindex yields incorrect outputs when WRAP-P is t")
  (is (equal (list 2 3 4 nil)
             (next-n (pindex (list 2 3 4) (pseries 0 1) nil) 4))
      "pindex yields incorrect outputs when WRAP-P is nil"))

(test prun
  "Test prun"
  (is-true (every-event-equal
            (list (event :foo 1 :bar 4)
                  (event :foo 2 :bar 5)
                  (event :foo 3 :bar 5)
                  (event :foo 4 :bar 6)
                  (event :foo 5 :bar 8))
            (next-upto-n (pbind :foo (pseq (list 1 2 3 4 5) 1)
                                :bar (prun (pseq (list 4 5 6 7 8) 1)
                                           (pseq (list 1 2 0.5 0.5 1) 1)))))
           "prun yields incorrect outputs")
  (is-true (every-event-equal
            (list (event :foo 1 :bar 4)
                  (event :foo 2 :bar 4)
                  (event :foo 3 :bar 5)
                  (event :foo 4 :bar 5)
                  (event :foo 5 :bar 6))
            (next-upto-n (pbind :foo (pseq (list 1 2 3 4 5) 1)
                                :bar (prun (pseq (list 4 5 6 7 8) 1)
                                           2))))
           "prun doesn't support numbers as its DUR"))

(test psym ;; FIX: add more
  "Test psym"
  (is-true (let ((cl-patterns::*pdef-dictionary* (make-hash-table)))
             (pdef :foo1 (pbind :dur (pn 1/2 8)))
             (pdef :foo2 (pbind :dur (pn 2/3 8)))
             (every-event-equal
              (mapcar (lambda (e) (remove-event-value e :pdef))
                      (next-upto-n (psym (pseq (list (list :foo1 :foo2)) 1))))
              (next-upto-n (ppar (list (pdef-pattern (pdef :foo1))
                                       (pdef-pattern (pdef :foo2)))))))
           "psym and ppar don't yield the same outputs when provided the same patterns as inputs")
  (is (every-event-equal
       (list
        (event :foo 0)
        (event :foo 1)
        (event :foo 2)
        (event :foo 3))
       (next-upto-n (psym (pseq (list (pbind :foo (pseries 0 1 4))) 1))))
      "psym doesn't allow regular patterns to be used instead of symbols"))

(test pchain
  "Test pchain"
  (is-true (every-event-equal
            (list (event :foo 1 :bar 7) (event :foo 2 :bar 8) (event :foo 3 :bar 9) eop)
            (next-n (pchain (pbind :foo (pseq (list 1 2 3))) (pbind :bar (pseq (list 7 8 9) 1))) 4))
           "pchain doesn't combines the outputs from each of its input patterns correctly")
  (is-true (every-event-equal
            (list (event :foo 1 :bar 1) (event :foo 2 :bar 2) (event :foo 3 :bar 3) eop)
            (next-n (pchain (pbind :foo (pseq (list 1 2 3) 1)) (pbind :bar (pk :foo))) 4))
           "outputs from previous patterns are accessible in subsequent patterns when pchain'd"))

(test pdiff
  "Test pdiff"
  (is (equal (list -2 3 -1 eop)
             (next-n (pdiff (pseq (list 3 1 4 3) 1)) 4))
      "pdiff yields incorrect outputs"))

(test pdelta
  "Test pdelta"
  (is (equal (list 1 1 1 1 1 1 1 1)
             (next-n (pdelta (pseq (list 0 1 2 3)) 4) 8))
      "pdelta yields incorrect outputs for a basic pattern")
  (is (equal (list 1 1 3 3 1 1 3 3)
             (next-n (pdelta (pseq (list 0 1 2 5)) 4) 8))
      "pdelta yields incorrect outputs for a pattern \"longer\" than the CYCLE")
  (is (equal (list 1 1 2 0 1 1 2 0)
             (next-n (pdelta (pseq (list 0 1 2 0)) 4) 8))
      "pdelta yields incorrect outputs for a pattern with successive outputs that decrease")
  (is (equal (list 1 1 2 eop eop)
             (next-n (pdelta (pseq (list 0 1 2 0) 1) 4) 5))
      "pdelta doesn't output eop when its source pattern ends"))

(test pdrop
  "Test pdrop"
  (is (equal (list 3 4 eop eop)
             (next-n (pdrop (pseq (list 1 2 3 4) 1) 2) 4))
      "pdrop doesn't drop the first N outputs")
  (is (equal (list 1 2 3 eop)
             (next-n (pdrop (pseq (list 1 2 3 4 5) 1) -2) 4))
      "pdrop doesn't drop the last N outputs"))

(test ppar
  "Test ppar"
  (is-true (let ((pat (pbind :dur (pn 4/3 8))))
             (every-event-equal
              (mapcar (lambda (ev) (combine-events ev (event :delta 4/3)))
                      (next-upto-n pat))
              (next-upto-n (ppar (list pat)))))
           "ppar yields incorrect outputs when its LIST has only one pattern")
  (is-false (typep (quant (as-pstream (ppar (list (pseq (list 1 2 3) 1)))))
                   'pstream)
            "patterns that use the default as-pstream method have their quant converted to a t-pstream" ;; FIX: rewrite this test so it doesn't depend on ppar not having its own as-pstream method and tests quant conversion more directly
            )
  (is (< 10 (length (next-upto-n (ppar (list (pbind :x (pn 1 4)) (pbind :y (pseries)))) 20)))
      "ppar doesn't continue with the rest of the patterns after one ends")
  (is (equal (list 0 1 0 1 0 1 0 1 0 1 1 1)
             (mapcar #'delta (next-upto-n (ppar (list (pbind :dur 1)
                                                      (pbind :dur (pn 1 4))))
                                          12)))
      "ppar doesn't pad its output with rests correctly when one pattern ends early")
  (is (= 8
         (let ((pstr (as-pstream (ppar (list (pbind :dur (pn 1/8 4))
                                             (pbind :dur (pn 1 8))
                                             (pbind :dur (pn 1/16 16)))))))
           (next-upto-n pstr)
           (beat pstr)))
      "ppar doesn't have the same duration as its longest subpattern"))

(test pmeta
  "Test pmeta"
  ;; FIX
  (let ((cl-patterns::*pdef-dictionary* (make-hash-table)))
    (pdef :foo (pbind :dur (pwhite 0.5 2.0 8)))
    (is (= 4
           (reduce #'+ (mapcar #'dur (next-upto-n (pmeta :pattern (pseq (list :foo) 1) :dur 4)))))
        "pmeta's :dur key doesn't work properly")))

(test pts
  "Test pts"
  (let ((dur (random-range 1 8)))
    (is (= dur
           (reduce #'+ (next-upto-n (pts (pbind :dur (pwhite 1 64 12))
                                         dur))
                   :key #'dur))
        "pts doesn't correctly timestretch a pattern to a duration of ~a"
        dur)))

(test pwalk
  "Test pwalk"
  (is (equal (list 0 1 3 3 3 0 2 2 2)
             (next-n (pwalk (list 0 1 2 3) (pseq (list 1 2 0 0)) 1) 9))
      "pwalk yields incorrect outputs for basic inputs")
  (is (equal (list 60 64 67 72 76 79 84 79 76 72 67 64)
             (next-n (pwalk (list 60 64 67 72 76 79 84) (pseq (list 1)) (pseq (list 1 -1)) 0) 12))
      "pwalk's DIRECTION-PATTERN input doesn't work properly"))

(test pparchain
  "Test pparchain"
  (is (every-event-equal
       (list
        (list (event :foo 0) (event :foo 3 :baz 1))
        (list (event :foo 1) (event :foo 4 :baz 2))
        (list (event :foo 2) (event :foo 5 :baz 3)))
       (next-upto-n (pparchain (pbind :foo (pseries 0 1 3)) (pbind :baz (p+ (pk :foo) 1) :foo (p+ (pk :foo) 3)))))
      "pparchain yields incorrect outputs"))

(test ppc
  "Test ppc"
  (is (every-event-equal
       (list
        (list (event :foo 1) (event :foo 1 :bar 3))
        (list (event :foo 2) (event :foo 2 :bar 4))
        (list (event :foo 3) (event :foo 3 :bar 5)))
       (next-upto-n (ppc :foo (pseq (list 1 2 3) 1)
                         :-
                         :bar (p+ (pk :foo) 2))))
      "ppc yields incorrect outputs"))

(test pclump
  "Test pclump"
  (is-true (equal (list (list 0 1) (list 2 3) (list 4))
                  (next-upto-n (pclump (pseries 0 1 5) 2)))
           "pclump yields incorrect outputs for constant N")
  (is-true (equal (list (list 0) (list 1 2) (list 3 4 5) (list 6 7 8 9) (list 10 11 12 13 14))
                  (next-upto-n (pclump (pseries 0 1) (pseries 1 1 5))))
           "pclump yields incorrect outputs for pattern N"))

(test paclump
  "Test paclump"
  (is-true (every-event-equal
            (list (event :foo (list 1) :bar (list 0))
                  (event :foo (list 1 2) :bar (list 1 2))
                  (event :foo (list 1 2 3) :bar (list 3 4 5)))
            (next-upto-n (pbind :foo (pseq (list (list 1) (list 1 2) (list 1 2 3)) 1) :bar (paclump (pseries)))))
           "paclump yields incorrect output"))

(test paccum
  "Test paccum"
  (is (equal (list 0 1 2 3 4 5 6 7 8 9 10 9 10 9 10)
             (next-upto-n (paccum #'+ 0 1 :inf :lo 0 :hi 10 :bound-by #'fold) 15))
      "paccum yields incorrect output")
  (is (equal (list 0 2 4 6 8 0 2 4 6 8 0 2 4 6 8)
             (next-upto-n (paccum #'+ 0 2 :inf :lo 0 :hi 10 :bound-by #'wrap) 15))
      "paccum yields incorrect output for wrapped bounding")
  (is (equal (list 1 2 4 8 16 32 64 72 56 88)
             (next-upto-n (paccum #'* 1 2 :inf :lo 0 :hi 100 :bound-by 'fold) 10))
      "paccum yields incorrect output for #'* as OPERATOR")
  (is (= 2
         (length (next-upto-n (paccum #'+ 0 1 2))))
      "paccum yields the wrong number of outputs when LENGTH is 2"))

(test ps
  "Test ps"
  (is (equal (list (list 0 1 2 3) (list 4 5 6 7) (list 8 9 10 11))
             (let ((pat (ps (pseries))))
               (loop :repeat 3
                     :collect (next-upto-n pat 4))))
      "ps fails to resume the pstream on subsequent calls to as-pstream"))
