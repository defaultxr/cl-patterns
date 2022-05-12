(in-package #:cl-patterns/tests)

(in-suite cl-patterns-tests)

;;;; t/conversions.lisp - tests for unit conversion functions.

;;; TODO:
;; FIX: add more.

;; NOTE: Many of the conversion functions are affected by floating point rounding errors.
;; This is why we use `mutility:approx=' for many of these comparisons rather than `cl:='.

(test amplitude-conversion
  "Test amplitude conversion functions"
  (is-true (let ((input (iota 10 :start 1/10 :step 1/10)))
             (every #'identity (mapcar #'approx=
                                       (mapcar #'db-amp (mapcar #'amp-db input))
                                       input)))
           "db-amp conversion is not equivalent to amp-db conversion"))

(test time-conversion
  "Test time conversion functions"
  (is-true (let ((input (iota 22 :step 1/10)))
             (equal input (mapcar #'time-dur (mapcar #'dur-time input))))
           "time-dur conversion is not equivalent to dur-time conversion"))

(test pitch-conversion
  "Test pitch conversion functions"
  (is-false (let ((input (iota 128)))
              (position nil (mapcar #'approx=
                                    input
                                    (mapcar #'freq-midinote (mapcar #'midinote-freq input)))))
            "freq-midinote conversion is not equivalent to midinote-freq conversion")
  (is (every #'identity (mapcar #'=
                                (list 13.75 27.5 55 110 220 440 880 3520)
                                (mapcar #'midinote-freq (list 9 21 33 45 57 69 81 105))))
      "midinote-freq conversion is not correct")
  (is (equal (loop :for i :from 0 :upto 10
                   :append (make-list 12 :initial-element i))
             (mapcar #'midinote-octave (iota 132)))
      "midinote-octave conversion is not correct")
  (is (= (degree-freq 0)
         (midinote-freq (degree-midinote 0)))
      "degree-freq and degree-midinote are not equivalent"))

(test rate-conversion
  "Test rate conversion functions"
  (is (equal (list 55 110 220 440 880 1760)
             (mapcar #'rate-freq (list 1/8 1/4 1/2 1 2 4)))
      "rate-freq conversion is not correct")
  (is (= 300
         (rate-freq 1 300))
      "rate-freq conversion is not correct when base-freq is provided")
  (is (equal (list 1/8 1/4 1/2 1 2 4)
             (mapcar #'freq-rate (list 55 110 220 440 880 1760)))
      "freq-rate conversion is not correct")
  (is (= 2
         (freq-rate 2600 1300))
      "freq-rate conversion is not correct when base-freq is provided"))


