(in-package #:cl-patterns)

;;; pbjorklund

(defun bjorklund (pulses &optional steps (offset 0))
  "Generate a list representing a Euclidean rhythm using the Bjorklund algorithm. PULSES is the number of \"hits\" in the sequence, STEPS is number of divisions of the sequence, and OFFSET is the number to rotate the sequence by. This function returns a list, where 1 represents a note and 0 represents a rest. If you want to use bjorklund in a pattern, you may be more interested in `pbjorklund' instead, which returns events with the correct duration and type.

Example: (bjorklund 3 7) ;=> (1 0 1 0 1 0 0)

See also: `pbjorklund'"
  (if (and (null steps) (typep pulses 'ratio))
      (bjorklund (numerator pulses) (denominator pulses))
      (progn
        (assert (> steps 0) (steps))
        (assert (>= steps pulses) (pulses))
        (labels ((from-array (arr)
                   (destructuring-bind (a b) (split arr)
                     (if (and (> (length b) 1) (> (length a) 0))
                         (from-array (lace a b))
                         (alexandria:flatten (append a b)))))
                 (split (arr)
                   (let ((index (position (car (last arr)) arr :test #'equal)))
                     (list (subseq arr 0 index)
                           (subseq arr index))))
                 (lace (a b)
                   (append (loop
                              :for x :in a
                              :for i :from 0
                              :collect (list x (nth i b)))
                           (when (<= (length a) (length b))
                             (subseq b (length a))))))
          (alexandria:rotate
           (from-array
            (append (make-list pulses :initial-element (list 1))
                    (make-list (- steps pulses) :initial-element (list 0))))
           offset)))))

(defpattern pbjorklund (pattern)
  (pulses
   steps
   (offset :default 0))
  "pbjorklund generates Euclidean rhythms using the Bjorklund algorithm. PULSES is the number of notes in the sequence, STEPS is number of steps in the sequence, and OFFSET is the number to rotate the sequence by. This pattern returns events which can be injected into another pattern. Each pulse is a note, and each subdivision of the sequence that is not a pulse is a rest. The total duration of the sequence is 1 beat, so it can be easily multiplied if you want it to be longer or shorter. If you just want the raw output from the Bjorklund algorithm, use `bjorklund' instead.

Example:

;; (next-upto-n (pbjorklund 3 7))
;; => ((EVENT :TYPE :NOTE :DUR 1/7) (EVENT :TYPE :REST :DUR 1/7) (EVENT :TYPE :NOTE :DUR 1/7) (EVENT :TYPE :REST :DUR 1/7) (EVENT :TYPE :NOTE :DUR 1/7) (EVENT :TYPE :REST :DUR 1/7) (EVENT :TYPE :REST :DUR 1/7))

See also: `bjorklund'")

;; FIX: add sustain-notes parameter which, when true, sustains each note until the next one instead of inserting rests.

(defmethod as-pstream ((pbjorklund pbjorklund))
  (with-slots (pulses steps offset) pbjorklund
    (make-instance 'pbjorklund-pstream
                   :pulses pulses
                   :steps steps
                   :offset (pattern-as-pstream offset))))

(defmethod next ((pattern pbjorklund-pstream))
  (with-slots (number pulses steps offset) pattern
    (alexandria:when-let ((val (nth number (bjorklund pulses steps (next offset)))))
      (event :type (if (= 1 val) :note :rest)
             :dur (/ 1 steps)))))

