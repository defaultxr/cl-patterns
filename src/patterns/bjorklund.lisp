;;;; bjorklund.lisp - euclidean rhythms generated with the bjorklund algorithm.
;;; TODO:
;; FIX: change :dur parameter to be a total duration, like in pfindur, and maybe make a new :dur-mul or similar parameter to provide the functionality that :dur currently has.
;; FIX: add sustain-notes parameter which, when true, sustains each note until the next one instead of inserting rests.

(in-package #:cl-patterns)

(defun bjorklund (pulses &optional steps (offset 0))
  "Generate a list representing a Euclidean rhythm using the Bjorklund algorithm. PULSES is the number of \"hits\" in the sequence, STEPS is number of divisions of the sequence, and OFFSET is the number to rotate the sequence by. This function returns a list, where 1 represents a note and 0 represents a rest. If you want to use bjorklund in a pattern, you may be more interested in `pbjorklund' instead, which returns events with the correct duration and type.

Example: (bjorklund 3 7) ;=> (1 0 1 0 1 0 0)

See also: `pbjorklund'"
  (when (and (null steps) (typep pulses 'ratio))
    (return-from bjorklund (bjorklund (numerator pulses) (denominator pulses))))
  (check-type steps (integer 1))
  (assert (>= steps pulses) (pulses))
  (labels ((from-array (arr)
             (destructuring-bind (a b) (split arr)
               (if (and (> (length b) 1) (> (length a) 0))
                   (from-array (lace a b))
                   (flatten (append a b)))))
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
    (rotate
     (from-array
      (append (make-list pulses :initial-element (list 1))
              (make-list (- steps pulses) :initial-element (list 0))))
     offset)))

(defpattern pbjorklund (pattern)
  (pulses
   steps
   (offset :initform 0)
   (dur :initform 1)
   (repeats :initform *default-pattern-repeats*))
  :documentation "pbjorklund generates Euclidean rhythms using the Bjorklund algorithm. PULSES is the number of notes in the sequence, and STEPS is number of steps in the sequence. Additionally, OFFSET is the number to rotate the sequence by, DUR is the total duration one repeat of the sequence should be, and REPEATS is the number of repeats that should be yielded. This pattern outputs events which can be embedded into another pattern. Each pulse is a note, and each subdivision of the sequence that is not a pulse is a rest. If you just want the raw output from the Bjorklund algorithm (not in pattern form), use `bjorklund' instead.

Example:

;; (next-upto-n (pbjorklund 3 7))
;; => ((EVENT :TYPE :NOTE :DUR 1/7) (EVENT :TYPE :REST :DUR 1/7) (EVENT :TYPE :NOTE :DUR 1/7) (EVENT :TYPE :REST :DUR 1/7) (EVENT :TYPE :NOTE :DUR 1/7) (EVENT :TYPE :REST :DUR 1/7) (EVENT :TYPE :REST :DUR 1/7))

See also: `bjorklund'"
  :defun (defun pbjorklund (pulses steps &key (offset 0) (dur 1) (repeats *default-pattern-repeats*))
           (make-instance 'pbjorklund
                          :pulses pulses
                          :steps steps
                          :offset offset
                          :dur dur
                          :repeats repeats)))

(defmethod as-pstream ((pbjorklund pbjorklund))
  (with-slots (pulses steps offset dur repeats) pbjorklund
    (make-instance 'pbjorklund-pstream
                   :pulses (next pulses)
                   :steps (next steps)
                   :offset (pattern-as-pstream offset)
                   :dur (next dur)
                   :repeats (next repeats))))

(defmethod next ((pattern pbjorklund-pstream))
  (with-slots (number pulses steps offset dur repeats) pattern
    (let* ((c-offset (next offset))
           (val (when (or (eql repeats :inf)
                          (< (/ number (* steps repeats)) 1))
                  (nth-wrap number (bjorklund pulses steps c-offset)))))
      (if (or (eop-p c-offset)
              (eop-p val)
              (null val))
          eop
          (event :type (if (= 1 val)
                           (if *event*
                               (e :type)
                               :note)
                           :rest)
                 :dur (* (/ 1 steps) dur))))))

