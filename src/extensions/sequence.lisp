;;;; sequence.lisp - extensible sequences functionality.
;;; May not be supported on all Lisp implementations, but should be enabled automatically when cl-patterns is loaded on an implementation with support.
;;; https://github.com/guicho271828/common-lisp-extensions/issues/8
;;; http://www.sbcl.org/manual/#Extensible-Sequences

(in-package #:cl-patterns)

;;; scale

(defmethod sequence:length ((this scale))
  (length (scale-notes this)))

(defmethod sequence:elt ((this scale) index)
  (elt (scale-notes this) index))

(defmethod sequence:emptyp ((this scale))
  nil)

;;; tuning

(defmethod sequence:length ((this tuning))
  (length (tuning-pitches this)))

(defmethod sequence:elt ((this tuning) index)
  (elt (tuning-pitches this) index))

(defmethod sequence:emptyp ((this tuning))
  nil)

;;; chord

(defmethod sequence:length ((this chord))
  (length (chord-indexes this)))

(defmethod sequence:elt ((this chord) index)
  (elt (chord-indexes this) index))

(defmethod sequence:emptyp ((this chord))
  nil)

;;; pstream

(defmethod sequence:length ((this pstream))
  (slot-value this 'number)) ; FIX: this should use the history-length pstream slot instead once it is implemented

(defmethod sequence:elt ((this pstream) index)
  (pstream-elt this index))

(defmethod (setf sequence:elt) (new-value (this pstream) index)
  (sequence:protocol-unimplemented 'sequence:elt this))

(defmethod sequence:adjust-sequence ((this pstream) length &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (sequence:protocol-unimplemented 'sequence:adjust-sequence this)) ; FIX?

(defmethod sequence:make-sequence-like ((this pstream) length &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (sequence:protocol-unimplemented 'sequence:make-sequence-like this)) ; FIX?

(defmethod sequence:emptyp ((this pstream))
  (and (ended-p this)
       (null (elt this 0))))

;; FIX: add more (see URL above)
