(in-package #:cl-patterns)

;;;; sequence-extensions.lisp
;;; code to implement extensible sequences functionality.
;;; at the moment, it is only supported by sbcl and abcl.

;; https://github.com/guicho271828/common-lisp-extensions/issues/8
;; http://www.sbcl.org/manual/#Extensible-Sequences

;;; pstream

(defmethod sequence:length ((this pstream))
  (slot-value this 'number)) ;; FIX: this should use the history-length pstream slot instead once it is implemented

(defmethod sequence:elt ((this pstream) index)
  (pstream-elt this index))

(defmethod (setf sequence:elt) (new-value (this pstream) index)
  ;; FIX: raise protocol unimplemented error
  )

(defmethod sequence:adjust-sequence ((this pstream) length &key initial-element initial-contents)
  ;; FIX: raise protocol unimplemented error?
  )

(defmethod sequence:make-sequence-like ((this pstream) length &key initial-element initial-contents)
  ;; FIX: raise protocol unimplemented error?
  )

(defmethod sequence:emptyp ((this pstream))
  ;; FIX
  )

;; FIX: add more (see URL above)

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
