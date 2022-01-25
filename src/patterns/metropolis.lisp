(in-package #:cl-patterns)

;;;; pmetropolis
;; has parameters: pitch, pulse count, gate type

(defpattern pmetropolis (pattern)
  (pattern)
  :documentation "Compact mini-language for rhythm patterns inspired by the Intellijel Metropolis.

Example: ;; FIX

;; (pmetropolis '())
;; =>

See also: `pcycles', `ptracker', `pdurstutter'"
  )
