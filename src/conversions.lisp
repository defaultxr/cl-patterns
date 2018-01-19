(in-package :cl-patterns)

;;; amp/db

(defun amp-db (amp)
  "Convert amplitude to dB."
  (* 20 (log amp 10)))

(defun db-amp (db)
  "Convert dB to amplitude."
  (expt 10 (* db 0.05)))

;;; dur

(defun dur-time (dur &optional tempo)
  "Convert duration in beats to time in seconds according to TEMPO in beats per second."
  (/ dur (or tempo
             (and (boundp '*clock*) (not (null *clock*)) (tempo *clock*))
             1)))

(defun time-dur (time &optional tempo)
  "Convert TIME in seconds to duration in beats according to TEMPO in beats per second."
  (* time (or tempo
              (and (boundp '*clock*) (not (null *clock*)) (tempo *clock*))
              1)))

;;; freq/midinote/octave/root/degree

(defun midinote-freq (midinote)
  "Convert a midi note number to a frequency."
  (* 440 (expt 2 (/ (- midinote 69) 12))))

(defun freq-midinote (freq)
  "Convert a frequency to a midi note number."
  (+ 69 (* 12 (log (/ freq 440) 2))))

(defun freq-octave (freq)
  "Given FREQ, return the octave number that it occurs in."
  (midinote-octave (freq-midinote freq)))

(defun midinote-octave (midinote)
  "Given MIDINOTE, return the octave number that it occurs in."
  (truncate (/ midinote 12)))

(defun midinote-degree (midinote &optional root octave scale)
  (flet ((index-of-greater-than (n list) ;; FIX
           ;; LIST should be a sorted list.
           (position n (sort list #'<=) :test #'<=))))
  (let* ((notes (scale-notes (scale (scale (or scale :major)))))
         (octave (or octave (truncate (/ midinote 12))))
         (diff (- midinote (* octave 12)))
         (root (or root (- midinote ;; FIX
                           (position diff notes)))) ;; FIX
         )
    (position midinote (mapcar (lambda (n) (+ (* octave 12) root n))
                               notes))))

(defun note-midinote (note &optional (root 0) (octave 5))
  (+ root (* octave 12) (note-number note)))

(defun degree-note (degree &optional (scale :major))
  (let* ((scale (scale (or scale :major)))
         (notes (scale-notes scale)))
    (+ (nth-wrap degree notes)
       (* (length (tuning-tuning (tuning (scale-tuning scale))))
          (floor (/ degree (length notes)))))))

(defun degree-midinote (degree &optional root octave scale)
  ;; (let* ((root (or root (if (and (boundp '*event*) (not (null *event*)))
  ;;                           (get-event-value *event* 'root)
  ;;                           0)))
  ;;        (octave (or octave (if (and (boundp '*event*) (not (null *event*)))
  ;;                               (get-event-value *event* 'octave)
  ;;                               5)))
  ;;        (scale (scale (or scale (if (and (boundp '*event*) (not (null *event*)))
  ;;                                    (get-event-value *event* 'scale)
  ;;                                    :major))))
  ;;        (note (degree-note degree scale))))
  (note-midinote (degree-note degree scale) root octave)
  ;; (+ (* (+ (/ (+ note root)
  ;;             (length (tuning-tuning (tuning (scale-tuning scale)))))
  ;;          octave
  ;;          -5)
  ;;       (* 12 (log (tuning-octave-ratio (tuning (scale-tuning scale))) 2)))
  ;;    60)
  )

(defun degree-freq (degree &optional root octave scale)
  (midinote-freq (degree-midinote degree root octave scale)))

(defun ratio-midi (ratio)
  (* 12 (log ratio 2)))

(defun midi-ratio (midi)
  (expt 2 (/ midi 12)))

