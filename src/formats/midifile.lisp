;;; midifile.lisp
;; cl-patterns functionality to interact with midi files
;; i.e. read a midi file as a pattern
;; or write a pattern as a midi file

;;; NOTES:
;; - https://www.recordingblogs.com/wiki/time-division-of-a-midi-file

(in-package #:cl-patterns)

(defmethod midinote ((this midi::voice-message))
  (slot-value this 'midi::key))

;; (defmethod beat ((this midi::voice-message)) ;; FIX: doesn't work, since we don't have division information here?
;;   (/ (midi:message-time i) division))

(defparameter *unknowns* (list))

(defun midi-track-as-pattern (track division)
  "Translate a list of MIDI events from the `midi' system into a cl-patterns pattern."
  (let (results
        (pseq (pseq (list) 1)))
    (setf *unknowns* (list))
    (setf (slot-value pseq 'list)
          (nreverse
           (dolist (msg track results)
             (typecase msg
               (midi:note-on-message
                (push (event :channel (midi:message-channel msg)
                             :beat (/ (midi:message-time msg) division)
                             :amp (/ (midi:message-velocity msg) 127)
                             :midinote (midinote msg))
                      results))
               (midi:note-off-message
                (let ((note-on (car (member-if (lambda (n)
                                                 (and (= (midi:message-channel msg) (event-value n :channel))
                                                      (= (midinote msg) (event-value n :midinote))))
                                               results))))
                  (if note-on
                      (setf (event-value note-on :sustain) (- (/ (midi:message-time msg) division) (event-value note-on :beat)))
                      (warn "Couldn't find the associated event for this note off message: ~s" msg))))
               (midi:sequence/track-name-message
                (setf (pattern-metadata pseq :track-name) (slot-value msg 'midi::text)))
               (midi:program-change-message
                (setf (pattern-metadata pseq :midi-program) (midi:message-program msg)
                      (pattern-metadata pseq :midi-channel) (midi:message-channel msg)))
               (midi:tempo-message
                (push (event :type :tempo-change
                             :beat (/ (midi:message-time msg) division)
                             :tempo (* 1000000 (/ 1 (midi:message-tempo msg))))
                      results))
               ;; (midi:time-signature-message
               ;;  )
               (t
                (push msg *unknowns*)
                (warn "Unknown MIDI event type: ~s; pushed to *UNKNOWNS*." msg))))))
    pseq))

(defun midifile-as-patterns (midifile)
  "Open MIDIFILE with the `midi' system and get its tracks as cl-patterns patterns."
  (typecase midifile
    ((or string pathname) (midifile-as-patterns (midi:read-midi-file midifile)))
    (midi:midifile
     (mapcar (lambda (track)
               (midi-track-as-pattern track (midi:midifile-division midifile)))
             (midi:midifile-tracks midifile)))))

(export '(midi-track-as-pattern midifile-as-patterns))

;; http://somascape.org/midi/tech/mfile.html
;; (if (logbitp 8 (midi:midifile-division midi))
;;     :timecode
;;     :metrical-timing)

;; 480 = 01e0
;; 224 = 00e0

;; (midi:midifile-division midi) = 480
;; (logand (midi:midifile-division midi) #b011111111) = 224

;;; from https://www.lispforum.com/viewtopic.php?f=2&t=1205

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))


