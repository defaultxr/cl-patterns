;;;; supercollider-score.lisp - support for importing/exporting SuperCollider score files.
;;; Primarily for non-realtime (NRT) synthesis; also used by the `render' method to implement pattern rendering.
;;;
;;; NOTES:
;; http://doc.sccode.org/Classes/Score.html

(in-package #:cl-patterns)

(defclass supercollider-score ()
  ((list :initarg :list :accessor score-list :documentation "The list of directives in the score."))
  (:documentation "A list of instructions for the SuperCollider server to `render' in non-realtime."))

(defun write-synthdef-file (synth)
  "Write the synthdef file for SYNTH to the synthdef directory. This is used by `as-supercollider-score' to ensure the non-realtime SuperCollider server is able to load the instruments in the score when rendering.

See also: `as-supercollider-score', `render', `supercollider-score-write-encoded'"
  (let ((cl-collider::*synth-definition-mode* :load)
        (meta (cl-collider:synthdef-metadata synth)))
    (if meta
        (eval `(cl-collider:defsynth ,synth ,(getf meta :controls)
                 ,@(getf meta :body)))
        (cerror "Couldn't find metadata for a synthdef with name ~S." synth))))

(defgeneric as-supercollider-score (object &key tempo dur max-length)
  (:documentation "Convert an object into SuperCollider's score format.

See also: `render', `supercollider-score-write-encoded'"))

(defmethod as-supercollider-score ((events list) &key (backend (find-backend 'supercollider)) (tempo (tempo *clock*)) (max-length *max-pattern-yield-length*) (duration nil) (dur nil) &allow-other-keys)
  ;; FIX: handle :set events, :mono, etc
  (when (and duration dur)
    (warn "Both DURATION and DUR were provided to `as-supercollider-score'; only DURATION will be used"))
  (let ((*clock* (make-clock tempo))
        (instruments (list))
        (result-events (list))
        (duration (or duration
                      (dur-time (or dur (last-dur events))
                                tempo)))
        (node-id 999))
    ;; process events
    (dolist* (idx event events)
      (unless (rest-p event)
        (let* ((ev-beat (beat event))
               (ev-time (float (dur-time (if dur (min ev-beat dur) ev-beat)
                                         tempo)
                               0d0))
               (instrument (instrument event))
               (cur-node (incf node-id)))
          (pushnew instrument instruments :test #'string-equal)
          (when (or (and duration ; cut off events that are outside DURATION/DUR or MAX-LENGTH
                         (> ev-time duration))
                    (> idx max-length))
            (return))
          (push (list ev-time
                      (append (list "/s_new"
                                    (string-downcase instrument)
                                    cur-node
                                    (or (event-value event :add-action) 0)
                                    (event-value event :group))
                              (loop :for (k v) :on (backend-instrument-args-list backend instrument event) :by #'cddr
                                    :collect (string-downcase k)
                                    :collect (typecase v
                                               (integer v)
                                               (number (coerce v 'single-float))
                                               (t v)))))
                result-events)
          (when (backend-instrument-has-gate-p backend instrument)
            (let ((end-beat (+ ev-beat (sustain event))))
              (push (list (float (dur-time (if dur
                                               (min end-beat dur)
                                               end-beat)
                                           tempo)
                                 0d0)
                          (list "/n_set" cur-node "gate" 0))
                    result-events))))))
    (append
     ;; create default group
     (list (list 0d0 (list "/g_new" 1 0 0)))
     ;; load instrument definitions
     (mapcar (lambda (instrument)
               ;; ensure synthdefs are written to disk so that the server can load them in NRT mode.
               (write-synthdef-file instrument)
               (list 0d0 (list "/d_load" (string-downcase instrument))))
             instruments)
     ;; sorted events
     (sort result-events #'< :key #'car)
     ;; last event in the score determines total output duration
     (when duration
       (list (list (float duration 0d0) (list "/c_set" 0 0)))))))

(defmethod as-supercollider-score (object &rest args &key (tempo (tempo *clock*)) (max-length *max-pattern-yield-length*) (duration nil) (dur nil) &allow-other-keys)
  (declare (ignore duration dur))
  (let ((*clock* (make-clock tempo)))
    (apply #'as-supercollider-score (next-upto-n object max-length) args)))

(defun supercollider-score-sclang-code (score &optional (stream t)) ; FIX: not exported or used anywhere
  "Write SCORE to STREAM as sclang code.

See also: `as-supercollider-score', `supercollider-score-write-encoded'"
  (format stream "[~%")
  (dolist (item score)
    (format stream "  [~F, [~{~S, ~}]],~%" (first item) (second item)))
  (format stream "]~%"))

(defun supercollider-score-write-encoded (score stream)
  "Write SCORE as an encoded score to STREAM. Note that the score's events must be in order based on start time, and all start times must be double floats. Additionally, all instrument parameters must be either integers or single floats."
  (dolist (bundle score)
    (let ((msg (sc-osc::encode-bundle (cadr bundle) (- (car bundle) osc::+unix-epoch+))))
      (write-sequence (osc::encode-int32 (length msg)) stream)
      (write-sequence msg stream))))

(defmethod render (object (output (eql :supercollider-file)) &rest args &key (backend (find-backend 'supercollider)) output-filename (channels 2) sample-rate (sample-format :int24))
  (check-type output-filename pathname-designator)
  (check-type channels (integer 1))
  (check-type sample-format (member :int16 :int24 :int32 :float :double))
  (let ((score (typecase object
                 (supercollider-score object)
                 (list (if (event-p (car object))
                           (apply #'as-supercollider-score object args)
                           (if (and (numberp (caar object))
                                    (listp (cadr object)))
                               object
                               (error "OBJECT does not appear to be a list of events or a score"))))
                 (t (apply #'as-supercollider-score object args))))
        (sample-rate (or sample-rate
                         (let ((s-sr (cl-collider::server-options-hardware-samplerate
                                      (cl-collider::server-options
                                       (backend-server backend)))))
                           (unless (zerop s-sr)
                             s-sr))
                         48000))
        (osc-bin-file (generate-temporary-file-name :directory "/tmp/cl-patterns/osc/"
                                                    :extension "osc"))
        (extension (pathname-type output-filename)))
    (with-open-file (stream osc-bin-file :direction :output :element-type '(unsigned-byte 8)
                                         :if-exists :rename-and-delete :if-does-not-exist :create)
      (supercollider-score-write-encoded score stream))
    (multiple-value-bind (standard-output error-output return-value)
        (uiop:run-program (list "scsynth"
                                "-o" (write-to-string channels) ; number of channels
                                "-N" ; non-realtime rendering
                                osc-bin-file ; OSC command file
                                "_" ; input audio file (underscore means none)
                                output-filename ; output audio file
                                (write-to-string sample-rate) ; sample rate
                                (if (find extension '(wav aiff) :test #'string-equal) ; header format
                                    extension
                                    "WAV")
                                (string-downcase sample-format)) ; sample format
                          :ignore-error-status t
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t))
      (if (zerop return-value)
          output-filename
          (values nil standard-output error-output return-value)))))

(defmethod render (object (output (eql :supercollider-buffer)) &rest args &key &allow-other-keys)
  (let ((wav-file-name (generate-temporary-file-name
                        :directory (namestring (merge-pathnames "wav/" *cl-patterns-temporary-directory*))
                        :extension "wav")))
    (apply #'render object wav-file-name args)
    (cl-collider:buffer-read wav-file-name)))

(defmethod render (object (output (eql :supercollider-score)) &rest args &key &allow-other-keys)
  (apply #'as-supercollider-score object args))
