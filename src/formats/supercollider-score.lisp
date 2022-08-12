(in-package #:cl-patterns)

;;;; supercollider-score.lisp - support for importing/exporting SuperCollider score files.
;;; Primarily for non-realtime (NRT) synthesis; also used by the `render' method to implement pattern rendering.

;;; NOTES:
;; http://doc.sccode.org/Classes/Score.html

(defclass supercollider-score ()
  ((list :initarg :list :accessor score-list :documentation "The list of directives in the score."))
  (:documentation "A list of instructions for the SuperCollider server to `render' in non-realtime."))

(defun write-synthdef-file (synth)
  "Helper function to write the synthdef file for SYNTH to the synthdef path.

See also: `as-score', `render', `write-encoded-score'"
  (let ((cl-collider::*synth-definition-mode* :load)
        (meta (cl-collider:synthdef-metadata synth)))
    (if meta
        (eval `(cl-collider:defsynth ,synth ,(getf meta :controls)
                 ,@(getf meta :body)))
        (cerror "Couldn't find metadata for a synthdef with name ~S." synth))))

(defgeneric as-score (object &key tempo dur max-length)
  (:documentation "Convert an object into score format.

See also: `render', `write-encoded-score'"))

(defmethod as-score ((events list) &key (tempo (tempo *clock*)) (dur nil dur-provided-p) (max-length *max-pattern-yield-length*) (backend (find-backend 'supercollider)))
  ;; FIX: handle :set events, :mono, etc
  (declare (ignore max-length))
  (let ((instruments (remove-duplicates (mapcar #'instrument events)))
        (gen-events (list))
        (dur (if dur-provided-p
                 dur
                 (last-dur events)))
        (node-id 999))
    (append
     ;; create default group
     (list (list 0d0 (list "/g_new" 1 0 0)))
     ;; load instruments (and make sure their definitions are written)
     (loop :for inst :in instruments
           :collect (list 0d0 (list "/d_load" (string-downcase inst)))
           :do (write-synthdef-file inst))
     ;; insert events
     (dolist (event events)
       (unless (rest-p event)
         (let ((ebeat (beat event))
               (inst (instrument event))
               (cur-node (incf node-id)))
           (push (list (float (dur-time (if dur
                                            (min ebeat dur)
                                            ebeat)
                                        tempo)
                              0d0)
                       (append (list "/s_new"
                                     (string-downcase inst)
                                     cur-node
                                     (or (event-value event :add-action) 0)
                                     (event-value event :group))
                               (loop :for (k v) :on (backend-instrument-args-list backend inst event) :by #'cddr
                                     :append (list (string-downcase k)
                                                   (typecase v
                                                     (integer v)
                                                     (number (coerce v 'single-float))
                                                     (t v))))))
                 gen-events)
           (when (backend-instrument-has-gate-p backend inst)
             (let ((end-beat (+ ebeat (sustain event))))
               (push (list (float (dur-time (if dur
                                                (min end-beat dur)
                                                end-beat)
                                            tempo)
                                  0d0)
                           (list "/n_set" cur-node "gate" 0))
                     gen-events))))))
     (sort gen-events #'< :key #'car)
     ;; add last event to set output length
     (when dur
       (list (list (float dur 0d0) (list "/c_set" 0 0)))))))

(defmethod as-score ((pattern pattern) &rest args &key (tempo (tempo *clock*)) (dur (dur pattern)) (max-length *max-pattern-yield-length*) &allow-other-keys)
  (apply #'as-score (next-upto-n pattern max-length) args))

(defun score-as-sclang-code (score &optional (stream t))
  "Write SCORE to STREAM as sclang code.

See also: `as-score', `write-encoded-score'"
  (format stream "[~%")
  (dolist (item score)
    (format stream "  [~F, [~{~S, ~}]],~%" (first item) (second item)))
  (format stream "]~%"))

(defun write-encoded-score (score stream)
  "Write SCORE as an encoded score to STREAM. Note that the score's events must be in order based on start time, and all start times must be double floats. Additionally, all instrument parameters must be either integers or single floats."
  (dolist (bundle score)
    (let ((msg (sc-osc::encode-bundle (cadr bundle) (- (car bundle) osc::+unix-epoch+))))
      (write-sequence (osc::encode-int32 (length msg)) stream)
      (write-sequence msg stream))))

(defmethod render ((list list) (filename string) &rest args &key sample-rate (sample-format :int24))
  (assert (member sample-format (list :int16 :int24 :int32 :float :double)) (sample-format))
  (when (event-p (car list))
    (return-from render (apply #'render (as-score list) filename args)))
  (let ((sample-rate (or sample-rate
                         (let ((s-sr (cl-collider::server-options-hardware-samplerate
                                      (cl-collider::server-options cl-collider:*s*))))
                           (unless (zerop s-sr)
                             s-sr))
                         48000))
        (osc-bin-file (generate-temporary-file-name :directory "/tmp/cl-patterns/osc/"
                                                    :extension "osc"))
        (extension (pathname-type filename)))
    (with-open-file (stream osc-bin-file :direction :output :element-type '(unsigned-byte 8)
                                         :if-exists :rename-and-delete :if-does-not-exist :create)
      (write-encoded-score list stream))
    (let ((result (multiple-value-list
                   (uiop:run-program (list "scsynth"
                                           "-o" "2" ;; 2 output channels
                                           "-N" ;; non-realtime rendering
                                           osc-bin-file ;; OSC command file
                                           "_" ;; input audio file (underscore means none)
                                           filename ;; output audio file
                                           (write-to-string sample-rate) ;; sample rate
                                           (if (position extension (list "wav" "aiff") :test #'string-equal) ;; header format
                                               extension
                                               "WAV")
                                           (string-downcase (symbol-name sample-format))) ;; sample format
                                     :ignore-error-status t
                                     :output (list :string :stripped t)
                                     :error-output (list :string :stripped t)))))
      (apply #'values (if (zerop (third result))
                          filename
                          nil)
             result))))

(defmethod render ((event event) output &rest args &key &allow-other-keys)
  ;; if the user wants to render a lone event without an explicitly-set beat, we assume they just want the event without its `beat' offset.
  ;; if the user is rendering multiple "tracks" then they will be provided as lists of events or as a pstream, pattern, etc, in which case we don't remove the `beat'.
  (apply #'render
         (as-score (if (eql t (nth-value 1 (beat event)))
                       (list (combine-events event (event :beat 0)))
                       (list event)))
         output
         args))

(defmethod render ((pattern pattern) output &rest args &key &allow-other-keys)
  (apply #'render (as-score pattern) output args))

(defmethod render ((pattern pattern) (output (eql :score)) &rest args &key &allow-other-keys)
  (apply #'as-score pattern args))

(defmethod render (object (output (eql :supercollider)) &rest args &key &allow-other-keys)
  (let ((wav-file-name (generate-temporary-file-name
                        :directory (namestring (merge-pathnames "wav/" *cl-patterns-temporary-directory*))
                        :extension "wav")))
    (apply #'render object wav-file-name args)
    (cl-collider:buffer-read wav-file-name)))
