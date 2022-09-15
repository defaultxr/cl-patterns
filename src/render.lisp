;;;; render.lisp - generic functionality for rendering patterns.
;; FIX: doesn't work when the path is relative

(in-package #:cl-patterns)

(defvar *cl-patterns-temporary-directory*
  (merge-pathnames "cl-patterns/" (uiop:temporary-directory))
  "The default directory to store `render'ed files in.")

;; FIX: implement padding-duration key
(defgeneric render (object output &key backend tempo max-length max-pattern-yield-length duration max-duration max-output-duration &allow-other-keys)
  (:documentation "Render a pattern or other object as audio or other format. OUTPUT is what the pattern should be rendered as. It can be one of the following values:

- A string or pathname - Output file name (file format is determined by the file extension).
- :file - Render to a file in the `*cl-patterns-temporary-directory*'.
- :buffer - Render to a buffer in memory.
- :bdef - Render to a `bdef:bdef' buffer object in memory. This type if the bdef library is loaded. Falls back to :buffer if bdef is not loaded.
- :score - Render as a \"score\" in memory, i.e. the sequence type supported by the backend.
- :eseq - Make an `eseq' from the pattern. Effectively defers to `as-eseq'.
- :pstream - Make a `pstream' from the pattern. Effectively defers to `as-pstream'.
- :list - Get a list of up to the first MAX-LENGTH outputs from this object using `next-upto-n'.

The following additional keyword arguments are also supported, depending on the output type:

- BACKEND - The backend to render with. Defaults to the first applicable backend, based on (in descending order of priority): the first event's :backend key, the first event's :instrument, or the first enabled backend that supports the specified output type.
- OUTPUT-FILENAME - The filename to write the rendered file to (when rendering to a file).
- TEMPO - The tempo of the result in beats per second. Defaults to `*clock*''s current tempo.
- MAX-LENGTH - Maximum number of outputs to grab from the source pattern. Must be an integer (cannot be :inf). Defaults to `*max-pattern-yield-length*'.
- DURATION - The total duration of the output in seconds.
- MAX-DURATION - The maximum duration of the output in seconds. Defaults to infinite, in which case the pattern is limited by MAX-PATTERN-YIELD-LENGTH.

See also: `as-eseq'"))

(defmethod render (object (output (eql :eseq)) &key)
  (as-eseq object))

(defmethod render (object (output (eql :pstream)) &key)
  (as-pstream object))

(defmethod render (object (output (eql :list)) &key (max-length *max-pattern-yield-length*))
  (next-upto-n object max-length))

(defun find-backend-supporting-render (render-type) ; FIX: detect the backend from the input events?
  "Get the output and backend names of the first enabled backend that supports RENDER-TYPE (i.e. :buffer, :file, :score, etc), or nil if none support it.

See also: `render'"
  (dolist (backend (enabled-backends))
    (let ((sym (upcase-intern (concat backend '- render-type) :keyword)))
      (when (find-method #'render nil (list t (list 'eql sym)) nil)
        (return-from find-backend-supporting-render (values sym backend))))))

;; make default `render' methods for buffer, file, and score
#.`(progn
     ,@(mapcar (lambda (type)
                 `(defmethod render (object (output (eql ,type)) &rest args &key &allow-other-keys)
                    (when-let ((backend (getf args :backend)))
                      (if-let ((backend (find-backend backend :enabled-p t)))
                        (return-from render
                          (apply #'render object (make-keyword (concat (class-name (class-of backend)) '- ,type))
                                 args))
                        (error "No enabled backend found with name or type ~S" backend)))
                    (when-let* ((first-event (typecase object
                                               (pattern (next object))
                                               (event object)
                                               (list (let ((first (first object)))
                                                       (when (typep first 'event)
                                                         first)))))
                                (backend (or (event-value first-event :backend)
                                             (when-let ((instrument (event-value first-event :instrument)))
                                               (dolist (backend (enabled-backends))
                                                 (when (backend-instrument-controls backend instrument)
                                                   (return backend)))))))
                      (return-from render
                        (apply #'render object
                               (make-keyword (concat (class-name (class-of (find-backend backend))) '- ,type))
                               args)))
                    (when-let ((backend (find-backend-supporting-render ,type)))
                      (return-from render
                        (apply #'render object backend args)))
                    (error "No enabled backend supports rendering as ~S." ,type)))
               (list :buffer :file :score)))

(defmethod render (object (output-filename string) &rest args &key &allow-other-keys)
  (apply #'render object :file :output-filename output-filename args))

(defmethod render (object (output-filename pathname) &rest args &key &allow-other-keys)
  (apply #'render object (namestring output-filename) args))

(defmethod render ((event event) output &rest args &key &allow-other-keys)
  ;; if the user wants to render a lone event without an explicitly-set beat, we assume they just want the event without its `beat' offset.
  ;; if the user is rendering multiple "tracks" then they will be provided as lists of events or as a pstream, pattern, etc, in which case we don't remove the `beat'.
  (apply #'render (if (eql t (nth-value 1 (beat event)))
                      (list (combine-events event (event :beat 0)))
                      (list event))
         output args))
