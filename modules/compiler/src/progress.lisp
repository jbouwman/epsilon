;;;; progress.lisp - Compilation progress events
;;;;
;;;; Provides real-time compilation progress events with multiple
;;;; handler implementations for console, JSON, and IDE integration.

(defpackage :epsilon.compiler.progress
  (:use :cl)
  (:local-nicknames
   (:diag :epsilon.compiler.diagnostics))
  (:export
   ;; Event kinds
   #:event-kind

   ;; Compilation events
   #:compilation-event
   #:make-compilation-event
   #:compilation-event-p
   #:compilation-event-kind
   #:compilation-event-timestamp
   #:compilation-event-file
   #:compilation-event-form
   #:compilation-event-form-number
   #:compilation-event-total-forms
   #:compilation-event-phase
   #:compilation-event-message
   #:compilation-event-data

   ;; Event emission
   #:emit-progress
   #:*progress-handler*
   #:with-progress-handler

   ;; Progress handlers
   #:console-progress-handler
   #:json-progress-handler
   #:silent-progress-handler
   #:callback-progress-handler

   ;; Progress aggregation
   #:progress-aggregator
   #:make-progress-aggregator
   #:progress-aggregator-p
   #:progress-aggregator-files-total
   #:progress-aggregator-files-completed
   #:progress-aggregator-forms-processed
   #:progress-aggregator-functions-compiled
   #:progress-aggregator-macros-expanded
   #:progress-aggregator-errors
   #:progress-aggregator-warnings
   #:with-progress-aggregator
   #:progress-summary

   ;; LSP integration
   #:lsp-progress-handler
   #:make-lsp-work-done-progress))

(in-package :epsilon.compiler.progress)

;;; Utility macro (must be defined before use)

(defmacro when-let (bindings &body body)
  "Bind VAR to FORM and execute BODY if non-nil.
   BINDINGS is ((var form))."
  (destructuring-bind ((var form)) bindings
    `(let ((,var ,form))
       (when ,var ,@body))))

;;; Event kinds

(deftype event-kind ()
  '(member
    ;; File-level events
    :compilation-started
    :compilation-finished
    :file-started
    :file-finished

    ;; Form-level events
    :form-started
    :form-finished
    :macro-expanded
    :function-compiled

    ;; Phase events
    :phase-started
    :phase-finished

    ;; Output events
    :fasl-written
    :diagnostic-emitted))

;;; Compilation event structure

(defstruct (compilation-event
            (:constructor %make-compilation-event)
            (:copier nil))
  "An event during compilation."
  (kind :info :type event-kind)
  (timestamp (get-internal-real-time) :type integer)
  (file nil :type (or pathname string null))
  (form nil :type t)
  (form-number nil :type (or integer null))
  (total-forms nil :type (or integer null))
  (phase nil :type (or keyword null))
  (message nil :type (or string null))
  (data nil :type t))

(defun make-compilation-event (&key (kind :info)
                                    (timestamp (get-internal-real-time))
                                    file
                                    form
                                    form-number
                                    total-forms
                                    phase
                                    message
                                    data)
  "Create a compilation event."
  (%make-compilation-event
   :kind kind
   :timestamp timestamp
   :file file
   :form form
   :form-number form-number
   :total-forms total-forms
   :phase phase
   :message message
   :data data))

;;; Event emission

(defvar *progress-handler* nil
  "Current progress event handler function.")

(defun emit-progress (kind &rest args &key file form form-number total-forms
                                           phase message data)
  "Emit a progress event.

   Example:
   (emit-progress :file-started :file #p\"foo.lisp\")
   (emit-progress :form-finished :form-number 42 :total-forms 100)"
  (declare (ignore file form form-number total-forms phase message data))
  (when *progress-handler*
    (let ((event (apply #'make-compilation-event :kind kind args)))
      (funcall *progress-handler* event)))
  (values))

(defmacro with-progress-handler (handler &body body)
  "Execute BODY with progress events sent to HANDLER.

   HANDLER should be a function that takes a compilation-event."
  `(let ((*progress-handler* ,handler))
     ,@body))

;;; Console progress handler

(defun console-progress-handler (&key (stream *standard-output*)
                                      (verbose nil))
  "Create a progress handler that writes to console.

   Returns a function suitable for use with with-progress-handler.

   Example:
   (with-progress-handler (console-progress-handler :verbose t)
     (compile-source \"foo.lisp\"))"
  (let ((current-file nil)
        (file-count 0)
        (total-files nil)
        (start-time nil))
    (lambda (event)
      (ecase (compilation-event-kind event)
        (:compilation-started
         (setf start-time (get-internal-real-time))
         (setf total-files (getf (compilation-event-data event) :file-count))
         (setf file-count 0)
         (format stream "~&Compiling~@[ ~D file~:P~]...~%"
                 total-files))

        (:file-started
         (incf file-count)
         (setf current-file (compilation-event-file event))
         (when verbose
           (format stream "  [~D~@[/~D~]] ~A~%"
                   file-count total-files
                   (if current-file
                       (file-namestring current-file)
                       "<unknown>"))))

        (:file-finished
         (unless verbose
           (write-char #\. stream)
           (force-output stream)))

        (:compilation-finished
         (let ((elapsed (when start-time
                          (/ (- (get-internal-real-time) start-time)
                             internal-time-units-per-second))))
           (format stream "~&Done~@[ in ~,2F seconds~].~%"
                   elapsed)))

        ((:form-started :form-finished)
         ;; Show progress for verbose mode
         (when (and verbose
                    (compilation-event-form-number event)
                    (compilation-event-total-forms event))
           (format stream "    Form ~D/~D~%"
                   (compilation-event-form-number event)
                   (compilation-event-total-forms event))))

        (:diagnostic-emitted
         ;; Just note that a diagnostic was emitted
         (when verbose
           (format stream "    [diagnostic]~%")))

        ((:macro-expanded :function-compiled :phase-started
          :phase-finished :fasl-written)
         ;; Ignore detailed events in console mode by default
         nil)))))

;;; JSON progress handler

(defun json-progress-handler (&key (stream *standard-output*))
  "Create a progress handler that emits newline-delimited JSON events.

   Suitable for parsing by build tools and IDEs."
  (lambda (event)
    (let ((json (event-to-json event)))
      (format stream "~A~%" json)
      (force-output stream))))

(defun event-to-json (event)
  "Convert a compilation event to JSON."
  (with-output-to-string (s)
    (format s "{")
    (format s "\"kind\":\"~A\"" (string-downcase (compilation-event-kind event)))
    (format s ",\"timestamp\":~D" (compilation-event-timestamp event))
    (when (compilation-event-file event)
      (format s ",\"file\":\"~A\""
              (diag::escape-json-string
               (namestring (compilation-event-file event)))))
    (when (compilation-event-form-number event)
      (format s ",\"formNumber\":~D" (compilation-event-form-number event)))
    (when (compilation-event-total-forms event)
      (format s ",\"totalForms\":~D" (compilation-event-total-forms event)))
    (when (compilation-event-phase event)
      (format s ",\"phase\":\"~A\""
              (string-downcase (compilation-event-phase event))))
    (when (compilation-event-message event)
      (format s ",\"message\":\"~A\""
              (diag::escape-json-string (compilation-event-message event))))
    (format s "}")))

;;; Silent progress handler

(defun silent-progress-handler ()
  "Create a progress handler that discards all events.

   Useful when you want to suppress all progress output."
  (lambda (event)
    (declare (ignore event))
    nil))

;;; Callback progress handler

(defun callback-progress-handler (callback &key (filter nil))
  "Create a progress handler that invokes CALLBACK for each event.

   FILTER, if provided, should be a function that takes an event-kind
   and returns true if that event should be passed to the callback."
  (lambda (event)
    (when (or (null filter)
              (funcall filter (compilation-event-kind event)))
      (funcall callback event))))

;;; Progress aggregation

(defstruct (progress-aggregator
            (:constructor %make-progress-aggregator)
            (:copier nil))
  "Aggregates progress events into a summary."
  (files-total 0 :type (integer 0))
  (files-completed 0 :type (integer 0))
  (forms-processed 0 :type (integer 0))
  (functions-compiled 0 :type (integer 0))
  (macros-expanded 0 :type (integer 0))
  (errors 0 :type (integer 0))
  (warnings 0 :type (integer 0))
  (start-time 0 :type integer)
  (end-time nil :type (or integer null))
  (phase-times (make-hash-table :test 'eq) :type hash-table))

(defun make-progress-aggregator ()
  "Create a progress aggregator."
  (%make-progress-aggregator
   :start-time (get-internal-real-time)
   :phase-times (make-hash-table :test 'eq)))

(defun make-aggregating-handler (aggregator)
  "Create a handler that aggregates events into AGGREGATOR."
  (lambda (event)
    (ecase (compilation-event-kind event)
      (:compilation-started
       (when-let ((count (getf (compilation-event-data event) :file-count)))
         (setf (progress-aggregator-files-total aggregator) count)))

      (:file-finished
       (incf (progress-aggregator-files-completed aggregator)))

      (:form-finished
       (incf (progress-aggregator-forms-processed aggregator)))

      (:function-compiled
       (incf (progress-aggregator-functions-compiled aggregator)))

      (:macro-expanded
       (incf (progress-aggregator-macros-expanded aggregator)))

      (:diagnostic-emitted
       (let ((diagnostic (getf (compilation-event-data event) :diagnostic)))
         (when diagnostic
           (case (diag:diagnostic-severity diagnostic)
             (:error (incf (progress-aggregator-errors aggregator)))
             ((:warning :style-warning)
              (incf (progress-aggregator-warnings aggregator)))))))

      (:phase-started
       (when (compilation-event-phase event)
         (setf (gethash (compilation-event-phase event)
                        (progress-aggregator-phase-times aggregator))
               (get-internal-real-time))))

      (:phase-finished
       (when (compilation-event-phase event)
         (let ((start (gethash (compilation-event-phase event)
                               (progress-aggregator-phase-times aggregator))))
           (when start
             (setf (gethash (compilation-event-phase event)
                            (progress-aggregator-phase-times aggregator))
                   (- (get-internal-real-time) start))))))

      (:compilation-finished
       (setf (progress-aggregator-end-time aggregator)
             (get-internal-real-time)))

      ((:file-started :form-started :fasl-written)
       nil))))

(defmacro with-progress-aggregator ((var) &body body)
  "Execute BODY collecting progress into aggregator VAR."
  `(let ((,var (make-progress-aggregator)))
     (with-progress-handler (make-aggregating-handler ,var)
       ,@body)
     ,var))

(defun progress-summary (aggregator)
  "Generate a summary plist from aggregated progress."
  (list :files-total (progress-aggregator-files-total aggregator)
        :files-completed (progress-aggregator-files-completed aggregator)
        :forms-processed (progress-aggregator-forms-processed aggregator)
        :functions-compiled (progress-aggregator-functions-compiled aggregator)
        :macros-expanded (progress-aggregator-macros-expanded aggregator)
        :errors (progress-aggregator-errors aggregator)
        :warnings (progress-aggregator-warnings aggregator)
        :elapsed-seconds (when (progress-aggregator-end-time aggregator)
                           (/ (- (progress-aggregator-end-time aggregator)
                                 (progress-aggregator-start-time aggregator))
                              internal-time-units-per-second))))

;;; LSP integration

(defun lsp-progress-handler (token callback)
  "Create a progress handler for Language Server Protocol.

   TOKEN is the progress token for work done progress.
   CALLBACK is called with LSP progress notification objects."
  (let ((started nil))
    (lambda (event)
      (let ((lsp-event (event-to-lsp-progress event token started)))
        (when lsp-event
          (when (eq (compilation-event-kind event) :compilation-started)
            (setf started t))
          (funcall callback lsp-event))))))

(defun make-lsp-work-done-progress (token kind &key title message percentage)
  "Create an LSP WorkDoneProgress notification.

   KIND is one of :begin, :report, :end."
  (list :token token
        :value (list* :kind (string-downcase kind)
                      (when title (list :title title))
                      (when message (list :message message))
                      (when percentage (list :percentage percentage)))))

(defun event-to-lsp-progress (event token started)
  "Convert a compilation event to LSP progress notification."
  (case (compilation-event-kind event)
    (:compilation-started
     (make-lsp-work-done-progress
      token :begin
      :title "Compiling"
      :message (when-let ((file (compilation-event-file event)))
                 (file-namestring file))
      :percentage 0))

    (:file-started
     (when started
       (let* ((num (compilation-event-form-number event))
              (total (compilation-event-total-forms event))
              (pct (when (and num total (plusp total))
                     (round (* 100 (/ num total))))))
         (make-lsp-work-done-progress
          token :report
          :message (when-let ((file (compilation-event-file event)))
                     (file-namestring file))
          :percentage pct))))

    (:compilation-finished
     (make-lsp-work-done-progress token :end))

    (otherwise nil)))

