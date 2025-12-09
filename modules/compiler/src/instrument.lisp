;;;; instrument.lisp - Code instrumentation for coverage, profiling, tracing
;;;;
;;;; Provides compile-time instrumentation support for code analysis tools.

(defpackage :epsilon.compiler.instrument
  (:use :cl)
  (:export
   ;; Instrumentation options
   #:instrumentation-options
   #:make-instrumentation-options
   #:instrumentation-options-p
   #:instrumentation-options-mode
   #:instrumentation-options-granularity
   #:instrumentation-options-output-file
   #:instrumentation-options-filter

   ;; Instrumentation modes
   #:+coverage+
   #:+profile+
   #:+trace+

   ;; Form instrumentation
   #:instrument-form
   #:instrument-function-body

   ;; Coverage support
   #:*coverage-data*
   #:coverage-hit
   #:coverage-report
   #:reset-coverage

   ;; Profiling support
   #:*profile-data*
   #:with-profiling-context
   #:profile-report
   #:reset-profile

   ;; Tracing support
   #:*trace-data*
   #:with-trace-context
   #:trace-report
   #:reset-trace

   ;; Compiled instrumented code
   #:compile-source-instrumented))

(in-package :epsilon.compiler.instrument)

;;; Instrumentation modes

(defconstant +coverage+ :coverage
  "Coverage instrumentation mode.")

(defconstant +profile+ :profile
  "Profiling instrumentation mode.")

(defconstant +trace+ :trace
  "Tracing instrumentation mode.")

;;; Instrumentation options

(defstruct (instrumentation-options
            (:constructor %make-instrumentation-options)
            (:copier nil))
  "Options for code instrumentation."
  (mode nil :type (or null (member :coverage :profile :trace)))
  (granularity :function :type (member :function :expression :branch))
  (output-file nil :type (or pathname null))
  (filter nil :type (or function null)))

(defun make-instrumentation-options (&key mode
                                          (granularity :function)
                                          output-file
                                          filter)
  "Create instrumentation options.

   MODE: :coverage, :profile, or :trace
   GRANULARITY: :function, :expression, or :branch
   OUTPUT-FILE: Where to write instrumentation data
   FILTER: Function (form) -> boolean to select forms to instrument"
  (%make-instrumentation-options
   :mode mode
   :granularity granularity
   :output-file output-file
   :filter filter))

;;; Coverage data

(defvar *coverage-data* (make-hash-table :test 'equal)
  "Hash table mapping coverage points to hit counts.")

(defvar *coverage-points* (make-hash-table :test 'equal)
  "Hash table mapping coverage point IDs to source locations.")

(defun coverage-hit (point-id)
  "Record a coverage hit for POINT-ID."
  (incf (gethash point-id *coverage-data* 0)))

(defun reset-coverage ()
  "Reset all coverage data."
  (clrhash *coverage-data*))

(defun coverage-report (&key (stream *standard-output*))
  "Generate a coverage report."
  (format stream "~&Coverage Report~%")
  (format stream "===============~%~%")
  (let ((total 0)
        (covered 0))
    (maphash (lambda (point-id location)
               (declare (ignore location))
               (incf total)
               (when (plusp (gethash point-id *coverage-data* 0))
                 (incf covered)))
             *coverage-points*)
    (format stream "Total points: ~D~%" total)
    (format stream "Covered: ~D~%" covered)
    (when (plusp total)
      (format stream "Coverage: ~,1F%~%" (* 100 (/ covered total))))))

;;; Profiling data

(defstruct profile-entry
  "Entry in the profile data."
  (call-count 0 :type integer)
  (total-time 0 :type integer)
  (self-time 0 :type integer)
  (max-time 0 :type integer)
  (min-time most-positive-fixnum :type integer))

(defvar *profile-data* (make-hash-table :test 'equal)
  "Hash table mapping function IDs to profile entries.")

(defvar *profile-stack* nil
  "Stack of currently profiling functions.")

(defmacro with-profiling-context (function-id &body body)
  "Execute BODY while profiling under FUNCTION-ID."
  (let ((start (gensym "START"))
        (end (gensym "END"))
        (elapsed (gensym "ELAPSED"))
        (entry (gensym "ENTRY"))
        (fid (gensym "FID")))
    `(let* ((,fid ,function-id)
            (,start (get-internal-real-time))
            (,entry (or (gethash ,fid *profile-data*)
                        (setf (gethash ,fid *profile-data*)
                              (make-profile-entry)))))
       (incf (profile-entry-call-count ,entry))
       (push ,fid *profile-stack*)
       (unwind-protect
            (progn ,@body)
         (pop *profile-stack*)
         (let* ((,end (get-internal-real-time))
                (,elapsed (- ,end ,start)))
           (incf (profile-entry-total-time ,entry) ,elapsed)
           (setf (profile-entry-max-time ,entry)
                 (max (profile-entry-max-time ,entry) ,elapsed))
           (setf (profile-entry-min-time ,entry)
                 (min (profile-entry-min-time ,entry) ,elapsed)))))))

(defun reset-profile ()
  "Reset all profiling data."
  (clrhash *profile-data*)
  (setf *profile-stack* nil))

(defun profile-report (&key (stream *standard-output*) (sort-by :total-time))
  "Generate a profiling report."
  (format stream "~&Profile Report~%")
  (format stream "==============~%~%")
  (format stream "~40A ~10A ~12A ~12A~%"
          "Function" "Calls" "Total (ms)" "Avg (ms)")
  (format stream "~40,,,'-A ~10,,,'-A ~12,,,'-A ~12,,,'-A~%"
          "" "" "" "")

  (let ((entries '()))
    (maphash (lambda (id entry)
               (push (cons id entry) entries))
             *profile-data*)

    ;; Sort entries
    (setf entries
          (sort entries
                (ecase sort-by
                  (:total-time
                   (lambda (a b)
                     (> (profile-entry-total-time (cdr a))
                        (profile-entry-total-time (cdr b)))))
                  (:call-count
                   (lambda (a b)
                     (> (profile-entry-call-count (cdr a))
                        (profile-entry-call-count (cdr b))))))))

    ;; Print entries
    (dolist (entry entries)
      (let* ((id (car entry))
             (data (cdr entry))
             (calls (profile-entry-call-count data))
             (total-ms (/ (profile-entry-total-time data)
                          (/ internal-time-units-per-second 1000)))
             (avg-ms (if (plusp calls) (/ total-ms calls) 0)))
        (format stream "~40A ~10D ~12,2F ~12,2F~%"
                id calls total-ms avg-ms)))))

;;; Tracing data

(defstruct trace-entry
  "Entry in the trace log."
  (timestamp 0 :type integer)
  (function-id nil :type t)
  (event :enter :type (member :enter :exit :error))
  (depth 0 :type integer)
  (args nil :type list)
  (result nil :type t))

(defvar *trace-data* nil
  "List of trace entries (in reverse order).")

(defvar *trace-depth* 0
  "Current trace nesting depth.")

(defvar *trace-enabled* t
  "Whether tracing is currently enabled.")

(defmacro with-trace-context (function-id &body body)
  "Execute BODY while tracing under FUNCTION-ID."
  (let ((result (gensym "RESULT"))
        (fid (gensym "FID")))
    `(let ((,fid ,function-id))
       (when *trace-enabled*
         (push (make-trace-entry
                :timestamp (get-internal-real-time)
                :function-id ,fid
                :event :enter
                :depth *trace-depth*)
               *trace-data*))
       (let ((*trace-depth* (1+ *trace-depth*)))
         (let ((,result (multiple-value-list (progn ,@body))))
           (when *trace-enabled*
             (push (make-trace-entry
                    :timestamp (get-internal-real-time)
                    :function-id ,fid
                    :event :exit
                    :depth (1- *trace-depth*)
                    :result (car ,result))
                   *trace-data*))
           (values-list ,result))))))

(defun reset-trace ()
  "Reset all tracing data."
  (setf *trace-data* nil)
  (setf *trace-depth* 0))

(defun trace-report (&key (stream *standard-output*) (max-entries 100))
  "Generate a trace report."
  (format stream "~&Trace Report~%")
  (format stream "============~%~%")

  (let ((entries (reverse *trace-data*))
        (count 0))
    (dolist (entry entries)
      (when (>= count max-entries)
        (format stream "... (~D more entries)~%"
                (- (length entries) max-entries))
        (return))
      (let ((depth (trace-entry-depth entry))
            (event (trace-entry-event entry))
            (fid (trace-entry-function-id entry)))
        (format stream "~V@T~A ~A~@[ => ~S~]~%"
                (* 2 depth)
                (ecase event
                  (:enter ">")
                  (:exit "<")
                  (:error "!"))
                fid
                (when (eq event :exit)
                  (trace-entry-result entry))))
      (incf count))))

;;; Form instrumentation

(defun form-identifier (form)
  "Generate a unique identifier for a form."
  (cond
    ((and (consp form)
          (member (car form) '(defun defmethod defmacro)))
     (second form))
    ((and (consp form)
          (eq (car form) 'lambda))
     (gensym "LAMBDA"))
    (t
     (gensym "FORM"))))

(defun instrument-form (form mode)
  "Wrap FORM with instrumentation for MODE.

   MODE is one of :coverage, :profile, or :trace."
  (let ((id (form-identifier form)))
    (ecase mode
      (:coverage
       ;; Record that this point was executed
       (let ((point-id (gensym "COV")))
         (setf (gethash point-id *coverage-points*)
               (list :form form))
         `(progn
            (coverage-hit ',point-id)
            ,form)))

      (:profile
       ;; Wrap with timing
       `(with-profiling-context ',id
          ,form))

      (:trace
       ;; Wrap with enter/exit logging
       `(with-trace-context ',id
          ,form)))))

(defun instrument-function-body (name args body mode)
  "Instrument a function body for MODE."
  (let ((instrumented-body (instrument-form `(progn ,@body) mode)))
    `(defun ,name ,args
       ,instrumented-body)))

;;; Compile with instrumentation

(defun compile-source-instrumented (input-file mode &rest compile-args)
  "Compile a source file with instrumentation.

   MODE is :coverage, :profile, or :trace.
   Additional COMPILE-ARGS are passed to compile-source."
  (declare (ignore input-file mode compile-args))
  ;; This would require a code walker to transform the source
  ;; For now, just a placeholder
  (error "compile-source-instrumented requires a code walker (not yet implemented)"))
