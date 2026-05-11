;;;; boot-log.lisp - Minimal logging shim for the bootstrap
;;;;
;;;; Provides the epsilon.log package with the same call signatures used by
;;;; the loader, commands, project, and main.  The full epsilon.log module
;;;; replaces this package when loaded, adding appenders, JSON output,
;;;; structured fields, and spec-based configuration.

(cl:defpackage epsilon.log
  (:use :cl)
  (:shadow error log trace debug warn)
  (:export
   log trace debug info warn error fatal
   configure-from-spec configure-from-string
   configure reset-configuration set-level get-level
   logger-enabled-p logger-level
   add-appender remove-appender console-appender
   *root-logger* get-logger make-logger
   emit-log emit-log-with-context emit-log-with-fields
   with-context add-context remove-context))

(in-package epsilon.log)

;;; Boot log level: :trace < :debug < :info < :warn < :error < :fatal
(defvar *boot-level* :info)

(defun %level-value (level)
  (case level (:trace 0) (:debug 1) (:info 2) (:warn 3) (:error 4) (:fatal 5) (otherwise 2)))

(defmacro log (level format-string &rest args)
  "Boot shim: emit a formatted line to *error-output* when level >= *boot-level*."
  `(when (>= (%level-value ,level) (%level-value *boot-level*))
     (let ((timestamp (multiple-value-bind (sec min hour) (get-decoded-time)
                        (cl:format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))
           (tag (string-upcase (string ,level))))
       (cl:format *error-output* "~A ~A ~A: ~?~%"
                  timestamp tag (cl:package-name *package*)
                  ,format-string (cl:list ,@args))
       (force-output *error-output*))))

(defmacro trace (format-string &rest args) `(log :trace ,format-string ,@args))
(defmacro debug (format-string &rest args) `(log :debug ,format-string ,@args))
(defmacro info  (format-string &rest args) `(log :info  ,format-string ,@args))

(defmacro warn (format-string-or-condition &rest args)
  (if (stringp format-string-or-condition)
      `(log :warn ,format-string-or-condition ,@args)
      (let ((g (gensym)))
        `(let ((,g ,format-string-or-condition))
           (if (typep ,g 'condition)
               (log :warn "~A" ,g ,@args)
               (log :warn ,g ,@args))))))

(defmacro error (format-string-or-condition &rest args)
  (if (stringp format-string-or-condition)
      `(log :error ,format-string-or-condition ,@args)
      (let ((g (gensym)))
        `(let ((,g ,format-string-or-condition))
           (if (typep ,g 'condition)
               (log :error "~A" ,g ,@args)
               (log :error ,g ,@args))))))

(defmacro fatal (format-string &rest args) `(log :fatal ,format-string ,@args))

;;; Stubs for configuration API used by main.lisp
(defun configure-from-spec (spec-string)
  "Boot shim: parse a simple level from spec (e.g. \"debug\")."
  (when (and spec-string (plusp (length spec-string)))
    (let ((level (find (string-upcase spec-string)
                       '(:trace :debug :info :warn :error :fatal)
                       :key #'symbol-name :test #'string=)))
      (when level (setf *boot-level* level)))))

(defun configure-from-string (spec-string)
  "Boot shim: alias for configure-from-spec."
  (configure-from-spec spec-string))

(defun configure (&key level format)
  "Boot shim: set level; format is ignored."
  (declare (ignore format))
  (when level (setf *boot-level* level)))

(defun reset-configuration () (setf *boot-level* :info))
(defun set-level (level) (setf *boot-level* level))
(defun get-level () *boot-level*)

(defmacro with-context (bindings &body body)
  "Boot shim: ignore context bindings, just execute body."
  (declare (ignore bindings))
  `(progn ,@body))

(defun add-context (key value) (declare (ignore key value)))
(defun remove-context (key) (declare (ignore key)))

;;; Boot-time stubs for logger/appender API. The full epsilon.log module
;;; replaces these with real implementations. They exist here so that
;;; macro-expanded log calls (e.g. from epsilon.compile-integration) do
;;; not blow up if invoked before the full module is loaded.
(defvar *root-logger* :boot)

(defun get-logger (&optional name)
  (declare (ignore name))
  *root-logger*)

(defun make-logger (name)
  (declare (ignore name))
  *root-logger*)

(defun logger-enabled-p (logger level)
  (declare (ignore logger))
  (>= (%level-value level) (%level-value *boot-level*)))

(defun logger-level (logger)
  (declare (ignore logger))
  *boot-level*)

(defun add-appender (logger appender)
  (declare (ignore logger appender))
  nil)

(defun remove-appender (logger appender)
  (declare (ignore logger appender))
  nil)

(defclass console-appender () ())
