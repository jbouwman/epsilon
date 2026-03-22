;;;; Bootstrap logging stub
;;;;
;;;; Provides minimal log:info and log:warn for the loader and CLI.
;;;; epsilon.log patches these when it loads.

(defpackage epsilon.log
  (:use :cl)
  (:shadow error log trace debug warn)
  (:export log trace debug info warn error fatal
           log-with-fields info-with-fields debug-with-fields
           warn-with-fields error-with-fields fields
           configure configure-from-spec configure-from-string reset-configuration
           *spec-format-applied-p* *format-explicitly-set*
           configure-for-service service-debugger-hook
           set-level get-level logger-enabled-p logger-level
           add-appender remove-appender console-appender file-appender
           tee-appender logger-appenders appender appender-formatter append-log
           tee-appenders console-stream configure-with-tee
           logger log-record logger-name
           log-level log-logger log-message log-timestamp log-context
           log-fields log-thread log-package log-file log-line
           emit-log emit-log-with-context emit-log-with-fields
           logger-effective-level
           with-context add-context remove-context
           *root-logger* get-logger make-logger
           format-timestamp current-thread-name
           escape-json-string
           json-appender with-source-location))

(in-package :epsilon.log)

;;; Boot log level: 0=trace 1=debug 2=info 3=warn 4=error 5=fatal

(defvar *boot-log-level* 2)

(defun %boot-log (level control &rest args)
  (when (>= level *boot-log-level*)
    (multiple-value-bind (sec min hour) (get-decoded-time)
      (let ((prefix (ecase level
                      (0 "TRC") (1 "DBG") (2 "INF")
                      (3 "WRN") (4 "ERR") (5 "FTL")))
            (caller (or (cl:ignore-errors
                          (when *package* (package-name *package*)))
                        "?")))
        (apply #'cl:format *error-output*
               (concatenate 'string
                            "~2,'0D:~2,'0D:~2,'0D ~A ~A: " control "~%")
               hour min sec prefix caller args)))))

;;; Macros -- epsilon.log redefines these

(defmacro trace (control &rest args) `(%boot-log 0 ,control ,@args))
(defmacro debug (control &rest args) `(%boot-log 1 ,control ,@args))
(defmacro info  (control &rest args) `(%boot-log 2 ,control ,@args))
(defmacro warn  (control &rest args) `(%boot-log 3 ,control ,@args))
(defmacro error (control &rest args) `(%boot-log 4 ,control ,@args))
(defmacro fatal (control &rest args) `(%boot-log 5 ,control ,@args))
(defmacro log   (control &rest args) `(%boot-log 2 ,control ,@args))

;;; Structured logging stubs

(defmacro log-with-fields (fields control &rest args)
  (declare (ignore fields)) `(%boot-log 2 ,control ,@args))
(defmacro info-with-fields (fields control &rest args)
  (declare (ignore fields)) `(%boot-log 2 ,control ,@args))
(defmacro debug-with-fields (fields control &rest args)
  (declare (ignore fields)) `(%boot-log 1 ,control ,@args))
(defmacro warn-with-fields (fields control &rest args)
  (declare (ignore fields)) `(%boot-log 3 ,control ,@args))
(defmacro error-with-fields (fields control &rest args)
  (declare (ignore fields)) `(%boot-log 4 ,control ,@args))

(defmacro with-context (bindings &body body)
  (declare (ignore bindings)) `(progn ,@body))

;;; Runtime stubs -- epsilon.log replaces all of these

(defun fields (&rest args) (declare (ignore args)) nil)
(defun get-logger (&optional name) (declare (ignore name)) nil)
(defun configure (&rest args) (declare (ignore args)) nil)
(defun reset-configuration () nil)
(defun add-appender (logger appender) (declare (ignore logger appender)) nil)
(defun remove-appender (logger appender) (declare (ignore logger appender)) nil)
(defun configure-for-service (&key (format :json)) (declare (ignore format)) nil)

(defun configure-from-spec (spec)
  (setf *boot-log-level*
        (cond ((string-equal spec "trace") 0)
              ((string-equal spec "debug") 1)
              ((string-equal spec "info")  2)
              ((string-equal spec "warn")  3)
              ((string-equal spec "error") 4)
              ((string-equal spec "fatal") 5)
              (t 2))))

(defun configure-from-string (spec) (configure-from-spec spec))

(defvar *spec-format-applied-p* nil)
(defvar *format-explicitly-set* nil)

(defun service-debugger-hook (condition hook)
  (declare (ignore hook))
  (%boot-log 4 "Unhandled condition: ~A" condition)
  (sb-ext:exit :code 1))

;;; Minimal logger -- epsilon.log redefines with full CLOS class

(defclass logger ()
  ((name :initarg :name :reader logger-name :initform "root")
   (level :initarg :level :accessor logger-level :initform :info)))

(defvar *root-logger* (make-instance 'logger :name "root" :level :info))

(defun logger-enabled-p (logger level)
  (declare (ignore logger))
  (>= (ecase level (:trace 0) (:debug 1) (:info 2) (:warn 3) (:error 4) (:fatal 5))
      *boot-log-level*))

(defun emit-log (logger level package-name message)
  (declare (ignore logger))
  (%boot-log (ecase level (:trace 0) (:debug 1) (:info 2) (:warn 3) (:error 4) (:fatal 5))
             "~A: ~A" package-name message))

(defun emit-log-with-context (logger level package-name message &key file line)
  (declare (ignore logger file line))
  (emit-log nil level package-name message))

(defun emit-log-with-fields (logger level package-name message fields)
  (declare (ignore logger fields))
  (emit-log nil level package-name message))
