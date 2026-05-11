;;;; epsilon.doc.cli -- `epsilon doc` command dispatcher
;;;;
;;;; Subcommands:
;;;;   site    Generate a static API reference HTML site

(defpackage epsilon.doc.cli
  (:use :cl)
  (:import (epsilon.doc.site site)
            (epsilon.doc.serve serve)
            (epsilon.loader loader))
  (:export #:run-doc #:run-site #:run-serve))

(in-package :epsilon.doc.cli)

;;; --------------------------------------------------------------------------
;;; Top-level dispatcher
;;; --------------------------------------------------------------------------

(defun %print-doc-usage (stream)
  (format stream "Usage: epsilon doc <subcommand> [options]~%")
  (format stream "~%Subcommands:~%")
  (format stream "  site    Generate a static API reference HTML site~%")
  (format stream "  serve   Serve the API reference live over HTTP~%")
  (format stream "~%Run `epsilon doc <subcommand> --help` for subcommand options.~%"))

(defun run-doc (args passthrough-args)
  "Dispatcher for `epsilon doc`."
  (let ((sub (first args))
        (rest (rest args)))
    (cond
      ((or (null sub) (string= sub "--help") (string= sub "-h"))
       (%print-doc-usage *standard-output*)
       (sb-ext:exit :code (if sub 0 2)))
      ((string= sub "site")
       (run-site rest passthrough-args))
      ((string= sub "serve")
       (run-serve rest passthrough-args))
      (t
       (format *error-output* "epsilon doc: unknown subcommand '~A'~%" sub)
       (%print-doc-usage *error-output*)
       (sb-ext:exit :code 2)))))

;;; --------------------------------------------------------------------------
;;; `epsilon doc site`
;;; --------------------------------------------------------------------------

(defun %print-site-usage (stream)
  (format stream "Usage: epsilon doc site --output DIR [--load-all] [--only MODULE...]~%")
  (format stream "~%Generate a static HTML API reference site under DIR.~%")
  (format stream "~%Only modules currently loaded into the image are documented;~%")
  (format stream "use --load-all to load every known module first (slow), or~%")
  (format stream "pass --only <module> repeatedly to restrict to specific modules.~%")
  (format stream "~%Options:~%")
  (format stream "  --output DIR   Output directory (required)~%")
  (format stream "  --load-all     Load every known module before generating~%")
  (format stream "  --only NAME    Restrict to NAMED module (repeatable)~%")
  (format stream "~%Examples:~%")
  (format stream "  epsilon doc site --output docs/api~%")
  (format stream "  epsilon doc site --output docs/api --load-all~%")
  (format stream "  epsilon doc site --output /tmp/site --only epsilon.json --only epsilon.http~%"))

(defun %parse-site-args (args)
  "Walk ARGS pulling --output VALUE, --load-all, --only VALUE.
   Returns (values OUTPUT LOAD-ALL-P ONLY-LIST)."
  (let ((output nil)
        (load-all nil)
        (only nil))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((or (string= arg "--help") (string= arg "-h"))
           (%print-site-usage *standard-output*)
           (sb-ext:exit :code 0))
          ((string= arg "--output")
           (let ((v (pop args)))
             (unless v
               (format *error-output* "epsilon doc site: --output requires a value~%")
               (sb-ext:exit :code 2))
             (setf output v)))
          ((string= arg "--load-all") (setf load-all t))
          ((string= arg "--only")
           (let ((v (pop args)))
             (unless v
               (format *error-output* "epsilon doc site: --only requires a value~%")
               (sb-ext:exit :code 2))
             (push v only)))
          (t
           (format *error-output* "epsilon doc site: unknown option ~A~%" arg)
           (sb-ext:exit :code 2)))))
    (values output load-all (nreverse only))))

(defun %load-all-modules ()
  "Best-effort load of every known module.  Failures are logged and
   skipped so a single broken module doesn't kill the whole pass."
  (dolist (info (loader:query-modules))
    (let ((name (loader:module-name info)))
      (handler-case (loader:load-module name)
        (error (e)
          (format *error-output* "  skipped ~A (~A)~%" name e))))))

(defun run-site (args passthrough-args)
  "Handler for `epsilon doc site`."
  (declare (ignore passthrough-args))
  (multiple-value-bind (output load-all only) (%parse-site-args args)
    (unless output
      (%print-site-usage *error-output*)
      (sb-ext:exit :code 2))
    (when load-all
      (format t "~&Loading all known modules...~%")
      (%load-all-modules))
    (let ((result (site:generate-site output :only only)))
      (format t "~&Wrote ~D module page~:P to ~A~%"
              (getf result :modules-written)
              (getf result :output-dir))
      (sb-ext:exit :code 0))))

;;; --------------------------------------------------------------------------
;;; `epsilon doc serve`
;;; --------------------------------------------------------------------------

(defun %print-serve-usage (stream)
  (format stream "Usage: epsilon doc serve [--port N] [--address ADDR] [--load-all]~%")
  (format stream "~%Serve the API reference site live over HTTP.  Each request~%")
  (format stream "renders from the *current* loader state, so newly-loaded modules~%")
  (format stream "appear without a restart.  Pass --load-all to load every known~%")
  (format stream "module before serving.~%")
  (format stream "~%Options:~%")
  (format stream "  --port N         Listen port (default: 8089)~%")
  (format stream "  --address ADDR   Bind address (default: 127.0.0.1)~%")
  (format stream "  --load-all       Pre-load every known module before serving~%")
  (format stream "~%Examples:~%")
  (format stream "  epsilon doc serve~%")
  (format stream "  epsilon doc serve --port 9000 --address 0.0.0.0 --load-all~%"))

(defun %parse-serve-args (args)
  "Walk ARGS pulling --port N, --address ADDR, --load-all.  Returns
   (values PORT ADDRESS LOAD-ALL-P)."
  (let ((port 8089)
        (address "127.0.0.1")
        (load-all nil))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((or (string= arg "--help") (string= arg "-h"))
           (%print-serve-usage *standard-output*)
           (sb-ext:exit :code 0))
          ((string= arg "--port")
           (let ((v (pop args)))
             (unless v
               (format *error-output* "epsilon doc serve: --port requires a value~%")
               (sb-ext:exit :code 2))
             (setf port (handler-case (parse-integer v)
                          (error ()
                            (format *error-output*
                                    "epsilon doc serve: --port must be an integer (got '~A')~%"
                                    v)
                            (sb-ext:exit :code 2))))))
          ((string= arg "--address")
           (let ((v (pop args)))
             (unless v
               (format *error-output* "epsilon doc serve: --address requires a value~%")
               (sb-ext:exit :code 2))
             (setf address v)))
          ((string= arg "--load-all") (setf load-all t))
          (t
           (format *error-output* "epsilon doc serve: unknown option ~A~%" arg)
           (sb-ext:exit :code 2)))))
    (values port address load-all)))

(defun run-serve (args passthrough-args)
  "Handler for `epsilon doc serve`.  Starts the HTTP server, blocks
   until interrupted (Ctrl-C / SIGTERM), then shuts down cleanly.
   The unwind-protect runs the server stop on any unwind path; the
   handler-case turns SB-SYS:INTERACTIVE-INTERRUPT into a normal
   exit instead of a backtrace dump from the toplevel debugger."
  (declare (ignore passthrough-args))
  (multiple-value-bind (port address load-all) (%parse-serve-args args)
    (when load-all
      (format t "~&Loading all known modules...~%")
      (%load-all-modules))
    (let ((server (serve:serve :port port :address address)))
      (format t "~&Serving API reference on http://~A:~D/  (Ctrl-C to stop)~%"
              address port)
      (unwind-protect
           (handler-case
               ;; Block forever; the server runs in its own thread.
               (loop (sleep 60))
             (sb-sys:interactive-interrupt ()
               (format t "~&Shutting down...~%")))
        (serve:stop server)))
    (sb-ext:exit :code 0)))
