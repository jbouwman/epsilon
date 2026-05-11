;;;; epsilon.repl.cli -- `epsilon repl` command handler

(defpackage epsilon.repl.cli
  (:use :cl)
  (:import (epsilon.repl repl))
  (:export #:run-repl))

(in-package :epsilon.repl.cli)

(defun %print-usage (stream)
  (format stream "Usage: epsilon repl [--package NAME] [--no-banner]~%")
  (format stream "~%Start a stdio read/eval/print loop.~%")
  (format stream "~%Options:~%")
  (format stream "  --package NAME   Initial package (default: CL-USER)~%")
  (format stream "  --no-banner      Suppress the startup banner~%")
  (format stream "~%Tips:~%")
  (format stream "  - Wrap with rlwrap for line editing + history:~%")
  (format stream "      rlwrap epsilon repl~%")
  (format stream "  - For LSP / MCP / wire-protocol surfaces, use `epsilon server`.~%"))

(defun run-repl (args passthrough-args)
  "Handler for `epsilon repl`.  Parses --package and --no-banner,
   then delegates to epsilon.repl:run."
  (declare (ignore passthrough-args))
  (let ((package "CL-USER")
        (banner t))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((or (string= arg "--help") (string= arg "-h"))
           (%print-usage *standard-output*)
           (sb-ext:exit :code 0))
          ((string= arg "--no-banner") (setf banner nil))
          ((string= arg "--package")
           (let ((v (pop args)))
             (unless v
               (format *error-output* "epsilon repl: --package requires a value~%")
               (sb-ext:exit :code 2))
             (setf package v)))
          (t
           (format *error-output* "epsilon repl: unknown option ~A~%" arg)
           (sb-ext:exit :code 2)))))
    (unless (find-package package)
      (format *error-output* "epsilon repl: package not found: ~A~%" package)
      (sb-ext:exit :code 2))
    (repl:run :banner banner :initial-package package)
    (sb-ext:exit :code 0)))
