;;;; epsilon.json.cli -- `epsilon json` command handler
;;;;
;;;; Pretty-prints / validates JSON.  By default reads stdin, parses
;;;; with epsilon.json:parse, and writes the document back to stdout
;;;; with two-space indentation.  --check skips emission for callers
;;;; that only need pass/fail.  --canonical produces RFC 8785 (JCS)
;;;; output for content-addressable hashing.
;;;;
;;;; Exit codes:
;;;;   0  parse + emit succeeded (or --check passed)
;;;;   1  parse error (location reported on stderr)
;;;;   2  usage error or I/O failure

(defpackage epsilon.json.cli
  (:use :cl)
  (:import (epsilon.json json))
  (:export #:run))

(in-package :epsilon.json.cli)

(defun %print-usage (stream)
  (format stream "Usage: epsilon json [options] [FILE]~%")
  (format stream "~%Pretty-print or validate a JSON document.  Reads FILE if given;~%")
  (format stream "otherwise reads stdin.  Writes formatted output to stdout unless~%")
  (format stream "--check is set.~%")
  (format stream "~%Options:~%")
  (format stream "  --check         Validate only; print nothing on success.  Exit 1~%")
  (format stream "                  with a parse error message on failure.~%")
  (format stream "  --compact       Emit on one line with no indentation (still valid JSON).~%")
  (format stream "  --sort-keys     Emit object members in lexicographic key order.~%")
  (format stream "  --canonical     RFC 8785 (JCS) canonical form: sorted keys, no~%")
  (format stream "                  whitespace, ECMA-262 number formatting.  Suitable~%")
  (format stream "                  for content-addressable hashing.  Implies --compact.~%")
  (format stream "  -h, --help      Show this message and exit.~%")
  (format stream "~%Examples:~%")
  (format stream "  epsilon json config.json                     # pretty-print to stdout~%")
  (format stream "  curl -s https://api/x | epsilon json         # pretty-print stdin~%")
  (format stream "  epsilon json --check config.json             # validate; exit 1 on error~%")
  (format stream "  epsilon json --canonical doc.json | sha256sum~%"))

(defun %parse-options (args)
  "Walk ARGS and return (values file check compact sort-keys canonical).
   Errors and exits with code 2 on usage problems."
  (let (file check compact sort-keys canonical)
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((or (string= arg "-h") (string= arg "--help"))
           (%print-usage *standard-output*) (sb-ext:exit :code 0))
          ((string= arg "--check")     (setf check t))
          ((string= arg "--compact")   (setf compact t))
          ((string= arg "--sort-keys") (setf sort-keys t))
          ((string= arg "--canonical") (setf canonical t))
          ((and (> (length arg) 0) (char= (char arg 0) #\-)
                (not (string= arg "-")))
           (format *error-output* "epsilon json: unknown option ~A~%" arg)
           (%print-usage *error-output*)
           (sb-ext:exit :code 2))
          (file
           (format *error-output* "epsilon json: only one input file may be given~%")
           (sb-ext:exit :code 2))
          (t (setf file arg)))))
    (when canonical (setf compact t sort-keys t))
    (values file check compact sort-keys canonical)))

(defun %resolve-against-user-cwd (path)
  "Resolve a relative PATH against EPSILON_CWD (the user's original
   shell directory before the launcher chdir'd into EPSILON_HOME).
   Absolute paths and stdin (\"-\") pass through."
  (cond
    ((or (null path) (string= path "-")) path)
    ((and (> (length path) 0) (char= (char path 0) #\/)) path)
    (t
     (let ((cwd (sb-ext:posix-getenv "EPSILON_CWD")))
       (if cwd
           (concatenate 'string (string-right-trim "/" cwd) "/" path)
           path)))))

(defun %with-input (file fn)
  "Call FN with an input stream.  FILE = NIL or \"-\" reads stdin."
  (cond
    ((or (null file) (string= file "-"))
     (funcall fn *standard-input*))
    (t
     (let ((path (%resolve-against-user-cwd file)))
       (unless (probe-file path)
         (format *error-output* "epsilon json: file not found: ~A~%" file)
         (sb-ext:exit :code 2))
       (with-open-file (s path :direction :input) (funcall fn s))))))

(defun %parse (stream file)
  "Parse JSON from STREAM, reporting source location on failure."
  (handler-case (json:parse stream)
    (error (e)
      (format *error-output* "epsilon json: ~A: ~A~%"
              (or file "<stdin>") e)
      (sb-ext:exit :code 1))))

(defun run (args passthrough-args)
  "Handler for `epsilon json`."
  (declare (ignore passthrough-args))
  (multiple-value-bind (file check compact sort-keys canonical)
      (%parse-options args)
    (%with-input file
      (lambda (in)
        (let ((value (%parse in file)))
          (cond
            (check
             (sb-ext:exit :code 0))
            (canonical
             (write-string (json:encode-jcs value) *standard-output*)
             (terpri *standard-output*))
            (t
             (json:encode value *standard-output*
                         :pretty (not compact)
                         :sort-keys sort-keys)
             (unless compact (force-output *standard-output*))
             (when compact (terpri *standard-output*))
             (sb-ext:exit :code 0))))))))
