;;;; This module provides advisory file locking to prevent concurrent
;;;; compilation of the same file, and atomic writes.

(defpackage epsilon.compile
  (:use cl)
  (:export
   compile-file-safely
   with-file-lock
   with-compilation-lock))

(in-package epsilon.compile)

;;; Ensure sb-posix is available for lockf
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

;;; Lock acquisition timeout in seconds
(defparameter *lock-timeout* 300
  "Maximum seconds to wait for a compilation lock.")

;;; Lock polling interval in seconds
(defparameter *lock-poll-interval* 0.1
  "Seconds between lock acquisition attempts.")

(defun lock-file-path (path)
  "Return the lock file path for a given file path."
  (concatenate 'string (namestring path) ".lock"))

(defun acquire-lock-with-timeout (lock-fd timeout)
  "Acquire lock with timeout using lockf. Returns T if acquired, signals error on timeout."
  (let ((start-time (get-internal-real-time))
        (timeout-internal (* timeout internal-time-units-per-second)))
    (loop
      ;; Try non-blocking lock (F_TLOCK)
      (handler-case
          (progn
            (sb-posix:lockf lock-fd sb-posix:f-tlock 0)
            (return t))
        (sb-posix:syscall-error (c)
          ;; EAGAIN or EACCES means lock is held by another process
          (let ((errno (sb-posix:syscall-errno c)))
            (unless (or (= errno sb-posix:eagain)
                        (= errno sb-posix:eacces))
              (error c)))))
      ;; Check timeout
      (when (> (- (get-internal-real-time) start-time) timeout-internal)
        (error "Timeout waiting for compilation lock after ~A seconds" timeout))
      ;; Sleep before retry
      (sleep *lock-poll-interval*))))

(defun release-lock (lock-fd)
  "Release the lock on the file descriptor using lockf."
  (handler-case
      (sb-posix:lockf lock-fd sb-posix:f-ulock 0)
    (sb-posix:syscall-error ()
      ;; Ignore errors on unlock - file may already be closed
      nil)))

(defmacro with-file-lock ((path &key (timeout '*lock-timeout*)) &body body)
  "Execute BODY while holding an exclusive advisory lock on PATH.
   Creates a .lock file for locking to avoid issues with the target file itself.
   Waits up to TIMEOUT seconds for the lock. The lock file is deleted after use."
  (let ((lock-path (gensym "LOCK-PATH"))
        (lock-stream (gensym "LOCK-STREAM"))
        (lock-fd (gensym "LOCK-FD")))
    `(let* ((,lock-path (lock-file-path ,path))
            (,lock-stream nil)
            (,lock-fd nil))
       (unwind-protect
            (progn
              ;; Ensure lock file directory exists
              (ensure-directories-exist ,lock-path)
              ;; Open/create lock file
              (setf ,lock-stream (open ,lock-path
                                       :direction :output
                                       :if-exists :overwrite
                                       :if-does-not-exist :create))
              (setf ,lock-fd (sb-sys:fd-stream-fd ,lock-stream))
              ;; Acquire lock with timeout
              (acquire-lock-with-timeout ,lock-fd ,timeout)
              ;; Execute body with lock held
              ,@body)
         ;; Cleanup: release lock, close file, and delete lock file
         (when ,lock-fd
           (release-lock ,lock-fd))
         (when ,lock-stream
           (close ,lock-stream :abort t))
         ;; Delete the lock file after releasing lock and closing stream
         (ignore-errors (delete-file ,lock-path))))))

(defmacro with-compilation-lock ((output-file &key (timeout '*lock-timeout*)) &body body)
  "Execute BODY while holding a compilation lock for OUTPUT-FILE.
   This is a convenience wrapper around with-file-lock for compilation operations."
  `(with-file-lock (,output-file :timeout ,timeout)
     ,@body))

(defun generate-temp-fasl-path (output-file)
  "Generate a unique temporary path for compilation output."
  (let* ((output-path (pathname output-file))
         (pid (sb-posix:getpid))
         (timestamp (get-internal-real-time)))
    ;; Create temp file in same directory with unique prefix
    (make-pathname :name (format nil ".tmp-~A-~A-~A"
                                 pid timestamp
                                 (pathname-name output-path))
                   :type (pathname-type output-path)
                   :defaults output-path)))

(defun safe-rename-file (temp-path final-path)
  "Atomically rename temp file to final path.
   On POSIX systems, rename(2) is atomic when source and dest are on same filesystem.
   Uses sb-posix:rename to avoid SBCL pathname transformation issues with
   directory names containing dots (e.g., _build/epsilon.crypto/)."
  ;; Delete existing file first if it exists (rename won't overwrite on some systems)
  (when (probe-file final-path)
    (delete-file final-path))
  (sb-posix:rename (namestring (merge-pathnames temp-path))
                   (namestring (merge-pathnames final-path))))

(defvar *in-warning-check-p* nil
  "Re-entrance guard for undefined-function-warning-p.
   Prevents infinite recursion when princ-to-string on a condition
   triggers another style-warning during compilation.")

(defun undefined-function-warning-p (condition)
  "Check if CONDITION is a style-warning about an undefined function.
   SBCL emits these as deferred style-warnings at end of compilation unit
   when a referenced function was never defined."
  (and (typep condition 'style-warning)
       (not *in-warning-check-p*)
       (let ((*in-warning-check-p* t))
         (let ((text (princ-to-string condition)))
           (search "undefined function" text :test #'char-equal)))))

(defun warnings-sidecar-path (fasl-path)
  "Return the .warnings sidecar file path for a FASL."
  (concatenate 'string (namestring fasl-path) ".warnings"))

(defun write-warnings-sidecar (fasl-path notes)
  "Write compiler notes to the .warnings sidecar file.
   NOTES is a list of note strings collected during compilation."
  (when notes
    (let ((warnings-path (warnings-sidecar-path fasl-path)))
      (handler-case
          (with-open-file (s warnings-path :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
            (dolist (note notes)
              (write-line note s)))
        (error () nil)))))

(defun compile-file-safely (input-file &key
                                         (output-file (compile-file-pathname input-file))
                                         (verbose nil)
                                         (print nil)
                                         (external-format :default)
                                         (timeout *lock-timeout*))
  "Compile INPUT-FILE to OUTPUT-FILE with file locking and atomic writes.

   This function:
   1. Acquires an exclusive advisory lock on OUTPUT-FILE
   2. Compiles to a temporary file
   3. Atomically renames the temp file to OUTPUT-FILE
   4. Releases the lock

   Undefined function references are treated as fatal errors to catch
   missing package qualifiers and typos at build time.

   Compiler notes (optimization hints, type inference) are muffled and
   written to a .warnings sidecar file alongside the FASL.

   This prevents parallel compilations from corrupting each other's output.

   Returns the same values as COMPILE-FILE: output-truename, warnings-p, failure-p"
  (let ((temp-path (generate-temp-fasl-path output-file))
        (undefined-warnings nil)
        (compiler-notes nil))
    (with-compilation-lock (output-file :timeout timeout)
      ;; Ensure output directory exists
      (ensure-directories-exist output-file)
      ;; Compile to temp file
      (unwind-protect
           (multiple-value-bind (output-truename warnings-p failure-p)
               (handler-bind
                   ((sb-ext:compiler-note
                      (lambda (c)
                        (push (princ-to-string c) compiler-notes)
                        (muffle-warning c)))
                    (style-warning
                      (lambda (c)
                        (when (undefined-function-warning-p c)
                          (push (princ-to-string c) undefined-warnings))))
                    ;; Muffle package variance warnings during compilation.
                    ;; Packages may already have auto-exported symbols from a
                    ;; prior load, causing defpackage forms to see variance.
                    (warning
                      (lambda (c)
                        (when (and (not (typep c 'style-warning))
                                   (let ((text (princ-to-string c)))
                                     (search "*ON-PACKAGE-VARIANCE*" text)))
                          (muffle-warning c)))))
                 (let ((sb-ext:*on-package-variance* nil))
                   (compile-file input-file
                                 :output-file temp-path
                                 :verbose verbose
                                 :print print
                                 :external-format external-format)))
             (declare (ignore output-truename))
             ;; Treat undefined function warnings as compilation failures
             (when undefined-warnings
               (dolist (w (nreverse undefined-warnings))
                 (format *error-output*
                         "~&; Fatal: ~A in ~A~%" w input-file))
               (setf failure-p t))
             (if failure-p
                 ;; Compilation failed - clean up temp and propagate failure
                 (progn
                   (when (probe-file temp-path)
                     (delete-file temp-path))
                   (values nil warnings-p failure-p))
                 ;; Compilation succeeded - atomic rename, write notes sidecar
                 (progn
                   (safe-rename-file temp-path output-file)
                   (write-warnings-sidecar output-file (nreverse compiler-notes))
                   (values (truename output-file) warnings-p failure-p))))
        ;; Cleanup temp file on any exit (error, etc.)
        (when (probe-file temp-path)
          (ignore-errors (delete-file temp-path)))))))
