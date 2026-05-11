(require :sb-posix)

;;; Prevent boot from being run concurrently.
(defparameter *boot-lock-file*
  "_build/epsilon.core/boot.lock")
(defparameter *boot-lock-timeout*
  300)

; 5 minutes
(defun boot-acquire-lock ()
  "Acquire an exclusive lock for the boot process.
   Returns (stream . fd) on success, NIL if already locked by us."
  (ensure-directories-exist *boot-lock-file*)
  (let* ((stream (open *boot-lock-file*
                       :direction
                       :output
                       :if-exists
                       :overwrite
                       :if-does-not-exist
                       :create))
         (fd (sb-sys:fd-stream-fd stream))
         (start-time (get-internal-real-time))
         (timeout-internal (* *boot-lock-timeout* internal-time-units-per-second)))
    (loop (handler-case (progn
            ;; F_TLOCK = non-blocking exclusive lock
            (sb-posix:lockf fd sb-posix:f-tlock 0)
            (return (cons stream fd)))
            (sb-posix:syscall-error
             (c)
             (let ((errno (sb-posix:syscall-errno c)))
               (unless (or (= errno sb-posix:eagain) (= errno sb-posix:eacces))
                 (close stream :abort t)
                 (error c))))) (when (> (- (get-internal-real-time) start-time) timeout-internal)
            (close stream :abort t)
            (error "Timeout waiting for boot lock after ~A seconds" *boot-lock-timeout*)) (sleep 0.1))))

(defun boot-release-lock (lock-info)
  "Release the boot lock."
  (when lock-info
    (let ((stream (car lock-info))
          (fd (cdr lock-info)))
      (handler-case (sb-posix:lockf fd sb-posix:f-ulock 0)
        (sb-posix:syscall-error
         ()
         nil))
      (close stream :abort t))))

(defparameter *core-module*
  "src/")

(defparameter *files*
  '("syntax"
    "interpolation" ; String interpolation #~"..." - no dependencies, self-activating
    "hamt" ; Shared HAMT infrastructure (used by map and set)
    "map"
    "mutable-map"
    "set"
    "graph" ; General-purpose graph utilities (Tarjan SCC, toposort)
    "sequence"
    "string"
    "sys/pkg"
    "sys/lock"
    "symbol"
    "type"
    "sys/env"
    "path"
    "file"
    "option"
    "result"
    "reader"
    "match" ; Pattern matching
    "data"
    "compile" ; File locking + atomic writes for parallel builds
    "typeclass" ; Type class system (ad-hoc polymorphism via CLOS)
    "typeclass-std" ; Standard type classes (show, eq-class, ord, hash-class, serialize)
    "record" ; Immutable record types with functional update
    "enum" ; Enumeration type definitions
    "boot-log" ; Minimal logging shim (full epsilon.log is a module)
    "loader" ; Module loader
    "project" ; Project manifest
    "commands" ; CLI commands
    "main"))
; CLI entry point

; CLI main entry point (uses commands + discovery)
(defparameter *boot-fasl*
  "_build/epsilon.core/bootstrap.fasl")

(defparameter *boot-dirs*
  '("_build/" "_build/epsilon.core/"))

(defun ensure-target-dir ()
  (dolist (dir *boot-dirs*)
    (unless (probe-file dir)
      (ensure-directories-exist dir))))

(defun file-newer-p (file1 file2)
  "Return true if file1 is newer than file2, or if file2 doesn't exist"
  (or (not (probe-file file2))
      (and (probe-file file1) (> (file-write-date file1) (file-write-date file2)))))

(defun epk-fasl-path (file)
  "Generate EPK FASL path for a given source file"
  (concatenate 'string "_build/epsilon.core/" file ".fasl"))

(defun bootfile-needs-rebuild-p ()
  "Check if any source files are newer than the boot FASL"
  (or (not (probe-file *boot-fasl*))
      (some (lambda (file)
              (let ((source-path (concatenate 'string *core-module* file ".lisp")))
                (file-newer-p source-path *boot-fasl*)))
            *files*)))

(defun %fasl-load-failure-p (condition)
  "True when CONDITION is SBCL rejecting a FASL itself (bad magic,
   version mismatch, truncated header) -- as opposed to an error
   raised by code inside a FASL that loaded fine.  Pattern-matches on
   condition class name + package so we don't have to mention
   sb-fasl internals that move between SBCL releases.

   Mirrors epsilon.loader::fasl-load-failure-p, duplicated here
   because the loader isn't available yet during boot."
  (let* ((cname (class-name (class-of condition)))
         (pkg (and cname (symbol-package cname))))
    (and pkg
         (member (package-name pkg)
                 '("SB-FASL" "SB-INT" "SB-KERNEL")
                 :test #'string=)
         (let ((n (symbol-name cname)))
           (or (search "FASL" n)
               (search "INVALID-FASL" n))))))

(defun %clear-stale-boot-fasls ()
  "Delete the concatenated boot FASL and every per-file FASL under
   _build/epsilon.core/ so the next generate-boot-fasl call rebuilds
   from source.  Run when SBCL rejects bootstrap.fasl as
   version-incompatible -- the per-file FASLs were produced by the
   same SBCL and would fail the same way on the rebuild's `load'
   step, so they have to go too."
  (when (probe-file *boot-fasl*)
    (delete-file *boot-fasl*))
  (dolist (file *files*)
    (let ((p (epk-fasl-path file)))
      (when (probe-file p)
        (ignore-errors (delete-file p))))))

(defun load-boot-fasl ()
  "Load the concatenated boot FASL if it exists and is current.
   Returns T on success, NIL if the FASL is absent, stale by mtime,
   or rejected by SBCL as version-incompatible.

   The version-rejection branch is what makes SBCL upgrades
   self-healing: on SB-FASL::INVALID-FASL-VERSION (or any other
   FASL-format failure) we wipe the boot FASLs and return NIL,
   which causes the outer BOOT to fall through to GENERATE-BOOT-FASL
   and recompile from source.  Per-module FASLs are handled by
   epsilon.loader's own load-fasl-with-recovery; once boot is up,
   the rest takes care of itself."
  (when (and (probe-file *boot-fasl*) (not (bootfile-needs-rebuild-p)))
    (handler-case
        (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
          (load *boot-fasl*)
          t)
      (error (c)
        (cond
          ((%fasl-load-failure-p c)
           (format *error-output*
                   "~&;; Boot FASL rejected by SBCL (~A): recompiling.~%"
                   (type-of c))
           (%clear-stale-boot-fasls)
           nil)
          (t (error c)))))))

(defun concat-fasls (fasl-files output-file)
  "Create a single FASL file from individual FASL files"
  (with-open-file (output output-file
                          :direction
                          :output
                          :element-type
                          '(unsigned-byte 8)
                          :if-exists
                          :supersede
                          :if-does-not-exist
                          :create)
    (dolist (fasl-file fasl-files)
      (when (probe-file fasl-file)
        (with-open-file (input fasl-file :direction :input :element-type '(unsigned-byte 8))
          (loop for byte = (read-byte input nil nil)
                while byte
                do (write-byte byte output)))))))

(defun boot-temp-path (path)
  "Generate a temporary path for atomic writes during boot."
  (let ((pid (sb-posix:getpid))
        (ts (get-internal-real-time)))
    (format nil "~A.~A.~A.tmp" path pid ts)))

(defun boot-atomic-rename (temp-path final-path)
  "Atomically rename temp file to final path."
  (when (probe-file final-path)
    (delete-file final-path))
  ;; Use sb-posix:rename for atomic rename without path transformation issues
  (sb-posix:rename (namestring temp-path) (namestring final-path)))

(defun generate-boot-fasl ()
  "Generate the bootstrap FASL with atomic file writes."
  (let ((fasl-files '()))
    (dolist (file *files*)
      (let* ((source-path (concatenate 'string *core-module* file ".lisp"))
             (target-fasl-path (epk-fasl-path file))
             (temp-fasl-path (boot-temp-path target-fasl-path)))
        ;; Ensure the directory exists for this FASL
        (ensure-directories-exist target-fasl-path)
        (force-output)
        (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
          ;; Compile to temp file first
          ;; Suppress package variance warnings: packages may already have
          ;; auto-exported symbols from a prior load of the bootstrap FASL.
          (let* ((sb-ext:*on-package-variance* nil)
                 (fasl-path (compile-file source-path
                                          :output-file
                                          temp-fasl-path
                                          :print
                                          nil
                                          :verbose
                                          nil)))
            (unless fasl-path
              (when (probe-file temp-fasl-path)
                (delete-file temp-fasl-path))
              (error "compile-file returned NIL for ~A" source-path))
            ;; Atomic rename to final location
            (boot-atomic-rename temp-fasl-path target-fasl-path)
            (push (truename target-fasl-path) fasl-files)
            (load target-fasl-path)))))
    ;; Also use atomic write for the final bootstrap.fasl
    (let ((temp-boot-fasl (boot-temp-path *boot-fasl*)))
      (concat-fasls (reverse fasl-files) temp-boot-fasl)
      (boot-atomic-rename temp-boot-fasl *boot-fasl*))))

(defun boot ()
  "Bootstrap the epsilon core module with parallel-safe locking."
  (ensure-target-dir)
  ;; First try to load existing FASL without lock
  ;; This is the fast path when boot is already complete
  (unless (load-boot-fasl)
    ;; Need to compile - acquire lock to prevent races
    (let ((lock-info nil))
      (unwind-protect (progn
        (setf lock-info (boot-acquire-lock))
        ;; Re-check after acquiring lock - another process may have completed
        (unless (load-boot-fasl)
          (generate-boot-fasl)))
        (boot-release-lock lock-info)))))
