(require :sb-posix)

;;; Prevent boot from being run concurrently.

(defparameter *boot-lock-file* "_build/epsilon.core/boot.lock")
(defparameter *boot-lock-timeout* 300) ; 5 minutes

(defun boot-acquire-lock ()
  "Acquire an exclusive lock for the boot process.
   Returns (stream . fd) on success, NIL if already locked by us."
  (ensure-directories-exist *boot-lock-file*)
  (let* ((stream (open *boot-lock-file*
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create))
         (fd (sb-sys:fd-stream-fd stream))
         (start-time (get-internal-real-time))
         (timeout-internal (* *boot-lock-timeout* internal-time-units-per-second)))
    (loop
      (handler-case
          (progn
            ;; F_TLOCK = non-blocking exclusive lock
            (sb-posix:lockf fd sb-posix:f-tlock 0)
            (return (cons stream fd)))
        (sb-posix:syscall-error (c)
          (let ((errno (sb-posix:syscall-errno c)))
            (unless (or (= errno sb-posix:eagain)
                        (= errno sb-posix:eacces))
              (close stream :abort t)
              (error c)))))
      (when (> (- (get-internal-real-time) start-time) timeout-internal)
        (close stream :abort t)
        (error "Timeout waiting for boot lock after ~A seconds" *boot-lock-timeout*))
      (sleep 0.1))))

(defun boot-release-lock (lock-info)
  "Release the boot lock."
  (when lock-info
    (let ((stream (car lock-info))
          (fd (cdr lock-info)))
      (handler-case
          (sb-posix:lockf fd sb-posix:f-ulock 0)
        (sb-posix:syscall-error () nil))
      (close stream :abort t))))

(defparameter *core-module* "src/")

(defparameter *files*
  '("syntax"
    "interpolation"        ; String interpolation #~"..." - no dependencies, self-activating
    "hamt"                 ; Shared HAMT infrastructure (used by map and set)
    "map"
    "mutable-map"
    "set"
    "graph"                  ; General-purpose graph utilities (Tarjan SCC, toposort)
    "sequence"
    "string"
    "sys/pkg"
    "sys/lock"
    "symbol"
    "type"
    "sys/env"
    "path"
    "file"
    "function"
    "option"
    "result"
    "reader"
    "match"                ; Pattern matching
    "data"
    "datalog/ast"          ; Datalog core engine (pure, depends on data + syntax)
    "datalog/unify"
    "datalog/parser"
    "datalog/stratify"
    "datalog/memory"
    "datalog/evaluate"
    "datalog/graph"
    "list"
    "compile"              ; File locking + atomic writes for parallel builds
    "log"                  ; Minimal logging
    "loader-base"          ; Package + classes for epsilon.loader
    "loader"               ; Full loader implementation
    "project"              ; Project manifest support (needs loader)
    "commands"             ; CLI subcommand infrastructure (needs loader + discovery)
    "main"))               ; CLI main entry point (uses commands + discovery)

(defparameter *boot-fasl*
  #+win32 "_build\\epsilon.core\\bootstrap.fasl"
  #-win32 "_build/epsilon.core/bootstrap.fasl")

(defparameter *boot-dirs*
  #+win32
  '("_build\\"
    "_build\\epsilon.core\\")
  #-win32
  '("_build/"
    "_build/epsilon.core/"))

(defun ensure-target-dir ()
  (dolist (dir *boot-dirs*)
    (unless (probe-file dir)
      (ensure-directories-exist dir))))

(defun file-newer-p (file1 file2)
  "Return true if file1 is newer than file2, or if file2 doesn't exist"
  (or (not (probe-file file2))
      (and (probe-file file1)
           (> (file-write-date file1) (file-write-date file2)))))

(defun epk-fasl-path (file)
  "Generate EPK FASL path for a given source file"
  (concatenate 'string
               #+win32 "_build\\epsilon.core\\"
               #-win32 "_build/epsilon.core/"
               #+win32 (substitute #\\ #\/ file)
               #-win32 file
               ".fasl"))

(defun bootfile-needs-rebuild-p ()
  "Check if any source files are newer than the boot FASL"
  (or (not (probe-file *boot-fasl*))
      (some (lambda (file)
              (let ((source-path (concatenate 'string *core-module*
                                              #+win32 (substitute #\\ #\/ file)
                                              #-win32 file
                                              ".lisp")))
                (file-newer-p source-path *boot-fasl*)))
            *files*)))

(defun load-boot-fasl ()
  "Load the concatenated boot FASL if it exists and is current"
  (when (and (probe-file *boot-fasl*)
             (not (bootfile-needs-rebuild-p)))
    (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
      (load *boot-fasl*))
    t))

(defun concat-fasls (fasl-files output-file)
  "Create a single FASL file from individual FASL files"
  (with-open-file (output output-file :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
    (dolist (fasl-file fasl-files)
      (when (probe-file fasl-file)
        (with-open-file (input fasl-file :direction :input
                                         :element-type '(unsigned-byte 8))
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
      (let* ((source-path (concatenate 'string *core-module*
                                       #+win32 (substitute #\\ #\/ file)
                                       #-win32 file
                                       ".lisp"))
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
                 (fasl-path
                  (compile-file source-path
                                :output-file temp-fasl-path
                                :print nil :verbose nil)))
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
      (unwind-protect
           (progn
             (setf lock-info (boot-acquire-lock))
             ;; Re-check after acquiring lock - another process may have completed
             (unless (load-boot-fasl)
               (generate-boot-fasl)))
        (boot-release-lock lock-info)))))
