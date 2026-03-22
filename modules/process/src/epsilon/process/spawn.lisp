;;;; spawn.lisp - High-level spawn specification and process handle
;;;;
;;;; Translates declarative spawn specifications into posix_spawn calls
;;;; with pre-computed file actions and attributes. All configuration is
;;;; collected before any system call, following the pattern established
;;;; by Go's os/exec, Rust's std::process::Command, and Python 3's subprocess.
;;;;
;;;; Part of IMPL-261: POSIX Subprocess Library (Stage 2)

(defpackage epsilon.process.spawn
  (:use :cl)
  (:local-nicknames
   (posix epsilon.process.posix)
   (lib epsilon.foreign)
   (lock epsilon.sys.lock))
  (:export
   ;; Spawn specification
   #:spawn-spec
   #:make-spawn-spec
   #:spawn-spec-program
   #:spawn-spec-args
   #:spawn-spec-environment
   #:spawn-spec-working-directory
   #:spawn-spec-stdin-action
   #:spawn-spec-stdout-action
   #:spawn-spec-stderr-action
   #:spawn-spec-process-group
   #:spawn-spec-cloexec-default
   #:spawn-spec-keep-fds
   #:spawn-spec-search-path

   ;; Process handle
   #:process-handle
   #:process-handle-pid
   #:process-handle-pgid
   #:process-handle-stdin-fd
   #:process-handle-stdout-fd
   #:process-handle-stderr-fd
   #:process-handle-status
   #:process-handle-exit-code
   #:process-handle-term-signal
   #:process-handle-pidfd
   #:process-handle-rusage
   #:process-handle-spawn-time

   ;; Process handle operations
   #:wait-for-handle
   #:handle-poll
   #:handle-kill
   #:handle-terminate
   #:handle-terminate-gracefully
   #:handle-exited-p
   #:handle-close-streams

   ;; Process registry
   #:registered-handles

   ;; FD leak tracking
   #:*fd-tracking-enabled*
   #:track-fd
   #:untrack-fd
   #:check-fd-leaks
   #:tracked-fd-count
   #:with-fd-tracking

   ;; Core spawn function
   #:spawn

   ;; Builder helpers
   #:build-file-actions
   #:build-spawnattr

   ;; Output reading
   #:read-handle-output

   ;; Error conditions
   #:spawn-error)
  (:enter t))

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition spawn-error (error)
  ((program :initarg :program :reader spawn-error-program)
   (message :initarg :message :reader spawn-error-message))
  (:report (lambda (c s)
             (format s "Spawn error for ~S: ~A"
                     (spawn-error-program c)
                     (spawn-error-message c)))))

;;; ============================================================================
;;; FD Leak Tracking (Debug Mode)
;;; ============================================================================

(defvar *fd-tracking-enabled* nil
  "When T, track all pipe fds created by the subprocess library.")

(defvar *tracked-fds* (make-hash-table :test 'eql)
  "Map of fd -> description for tracked file descriptors.")

(defvar *tracked-fds-lock* (lock:make-lock "tracked-fds"))

(defun track-fd (fd description)
  "Record an open fd for leak tracking. No-op when tracking is disabled."
  (when *fd-tracking-enabled*
    (lock:with-lock (*tracked-fds-lock*)
      (setf (gethash fd *tracked-fds*) description))))

(defun untrack-fd (fd)
  "Remove an fd from tracking. No-op when tracking is disabled."
  (when *fd-tracking-enabled*
    (lock:with-lock (*tracked-fds-lock*)
      (remhash fd *tracked-fds*))))

(defun check-fd-leaks ()
  "Return a list of (fd . description) for any tracked fds not yet closed.
   Returns nil if no leaks detected."
  (lock:with-lock (*tracked-fds-lock*)
    (let ((leaks nil))
      (maphash (lambda (fd desc)
                 (push (cons fd desc) leaks))
               *tracked-fds*)
      leaks)))

(defun tracked-fd-count ()
  "Return the number of currently tracked (open) fds."
  (lock:with-lock (*tracked-fds-lock*)
    (hash-table-count *tracked-fds*)))

(defmacro with-fd-tracking (&body body)
  "Execute BODY with fd tracking enabled. Asserts no leaks on exit."
  `(let ((*fd-tracking-enabled* t))
     (lock:with-lock (*tracked-fds-lock*)
       (clrhash *tracked-fds*))
     (unwind-protect
          (progn ,@body)
       (let ((leaks (check-fd-leaks)))
         (when leaks
           (error "FD leak detected: ~{fd ~D (~A)~^, ~}"
                  (mapcan (lambda (pair) (list (car pair) (cdr pair))) leaks)))))))

;;; ============================================================================
;;; Spawn Specification
;;; ============================================================================

(defstruct (spawn-spec (:constructor %make-spawn-spec))
  "Immutable specification for spawning a process.
   All configuration is collected before any system call."
  (program        nil :type (or string null))
  (args           nil :type list)
  (environment    nil :type (or list (eql :inherit)))
  (working-directory nil :type (or string pathname null))
  (stdin-action   :inherit :type (or keyword string pathname))
  (stdout-action  :inherit :type (or keyword string pathname))
  (stderr-action  :inherit :type (or keyword string pathname))
  (process-group  :inherit :type (or keyword integer))
  (cloexec-default t   :type boolean)
  (keep-fds       nil :type list)
  (search-path    t   :type boolean))

(defun make-spawn-spec (program &key
                                  (args nil)
                                  (environment :inherit)
                                  working-directory
                                  (stdin :inherit)
                                  (stdout :inherit)
                                  (stderr :inherit)
                                  (process-group :inherit)
                                  (cloexec-default t)
                                  keep-fds
                                  (search-path t))
  "Create a spawn specification.

   PROGRAM: command name or absolute path.
   ARGS: argument list (should include program name as first element).
         If nil, defaults to (list program).
   ENVIRONMENT: list of \"KEY=VALUE\" strings, or :inherit (default).
   WORKING-DIRECTORY: directory to chdir to before exec, or nil.
   STDIN: :inherit | :null | :pipe | pathname
   STDOUT: :inherit | :null | :pipe | :merge-stderr | pathname
   STDERR: :inherit | :null | :pipe | pathname
   PROCESS-GROUP: :inherit | :new-group | :new-session | pgid-integer
   CLOEXEC-DEFAULT: if T (default), close all fds > 2 in child except keep-fds.
   KEEP-FDS: list of fds to NOT close in child.
   SEARCH-PATH: if T (default), search PATH for program (posix_spawnp)."
  (%make-spawn-spec
   :program program
   :args (or args (list program))
   :environment environment
   :working-directory (when working-directory
                        (namestring (pathname working-directory)))
   :stdin-action stdin
   :stdout-action stdout
   :stderr-action stderr
   :process-group process-group
   :cloexec-default cloexec-default
   :keep-fds keep-fds
   :search-path search-path))

;;; ============================================================================
;;; Process Handle
;;; ============================================================================

(defstruct (process-handle (:constructor %make-process-handle))
  "Mutable handle to a spawned process."
  (pid          0   :type integer)
  (pgid         0   :type integer)
  (stdin-fd     nil :type (or integer null))
  (stdout-fd    nil :type (or integer null))
  (stderr-fd    nil :type (or integer null))
  (status       :running :type keyword)
  (exit-code    nil :type (or integer null))
  (term-signal  nil :type (or integer null))
  (pidfd        nil :type (or integer null))
  (rusage       nil :type (or list null))
  (spawn-time   nil :type (or integer null)))

(defun handle-exited-p (handle)
  "Return T if the process has exited (normally or by signal)."
  (not (eq :running (process-handle-status handle))))

(defun handle-poll (handle)
  "Poll the process status without blocking.
   Updates and returns the handle's status."
  (when (eq :running (process-handle-status handle))
    (multiple-value-bind (waited-pid raw-status)
        (posix:waitpid (process-handle-pid handle) posix:+wnohang+)
      (when (> waited-pid 0)
        (update-handle-status handle raw-status))))
  (process-handle-status handle))

(defun wait-for-handle (handle &optional timeout)
  "Wait for the process to exit.
   TIMEOUT: seconds to wait, or nil for indefinite.
   Returns the handle. Updates status, exit-code, and term-signal."
  (when (eq :running (process-handle-status handle))
    (if timeout
        (wait-for-handle-timeout handle timeout)
        (wait-for-handle-blocking handle)))
  handle)

(defun handle-kill (handle &optional (signal posix:+sigkill+))
  "Send a signal to the process.
   Default signal is SIGKILL."
  (when (eq :running (process-handle-status handle))
    (let ((target (if (and (process-handle-pgid handle)
                           (> (process-handle-pgid handle) 0))
                      ;; Kill the process group
                      (- (process-handle-pgid handle))
                      (process-handle-pid handle))))
      (handler-case
          (sb-posix:kill target signal)
        (sb-posix:syscall-error ()
          ;; Process may already be dead
          nil))))
  handle)

(defun handle-terminate (handle)
  "Send SIGTERM to the process."
  (handle-kill handle posix:+sigterm+))

(defun handle-terminate-gracefully (handle &key (timeout 5))
  "Send SIGTERM, wait up to TIMEOUT seconds, then SIGKILL if still running."
  (handle-terminate handle)
  (wait-for-handle handle timeout)
  (when (eq :running (process-handle-status handle))
    (handle-kill handle posix:+sigkill+)
    (wait-for-handle handle 3))
  handle)

(defun handle-close-streams (handle)
  "Close all open pipe fds on the handle."
  (when (process-handle-stdin-fd handle)
    (posix:fd-close (process-handle-stdin-fd handle))
    (untrack-fd (process-handle-stdin-fd handle))
    (setf (process-handle-stdin-fd handle) nil))
  (when (process-handle-stdout-fd handle)
    (posix:fd-close (process-handle-stdout-fd handle))
    (untrack-fd (process-handle-stdout-fd handle))
    (setf (process-handle-stdout-fd handle) nil))
  (when (process-handle-stderr-fd handle)
    (posix:fd-close (process-handle-stderr-fd handle))
    (untrack-fd (process-handle-stderr-fd handle))
    (setf (process-handle-stderr-fd handle) nil))
  (when (process-handle-pidfd handle)
    (posix:fd-close (process-handle-pidfd handle))
    (untrack-fd (process-handle-pidfd handle))
    (setf (process-handle-pidfd handle) nil))
  handle)

;;; --- Internal status helpers ---

(defun update-handle-status (handle raw-status)
  "Update process-handle fields from a waitpid raw status."
  (cond
    ((posix:wifexited raw-status)
     (setf (process-handle-status handle) :exited
           (process-handle-exit-code handle) (posix:wexitstatus raw-status))
     (unregister-handle handle))
    ((posix:wifsignaled raw-status)
     (setf (process-handle-status handle) :signaled
           (process-handle-term-signal handle) (posix:wtermsig raw-status))
     (unregister-handle handle))
    ((posix:wifstopped raw-status)
     ;; Stopped, not exited -- treat as still running
     nil))
  handle)

(defun wait-for-handle-blocking (handle)
  "Block until the process exits. Collects rusage via wait4.
   With no background reaper, the zombie persists until this call
   and wait4 will always succeed for a valid child PID."
  (multiple-value-bind (waited-pid raw-status rusage)
      (posix:wait4 (process-handle-pid handle) 0)
    (declare (ignore waited-pid))
    (when rusage
      (setf (process-handle-rusage handle) rusage))
    (update-handle-status handle raw-status)))

(defun wait-for-handle-timeout (handle timeout)
  "Wait for process exit with timeout (in seconds).
   Uses non-blocking waitpid with short sleeps as a portable fallback.
   Stage 3 replaces this with epoll/kqueue-based waiting."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) deadline)
          do (multiple-value-bind (waited-pid raw-status)
                 (posix:waitpid (process-handle-pid handle) posix:+wnohang+)
               (when (> waited-pid 0)
                 (update-handle-status handle raw-status)
                 (return)))
             (sleep 0.01))))

;;; ============================================================================
;;; File Actions Builder
;;; ============================================================================

(defun build-file-actions (fa-ptr spec stdin-pipes stdout-pipes stderr-pipes)
  "Populate a posix_spawn_file_actions_t from a spawn-spec and pipe fds.
   STDIN-PIPES, STDOUT-PIPES, STDERR-PIPES: each is (read-fd . write-fd) or nil.

   For stdin:  child reads from read-fd, parent writes to write-fd.
   For stdout: child writes to write-fd, parent reads from read-fd.
   For stderr: child writes to write-fd, parent reads from read-fd."

  ;; --- stdin ---
  (let ((action (spawn-spec-stdin-action spec)))
    (cond
      ((eq action :pipe)
       (let ((read-fd (car stdin-pipes)))
         ;; Child: dup read-fd to fd 0, close write-fd
         (posix:file-actions-adddup2 fa-ptr read-fd 0)
         (posix:file-actions-addclose fa-ptr (cdr stdin-pipes))))
      ((eq action :null)
       (posix:file-actions-addopen fa-ptr 0 "/dev/null" posix:+o-rdonly+ 0))
      ((stringp action)
       (posix:file-actions-addopen fa-ptr 0 action posix:+o-rdonly+ 0))
      ((pathnamep action)
       (posix:file-actions-addopen fa-ptr 0 (namestring action) posix:+o-rdonly+ 0))
      ;; :inherit -- no action needed; fd passes through to child
      ))

  ;; --- stdout ---
  (let ((action (spawn-spec-stdout-action spec)))
    (cond
      ((eq action :pipe)
       (let ((write-fd (cdr stdout-pipes)))
         ;; Child: dup write-fd to fd 1, close read-fd
         (posix:file-actions-adddup2 fa-ptr write-fd 1)
         (posix:file-actions-addclose fa-ptr (car stdout-pipes))))
      ((eq action :null)
       (posix:file-actions-addopen fa-ptr 1 "/dev/null"
                                   (logior posix:+o-wronly+) #o644))
      ((eq action :merge-stderr)
       ;; stdout and stderr go to the same fd -- handled after stderr setup
       nil)
      ((stringp action)
       (posix:file-actions-addopen fa-ptr 1 action
                                   (logior posix:+o-wronly+ posix:+o-creat+ posix:+o-trunc+)
                                   #o644))
      ((pathnamep action)
       (posix:file-actions-addopen fa-ptr 1 (namestring action)
                                   (logior posix:+o-wronly+ posix:+o-creat+ posix:+o-trunc+)
                                   #o644))))

  ;; --- stderr ---
  (let ((action (spawn-spec-stderr-action spec)))
    (cond
      ((eq action :pipe)
       (let ((write-fd (cdr stderr-pipes)))
         ;; Child: dup write-fd to fd 2, close read-fd
         (posix:file-actions-adddup2 fa-ptr write-fd 2)
         (posix:file-actions-addclose fa-ptr (car stderr-pipes))))
      ((eq action :null)
       (posix:file-actions-addopen fa-ptr 2 "/dev/null"
                                   (logior posix:+o-wronly+) #o644))
      ((stringp action)
       (posix:file-actions-addopen fa-ptr 2 action
                                   (logior posix:+o-wronly+ posix:+o-creat+ posix:+o-trunc+)
                                   #o644))
      ((pathnamep action)
       (posix:file-actions-addopen fa-ptr 2 (namestring action)
                                   (logior posix:+o-wronly+ posix:+o-creat+ posix:+o-trunc+)
                                   #o644))))

  ;; --- :merge-stderr -- dup stdout to stderr ---
  (when (eq (spawn-spec-stdout-action spec) :merge-stderr)
    ;; If stdout is piped, dup the pipe write-fd to both fd 1 and fd 2
    ;; Otherwise just dup fd 1 to fd 2
    (if stdout-pipes
        (let ((write-fd (cdr stdout-pipes)))
          (posix:file-actions-adddup2 fa-ptr write-fd 1)
          (posix:file-actions-adddup2 fa-ptr write-fd 2)
          (posix:file-actions-addclose fa-ptr (car stdout-pipes)))
        (posix:file-actions-adddup2 fa-ptr 1 2)))

  ;; --- Close pipe fds that child doesn't need ---
  ;; The dup2 targets (0/1/2) don't have CLOEXEC, but the original pipe fds
  ;; were created with O_CLOEXEC and will be closed on exec(). However, we
  ;; still need to close the "other side" of each pipe in the child.
  ;; Those close actions were already added above (one per pipe pair).

  ;; --- Working directory ---
  (when (spawn-spec-working-directory spec)
    (posix:file-actions-addchdir fa-ptr (spawn-spec-working-directory spec)))

  ;; --- O_CLOEXEC: close inherited fds > 2 on Linux ---
  #+linux
  (when (spawn-spec-cloexec-default spec)
    (let* ((keep (spawn-spec-keep-fds spec))
           ;; Also keep the pipe fds we're using (they have CLOEXEC but
           ;; we reference them in file actions, so they must exist when
           ;; file actions execute)
           (pipe-fds (remove nil (list
                                  (when stdin-pipes (car stdin-pipes))
                                  (when stdin-pipes (cdr stdin-pipes))
                                  (when stdout-pipes (car stdout-pipes))
                                  (when stdout-pipes (cdr stdout-pipes))
                                  (when stderr-pipes (car stderr-pipes))
                                  (when stderr-pipes (cdr stderr-pipes)))))
           (all-keep (append '(0 1 2) keep pipe-fds))
           (open-fds (posix:list-open-fds)))
      (dolist (fd open-fds)
        (unless (member fd all-keep)
          ;; Add close action -- ignore errors for fds that may already
          ;; be handled by CLOEXEC
          (handler-case
              (posix:file-actions-addclose fa-ptr fd)
            (error () nil)))))))

;;; ============================================================================
;;; Spawn Attributes Builder
;;; ============================================================================

(defun build-spawnattr (attr-ptr spec)
  "Populate a posix_spawnattr_t from a spawn-spec."
  (let ((flags 0))

    ;; --- Process group ---
    (let ((pg (spawn-spec-process-group spec)))
      (cond
        ((eq pg :new-group)
         (setf flags (logior flags posix:+posix-spawn-setpgroup+))
         (posix:spawnattr-setpgroup attr-ptr 0))
        ((eq pg :new-session)
         #+linux
         (setf flags (logior flags posix:+posix-spawn-setsid+))
         #-linux
         (progn
           (setf flags (logior flags posix:+posix-spawn-setpgroup+))
           (posix:spawnattr-setpgroup attr-ptr 0)))
        ((integerp pg)
         (setf flags (logior flags posix:+posix-spawn-setpgroup+))
         (posix:spawnattr-setpgroup attr-ptr pg))))

    ;; --- Signal mask: unblock all signals in child ---
    ;; Only SETSIGMASK is needed; SETSIGDEF is unnecessary because exec()
    ;; resets all signal handlers to SIG_DFL (POSIX guarantee).
    (setf flags (logior flags posix:+posix-spawn-setsigmask+))
    (posix:with-empty-sigset (empty-mask)
      (posix:spawnattr-setsigmask attr-ptr empty-mask))

    ;; --- fd cleanup ---
    ;; DO NOT use POSIX_SPAWN_CLOEXEC_DEFAULT on Darwin: it causes a macOS
    ;; kernel interaction where waitpid never returns the child's exit
    ;; status (returns 0 with WNOHANG, blocks forever with 0).  This is
    ;; reproducible regardless of SIGCHLD handler or reaper configuration.
    ;; Instead, rely on O_CLOEXEC set on individual pipe fds (which our
    ;; pipe creation already does).  On Linux, closefrom_np is added in
    ;; build-file-actions.

    ;; --- Apply accumulated flags ---
    (posix:spawnattr-setflags attr-ptr flags)))

;;; ============================================================================
;;; Process Registry
;;; ============================================================================

(defvar *process-registry* (make-hash-table :test 'eql)
  "Map of pid -> process-handle for all tracked children.")

(defvar *registry-lock* (lock:make-lock "process-registry"))

(defun register-handle (handle)
  "Register a process handle in the global registry."
  (lock:with-lock (*registry-lock*)
    (setf (gethash (process-handle-pid handle) *process-registry*) handle)))

(defun unregister-handle (handle)
  "Remove a process handle from the global registry."
  (lock:with-lock (*registry-lock*)
    (remhash (process-handle-pid handle) *process-registry*)))

(defun registered-handles ()
  "Return a list of all registered process handles."
  (lock:with-lock (*registry-lock*)
    (loop for handle being the hash-values of *process-registry*
          collect handle)))

;;; ============================================================================
;;; SBCL Signal Handler Override
;;; ============================================================================

;;; SBCL installs a SIGCHLD handler that calls get-processes-status-changes,
;;; which iterates sb-impl::*active-processes* and calls waitpid on each PID
;;; registered via sb-ext:run-program.  Epsilon manages children exclusively
;;; through posix_spawn and its own waitpid calls.  To prevent any interaction
;;; (especially wait4(-1) from a hypothetical future SBCL change), we neuter
;;; the handler.  The SIGCHLD signal still arrives and SBCL's trampoline
;;; dispatches to this function, but it does nothing.

(sb-ext:without-package-locks
  (handler-bind ((warning #'muffle-warning))
    (defun sb-impl::get-processes-status-changes ()
      "Neutered by epsilon.process.spawn -- Epsilon manages child processes directly."
      nil)))

;;; ============================================================================
;;; Core Spawn Function
;;; ============================================================================

(defun spawn (spec)
  "Spawn a process according to SPEC (a spawn-spec).
   Returns a process-handle.

   Protocol:
   1. Validate spec
   2. Create pipes for :pipe stdin/stdout/stderr
   3. Build posix_spawn_file_actions_t
   4. Build posix_spawnattr_t
   5. Call posix_spawnp (or posix_spawn)
   6. Close child-side pipe ends in parent
   7. Optionally open pidfd (Linux 5.3+)
   8. Return process-handle"
  (validate-spawn-spec spec)

  (let ((stdin-pipes nil)
        (stdout-pipes nil)
        (stderr-pipes nil)
        (child-pid nil)
        (child-pgid nil)
        (pidfd nil))

    ;; Step 2: Create pipes for requested :pipe actions
    (handler-bind ((error (lambda (e)
                           (declare (ignore e))
                           ;; Clean up any pipes we created before the error
                           (cleanup-pipes stdin-pipes stdout-pipes stderr-pipes))))
      (when (eq (spawn-spec-stdin-action spec) :pipe)
        (multiple-value-bind (r w) (posix:make-pipe-cloexec)
          (track-fd r "stdin-pipe-read")
          (track-fd w "stdin-pipe-write")
          (setf stdin-pipes (cons r w))))

      (when (or (eq (spawn-spec-stdout-action spec) :pipe)
                (eq (spawn-spec-stdout-action spec) :merge-stderr))
        (multiple-value-bind (r w) (posix:make-pipe-cloexec)
          (track-fd r "stdout-pipe-read")
          (track-fd w "stdout-pipe-write")
          (setf stdout-pipes (cons r w))))

      (when (eq (spawn-spec-stderr-action spec) :pipe)
        (multiple-value-bind (r w) (posix:make-pipe-cloexec)
          (track-fd r "stderr-pipe-read")
          (track-fd w "stderr-pipe-write")
          (setf stderr-pipes (cons r w)))))

    (handler-case
        (progn
          ;; Steps 3-5: Build file actions, attrs, and spawn
          (posix:with-file-actions (fa)
            (build-file-actions fa spec stdin-pipes stdout-pipes stderr-pipes)
            (posix:with-spawnattr (attr)
              (build-spawnattr attr spec)
              ;; Step 5: Spawn
              (let* ((program (spawn-spec-program spec))
                     (args (spawn-spec-args spec))
                     (env (spawn-spec-environment spec)))
                (posix:with-string-array (argv args)
                  (if (eq env :inherit)
                      (let ((parent-env (sb-ext:posix-environ)))
                        (posix:with-string-array (envp parent-env)
                          (setf child-pid
                                (if (spawn-spec-search-path spec)
                                    (posix:posix-spawnp program argv envp fa attr)
                                    (posix:posix-spawn program argv envp fa attr)))))
                      (posix:with-string-array (envp env)
                        (setf child-pid
                              (if (spawn-spec-search-path spec)
                                  (posix:posix-spawnp program argv envp fa attr)
                                  (posix:posix-spawn program argv envp fa attr))))))))))

      (error (e)
        ;; Spawn failed -- clean up pipes
        (cleanup-pipes stdin-pipes stdout-pipes stderr-pipes)
        (error e)))

    ;; Step 6: Close child-side pipe ends in parent
    (when stdin-pipes
      (posix:fd-close (car stdin-pipes))   ; Close read end (child's side)
      (untrack-fd (car stdin-pipes)))
    (when stdout-pipes
      (posix:fd-close (cdr stdout-pipes))  ; Close write end (child's side)
      (untrack-fd (cdr stdout-pipes)))
    (when stderr-pipes
      (posix:fd-close (cdr stderr-pipes))  ; Close write end (child's side)
      (untrack-fd (cdr stderr-pipes)))

    ;; Step 7: Determine child's pgid
    (let ((pg (spawn-spec-process-group spec)))
      (setf child-pgid
            (cond
              ((eq pg :new-group) child-pid)
              ((eq pg :new-session) child-pid)
              ((integerp pg) pg)
              (t nil))))

    ;; Step 7b: Optionally open pidfd (Linux 5.3+)
    #+linux
    (when (posix:pidfd-available-p)
      (handler-case
          (setf pidfd (posix:pidfd-open child-pid))
        (error () nil)))

    ;; Step 8: Build process-handle, register, and return
    (let ((handle (%make-process-handle
                   :pid child-pid
                   :pgid (or child-pgid 0)
                   :stdin-fd (when stdin-pipes (cdr stdin-pipes))   ; Parent's write end
                   :stdout-fd (when stdout-pipes (car stdout-pipes)) ; Parent's read end
                   :stderr-fd (when stderr-pipes (car stderr-pipes)) ; Parent's read end
                   :status :running
                   :pidfd pidfd
                   :spawn-time (get-internal-real-time))))
      (register-handle handle)
      handle)))

;;; ============================================================================
;;; Validation
;;; ============================================================================

(defun validate-spawn-spec (spec)
  "Validate a spawn-spec before attempting to spawn."
  (let ((program (spawn-spec-program spec)))
    (unless program
      (error 'spawn-error :program "(nil)" :message "program is required"))
    ;; Validate I/O actions
    (let ((valid-stdin '(:inherit :null :pipe))
          (valid-stdout '(:inherit :null :pipe :merge-stderr))
          (valid-stderr '(:inherit :null :pipe)))
      (let ((stdin-a (spawn-spec-stdin-action spec)))
        (unless (or (member stdin-a valid-stdin)
                    (stringp stdin-a)
                    (pathnamep stdin-a))
          (error 'spawn-error :program program
                              :message (format nil "invalid stdin action: ~S" stdin-a))))
      (let ((stdout-a (spawn-spec-stdout-action spec)))
        (unless (or (member stdout-a valid-stdout)
                    (stringp stdout-a)
                    (pathnamep stdout-a))
          (error 'spawn-error :program program
                              :message (format nil "invalid stdout action: ~S" stdout-a))))
      (let ((stderr-a (spawn-spec-stderr-action spec)))
        (unless (or (member stderr-a valid-stderr)
                    (stringp stderr-a)
                    (pathnamep stderr-a))
          (error 'spawn-error :program program
                              :message (format nil "invalid stderr action: ~S" stderr-a)))))
    ;; Validate process group
    (let ((pg (spawn-spec-process-group spec)))
      (unless (or (member pg '(:inherit :new-group :new-session))
                  (integerp pg))
        (error 'spawn-error :program program
                            :message (format nil "invalid process-group: ~S" pg))))))

;;; ============================================================================
;;; Pipe Cleanup
;;; ============================================================================

(defun cleanup-pipes (&rest pipe-pairs)
  "Close all pipe fds in the given (read-fd . write-fd) pairs."
  (dolist (pair pipe-pairs)
    (when pair
      (handler-case
          (progn (posix:fd-close (car pair)) (untrack-fd (car pair)))
        (error () nil))
      (handler-case
          (progn (posix:fd-close (cdr pair)) (untrack-fd (cdr pair)))
        (error () nil)))))

;;; ============================================================================
;;; Convenience: read all output from a handle
;;; ============================================================================

(defun read-handle-output (handle &key (stream :stdout) (max-bytes 1048576))
  "Read all available output from a process handle's stdout or stderr fd.
   Returns a string. Reads up to MAX-BYTES (default 1MB).
   The handle must have been created with :pipe for the requested stream."
  (let ((fd (ecase stream
              (:stdout (process-handle-stdout-fd handle))
              (:stderr (process-handle-stderr-fd handle)))))
    (unless fd
      (return-from read-handle-output ""))
    (let ((chunks nil)
          (total 0))
      (lib:with-foreign-memory ((buf 4096))
        (loop
          (let ((n (posix:fd-read fd buf (min 4096 (- max-bytes total)))))
            (when (<= n 0) (return))
            (let ((chunk (make-array n :element-type '(unsigned-byte 8))))
              (dotimes (i n)
                (setf (aref chunk i) (sb-sys:sap-ref-8 buf i)))
              (push chunk chunks)
              (incf total n)
              (when (>= total max-bytes) (return))))))
      (if chunks
          (let ((all (make-array total :element-type '(unsigned-byte 8))))
            (let ((pos 0))
              (dolist (chunk (nreverse chunks))
                (replace all chunk :start1 pos)
                (incf pos (length chunk))))
            (sb-ext:octets-to-string all :external-format :utf-8))
          ""))))
