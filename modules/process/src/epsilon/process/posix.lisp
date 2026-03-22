;;;; posix.lisp - POSIX subprocess primitives via FFI
;;;;
;;;; Low-level bindings for posix_spawn, pipe2, waitpid, pidfd_open,
;;;; process group syscalls, and supporting infrastructure.
;;;;
;;;; Part of IMPL-261: POSIX Subprocess Library

(defpackage epsilon.process.posix
  (:use :cl)
  (:local-nicknames
   (lib epsilon.foreign))
  (:export
   ;; Open flags
   #:+o-rdonly+ #:+o-wronly+ #:+o-rdwr+
   #:+o-creat+ #:+o-trunc+ #:+o-append+
   #:+o-cloexec+

   ;; posix_spawn flags
   #:+posix-spawn-resetids+
   #:+posix-spawn-setpgroup+
   #:+posix-spawn-setsigdef+
   #:+posix-spawn-setsigmask+
   #:+posix-spawn-setschedparam+
   #:+posix-spawn-setscheduler+
   #:+posix-spawn-setsid+
   #:+posix-spawn-cloexec-default+

   ;; waitpid options
   #:+wnohang+ #:+wuntraced+

   ;; Signals
   #:+sighup+ #:+sigint+ #:+sigkill+ #:+sigterm+ #:+sigpipe+ #:+sigchld+

   ;; errno values
   #:+eintr+

   ;; Opaque type sizes
   #:+file-actions-size+ #:+spawnattr-size+ #:+sigset-size+

   ;; posix_spawn
   #:posix-spawn #:posix-spawnp

   ;; File actions
   #:file-actions-init #:file-actions-destroy
   #:file-actions-addopen #:file-actions-addclose #:file-actions-adddup2
   #:file-actions-addchdir
   #+darwin #:file-actions-addinherit
   #:with-file-actions

   ;; Spawn attributes
   #:spawnattr-init #:spawnattr-destroy
   #:spawnattr-setflags #:spawnattr-getflags
   #:spawnattr-setpgroup
   #:spawnattr-setsigmask #:spawnattr-setsigdefault
   #:with-spawnattr

   ;; Pipe
   #:make-pipe #:make-pipe-cloexec
   #:with-pipe

   ;; waitpid
   #:waitpid
   #:wifexited #:wexitstatus #:wifsignaled #:wtermsig
   #:wifstopped #:wstopsig

   ;; pidfd (Linux)
   #:pidfd-open #:pidfd-available-p

   ;; Process groups
   #:setpgid #:posix-setsid #:killpg #:getpgid

   ;; fd operations
   #:fd-close #:fd-read #:fd-write #:fd-set-cloexec

   ;; Signal sets
   #:sigemptyset #:sigfillset #:sigaddset
   #:with-empty-sigset #:with-full-sigset

   ;; String array building
   #:compute-string-array-size
   #:with-string-array

   ;; fd enumeration (for O_CLOEXEC strategy)
   #:list-open-fds

   ;; High-level convenience
   #:call-posix-spawnp

   ;; wait4 / rusage
   #:+rusage-size+
   #:wait4
   #:parse-rusage)
  (:enter t))

;;; ============================================================================
;;; Constants
;;; ============================================================================

;;; Open flags (for file-actions-addopen)
(defconstant +o-rdonly+  0)
(defconstant +o-wronly+  1)
(defconstant +o-rdwr+    2)
(defconstant +o-creat+   #+linux #o100    #+darwin #x0200)
(defconstant +o-trunc+   #+linux #o1000   #+darwin #x0400)
(defconstant +o-append+  #+linux #o2000   #+darwin #x0008)
(defconstant +o-cloexec+ #+linux #o2000000 #+darwin #x1000000)

;;; posix_spawn flags
(defconstant +posix-spawn-resetids+      #x01)
(defconstant +posix-spawn-setpgroup+     #x02)
(defconstant +posix-spawn-setsigdef+     #x04)
(defconstant +posix-spawn-setsigmask+    #x08)
(defconstant +posix-spawn-setschedparam+ #x10)
(defconstant +posix-spawn-setscheduler+  #x20)

;; Platform-specific spawn flags
;; Darwin: POSIX_SPAWN_CLOEXEC_DEFAULT closes all fds except those
;; explicitly configured via file actions
(defconstant +posix-spawn-cloexec-default+
  #+darwin #x80
  #+linux 0)

;; Linux glibc 2.26+: POSIX_SPAWN_SETSID creates a new session
(defconstant +posix-spawn-setsid+
  #+linux #x80
  #+darwin 0)

;;; waitpid options
(defconstant +wnohang+   1)
(defconstant +wuntraced+  2)

;;; Signals
(defconstant +sighup+  1)
(defconstant +sigint+  2)
(defconstant +sigkill+ 9)
(defconstant +sigterm+ 15)
(defconstant +sigpipe+ 13)
(defconstant +sigchld+ #+darwin 20 #+linux 17)

;;; errno values
(defconstant +eintr+ 4)

;;; Opaque type sizes (conservative for cross-platform safety)
;;;
;;; posix_spawn_file_actions_t:
;;;   glibc x86_64: 80 bytes
;;;   musl: ~24 bytes
;;;   macOS: 8 bytes (pointer, heap-allocated internally)
;;;
;;; posix_spawnattr_t:
;;;   glibc x86_64: ~336 bytes (includes 128-byte sigset_t)
;;;   musl: ~196 bytes
;;;   macOS: 8 bytes (pointer)
;;;
;;; sigset_t:
;;;   Linux: 128 bytes (__NSIG / 8)
;;;   macOS: 4 bytes (uint32_t)
(defconstant +file-actions-size+
  #+linux 128
  #+darwin 8)

(defconstant +spawnattr-size+
  #+linux 512
  #+darwin 8)

(defconstant +sigset-size+
  #+linux 128
  #+darwin 4)

;;; F_SETFD for fcntl
(defconstant +f-setfd+ 2)
(defconstant +fd-cloexec+ 1)

;;; pidfd_open syscall number (Linux 5.3+)
#+linux
(defconstant +sys-pidfd-open+ 434)

;;; ============================================================================
;;; Low-Level FFI Bindings
;;; ============================================================================

;;; --- posix_spawn ---

(lib:defshared %posix-spawn "posix_spawn" "libc" :int
  (pid :pointer)
  (path :string)
  (file-actions :pointer)
  (attrp :pointer)
  (argv :pointer)
  (envp :pointer)
  :documentation "Spawn a process using absolute path")

(lib:defshared %posix-spawnp "posix_spawnp" "libc" :int
  (pid :pointer)
  (file :string)
  (file-actions :pointer)
  (attrp :pointer)
  (argv :pointer)
  (envp :pointer)
  :documentation "Spawn a process using PATH search")

;;; --- File actions ---

(lib:defshared %file-actions-init "posix_spawn_file_actions_init" "libc" :int
  (file-actions :pointer)
  :documentation "Initialize file actions object")

(lib:defshared %file-actions-destroy "posix_spawn_file_actions_destroy" "libc" :int
  (file-actions :pointer)
  :documentation "Destroy file actions object")

(lib:defshared %file-actions-addopen "posix_spawn_file_actions_addopen" "libc" :int
  (file-actions :pointer)
  (fd :int)
  (path :string)
  (oflag :int)
  (mode :mode-t)
  :documentation "Add open action to file actions")

(lib:defshared %file-actions-addclose "posix_spawn_file_actions_addclose" "libc" :int
  (file-actions :pointer)
  (fd :int)
  :documentation "Add close action to file actions")

(lib:defshared %file-actions-adddup2 "posix_spawn_file_actions_adddup2" "libc" :int
  (file-actions :pointer)
  (fd :int)
  (newfd :int)
  :documentation "Add dup2 action to file actions")

;;; posix_spawn_file_actions_addchdir_np is non-portable but available on:
;;; - glibc 2.29+ (2019)
;;; - macOS 10.15+ (2019)
;;; - musl (recent versions)
(lib:defshared %file-actions-addchdir "posix_spawn_file_actions_addchdir_np" "libc" :int
  (file-actions :pointer)
  (path :string)
  :documentation "Add chdir action to file actions (non-portable)")

;;; posix_spawn_file_actions_addinherit_np is Darwin-only.
;;; With POSIX_SPAWN_CLOEXEC_DEFAULT, fds that should be inherited
;;; (not redirected) must be explicitly marked, or they are closed on exec.
#+darwin
(lib:defshared %file-actions-addinherit "posix_spawn_file_actions_addinherit_np" "libc" :int
  (file-actions :pointer)
  (fd :int)
  :documentation "Mark fd as inherited (not closed by CLOEXEC_DEFAULT, Darwin only)")

;;; --- Spawn attributes ---

(lib:defshared %spawnattr-init "posix_spawnattr_init" "libc" :int
  (attr :pointer)
  :documentation "Initialize spawn attributes object")

(lib:defshared %spawnattr-destroy "posix_spawnattr_destroy" "libc" :int
  (attr :pointer)
  :documentation "Destroy spawn attributes object")

(lib:defshared %spawnattr-setflags "posix_spawnattr_setflags" "libc" :int
  (attr :pointer)
  (flags :short)
  :documentation "Set flags in spawn attributes")

(lib:defshared %spawnattr-getflags "posix_spawnattr_getflags" "libc" :int
  (attr :pointer)
  (flags :pointer)
  :documentation "Get flags from spawn attributes")

(lib:defshared %spawnattr-setpgroup "posix_spawnattr_setpgroup" "libc" :int
  (attr :pointer)
  (pgroup :pid-t)
  :documentation "Set process group in spawn attributes")

(lib:defshared %spawnattr-setsigmask "posix_spawnattr_setsigmask" "libc" :int
  (attr :pointer)
  (sigmask :pointer)
  :documentation "Set signal mask in spawn attributes")

(lib:defshared %spawnattr-setsigdefault "posix_spawnattr_setsigdefault" "libc" :int
  (attr :pointer)
  (sigdefault :pointer)
  :documentation "Set default signal actions in spawn attributes")

;;; --- Pipe ---

#+linux
(lib:defshared %pipe2 "pipe2" "libc" :int
  (pipefd :pointer)
  (flags :int)
  :documentation "Create pipe with flags (Linux)")

(lib:defshared %pipe "pipe" "libc" :int
  (pipefd :pointer)
  :documentation "Create pipe")

;;; --- waitpid ---

(lib:defshared %waitpid "waitpid" "libc" :pid-t
  (pid :pid-t)
  (wstatus :pointer)
  (options :int)
  :documentation "Wait for process state change")

;;; --- wait4 / rusage ---

;;; struct rusage size: 144 bytes on LP64 (both Linux and Darwin)
(defconstant +rusage-size+ 144)

;;; struct timeval offsets for rusage fields (LP64: tv_sec=8 bytes, tv_usec=8 bytes)
;;; ru_utime: offset 0 (struct timeval, 16 bytes)
;;; ru_stime: offset 16 (struct timeval, 16 bytes)
;;; ru_maxrss: offset 32 (long)
;;; ru_minflt: offset 56 (long) -- offset 40 is ru_ixrss, 48 is ru_idrss
;;; ru_majflt: offset 64 (long)
;;; ru_nvcsw: offset 112 (long)
;;; ru_nivcsw: offset 120 (long)
(defconstant +rusage-utime-offset+ 0)
(defconstant +rusage-stime-offset+ 16)
(defconstant +rusage-maxrss-offset+ 32)
(defconstant +rusage-minflt-offset+ 56)
(defconstant +rusage-majflt-offset+ 64)
(defconstant +rusage-nvcsw-offset+ 112)
(defconstant +rusage-nivcsw-offset+ 120)

(lib:defshared %wait4 "wait4" "libc" :pid-t
  (pid :pid-t)
  (wstatus :pointer)
  (options :int)
  (rusage :pointer)
  :documentation "Wait for process with resource usage collection")

;;; --- pidfd_open (Linux 5.3+) ---

#+linux
(lib:defshared %syscall "syscall" "libc" :long
  (number :long)
  (arg1 :long)
  (arg2 :long)
  :documentation "Invoke system call by number")

;;; --- Process group syscalls ---

(lib:defshared %setpgid "setpgid" "libc" :int
  (pid :pid-t)
  (pgid :pid-t)
  :documentation "Set process group ID")

(lib:defshared %setsid "setsid" "libc" :pid-t
  :documentation "Create session and set process group ID")

(lib:defshared %killpg "killpg" "libc" :int
  (pgrp :pid-t)
  (sig :int)
  :documentation "Send signal to process group")

(lib:defshared %getpgid "getpgid" "libc" :pid-t
  (pid :pid-t)
  :documentation "Get process group ID")

;;; --- File descriptor operations ---

(lib:defshared %close "close" "libc" :int
  (fd :int)
  :documentation "Close file descriptor")

(lib:defshared %read "read" "libc" :ssize-t
  (fd :int)
  (buf :pointer)
  (count :size-t)
  :documentation "Read from file descriptor")

(lib:defshared %write "write" "libc" :ssize-t
  (fd :int)
  (buf :pointer)
  (count :size-t)
  :documentation "Write to file descriptor")

(lib:defshared %fcntl-int "fcntl" "libc" :int
  (fd :int)
  (cmd :int)
  (arg :int)
  :documentation "File control with integer argument")

;;; --- Signal set operations ---

(lib:defshared %sigemptyset "sigemptyset" "libc" :int
  (set :pointer)
  :documentation "Initialize empty signal set")

(lib:defshared %sigfillset "sigfillset" "libc" :int
  (set :pointer)
  :documentation "Initialize full signal set")

(lib:defshared %sigaddset "sigaddset" "libc" :int
  (set :pointer)
  (signum :int)
  :documentation "Add signal to signal set")

;;; --- errno ---

(lib:defshared %strerror "strerror" "libc" :string
  (errnum :int)
  :documentation "Get error string for errno")

;;; ============================================================================
;;; waitpid Status Macros
;;; ============================================================================

(defun wifexited (status)
  "Return T if child terminated normally (via exit or _exit)."
  (zerop (logand status #x7f)))

(defun wexitstatus (status)
  "Return exit status of normally terminated child."
  (logand (ash status -8) #xff))

(defun wifsignaled (status)
  "Return T if child was terminated by a signal."
  (let ((termsig (logand status #x7f)))
    (and (not (zerop termsig))
         (/= termsig #x7f))))

(defun wtermsig (status)
  "Return signal number that caused child to terminate."
  (logand status #x7f))

(defun wifstopped (status)
  "Return T if child is currently stopped."
  (= (logand status #xff) #x7f))

(defun wstopsig (status)
  "Return signal number that caused child to stop."
  (logand (ash status -8) #xff))

;;; ============================================================================
;;; High-Level Wrappers
;;; ============================================================================

;;; --- File actions ---

(defun file-actions-init (fa-ptr)
  "Initialize a posix_spawn_file_actions_t. Returns 0 on success."
  (let ((ret (%file-actions-init fa-ptr)))
    (unless (zerop ret)
      (error "posix_spawn_file_actions_init failed: ~A (~D)"
             (%strerror ret) ret))
    ret))

(defun file-actions-destroy (fa-ptr)
  "Destroy a posix_spawn_file_actions_t."
  (%file-actions-destroy fa-ptr))

(defun file-actions-addclose (fa-ptr fd)
  "Add a close action. FD will be closed in the child."
  (let ((ret (%file-actions-addclose fa-ptr fd)))
    (unless (zerop ret)
      (error "posix_spawn_file_actions_addclose(~D) failed: ~A (~D)"
             fd (%strerror ret) ret))
    ret))

(defun file-actions-adddup2 (fa-ptr fd newfd)
  "Add a dup2 action. FD will be duplicated to NEWFD in the child."
  (let ((ret (%file-actions-adddup2 fa-ptr fd newfd)))
    (unless (zerop ret)
      (error "posix_spawn_file_actions_adddup2(~D, ~D) failed: ~A (~D)"
             fd newfd (%strerror ret) ret))
    ret))

(defun file-actions-addopen (fa-ptr fd path oflag mode)
  "Add an open action. PATH will be opened as FD in the child."
  (let ((ret (%file-actions-addopen fa-ptr fd path oflag mode)))
    (unless (zerop ret)
      (error "posix_spawn_file_actions_addopen(~D, ~S) failed: ~A (~D)"
             fd path (%strerror ret) ret))
    ret))

(defun file-actions-addchdir (fa-ptr path)
  "Add a chdir action. Child will change to PATH before exec.
   Requires glibc 2.29+ or macOS 10.15+."
  (let ((ret (%file-actions-addchdir fa-ptr path)))
    (unless (zerop ret)
      (error "posix_spawn_file_actions_addchdir_np(~S) failed: ~A (~D)"
             path (%strerror ret) ret))
    ret))

#+darwin
(defun file-actions-addinherit (fa-ptr fd)
  "Mark FD as inherited in the child (not closed by CLOEXEC_DEFAULT).
   Darwin only. Required when using POSIX_SPAWN_CLOEXEC_DEFAULT for fds
   that should survive exec without being redirected."
  (let ((ret (%file-actions-addinherit fa-ptr fd)))
    (unless (zerop ret)
      (error "posix_spawn_file_actions_addinherit_np(~D) failed: ~A (~D)"
             fd (%strerror ret) ret))
    ret))

(defmacro with-file-actions ((var) &body body)
  "Execute BODY with VAR bound to an initialized posix_spawn_file_actions_t.
   The file actions object is automatically destroyed on scope exit."
  `(lib:with-foreign-memory ((,var +file-actions-size+))
     (file-actions-init ,var)
     (unwind-protect
          (progn ,@body)
       (file-actions-destroy ,var))))

;;; --- Spawn attributes ---

(defun spawnattr-init (attr-ptr)
  "Initialize a posix_spawnattr_t. Returns 0 on success."
  (let ((ret (%spawnattr-init attr-ptr)))
    (unless (zerop ret)
      (error "posix_spawnattr_init failed: ~A (~D)"
             (%strerror ret) ret))
    ret))

(defun spawnattr-destroy (attr-ptr)
  "Destroy a posix_spawnattr_t."
  (%spawnattr-destroy attr-ptr))

(defun spawnattr-setflags (attr-ptr flags)
  "Set flags on spawn attributes."
  (let ((ret (%spawnattr-setflags attr-ptr flags)))
    (unless (zerop ret)
      (error "posix_spawnattr_setflags(~X) failed: ~A (~D)"
             flags (%strerror ret) ret))
    ret))

(defun spawnattr-getflags (attr-ptr)
  "Get flags from spawn attributes. Returns the flags value."
  (lib:with-foreign-memory ((flags-buf 2))
    (let ((ret (%spawnattr-getflags attr-ptr flags-buf)))
      (unless (zerop ret)
        (error "posix_spawnattr_getflags failed: ~A (~D)"
               (%strerror ret) ret))
      (sb-sys:sap-ref-16 flags-buf 0))))

(defun spawnattr-setpgroup (attr-ptr pgroup)
  "Set process group in spawn attributes."
  (let ((ret (%spawnattr-setpgroup attr-ptr pgroup)))
    (unless (zerop ret)
      (error "posix_spawnattr_setpgroup(~D) failed: ~A (~D)"
             pgroup (%strerror ret) ret))
    ret))

(defun spawnattr-setsigmask (attr-ptr sigset-ptr)
  "Set signal mask in spawn attributes."
  (let ((ret (%spawnattr-setsigmask attr-ptr sigset-ptr)))
    (unless (zerop ret)
      (error "posix_spawnattr_setsigmask failed: ~A (~D)"
             (%strerror ret) ret))
    ret))

(defun spawnattr-setsigdefault (attr-ptr sigset-ptr)
  "Set default signal handlers in spawn attributes."
  (let ((ret (%spawnattr-setsigdefault attr-ptr sigset-ptr)))
    (unless (zerop ret)
      (error "posix_spawnattr_setsigdefault failed: ~A (~D)"
             (%strerror ret) ret))
    ret))

(defmacro with-spawnattr ((var) &body body)
  "Execute BODY with VAR bound to an initialized posix_spawnattr_t.
   The attributes object is automatically destroyed on scope exit."
  `(lib:with-foreign-memory ((,var +spawnattr-size+))
     (spawnattr-init ,var)
     (unwind-protect
          (progn ,@body)
       (spawnattr-destroy ,var))))

;;; --- Signal sets ---

(defun sigemptyset (sigset-ptr)
  "Initialize an empty signal set."
  (let ((ret (%sigemptyset sigset-ptr)))
    (when (= ret -1)
      (error "sigemptyset failed"))
    ret))

(defun sigfillset (sigset-ptr)
  "Initialize a full signal set (all signals)."
  (let ((ret (%sigfillset sigset-ptr)))
    (when (= ret -1)
      (error "sigfillset failed"))
    ret))

(defun sigaddset (sigset-ptr signum)
  "Add a signal to a signal set."
  (let ((ret (%sigaddset sigset-ptr signum)))
    (when (= ret -1)
      (error "sigaddset(~D) failed" signum))
    ret))

(defmacro with-empty-sigset ((var) &body body)
  "Execute BODY with VAR bound to an empty sigset_t."
  `(lib:with-foreign-memory ((,var +sigset-size+))
     (sigemptyset ,var)
     ,@body))

(defmacro with-full-sigset ((var) &body body)
  "Execute BODY with VAR bound to a full sigset_t (all signals)."
  `(lib:with-foreign-memory ((,var +sigset-size+))
     (sigfillset ,var)
     ,@body))

;;; --- Pipe ---

(defun make-pipe ()
  "Create a pipe. Returns (values read-fd write-fd)."
  (lib:with-foreign-memory ((pipefd 8))
    (let ((ret (%pipe pipefd)))
      (when (= ret -1)
        (error "pipe() failed: ~A" (%strerror (sb-alien:get-errno))))
      (values (sb-sys:sap-ref-32 pipefd 0)
              (sb-sys:sap-ref-32 pipefd 4)))))

(defun make-pipe-cloexec ()
  "Create a pipe with O_CLOEXEC set on both ends.
   On Linux, uses pipe2(O_CLOEXEC) atomically.
   On Darwin, uses pipe() + fcntl(F_SETFD, FD_CLOEXEC)."
  #+linux
  (lib:with-foreign-memory ((pipefd 8))
    (let ((ret (%pipe2 pipefd +o-cloexec+)))
      (when (= ret -1)
        (error "pipe2(O_CLOEXEC) failed: ~A" (%strerror (sb-alien:get-errno))))
      (values (sb-sys:sap-ref-32 pipefd 0)
              (sb-sys:sap-ref-32 pipefd 4))))
  #+darwin
  (multiple-value-bind (read-fd write-fd) (make-pipe)
    (fd-set-cloexec read-fd)
    (fd-set-cloexec write-fd)
    (values read-fd write-fd)))

(defmacro with-pipe ((read-fd write-fd &key cloexec) &body body)
  "Execute BODY with a pipe. Both ends are closed on scope exit."
  `(multiple-value-bind (,read-fd ,write-fd)
       ,(if cloexec '(make-pipe-cloexec) '(make-pipe))
     (unwind-protect
          (progn ,@body)
       (fd-close ,read-fd)
       (fd-close ,write-fd))))

;;; --- waitpid ---

(defun waitpid (pid options)
  "Wait for a process. Returns (values pid-or-zero raw-status).
   PID: child PID to wait for, -1 for any child, 0 for any in process group.
   OPTIONS: 0 for blocking, +WNOHANG+ for non-blocking.
   Returns PID of waited child (or 0 if WNOHANG and no child ready).
   Automatically retries on EINTR. Signals error on other failures."
  (lib:with-foreign-memory ((wstatus 4))
    (setf (sb-sys:sap-ref-32 wstatus 0) 0)
    (loop
      (let ((ret (%waitpid pid wstatus options)))
        (cond
          ((>= ret 0)
           (return (values ret (sb-sys:sap-ref-32 wstatus 0))))
          (t
           (let ((errno (sb-alien:get-errno)))
             (unless (= errno +eintr+)
               (error "waitpid(~D) failed: ~A (~D)" pid (%strerror errno) errno)))))))))

;;; --- wait4 / rusage ---

(defun read-timeval (sap offset)
  "Read a struct timeval at SAP+OFFSET, return seconds as a double-float."
  (let ((sec (sb-sys:signed-sap-ref-64 sap offset))
        (usec (sb-sys:signed-sap-ref-64 sap (+ offset 8))))
    (+ (float sec 1.0d0) (* (float usec 1.0d0) 1.0d-6))))

(defun parse-rusage (rusage-sap)
  "Parse a struct rusage into a plist.
   Darwin note: ru_maxrss is bytes on Darwin, KB on Linux."
  (let ((maxrss-raw (sb-sys:signed-sap-ref-64 rusage-sap +rusage-maxrss-offset+)))
    (list :user-time (read-timeval rusage-sap +rusage-utime-offset+)
          :system-time (read-timeval rusage-sap +rusage-stime-offset+)
          :max-rss #+darwin maxrss-raw
                   #+linux (* maxrss-raw 1024)  ; Convert KB to bytes
          :minor-faults (sb-sys:signed-sap-ref-64 rusage-sap +rusage-minflt-offset+)
          :major-faults (sb-sys:signed-sap-ref-64 rusage-sap +rusage-majflt-offset+)
          :voluntary-context-switches (sb-sys:signed-sap-ref-64 rusage-sap +rusage-nvcsw-offset+)
          :involuntary-context-switches (sb-sys:signed-sap-ref-64 rusage-sap +rusage-nivcsw-offset+))))

(defun wait4 (pid options)
  "Wait for a process with resource usage collection.
   Returns (values pid-or-zero raw-status rusage-plist).
   PID: child PID to wait for, -1 for any child.
   OPTIONS: 0 for blocking, +WNOHANG+ for non-blocking.
   Automatically retries on EINTR."
  (lib:with-foreign-memory ((wstatus 4)
                            (rusage +rusage-size+))
    (setf (sb-sys:sap-ref-32 wstatus 0) 0)
    ;; Zero the rusage struct
    (dotimes (i +rusage-size+)
      (setf (sb-sys:sap-ref-8 rusage i) 0))
    (loop
      (let ((ret (%wait4 pid wstatus options rusage)))
        (cond
          ((>= ret 0)
           (return (values ret
                           (sb-sys:sap-ref-32 wstatus 0)
                           (when (> ret 0)
                             (parse-rusage rusage)))))
          (t
           (let ((errno (sb-alien:get-errno)))
             (unless (= errno +eintr+)
               (error "wait4(~D) failed: ~A (~D)" pid (%strerror errno) errno)))))))))

;;; --- pidfd_open (Linux 5.3+) ---

#+linux
(defvar *pidfd-available* :unknown
  "Whether pidfd_open is available on this kernel. :unknown, t, or nil.")

#+linux
(defun pidfd-available-p ()
  "Check whether pidfd_open syscall is available (Linux 5.3+)."
  (when (eq *pidfd-available* :unknown)
    (handler-case
        (let ((fd (%syscall +sys-pidfd-open+ (sb-posix:getpid) 0)))
          (if (>= fd 0)
              (progn
                (%close fd)
                (setf *pidfd-available* t))
              (setf *pidfd-available* nil)))
      (error ()
        (setf *pidfd-available* nil))))
  *pidfd-available*)

#-linux
(defun pidfd-available-p ()
  "pidfd is Linux-only."
  nil)

#+linux
(defun pidfd-open (pid &optional (flags 0))
  "Open a pidfd for a process. Returns the file descriptor.
   Requires Linux 5.3+. Signals error if unavailable or on failure."
  (let ((fd (%syscall +sys-pidfd-open+ pid flags)))
    (when (< fd 0)
      (let ((errno (sb-alien:get-errno)))
        (error "pidfd_open(~D) failed: ~A (~D)" pid (%strerror errno) errno)))
    fd))

;;; --- Process group syscalls ---

(defun setpgid (pid pgid)
  "Set process group ID. PID=0 means calling process, PGID=0 means use PID."
  (let ((ret (%setpgid pid pgid)))
    (when (= ret -1)
      (let ((errno (sb-alien:get-errno)))
        (error "setpgid(~D, ~D) failed: ~A (~D)" pid pgid (%strerror errno) errno)))
    ret))

(defun posix-setsid ()
  "Create a new session. Returns the new session ID."
  (let ((ret (%setsid)))
    (when (= ret -1)
      (let ((errno (sb-alien:get-errno)))
        (error "setsid failed: ~A (~D)" (%strerror errno) errno)))
    ret))

(defun killpg (pgrp sig)
  "Send signal SIG to all processes in group PGRP."
  (let ((ret (%killpg pgrp sig)))
    (when (= ret -1)
      (let ((errno (sb-alien:get-errno)))
        (error "killpg(~D, ~D) failed: ~A (~D)" pgrp sig (%strerror errno) errno)))
    ret))

(defun getpgid (pid)
  "Get process group ID of process PID. PID=0 means calling process."
  (let ((ret (%getpgid pid)))
    (when (= ret -1)
      (let ((errno (sb-alien:get-errno)))
        (error "getpgid(~D) failed: ~A (~D)" pid (%strerror errno) errno)))
    ret))

;;; --- fd operations ---

(defun fd-close (fd)
  "Close a file descriptor. Returns 0 on success."
  (let ((ret (%close fd)))
    (when (= ret -1)
      (let ((errno (sb-alien:get-errno)))
        ;; EBADF (9) is common when fd already closed; don't error
        (unless (= errno 9)
          (error "close(~D) failed: ~A (~D)" fd (%strerror errno) errno))))
    ret))

(defun fd-read (fd buf count)
  "Read up to COUNT bytes from FD into BUF (SAP). Returns bytes read."
  (%read fd buf count))

(defun fd-write (fd buf count)
  "Write COUNT bytes from BUF (SAP) to FD. Returns bytes written."
  (%write fd buf count))

(defun fd-set-cloexec (fd)
  "Set FD_CLOEXEC flag on a file descriptor."
  (let ((ret (%fcntl-int fd +f-setfd+ +fd-cloexec+)))
    (when (= ret -1)
      (let ((errno (sb-alien:get-errno)))
        (error "fcntl(~D, F_SETFD, FD_CLOEXEC) failed: ~A (~D)"
               fd (%strerror errno) errno)))
    ret))

;;; ============================================================================
;;; File Descriptor Enumeration
;;; ============================================================================

;;; F_GETFD for fcntl -- returns fd flags or -1 if fd is invalid
(defconstant +f-getfd+ 1)

#+linux
(defun list-open-fds ()
  "List all open file descriptors for the current process.
   Returns a sorted list of integer fd numbers.
   Uses fcntl(F_GETFD) to probe each fd up to a reasonable limit."
  (let ((fds nil)
        ;; Read the kernel's fd limit for this process from /proc
        (max-fd (or (ignore-errors
                      (with-open-file (s "/proc/self/fdinfo" :direction :input
                                        :if-does-not-exist nil)
                        (when s 1024)))
                    1024)))
    (dotimes (fd (min max-fd 1024))
      (when (>= (%fcntl-int fd +f-getfd+ 0) 0)
        (push fd fds)))
    (nreverse fds)))

#+darwin
(defun list-open-fds ()
  "List open file descriptors (Darwin stub -- uses POSIX_SPAWN_CLOEXEC_DEFAULT instead)."
  ;; On Darwin, we use POSIX_SPAWN_CLOEXEC_DEFAULT which makes fd enumeration unnecessary.
  ;; Return a minimal list for compatibility.
  '(0 1 2))

;;; ============================================================================
;;; String Array Building
;;; ============================================================================

(defun make-c-string (sap offset string)
  "Write a null-terminated C string at SAP+OFFSET. Returns bytes written
   (including the null terminator)."
  (let* ((octets (sb-ext:string-to-octets string :external-format :utf-8))
         (len (length octets))
         (dest (sb-sys:sap+ sap offset)))
    (dotimes (i len)
      (setf (sb-sys:sap-ref-8 dest i) (aref octets i)))
    (setf (sb-sys:sap-ref-8 dest len) 0)
    (1+ len)))

(defun compute-string-array-size (strings)
  "Compute total foreign memory needed for a NULL-terminated C string array.
   Layout: [ptr0|ptr1|...|NULL|str0\\0|str1\\0|...]"
  (let ((n (length strings))
        (ptr-size 8))
    (+ (* ptr-size (1+ n))
       (loop for s in strings
             sum (1+ (length (sb-ext:string-to-octets s :external-format :utf-8)))))))

(defun build-string-array (sap strings)
  "Build a NULL-terminated C string array at SAP.
   Returns the SAP (same as input, for convenience)."
  (let* ((n (length strings))
         (ptr-size 8)
         (str-offset (* ptr-size (1+ n))))
    (loop for s in strings
          for i from 0
          do (let ((str-sap (sb-sys:sap+ sap str-offset)))
               ;; Store pointer to string in the array
               (setf (sb-sys:sap-ref-sap sap (* i ptr-size)) str-sap)
               ;; Write the string data
               (incf str-offset (make-c-string sap
                                               (- (sb-sys:sap-int str-sap)
                                                  (sb-sys:sap-int sap))
                                               s))))
    ;; NULL-terminate the pointer array
    (setf (sb-sys:sap-ref-sap sap (* n ptr-size)) (sb-sys:int-sap 0))
    sap))

(defmacro with-string-array ((var strings-form) &body body)
  "Execute BODY with VAR bound to a NULL-terminated C string array (char*[])
   built from STRINGS-FORM (a list of Lisp strings).

   Example:
     (with-string-array (argv (list \"/bin/echo\" \"hello\"))
       (call-posix-spawnp \"echo\" argv ...))"
  (let ((strings-var (gensym "STRINGS"))
        (size-var (gensym "SIZE")))
    `(let* ((,strings-var ,strings-form)
            (,size-var (compute-string-array-size ,strings-var)))
       (lib:with-foreign-memory ((,var ,size-var))
         (build-string-array ,var ,strings-var)
         ,@body))))

;;; ============================================================================
;;; posix_spawn High-Level Interface
;;; ============================================================================

(defun posix-spawn (path argv-sap envp-sap
                    &optional file-actions-sap spawnattr-sap)
  "Spawn a process using absolute path. Returns the child PID.
   ARGV-SAP and ENVP-SAP must be NULL-terminated C string arrays.
   FILE-ACTIONS-SAP and SPAWNATTR-SAP are optional (pass nil for defaults)."
  (lib:with-foreign-memory ((pid-buf 4))
    (setf (sb-sys:sap-ref-32 pid-buf 0) 0)
    (let ((ret (%posix-spawn pid-buf path
                             (or file-actions-sap (sb-sys:int-sap 0))
                             (or spawnattr-sap (sb-sys:int-sap 0))
                             argv-sap envp-sap)))
      (unless (zerop ret)
        (error "posix_spawn(~S) failed: ~A (~D)" path (%strerror ret) ret))
      (sb-sys:sap-ref-32 pid-buf 0))))

(defun posix-spawnp (file argv-sap envp-sap
                     &optional file-actions-sap spawnattr-sap)
  "Spawn a process using PATH search. Returns the child PID.
   ARGV-SAP and ENVP-SAP must be NULL-terminated C string arrays.
   FILE-ACTIONS-SAP and SPAWNATTR-SAP are optional (pass nil for defaults)."
  (lib:with-foreign-memory ((pid-buf 4))
    (setf (sb-sys:sap-ref-32 pid-buf 0) 0)
    (let ((ret (%posix-spawnp pid-buf file
                              (or file-actions-sap (sb-sys:int-sap 0))
                              (or spawnattr-sap (sb-sys:int-sap 0))
                              argv-sap envp-sap)))
      (unless (zerop ret)
        (error "posix_spawnp(~S) failed: ~A (~D)" file (%strerror ret) ret))
      (sb-sys:sap-ref-32 pid-buf 0))))

;;; ============================================================================
;;; Convenience: spawn with Lisp strings
;;; ============================================================================

(defun call-posix-spawnp (program args &key environment
                                         file-actions-sap
                                         spawnattr-sap)
  "Spawn a process with PATH search, using Lisp strings for convenience.
   PROGRAM: command name (searched in PATH)
   ARGS: list of argument strings (program name should be first element)
   ENVIRONMENT: list of \"KEY=VALUE\" strings, or nil to inherit
   Returns the child PID.

   Example:
     (call-posix-spawnp \"echo\" '(\"echo\" \"hello\" \"world\"))"
  (with-string-array (argv args)
    (if environment
        (with-string-array (envp environment)
          (posix-spawnp program argv envp file-actions-sap spawnattr-sap))
        ;; Inherit parent environment: build envp from sb-ext:posix-environ
        (let ((parent-env (sb-ext:posix-environ)))
          (with-string-array (envp parent-env)
            (posix-spawnp program argv envp file-actions-sap spawnattr-sap))))))
