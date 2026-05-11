;;;; epsilon.sys.process -- POSIX process control wrappers
;;;;
;;;; Thin wrappers over `sb-posix:getpid' and `sb-posix:kill' so callers
;;;; don't need to import `sb-posix' for routine pid/signal handling.
;;;; The conversion to a higher-level facility (e.g. structured process
;;;; supervision) belongs in `epsilon.process'; this module is just the
;;;; raw POSIX surface.

(cl:defpackage epsilon.sys.process
  (:use cl)
  (:export
   #:current-pid
   #:signal-process
   #:process-alive-p
   #:exit))

(in-package epsilon.sys.process)

(defun current-pid ()
  "Return the calling process's PID as an integer."
  #-win32 (sb-posix:getpid)
  #+win32 (error "current-pid: not implemented on Windows"))

(defun signal-process (pid signal)
  "Send SIGNAL (an integer signum) to PID.  Errors propagate as
`sb-posix:syscall-error'.  Use 0 for a liveness check."
  #-win32 (sb-posix:kill pid signal)
  #+win32 (declare (ignore pid signal))
  #+win32 (error "signal-process: not implemented on Windows"))

(defun exit (&key (code 0))
  "Terminate the current process with exit status CODE."
  (sb-ext:exit :code code))

(defun process-alive-p (pid)
  "Return T if a process with PID is still running and we can signal it,
NIL otherwise.  Implemented as `kill -0' on POSIX: a successful syscall
means the process exists; ESRCH means it doesn't.  Other errors (EPERM)
are treated as 'alive but inaccessible' -- the lockfile pattern can
still distinguish 'we own it' from 'somebody else does'."
  #-win32
  (handler-case
      (progn (sb-posix:kill pid 0) t)
    (sb-posix:syscall-error (e)
      ;; ESRCH = 3 on Linux/Darwin: process does not exist.
      (not (eql (sb-posix:syscall-errno e) 3))))
  #+win32 (declare (ignore pid))
  #+win32 (error "process-alive-p: not implemented on Windows"))
