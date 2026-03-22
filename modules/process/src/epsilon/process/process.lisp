(defpackage #:epsilon.process
  (:use #:cl)
  (:shadow find)
  (:require (epsilon.process.spawn spawn)
            (epsilon.process.io io))
  (:local-nicknames
   (log epsilon.log)
   (th epsilon.syntax)
   (tc epsilon.typeclass))
  (:export #:process
           #:start #:stop #:running-p
           #:subprocess #:make-subprocess
           #:run #:run-sync #:run-async
           #:process-output #:process-error
           #:process-exit-code #:process-pid
           #:kill-process #:wait-for-process
           #:terminate-gracefully
           #:pipe #:redirect
           #:with-environment
           #:command-line-escape
           #:escape-shell-arg
           #:build-command-line
           #:find-executable
           #:process-error-condition
           #:process-error-exit-code
           #:process-error-command
           #:subprocess-command
           #:subprocess-args
           #:command-not-found
           #:process-timeout-error
           #:*default-timeout*
           #:*default-shell*
           #:process-output-lines
           #:drain-stream
           #:monitor-process
           #:process-group #:make-process-group
           #:add-process #:start-group #:stop-group
           #:group-running-p #:wait-for-group
           #:list-children #:terminate-all-children)
  (:enter t))

(tc:deftypeclass process-lifecycle ()
  "Interface for process lifecycle management."
  (start (process &key &allow-other-keys)
    "Start a process.")
  (stop (process)
    "Stop a process.")
  (running-p (process)
    "Check if process is running."))

(defparameter *default-timeout* 30
  "Default timeout in seconds for process operations.")

(defparameter *default-shell* nil
  "Default shell to use for shell commands. If nil, auto-detect based on platform.")

(define-condition process-error-condition (error)
  ((command :initarg :command :reader process-error-command)
   (exit-code :initarg :exit-code :reader process-error-exit-code)
   (output :initarg :output :reader process-error-output)
   (error-output :initarg :error-output :reader process-error-error-output)
   (working-directory :initarg :working-directory :reader process-error-working-directory
                      :initform nil))
  (:report (lambda (condition stream)
             (format stream "Process command '~A' failed with exit code ~A"
                     (process-error-command condition)
                     (process-error-exit-code condition))
             (when (process-error-working-directory condition)
               (format stream " in directory ~A"
                       (process-error-working-directory condition))))))

(define-condition command-not-found (process-error-condition)
  ()
  (:report (lambda (condition stream)
             (format stream "Command not found: ~A"
                     (process-error-command condition)))))

(define-condition process-timeout-error (process-error-condition)
  ((timeout :initarg :timeout :reader process-timeout))
  (:report (lambda (condition stream)
             (format stream "Process '~A' timed out after ~A seconds"
                     (process-error-command condition)
                     (process-timeout condition)))))

(defclass subprocess ()
  ((command :initarg :command :reader subprocess-command)
   (args :initarg :args :reader subprocess-args :initform nil)
   (environment :initarg :environment :reader subprocess-environment :initform nil)
   (working-directory :initarg :working-directory :reader subprocess-working-directory :initform nil)
   (input :initarg :input :accessor subprocess-input :initform nil)
   (output :initarg :output :accessor subprocess-output :initform :stream)
   (error :initarg :error :reader subprocess-error :initform :stream)
   (process :accessor subprocess-process :initform nil)
   (exit-code :accessor subprocess-exit-code :initform nil)
   (output-stream :accessor subprocess-output-stream :initform nil)
   (error-stream :accessor subprocess-error-stream :initform nil)
   (start-time :accessor subprocess-start-time :initform nil)
   (use-shell :initarg :use-shell :reader subprocess-use-shell :initform nil)
   (shell :initarg :shell :reader subprocess-shell :initform nil))
  (:documentation "Represents a subprocess with full control over execution."))

(defun make-subprocess (command &key args environment working-directory
                               input output error)
  "Create a new subprocess object."
  (make-instance 'subprocess
                 :command command
                 :args args
                 :environment environment
                 :working-directory working-directory
                 :input input
                 :output output
                 :error error))

(defun find-executable-in-path (command)
  "Find the full path to an executable in the system PATH."
  (let ((path-var (or (sb-ext:posix-getenv "PATH")
                      "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin"))
        (path-separator #+win32 #\; #-win32 #\:))
    (dolist (dir (epsilon.sequence:realize (epsilon.string:split path-separator path-var)))
      (let ((full-path (format nil "~A/~A" dir command)))
        (when (probe-file full-path)
          (handler-case
              (when (logtest #o111 (sb-posix:stat-mode (sb-posix:stat full-path)))
                (return-from find-executable-in-path full-path))
            (error () nil)))))  ; Ignore stat errors
    nil))

(tc:definstance process-lifecycle subprocess
  (start (subprocess &key wait timeout)
    (declare (ignore timeout))
    (let* ((cmd (subprocess-command subprocess))
           (args (subprocess-args subprocess))
           (env (subprocess-environment subprocess))
           (wd (subprocess-working-directory subprocess))
           (output-setting (subprocess-output subprocess))
           (error-setting (subprocess-error subprocess))
           (merge-stderr-p (eq error-setting :output))
           (stdout-action (cond
                            (merge-stderr-p :merge-stderr)
                            ((eq output-setting :stream) :pipe)
                            (t :inherit)))
           (stderr-action (cond
                            (merge-stderr-p :inherit)
                            ((eq error-setting :stream) :pipe)
                            (t :inherit)))
           (is-absolute-path (and (> (length cmd) 0)
                                  (char= (char cmd 0) #\/)))
           (spec (spawn:make-spawn-spec cmd
                   :args (cons cmd args)
                   :stdout stdout-action
                   :stderr stderr-action
                   :environment (if env env :inherit)
                   :working-directory wd
                   :search-path (not is-absolute-path))))
      (handler-case
          (let ((handle (spawn:spawn spec)))
            (setf (subprocess-process subprocess) handle)
            (log:info "spawned pid ~D: ~A~{ ~A~}" (spawn:process-handle-pid handle) cmd args)
            ;; Create Lisp streams from raw fds for :stream outputs
            (when (spawn:process-handle-stdout-fd handle)
              (setf (subprocess-output-stream subprocess)
                    (sb-sys:make-fd-stream (spawn:process-handle-stdout-fd handle)
                                           :input t :external-format :utf-8
                                           :buffering :full)))
            (when (spawn:process-handle-stderr-fd handle)
              (setf (subprocess-error-stream subprocess)
                    (sb-sys:make-fd-stream (spawn:process-handle-stderr-fd handle)
                                           :input t :external-format :utf-8
                                           :buffering :full)))
            (when wait
              (spawn:wait-for-handle handle)
              (setf (subprocess-exit-code subprocess)
                    (spawn:process-handle-exit-code handle))
              (let ((elapsed-ms (when (spawn:process-handle-spawn-time handle)
                                  (round (* 1000 (/ (- (get-internal-real-time)
                                                       (spawn:process-handle-spawn-time handle))
                                                    internal-time-units-per-second))))))
                (log:debug "pid ~D exited ~D (~Dms)"
                           (spawn:process-handle-pid handle)
                           (or (spawn:process-handle-exit-code handle) -1)
                           (or elapsed-ms 0)))))
        (spawn:spawn-error (e)
          (log:error "spawn failed: ~A -- ~A" cmd e)
          (error 'process-error-condition
                 :command cmd
                 :exit-code -1
                 :output ""
                 :error-output (format nil "~A" e)))))
    subprocess)

  (stop (subprocess)
    (let ((handle (subprocess-process subprocess)))
      (when handle
        (spawn:handle-terminate handle)))
    subprocess)

  (running-p (subprocess)
    (let ((handle (subprocess-process subprocess)))
      (when handle
        (spawn:handle-poll handle)
        (not (spawn:handle-exited-p handle))))))

(defun process-pid (subprocess)
  "Get the process ID of the subprocess."
  (let ((handle (subprocess-process subprocess)))
    (when handle
      (spawn:process-handle-pid handle))))

(defun process-output (subprocess)
  "Get the output stream of the subprocess."
  (subprocess-output-stream subprocess))

(defun process-error (subprocess)
  "Get the error stream of the subprocess."
  (subprocess-error-stream subprocess))

(defun process-exit-code (subprocess)
  "Get the exit code of the subprocess."
  (subprocess-exit-code subprocess))

(defun kill-process (subprocess &optional (signal 15))
  "Kill the subprocess with the specified signal."
  (let ((handle (subprocess-process subprocess)))
    (when handle
      (spawn:handle-kill handle signal))))

(defun wait-for-process (subprocess &optional timeout)
  "Wait for the subprocess to complete, optionally with timeout."
  (let ((handle (subprocess-process subprocess)))
    (when handle
      (if timeout
          (progn
            (spawn:wait-for-handle handle timeout)
            (unless (spawn:handle-exited-p handle)
              (signal 'sb-ext:timeout)))
          (spawn:wait-for-handle handle))
      (when (spawn:handle-exited-p handle)
        (setf (subprocess-exit-code subprocess)
              (spawn:process-handle-exit-code handle))))
    subprocess))

(defun terminate-gracefully (subprocess &key (timeout 5))
  "Terminate a subprocess gracefully, using SIGTERM then SIGKILL if needed."
  (let ((handle (subprocess-process subprocess)))
    (when (and handle (not (spawn:handle-exited-p handle)))
      (handler-case
          (spawn:handle-terminate-gracefully handle :timeout timeout)
        (error ()
          nil))))
  subprocess)

(defun process-output-lines (stream line-callback)
  "Process output from stream line by line, calling callback for each line."
  (when stream
    (loop for line = (read-line stream nil)
          while line
          do (funcall line-callback line))))

(defun drain-stream (stream buffer)
  "Safely drain a stream into a buffer without blocking."
  (when stream
    (loop for line = (read-line stream nil)
          while line
          do (vector-push-extend line buffer))))

(defun monitor-process (subprocess &key
                        on-stdout on-stderr
                        on-exit timeout)
  "Monitor a running process with callbacks for output and completion."
  (let ((handle (subprocess-process subprocess)))
    (when (and handle (not (spawn:handle-exited-p handle)))
      (io:monitor-handle handle
                         :on-stdout on-stdout
                         :on-stderr on-stderr
                         :on-exit (lambda (code sig)
                                    (declare (ignore sig))
                                    (setf (subprocess-exit-code subprocess) code)
                                    (when on-exit
                                      (funcall on-exit code)))
                         :timeout timeout)))
  subprocess)

(defun command-line-escape (string)
  "Escape a string for safe use in command line."
  (format nil "'~A'" (substitute "'\"'\"'" "'" string)))

(defun detect-shell ()
  "Detect the appropriate shell for the current platform."
  (or *default-shell*
      #+windows (if (sb-ext:posix-getenv "COMSPEC")
                    (sb-ext:posix-getenv "COMSPEC")
                    "cmd.exe")
      #-windows (or (sb-ext:posix-getenv "SHELL") "/bin/sh")))

(defun escape-shell-arg (arg &key (shell :posix))
  "Escape an argument for safe shell execution."
  (case shell
    (:posix
     ;; For POSIX shells, single quotes are safest
     ;; We need to handle existing single quotes specially
     (if (position #\' arg)
         (format nil "'~A'"
                 (with-output-to-string (s)
                   (loop for char across arg
                         do (if (char= char #\')
                                (write-string "'\\''" s)
                                (write-char char s)))))
         (format nil "'~A'" arg)))
    (:windows
     ;; For Windows, use double quotes and escape special chars
     (format nil "\"~A\""
             (with-output-to-string (s)
               (loop for char across arg
                     do (case char
                          (#\" (write-string "\\\"" s))
                          (#\\ (write-string "\\\\" s))
                          (otherwise (write-char char s)))))))
    (otherwise
     (error "Unknown shell type: ~A" shell))))

(defun build-command-line (command args &key (shell :posix))
  "Build a properly escaped command line string."
  (format nil "~A~{ ~A~}"
          (if (position #\Space command)
              (escape-shell-arg command :shell shell)
              command)
          (mapcar (lambda (arg) (escape-shell-arg arg :shell shell)) args)))

(defun find-executable (name)
  "Find an executable in PATH and return its full path, or nil if not found."
  (let ((path-var (sb-ext:posix-getenv "PATH")))
    (when path-var
      (let ((paths (loop with start = 0
                         with separator = #+windows #\; #-windows #\:
                         for pos = (position separator path-var :start start)
                         collect (subseq path-var start pos)
                         while pos
                         do (setf start (1+ pos)))))
        (dolist (dir paths)
          (when (> (length dir) 0)
            (let ((full-path (merge-pathnames name (pathname (concatenate 'string dir "/")))))
              (when (and (probe-file full-path)
                         #+unix (let ((stat (sb-posix:stat (namestring full-path))))
                                  (not (zerop (logand (sb-posix:stat-mode stat)
                                                      #o111)))))
                (return-from find-executable (namestring full-path))))))
        ;; On Windows, also check with common extensions
        #+windows
        (dolist (ext '(".exe" ".bat" ".cmd"))
          (dolist (dir paths)
            (when (> (length dir) 0)
              (let ((full-path (merge-pathnames (concatenate 'string name ext)
                                                (pathname (concatenate 'string dir "/")))))
                (when (probe-file full-path)
                  (return-from find-executable (namestring full-path))))))))))
  nil)

(defun run-sync (command &key args environment working-directory
                         input timeout (error-on-failure t)
                         (stream-output nil)
                         (merge-error nil)
                         (shell nil)
                         (check-executable t))
  "Run a command synchronously and return output, error output, and exit code.
   OPTIONS:
   - stream-output: function called with each line of output as it's produced
   - merge-error: if true, merge stderr into stdout
   - shell: if true, run command through shell
   - check-executable: if true, verify command exists before running"
  (declare (ignore input))

  ;; Check if command exists if requested
  (when (and check-executable (not shell))
    (unless (or (position #\/ command)
                (find-executable command))
      (log:error "Command not found: ~A" command)
      (error 'command-not-found
             :command command
             :exit-code -1
             :output ""
             :error-output ""
             :working-directory working-directory)))

  ;; Handle shell execution
  (let ((actual-command command)
        (actual-args args))
    (log:debug "run-sync cmd ~a args ~a" actual-command actual-args)
    (when shell
      (let ((shell-cmd (detect-shell)))
        (setf actual-command shell-cmd
              actual-args (list "-c" (build-command-line command args)))))

    (let* ((is-absolute (and (> (length actual-command) 0)
                             (char= (char actual-command 0) #\/)))
           (stdout-action (if merge-error :merge-stderr :pipe))
           (stderr-action (if merge-error :inherit :pipe))
           (spec (spawn:make-spawn-spec actual-command
                   :args (cons actual-command actual-args)
                   :stdout stdout-action
                   :stderr stderr-action
                   :environment (if environment environment :inherit)
                   :working-directory working-directory
                   :search-path (not is-absolute))))

      (handler-case
          (let ((handle (spawn:spawn spec)))
            (log:info "run-sync pid ~D: ~A~{ ~A~}" (spawn:process-handle-pid handle) command args)
            (if stream-output
                ;; Streaming mode: use io:monitor-handle with callbacks
                (let ((output-lines nil)
                      (error-lines nil)
                      (exit-code nil))
                  (io:monitor-handle handle
                                     :on-stdout (lambda (line)
                                                  (push line output-lines)
                                                  (funcall stream-output line))
                                     :on-stderr (unless merge-error
                                                  (lambda (line)
                                                    (push line error-lines)))
                                     :on-exit (lambda (code sig)
                                                (declare (ignore sig))
                                                (setf exit-code code))
                                     :timeout timeout)
                  ;; Check for timeout (process still running)
                  (unless (spawn:handle-exited-p handle)
                    (log:warn "pid ~D timed out after ~As: ~A"
                              (spawn:process-handle-pid handle) timeout command)
                    (spawn:handle-kill handle)
                    (spawn:wait-for-handle handle 3)
                    (spawn:handle-close-streams handle)
                    (error 'process-timeout-error
                           :command (format nil "~A ~{~A~^ ~}" command args)
                           :timeout timeout
                           :exit-code -1
                           :output ""
                           :error-output ""
                           :working-directory working-directory))
                  (spawn:handle-close-streams handle)
                  (let ((output (if output-lines
                                    (format nil "~{~A~%~}" (nreverse output-lines))
                                    ""))
                        (error-output (if error-lines
                                         (format nil "~{~A~%~}" (nreverse error-lines))
                                         "")))
                    (setf exit-code (or exit-code
                                        (spawn:process-handle-exit-code handle)
                                        0))
                    (when (and error-on-failure (not (zerop exit-code)))
                      (error 'process-error-condition
                             :command (format nil "~A ~{~A~^ ~}" command args)
                             :exit-code exit-code
                             :output output
                             :error-output error-output
                             :working-directory working-directory))
                    (values output error-output exit-code)))

                ;; Non-streaming mode: use io:collect-output
                (multiple-value-bind (output error-output exit-code)
                    (io:collect-output handle :timeout timeout)
                  ;; Check for timeout
                  (unless (spawn:handle-exited-p handle)
                    (log:warn "pid ~D timed out after ~As: ~A"
                              (spawn:process-handle-pid handle) timeout command)
                    (spawn:handle-kill handle)
                    (spawn:wait-for-handle handle 3)
                    (spawn:handle-close-streams handle)
                    (error 'process-timeout-error
                           :command (format nil "~A ~{~A~^ ~}" command args)
                           :timeout timeout
                           :exit-code -1
                           :output ""
                           :error-output ""
                           :working-directory working-directory))
                  (spawn:handle-close-streams handle)
                  (when (and error-on-failure (not (zerop exit-code)))
                    (error 'process-error-condition
                           :command (format nil "~A ~{~A~^ ~}" command args)
                           :exit-code exit-code
                           :output output
                           :error-output error-output
                           :working-directory working-directory))
                  (values output error-output exit-code))))

        (spawn:spawn-error (e)
          (log:error "spawn failed: ~A ~{~A~^ ~} -- ~A" command args e)
          (error 'process-error-condition
                 :command (format nil "~A ~{~A~^ ~}" command args)
                 :exit-code -1
                 :output ""
                 :error-output (format nil "~A" e)
                 :working-directory working-directory))))))

(defun run-async (command &key args environment working-directory input)
  "Run a command asynchronously and return the subprocess object."
  (declare (ignore input))
  (let ((subprocess (make-subprocess command
                                     :args args
                                     :environment environment
                                     :working-directory working-directory
                                     :output :stream
                                     :error :stream)))
    (start subprocess :wait nil)
    subprocess))

(defun run (command &key args environment working-directory
                   input timeout (error-on-failure t) (async nil))
  "Run a command either synchronously or asynchronously."
  (if async
      (run-async command :args args :environment environment
                         :working-directory working-directory :input input)
      (run-sync command :args args :environment environment
                        :working-directory working-directory :input input
                        :timeout timeout :error-on-failure error-on-failure)))

(defmacro with-environment (environment &body body)
  "Execute body with modified environment variables."
  (let ((old-env (gensym "OLD-ENV")))
    `(let ((,old-env (copy-list sb-ext:*posix-argv*)))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (env-pair)
                          `(setf (getenv ,(car env-pair)) ,(cdr env-pair)))
                        environment)
              ,@body)
         (progn
           ,@(mapcar (lambda (env-pair)
                       `(setf (getenv ,(car env-pair)) nil))
                     environment))))))

(defun pipe (&rest commands)
  "Create a pipeline of commands via shell."
  (when commands
    (let* ((cmd-strings (mapcar (lambda (cmd)
                                  (if (listp cmd)
                                      (build-command-line (first cmd) (rest cmd))
                                      cmd))
                                commands))
           (pipeline (format nil "~{~A~^ | ~}" cmd-strings)))
      (run-sync (detect-shell)
                :args (list "-c" pipeline)
                :check-executable nil
                :error-on-failure nil))))

(defun redirect (command &key input-file output-file error-file append)
  "Run command with I/O redirection."
  ;; Build shell redirection command for append support
  (if append
      (let* ((parts (list command))
             (shell-cmd (progn
                          (when input-file
                            (push (format nil "< ~A" (escape-shell-arg (namestring input-file))) parts))
                          (when output-file
                            (push (format nil ">> ~A" (escape-shell-arg (namestring output-file))) parts))
                          (when error-file
                            (push (format nil "2>> ~A" (escape-shell-arg (namestring error-file))) parts))
                          (format nil "~{~A~^ ~}" (nreverse parts)))))
        (run-sync (detect-shell)
                  :args (list "-c" shell-cmd)
                  :check-executable nil
                  :error-on-failure nil))
      ;; Non-append: use spawn-spec with pathname I/O actions
      (let* ((stdin-action (if input-file (namestring input-file) :inherit))
             (stdout-action (if output-file (namestring output-file) :pipe))
             (stderr-action (if error-file (namestring error-file) :pipe))
             (is-absolute (and (> (length command) 0)
                               (char= (char command 0) #\/)))
             (spec (spawn:make-spawn-spec command
                     :args (list command)
                     :stdin stdin-action
                     :stdout stdout-action
                     :stderr stderr-action
                     :search-path (not is-absolute))))
        (let ((handle (spawn:spawn spec)))
          (unwind-protect
               (progn
                 (spawn:wait-for-handle handle)
                 (let ((output (if (spawn:process-handle-stdout-fd handle)
                                   (spawn:read-handle-output handle :stream :stdout)
                                   ""))
                       (err-out (if (spawn:process-handle-stderr-fd handle)
                                    (spawn:read-handle-output handle :stream :stderr)
                                    "")))
                   (values output err-out
                           (spawn:process-handle-exit-code handle))))
            (spawn:handle-close-streams handle))))))

;; Higher-level process management

(defclass process-group ()
  ((processes :initarg :processes :accessor process-group-processes :initform nil)
   (name :initarg :name :reader process-group-name))
  (:documentation "Manage a group of related processes."))

(defun make-process-group (name)
  "Create a new process group."
  (make-instance 'process-group :name name))

(defun add-process (group subprocess)
  "Add a subprocess to a process group."
  (push subprocess (process-group-processes group))
  group)

(defun start-group (group)
  "Start all processes in the group."
  (dolist (process (process-group-processes group))
    (start process :wait nil))
  group)

(defun stop-group (group)
  "Stop all processes in the group."
  (dolist (process (process-group-processes group))
    (when (running-p process)
      (stop process)))
  group)

(defun group-running-p (group)
  "Check if any process in the group is running."
  (some #'running-p (process-group-processes group)))

(defun wait-for-group (group &optional timeout)
  "Wait for all processes in the group to complete."
  (dolist (process (process-group-processes group))
    (wait-for-process process timeout))
  group)

;;; ============================================================================
;;; Process Census
;;; ============================================================================

(defun list-children ()
  "Return a list of plists describing all tracked child processes.
   Each plist contains :pid, :pgid, :status, :exit-code, :elapsed-seconds, :rusage."
  (mapcar (lambda (handle)
            (let ((elapsed (when (spawn:process-handle-spawn-time handle)
                            (/ (- (get-internal-real-time)
                                  (spawn:process-handle-spawn-time handle))
                               (float internal-time-units-per-second 1.0d0)))))
              (list :pid (spawn:process-handle-pid handle)
                    :pgid (spawn:process-handle-pgid handle)
                    :status (spawn:process-handle-status handle)
                    :exit-code (spawn:process-handle-exit-code handle)
                    :elapsed-seconds elapsed
                    :rusage (spawn:process-handle-rusage handle))))
          (spawn:registered-handles)))

(defun terminate-all-children (&key (timeout 5))
  "Gracefully terminate all tracked child processes.
   Sends SIGTERM, waits up to TIMEOUT seconds, then SIGKILL."
  (let ((handles (spawn:registered-handles)))
    (log:info "terminating ~D tracked children" (length handles))
    (dolist (handle handles)
      (unless (spawn:handle-exited-p handle)
        (handler-case
            (spawn:handle-terminate-gracefully handle :timeout timeout)
          (error (e)
            (log:warn "error terminating pid ~D: ~A"
                      (spawn:process-handle-pid handle) e)))))))
