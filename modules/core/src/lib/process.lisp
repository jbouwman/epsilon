(defpackage #:epsilon.process
  (:use #:cl)
  (:shadow find)
  (:local-nicknames
   (log epsilon.log))
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
           #:command-not-found
           #:process-timeout-error
           #:*default-timeout*
           #:*default-shell*
           #:process-output-lines
           #:drain-stream
           #:monitor-process
           #:process-group #:make-process-group
           #:add-process #:start-group #:stop-group
           #:group-running-p #:wait-for-group))

(in-package #:epsilon.process)

(defgeneric start (process &key &allow-other-keys)
  (:documentation "Start a process"))

(defgeneric stop (process)
  (:documentation "Stop a process"))

(defgeneric running-p (process)
  (:documentation "Check if process is running"))

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

(defmethod start ((subprocess subprocess) &key wait timeout)
  "Start the subprocess. If WAIT is true, wait for completion."
  (declare (ignore timeout)) ; Timeout handling not implemented yet
  (let* ((cmd (subprocess-command subprocess))
         (args (subprocess-args subprocess))
         (env (subprocess-environment subprocess))
         (wd (subprocess-working-directory subprocess))
         ;; Determine if the command is an absolute path
         (is-absolute-path (or (char= (char cmd 0) #\/)
                                #+win32 (and (>= (length cmd) 3)
                                            (char= (char cmd 1) #\:))))
         ;; For non-absolute paths, just use the command name with :search t
         ;; For absolute paths, use as-is with :search nil
         (actual-cmd cmd)
         (use-search (not is-absolute-path)))
    
    ;; sb-ext:run-program behaves differently based on output/error settings:
    ;; - When :output/:error are :stream, it returns multiple values: (output-stream error-stream process)
    ;; - When :output/:error are not :stream, it returns just the process
    ;; - If the process can't be started, it returns NIL in all cases
    (let ((output-setting (subprocess-output subprocess))
          (error-setting (subprocess-error subprocess)))
      (if (and (eq output-setting :stream) (eq error-setting :stream))
          ;; Case 1: Both output and error are streams - process has the streams
          (let ((process (apply #'sb-ext:run-program 
                                actual-cmd args
                                `(:directory ,wd
                                  :input ,(subprocess-input subprocess)
                                  :output ,output-setting
                                  :error ,error-setting
                                  :wait ,wait
                                  :search ,use-search
                                  ,@(when env `(:environment ,env))))))
            (unless process
              (error 'process-error-condition 
                     :command cmd
                     :exit-code -1
                     :output ""
                     :error-output (format nil "sb-ext:run-program returned NIL for command: ~A~%Args: ~A~%Working directory: ~A~%Environment: ~A~%~%This typically means:~%1. Command not found in PATH or at specified location~%2. Permission denied (file not executable)~%3. Invalid working directory~%4. Memory/resource limits reached" 
                                           actual-cmd args wd env)))
            (setf (subprocess-process subprocess) process
                  (subprocess-output-stream subprocess) (sb-ext:process-output process)
                  (subprocess-error-stream subprocess) (sb-ext:process-error process))
            (when wait
              (setf (subprocess-exit-code subprocess)
                    (sb-ext:process-exit-code process)))
            subprocess)
          ;; Case 2: Other output/error settings - returns just the process
          (let ((process (apply #'sb-ext:run-program 
                                actual-cmd args
                                `(:directory ,wd
                                  :input ,(subprocess-input subprocess)
                                  :output ,output-setting
                                  :error ,error-setting
                                  :wait ,wait
                                  :search ,use-search
                                  ,@(when env `(:environment ,env))))))
            (unless process
              (error 'process-error-condition 
                     :command cmd
                     :exit-code -1
                     :output ""
                     :error-output (format nil "sb-ext:run-program returned NIL for command: ~A~%Args: ~A~%Working directory: ~A~%Environment: ~A~%~%This typically means:~%1. Command not found in PATH or at specified location~%2. Permission denied (file not executable)~%3. Invalid working directory~%4. Memory/resource limits reached" 
                                           actual-cmd args wd env)))
            (setf (subprocess-process subprocess) process
                  (subprocess-output-stream subprocess) nil
                  (subprocess-error-stream subprocess) nil)
            (when wait
              (setf (subprocess-exit-code subprocess)
                    (sb-ext:process-exit-code process)))
            subprocess)))))

(defmethod stop ((subprocess subprocess))
  "Stop the subprocess."
  (when (subprocess-process subprocess)
    (sb-ext:process-kill (subprocess-process subprocess) sb-unix:sigterm))
  subprocess)

(defmethod running-p ((subprocess subprocess))
  "Check if the subprocess is still running."
  (when (subprocess-process subprocess)
    (eq :running (sb-ext:process-status (subprocess-process subprocess)))))

(defun process-pid (subprocess)
  "Get the process ID of the subprocess."
  (when (subprocess-process subprocess)
    (sb-ext:process-pid (subprocess-process subprocess))))

(defun process-output (subprocess)
  "Get the output stream of the subprocess."
  (when (subprocess-process subprocess)
    (sb-ext:process-output (subprocess-process subprocess))))

(defun process-error (subprocess)
  "Get the error stream of the subprocess."
  (when (subprocess-process subprocess)
    (sb-ext:process-error (subprocess-process subprocess))))

(defun process-exit-code (subprocess)
  "Get the exit code of the subprocess."
  (subprocess-exit-code subprocess))

(defun kill-process (subprocess &optional (signal 15))
  "Kill the subprocess with the specified signal."
  (when (subprocess-process subprocess)
    (sb-ext:process-kill (subprocess-process subprocess) signal)))

(defun wait-for-process (subprocess &optional timeout)
  "Wait for the subprocess to complete, optionally with timeout."
  (let ((process (subprocess-process subprocess)))
    (when process
      (if timeout
          (let ((start-time (get-universal-time)))
            (loop while (running-p subprocess)
                  when (> (- (get-universal-time) start-time) timeout)
                    do (signal 'sb-ext:timeout)
                  do (sleep 0.1)))
          (sb-ext:process-wait process))
      
      (setf (subprocess-exit-code subprocess)
            (sb-ext:process-exit-code process)))
    subprocess))

(defun terminate-gracefully (subprocess &key (timeout 5))
  "Terminate a subprocess gracefully, using SIGTERM then SIGKILL if needed."
  (when (and (subprocess-process subprocess) (running-p subprocess))
    (handler-case 
        (progn
          ;; First try SIGTERM
          (kill-process subprocess #+unix sb-unix:sigterm #-unix 15)
          ;; Wait for graceful shutdown
          (let ((start-time (get-universal-time)))
            (loop while (and (running-p subprocess)
                           (< (- (get-universal-time) start-time) timeout))
                  do (sleep 0.1)))
          ;; If still running, force kill
          (when (running-p subprocess)
            (kill-process subprocess #+unix sb-unix:sigkill #-unix 9)))
      (error () 
        ;; Process might already be dead
        nil)))
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
  (when (running-p subprocess)
    (let ((start-time (get-universal-time)))
      ;; Process output streams in a non-blocking way
      (loop while (running-p subprocess)
            do (progn
                 ;; Check stdout
                 (when (and on-stdout (process-output subprocess))
                   (handler-case
                       (let ((line (read-line (process-output subprocess) nil)))
                         (when line
                           (funcall on-stdout line)))
                     (end-of-file ())))
                 
                 ;; Check stderr
                 (when (and on-stderr (process-error subprocess))
                   (handler-case
                       (let ((line (read-line (process-error subprocess) nil)))
                         (when line
                           (funcall on-stderr line)))
                     (end-of-file ())))
                 
                 ;; Check timeout
                 (when (and timeout (> (- (get-universal-time) start-time) timeout))
                   (terminate-gracefully subprocess)
                   (return))
                 
                 ;; Small delay to prevent busy waiting
                 (sleep 0.01)))
      
      ;; Process completed, call exit callback if provided
      (when on-exit
        (funcall on-exit (process-exit-code subprocess)))))
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
  
  ;; Check if command exists if requested
  (when (and check-executable (not shell))
    (unless (or (position #\/ command) ;; absolute path
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
              actual-args (list #+windows "/c" #-windows "-c"
                                (build-command-line command args)))))
    
    (let ((subprocess (make-subprocess actual-command
                                       :args actual-args
                                       :environment environment
                                       :working-directory working-directory
                                       :input input
                                       :output :stream
                                       :error (if merge-error :output :stream))))
      
      (setf (subprocess-start-time subprocess) (get-universal-time))
      (start subprocess :wait (not stream-output))

      (log:debug "A")

      (handler-case
          (if stream-output
              ;; Stream processing mode
              (let ((output-lines '())
                    (error-lines '()))
                (unwind-protect
                     (progn
                       ;; Process output streams
                       (when (process-output subprocess)
                         (loop for line = (read-line (process-output subprocess) nil)
                               while line
                               do (push line output-lines)
                                  (funcall stream-output line)))
                       ;; Process error stream if separate
                       (when (and (not merge-error) (process-error subprocess))
                         (loop for line = (read-line (process-error subprocess) nil)
                               while line
                               do (push line error-lines)))
                       ;; Wait for completion
                       (log:debug "B")
                       (wait-for-process subprocess timeout))
                  ;; Always get exit code
                  (when (subprocess-process subprocess)
                    (setf (subprocess-exit-code subprocess)
                          (sb-ext:process-exit-code (subprocess-process subprocess)))))
                
                (let ((output (format nil "~{~A~%~}" (nreverse output-lines)))
                      (error-output (format nil "~{~A~%~}" (nreverse error-lines)))
                      (exit-code (process-exit-code subprocess)))
                  
                  (log:debug "C")
                  (when (and error-on-failure (not (zerop exit-code)))
                    (error 'process-error-condition
                           :command (format nil "~A ~{~A~^ ~}" command args)
                           :exit-code exit-code
                           :output output
                           :error-output error-output
                           :working-directory working-directory))
                  
                  (values output error-output exit-code)))
              
              ;; Non-streaming mode (original behavior)
              (progn
                (wait-for-process subprocess timeout)
                (log:debug "D")
                
                (let ((output "")
                      (error-output "")
                      (exit-code (process-exit-code subprocess)))
                  
                  (when (process-output subprocess)
                    (setf output (with-output-to-string (s)
                                   (loop for line = (read-line (process-output subprocess) nil)
                                         while line
                                         do (write-line line s)))))
                  
                  (when (and (not merge-error) (process-error subprocess))
                    (setf error-output (with-output-to-string (s)
                                         (loop for line = (read-line (process-error subprocess) nil)
                                               while line
                                               do (write-line line s)))))
                  
                  (when (and error-on-failure (not (zerop exit-code)))
                    (error 'process-error-condition
                           :command (format nil "~A ~{~A~^ ~}" command args)
                           :exit-code exit-code
                           :output output
                           :error-output error-output
                           :working-directory working-directory))
                  
                  (values output error-output exit-code))))
        
        (sb-ext:timeout ()
          (when (running-p subprocess)
            (kill-process subprocess))
          (error 'process-timeout-error
                 :command (format nil "~A ~{~A~^ ~}" command args)
                 :timeout timeout
                 :exit-code -1
                 :output ""
                 :error-output ""
                 :working-directory working-directory))))))

(defun run-async (command &key args environment working-directory input)
  "Run a command asynchronously and return the subprocess object."
  (let ((subprocess (make-subprocess command
                                     :args args
                                     :environment environment
                                     :working-directory working-directory
                                     :input input
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
  "Create a pipeline of commands."
  (let ((processes nil)
        (current-input nil))
    (unwind-protect
         (progn
           (dolist (cmd commands)
             (let ((subprocess (if (listp cmd)
                                   (apply #'make-subprocess cmd)
                                   (make-subprocess cmd))))
               (when current-input
                 (setf (subprocess-input subprocess) current-input))
               (setf (subprocess-output subprocess) :stream)
               (start subprocess :wait nil)
               (push subprocess processes)
               (setf current-input (process-output subprocess))))
           
           (dolist (process (reverse processes))
             (wait-for-process process))
           
           (let ((final-process (first processes)))
             (values (process-output final-process)
                     (process-error final-process)
                     (process-exit-code final-process))))
      (dolist (process processes)
        (when (running-p process)
          (stop process))))))

(defun redirect (command &key input-file output-file error-file append)
  "Run command with I/O redirection."
  (let ((subprocess (make-subprocess command
                                     :input (when input-file
                                              (open input-file :direction :input))
                                     :output (when output-file
                                               (open output-file 
                                                     :direction :output
                                                     :if-exists (if append :append :supersede)
                                                     :if-does-not-exist :create))
                                     :error (when error-file
                                              (open error-file 
                                                    :direction :output
                                                    :if-exists (if append :append :supersede)
                                                    :if-does-not-exist :create)))))
    (unwind-protect
         (progn
           (start subprocess :wait t)
           (wait-for-process subprocess)
           (values (process-output subprocess)
                   (process-error subprocess)
                   (process-exit-code subprocess)))
      (when (subprocess-input subprocess)
        (close (subprocess-input subprocess)))
      (when (subprocess-output subprocess)
        (close (subprocess-output subprocess)))
      (when (subprocess-error subprocess)
        (close (subprocess-error subprocess))))))

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
