(defpackage #:epsilon.process
  (:use #:cl)
  (:shadow find)
  (:export #:process
           #:start #:stop #:running-p
           #:subprocess #:make-subprocess
           #:run #:run-sync #:run-async
           #:process-output #:process-error
           #:process-exit-code #:process-pid
           #:kill-process #:wait-for-process
           #:pipe #:redirect
           #:with-environment
           #:command-line-escape
           #:process-error-condition
           #:*default-timeout*
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

(define-condition process-error-condition (error)
  ((command :initarg :command :reader process-error-command)
   (exit-code :initarg :exit-code :reader process-error-exit-code)
   (output :initarg :output :reader process-error-output)
   (error-output :initarg :error-output :reader process-error-error-output))
  (:report (lambda (condition stream)
             (format stream "Process command '~A' failed with exit code ~A"
                     (process-error-command condition)
                     (process-error-exit-code condition)))))

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
   (error-stream :accessor subprocess-error-stream :initform nil))
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

(defmethod start ((subprocess subprocess) &key wait timeout)
  "Start the subprocess. If WAIT is true, wait for completion."
  (declare (ignore timeout)) ; Timeout handling not implemented yet
  (let* ((cmd (subprocess-command subprocess))
         (args (subprocess-args subprocess))
         (env (subprocess-environment subprocess))
         (wd (subprocess-working-directory subprocess)))
    
    (multiple-value-bind (output-stream error-stream process)
        #+sbcl
        (sb-ext:run-program cmd args
                            :environment env
                            :directory wd
                            :input (subprocess-input subprocess)
                            :output (subprocess-output subprocess)
                            :error (subprocess-error subprocess)
                            :wait wait
                            :search t)
        #+ccl
        (ccl:run-program cmd args
                         :environment env
                         :directory wd
                         :input (subprocess-input subprocess)
                         :output (subprocess-output subprocess)
                         :error (subprocess-error subprocess)
                         :wait wait)
        #-(or sbcl ccl)
        (error "Process control not implemented for this Lisp implementation")
      
      (setf (subprocess-process subprocess) process
            (subprocess-output-stream subprocess) output-stream
            (subprocess-error-stream subprocess) error-stream)
      
      (when wait
        (setf (subprocess-exit-code subprocess)
              #+sbcl (sb-ext:process-exit-code process)
              #+ccl (ccl:external-process-status process)
              #-(or sbcl ccl) 0))
      
      subprocess)))

(defmethod stop ((subprocess subprocess))
  "Stop the subprocess."
  (when (subprocess-process subprocess)
    #+sbcl
    (sb-ext:process-kill (subprocess-process subprocess) sb-unix:sigterm)
    #+ccl
    (ccl:signal-external-process (subprocess-process subprocess) 15)
    #-(or sbcl ccl)
    (error "Process termination not implemented for this Lisp implementation"))
  subprocess)

(defmethod running-p ((subprocess subprocess))
  "Check if the subprocess is still running."
  (when (subprocess-process subprocess)
    #+sbcl
    (eq :running (sb-ext:process-status (subprocess-process subprocess)))
    #+ccl
    (eq :running (ccl:external-process-status (subprocess-process subprocess)))
    #-(or sbcl ccl)
    nil))

(defun process-pid (subprocess)
  "Get the process ID of the subprocess."
  (when (subprocess-process subprocess)
    #+sbcl
    (sb-ext:process-pid (subprocess-process subprocess))
    #+ccl
    (ccl:external-process-id (subprocess-process subprocess))
    #-(or sbcl ccl)
    nil))

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
  (when (subprocess-process subprocess)
    #+sbcl
    (sb-ext:process-kill (subprocess-process subprocess) signal)
    #+ccl
    (ccl:signal-external-process (subprocess-process subprocess) signal)
    #-(or sbcl ccl)
    (error "Process signaling not implemented for this Lisp implementation")))

(defun wait-for-process (subprocess &optional timeout)
  "Wait for the subprocess to complete, optionally with timeout."
  (let ((process (subprocess-process subprocess)))
    (when process
      (if timeout
          (let ((start-time (get-universal-time)))
            (loop while (running-p subprocess)
                  when (> (- (get-universal-time) start-time) timeout)
                    do (error "Process timeout after ~A seconds" timeout)
                  do (sleep 0.1)))
          (progn
            #+sbcl
            (sb-ext:process-wait process)
            #+ccl
            (ccl:external-process-wait process)
            #-(or sbcl ccl)
            (loop while (running-p subprocess) do (sleep 0.1))))
      
      (setf (subprocess-exit-code subprocess)
            #+sbcl (sb-ext:process-exit-code process)
            #+ccl (ccl:external-process-status process)
            #-(or sbcl ccl) 0))
    subprocess))

(defun command-line-escape (string)
  "Escape a string for safe use in command line."
  (format nil "'~A'" (substitute "'\"'\"'" "'" string)))

(defun run-sync (command &key args environment working-directory 
                         input timeout (error-on-failure t))
  "Run a command synchronously and return output, error output, and exit code."
  (let ((subprocess (make-subprocess command
                                     :args args
                                     :environment environment
                                     :working-directory working-directory
                                     :input input
                                     :output :stream
                                     :error :stream)))
    (start subprocess :wait t)
    (wait-for-process subprocess timeout)
    
    (let ((output "")
          (error-output "")
          (exit-code (process-exit-code subprocess)))
      
      (when (process-output subprocess)
        (setf output (with-output-to-string (s)
                       (loop for line = (read-line (process-output subprocess) nil)
                             while line
                             do (write-line line s)))))
      
      (when (process-error subprocess)
        (setf error-output (with-output-to-string (s)
                             (loop for line = (read-line (process-error subprocess) nil)
                                   while line
                                   do (write-line line s)))))
      
      (when (and error-on-failure (not (zerop exit-code)))
        (error 'process-error-condition
               :command (format nil "~A ~{~A~^ ~}" command args)
               :exit-code exit-code
               :output output
               :error-output error-output))
      
      (values output error-output exit-code))))

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
    `(let ((,old-env (copy-list #+sbcl sb-ext:*posix-argv*
                                #+ccl ccl:*command-line-argument-list*
                                #-(or sbcl ccl) nil)))
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
