;;;; Parallel Build Support
;;;;
;;;; This module provides parallel compilation capabilities while ensuring
;;;; proper package loading and dependency ordering.

(defpackage epsilon.loader.parallel-build
  (:use cl)
  (:local-nicknames
   (loader epsilon.loader)
   (seq epsilon.sequence)
   (log epsilon.log)
   (map epsilon.map)
   (fs epsilon.sys.fs)
   (path epsilon.path)
   (log epsilon.log))
  (:export
   #:compile-parallel
   #:*parallel-workers*
   #:*use-parallel-compilation*))

(in-package :epsilon.loader.parallel-build)

(defparameter *parallel-workers* 4
  "Number of parallel compilation workers")

(defparameter *use-parallel-compilation* nil
  "Whether to use parallel compilation")

(defstruct compilation-task
  "A unit of compilation work"
  build-input
  dependencies
  status ; :pending :compiling :compiled :loaded :failed
  result
  load-lock)

(defstruct compilation-batch
  "A batch of files that can be compiled in parallel"
  tasks)

(defun group-by-dependencies (build-inputs)
  "Group build inputs into batches that can be compiled in parallel.
   Files in the same batch have no dependencies on each other."
  (let* ((tasks (map:make-map)))
    ;; Create tasks
    (seq:each (lambda (build-input)
                (let* ((source-info (loader::source-info build-input))
                       (defines (loader::source-info-defines source-info))
                       (requires (loader::source-info-requires source-info))
                       (task (make-compilation-task
                              :build-input build-input
                              :dependencies requires
                              :status :pending
                              :load-lock (sb-thread:make-mutex 
                                         :name (format nil "load-~A" defines)))))
                  (when defines
                    (map:assoc! tasks defines task))))
              build-inputs)
    
    ;; Group into batches
    (loop while (seq:not-empty-p 
                 (seq:filter (lambda (task)
                              (eq (compilation-task-status task) :pending))
                            (map:vals tasks)))
          collect (let ((batch-tasks '()))
                    (map:each (lambda (name task)
                               (declare (ignore name))
                               (when (and (eq (compilation-task-status task) :pending)
                                        (every (lambda (dep)
                                                (let ((dep-task (map:get tasks dep)))
                                                  (or (null dep-task)
                                                      (eq (compilation-task-status dep-task) :loaded))))
                                              (compilation-task-dependencies task)))
                                 (push task batch-tasks)
                                 (setf (compilation-task-status task) :compiling)))
                             tasks)
                    (make-compilation-batch :tasks batch-tasks)))))

(defun compile-in-process (build-input)
  "Compile a file in the current process (not in a separate thread)"
  (let ((result (make-instance 'loader::build-result
                              :build-input build-input)))
    (setf (loader::start-time result) (get-internal-real-time))
    (handler-case
        (progn
          (fs:make-dirs (path:path-parent 
                        (path:make-path 
                         (path:path-from-uri (loader::target-uri build-input)))))
          (compile-file (path:path-from-uri (loader::source-uri build-input))
                       :output-file (path:path-from-uri (loader::target-uri build-input))
                       :verbose nil
                       :print nil)
          (setf (loader::compilation-status result) :success))
      (warning (w)
        (push w (loader::compilation-warnings result)))
      (error (e)
        (push e (loader::compilation-errors result))
        (setf (loader::compilation-status result) :failed)))
    (setf (loader::end-time result) (get-internal-real-time))
    result))

(defun load-in-main-thread (task)
  "Load a compiled file in the main thread with proper locking"
  (sb-thread:with-mutex ((compilation-task-load-lock task))
    (handler-case
        (progn
          (load (path:path-from-uri 
                 (loader::target-uri (compilation-task-build-input task))))
          (setf (compilation-task-status task) :loaded)
          t)
      (error (e)
       (log:error "Failed to load ~A: ~A" 
                   (loader::source-uri (compilation-task-build-input task)) e)
        (setf (compilation-task-status task) :failed)
        nil))))

(defun compile-batch-parallel (batch)
  "Compile a batch of files in parallel"
  (let* ((tasks (compilation-batch-tasks batch))
         (threads '()))
    ;; Start compilation threads
    (dolist (task tasks)
      (let ((thread (sb-thread:make-thread
                    (lambda ()
                      (let ((result (compile-in-process 
                                    (compilation-task-build-input task))))
                        (setf (compilation-task-result task) result)
                        (if (loader::compilation-errors result)
                            (setf (compilation-task-status task) :failed)
                            (setf (compilation-task-status task) :compiled))))
                    :name (format nil "compile-~A" 
                                 (loader::source-info-defines 
                                  (loader::source-info 
                                   (compilation-task-build-input task)))))))
        (push thread threads)))
    
    ;; Wait for all compilations to complete
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    
    ;; Load compiled files in dependency order in main thread
    (dolist (task tasks)
      (when (eq (compilation-task-status task) :compiled)
        (load-in-main-thread task)))))

(defun compile-batch-sequential (environment batch)
  "Compile a batch of files sequentially (fallback)"
  (dolist (task (compilation-batch-tasks batch))
    (let ((result (loader::compile-source environment (compilation-task-build-input task))))
      (setf (compilation-task-result task) result)
      (if (loader::compilation-errors result)
          (setf (compilation-task-status task) :failed)
          (setf (compilation-task-status task) :loaded)))))

(defun compile-parallel (environment build-inputs &key force parallel)
  "Compile build inputs with optional parallelism"
  (if (and parallel (> (seq:count build-inputs) 1))
      ;; Filter inputs that need compilation vs just loading
      (let* ((to-compile '())
             (to-load '())
             (results '()))
        ;; Separate files that need compilation from those that just need loading
        (seq:each (lambda (build-input)
                   (if (or force
                          (member (loader::build-input-status build-input) 
                                  '(:target-missing :source-newer)))
                       (push build-input to-compile)
                       (push build-input to-load)))
                 build-inputs)
        
        ;; Load already compiled files first
        (dolist (build-input (nreverse to-load))
          (push (loader::load-source environment build-input) results))
        
        ;; Then compile remaining files in parallel
        (when to-compile
          (let ((batches (group-by-dependencies (seq:seq (nreverse to-compile)))))
            (log:info "Parallel compilation: ~D files in ~D batches" 
                     (length to-compile) (length batches))
            (dolist (batch batches)
              (if (> (length (compilation-batch-tasks batch)) 1)
                  (compile-batch-parallel batch)
                  (compile-batch-sequential environment batch)))
            ;; Add compilation results
            (dolist (batch batches)
              (dolist (task (compilation-batch-tasks batch))
                (push (compilation-task-result task) results)))))
        
        (nreverse results))
      ;; Sequential compilation (existing behavior)
      (seq:map (lambda (build-input)
                (if force
                    (loader::compile-source environment build-input)
                    (case (loader::build-input-status build-input)
                      ((:target-missing :source-newer)
                       (loader::compile-source environment build-input))
                      (t
                       (loader::load-source environment build-input)))))
              build-inputs)))
