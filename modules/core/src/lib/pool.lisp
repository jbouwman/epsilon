;;;; Generic Object Pool
;;;;
;;;; A reusable, thread-safe object pool abstraction inspired by Rust's Arc<Mutex<T>>
;;;; and Go's sync.Pool. This provides the foundation for connection pools, worker pools,
;;;; and other resource pooling throughout epsilon.

(defpackage :epsilon.pool
  (:use :cl)
  (:local-nicknames
   (#:thread #:epsilon.sys.thread))
  (:export
   ;; Pool creation and management
   #:pool
   #:create-pool
   #:pool-p
   #:destroy-pool
   #:pool-size
   #:pool-capacity
   
   ;; Resource acquisition/release
   #:acquire
   #:release
   #:with-resource
   #:try-acquire
   
   ;; Pool configuration
   #:pool-config
   #:make-pool-config
   #:pool-factory
   #:pool-destroyer
   #:pool-validator
   #:pool-max-size
   #:pool-min-size
   #:pool-idle-timeout
   #:pool-acquire-timeout
   #:pool-config-factory
   #:pool-config-destroyer
   #:pool-config-validator
   #:pool-config-max-size
   #:pool-config-min-size
   #:pool-config-idle-timeout
   #:pool-config-acquire-timeout
   #:pool-config-validation-on-acquire
   #:pool-config-validation-on-release
   
   ;; Pool statistics
   #:pool-stats
   #:pool-stats-p
   #:make-pool-stats
   #:pool-stats-created
   #:pool-stats-destroyed
   #:pool-stats-acquired
   #:pool-stats-released
   #:pool-stats-timeouts
   #:pool-stats-validation-failures
   
   ;; Pool health
   #:pool-health-check
   #:pool-clear
   #:pool-warm-up))

(in-package :epsilon.pool)

;;;; Utility Macros

(defmacro when-let (bindings &body body)
  "Execute body when expr is non-nil, binding it to var"
  (destructuring-bind ((var expr)) bindings
    `(let ((,var ,expr))
       (when ,var
         ,@body))))

(defmacro if-let (bindings then &optional else)
  "Execute then when expr is non-nil, binding it to var, else otherwise"
  (destructuring-bind ((var expr)) bindings
    `(let ((,var ,expr))
       (if ,var
           ,then
           ,else))))

;;;; Pool Configuration

(defstruct pool-config
  "Configuration for object pool behavior"
  (factory nil :type (or null function))
  (destroyer nil :type (or null function)) 
  (validator nil :type (or null function))
  (max-size 10 :type (integer 1 *))
  (min-size 0 :type (integer 0 *))
  (idle-timeout 300 :type (integer 0 *)) ; seconds
  (acquire-timeout 30 :type (integer 0 *)) ; seconds
  (validation-on-acquire t :type boolean)
  (validation-on-release t :type boolean))

(defstruct pool-stats
  "Pool operation statistics"
  (created 0 :type integer)
  (destroyed 0 :type integer)
  (acquired 0 :type integer)
  (released 0 :type integer)
  (timeouts 0 :type integer)
  (validation-failures 0 :type integer)
  (last-maintenance 0 :type integer))

(defstruct pooled-resource
  "Wrapper for pooled resources with metadata"
  resource
  (created-at 0 :type integer)
  (last-used-at 0 :type integer)
  (use-count 0 :type integer)
  (valid-p t :type boolean))

;;;; Core Pool Implementation

(defstruct pool
  "Thread-safe object pool with lifecycle management"
  (available '() :type list) ; Available resources
  (in-use (make-hash-table :test 'eq) :type hash-table) ; Resource -> checkout time
  (config (make-pool-config) :type pool-config)
  (stats (make-pool-stats) :type pool-stats)
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (condition (sb-thread:make-waitqueue) :type sb-thread:waitqueue)
  (maintenance-thread nil :type (or null sb-thread:thread))
  (shutdown-p nil :type boolean))

;;;; Pool Creation and Destruction

(defun create-pool (&key factory destroyer validator 
                         (max-size 10) (min-size 0) 
                         (idle-timeout 300) (acquire-timeout 30)
                         (validation-on-acquire t) (validation-on-release t)
                         warm-up-p)
  "Create a new object pool
  
  - FACTORY: Function () -> resource. Creates new resources.
  - DESTROYER: Function (resource) -> nil. Cleans up resources.
  - VALIDATOR: Function (resource) -> boolean. Validates resource health.
  - MAX-SIZE: Maximum pool size
  - MIN-SIZE: Minimum pool size to maintain
  - IDLE-TIMEOUT: Seconds before idle resources are destroyed
  - ACQUIRE-TIMEOUT: Seconds to wait for available resource
  - VALIDATION-ON-*: Whether to validate on acquire/release
  - WARM-UP-P: Whether to pre-populate with min-size resources"
  
  (unless factory
    (error "Pool factory function is required"))
  
  (let* ((config (make-pool-config 
                  :factory factory
                  :destroyer destroyer
                  :validator validator
                  :max-size max-size
                  :min-size min-size
                  :idle-timeout idle-timeout
                  :acquire-timeout acquire-timeout
                  :validation-on-acquire validation-on-acquire
                  :validation-on-release validation-on-release))
         (pool (make-pool :config config
                         :stats (make-pool-stats))))
    
    ;; Start maintenance thread
    (setf (pool-maintenance-thread pool)
          (sb-thread:make-thread
           (lambda () (maintenance-loop pool))
           :name "pool-maintenance"))
    
    ;; Warm up pool if requested
    (when warm-up-p
      (pool-warm-up pool))
    
    pool))

(defun destroy-pool (pool)
  "Shutdown pool and cleanup all resources"
  ;; First signal shutdown and get maintenance thread
  (let ((maintenance-thread nil))
    (sb-thread:with-mutex ((pool-lock pool))
      (setf (pool-shutdown-p pool) t)
      (setf maintenance-thread (pool-maintenance-thread pool))
      (setf (pool-maintenance-thread pool) nil)
      
      ;; Cleanup available resources
      (dolist (wrapped (pool-available pool))
        (safely-destroy-resource pool (pooled-resource-resource wrapped)))
      (setf (pool-available pool) '())
      
      ;; Note: In-use resources will be cleaned up when released
      
      ;; Signal waiting threads
      (sb-thread:condition-broadcast (pool-condition pool)))
    
    ;; Shutdown maintenance thread outside of mutex
    (when maintenance-thread
      (sb-thread:join-thread maintenance-thread))))

;;;; Resource Acquisition and Release

(defun acquire (pool &key (timeout nil))
  "Acquire a resource from the pool
  
  Returns the resource or signals an error on timeout.
  Use WITH-RESOURCE for automatic cleanup."
  
  (when (pool-shutdown-p pool)
    (error "Pool has been shutdown"))
  
  (let ((timeout-time (when timeout (+ (get-universal-time) timeout)))
        (config (pool-config pool)))
    
    (loop
      (sb-thread:with-mutex ((pool-lock pool))
        ;; Try to get available resource
        (when-let ((wrapped (pop (pool-available pool))))
          (when (validate-resource pool wrapped)
            ;; Move to in-use and return
            (setf (gethash (pooled-resource-resource wrapped) (pool-in-use pool))
                  (get-universal-time))
            (incf (pooled-resource-use-count wrapped))
            (setf (pooled-resource-last-used-at wrapped) (get-universal-time))
            (incf (pool-stats-acquired (pool-stats pool)))
            (return (pooled-resource-resource wrapped)))
          ;; Resource failed validation, destroy it
          (safely-destroy-resource pool (pooled-resource-resource wrapped)))
        
        ;; No valid available resource, try to create new one
        (when (< (total-pool-size pool) (pool-config-max-size config))
          (when-let ((resource (safely-create-resource pool)))
            (let ((wrapped (make-pooled-resource 
                           :resource resource
                           :created-at (get-universal-time)
                           :last-used-at (get-universal-time))))
              (setf (gethash resource (pool-in-use pool)) (get-universal-time))
              (incf (pooled-resource-use-count wrapped))
              (incf (pool-stats-acquired (pool-stats pool)))
              (return resource))))
        
        ;; Pool full, wait with timeout
        (let ((remaining-time (when timeout-time
                                (- timeout-time (get-universal-time)))))
          (when (and timeout-time (<= remaining-time 0))
            (incf (pool-stats-timeouts (pool-stats pool)))
            (error "Pool acquire timeout after ~A seconds" timeout))
          
          (sb-thread:condition-wait (pool-condition pool) (pool-lock pool)
                                    :timeout remaining-time))))))

(defun try-acquire (pool)
  "Try to acquire a resource without blocking
  
  Returns resource or NIL if none available immediately."
  (when (pool-shutdown-p pool)
    (return-from try-acquire nil))
  
  (sb-thread:with-mutex ((pool-lock pool))
    (when-let ((wrapped (pop (pool-available pool))))
      (when (validate-resource pool wrapped)
        ;; Move to in-use and return
        (setf (gethash (pooled-resource-resource wrapped) (pool-in-use pool))
              (get-universal-time))
        (incf (pooled-resource-use-count wrapped))
        (setf (pooled-resource-last-used-at wrapped) (get-universal-time))
        (incf (pool-stats-acquired (pool-stats pool)))
        (return-from try-acquire (pooled-resource-resource wrapped)))
      ;; Resource failed validation
      (safely-destroy-resource pool (pooled-resource-resource wrapped)))
    
    ;; Try to create new resource if pool not full
    (when (< (total-pool-size pool) (pool-config-max-size (pool-config pool)))
      (when-let ((resource (safely-create-resource pool)))
        (let ((wrapped (make-pooled-resource 
                       :resource resource
                       :created-at (get-universal-time)
                       :last-used-at (get-universal-time))))
          (setf (gethash resource (pool-in-use pool)) (get-universal-time))
          (incf (pooled-resource-use-count wrapped))
          (incf (pool-stats-acquired (pool-stats pool)))
          resource)))))

(defun release (pool resource)
  "Release a resource back to the pool"
  (when (pool-shutdown-p pool)
    ;; Pool shutdown, just destroy the resource
    (safely-destroy-resource pool resource)
    (return-from release))
  
  (sb-thread:with-mutex ((pool-lock pool))
    ;; Remove from in-use
    (remhash resource (pool-in-use pool))
    (incf (pool-stats-released (pool-stats pool)))
    
    ;; Validate if configured
    (if (and (pool-config-validation-on-release (pool-config pool))
             (not (validate-resource-direct pool resource)))
        ;; Failed validation, destroy
        (safely-destroy-resource pool resource)
        ;; Return to available pool
        (let ((wrapped (make-pooled-resource 
                       :resource resource
                       :last-used-at (get-universal-time))))
          (push wrapped (pool-available pool))
          ;; Signal waiting threads
          (sb-thread:condition-notify (pool-condition pool))))))

(defmacro with-resource ((var pool &key timeout) &body body)
  "Execute body with an acquired resource, automatically releasing it"
  (let ((pool-var (gensym "POOL"))
        (resource-var (gensym "RESOURCE")))
    `(let* ((,pool-var ,pool)
            (,resource-var (acquire ,pool-var :timeout ,timeout))
            (,var ,resource-var))
       (unwind-protect
            (progn ,@body)
         (when ,resource-var
           (release ,pool-var ,resource-var))))))

;;;; Pool Maintenance and Health

(defun maintenance-loop (pool)
  "Background maintenance loop for pool cleanup"
  (loop
    ;; Check for shutdown more frequently during sleep
    (dotimes (i 60) ; Check every second for 60 seconds
      (when (pool-shutdown-p pool)
        (return-from maintenance-loop))
      (sleep 1))
    
    ;; Check again before maintenance
    (when (pool-shutdown-p pool)
      (return))
    
    (handler-case
        (perform-maintenance pool)
      (error (e)
        (warn "Pool maintenance error: ~A" e)))))

(defun perform-maintenance (pool)
  "Perform pool maintenance: cleanup expired resources, ensure min size"
  (sb-thread:with-mutex ((pool-lock pool))
    (let ((config (pool-config pool))
          (stats (pool-stats pool))
          (current-time (get-universal-time)))
      
      ;; Remove expired idle resources
      (setf (pool-available pool)
            (remove-if 
             (lambda (wrapped)
               (let ((idle-time (- current-time (pooled-resource-last-used-at wrapped))))
                 (when (> idle-time (pool-config-idle-timeout config))
                   (safely-destroy-resource pool (pooled-resource-resource wrapped))
                   t)))
             (pool-available pool)))
      
      ;; Ensure minimum pool size
      (let ((current-size (total-pool-size pool))
            (min-size (pool-config-min-size config)))
        (when (< current-size min-size)
          (dotimes (i (- min-size current-size))
            (when-let ((resource (safely-create-resource pool)))
              (push (make-pooled-resource 
                     :resource resource
                     :created-at current-time
                     :last-used-at current-time)
                    (pool-available pool))))))
      
      (setf (pool-stats-last-maintenance stats) current-time))))

(defun pool-health-check (pool)
  "Validate all available resources in pool"
  (sb-thread:with-mutex ((pool-lock pool))
    (setf (pool-available pool)
          (remove-if-not 
           (lambda (wrapped)
             (if (validate-resource pool wrapped)
                 t
                 (progn
                   (safely-destroy-resource pool (pooled-resource-resource wrapped))
                   nil)))
           (pool-available pool)))))

(defun pool-clear (pool)
  "Remove all available resources from pool"
  (sb-thread:with-mutex ((pool-lock pool))
    (dolist (wrapped (pool-available pool))
      (safely-destroy-resource pool (pooled-resource-resource wrapped)))
    (setf (pool-available pool) '())))

(defun pool-warm-up (pool)
  "Pre-populate pool with minimum number of resources"
  (let ((min-size (pool-config-min-size (pool-config pool))))
    (dotimes (i min-size)
      (when-let ((resource (safely-create-resource pool)))
        (sb-thread:with-mutex ((pool-lock pool))
          (push (make-pooled-resource 
                 :resource resource
                 :created-at (get-universal-time)
                 :last-used-at (get-universal-time))
                (pool-available pool)))))))

;;;; Pool Statistics and Introspection

(defun pool-size (pool)
  "Get number of available resources in pool"
  (sb-thread:with-mutex ((pool-lock pool))
    (length (pool-available pool))))

(defun pool-capacity (pool)
  "Get maximum pool capacity"
  (pool-config-max-size (pool-config pool)))

(defun total-pool-size (pool)
  "Get total number of resources (available + in-use)"
  (+ (length (pool-available pool))
     (hash-table-count (pool-in-use pool))))

;;;; Helper Functions

(defun safely-create-resource (pool)
  "Safely create a new resource, handling errors"
  (handler-case
      (let ((resource (funcall (pool-config-factory (pool-config pool)))))
        (incf (pool-stats-created (pool-stats pool)))
        resource)
    (error (e)
      (warn "Failed to create pool resource: ~A" e)
      nil)))

(defun safely-destroy-resource (pool resource)
  "Safely destroy a resource, handling errors"
  (when-let ((destroyer (pool-config-destroyer (pool-config pool))))
    (handler-case
        (progn
          (funcall destroyer resource)
          (incf (pool-stats-destroyed (pool-stats pool))))
      (error (e)
        (warn "Failed to destroy pool resource: ~A" e)))))

(defun validate-resource (pool wrapped-resource)
  "Validate a wrapped resource"
  (and (pooled-resource-valid-p wrapped-resource)
       (validate-resource-direct pool (pooled-resource-resource wrapped-resource))))

(defun validate-resource-direct (pool resource)
  "Validate a resource directly"
  (if-let ((validator (pool-config-validator (pool-config pool))))
    (handler-case
        (funcall validator resource)
      (error (e)
        (incf (pool-stats-validation-failures (pool-stats pool)))
        (warn "Resource validation failed: ~A" e)
        nil))
    t)) ; No validator means always valid