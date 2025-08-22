;;;; HTTP Retry Logic
;;;;
;;;; Automatic retry with exponential backoff and circuit breaker

(defpackage :epsilon.http.retry
  (:use :cl)
  (:local-nicknames
   (#:http #:epsilon.http)
   (#:errors #:epsilon.http.errors)
   (#:thread #:epsilon.sys.thread)
   (#:time #:epsilon.time))
  (:export
   ;; Retry policies
   #:retry-policy
   #:make-retry-policy
   #:default-retry-policy
   #:no-retry-policy
   #:aggressive-retry-policy
   
   ;; Retry strategies
   #:exponential-backoff
   #:linear-backoff
   #:fixed-delay
   #:fibonacci-backoff
   
   ;; Conditions to retry
   #:should-retry-p
   #:retryable-error-p
   #:retryable-status-p
   
   ;; Execution
   #:with-retry
   #:call-with-retry
   
   ;; Circuit breaker
   #:circuit-breaker
   #:make-circuit-breaker
   #:circuit-state
   #:circuit-open-p
   #:circuit-closed-p
   #:circuit-half-open-p
   #:record-success
   #:record-failure
   #:with-circuit-breaker))

(in-package :epsilon.http.retry)

;;;; Retry Policy

(defstruct retry-policy
  "Configuration for retry behavior"
  (max-attempts 3 :type integer)
  (initial-delay 1.0 :type number)
  (max-delay 60.0 :type number)
  (multiplier 2.0 :type number)
  (jitter 0.1 :type number)
  (retryable-conditions '(errors:http-server-error
                         errors:http-network-error
                         errors:http-timeout-error
                         errors:http-service-unavailable-error
                         errors:http-bad-gateway-error
                         errors:http-gateway-timeout-error))
  (retryable-statuses '(408 429 500 502 503 504))
  (backoff-strategy nil))

(defparameter *default-retry-policy* nil
  "Default retry policy")

(defun default-retry-policy ()
  "Get default retry policy"
  *default-retry-policy*)

(defun no-retry-policy ()
  "Policy that never retries"
  (make-retry-policy :max-attempts 1))

(defun aggressive-retry-policy ()
  "Aggressive retry policy with more attempts"
  (make-retry-policy 
   :max-attempts 5
   :initial-delay 0.5
   :max-delay 120.0
   :multiplier 1.5))

;;;; Backoff Strategies

(defun exponential-backoff (attempt policy)
  "Calculate exponential backoff delay"
  (let* ((base-delay (* (retry-policy-initial-delay policy)
                       (expt (retry-policy-multiplier policy) (1- attempt))))
         (jittered-delay (add-jitter base-delay (retry-policy-jitter policy))))
    (min jittered-delay (retry-policy-max-delay policy))))

(defun linear-backoff (attempt policy)
  "Calculate linear backoff delay"
  (let* ((base-delay (* (retry-policy-initial-delay policy) attempt))
         (jittered-delay (add-jitter base-delay (retry-policy-jitter policy))))
    (min jittered-delay (retry-policy-max-delay policy))))

(defun fixed-delay (attempt policy)
  "Fixed delay between retries"
  (declare (ignore attempt))
  (add-jitter (retry-policy-initial-delay policy) 
              (retry-policy-jitter policy)))

(defun fibonacci-backoff (attempt policy)
  "Calculate fibonacci backoff delay"
  (let* ((fib-value (fibonacci attempt))
         (base-delay (* (retry-policy-initial-delay policy) fib-value))
         (jittered-delay (add-jitter base-delay (retry-policy-jitter policy))))
    (min jittered-delay (retry-policy-max-delay policy))))

(defun fibonacci (n)
  "Calculate nth fibonacci number"
  (if (<= n 2)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(defun add-jitter (delay jitter-factor)
  "Add random jitter to delay"
  (let ((jitter-amount (* delay jitter-factor)))
    (+ delay (- (random (* 2 jitter-amount)) jitter-amount))))

;; Initialize default retry policy after functions are defined
(setf *default-retry-policy* 
      (make-retry-policy :backoff-strategy #'exponential-backoff))

;;;; Retry Conditions

(defun should-retry-p (condition policy)
  "Check if condition should trigger a retry"
  (or (retryable-error-p condition policy)
      (and (typep condition 'errors:http-error)
           (retryable-status-p (errors:http-error-status-code condition) policy))))

(defun retryable-error-p (condition policy)
  "Check if error type is retryable"
  (some (lambda (error-type)
          (typep condition error-type))
        (retry-policy-retryable-conditions policy)))

(defun retryable-status-p (status-code policy)
  "Check if status code is retryable"
  (member status-code (retry-policy-retryable-statuses policy)))

;;;; Retry Execution

(defmacro with-retry ((&key (policy '*default-retry-policy*)
                            (on-retry nil))
                     &body body)
  "Execute body with retry logic"
  `(call-with-retry (lambda () ,@body) 
                   :policy ,policy 
                   :on-retry ,on-retry))

(defun call-with-retry (function &key 
                                 (policy *default-retry-policy*)
                                 on-retry)
  "Call function with retry logic"
  (let ((attempt 0))
    (loop
      (incf attempt)
      (handler-case
          (return (funcall function))
        (error (e)
          (cond
            ;; Check if we should retry
            ((and (< attempt (retry-policy-max-attempts policy))
                  (should-retry-p e policy))
             ;; Calculate delay
             (let ((delay (funcall (retry-policy-backoff-strategy policy) 
                                  attempt policy)))
               ;; Call retry callback
               (when on-retry
                 (funcall on-retry attempt e delay))
               ;; Sleep before retry
               (sleep delay)))
            ;; No more retries
            (t
             (error e))))))))

;;;; Circuit Breaker

(defstruct circuit-breaker
  "Circuit breaker to prevent cascading failures"
  (failure-threshold 5 :type integer)
  (success-threshold 2 :type integer)
  (timeout 60 :type number)
  (half-open-requests 3 :type integer)
  (state :closed :type keyword)
  (failure-count 0 :type integer)
  (success-count 0 :type integer)
  (last-failure-time 0 :type integer)
  (half-open-attempts 0 :type integer)
  (lock (sb-thread:make-mutex) :type sb-thread:mutex))

(defun make-default-circuit-breaker ()
  "Create a default circuit breaker"
  (make-circuit-breaker))

(defun circuit-state (breaker)
  "Get current circuit state"
  (sb-thread:with-mutex ((circuit-breaker-lock breaker))
    (update-circuit-state breaker)
    (circuit-breaker-state breaker)))

(defun circuit-open-p (breaker)
  "Check if circuit is open"
  (eq (circuit-state breaker) :open))

(defun circuit-closed-p (breaker)
  "Check if circuit is closed"
  (eq (circuit-state breaker) :closed))

(defun circuit-half-open-p (breaker)
  "Check if circuit is half-open"
  (eq (circuit-state breaker) :half-open))

(defun update-circuit-state (breaker)
  "Update circuit state based on current conditions"
  (let ((current-time (get-universal-time)))
    (case (circuit-breaker-state breaker)
      (:open
       ;; Check if timeout has passed
       (when (> (- current-time (circuit-breaker-last-failure-time breaker))
                (circuit-breaker-timeout breaker))
         (setf (circuit-breaker-state breaker) :half-open
               (circuit-breaker-half-open-attempts breaker) 0)))
      (:half-open
       ;; Check if we've had enough successes
       (when (>= (circuit-breaker-success-count breaker)
                 (circuit-breaker-success-threshold breaker))
         (setf (circuit-breaker-state breaker) :closed
               (circuit-breaker-failure-count breaker) 0
               (circuit-breaker-success-count breaker) 0))))))

(defun record-success (breaker)
  "Record a successful request"
  (sb-thread:with-mutex ((circuit-breaker-lock breaker))
    (case (circuit-breaker-state breaker)
      (:half-open
       (incf (circuit-breaker-success-count breaker))
       (when (>= (circuit-breaker-success-count breaker)
                 (circuit-breaker-success-threshold breaker))
         (setf (circuit-breaker-state breaker) :closed
               (circuit-breaker-failure-count breaker) 0
               (circuit-breaker-success-count breaker) 0)))
      (:closed
       (setf (circuit-breaker-failure-count breaker) 0)))))

(defun record-failure (breaker)
  "Record a failed request"
  (sb-thread:with-mutex ((circuit-breaker-lock breaker))
    (let ((current-time (get-universal-time)))
      (incf (circuit-breaker-failure-count breaker))
      (setf (circuit-breaker-last-failure-time breaker) current-time)
      
      (case (circuit-breaker-state breaker)
        (:closed
         (when (>= (circuit-breaker-failure-count breaker)
                   (circuit-breaker-failure-threshold breaker))
           (setf (circuit-breaker-state breaker) :open)))
        (:half-open
         (setf (circuit-breaker-state breaker) :open
               (circuit-breaker-success-count breaker) 0))))))

(defmacro with-circuit-breaker ((breaker) &body body)
  "Execute body with circuit breaker protection"
  (let ((breaker-var (gensym "BREAKER"))
        (result-var (gensym "RESULT")))
    `(let ((,breaker-var ,breaker))
       ;; Check if circuit is open
       (when (circuit-open-p ,breaker-var)
         (error 'errors:http-service-unavailable-error
                :message "Circuit breaker is open"))
       
       ;; Try to execute
       (handler-case
           (let ((,result-var (progn ,@body)))
             (record-success ,breaker-var)
             ,result-var)
         (error (e)
           (record-failure ,breaker-var)
           (error e))))))

;;;; Integration Example

(defun make-resilient-request (url &key 
                                   (method "GET")
                                   headers
                                   body
                                   (retry-policy *default-retry-policy*)
                                   circuit-breaker)
  "Make an HTTP request with retry and circuit breaker"
  (let ((request-fn (lambda ()
                      (if circuit-breaker
                          (with-circuit-breaker (circuit-breaker)
                            (http:request url 
                                          :method method
                                          :headers headers
                                          :body body))
                          (http:request url 
                                        :method method
                                        :headers headers
                                        :body body)))))
    (with-retry (:policy retry-policy
                 :on-retry (lambda (attempt error delay)
                            (format *error-output* 
                                   "Retry ~A after ~A: ~A~%" 
                                   attempt delay error)))
      (funcall request-fn))))
