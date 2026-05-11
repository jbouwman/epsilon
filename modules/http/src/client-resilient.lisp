;;;; epsilon.http.client.resilient - Resilient HTTP Client
;;;;
;;;; Composes the connection pool, retry policy, and circuit breaker into
;;;; a single high-level entry point. The plain `client:request` is fast
;;;; and does the right thing for most requests, but production callers
;;;; (the Loki exporter that prompted IMPL-322 and similar long-running
;;;; collectors) want:
;;;;
;;;;   * connection reuse via a pool,
;;;;   * automatic retry with exponential backoff for transient failures,
;;;;   * a circuit breaker that stops hammering a host that is clearly down.
;;;;
;;;; This module wires those three pieces together so callers don't have to.
;;;; The retry policy decides whether an error is transient (network errors,
;;;; 5xx, 408, 429); the circuit breaker tracks the failure rate and short-
;;;; circuits subsequent requests once a host has crossed the failure
;;;; threshold; the pool reuses sockets across requests.

(defpackage :epsilon.http.client.resilient
  (:use :cl)
  (:import (epsilon.http.client client)
            (epsilon.http.connection-pool pool)
            (epsilon.http.headers headers)
            (epsilon.http.response response)
            (epsilon.http.retry retry)
            (epsilon.http.errors errors))
  (:export
   ;; Top-level resilient request entry point
   #:resilient-request
   #:resilient-get
   #:resilient-post
   ;; Configuration
   #:make-resilient-config
   #:resilient-config
   #:resilient-config-pool
   #:resilient-config-retry-policy
   #:resilient-config-circuit-breaker
   ;; Status check
   #:status-code-error-p))

(in-package :epsilon.http.client.resilient)

;;;; Configuration

(defstruct resilient-config
  "Configuration for a resilient HTTP client. Bundles a connection pool,
   a retry policy, and an optional circuit breaker."
  (pool nil :type (or null pool:connection-pool))
  (retry-policy nil)
  (circuit-breaker nil))

(defun make-default-config ()
  "Build a resilient config with sensible defaults: a fresh pool, the
   default retry policy, and a default circuit breaker."
  (make-resilient-config
   :pool (pool:create-connection-pool)
   :retry-policy (retry:default-retry-policy)
   :circuit-breaker (retry:make-default-circuit-breaker)))

;;;; Status code classification

(defun status-code-error-p (status)
  "Return T if STATUS represents an error response that should propagate
   as a condition (so retry/circuit-breaker can react to it). 4xx is
   client error, 5xx is server error. We treat 5xx and 408/429 as
   retryable; the retry policy makes the actual decision."
  (and (integerp status) (>= status 400)))

(defun signal-status-error (status response url method)
  "Translate an HTTP status code into the appropriate error condition so
   the retry policy and circuit breaker can react. We only signal for 5xx
   and a few transient 4xx codes; non-retryable errors propagate as a
   plain http-error."
  (let ((args (list :status-code status
                    :url url
                    :method method
                    :headers (response:response-headers response)
                    :body (response:response-body response))))
    (cond
      ((= status 408)
       (apply #'error 'errors:http-timeout-error args))
      ((= status 429)
       (apply #'error 'errors:http-too-many-requests-error args))
      ((= status 502)
       (apply #'error 'errors:http-bad-gateway-error args))
      ((= status 503)
       (apply #'error 'errors:http-service-unavailable-error args))
      ((= status 504)
       (apply #'error 'errors:http-gateway-timeout-error args))
      ((>= status 500)
       (apply #'error 'errors:http-server-error
              :message (format nil "HTTP ~D" status) args))
      (t
       ;; 4xx other than the transient ones above: not retryable
       (apply #'error 'errors:http-client-error
              :message (format nil "HTTP ~D" status) args)))))

;;;; Top-level resilient request

(defun resilient-request (url &key
                              (method "GET")
                              headers
                              body
                              timeout
                              (config (make-default-config)))
  "Issue an HTTP request with pooling, retry, and circuit-breaker
   protection.

   Behavior:
   - Pool: connections are obtained from CONFIG's pool and returned after
     each attempt; on transient failure the pool entry is force-closed so
     a fresh connection is used on the next attempt.
   - Retry: transient failures (network errors, 5xx, 408, 429) are
     retried per the policy in CONFIG. Non-transient errors (most 4xx)
     are propagated immediately.
   - Circuit breaker: if too many failures occur in a row, the breaker
     trips and subsequent calls short-circuit with a service-unavailable
     error until the breaker times out and re-tries.

   Returns the http-response on success."
  (let ((breaker (resilient-config-circuit-breaker config))
        (policy  (resilient-config-retry-policy config))
        (the-pool (resilient-config-pool config)))
    (flet ((attempt ()
             (let ((resp (client:request url
                                         :method method
                                         :headers headers
                                         :body body
                                         :pool the-pool
                                         :timeout timeout)))
               (cond
                 ((null resp)
                  (error 'errors:http-network-error
                         :message "No response received"
                         :url url :method method))
                 ((status-code-error-p (response:response-status resp))
                  (signal-status-error (response:response-status resp)
                                       resp url method))
                 (t resp)))))
      (cond
        ;; Both retry and circuit breaker
        ((and policy breaker)
         (retry:call-with-retry
          (lambda ()
            (retry:with-circuit-breaker (breaker)
              (attempt)))
          :policy policy))
        ;; Retry only
        (policy
         (retry:call-with-retry #'attempt :policy policy))
        ;; Circuit breaker only
        (breaker
         (retry:with-circuit-breaker (breaker) (attempt)))
        ;; Neither: just call the pooled request
        (t (attempt))))))

(defun resilient-get (url &rest options &key &allow-other-keys)
  "Resilient GET request. See RESILIENT-REQUEST."
  (apply #'resilient-request url :method "GET" options))

(defun resilient-post (url &rest options &key body &allow-other-keys)
  "Resilient POST request. See RESILIENT-REQUEST."
  (declare (ignorable body))
  (apply #'resilient-request url :method "POST" options))
