;;;; epsilon.http.middleware - HTTP Middleware System
;;;;
;;;; Composable middleware for HTTP request/response processing.
;;;; Middleware wraps requests and can modify them, handle responses,
;;;; or short-circuit the chain.

(defpackage :epsilon.http.middleware
  (:use :cl)
  (:import
   (epsilon.map map)
   (epsilon.http.retry retry)
   (epsilon.http.errors errors))
  (:export
   ;; Middleware composition
   #:compose-middleware
   #:apply-middleware
   #:wrap-handler

   ;; Built-in middleware
   #:logging-middleware
   #:retry-middleware
   #:timeout-middleware
   #:default-headers-middleware
   #:user-agent-middleware

   ;; Logging configuration
   #:*log-requests*
   #:*log-responses*
   #:*log-timing*

   ;; Request/response context
   #:request-context
   #:make-request-context
   #:context-request
   #:context-response
   #:context-start-time
   #:context-metadata))

;;;; Logging Configuration

(defvar *log-requests* t
  "Whether to log outgoing requests")

(defvar *log-responses* t
  "Whether to log incoming responses")

(defvar *log-timing* t
  "Whether to log request timing")

;;;; Request Context

(defstruct request-context
  "Context passed through middleware chain"
  (request nil)                          ; The HTTP request
  (response nil)                         ; The HTTP response (after execution)
  (start-time 0 :type integer)          ; Request start time (internal-time)
  (metadata (make-hash-table :test 'equal) :type hash-table)) ; Arbitrary metadata

(defun context-get (context key &optional default)
  "Get metadata from context"
  (gethash key (request-context-metadata context) default))

(defun context-set (context key value)
  "Set metadata in context"
  (setf (gethash key (request-context-metadata context)) value))

;;;; Middleware Composition

(defun compose-middleware (middleware-list)
  "Compose a list of middleware into a single function.
   Each middleware should be a function: (handler) -> wrapped-handler
   where handler is a function: (request) -> response"
  (lambda (handler)
    (reduce (lambda (h mw) (funcall mw h))
            middleware-list
            :initial-value handler
            :from-end t)))

(defun apply-middleware (middleware-list handler request)
  "Apply middleware chain to handler and execute with request"
  (let ((composed-handler (funcall (compose-middleware middleware-list) handler)))
    (funcall composed-handler request)))

(defun wrap-handler (handler &rest middleware)
  "Wrap a handler with middleware.
   Returns a new handler function."
  (funcall (compose-middleware middleware) handler))

;;;; Logging Middleware

(defun logging-middleware (&key (level :info) (log-body nil))
  "Create logging middleware that logs requests and responses.

   Arguments:
     LEVEL - Log level (:debug, :info, :warn). Controls verbosity --
             :debug logs everything, :info skips debug details,
             :warn only logs on slow responses (>1s).
     LOG-BODY - Whether to log request/response bodies

   Returns: Middleware function"
  (lambda (handler)
    (lambda (request)
      (let ((start-time (get-internal-real-time)))
        (when (and *log-requests* (member level '(:debug :info)))
          (log-request request)
          (when (and log-body (listp request))
            (let ((body (getf request :body)))
              (when body
                (format *trace-output* "~&[HTTP] Body: ~A~%"
                        (if (> (length body) 200)
                            (concatenate 'string (subseq body 0 200) "...")
                            body))))))
        (let ((response (funcall handler request)))
          (let* ((end-time (get-internal-real-time))
                 (elapsed-ms (/ (* 1000.0 (- end-time start-time))
                                internal-time-units-per-second)))
            (when (and *log-responses*
                       (or (member level '(:debug :info))
                           (and (eq level :warn) (> elapsed-ms 1000))))
              (log-response response)
              (when (and log-body (listp response))
                (let ((body (getf response :body)))
                  (when (and body (stringp body))
                    (format *trace-output* "~&[HTTP] Body: ~A~%"
                            (if (> (length body) 200)
                                (concatenate 'string (subseq body 0 200) "...")
                                body))))))
            (when *log-timing*
              (log-timing start-time)))
          response)))))

(defun log-request (request)
  "Log outgoing request"
  (format *trace-output* "~&[HTTP] --> ~A~%" request))

(defun log-response (response)
  "Log incoming response"
  (format *trace-output* "~&[HTTP] <-- ~A~%" response))

(defun log-timing (start-time)
  "Log request timing"
  (let* ((end-time (get-internal-real-time))
         (elapsed-ms (/ (* 1000.0 (- end-time start-time))
                       internal-time-units-per-second)))
    (format *trace-output* "~&[HTTP] Completed in ~,2Fms~%" elapsed-ms)))

;;;; Retry Middleware

(defun retry-middleware (&key (max-attempts 3)
                              (initial-delay 1.0)
                              (max-delay 60.0)
                              (multiplier 2.0)
                              (retryable-statuses '(408 429 500 502 503 504)))
  "Create retry middleware with exponential backoff.

   Arguments:
     MAX-ATTEMPTS - Maximum number of attempts (default 3)
     INITIAL-DELAY - Initial delay in seconds (default 1.0)
     MAX-DELAY - Maximum delay in seconds (default 60.0)
     MULTIPLIER - Backoff multiplier (default 2.0)
     RETRYABLE-STATUSES - HTTP status codes to retry

   Returns: Middleware function"
  (let ((policy (retry:make-retry-policy
                 :max-attempts max-attempts
                 :initial-delay initial-delay
                 :max-delay max-delay
                 :multiplier multiplier
                 :retryable-statuses retryable-statuses)))
    (lambda (handler)
      (lambda (request)
        (retry-with-policy handler request policy)))))

(defun retry-with-policy (handler request policy)
  "Execute handler with retry policy"
  (let ((attempts 0)
        (max-attempts (retry:retry-policy-max-attempts policy)))
    (loop
      (incf attempts)
      (handler-case
          (let ((response (funcall handler request)))
            ;; Check if we should retry based on status
            (if (and (< attempts max-attempts)
                     (should-retry-status-p response policy))
                (progn
                  (sleep-with-backoff attempts policy)
                  ;; Continue loop
                  )
                (return response)))
        (error (e)
          (if (< attempts max-attempts)
              (progn
                (sleep-with-backoff attempts policy)
                ;; Continue loop
                )
              (error e)))))))

(defun should-retry-status-p (response policy)
  "Check if response status should trigger retry"
  (when response
    (let ((status (response-status response)))
      (member status (retry:retry-policy-retryable-statuses policy)))))

(defun response-status (response)
  "Extract status code from response"
  ;; Handle different response types
  (cond
    ((null response) nil)
    ((listp response) (getf response :status))
    ((hash-table-p response) (gethash :status response))
    (t nil)))

(defun sleep-with-backoff (attempt policy)
  "Sleep for backoff duration"
  (let ((delay (calculate-backoff attempt policy)))
    (sleep delay)))

(defun calculate-backoff (attempt policy)
  "Calculate backoff delay with jitter"
  (let* ((base-delay (* (retry:retry-policy-initial-delay policy)
                       (expt (retry:retry-policy-multiplier policy) (1- attempt))))
         (max-delay (retry:retry-policy-max-delay policy))
         (capped-delay (min base-delay max-delay))
         (jitter-range (* capped-delay 0.1))
         (jitter (- (random (* 2.0 jitter-range)) jitter-range)))
    (max 0.0 (+ capped-delay jitter))))

;;;; Timeout Middleware

(defmacro with-timeout ((seconds) &body body)
  "Execute body with timeout. Signals TIMEOUT-ERROR if timeout exceeded."
  `(sb-ext:with-timeout ,seconds
     ,@body))

(defun timeout-middleware (seconds)
  "Create timeout middleware.

   Arguments:
     SECONDS - Timeout in seconds

   Returns: Middleware function"
  (lambda (handler)
    (lambda (request)
      (with-timeout (seconds)
        (funcall handler request)))))

;;;; Default Headers Middleware

(defun default-headers-middleware (headers)
  "Create middleware that adds default headers to all requests.

   Arguments:
     HEADERS - Alist of (name . value) pairs

   Returns: Middleware function"
  (lambda (handler)
    (lambda (request)
      (let ((merged-request (merge-headers request headers)))
        (funcall handler merged-request)))))

(defun merge-headers (request new-headers)
  "Merge headers into request, not overwriting existing ones.
   REQUEST is a plist with :headers (a map). NEW-HEADERS is an alist
   of (name . value) pairs. Existing headers take precedence."
  (when (and request new-headers)
    (let ((existing-headers (getf request :headers)))
      (dolist (pair new-headers)
        (let ((name (car pair))
              (value (cdr pair)))
          (unless (and existing-headers (map:get existing-headers name))
            (setf existing-headers (map:assoc (or existing-headers (map:make-map))
                                              name value)))))
      (setf (getf request :headers) existing-headers)))
  request)

;;;; User-Agent Middleware

(defun user-agent-middleware (user-agent)
  "Create middleware that sets User-Agent header.

   Arguments:
     USER-AGENT - User-Agent string

   Returns: Middleware function"
  (default-headers-middleware
   (list (cons "User-Agent" user-agent))))

;;;; Predefined Middleware Chains

(defun standard-middleware ()
  "Return list of standard middleware in recommended order"
  (list
   (logging-middleware)
   (retry-middleware)))
