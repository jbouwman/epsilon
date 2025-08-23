;;;; HTTP Error Conditions
;;;;
;;;;  error handling hierarchy for HTTP operations

(defpackage :epsilon.http.errors
  (:use :cl)
  (:export
   ;; Base conditions
   #:http-error
   #:http-client-error
   #:http-server-error
   #:http-network-error
   
   ;; Client errors (4xx)
   #:http-bad-request-error          ; 400
   #:http-unauthorized-error         ; 401
   #:http-forbidden-error            ; 403
   #:http-not-found-error           ; 404
   #:http-method-not-allowed-error  ; 405
   #:http-timeout-error             ; 408
   #:http-conflict-error            ; 409
   #:http-gone-error                ; 410
   #:http-unprocessable-entity-error ; 422
   #:http-too-many-requests-error   ; 429
   
   ;; Server errors (5xx)
   #:http-internal-server-error     ; 500
   #:http-not-implemented-error     ; 501
   #:http-bad-gateway-error         ; 502
   #:http-service-unavailable-error ; 503
   #:http-gateway-timeout-error     ; 504
   
   ;; Network errors
   #:connection-refused-error
   #:connection-reset-error
   #:connection-timeout-error
   #:dns-resolution-error
   #:ssl-error
   
   ;; Request/Response errors
   #:invalid-url-error
   #:invalid-header-error
   #:body-encoding-error
   #:response-parse-error
   
   ;; Accessors
   #:http-error-status-code
   #:http-error-message
   #:http-error-url
   #:http-error-method
   #:http-error-headers
   #:http-error-body
   #:http-error-cause
   
   ;; Utilities
   #:status-code-to-condition
   #:signal-http-error
   #:with-http-error-handling))

(in-package :epsilon.http.errors)

;;;; Base Condition Types

(define-condition http-error (error)
  ((status-code :initarg :status-code
                :initform nil
                :reader http-error-status-code
                :documentation "HTTP status code associated with the error")
   (message :initarg :message
            :initform ""
            :reader http-error-message
            :documentation "Human-readable error message")
   (url :initarg :url
        :initform nil
        :reader http-error-url
        :documentation "URL that caused the error")
   (method :initarg :method
           :initform nil
           :reader http-error-method
           :documentation "HTTP method used")
   (headers :initarg :headers
            :initform nil
            :reader http-error-headers
            :documentation "Request/response headers")
   (body :initarg :body
         :initform nil
         :reader http-error-body
         :documentation "Request/response body")
   (cause :initarg :cause
          :initform nil
          :reader http-error-cause
          :documentation "Underlying cause of the error"))
  (:documentation "Base condition for all HTTP errors")
  (:report (lambda (condition stream)
             (format stream "HTTP Error~@[ (~A)~]: ~A~@[ for ~A ~A~]"
                     (http-error-status-code condition)
                     (http-error-message condition)
                     (http-error-method condition)
                     (http-error-url condition)))))

(define-condition http-client-error (http-error)
  ()
  (:documentation "Base condition for HTTP client errors (4xx)"))

(define-condition http-server-error (http-error)
  ()
  (:documentation "Base condition for HTTP server errors (5xx)"))

(define-condition http-network-error (http-error)
  ()
  (:documentation "Base condition for network-level errors"))

;;;; Specific Client Errors (4xx)

(define-condition http-bad-request-error (http-client-error)
  ()
  (:documentation "400 Bad Request")
  (:default-initargs :status-code 400 :message "Bad Request"))

(define-condition http-unauthorized-error (http-client-error)
  ()
  (:documentation "401 Unauthorized")
  (:default-initargs :status-code 401 :message "Unauthorized"))

(define-condition http-forbidden-error (http-client-error)
  ()
  (:documentation "403 Forbidden")
  (:default-initargs :status-code 403 :message "Forbidden"))

(define-condition http-not-found-error (http-client-error)
  ()
  (:documentation "404 Not Found")
  (:default-initargs :status-code 404 :message "Not Found"))

(define-condition http-method-not-allowed-error (http-client-error)
  ()
  (:documentation "405 Method Not Allowed")
  (:default-initargs :status-code 405 :message "Method Not Allowed"))

(define-condition http-timeout-error (http-client-error)
  ()
  (:documentation "408 Request Timeout")
  (:default-initargs :status-code 408 :message "Request Timeout"))

(define-condition http-conflict-error (http-client-error)
  ()
  (:documentation "409 Conflict")
  (:default-initargs :status-code 409 :message "Conflict"))

(define-condition http-gone-error (http-client-error)
  ()
  (:documentation "410 Gone")
  (:default-initargs :status-code 410 :message "Gone"))

(define-condition http-unprocessable-entity-error (http-client-error)
  ()
  (:documentation "422 Unprocessable Entity")
  (:default-initargs :status-code 422 :message "Unprocessable Entity"))

(define-condition http-too-many-requests-error (http-client-error)
  ((retry-after :initarg :retry-after
                :initform nil
                :reader retry-after
                :documentation "Seconds to wait before retrying"))
  (:documentation "429 Too Many Requests")
  (:default-initargs :status-code 429 :message "Too Many Requests"))

;;;; Specific Server Errors (5xx)

(define-condition http-internal-server-error (http-server-error)
  ()
  (:documentation "500 Internal Server Error")
  (:default-initargs :status-code 500 :message "Internal Server Error"))

(define-condition http-not-implemented-error (http-server-error)
  ()
  (:documentation "501 Not Implemented")
  (:default-initargs :status-code 501 :message "Not Implemented"))

(define-condition http-bad-gateway-error (http-server-error)
  ()
  (:documentation "502 Bad Gateway")
  (:default-initargs :status-code 502 :message "Bad Gateway"))

(define-condition http-service-unavailable-error (http-server-error)
  ((retry-after :initarg :retry-after
                :initform nil
                :reader retry-after
                :documentation "Seconds to wait before retrying"))
  (:documentation "503 Service Unavailable")
  (:default-initargs :status-code 503 :message "Service Unavailable"))

(define-condition http-gateway-timeout-error (http-server-error)
  ()
  (:documentation "504 Gateway Timeout")
  (:default-initargs :status-code 504 :message "Gateway Timeout"))

;;;; Network Errors

(define-condition connection-refused-error (http-network-error)
  ()
  (:documentation "Connection refused by server")
  (:default-initargs :message "Connection refused"))

(define-condition connection-reset-error (http-network-error)
  ()
  (:documentation "Connection reset by peer")
  (:default-initargs :message "Connection reset"))

(define-condition connection-timeout-error (http-network-error)
  ()
  (:documentation "Connection timed out")
  (:default-initargs :message "Connection timeout"))

(define-condition dns-resolution-error (http-network-error)
  ((hostname :initarg :hostname
             :reader hostname
             :documentation "Hostname that couldn't be resolved"))
  (:documentation "DNS resolution failed")
  (:default-initargs :message "DNS resolution failed"))

(define-condition ssl-error (http-network-error)
  ((certificate-error :initarg :certificate-error
                      :initform nil
                      :reader certificate-error
                      :documentation "Certificate validation error"))
  (:documentation "SSL/TLS error")
  (:default-initargs :message "SSL/TLS error"))

;;;; Request/Response Errors

(define-condition invalid-url-error (http-error)
  ()
  (:documentation "Invalid URL format")
  (:default-initargs :message "Invalid URL"))

(define-condition invalid-header-error (http-error)
  ((header-name :initarg :header-name
                :reader header-name
                :documentation "Name of the invalid header")
   (header-value :initarg :header-value
                 :reader header-value
                 :documentation "Invalid header value"))
  (:documentation "Invalid HTTP header")
  (:default-initargs :message "Invalid header"))

(define-condition body-encoding-error (http-error)
  ((encoding :initarg :encoding
             :reader encoding
             :documentation "Encoding that failed"))
  (:documentation "Body encoding/decoding error")
  (:default-initargs :message "Body encoding error"))

(define-condition response-parse-error (http-error)
  ((response-text :initarg :response-text
                  :reader response-text
                  :documentation "Raw response that couldn't be parsed"))
  (:documentation "Failed to parse HTTP response")
  (:default-initargs :message "Response parse error"))

;;;; Utility Functions

(defun status-code-to-condition (status-code)
  "Return the appropriate condition class for an HTTP status code"
  (case status-code
    (400 'http-bad-request-error)
    (401 'http-unauthorized-error)
    (403 'http-forbidden-error)
    (404 'http-not-found-error)
    (405 'http-method-not-allowed-error)
    (408 'http-timeout-error)
    (409 'http-conflict-error)
    (410 'http-gone-error)
    (422 'http-unprocessable-entity-error)
    (429 'http-too-many-requests-error)
    (500 'http-internal-server-error)
    (501 'http-not-implemented-error)
    (502 'http-bad-gateway-error)
    (503 'http-service-unavailable-error)
    (504 'http-gateway-timeout-error)
    (otherwise
     (cond
       ((< status-code 400) nil)
       ((< status-code 500) 'http-client-error)
       ((< status-code 600) 'http-server-error)
       (t 'http-error)))))

(defun signal-http-error (status-code &key message url method headers body cause)
  "Signal an appropriate HTTP error based on status code"
  (let ((condition-type (or (status-code-to-condition status-code)
                            'http-error)))
    (error condition-type
           :status-code status-code
           :message (or message (format nil "HTTP ~A" status-code))
           :url url
           :method method
           :headers headers
           :body body
           :cause cause)))

(defmacro with-http-error-handling ((&key (on-client-error :error)
                                          (on-server-error :retry)
                                          (on-network-error :retry)
                                          (max-retries 3))
                                    &body body)
  "Execute body with HTTP error handling
   
   Options for error handling:
   - :error - Re-signal the error
   - :retry - Retry the operation
   - :ignore - Ignore and return nil
   - function - Call function with the condition"
  (let ((retries (gensym "RETRIES"))
        (result (gensym "RESULT"))
        (condition-var (gensym "CONDITION")))
    `(let ((,retries 0))
       (loop
         (handler-case
             (return (progn ,@body))
           (http-client-error (,condition-var)
             ,(case on-client-error
                (:error `(error ,condition-var))
                (:retry `(if (< ,retries ,max-retries)
                            (progn (incf ,retries)
                                   (sleep (expt 2 ,retries)))
                            (error ,condition-var)))
                (:ignore nil)
                (otherwise `(funcall ,on-client-error ,condition-var))))
           (http-server-error (,condition-var)
             ,(case on-server-error
                (:error `(error ,condition-var))
                (:retry `(if (< ,retries ,max-retries)
                            (progn (incf ,retries)
                                   (sleep (expt 2 ,retries)))
                            (error ,condition-var)))
                (:ignore nil)
                (otherwise `(funcall ,on-server-error ,condition-var))))
           (http-network-error (,condition-var)
             ,(case on-network-error
                (:error `(error ,condition-var))
                (:retry `(if (< ,retries ,max-retries)
                            (progn (incf ,retries)
                                   (sleep (expt 2 ,retries)))
                            (error ,condition-var)))
                (:ignore nil)
                (otherwise `(funcall ,on-network-error ,condition-var)))))))))