;;;; Main HTTP Package
;;;; 
;;;; Unified interface to HTTP client and server functionality

(defpackage #:epsilon.http
  (:use #:cl)
  (:local-nicknames
   (#:client #:epsilon.http.client)
   (#:server #:epsilon.http.server)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:security #:epsilon.http.security)
   (#:validation #:epsilon.http.validation)
   (#:pool #:epsilon.http.connection-pool)
   (#:simple #:epsilon.http.simple))
  (:export
   ;; Client functions
   #:request
   #:http-get
   #:http-post
   #:http-put
   #:http-delete
   #:http-head
   #:http-options
   
   ;; Server functions
   #:start-server
   #:stop-server
   #:with-server
   
   ;; Request/Response objects
   #:make-request
   #:make-response
   #:request-method
   #:request-path
   #:request-headers
   #:request-body
   #:response-status
   #:response-headers
   #:response-body
   
   ;; Security middleware
   #:cors-middleware
   #:csrf-middleware
   #:rate-limit-middleware
   #:create-rate-limiter
   
   
   ;; Connection pooling
   #:with-pooled-connection
   #:pool-stats
   
   ;; Simple API (re-export from simple package)
   #:http-get
   #:http-post
   #:http-put
   #:http-patch
   #:http-delete
   #:http-head
   #:http-options
   #:response-ok-p
   #:response-json
   #:response-text
   #:response-headers
   #:response-status
   #:response-header
   #:download-file
   #:upload-file
   #:url-encode
   #:url-decode
   #:add-header
   #:json-response
   #:html-response
   #:text-response))

(in-package #:epsilon.http)

;;; Simple API Functions (delegating to simple package)
(defun http-get (url &rest options)
  "Simple GET request"
  (apply #'simple:http-get url options))

(defun http-post (url &rest options)
  "Simple POST request"
  (apply #'simple:http-post url options))

(defun http-put (url &rest options)
  "Simple PUT request"
  (apply #'simple:http-put url options))

(defun http-patch (url &rest options)
  "Simple PATCH request"
  (apply #'simple:http-patch url options))

(defun http-delete (url &rest options)
  "Simple DELETE request"
  (apply #'simple:http-delete url options))

(defun http-head (url &rest options)
  "Simple HEAD request"
  (apply #'simple:http-head url options))

(defun http-options (url &rest options)
  "Simple OPTIONS request"
  (apply #'simple:http-options url options))

(defun response-ok-p (response)
  "Check if response is successful"
  (simple:response-ok-p response))

(defun response-json (response)
  "Parse response as JSON"
  (simple:response-json response))

(defun response-text (response)
  "Get response text"
  (simple:response-text response))

(defun response-headers (response)
  "Get response headers"
  (simple:response-headers response))

(defun response-status (response)
  "Get response status code"
  (simple:response-status response))

(defun response-header (response header)
  "Get specific response header"
  (simple:response-header response header))

(defun response-body (response)
  "Get response body"
  (simple:response-text response))

(defun download-file (url filepath &rest options)
  "Download file from URL"
  (apply #'simple:download-file url filepath options))

(defun upload-file (url filepath &rest options)
  "Upload file to URL"
  (apply #'simple:upload-file url filepath options))

;;; Server Functions
(defun start-server (handler &key (port 8080) (address "127.0.0.1"))
  "Start HTTP server"
  (server:start-server handler :port port :address address))

(defun stop-server (server)
  "Stop HTTP server"
  (server:stop-server server))

(defmacro with-server ((server-var handler &key (port 8080) (address "127.0.0.1")) &body body)
  "Run server with automatic cleanup"
  `(let ((,server-var (start-server ,handler :port ,port :address ,address)))
     (unwind-protect
          (progn ,@body)
       (when ,server-var
         (stop-server ,server-var)))))

;;; Request/Response Functions  
(defun make-request (&rest args)
  "Create HTTP request object"
  (apply #'request:make-request args))

(defun make-response (&rest args)
  "Create HTTP response object"
  (apply #'response:make-response args))

;; Request accessors
(defun request-method (req)
  "Get request method"
  (request:request-method req))

(defun request-path (req)
  "Get request path"
  (request:request-path req))

(defun request-headers (req)
  "Get request headers"
  (request:request-headers req))

(defun request-body (req)
  "Get request body"
  (request:request-body req))


;;; Security Functions
(defun cors-middleware (&rest args)
  "Create CORS middleware"
  (apply #'security:cors-middleware args))

(defun csrf-middleware (&rest args)
  "Create CSRF middleware"
  (apply #'security:csrf-middleware args))

(defun rate-limit-middleware (&rest args)
  "Create rate limiting middleware"
  (apply #'security:rate-limit-middleware args))

(defun create-rate-limiter (&rest args)
  "Create rate limiter"
  (apply #'security:create-rate-limiter args))


;;; Connection Pool Functions
(defmacro with-pooled-connection ((&rest args) &body body)
  "Execute with pooled connection"
  `(pool:with-pooled-connection ,args ,@body))

(defun pool-stats (&rest args)
  "Get connection pool statistics"
  (apply #'pool:pool-stats args))

;;; URL Encoding Functions
(defun url-encode (string)
  "URL encode a string"
  (request:url-encode string))

(defun url-decode (string)
  "URL decode a string"
  (request:url-decode string))

(defun add-header (request header-name header-value)
  "Add or update a header in a request object"
  (request:add-header request header-name header-value))

;;; Response Builder Functions
(defun json-response (data &key (status 200))
  "Create a JSON response"
  (response:json-response data :status status))

(defun html-response (html &key (status 200))
  "Create an HTML response"
  (response:html-response html :status status))

(defun text-response (text &key (status 200))
  "Create a plain text response"
  (response:text-response text :status status))