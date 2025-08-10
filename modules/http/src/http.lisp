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
   (#:pool #:epsilon.http.connection-pool))
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
   #:pool-stats))

(in-package #:epsilon.http)

;;; Client Functions
(defun request (url &key method headers body)
  "Make HTTP request"
  (client:request url :method (or method "GET") :headers headers :body body))

(defun http-get (url &key headers)
  "Make GET request"
  (client:http-get url :headers headers))

(defun http-post (url &key headers body)
  "Make POST request"
  (client:http-post url :headers headers :body body))

(defun http-put (url &key headers body)
  "Make PUT request"
  (client:http-put url :headers headers :body body))

(defun http-delete (url &key headers)
  "Make DELETE request"
  (client:http-delete url :headers headers))

(defun http-head (url &key headers)
  "Make HEAD request"
  (client:http-head url :headers headers))

(defun http-options (url &key headers)  
  "Make OPTIONS request"
  (client:http-options url :headers headers))


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

;; Response accessors
(defun response-status (resp)
  "Get response status"
  (response:response-status resp))

(defun response-headers (resp)
  "Get response headers"
  (response:response-headers resp))

(defun response-body (resp)
  "Get response body"
  (response:response-body resp))

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