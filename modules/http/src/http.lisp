;;;; epsilon.http - Main HTTP Package
;;;;
;;;; Unified interface to HTTP client and server functionality

(defpackage epsilon.http
  (:use :cl)
  (:shadow #:get #:delete)
  (:import (epsilon.http.client client)
            (epsilon.http.server server)
            (epsilon.http.request request)
            (epsilon.http.response response)
            (epsilon.http.security security)
            (epsilon.http.validation validation)
            (epsilon.http.connection-pool pool)
            (epsilon.http.simple simple)))

;;; Simple API Functions (delegating to simple package)
(defun get (url &rest options)
  "Simple GET request"
  (apply #'simple:get url options))

(defun post (url &rest options)
  "Simple POST request"
  (apply #'simple:post url options))

(defun put (url &rest options)
  "Simple PUT request"
  (apply #'simple:put url options))

(defun patch (url &rest options)
  "Simple PATCH request"
  (apply #'simple:patch url options))

(defun delete (url &rest options)
  "Simple DELETE request"
  (apply #'simple:delete url options))

(defun head (url &rest options)
  "Simple HEAD request"
  (apply #'simple:head url options))

(defun options (url &rest options)
  "Simple OPTIONS request"
  (apply #'simple:options url options))

(defun request (url &rest options)
  "Make an HTTP request with full control over method, headers, and body"
  (apply #'simple:request url options))

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
  "Get response body (raw, may be string or bytes)"
  (response:response-body response))

(defun response-body-bytes (response)
  "Get response body as byte vector"
  (response:response-body-bytes response))

(defun response-body-string (response &key (encoding :utf-8))
  "Get response body as string, decoding from bytes if necessary"
  (response:response-body-string response :encoding encoding))

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

;;; Simple Router Support
;;; A router is a list of (method path handler) entries

(defun make-router ()
  "Create a new router (returns an empty list that can accumulate routes)"
  (list))

(defun route (router method path handler)
  "Register a route with the router.
   ROUTER: Router object (a list of routes)
   METHOD: HTTP method keyword (:get, :post, etc.)
   PATH: URL path pattern (supports trailing * for wildcards)
   HANDLER: Function to handle matching requests
   Returns the modified router."
  (nconc router (list (list method path handler)))
  router)

(defun router-handler (router)
  "Create a request handler function from a router.
   Returns a function suitable for use with start-server."
  (lambda (request)
    (let* ((method (request-method request))
           (path (request-path request)))
      (dolist (route-entry router)
        (destructuring-bind (route-method route-path route-handler) route-entry
          (when (and (eq method route-method)
                     (path-matches-p route-path path))
            (return-from router-handler (funcall route-handler request)))))
      ;; No matching route - return 404
      (make-response :status 404 :body "Not Found"))))

(defun path-matches-p (pattern path)
  "Check if PATH matches PATTERN.
   Supports trailing * for wildcard matching."
  (cond
    ;; Exact match
    ((string= pattern path) t)
    ;; Wildcard pattern (ends with *)
    ((and (> (length pattern) 0)
          (char= (char pattern (1- (length pattern))) #\*))
     (let ((prefix (subseq pattern 0 (1- (length pattern)))))
       (and (>= (length path) (length prefix))
            (string= prefix (subseq path 0 (length prefix))))))
    ;; No match
    (t nil)))

;;; Query Parameter Parsing

(defun query-params (request)
  "Extract query parameters from request as an alist.
   Parses the query string from the request path."
  (let* ((path (request-path request))
         (query-start (position #\? path)))
    (if query-start
        (parse-query-string (subseq path (1+ query-start)))
        nil)))

(defun parse-query-string (query-string)
  "Parse a query string into an alist of key-value pairs."
  (when (and query-string (> (length query-string) 0))
    (let ((pairs nil))
      (dolist (pair (split-string query-string #\&))
        (let ((eq-pos (position #\= pair)))
          (if eq-pos
              (push (cons (url-decode (subseq pair 0 eq-pos))
                          (url-decode (subseq pair (1+ eq-pos))))
                    pairs)
              (push (cons (url-decode pair) "") pairs))))
      (nreverse pairs))))

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
          do (push (subseq string start i) result)
             (setf start (1+ i)))
    (push (subseq string start) result)
    (nreverse result)))
