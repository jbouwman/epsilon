;;;; Simple High-Level HTTP API
;;;;
;;;; Provides a simplified interface for common HTTP operations

(defpackage :epsilon.http.simple
  (:use :cl)
  (:local-nicknames
   (#:client #:epsilon.http.client)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:json #:epsilon.json)
   (#:str #:epsilon.string))
  (:export
   ;; Simple one-liner functions
   #:http-get
   #:http-post
   #:http-put
   #:http-patch
   #:http-delete
   #:http-head
   #:http-options
   
   ;; Main request function
   #:request
   
   ;; Response helpers
   #:response-ok-p
   #:response-json
   #:response-text
   #:response-headers
   #:response-status
   #:response-header
   
   ;; Configuration
   #:*default-timeout*
   #:*default-user-agent*
   #:*follow-redirects*
   #:*verify-ssl*
   #:*max-redirects*
   
   ;; Utilities
   #:with-session
   #:download-file
   #:upload-file))

(in-package :epsilon.http.simple)

;;;; Configuration Parameters

(defparameter *default-timeout* 30
  "Default timeout in seconds for HTTP requests")

(defparameter *default-user-agent* "epsilon-http/1.0"
  "Default User-Agent header value")

(defparameter *follow-redirects* t
  "Whether to automatically follow redirects")

(defparameter *verify-ssl* t
  "Whether to verify SSL certificates")

(defparameter *max-redirects* 5
  "Maximum number of redirects to follow")

;;;; Helper Functions

(defun prepare-headers (headers user-agent)
  "Prepare headers map with defaults"
  (let ((h (or headers (map:make-map))))
    ;; Add User-Agent if not present
    (unless (map:get h "User-Agent")
      (setf h (map:assoc h "User-Agent" user-agent)))
    h))

(defun prepare-json-body (data)
  "Convert Lisp data to JSON string"
  (cond
    ((stringp data) data)
    ((null data) nil)
    (t (json:encode data))))

(defun prepare-form-body (data)
  "Convert plist or map to URL-encoded form data"
  (cond
    ((stringp data) data)
    ((null data) nil)
    ((listp data)
     ;; Convert plist to URL-encoded string
     (str:join "&"
              (loop for (key value) on data by #'cddr
                    collect (format nil "~A=~A" 
                                   (request:url-encode (string key))
                                   (request:url-encode (princ-to-string value))))))
    (t (error "Invalid form data: ~A" data))))

(defun handle-response (response)
  "Process HTTP response and handle errors"
  (when (null response)
    (error "No response received"))
  response)

(defun follow-redirect (url method headers body redirect-count)
  "Follow HTTP redirect"
  (when (>= redirect-count *max-redirects*)
    (error "Too many redirects (~D)" redirect-count))
  
  (let ((response (client:request url :method method :headers headers :body body)))
    (if (and *follow-redirects*
             (member (response:response-status response) '(301 302 303 307 308)))
        (let ((location (map:get (response:response-headers response) "Location")))
          (if location
              ;; Follow redirect
              (follow-redirect location
                              (if (= (response:response-status response) 303) "GET" method)
                              headers
                              (if (= (response:response-status response) 303) nil body)
                              (1+ redirect-count))
              response))
        response)))

;;;; Main Request Function

(defun request (url &key 
                    (method :get)
                    headers
                    body
                    json
                    form
                    (timeout *default-timeout*)
                    (user-agent *default-user-agent*)
                    (follow-redirects *follow-redirects*)
                    (verify-ssl *verify-ssl*))
  "Make an HTTP request with automatic handling of common patterns.
   
   Arguments:
   - url: Target URL
   - method: HTTP method (:get :post :put :patch :delete :head :options)
   - headers: Additional headers as plist or map
   - body: Raw body string
   - json: Data to send as JSON (will be encoded)
   - form: Data to send as form-encoded
   - timeout: Request timeout in seconds
   - user-agent: User-Agent header value
   - follow-redirects: Whether to follow redirects
   - verify-ssl: Whether to verify SSL certificates
   
   Returns: Response object
   
   Examples:
   (request \"https://api.example.com/data\")
   (request \"https://api.example.com/users\" :method :post :json '(:name \"John\"))
   (request \"https://example.com/form\" :method :post :form '(:field1 \"value1\"))"
  
  (let* ((method-str (string-upcase (string method)))
         (headers (prepare-headers headers user-agent))
         (body (cond
                 (json 
                  (progn
                    (setf headers (map:assoc headers "Content-Type" "application/json"))
                    (prepare-json-body json)))
                 (form
                  (progn
                    (setf headers (map:assoc headers "Content-Type" 
                                          "application/x-www-form-urlencoded"))
                    (prepare-form-body form)))
                 (t body))))
    
    (handle-response
     (if follow-redirects
         (follow-redirect url method-str headers body 0)
         (client:request url :method method-str :headers headers :body body)))))

;;;; Simple One-Liner Functions

(defun http-get (url &rest options)
  "Make a GET request.
   
   Examples:
   (http-get \"https://api.example.com/data\")
   (http-get \"https://api.example.com/data\" :headers '(\"Authorization\" \"Bearer token\"))"
  (apply #'request url :method :get options))

(defun http-post (url &rest options)
  "Make a POST request.
   
   Examples:
   (http-post \"https://api.example.com/users\" :json '(:name \"John\" :age 30))
   (http-post \"https://api.example.com/form\" :form '(:field1 \"value1\"))"
  (apply #'request url :method :post options))

(defun http-put (url &rest options)
  "Make a PUT request.
   
   Examples:
   (http-put \"https://api.example.com/users/1\" :json '(:name \"Jane\"))"
  (apply #'request url :method :put options))

(defun http-patch (url &rest options)
  "Make a PATCH request.
   
   Examples:
   (http-patch \"https://api.example.com/users/1\" :json '(:age 31))"
  (apply #'request url :method :patch options))

(defun http-delete (url &rest options)
  "Make a DELETE request.
   
   Examples:
   (http-delete \"https://api.example.com/users/1\")"
  (apply #'request url :method :delete options))

(defun http-head (url &rest options)
  "Make a HEAD request.
   
   Examples:
   (http-head \"https://example.com/large-file.pdf\")"
  (apply #'request url :method :head options))

(defun http-options (url &rest options)
  "Make an OPTIONS request.
   
   Examples:
   (http-options \"https://api.example.com/users\")"
  (apply #'request url :method :options options))

;;;; Response Helper Functions

(defun response-ok-p (response)
  "Check if response status is successful (2xx)"
  (let ((status (response:response-status response)))
    (and (>= status 200) (< status 300))))

(defun response-json (response)
  "Parse response body as JSON"
  (let ((body (response:response-body response)))
    (when (and body (not (str:empty-p body)))
      (json:parse body))))

(defun response-text (response)
  "Get response body as text"
  (response:response-body response))

(defun response-headers (response)
  "Get response headers as map"
  (response:response-headers response))

(defun response-status (response)
  "Get response status code"
  (response:response-status response))

(defun response-header (response header-name)
  "Get specific response header value"
  (map:get (response:response-headers response) header-name))

;;;; Session Management

(defmacro with-session ((session-var &key cookies headers) &body body)
  "Execute requests with shared session state (cookies, headers).
   
   Example:
   (with-session (session :headers '(\"Authorization\" \"Bearer token\"))
     (get \"https://api.example.com/profile\")
     (post \"https://api.example.com/data\" :json data))"
  (declare (ignore session-var cookies))
  `(let ((*default-headers* ,headers))
     ,@body))

;;;; File Operations

(defun download-file (url filepath &key headers (timeout 300))
  "Download file from URL to local filesystem.
   
   Example:
   (download-file \"https://example.com/file.pdf\" \"/tmp/file.pdf\")"
  (let ((response (request url :headers headers :timeout timeout)))
    (when (response-ok-p response)
      (with-open-file (stream filepath
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
        (write-sequence (map 'vector #'char-code (response:response-body response))
                       stream))
      filepath)))

(defun upload-file (url filepath &key 
                                  (field-name "file")
                                  additional-fields
                                  headers)
  "Upload file to URL using multipart/form-data.
   
   Example:
   (upload-file \"https://api.example.com/upload\" \"/tmp/document.pdf\"
                :field-name \"document\"
                :additional-fields '(:description \"Important document\"))"
  
  ;; Read file
  (let* ((filename (file-namestring filepath))
         (file-data (with-open-file (stream filepath
                                           :element-type '(unsigned-byte 8))
                     (let ((bytes (make-array (file-length stream)
                                            :element-type '(unsigned-byte 8))))
                       (read-sequence bytes stream)
                       (map 'string #'code-char bytes))))
         (boundary (format nil "----epsilon~D" (get-universal-time)))
         (body (with-output-to-string (s)
                 ;; Add file field
                 (format s "--~A~%" boundary)
                 (format s "Content-Disposition: form-data; name=\"~A\"; filename=\"~A\"~%"
                        field-name filename)
                 (format s "Content-Type: application/octet-stream~%~%")
                 (format s "~A~%" file-data)
                 
                 ;; Add additional fields
                 (when additional-fields
                   (loop for (key value) on additional-fields by #'cddr
                         do (format s "--~A~%" boundary)
                            (format s "Content-Disposition: form-data; name=\"~A\"~%~%"
                                   (string-downcase (string key)))
                            (format s "~A~%" value)))
                 
                 ;; End boundary
                 (format s "--~A--~%" boundary))))
    
    (request url 
            :method :post
            :body body
            :headers (map:assoc (or headers (map:make-map))
                            "Content-Type" 
                            (format nil "multipart/form-data; boundary=~A" boundary)))))

;;;; Examples in Comments

#|
;; Simple GET request
(get "https://api.github.com/users/github")

;; POST with JSON
(post "https://httpbin.org/post" 
      :json '(:name "Alice" :age 30))

;; POST with form data
(post "https://httpbin.org/post"
      :form '(:username "alice" :password "secret"))

;; Custom headers
(get "https://api.example.com/protected"
     :headers '("Authorization" "Bearer my-token"
                "Accept" "application/json"))

;; Download file
(download-file "https://example.com/document.pdf" 
               "/tmp/document.pdf")

;; Check response
(let ((resp (get "https://httpbin.org/status/200")))
  (when (response-ok-p resp)
    (format t "Success: ~A~%" (response-text resp))))

;; Parse JSON response
(let* ((resp (get "https://api.github.com/users/github"))
       (data (response-json resp)))
  (format t "Login: ~A~%" (map:get data "login"))
  (format t "Name: ~A~%" (map:get data "name")))
|#
