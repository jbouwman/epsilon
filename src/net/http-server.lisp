(defpackage :epsilon.net.http.server
  (:use
   :cl
   :epsilon.lib.syntax)
  (:local-nicknames
   (:seq :epsilon.lib.sequence)
   (:str :epsilon.lib.string)
   (:time :epsilon.lib.time)
   (:uuid :epsilon.lib.uuid)
   (:uri :epsilon.lib.uri))
  (:export
   ;; Server interface
   :server
   :make-server
   :start
   :stop
   :server-running-p
   :server-address
   :server-port
   :server-handler
   
   ;; Request and response interface
   :request
   :request-method
   :request-url
   :request-path
   :request-query
   :request-headers
   :request-body
   :request-remote-addr
   :request-protocol
   
   :response
   :response-status
   :response-headers
   :response-body
   :with-response
   :make-response
   :set-header
   :set-status
   :write-body
   
   ;; Handler interface
   :handler
   :handler-func
   :defhandler
   :serve-file
   :serve-directory
   
   ;; Routing
   :router
   :make-router
   :connect-route
   :route
   
   ;; Middleware
   :middleware
   :apply-middleware
   :with-middleware
   :logging-middleware
   :cors-middleware
   :static-files-middleware
   :session-middleware
   
   ;; Configuration
   :*default-server-port*
   :*default-read-timeout*
   :*default-write-timeout*
   :*default-idle-timeout*
   :*default-max-header-bytes*))

(in-package :epsilon.net.http.server)

;;; Constants and global variables

(define-constant +crlf+ (format nil "~C~C" #\Return #\Linefeed)
  "HTTP line ending sequence.")

(defparameter *default-server-port* 8080
  "Default HTTP server port.")

(defparameter *default-read-timeout* 60
  "Default read timeout in seconds.")

(defparameter *default-write-timeout* 60
  "Default write timeout in seconds.")

(defparameter *default-idle-timeout* 180
  "Default idle timeout in seconds.")

(defparameter *default-max-header-bytes* 1048576
  "Default max header size (1MB).")

(defparameter *default-server-name* 
  (format nil "epsilon-http/1.0 (~A ~A)"
          (lisp-implementation-type)
          (lisp-implementation-version))
  "Default server name for Server header.")

;;; HTTP Status Code utilities

(defparameter *status-codes*
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (102 . "Processing")
    (103 . "Early Hints")
    (200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (203 . "Non-Authoritative Information")
    (204 . "No Content")
    (205 . "Reset Content")
    (206 . "Partial Content")
    (207 . "Multi-Status")
    (208 . "Already Reported")
    (226 . "IM Used")
    (300 . "Multiple Choices")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (305 . "Use Proxy")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment Required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (407 . "Proxy Authentication Required")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (410 . "Gone")
    (411 . "Length Required")
    (412 . "Precondition Failed")
    (413 . "Payload Too Large")
    (414 . "URI Too Long")
    (415 . "Unsupported Media Type")
    (416 . "Range Not Satisfiable")
    (417 . "Expectation Failed")
    (418 . "I'm a teapot")
    (421 . "Misdirected Request")
    (422 . "Unprocessable Entity")
    (423 . "Locked")
    (424 . "Failed Dependency")
    (425 . "Too Early")
    (426 . "Upgrade Required")
    (428 . "Precondition Required")
    (429 . "Too Many Requests")
    (431 . "Request Header Fields Too Large")
    (451 . "Unavailable For Legal Reasons")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout")
    (505 . "HTTP Version Not Supported")
    (506 . "Variant Also Negotiates")
    (507 . "Insufficient Storage")
    (508 . "Loop Detected")
    (510 . "Not Extended")
    (511 . "Network Authentication Required"))
  "HTTP status codes and their text descriptions.")

(defun status-text (code)
  "Get the text description for a status code."
  (or (cdr (assoc code *status-codes*))
      "Unknown Status Code"))

;;; MIME Types

(defparameter *mime-types*
  '(("html" . "text/html")
    ("htm" . "text/html")
    ("css" . "text/css")
    ("js" . "application/javascript")
    ("json" . "application/json")
    ("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("svg" . "image/svg+xml")
    ("ico" . "image/x-icon")
    ("pdf" . "application/pdf")
    ("txt" . "text/plain")
    ("xml" . "application/xml")
    ("csv" . "text/csv")
    ("mp3" . "audio/mpeg")
    ("mp4" . "video/mp4")
    ("webm" . "video/webm")
    ("zip" . "application/zip")
    ("woff" . "font/woff")
    ("woff2" . "font/woff2")
    ("ttf" . "font/ttf")
    ("otf" . "font/otf"))
  "Common MIME types by file extension.")

(defun get-mime-type (filename)
  "Determine the MIME type for a file based on its extension."
  (let ((extension (pathname-type filename)))
    (or (cdr (assoc extension *mime-types* :test #'string-equal))
        "application/octet-stream")))

;;; Request implementation

(defstruct request
  "HTTP request structure."
  (method :get :type keyword)
  (url "" :type string)
  (path "/" :type string)
  (query nil :type list)
  (protocol "HTTP/1.1" :type string)
  (headers (make-hash-table :test 'equal) :type hash-table)
  (body nil)
  (remote-addr nil)
  (cookies nil)
  (params nil))

(defun parse-request-uri (uri-string)
  "Parse a request URI into path and query parameters."
  (let* ((uri (uri:uri uri-string))
         (path (uri:path uri))
         (query (uri:query uri))
         (params nil))
    
    ;; Parse query params if present
    (when query
      (setf params
            (loop for param-pair in (str:split #\& query)
                  for eq-pos = (position #\= param-pair)
                  collect (if eq-pos
                              (cons (subseq param-pair 0 eq-pos)
                                    (uri:url-decode (subseq param-pair (1+ eq-pos))))
                              (cons param-pair "")))))
    
    (values (if (string= path "") "/" path)
            params)))

(defun parse-request-line (line)
  "Parse an HTTP request line into method, URI, and protocol."
  (let ((parts (str:split #\Space line :remove-empty-subseqs t)))
    (when (< (length parts) 3)
      (error "Invalid HTTP request line: ~A" line))
    
    (let ((method (intern (string-upcase (first parts)) :keyword))
          (uri (second parts))
          (protocol (third parts)))
      
      (multiple-value-bind (path query-params)
          (parse-request-uri uri)
        (values method uri path query-params protocol)))))

(defun parse-headers (stream)
  "Parse HTTP headers from a stream."
  (let ((headers (make-hash-table :test 'equal)))
    (loop for line = (read-line stream nil nil)
          until (or (null line) (string= line ""))
          do (let* ((colon-pos (position #\: line))
                    (name (string-trim '(#\Space #\Tab)
                                       (subseq line 0 colon-pos)))
                    (value (string-trim '(#\Space #\Tab)
                                        (subseq line (1+ colon-pos)))))
               (setf (gethash (string-downcase name) headers) value)))
    headers))

(defun read-request-body (stream headers max-body-size)
  "Read the request body based on Content-Length or Transfer-Encoding."
  (let ((content-length (gethash "content-length" headers))
        (transfer-encoding (gethash "transfer-encoding" headers)))
    
    (cond
      ;; Handle chunked encoding
      ((and transfer-encoding (string-equal transfer-encoding "chunked"))
       (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
         (loop
           (let* ((chunk-size-line (read-line stream))
                  (chunk-size (parse-integer chunk-size-line
                                            :radix 16 :junk-allowed t)))
             (when (zerop chunk-size)
               ;; Read trailing CRLF
               (read-line stream)
               (return buffer))
             
             ;; Check max size limit
             (when (and max-body-size 
                       (> (+ (length buffer) chunk-size) max-body-size))
               (error "Request body too large"))
             
             ;; Read chunk data
             (let ((chunk (make-array chunk-size :element-type '(unsigned-byte 8))))
               (read-sequence chunk stream)
               (adjust-array buffer (+ (length buffer) chunk-size)
                            :fill-pointer (+ (length buffer) chunk-size))
               (replace buffer chunk :start1 (- (length buffer) chunk-size)))
             
             ;; Skip CRLF after chunk
             (read-line stream)))))
      
      ;; Handle Content-Length
      (content-length
       (let* ((length (parse-integer content-length))
              (buffer (make-array length :element-type '(unsigned-byte 8))))
         
         ;; Check max size limit
         (when (and max-body-size (> length max-body-size))
           (error "Request body too large"))
         
         (read-sequence buffer stream)
         buffer))
      
      ;; No body
      (t nil))))

(defun parse-cookies (cookie-header)
  "Parse Cookie header into alist of cookies."
  (when cookie-header
    (loop for cookie-pair in (str:split #\; cookie-header)
          for pair = (string-trim '(#\Space) cookie-pair)
          for eq-pos = (position #\= pair)
          when eq-pos
          collect (cons (subseq pair 0 eq-pos)
                        (subseq pair (1+ eq-pos))))))

(defun parse-request (stream &key (max-header-size *default-max-header-bytes*)
                                 (max-body-size nil))
  "Parse a complete HTTP request from a stream."
  (handler-case
      (let* ((request-line (read-line stream))
             (headers (parse-headers stream)))
        
        (multiple-value-bind (method uri path query-params protocol)
            (parse-request-line request-line)
          
          (let ((body (read-request-body stream headers max-body-size))
                (cookies (parse-cookies (gethash "cookie" headers))))
            
            (make-request :method method
                         :url uri
                         :path path
                         :query query-params
                         :protocol protocol
                         :headers headers
                         :body body
                         :cookies cookies))))
    (error (e)
      (format *error-output* "Error parsing request: ~A~%" e)
      nil)))

;;; Response implementation

(defclass response ()
  ((status :initform 200
           :accessor response-status)
   (headers :initform (make-hash-table :test 'equal)
            :accessor response-headers)
   (body :initform nil
         :accessor response-body)
   (stream :initarg :stream
           :accessor response-stream)
   (sent :initform nil
         :accessor response-sent-p)))

(defun make-response (stream)
  "Create a new response object for the given stream."
  (let ((response (make-instance 'response :stream stream)))
    ;; Set default headers
    (setf (gethash "server" (response-headers response)) *default-server-name*)
    (setf (gethash "date" (response-headers response)) 
          (time:format-rfc1123-timestring nil (time:now)))
    response))

(defun set-status (response status)
  "Set the HTTP status code for a response."
  (setf (response-status response) status))

(defun set-header (response name value)
  "Set a header in the response."
  (setf (gethash (string-downcase name) (response-headers response)) value))

(defun write-response-headers (response)
  "Write the status line and headers to the response stream."
  (let ((stream (response-stream response)))
    
    ;; Write status line
    (format stream "HTTP/1.1 ~D ~A~A"
            (response-status response)
            (status-text (response-status response))
            +crlf+)
    
    ;; Write headers
    (maphash (lambda (name value)
               (format stream "~A: ~A~A" name value +crlf+))
             (response-headers response))
    
    ;; End headers
    (format stream +crlf+)
    (force-output stream)))

(defun write-body (response data)
  "Write data to the response body."
  (unless (response-sent-p response)
    (write-response-headers response)
    (setf (response-sent-p response) t))
  
  (let ((stream (response-stream response)))
    (etypecase data
      (string (write-string data stream))
      ((array (unsigned-byte 8) (*)) (write-sequence data stream))
      (pathname (with-open-file (file data :element-type '(unsigned-byte 8))
                  (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
                    (loop for bytes-read = (read-sequence buffer file)
                          until (zerop bytes-read)
                          do (write-sequence buffer stream :end bytes-read))))))
    
    (force-output stream)))

(defun send-response (response)
  "Send the response if it hasn't been sent already."
  (unless (response-sent-p response)
    ;; If we have a body, set Content-Length if not already set
    (when (response-body response)
      (unless (gethash "content-length" (response-headers response))
        (set-header response "content-length" 
                    (etypecase (response-body response)
                      (string (length (the string (response-body response))))
                      ((array (unsigned-byte 8) (*)) 
                       (length (the (array (unsigned-byte 8) (*)) 
                                   (response-body response))))
                      (pathname (file-size (response-body response))))))
      
      ;; Write headers and then body
      (write-response-headers response)
      (write-body response (response-body response)))
    
    ;; Just write headers if no body (or already written)
    (unless (response-body response)
      (write-response-headers response))
    
    (setf (response-sent-p response) t)))

(defmacro with-response ((var stream) &body body)
  "Execute body with VAR bound to a new response, and send it at the end."
  `(let ((,var (make-response ,stream)))
     (unwind-protect
          (progn ,@body)
       (send-response ,var))))

;;; Routing

(defclass route ()
  ((path :initarg :path
         :reader route-path)
   (handler :initarg :handler
           :reader route-handler)
   (method :initarg :method
           :reader route-method)))

(defun path-match-p (route-path request-path)
  "Check if a route path matches a request path, handling patterns."
  ;; Simple exact match first
  (if (string= route-path request-path)
      (values t nil)
      ;; Handle patterns with placeholders
      (let* ((route-parts (str:split #\/ route-path 
                                        :remove-empty-subseqs t))
             (request-parts (str:split #\/ request-path
                                          :remove-empty-subseqs t))
             (params nil))
        
        ;; Different number of path segments => no match
        (when (/= (length route-parts) (length request-parts))
          (return-from path-match-p (values nil nil)))
        
        ;; Check each segment
        (loop for route-part in route-parts
              for request-part in request-parts
              for i from 0
              do (if (and (> (length route-part) 0)
                          (char= (char route-part 0) #\:))
                     ;; This is a placeholder - extract parameter
                     (push (cons (subseq route-part 1) request-part) params)
                     ;; Regular segment - must match exactly
                     (unless (string= route-part request-part)
                       (return-from path-match-p (values nil nil)))))
        
        ;; If we got here, route matches with parameters
        (values t params))))

(defclass router ()
  ((routes :initform nil
           :accessor router-routes)))

(defun make-router ()
  "Create a new router."
  (make-instance 'router))

(defun connect-route (router path handler &key (method :any))
  "Add a route to the router."
  (push (make-instance 'route :path path :handler handler :method method)
        (router-routes router))
  router)

(defun find-route (router request)
  "Find a matching route for a request."
  (loop for route in (router-routes router)
        for path = (request-path request)
        for method = (request-method request)
        when (and (or (eq (route-method route) :any)
                      (eq (route-method route) method))
                  (multiple-value-bind (match-p params)
                      (path-match-p (route-path route) path)
                    (when match-p
                      (setf (request-params request) 
                            (append params (request-params request)))
                      t)))
        return (route-handler route)
        finally (return nil)))

(defun route (router request response)
  "Route a request to the appropriate handler."
  (let ((handler (find-route router request)))
    (if handler
        (funcall handler request response)
        ;; No route found - 404
        (progn
          (setf (response-status response) 404)
          (setf (response-body response) "404 Not Found")
          (set-header response "content-type" "text/plain")))))

;;; Handler implementation

(defclass handler ()
  ((func :initarg :func
         :reader handler-func)))

(defun make-handler (func)
  "Create a handler with the given function."
  (make-instance 'handler :func func))

(defmethod print-object ((handler handler) stream)
  (print-unreadable-object (handler stream :type t)
    (format stream "~S" (handler-func handler))))

(defmacro defhandler (name args &body body)
  "Define a handler function."
  `(defun ,name ,args
     ,@body))

;;; Middleware

(defclass middleware ()
  ((handler :initarg :handler
            :accessor middleware-handler)
   (next :initarg :next
         :accessor middleware-next)))

(defun apply-middleware (request response middleware-handler)
  "Apply a middleware handler to a request/response."
  (funcall (middleware-handler middleware-handler) request response))

(defun compose-middleware (middleware-list final-handler)
  "Compose a chain of middleware ending with the final handler."
  (reduce (lambda (next middleware-constructor)
            (funcall middleware-constructor next))
          middleware-list
          :initial-value final-handler
          :from-end t))

(defmacro with-middleware ((request response) middleware-list &body body)
  "Execute body with middleware applied."
  (let ((request-var (gensym "REQUEST"))
        (response-var (gensym "RESPONSE"))
        (handler-var (gensym "HANDLER")))
    `(let* ((,request-var ,request)
            (,response-var ,response)
            (,handler-var (lambda (req res)
                            (declare (ignore req res))
                            ,@body)))
       (funcall (compose-middleware ,middleware-list ,handler-var)
                ,request-var ,response-var))))

;;; Standard middleware implementations

(defun logging-middleware (next-handler)
  "Middleware that logs requests."
  (lambda (request response)
    (let ((start-time (get-internal-real-time)))
      (format t "[~A] ~A ~A~%"
              (time:format-rfc3339-timestring nil (time:now))
              (request-method request)
              (request-url request))
      (multiple-value-prog1
          (funcall next-handler request response)
        (let* ((end-time (get-internal-real-time))
               (duration (/ (- end-time start-time) 
                           internal-time-units-per-second)))
          (format t "[~A] ~A ~A -> ~D (~,3Fs)~%"
                  (time:format-rfc3339-timestring nil (time:now))
                  (request-method request)
                  (request-url request)
                  (response-status response)
                  duration))))))

(defun cors-middleware (&key (origins "*")
                             (methods "GET, POST, PUT, DELETE, OPTIONS")
                             (headers "Content-Type, Authorization")
                             (credentials nil))
  "Middleware that handles CORS."
  (lambda (next-handler)
    (lambda (request response)
      ;; Set CORS headers
      (set-header response "Access-Control-Allow-Origin" origins)
      (set-header response "Access-Control-Allow-Methods" methods)
      (set-header response "Access-Control-Allow-Headers" headers)
      (when credentials
        (set-header response "Access-Control-Allow-Credentials" "true"))
      
      ;; Handle preflight request
      (if (eq (request-method request) :options)
          (progn
            (setf (response-status response) 204)
            (setf (response-body response) nil))
          (funcall next-handler request response)))))

(defun static-files-middleware (root-dir &key (url-prefix "/static/"))
  "Middleware that serves static files."
  (lambda (next-handler)
    (lambda (request response)
      (let ((path (request-path request)))
        (if (and (eq (request-method request) :get)
                 (string-prefix-p url-prefix path))
            (let* ((file-path (subseq path (length url-prefix)))
                   (full-path (merge-pathnames file-path root-dir)))
              (if (and (probe-file full-path)
                       (not (directory-pathname-p full-path)))
                  (serve-file full-path response)
                  (funcall next-handler request response)))
            (funcall next-handler request response))))))

(defun session-middleware (&key (cookie-name "session") 
                               (timeout 3600)
                               (secure nil))
  "Simple session middleware."
  (let ((sessions (make-hash-table :test 'equal))
        (lock (make-lock "session-middleware-lock")))
    (lambda (next-handler)
      (lambda (request response)
        (let* ((cookies (request-cookies request))
               (session-id (cdr (assoc cookie-name cookies :test #'string=)))
               (session nil))
          
          ;; Get or create session
          (with-lock (lock)
            (if session-id
                (setf session (gethash session-id sessions))
                (let ((new-id (write-to-string (uuid:make-v4-uuid))))
                  (setf session-id new-id)
                  (setf session (make-hash-table :test 'equal))
                  (setf (gethash session-id sessions) session)
                  
                  ;; Set session cookie
                  (set-header response "Set-Cookie"
                              (format nil "~A=~A; Path=/; Max-Age=~D~A"
                                      cookie-name
                                      session-id
                                      timeout
                                      (if secure "; Secure; HttpOnly" ""))))))
          
          ;; Add session to request and proceed
          (setf (request-session request) session)
          (funcall next-handler request response))))))

;;; Utilities for serving files and directories

(defun serve-file (path response)
  "Serve a file with appropriate headers."
  (if (probe-file path)
      (let ((mime-type (get-mime-type path)))
        (set-header response "Content-Type" mime-type)
        (set-header response "Content-Length" (file-size path))
        (setf (response-body response) path))
      (progn
        (setf (response-status response) 404)
        (setf (response-body response) "404 Not Found")
        (set-header response "Content-Type" "text/plain"))))

(defun generate-directory-listing (path request)
  "Generate HTML for a directory listing."
  (with-output-to-string (s)
    (format s "<html><head><title>Directory listing for ~A</title>~%" 
            (request-path request))
    (format s "<style>body{font-family:sans-serif}table{border-collapse:collapse}~%")
    (format s "th,td{padding:8px;text-align:left}th{border-bottom:2px solid #ddd}~%")
    (format s "tr:hover{background-color:#f5f5f5}</style></head>~%")
    (format s "<body><h1>Directory listing for ~A</h1>~%" 
            (request-path request))
    (format s "<table><tr><th>Name</th><th>Size</th><th>Modified</th></tr>~%")
    
    ;; Parent directory link
    (unless (string= (request-path request) "/")
      (format s "<tr><td><a href=\"../\">../</a></td><td>-</td><td>-</td></tr>~%"))
    
    ;; List directories first
    (dolist (entry (sort (directory (merge-pathnames "*.*" path))
                         #'string<
                         :key #'pathname-name))
      (let ((name (file-namestring entry))
            (size (if (directory-pathname-p entry) "-" (file-size entry)))
            (mtime (file-write-date entry)))
        (format s "<tr><td><a href=\"~A~A\">~A~A</a></td><td>~A</td><td>~A</td></tr>~%"
                name
                (if (directory-pathname-p entry) "/" "")
                name
                (if (directory-pathname-p entry) "/" "")
                size
                (time:format-rfc3339-timestring 
                 nil (time:universal-to-timestamp mtime)))))
    
    (format s "</table></body></html>~%")))

(defun serve-directory (path request response &key (allow-listing t) (index "index.html"))
  "Serve a directory with option for directory listing."
  ;; Try to serve index file first
  (let ((index-path (merge-pathnames index path)))
    (if (probe-file index-path)
        (serve-file index-path response)
        ;; No index file, generate listing if allowed
        (if allow-listing
            (let ((listing (generate-directory-listing path request)))
              (set-header response "Content-Type" "text/html")
              (setf (response-body response) listing))
            ;; Listing not allowed - return 403
            (progn
              (setf (response-status response) 403)
              (setf (response-body response) "403 Forbidden")
              (set-header response "Content-Type" "text/plain"))))))

;;; Server implementation

(defclass server ()
  ((address :initarg :address
            :initform "127.0.0.1"
            :reader server-address)
   (port :initarg :port
         :initform *default-server-port*
         :reader server-port)
   (handler :initarg :handler
            :accessor server-handler)
   (middleware :initarg :middleware
              :initform nil
              :accessor server-middleware)
   (read-timeout :initarg :read-timeout
                :initform *default-read-timeout*
                :reader server-read-timeout)
   (write-timeout :initarg :write-timeout
                 :initform *default-write-timeout*
                 :reader server-write-timeout)
   (idle-timeout :initarg :idle-timeout
                :initform *default-idle-timeout*
                :reader server-idle-timeout)
   (max-header-bytes :initarg :max-header-bytes
                    :initform *default-max-header-bytes*
                    :reader server-max-header-bytes)
   (running :initform nil
            :accessor server-running)
   (listener :initform nil
             :accessor server-listener)
   (threads :initform nil
            :accessor server-threads)
   (thread-count :initarg :thread-count
                :initform 10
                :reader server-thread-count)
   (ssl-context :initarg :ssl-context
               :initform nil
               :reader server-ssl-context)))

(defun make-server (handler &key 
                         (address "127.0.0.1")
                         (port *default-server-port*)
                         (middleware nil)
                         (read-timeout *default-read-timeout*)
                         (write-timeout *default-write-timeout*)
                         (idle-timeout *default-idle-timeout*)
                         (max-header-bytes *default-max-header-bytes*)
                         (thread-count 10)
                         ssl-cert-file
                         ssl-key-file)
  "Create a new HTTP server."
  (let ((ssl-context (when (and ssl-cert-file ssl-key-file)
                       (let ((ctx (epsilon.net.tls:make-context)))
                         (epsilon.net.tls:use-certificate-chain-file ctx ssl-cert-file)
                         (epsilon.net.tls:use-private-key-file ctx ssl-key-file)
                         ctx))))
    
    (make-instance 'server
                   :address address
                   :port port
                   :handler handler
                   :middleware middleware
                   :read-timeout read-timeout
                   :write-timeout write-timeout
                   :idle-timeout idle-timeout
                   :max-header-bytes max-header-bytes
                   :thread-count thread-count
                   :ssl-context ssl-context)))

(defun server-running-p (server)
  "Check if the server is running."
  (server-running server))

(defun handle-client-connection (connection server)
  "Handle a client connection with the server's handler."
  (handler-case
      (let* ((stream (if (server-ssl-context server)
                        (epsilon.net.tls:make-ssl-server-stream
                         (epsilon.net:socket-stream connection)
                         (server-ssl-context server))
                        (epsilon.net:socket-stream connection)))
             (request (parse-request stream
                                   :max-header-size (server-max-header-bytes server)))
             (response (make-response stream)))
        
        ;; Set connection timeout
        (setf (epsilon.net:socket-option connection :receive-timeout)
              (server-read-timeout server))
        
        ;; Set client address in request
        (setf (request-remote-addr request)
              (epsilon.net:socket-peer-address connection))
        
        ;; Process request with middleware chain
        (let ((handler-func 
                (if (server-middleware server)
                    (compose-middleware (server-middleware server)
                                       (server-handler server))
                    (server-handler server))))
          (funcall handler-func request response)
          (send-response response))
        
        ;; Close the stream
        (close stream))
    (error (e)
      (format *error-output* "Error handling client connection: ~A~%" e))))

(defun accept-connections (server)
  "Accept and handle incoming connections."
  (loop while (server-running-p server)
        do (handler-case
               (let ((connection (epsilon.net:socket-accept 
                                  (server-listener server))))
                 ;; Handle in a new thread
                 (epsilon.sys.sync.thread:make-thread
                  (lambda () 
                    (unwind-protect
                         (handle-client-connection connection server)
                      (epsilon.net:socket-close connection)))
                  :name "http-server-client-thread"))
             (error (e)
               (format *error-output* "Error accepting connection: ~A~%" e)))))

(defun start (server)
  "Start the HTTP server."
  (when (server-running-p server)
    (error "Server is already running"))
  
  ;; Create listener socket
  (setf (server-listener server)
        (epsilon.net:socket-listen 
         (server-address server)
         (server-port server)
         :reuse-address t
         :backlog 10))
  
  ;; Mark as running
  (setf (server-running server) t)
  
  ;; Start accept thread
  (setf (server-threads server)
        (list (epsilon.sys.sync.thread:make-thread
               (lambda () (accept-connections server))
               :name "http-server-accept-thread")))
  
  ;; Return server
  server)

(defun stop (server)
  "Stop the HTTP server."
  (unless (server-running-p server)
    (error "Server is not running"))
  
  ;; Mark as not running
  (setf (server-running server) nil)
  
  ;; Close listener socket
  (epsilon.net:socket-close (server-listener server))
  
  ;; Wait for threads to finish
  (dolist (thread (server-threads server))
    (epsilon.sys.sync.thread:join-thread thread))
  
  (setf (server-threads server) nil)
  (setf (server-listener server) nil)
  
  ;; Return server
  server)

;;; String utilities

(defun string-prefix-p (prefix string)
  "Check if STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

(defun file-size (path)
  "Get the size of a file in bytes."
  (with-open-file (file path :element-type '(unsigned-byte 8))
    (file-length file)))

(defun directory-pathname-p (pathname)
  "Return true if PATHNAME refers to a directory."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and (not (component-present-p (pathname-name pathname)))
         (not (component-present-p (pathname-type pathname)))
         pathname)))
