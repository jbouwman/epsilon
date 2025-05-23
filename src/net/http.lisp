(defpackage :epsilon.net.http
  (:use
   :cl
   :epsilon.lib.syntax)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)
   (:uri :epsilon.lib.uri)
   (:time :epsilon.lib.time)
   (:stream :epsilon.lib.stream))
  (:shadow
   :get
   :delete)
  (:export
   ;; Main interface
   :request
   :get
   :post
   :head
   :put
   :patch
   :delete
   :fetch
   
   ;; Client
   :client
   :make-client
   :client-transport
   :with-client
   
   ;; Transport interface
   :transport
   :default-transport
   :tls-transport
   :make-transport
   :round-trip
   
   ;; Request/response objects
   :request-object
   :make-request
   :request-method
   :request-url
   :request-headers
   :request-body
   :request-timeout
   
   ;; Response object
   :response-object
   :response-status
   :response-headers
   :response-body
   :response-proto
   :response-request
   :response-content-length))

(in-package :epsilon.net.http)

;;;; HTTP Client with Transport Abstraction
;;;
;;; This implementation is inspired by Go's net/http package, which uses
;;; a transport abstraction to separate the HTTP protocol handling from
;;; the underlying network operations. This allows different transport
;;; implementations (e.g., plain TCP, TLS, etc.) and features like
;;; connection pooling.

;;; Constants and utilities

(defparameter *default-user-agent* 
  "epsilon-http/1.0"
  "Default User-Agent header to send with requests.")

(defparameter *default-timeout* 30
  "Default request timeout in seconds.")

(defparameter *max-idle-conns-per-host* 2
  "Maximum number of idle connections to keep per host.")

(defparameter *max-idle-conns* 100
  "Maximum total number of idle connections across all hosts.")

(defparameter *idle-conn-timeout* 90
  "Time in seconds to keep an idle connection before closing it.")

(defvar *crlf* (format nil "~C~C" #\Return #\Linefeed)
  "HTTP line ending sequence.")

(defconstant +max-redirects+ 10
  "Maximum number of redirects to follow.")

;;; Request object

(defstruct request-object
  "HTTP request structure."
  (method :get :type keyword)
  (url "" :type string)
  (headers (map:+empty+) :type map::hamt)
  (body nil)
  (timeout *default-timeout* :type number))

;;; Response object

(defstruct response-object
  "HTTP response structure."
  (status 0 :type integer)
  (headers (map:+empty+) :type map::hamt)
  (body nil)
  (proto "HTTP/1.1" :type string)
  (request nil :type (or null request-object))
  (content-length -1 :type integer))

;;; Transport interface

(defclass transport ()
  ((name :initarg :name
         :initform "abstract-transport"
         :reader transport-name)
   (dial-timeout :initarg :dial-timeout
                :initform 30
                :accessor transport-dial-timeout)
   (read-timeout :initarg :read-timeout
                :initform 30
                :accessor transport-read-timeout)
   (write-timeout :initarg :write-timeout
                 :initform 30
                 :accessor transport-write-timeout))
  (:documentation "Abstract transport interface for HTTP client."))

(defgeneric round-trip (transport request)
  (:documentation "Execute a request using the transport and return a response."))

;;; Default transport implementation

(defclass default-transport (transport)
  ((idle-conns :initform (make-hash-table :test 'equal)
              :accessor transport-idle-conns)
   (idle-conns-mutex :initform (epsilon.sys.sync.lock:make-lock "idle-conns-lock")
                    :reader transport-idle-conns-mutex)
   (max-idle-conns :initarg :max-idle-conns
                  :initform *max-idle-conns*
                  :accessor transport-max-idle-conns)
   (max-idle-conns-per-host :initarg :max-idle-conns-per-host
                           :initform *max-idle-conns-per-host*
                           :accessor transport-max-idle-conns-per-host)
   (idle-conn-timeout :initarg :idle-conn-timeout
                     :initform *idle-conn-timeout*
                     :accessor transport-idle-conn-timeout))
  (:documentation "Default transport implementation with connection pooling."))

(defmethod initialize-instance :after ((transport default-transport) &key)
  (setf (slot-value transport 'name) "default-transport"))

;;; TLS transport implementation

(defclass tls-transport (default-transport)
  ((verify :initarg :verify
          :initform t
          :accessor transport-verify)
   (ca-file :initarg :ca-file
           :initform nil
           :accessor transport-ca-file)
   (cert-file :initarg :cert-file
             :initform nil
             :accessor transport-cert-file)
   (key-file :initarg :key-file
            :initform nil
            :accessor transport-key-file))
  (:documentation "TLS transport implementation for HTTPS connections."))

(defmethod initialize-instance :after ((transport tls-transport) &key)
  (setf (slot-value transport 'name) "tls-transport"))

;;; HTTP client

(defclass client ()
  ((transport :initarg :transport
             :accessor client-transport)
   (timeout :initarg :timeout
           :initform *default-timeout*
           :accessor client-timeout)
   (jar :initarg :jar
       :initform nil
       :accessor client-jar)
   (follow-redirects :initarg :follow-redirects
                    :initform t
                    :accessor client-follow-redirects))
  (:documentation "HTTP client with configurable transport."))

(defun make-client (&key 
                   (transport nil) 
                   (timeout *default-timeout*)
                   (jar nil)
                   (follow-redirects t)
                   (tls nil))
  "Create a new HTTP client.
   
   If TRANSPORT is nil, creates a default transport.
   If TLS is true, creates a TLS transport.
   JAR is a cookie jar (not implemented yet).
   FOLLOW-REDIRECTS determines whether to automatically follow redirects."
  (let ((transport (cond
                    (transport transport)
                    (tls (make-instance 'tls-transport))
                    (t (make-instance 'default-transport)))))
    (make-instance 'client
                  :transport transport
                  :timeout timeout
                  :jar jar
                  :follow-redirects follow-redirects)))

(defun make-transport (&key 
                      (type :default)
                      (dial-timeout 30)
                      (read-timeout 30)
                      (write-timeout 30)
                      (max-idle-conns *max-idle-conns*)
                      (max-idle-conns-per-host *max-idle-conns-per-host*)
                      (idle-conn-timeout *idle-conn-timeout*)
                      (verify t)
                      (ca-file nil)
                      (cert-file nil)
                      (key-file nil))
  "Create a transport instance of the specified type.
   
   TYPE can be :DEFAULT or :TLS.
   DIAL-TIMEOUT, READ-TIMEOUT, WRITE-TIMEOUT are in seconds.
   MAX-IDLE-CONNS is the maximum number of idle connections.
   MAX-IDLE-CONNS-PER-HOST is the maximum number of idle connections per host.
   IDLE-CONN-TIMEOUT is the maximum time to keep an idle connection.
   
   For TLS transport:
   VERIFY determines whether to verify certificates.
   CA-FILE is the path to a CA certificate file.
   CERT-FILE is the path to a client certificate file.
   KEY-FILE is the path to a client key file."
  (case type
    (:tls
     (make-instance 'tls-transport
                   :dial-timeout dial-timeout
                   :read-timeout read-timeout
                   :write-timeout write-timeout
                   :max-idle-conns max-idle-conns
                   :max-idle-conns-per-host max-idle-conns-per-host
                   :idle-conn-timeout idle-conn-timeout
                   :verify verify
                   :ca-file ca-file
                   :cert-file cert-file
                   :key-file key-file))
    (t
     (make-instance 'default-transport
                   :dial-timeout dial-timeout
                   :read-timeout read-timeout
                   :write-timeout write-timeout
                   :max-idle-conns max-idle-conns
                   :max-idle-conns-per-host max-idle-conns-per-host
                   :idle-conn-timeout idle-conn-timeout))))

(defmacro with-client ((var &rest options) &body body)
  "Execute BODY with VAR bound to a new client.
   
   OPTIONS are passed to MAKE-CLIENT."
  `(let ((,var (make-client ,@options)))
     (unwind-protect
          (progn ,@body)
       ;; Could add cleanup here if needed
       )))

;;; Connection pooling implementation

(defstruct connection
  "Connection structure for the connection pool."
  socket
  stream
  (last-used (time:now) :type time:timestamp)
  (host "" :type string)
  (port 0 :type integer)
  (tls nil :type boolean))

(defun get-connection-key (host port tls)
  "Generate a key for the connection pool."
  (format nil "~A:~D:~A" host port (if tls "tls" "plain")))

(defun get-idle-connection (transport host port tls)
  "Get an idle connection from the pool or nil if none available."
  (epsilon.sys.sync.lock:with-lock ((transport-idle-conns-mutex transport))
    (let* ((key (get-connection-key host port tls))
           (conns (gethash key (transport-idle-conns transport))))
      (when conns
        (let ((conn (pop conns)))
          (if conns
              (setf (gethash key (transport-idle-conns transport)) conns)
              (remhash key (transport-idle-conns transport)))
          conn)))))

(defun put-idle-connection (transport conn)
  "Put a connection back into the idle pool."
  (let ((host (connection-host conn))
        (port (connection-port conn))
        (tls (connection-tls conn)))
    (epsilon.sys.sync.lock:with-lock ((transport-idle-conns-mutex transport))
      (let* ((key (get-connection-key host port tls))
             (conns (gethash key (transport-idle-conns transport) nil)))
        (if (and conns (< (length conns) (transport-max-idle-conns-per-host transport)))
            (push conn conns)
            (setf conns (list conn)))
        (setf (gethash key (transport-idle-conns transport)) conns)))))

(defun close-idle-connections (transport)
  "Close all idle connections in the pool."
  (epsilon.sys.sync.lock:with-lock ((transport-idle-conns-mutex transport))
    (maphash (lambda (key conns)
               (declare (ignore key))
               (dolist (conn conns)
                 (close (connection-stream conn))
                 (epsilon.net:socket-close (connection-socket conn))))
             (transport-idle-conns transport))
    (clrhash (transport-idle-conns transport))))

;;; Connection establishment

(defun dial (transport host port tls)
  "Establish a connection to the given host and port."
  (let ((conn (get-idle-connection transport host port tls)))
    (when conn
      ;; Check if the connection is still valid
      (when (time:timestamp< (time:now)
                             (time:timestamp+ (connection-last-used conn)
                                              (transport-idle-conn-timeout transport)
                                              :unit :sec))
        (return-from dial conn))
      ;; Connection too old, close it
      (close (connection-stream conn))
      (epsilon.net:socket-close (connection-socket conn)))
    
    ;; Create a new connection
    (let ((socket (epsilon.net:socket-connect host port 
                                            :timeout (transport-dial-timeout transport))))
      (if tls
          (let* ((ssl-stream (epsilon.net.tls:make-ssl-client-stream 
                             (epsilon.net:socket-stream socket)
                             :hostname host
                             :deadline (+ (get-internal-real-time)
                                        (* (transport-dial-timeout transport) 
                                           internal-time-units-per-second)))))
            (make-connection :socket socket
                            :stream ssl-stream
                            :last-used (time:now)
                            :host host
                            :port port
                            :tls t))
          (make-connection :socket socket
                          :stream (epsilon.net:socket-stream socket)
                          :last-used (time:now)
                          :host host
                          :port port
                          :tls nil)))))

;;; HTTP protocol implementation

(defun format-request-headers (request method path)
  "Format HTTP request headers."
  (let ((headers (request-object-headers request))
        (host (uri:host (uri:uri (request-object-url request))))
        (lines (list (format nil "~A ~A HTTP/1.1" method path))))
    
    ;; Add Host header if not present
    (unless (map:get headers "Host")
      (setf headers (map:assoc headers "Host" host)))
    
    ;; Add User-Agent if not present
    (unless (map:get headers "User-Agent")
      (setf headers (map:assoc headers "User-Agent" *default-user-agent*)))
    
    ;; Add body-related headers if needed
    (let ((body (request-object-body request)))
      (when body
        (unless (map:get headers "Content-Length")
          (setf headers (map:assoc headers "Content-Length" 
                                 (princ-to-string 
                                  (cond
                                    ((stringp body) (length body))
                                    ((vectorp body) (length body))
                                    (t 0))))))
        
        ;; Add Content-Type if not present and we have a body
        (unless (map:get headers "Content-Type")
          (setf headers (map:assoc headers "Content-Type" "application/octet-stream")))))
    
    ;; Add Connection: close by default
    (unless (map:get headers "Connection")
      (setf headers (map:assoc headers "Connection" "close")))
    
    ;; Format headers
    (loop :for (name . value) :in (map:seq headers)
          :do (push (format nil "~A: ~A" name value) lines))
    
    ;; Return formatted headers
    (str:join *crlf* (nreverse lines))))

(defun write-request (stream request)
  "Write an HTTP request to a stream."
  (let* ((url (uri:uri (request-object-url request)))
         (path (or (uri:path url) "/"))
         (query (uri:query url))
         (method (string-upcase (string (request-object-method request))))
         (full-path (if query (format nil "~A?~A" path query) path)))
    
    ;; Write request line and headers
    (let ((header-string (format-request-headers request method full-path)))
      (write-string header-string stream)
      (write-string (format nil "~A~A" *crlf* *crlf*) stream))
    
    ;; Write body if present
    (let ((body (request-object-body request)))
      (when body
        (etypecase body
          (string (write-string body stream))
          ((vector (unsigned-byte 8)) (write-sequence body stream)))))
    
    ;; Flush the stream
    (force-output stream)))

(defun read-response-status-line (stream)
  "Read the status line from an HTTP response."
  (let* ((line (read-line stream))
         (space1 (position #\Space line))
         (space2 (position #\Space line :start (1+ space1))))
    (unless (and space1 space2)
      (error "Invalid HTTP status line: ~A" line))
    
    (values
     (subseq line 0 space1)              ; Protocol
     (parse-integer (subseq line (1+ space1) space2))  ; Status code
     (subseq line (1+ space2)))))        ; Status message

(defun read-response-headers (stream)
  "Read HTTP headers from a stream."
  (let ((headers map:+empty+))
    (loop for line = (read-line stream)
          until (string= line "")
          do (let* ((colon-pos (position #\: line))
                   (name (string-trim '(#\Space #\Tab)
                                     (subseq line 0 colon-pos)))
                   (value (string-trim '(#\Space #\Tab)
                                      (subseq line (1+ colon-pos)))))
               (setf headers (map:assoc headers (string-downcase name) value))))
    headers))

(defun read-response-body (stream headers)
  "Read the response body based on Content-Length or Transfer-Encoding."
  (let ((content-length (map:get headers "content-length"))
        (transfer-encoding (map:get headers "transfer-encoding")))
    
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
         (read-sequence buffer stream)
         buffer))
      
      ;; No body indicators, read until EOF
      (t 
       (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
         (handler-case
             (loop for byte = (read-byte stream nil nil)
                  while byte
                  do (vector-push-extend byte buffer))
           (end-of-file () buffer)))))))

(defmethod round-trip ((transport default-transport) request)
  "Execute an HTTP request using the default transport."
  (let* ((url (uri:uri (request-object-url request)))
         (host (uri:host url))
         (port (or (uri:port url) 
                   (if (string-equal (uri:scheme url) "https") 443 80)))
         (tls (string-equal (uri:scheme url) "https"))
         (conn nil)
         (stream nil))
    
    (unwind-protect
         (progn
           ;; Establish connection
           (setf conn (dial transport host port tls))
           (setf stream (connection-stream conn))
           
           ;; TODO Set timeouts if supported
           ;(when (typep stream 'stream:timeout-stream)
           ;  (setf (stream:read-timeout stream) (transport-read-timeout transport))
           ;  (setf (stream:write-timeout stream) (transport-write-timeout transport)))
           
           ;; Write request
           (write-request stream request)
           
           ;; Read response
           (multiple-value-bind (proto status message)
               (read-response-status-line stream)
             (declare (ignore message))
             (let* ((headers (read-response-headers stream))
                   (body (read-response-body stream headers))
                   (content-length (if (map:get headers "content-length")
                                      (parse-integer (map:get headers "content-length"))
                                      (length body))))
               
               ;; Create response object
               (make-response-object
                :status status
                :headers headers
                :body body
                :proto proto
                :request request
                :content-length content-length))))
      
      ;; Cleanup
      (when conn
        (if (and (string-equal (map:get (request-object-headers request) "Connection" "close") "keep-alive")
                (not (string-equal (map:get (response-object-headers res) "Connection" "close") "close")))
            ;; Put connection back in the pool
            (put-idle-connection transport conn)
            ;; Close the connection
            (progn
              (close stream)
              (epsilon.net:socket-close (connection-socket conn))))))))

;;; Public interface functions

(defun request (url &key
                   (method :get)
                   (headers nil)
                   (content nil)
                   (timeout *default-timeout*)
                   (client nil)
                   (redirect t))
  "Make an HTTP request.
   
   URL is the target URL as a string.
   METHOD is the HTTP method (:GET, :POST, etc.)
   HEADERS is an alist of HTTP headers.
   CONTENT is the request body as a string or byte array.
   TIMEOUT is the request timeout in seconds.
   CLIENT is an optional client instance to use.
   REDIRECT determines whether to follow redirects.
   
   Returns a property list with :STATUS, :HEADERS, and :BODY."
  (let ((req (make-request-object
              :method method
              :url url
              :body content
              :timeout timeout)))
    
    ;; Convert headers from alist to map
    (when headers
      (let ((header-map map:+empty+))
        (dolist (header headers)
          (setf header-map (map:assoc header-map 
                                    (string-downcase (car header)) 
                                    (cdr header))))
        (setf (request-object-headers req) header-map)))
    
    ;; Create client if not provided
    (let ((created-client nil))
      (unless client
        (setf client (make-client :tls (string-equal (uri:scheme (uri:uri url)) "https")))
        (setf created-client t))
      
      (unwind-protect
           (let* ((transport (client-transport client))
                 (response (round-trip transport req))
                 (status (response-object-status response))
                 (headers (response-object-headers response))
                 (body (response-object-body response)))
             
             ;; Handle redirects if requested
             (when (and redirect
                       (client-follow-redirects client)
                       (<= 300 status 399)
                       (map:get headers "location"))
               (let ((redirect-url (map:get headers "location"))
                     (redirect-count 0))
                 (loop while (and redirect-url (<= 300 status 399) (< redirect-count +max-redirects+))
                      do (progn
                           (incf redirect-count)
                           (setf (request-object-url req) 
                                 (uri:merge (uri:uri (request-object-url req))
                                            (uri:uri redirect-url)))
                           
                           ;; Change method to GET for 301, 302, 303
                           (when (<= status 303)
                             (setf (request-object-method req) :get)
                             (setf (request-object-body req) nil))
                           
                           ;; Make the redirect request
                           (setf response (round-trip transport req))
                           (setf status (response-object-status response))
                           (setf headers (response-object-headers response))
                           (setf body (response-object-body response))
                           (setf redirect-url (map:get headers "location"))))))
             
             ;; Convert to property list for API compatibility
             (list :status status
                   :headers headers
                   :body body))
        
        ;; Cleanup
        (when created-client
          ;; Close any idle connections
          (close-idle-connections (client-transport client)))))))

;; Convenience methods for common HTTP verbs

(defun get (url &rest args)
  "Make an HTTP GET request."
  (apply #'request url :method :get args))

(defun post (url &rest args)
  "Make an HTTP POST request."
  (apply #'request url :method :post args))

(defun head (url &rest args)
  "Make an HTTP HEAD request."
  (apply #'request url :method :head args))

(defun put (url &rest args)
  "Make an HTTP PUT request."
  (apply #'request url :method :put args))

(defun patch (url &rest args)
  "Make an HTTP PATCH request."
  (apply #'request url :method :patch args))

(defun delete (url &rest args)
  "Make an HTTP DELETE request."
  (apply #'request url :method :delete args))

(defun fetch (url &rest args)
  "Alias for request function."
  (apply #'request url args))
