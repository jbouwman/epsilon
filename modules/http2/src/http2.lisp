;;;; HTTP/2 Protocol Implementation
;;;;
;;;; Main module for HTTP/2 protocol support

(defpackage :epsilon.http2
  (:use :cl)
  (:local-nicknames
   (#:frames #:epsilon.http2.frames)
   (#:hpack #:epsilon.http2.hpack)
   (#:stream #:epsilon.http2.stream)
   (#:connection #:epsilon.http2.connection))
  (:export
   ;; Connection management
   #:make-http2-connection
   #:http2-connection-p
   #:connection-send-frame
   #:connection-receive-frame
   #:connection-close
   
   ;; Stream management
   #:create-stream
   #:stream-send-headers
   #:stream-send-data
   #:stream-receive-headers
   #:stream-receive-data
   #:stream-close
   
   ;; Client functions
   #:http2-request
   #:http2-get
   #:http2-post
   
   ;; Server functions
   #:http2-server
   #:handle-http2-connection
   
   ;; Protocol negotiation
   #:upgrade-to-http2
   #:is-http2-connection
   
   ;; Settings
   #:make-settings
   #:apply-settings
   #:+default-settings+))

(in-package :epsilon.http2)

;;;; Constants

(defparameter +http2-preface+ 
  "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
  "HTTP/2 connection preface")

(defparameter +default-settings+
  '((:header-table-size . 4096)
    (:enable-push . 1)
    (:max-concurrent-streams . 100)
    (:initial-window-size . 65535)
    (:max-frame-size . 16384)
    (:max-header-list-size . 8192))
  "Default HTTP/2 settings")

;;;; Connection Management

(defclass http2-connection ()
  ((socket :initarg :socket :accessor connection-socket)
   (tls-connection :initarg :tls-connection :accessor connection-tls-connection)
   (client-p :initarg :client-p :accessor connection-client-p :initform nil)
   (streams :initform (make-hash-table) :accessor connection-streams)
   (next-stream-id :initform 1 :accessor connection-next-stream-id)
   (local-settings :initform +default-settings+ :accessor connection-local-settings)
   (remote-settings :initform +default-settings+ :accessor connection-remote-settings)
   (hpack-encoder :accessor connection-hpack-encoder)
   (hpack-decoder :accessor connection-hpack-decoder)
   (send-window :initform 65535 :accessor connection-send-window)
   (recv-window :initform 65535 :accessor connection-recv-window)))

(defun make-http2-connection (socket &key tls-connection client-p)
  "Create a new HTTP/2 connection"
  (let ((conn (make-instance 'http2-connection
                             :socket socket
                             :tls-connection tls-connection
                             :client-p client-p)))
    ;; Initialize HPACK encoder/decoder
    (setf (connection-hpack-encoder conn) (hpack:make-encoder))
    (setf (connection-hpack-decoder conn) (hpack:make-decoder))
    
    ;; Send connection preface if client
    (when client-p
      (send-connection-preface conn))
    
    ;; Send initial SETTINGS frame
    (send-settings-frame conn +default-settings+)
    
    conn))

(defun send-connection-preface (connection)
  "Send HTTP/2 connection preface (client only)"
  (let ((preface-bytes (string-to-bytes +http2-preface+)))
    (if (connection-tls-connection connection)
        (epsilon.crypto:tls-write (connection-tls-connection connection) preface-bytes)
      (epsilon.net:tcp-send (connection-socket connection) preface-bytes))))

(defun send-settings-frame (connection settings)
  "Send SETTINGS frame"
  (let ((frame (frames:make-settings-frame settings)))
    (connection-send-frame connection frame)))

(defun connection-send-frame (connection frame)
  "Send a frame over the connection"
  (let ((frame-bytes (frames:serialize-frame frame)))
    (if (connection-tls-connection connection)
        (epsilon.crypto:tls-write (connection-tls-connection connection) frame-bytes)
      (epsilon.net:tcp-send (connection-socket connection) frame-bytes))))

(defun connection-receive-frame (connection)
  "Receive and parse a frame from the connection"
  (let ((header-bytes (make-array 9 :element-type '(unsigned-byte 8))))
    ;; Read frame header (9 bytes)
    (if (connection-tls-connection connection)
        (epsilon.crypto:tls-read (connection-tls-connection connection) header-bytes 0 9)
      (epsilon.net:tcp-receive (connection-socket connection) header-bytes 0 9))
    
    ;; Parse header to get payload length
    (let* ((payload-length (frames:parse-frame-header header-bytes))
           (payload-bytes (when (> payload-length 0)
                           (make-array payload-length :element-type '(unsigned-byte 8)))))
      
      ;; Read payload if present
      (when payload-bytes
        (if (connection-tls-connection connection)
            (epsilon.crypto:tls-read (connection-tls-connection connection) payload-bytes 0 payload-length)
          (epsilon.net:tcp-receive (connection-socket connection) payload-bytes 0 payload-length)))
      
      ;; Parse complete frame
      (frames:parse-frame header-bytes payload-bytes))))

(defun connection-close (connection)
  "Close HTTP/2 connection"
  ;; Send GOAWAY frame
  (let ((goaway-frame (frames:make-goaway-frame 
                       (1- (connection-next-stream-id connection))
                       frames:+no-error+
                       "Connection closing")))
    (connection-send-frame connection goaway-frame))
  
  ;; Close underlying connection
  (when (connection-tls-connection connection)
    (epsilon.crypto:tls-close (connection-tls-connection connection)))
  (when (connection-socket connection)
    (epsilon.net:tcp-close (connection-socket connection))))

;;;; Stream Management

(defun create-stream (connection)
  "Create a new stream on the connection"
  (let* ((stream-id (connection-next-stream-id connection))
         (stream (stream:make-http2-stream stream-id connection)))
    ;; Increment next stream ID (odd for client, even for server)
    (incf (connection-next-stream-id connection) 2)
    ;; Store stream
    (setf (gethash stream-id (connection-streams connection)) stream)
    stream))

(defun stream-send-headers (stream headers &key end-stream)
  "Send headers on a stream"
  (let* ((connection (stream:stream-connection stream))
         (encoded-headers (hpack:encode-headers 
                          (connection-hpack-encoder connection)
                          headers))
         (frame (frames:make-headers-frame 
                (stream:stream-id stream)
                encoded-headers
                :end-stream end-stream
                :end-headers t)))
    (connection-send-frame connection frame)))

(defun stream-send-data (stream data &key end-stream)
  "Send data on a stream"
  (let* ((connection (stream:stream-connection stream))
         (frame (frames:make-data-frame 
                (stream:stream-id stream)
                data
                :end-stream end-stream)))
    (connection-send-frame connection frame)))

;;;; Client Functions

(defun http2-request (url &key method headers body)
  "Make an HTTP/2 request"
  ;; Parse URL
  (multiple-value-bind (scheme host port path query)
      (epsilon.http.client::parse-url url)
    (declare (ignore scheme query))
    
    ;; Create TLS connection with ALPN
    (let* ((socket (epsilon.net:tcp-connect (epsilon.net:make-socket-address host port)))
           (tls-ctx (epsilon.crypto:create-openssl-context 
                    :server-p nil
                    :alpn-protocols '("h2")))
           (tls-conn (epsilon.crypto:openssl-connect socket tls-ctx 
                                                     :hostname host
                                                     :alpn-protocols '("h2")))
           (h2-conn (make-http2-connection socket 
                                           :tls-connection tls-conn
                                           :client-p t)))
      
      ;; Verify we negotiated HTTP/2
      (let ((protocol (epsilon.crypto:tls-selected-alpn-protocol tls-conn)))
        (unless (string= protocol "h2")
          (error "Failed to negotiate HTTP/2, got: ~A" protocol)))
      
      ;; Create stream and send request
      (let ((stream (create-stream h2-conn)))
        ;; Prepare headers
        (let ((request-headers (list (cons ":method" (or method "GET"))
                                    (cons ":scheme" "https")
                                    (cons ":authority" host)
                                    (cons ":path" path))))
          ;; Add custom headers
          (when headers
            (setf request-headers (append request-headers headers)))
          
          ;; Send headers
          (stream-send-headers stream request-headers :end-stream (null body))
          
          ;; Send body if present
          (when body
            (stream-send-data stream body :end-stream t))
          
          ;; Read response
          (read-http2-response h2-conn stream))))))

(defun http2-get (url &key headers)
  "Make an HTTP/2 GET request"
  (http2-request url :method "GET" :headers headers))

(defun http2-post (url &key headers body)
  "Make an HTTP/2 POST request"
  (http2-request url :method "POST" :headers headers :body body))

;;;; Helper Functions

(defun string-to-bytes (string)
  "Convert string to byte array"
  (map 'vector #'char-code string))

(defun bytes-to-string (bytes)
  "Convert byte array to string"
  (map 'string #'code-char bytes))

(defun read-http2-response (connection stream)
  "Read HTTP/2 response from stream"
  ;; This is simplified - real implementation would handle multiplexing
  (let ((response-headers nil)
        (response-data nil))
    
    (loop
      (let ((frame (connection-receive-frame connection)))
        (cond
          ;; HEADERS frame
          ((eq (frames:frame-type frame) frames:+frame-type-headers+)
           (when (= (frames:frame-stream-id frame) (stream:stream-id stream))
             (setf response-headers 
                   (hpack:decode-headers 
                    (connection-hpack-decoder connection)
                    (frames:frame-payload frame)))
             (when (frames:frame-flag-set-p frame frames:+flag-end-stream+)
               (return))))
          
          ;; DATA frame
          ((eq (frames:frame-type frame) frames:+frame-type-data+)
           (when (= (frames:frame-stream-id frame) (stream:stream-id stream))
             (setf response-data 
                   (append response-data (coerce (frames:frame-payload frame) 'list)))
             (when (frames:frame-flag-set-p frame frames:+flag-end-stream+)
               (return))))
          
          ;; Other frames - handle as needed
          (t nil))))
    
    (list :headers response-headers
          :body (when response-data
                  (bytes-to-string (coerce response-data 'vector))))))