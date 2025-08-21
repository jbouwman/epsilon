;;;; HTTP/2 Protocol Implementation
;;;;
;;;; Main module for HTTP/2 protocol support

(defpackage :epsilon.http2
  (:use :cl)
  (:import-from :epsilon.http2.hpack
                #:create-encoder
                #:create-decoder
                #:encode-headers
                #:decode-headers)
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
   (streams :initform (make-hash-table) :accessor connection-streams :accessor http2-connection-streams)
   (next-stream-id :initform 1 :accessor connection-next-stream-id)
   (local-settings :initform +default-settings+ :accessor connection-local-settings :accessor http2-connection-local-settings)
   (remote-settings :initform +default-settings+ :accessor connection-remote-settings)
   (hpack-encoder :accessor connection-hpack-encoder)
   (hpack-decoder :accessor connection-hpack-decoder)
   (send-window :initform 65535 :accessor connection-send-window)
   (recv-window :initform 65535 :accessor connection-recv-window)
   (flow-controller :accessor http2-connection-flow-controller :initform nil)
   (last-stream-id :initform 0 :accessor http2-connection-last-stream-id)))

(defun make-http2-connection (socket &key tls-connection client-p)
  "Create a new HTTP/2 connection"
  (let ((conn (make-instance 'http2-connection
                             :socket socket
                             :tls-connection tls-connection
                             :client-p client-p)))
    ;; Initialize HPACK encoder/decoder
    (setf (connection-hpack-encoder conn) (create-encoder))
    (setf (connection-hpack-decoder conn) (create-decoder))
    
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
      (epsilon.net:tcp-write (connection-socket connection) preface-bytes))))

(defun send-settings-frame (connection settings)
  "Send SETTINGS frame"
  (let ((frame (make-settings-frame settings)))
    (connection-send-frame connection frame)))

(defun connection-send-frame (connection frame)
  "Send a frame over the connection"
  (let ((frame-bytes (serialize-frame frame)))
    (if (connection-tls-connection connection)
        (epsilon.crypto:tls-write (connection-tls-connection connection) frame-bytes)
      (epsilon.net:tcp-write (connection-socket connection) frame-bytes))))

(defun connection-receive-frame (connection)
  "Receive and parse a frame from the connection"
  (let ((header-bytes (make-array 9 :element-type '(unsigned-byte 8))))
    ;; Read frame header (9 bytes)
    (if (connection-tls-connection connection)
        (epsilon.crypto:tls-read (connection-tls-connection connection) header-bytes :start 0 :end 9)
      (epsilon.net:tcp-read (connection-socket connection) header-bytes :start 0 :end 9))
    
    ;; Parse header to get payload length
    (let* ((payload-length (parse-frame-header header-bytes))
           (payload-bytes (when (> payload-length 0)
                           (make-array payload-length :element-type '(unsigned-byte 8)))))
      
      ;; Read payload if present
      (when payload-bytes
        (if (connection-tls-connection connection)
            (epsilon.crypto:tls-read (connection-tls-connection connection) payload-bytes :start 0 :end payload-length)
          (epsilon.net:tcp-read (connection-socket connection) payload-bytes :start 0 :end payload-length)))
      
      ;; Parse complete frame
      (parse-frame header-bytes payload-bytes))))

(defun connection-close (connection)
  "Close HTTP/2 connection"
  ;; Send GOAWAY frame
  (let ((goaway-frame (make-goaway-frame 
                       (1- (connection-next-stream-id connection))
                       +error-no-error+
                       "Connection closing")))
    (connection-send-frame connection goaway-frame))
  
  ;; Close underlying connection
  (when (connection-tls-connection connection)
    (epsilon.crypto:tls-close (connection-tls-connection connection)))
  (when (connection-socket connection)
    (epsilon.net:tcp-shutdown (connection-socket connection) :how :both)))

;;;; Stream Management

(defun create-stream (connection)
  "Create a new stream on the connection"
  (let* ((stream-id (connection-next-stream-id connection))
         (stream (make-http2-stream stream-id connection)))
    ;; Increment next stream ID (odd for client, even for server)
    (incf (connection-next-stream-id connection) 2)
    ;; Store stream
    (setf (gethash stream-id (connection-streams connection)) stream)
    stream))

(defun stream-send-headers (stream headers &key end-stream)
  "Send headers on a stream"
  (let* ((connection (stream-connection stream))
         (encoded-headers (encode-headers 
                          (connection-hpack-encoder connection)
                          headers))
         (frame (make-headers-frame 
                (stream-id stream)
                encoded-headers
                :end-stream end-stream
                :end-headers t)))
    (connection-send-frame connection frame)))

(defun stream-send-data (stream data &key end-stream)
  "Send data on a stream"
  (let* ((connection (stream-connection stream))
         (frame (make-data-frame 
                (stream-id stream)
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
          ((eq (frame-type frame) +frame-headers+)
           (when (= (frame-stream-id frame) (stream-id stream))
             (setf response-headers 
                   (decode-headers 
                    (connection-hpack-decoder connection)
                    (frame-payload frame)))
             (when (frame-flag-set-p frame +flag-end-stream+)
               (return))))
          
          ;; DATA frame
          ((eq (frame-type frame) +frame-data+)
           (when (= (frame-stream-id frame) (stream-id stream))
             (setf response-data 
                   (append response-data (coerce (frame-payload frame) 'list)))
             (when (frame-flag-set-p frame +flag-end-stream+)
               (return))))
          
          ;; Other frames - handle as needed
          (t nil))))
    
    (list :headers response-headers
          :body (when response-data
                  (bytes-to-string (coerce response-data 'vector))))))

;;;; Stream Management

(defstruct http2-stream
  "HTTP/2 stream"
  id
  connection
  state  ; :idle :open :half-closed-remote :half-closed-local :closed
  headers
  data
  (flow-controller nil))

(defun make-http2-stream (id connection)
  "Create a new HTTP/2 stream"
  (make-instance 'http2-stream
                 :id id
                 :connection connection
                 :state :idle))

(defun stream-connection (stream)
  "Get connection from stream"
  (http2-stream-connection stream))

(defun stream-id (stream)
  "Get stream ID"
  (http2-stream-id stream))


;;;; Frame Helper Functions

(defun serialize-frame (frame)
  "Serialize frame to bytes"
  ;; Simple serialization - just return the frame object for now
  frame)

(defun parse-frame-header (header-bytes)
  "Parse frame header and return length"
  (logior (ash (aref header-bytes 0) 16)
          (ash (aref header-bytes 1) 8)
          (aref header-bytes 2)))

(defun parse-frame (header-bytes payload-bytes)
  "Parse complete frame from header and payload"
  (declare (ignore header-bytes payload-bytes))
  ;; Simplified for now
  nil)

(defun frame-type (frame)
  "Get frame type"
  (http2-frame-type frame))

(defun frame-stream-id (frame)
  "Get frame stream ID"
  (http2-frame-stream-id frame))

(defun frame-payload (frame)
  "Get frame payload"
  (http2-frame-payload frame))

(defun frame-flag-set-p (frame flag)
  "Check if flag is set in frame"
  (logtest (http2-frame-flags frame) flag))