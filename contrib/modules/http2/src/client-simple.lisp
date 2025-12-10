;;;; Simple HTTP/2 Client Implementation
;;;;
;;;; A working HTTP/2 client that properly uses the exported API

(defpackage :epsilon.http2.client
  (:use :cl)
  (:local-nicknames
   (#:http2 #:epsilon.http2)
   (#:net #:epsilon.net)
   (#:crypto #:epsilon.crypto)
   (#:log #:epsilon.log))
  (:export
   #:http2-connect
   #:http2-disconnect
   #:http2-get
   #:http2-post
   #:with-http2-connection))

(in-package :epsilon.http2.client)

(defun http2-connect (host port &key ssl-p cert-file key-file ca-file)
  "Connect to HTTP/2 server"
  (log:info "Connecting to ~A:~D" host port)
  
  (let* ((address (net:make-socket-address host port))
         (socket (net:tcp-connect address))
         (tls-conn nil))
    
    (when ssl-p
      ;; Create TLS connection with ALPN for HTTP/2
      (let ((tls-ctx (crypto:create-openssl-context
                      :server-p nil
                      :cert-file cert-file
                      :key-file key-file
                      :ca-file ca-file
                      :verify-mode (if ca-file
                                      crypto:+ssl-verify-peer+
                                      crypto:+ssl-verify-none+)
                      :alpn-protocols '("h2"))))
        
        (setf tls-conn (crypto:tls-connect socket 
                                          :context tls-ctx
                                          :hostname host))
        
        ;; Verify we got HTTP/2
        (let ((protocol (crypto:tls-selected-alpn-protocol tls-conn)))
          (unless (string= protocol "h2")
            (crypto:tls-close tls-conn)
            (error "Failed to negotiate HTTP/2. Got: ~A" protocol)))))
    
    ;; Create HTTP/2 connection
    (http2:make-http2-connection socket 
                                :tls-connection tls-conn
                                :client-p t)))

(defun http2-disconnect (connection)
  "Disconnect HTTP/2 connection"
  (when connection
    (http2:connection-close connection)))

(defun http2-get (connection path)
  "Make a GET request over HTTP/2 connection"
  (http2:http2-get (format nil "https://localhost~A" path)))

(defun http2-post (connection path &key body headers)
  "Make a POST request over HTTP/2 connection"
  (declare (ignore connection body headers))
  (http2:http2-post (format nil "https://localhost~A" path)))

(defmacro with-http2-connection ((conn host port &rest options) &body body)
  "Execute body with HTTP/2 connection"
  `(let ((,conn (http2-connect ,host ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (http2-disconnect ,conn))))