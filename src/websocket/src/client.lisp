;;;; WebSocket Client Implementation
;;;;
;;;; This module provides a WebSocket client that can connect to WebSocket servers.

(defpackage epsilon.websocket.client
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (net epsilon.net)
   (uri epsilon.uri)
   (str epsilon.string)
   (http epsilon.http.client)
   (handshake epsilon.websocket.handshake)
   (conn epsilon.websocket.connection))
  (:export
   ;; Client connection
   connect
   connect-async
   
   ;; Client options
   websocket-client-options
   make-client-options
   
   ;; Client utilities
   parse-websocket-uri))

(in-package epsilon.websocket.client)

;;; Client options

(defstruct websocket-client-options
  "WebSocket client connection options"
  (subprotocols nil :type list)              ; Requested subprotocols
  (extensions nil :type (or null string))    ; Requested extensions
  (headers nil :type list)                   ; Additional HTTP headers
  (timeout 30 :type number)                 ; Connection timeout in seconds
  (ping-interval nil :type (or null number)) ; Ping interval in seconds
  (ping-timeout 5 :type number)             ; Ping response timeout
  
  ;; Event callbacks
  (on-open nil :type (or null function))     ; Connection opened
  (on-message nil :type (or null function))  ; Message received
  (on-close nil :type (or null function))    ; Connection closed
  (on-error nil :type (or null function))    ; Error occurred
  (on-ping nil :type (or null function))     ; Ping received
  (on-pong nil :type (or null function)))    ; Pong received

(defun make-client-options (&rest args)
  "Create client options structure"
  (apply #'make-websocket-client-options args))

;;; URI parsing

(defun parse-websocket-uri (uri-string)
  "Parse WebSocket URI and return (values host port path secure-p)"
  (let ((uri (uri:parse-uri uri-string)))
    (unless (member (uri:uri-scheme uri) '("ws" "wss") :test #'string=)
      (error "Invalid WebSocket URI scheme: ~A" (uri:uri-scheme uri)))
    
    (let ((host (uri:uri-host uri))
          (port (or (uri:uri-port uri)
                    (if (string= (uri:uri-scheme uri) "wss") 443 80)))
          (path (or (uri:uri-path uri) "/"))
          (secure-p (string= (uri:uri-scheme uri) "wss")))
      
      (values host port path secure-p))))

;;; Connection establishment

(defun connect (uri-string &key (options (make-client-options)))
  "Connect to WebSocket server synchronously"
  (multiple-value-bind (host port path secure-p)
      (parse-websocket-uri uri-string)
    
    ;; Establish TCP connection
    (let ((socket (net:socket-connect host port :timeout (websocket-client-options-timeout options))))
      (unwind-protect
           (let ((stream (net:socket-stream socket :binary t)))
             ;; Perform WebSocket handshake
             (perform-handshake stream host path options)
             
             ;; Create WebSocket connection
             (let ((connection (conn:make-connection 
                               stream
                               :client-side t
                               :on-open (websocket-client-options-on-open options)
                               :on-message (websocket-client-options-on-message options)
                               :on-close (websocket-client-options-on-close options)
                               :on-error (websocket-client-options-on-error options)
                               :on-ping (websocket-client-options-on-ping options)
                               :on-pong (websocket-client-options-on-pong options))))
               
               ;; Start ping timer if configured
               (when (websocket-client-options-ping-interval options)
                 (start-ping-timer connection (websocket-client-options-ping-interval options)))
               
               ;; Trigger open event
               (when (websocket-client-options-on-open options)
                 (funcall (websocket-client-options-on-open options) connection))
               
               connection))
        
        ;; Cleanup on error
        (when socket
          (net:socket-close socket))))))

(defun perform-handshake (stream host path options)
  "Perform WebSocket handshake over stream"
  (let ((uri (format nil "~A" path)))
    ;; Create upgrade request
    (multiple-value-bind (request websocket-key)
        (handshake:create-upgrade-request 
         uri
         :host host
         :subprotocols (websocket-client-options-subprotocols options)
         :extensions (websocket-client-options-extensions options))
      
      ;; Add additional headers
      (dolist (header (websocket-client-options-headers options))
        (http:add-header request (car header) (cdr header)))
      
      ;; Send HTTP request
      (let ((request-data (http:serialize-request request)))
        (write-sequence request-data stream)
        (force-output stream))
      
      ;; Read HTTP response
      (let ((response (http:parse-response stream)))
        ;; Validate handshake response
        (handshake:validate-handshake-response response websocket-key)
        
        ;; Extract negotiated protocols
        (let ((subprotocol (http:get-header (http:response-headers response) 
                                           "sec-websocket-protocol"))
              (extensions (http:get-header (http:response-headers response)
                                          "sec-websocket-extensions")))
          (values subprotocol extensions))))))

;;; Asynchronous connection

(defun connect-async (uri-string &key (options (make-client-options)) callback)
  "Connect to WebSocket server asynchronously"
  (thread:make-thread
   (lambda ()
     (handler-case
         (let ((connection (connect uri-string :options options)))
           (when callback
             (funcall callback connection nil)))
       (error (e)
         (when callback
           (funcall callback nil e)))))
   :name (format nil "websocket-client-~A" (get-universal-time))))

;;; Ping management

(defun start-ping-timer (connection interval)
  "Start automatic ping timer for connection"
  (thread:make-thread
   (lambda ()
     (loop while (conn:connection-open-p connection)
           do (sleep interval)
              (when (conn:connection-open-p connection)
                (handler-case
                    (conn:send-ping connection)
                  (error (e)
                    (when (conn:websocket-connection-on-error connection)
                      (funcall (conn:websocket-connection-on-error connection)
                               connection e)))))))
   :name (format nil "websocket-ping-~A" (get-universal-time))))

;;; Connection utilities

(defun connect-with-retry (uri-string &key (options (make-client-options)) 
                                          (max-retries 3) (retry-delay 1))
  "Connect to WebSocket server with automatic retry"
  (let ((retries 0))
    (loop
      (handler-case
          (return (connect uri-string :options options))
        (error (e)
          (incf retries)
          (if (>= retries max-retries)
              (error "Failed to connect after ~D retries: ~A" max-retries e)
              (progn
                (when (websocket-client-options-on-error options)
                  (funcall (websocket-client-options-on-error options) nil e))
                (sleep retry-delay))))))))

;;; High-level client interface

(defmacro with-websocket-connection ((connection uri-string &rest options) &body body)
  "Execute body with established WebSocket connection"
  `(let ((,connection (connect ,uri-string ,@options)))
     (unwind-protect
          (progn ,@body)
       (when (conn:connection-open-p ,connection)
         (conn:close-connection ,connection)
         (conn:wait-for-close ,connection)))))

;;; Simple client functions

(defun send-text-message (uri-string message &key (options (make-client-options)))
  "Send a single text message to WebSocket server"
  (with-websocket-connection (conn uri-string :options options)
    (conn:send-text conn message)))

(defun send-binary-message (uri-string data &key (options (make-client-options)))
  "Send a single binary message to WebSocket server"
  (with-websocket-connection (conn uri-string :options options)
    (conn:send-binary conn data)))

;;; Example client

(defun echo-client (uri-string message &key (options (make-client-options)))
  "Simple echo client example"
  (let ((received-message nil))
    ;; Set up message handler
    (setf (websocket-client-options-on-message options)
          (lambda (connection message is-text)
            (declare (ignore connection))
            (setf received-message message)
            (format t "Received ~A message: ~A~%" 
                    (if is-text "text" "binary") message)))
    
    ;; Connect and send message
    (with-websocket-connection (conn uri-string :options options)
      (conn:send-text conn message)
      
      ;; Wait for response
      (loop for i from 0 below 100
            when received-message
              return received-message
            do (sleep 0.1))
      
      received-message)))
