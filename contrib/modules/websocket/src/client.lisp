;;;; WebSocket Client Implementation
;;;;
;;;; This module provides a WebSocket client that can connect to WebSocket servers.

(defpackage epsilon.websocket.client
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (net epsilon.net)
   (url epsilon.url)
   (str epsilon.string)
   (map epsilon.map)
   (thread epsilon.sys.thread)
   (http epsilon.http.client)
   (req epsilon.http.request)
   (resp epsilon.http.response)
   (handshake epsilon.websocket.handshake)
   (conn epsilon.websocket.connection))
  (:export
   ;; Client connection
   connect
   connect-async
   with-websocket-connection
   
   ;; Client options
   websocket-client-options
   make-client-options
   
   ;; Client utilities
   parse-websocket-uri
   send-text-message
   send-binary-message
   echo-client))

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
  (let ((uri (url:parse-url uri-string)))
    (unless (member (url:url-scheme uri) '("ws" "wss") :test #'string=)
      (error "Invalid WebSocket URI scheme: ~A" (url:url-scheme uri)))
    
    (let ((host (url:url-host uri))
          (port (or (url:url-port uri)
                    (if (string= (url:url-scheme uri) "wss") 443 80)))
          (path (or (url:url-path uri) "/"))
          (secure-p (string= (url:url-scheme uri) "wss")))
      
      (values host port path secure-p))))

;;; Connection establishment

(defun connect (uri-string &key (options (make-client-options)))
  "Connect to WebSocket server synchronously"
  (multiple-value-bind (host port path secure-p)
      (parse-websocket-uri uri-string)
    
    ;; Establish TCP connection  
    (let ((socket (net:tcp-connect (format nil "~A:~A" host port))))
      (unwind-protect
           (let ((stream (sb-sys:make-fd-stream (net:tcp-stream-handle socket)
                                               :input t :output t
                                               :element-type 'character
                                               :buffering :line)))
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
          (net:tcp-shutdown socket))))))

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
      
      ;; Get existing headers from request
      (let ((headers (req:request-headers request)))
        
        ;; Add additional headers from options
        (dolist (header (websocket-client-options-headers options))
          (setf headers (map:assoc headers (car header) (cdr header))))
        
        ;; Build HTTP request string manually
        (let ((request-string 
                (format nil "GET ~A HTTP/1.1~C~C" 
                        (req:request-path request) #\Return #\Newline)))
          
          ;; Add headers
          (map:each (lambda (name value)
                      (setf request-string 
                            (concatenate 'string request-string
                                         (format nil "~A: ~A~C~C" name value #\Return #\Newline))))
                    headers)
          
          ;; Add final CRLF
          (setf request-string 
                (concatenate 'string request-string (format nil "~C~C" #\Return #\Newline)))
          
          ;; Send HTTP request
          (write-string request-string stream)
          (force-output stream)
          
          ;; Read HTTP response - simplified response parsing
          (let ((response-line (read-line stream)))
            (unless (search "101" response-line)
              (error "WebSocket upgrade failed: ~A" response-line))
            
            ;; Read headers until empty line
            (let ((response-headers map:+empty+))
              (loop for line = (read-line stream)
                    while (and line (> (length line) 0) (not (string= line (string #\Return))))
                    do (let ((colon-pos (position #\: line)))
                         (when colon-pos
                           (let ((header-name (string-downcase (str:trim (subseq line 0 colon-pos))))
                                 (header-value (str:trim (subseq line (1+ colon-pos)))))
                             (setf response-headers (map:assoc response-headers header-name header-value))))))
              
              ;; Create mock response for handshake validation
              (let ((response (resp:make-response :status 101 :headers response-headers)))
                ;; Validate handshake response
                (handshake:validate-handshake-response response websocket-key)
                
                ;; Extract negotiated protocols
                (let ((subprotocol (map:get response-headers "sec-websocket-protocol"))
                      (extensions (map:get response-headers "sec-websocket-extensions")))
                  
                  (values subprotocol extensions))))))))))

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
