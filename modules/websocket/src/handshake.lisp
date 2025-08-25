;;;; WebSocket Handshake Implementation (RFC 6455)
;;;;
;;;; This module implements the WebSocket opening handshake protocol.
;;;; The handshake process upgrades an HTTP connection to a WebSocket connection.

(defpackage epsilon.websocket.handshake
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (str epsilon.string)
   (seq epsilon.sequence)
   (digest epsilon.digest)
   (base64 epsilon.base64)
   (uuid epsilon.uuid)
   (http epsilon.http.request)
   (resp epsilon.http.response)
   (map epsilon.map)
   (url epsilon.url))
  (:export
   ;; Constants
   +websocket-magic-string+
   
   ;; Handshake validation
   validate-websocket-request
   generate-accept-key
   create-handshake-response
   
   ;; Client handshake
   generate-websocket-key
   create-upgrade-request
   validate-handshake-response
   
   ;; Protocol negotiation
   negotiate-subprotocol
   negotiate-extensions
   parse-extensions
   
   ;; Utility functions
   websocket-request-p
   extract-websocket-key
   extract-subprotocols
   extract-extensions
   
   ;; Test helper functions
   generate-handshake-headers
   valid-handshake-response-p
   
   ;; Errors
   websocket-handshake-error))

(in-package epsilon.websocket.handshake)

;;; Constants

(define-constant +websocket-magic-string+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "WebSocket GUID as defined in RFC 6455")

;;; Conditions

(define-condition websocket-handshake-error (error)
  ((message :initarg :message :reader handshake-error-message))
  (:report (lambda (condition stream)
             (format stream "WebSocket handshake error: ~A"
                     (handshake-error-message condition)))))

;;; Key generation and validation

(defun generate-websocket-key ()
  "Generate a random 16-byte key encoded as base64"
  (let ((random-bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref random-bytes i) (random 256)))
    (base64:octets-to-base64 random-bytes)))

(defun generate-accept-key (websocket-key)
  "Generate Sec-WebSocket-Accept value from Sec-WebSocket-Key"
  (let* ((concatenated (str:concat websocket-key +websocket-magic-string+))
         (sha1-hash (digest:sha1-digest (str:string-to-octets concatenated))))
    (base64:octets-to-base64 sha1-hash)))

;;; Request validation

(defun validate-websocket-request (request)
  "Validate that HTTP request is a valid WebSocket upgrade request"
  (let ((method (http:request-method request))
        (headers (http:request-headers request)))
    
    ;; Must be GET request
    (unless (string= method "GET")
      (error 'websocket-handshake-error
             :message (format nil "Invalid method: ~A (expected GET)" method)))
    
    ;; Must have Upgrade: websocket
    (let ((upgrade (or (map:get headers "upgrade")
                       (map:get headers "Upgrade")
                       (map:get headers "UPGRADE"))))
      (unless (and upgrade (string-equal upgrade "websocket"))
        (error 'websocket-handshake-error
               :message "Missing or invalid Upgrade header")))
    
    ;; Must have Connection: Upgrade
    (let ((connection (or (map:get headers "connection")
                          (map:get headers "Connection")
                          (map:get headers "CONNECTION"))))
      (unless (and connection 
                   (find "upgrade" (seq:realize (str:split #\, connection))
                         :test #'string-equal :key #'str:trim))
        (error 'websocket-handshake-error
               :message "Missing or invalid Connection header")))
    
    ;; Must have Sec-WebSocket-Version: 13
    (let ((version (or (map:get headers "sec-websocket-version")
                       (map:get headers "Sec-WebSocket-Version")
                       (map:get headers "SEC-WEBSOCKET-VERSION"))))
      (unless (string= version "13")
        (error 'websocket-handshake-error
               :message (format nil "Unsupported WebSocket version: ~A" version))))
    
    ;; Must have Sec-WebSocket-Key
    (let ((key (or (map:get headers "sec-websocket-key")
                   (map:get headers "Sec-WebSocket-Key"))))
      (unless (and key (= (length (base64:base64-to-octets key)) 16))
        (error 'websocket-handshake-error
               :message "Missing or invalid Sec-WebSocket-Key")))
    
    t))

;;; Response generation

(defun create-handshake-response (request &key subprotocol extensions)
  "Create WebSocket handshake response for valid upgrade request"
  (validate-websocket-request request)
  
  (let* ((headers (http:request-headers request))
         (websocket-key (or (map:get headers "sec-websocket-key")
                            (map:get headers "Sec-WebSocket-Key")))
         (accept-key (generate-accept-key websocket-key))
         (response-headers (map:from-pairs (list (cons "Upgrade" "websocket")
                                                 (cons "Connection" "Upgrade")
                                                 (cons "Sec-WebSocket-Accept" accept-key)))))
    
    ;; Add subprotocol if negotiated
    (when subprotocol
      (setf response-headers (map:assoc response-headers "Sec-WebSocket-Protocol" subprotocol)))
    
    ;; Add extensions if negotiated
    (when extensions
      (setf response-headers (map:assoc response-headers "Sec-WebSocket-Extensions" extensions)))
    
    (resp:make-response :status 101
                       :headers response-headers)))

;;; Client-side handshake

(defun create-upgrade-request (uri &key subprotocols extensions host)
  "Create HTTP upgrade request to establish WebSocket connection"
  (let* ((websocket-key (generate-websocket-key))
         (headers (map:from-pairs (list (cons "Host" (or host (url:url-host (url:parse-url uri))))
                                       (cons "Upgrade" "websocket")
                                       (cons "Connection" "Upgrade")
                                       (cons "Sec-WebSocket-Key" websocket-key)
                                       (cons "Sec-WebSocket-Version" "13")))))
    
    ;; Add subprotocols if specified
    (when subprotocols
      (setf headers (map:assoc headers "Sec-WebSocket-Protocol" 
                               (if (listp subprotocols)
                                   (str:join #\, (seq:seq subprotocols))
                                   subprotocols))))
    
    ;; Add extensions if specified
    (when extensions
      (setf headers (map:assoc headers "Sec-WebSocket-Extensions" extensions)))
    
    (values 
     (http:make-request "GET"
                       (url:url-path (url:parse-url uri))
                       :headers headers)
     websocket-key)))

(defun validate-handshake-response (response expected-key)
  "Validate server's handshake response"
  (let ((status (resp:response-status response))
        (headers (resp:response-headers response)))
    
    ;; Must be 101 Switching Protocols
    (unless (= status 101)
      (error 'websocket-handshake-error
             :message (format nil "Invalid status code: ~D" status)))
    
    ;; Must have Upgrade: websocket
    (let ((upgrade (or (map:get headers "upgrade")
                       (map:get headers "Upgrade")
                       (map:get headers "UPGRADE"))))
      (unless (and upgrade (string-equal upgrade "websocket"))
        (error 'websocket-handshake-error
               :message "Missing or invalid Upgrade header")))
    
    ;; Must have Connection: Upgrade
    (let ((connection (or (map:get headers "connection")
                          (map:get headers "Connection"))))
      (unless (and connection
                   (find "upgrade" (seq:realize (str:split #\, connection))
                         :test #'string-equal :key #'str:trim))
        (error 'websocket-handshake-error
               :message "Missing or invalid Connection header")))
    
    ;; Must have correct Sec-WebSocket-Accept
    (let ((accept (or (map:get headers "sec-websocket-accept")
                      (map:get headers "Sec-WebSocket-Accept")))
          (expected-accept (generate-accept-key expected-key)))
      (unless (string= accept expected-accept)
        (error 'websocket-handshake-error
               :message "Invalid Sec-WebSocket-Accept header")))
    
    t))

;;; Protocol negotiation

(defun negotiate-subprotocol (client-protocols server-protocols)
  "Negotiate subprotocol between client and server preferences"
  (when (and client-protocols server-protocols)
    (let ((client-list (if (listp client-protocols)
                           client-protocols
                           (mapcar #'str:trim (seq:realize (str:split #\, client-protocols)))))
          (server-list (if (listp server-protocols)
                           server-protocols
                           (mapcar #'str:trim (seq:realize (str:split #\, server-protocols))))))
      ;; Return first client protocol that server supports
      (find-if (lambda (protocol)
                 (member protocol server-list :test #'string=))
               client-list))))

(defun parse-extensions (extension-header)
  "Parse Sec-WebSocket-Extensions header into list of extensions"
  (when extension-header
    (mapcar (lambda (ext)
              (let ((parts (seq:realize (str:split #\; (str:trim ext)))))
                (cons (str:trim (first parts))  ; extension name (trimmed)
                      (mapcar #'str:trim (rest parts))))) ; parameters (trimmed)
            (seq:realize (str:split #\, extension-header)))))

(defun negotiate-extensions (client-extensions server-extensions)
  "Negotiate extensions between client and server"
  ;; Simple implementation - just find common extensions
  ;; In a full implementation, this would handle extension parameters
  (when (and client-extensions server-extensions)
    (let ((client-names (mapcar #'car (parse-extensions client-extensions)))
          (server-names (mapcar #'car (parse-extensions server-extensions))))
      (intersection client-names server-names :test #'string=))))

;;; Utility functions

(defun websocket-request-p (request)
  "Check if HTTP request is a WebSocket upgrade request"
  (handler-case
      (progn
        (validate-websocket-request request)
        t)
    (websocket-handshake-error ()
      nil)))

(defun extract-websocket-key (request)
  "Extract Sec-WebSocket-Key from request headers"
  (let ((headers (http:request-headers request)))
    (or (map:get headers "sec-websocket-key")
        (map:get headers "Sec-WebSocket-Key"))))

(defun extract-subprotocols (request)
  "Extract requested subprotocols from request headers"
  (let* ((headers (http:request-headers request))
         (protocol-header (or (map:get headers "sec-websocket-protocol")
                              (map:get headers "Sec-WebSocket-Protocol"))))
    (when protocol-header
      (mapcar #'str:trim (seq:realize (str:split #\, protocol-header))))))

(defun extract-extensions (request)
  "Extract requested extensions from request headers"
  (let ((headers (http:request-headers request)))
    (or (map:get headers "sec-websocket-extensions")
        (map:get headers "Sec-WebSocket-Extensions"))))

;;; Helper functions for tests

(defun generate-handshake-headers (host path)
  "Generate WebSocket handshake request headers"
  (let ((key (generate-websocket-key)))
    (map:from-pairs
     `(("Host" . ,host)
       ("Upgrade" . "websocket")
       ("Connection" . "Upgrade")
       ("Sec-WebSocket-Key" . ,key)
       ("Sec-WebSocket-Version" . "13")))))

(defun valid-handshake-response-p (accept-key expected-key)
  "Check if accept key matches expected key"
  (string= accept-key (generate-accept-key expected-key)))
