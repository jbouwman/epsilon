;;;; WebSocket Handshake Tests

(defpackage epsilon.websocket.handshake.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (handshake epsilon.websocket.handshake)
   (req epsilon.http.request)
   (resp epsilon.http.response)
   (str epsilon.lib.string)
   (base64 epsilon.lib.base64)))

(in-package epsilon.websocket.handshake.tests)

(deftest key-generation
  "Test WebSocket key generation"
  (let ((key1 (handshake:generate-websocket-key))
        (key2 (handshake:generate-websocket-key)))
    
    ;; Keys should be different
    (is-not (string= key1 key2))
    
    ;; Keys should decode to 16 bytes
    (is-= (length (base64:base64-to-octets key1)) 16)
    (is-= (length (base64:base64-to-octets key2)) 16)))

(deftest accept-key-generation
  "Test Sec-WebSocket-Accept generation"
  ;; Test vector from RFC 6455
  (let* ((client-key "dGhlIHNhbXBsZSBub25jZQ==")
         (expected-accept "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
         (actual-accept (handshake:generate-accept-key client-key)))
    
    (is-equal expected-accept actual-accept)))

(deftest valid-upgrade-request
  "Test validation of valid WebSocket upgrade request"
  (let ((request (req:make-request
                 :method "GET"
                 :uri "/"
                 :headers (list (cons "Host" "example.com")
                               (cons "Upgrade" "websocket")
                               (cons "Connection" "Upgrade")
                               (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                               (cons "Sec-WebSocket-Version" "13")))))
    
    (is (handshake:validate-websocket-request request))))

(deftest invalid-upgrade-requests
  "Test validation rejects invalid upgrade requests"
  ;; Wrong method
  (let ((request (req:make-request
                 :method "POST"
                 :uri "/"
                 :headers (list (cons "Upgrade" "websocket")
                               (cons "Connection" "Upgrade")
                               (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                               (cons "Sec-WebSocket-Version" "13")))))
    (is-thrown (handshake:websocket-handshake-error)
               (handshake:validate-websocket-request request)))
  
  ;; Missing Upgrade header
  (let ((request (req:make-request
                 :method "GET"
                 :uri "/"
                 :headers (list (cons "Connection" "Upgrade")
                               (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                               (cons "Sec-WebSocket-Version" "13")))))
    (is-thrown (handshake:websocket-handshake-error)
               (handshake:validate-websocket-request request)))
  
  ;; Wrong version
  (let ((request (req:make-request
                 :method "GET"
                 :uri "/"
                 :headers (list (cons "Upgrade" "websocket")
                               (cons "Connection" "Upgrade")
                               (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                               (cons "Sec-WebSocket-Version" "8")))))
    (is-thrown (handshake:websocket-handshake-error)
               (handshake:validate-websocket-request request)))
  
  ;; Invalid WebSocket key
  (let ((request (req:make-request
                 :method "GET"
                 :uri "/"
                 :headers (list (cons "Upgrade" "websocket")
                               (cons "Connection" "Upgrade")
                               (cons "Sec-WebSocket-Key" "short")
                               (cons "Sec-WebSocket-Version" "13")))))
    (is-thrown (handshake:websocket-handshake-error)
               (handshake:validate-websocket-request request))))

(deftest handshake-response-creation
  "Test creation of handshake response"
  (let* ((request (req:make-request
                  :method "GET"
                  :uri "/"
                  :headers (list (cons "Host" "example.com")
                                (cons "Upgrade" "websocket")
                                (cons "Connection" "Upgrade")
                                (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                                (cons "Sec-WebSocket-Version" "13"))))
         (response (handshake:create-handshake-response request)))
    
    (is-= (resp:response-status response) 101)
    (is-equal (resp:response-reason response) "Switching Protocols")
    
    (let ((headers (resp:response-headers response)))
      (is-equal (resp:get-header headers "Upgrade") "websocket")
      (is-equal (resp:get-header headers "Connection") "Upgrade")
      (is-equal (resp:get-header headers "Sec-WebSocket-Accept") 
                "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))))

(deftest handshake-with-subprotocol
  "Test handshake with subprotocol negotiation"
  (let* ((request (req:make-request
                  :method "GET"
                  :uri "/"
                  :headers (list (cons "Host" "example.com")
                                (cons "Upgrade" "websocket")
                                (cons "Connection" "Upgrade")
                                (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                                (cons "Sec-WebSocket-Version" "13")
                                (cons "Sec-WebSocket-Protocol" "chat, superchat"))))
         (response (handshake:create-handshake-response request :subprotocol "chat")))
    
    (let ((headers (resp:response-headers response)))
      (is-equal (resp:get-header headers "Sec-WebSocket-Protocol") "chat"))))

(deftest client-upgrade-request
  "Test client upgrade request creation"
  (multiple-value-bind (request websocket-key)
      (handshake:create-upgrade-request "/test" :host "example.com")
    
    (is-equal (req:request-method request) "GET")
    (is-equal (req:request-uri request) "/test")
    
    (let ((headers (req:request-headers request)))
      (is-equal (req:get-header headers "Host") "example.com")
      (is-equal (req:get-header headers "Upgrade") "websocket")
      (is-equal (req:get-header headers "Connection") "Upgrade")
      (is-equal (req:get-header headers "Sec-WebSocket-Version") "13")
      (is-equal (req:get-header headers "Sec-WebSocket-Key") websocket-key))
    
    ;; Verify key is valid base64 and 16 bytes when decoded
    (is-= (length (base64:base64-to-octets websocket-key)) 16)))

(deftest client-upgrade-with-subprotocols
  "Test client upgrade request with subprotocols"
  (multiple-value-bind (request websocket-key)
      (handshake:create-upgrade-request "/test" 
                                       :host "example.com"
                                       :subprotocols '("chat" "superchat"))
    (declare (ignore websocket-key))
    
    (let ((headers (req:request-headers request)))
      (is-equal (req:get-header headers "Sec-WebSocket-Protocol") "chat, superchat"))))

(deftest handshake-response-validation
  "Test validation of server handshake response"
  (let* ((websocket-key "dGhlIHNhbXBsZSBub25jZQ==")
         (valid-response (resp:make-response
                         :status 101
                         :reason "Switching Protocols"
                         :headers (list (cons "Upgrade" "websocket")
                                       (cons "Connection" "Upgrade")
                                       (cons "Sec-WebSocket-Accept" 
                                             "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))))
         (invalid-response (resp:make-response
                           :status 101
                           :reason "Switching Protocols"
                           :headers (list (cons "Upgrade" "websocket")
                                         (cons "Connection" "Upgrade")
                                         (cons "Sec-WebSocket-Accept" "wrong-key")))))
    
    ;; Valid response should pass
    (is (handshake:validate-handshake-response valid-response websocket-key))
    
    ;; Invalid response should fail
    (is-thrown (handshake:websocket-handshake-error)
               (handshake:validate-handshake-response invalid-response websocket-key))))

(deftest subprotocol-negotiation
  "Test subprotocol negotiation"
  ;; Client and server have common protocol
  (let ((result (handshake:negotiate-subprotocol 
                '("chat" "superchat") 
                '("superchat" "echo"))))
    (is-equal result "superchat"))
  
  ;; No common protocol
  (let ((result (handshake:negotiate-subprotocol 
                '("chat" "superchat") 
                '("echo" "binary"))))
    (is (null result)))
  
  ;; String input
  (let ((result (handshake:negotiate-subprotocol 
                "chat, superchat" 
                '("superchat" "echo"))))
    (is-equal result "superchat")))

(deftest extension-parsing
  "Test extension header parsing"
  (let ((extensions (handshake:parse-extensions 
                    "permessage-deflate; client_max_window_bits, x-webkit-deflate-frame")))
    
    (is-= (length extensions) 2)
    (is-equal (caar extensions) "permessage-deflate")
    (is (member "client_max_window_bits" (cdar extensions) :test #'string=))
    (is-equal (caadr extensions) "x-webkit-deflate-frame")))

(deftest websocket-request-detection
  "Test WebSocket request detection"
  (let ((websocket-request (req:make-request
                           :method "GET"
                           :uri "/"
                           :headers (list (cons "Upgrade" "websocket")
                                         (cons "Connection" "Upgrade")
                                         (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                                         (cons "Sec-WebSocket-Version" "13"))))
        (http-request (req:make-request
                      :method "GET"
                      :uri "/"
                      :headers (list (cons "Accept" "text/html")))))
    
    (is (handshake:websocket-request-p websocket-request))
    (is-not (handshake:websocket-request-p http-request))))

(deftest header-extraction
  "Test extraction of WebSocket-specific headers"
  (let ((request (req:make-request
                 :method "GET"
                 :uri "/"
                 :headers (list (cons "Sec-WebSocket-Key" "test-key")
                               (cons "Sec-WebSocket-Protocol" "chat, echo")
                               (cons "Sec-WebSocket-Extensions" "deflate")))))
    
    (is-equal (handshake:extract-websocket-key request) "test-key")
    (is-equal (handshake:extract-subprotocols request) '("chat" "echo"))
    (is-equal (handshake:extract-extensions request) "deflate")))

(deftest case-insensitive-headers
  "Test case-insensitive header handling"
  (let ((request (req:make-request
                 :method "GET"
                 :uri "/"
                 :headers (list (cons "upgrade" "websocket")      ; lowercase
                               (cons "CONNECTION" "UPGRADE")     ; uppercase
                               (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                               (cons "Sec-WebSocket-Version" "13")))))
    
    (is (handshake:validate-websocket-request request))))

(deftest connection-header-variants
  "Test various Connection header formats"
  (dolist (connection-value '("Upgrade" "upgrade" "keep-alive, upgrade" "Upgrade, close"))
    (let ((request (req:make-request
                   :method "GET"
                   :uri "/"
                   :headers (list (cons "Upgrade" "websocket")
                                 (cons "Connection" connection-value)
                                 (cons "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ==")
                                 (cons "Sec-WebSocket-Version" "13")))))
      
      (is (handshake:validate-websocket-request request)
          (format nil "Failed with Connection: ~A" connection-value)))))

(deftest malformed-headers
  "Test handling of malformed headers"
  ;; Empty WebSocket key
  (let ((request (req:make-request
                 :method "GET"
                 :uri "/"
                 :headers (list (cons "Upgrade" "websocket")
                               (cons "Connection" "Upgrade")
                               (cons "Sec-WebSocket-Key" "")
                               (cons "Sec-WebSocket-Version" "13")))))
    (is-thrown (handshake:websocket-handshake-error)
               (handshake:validate-websocket-request request)))
  
  ;; Malformed base64 key
  (let ((request (req:make-request
                 :method "GET"
                 :uri "/"
                 :headers (list (cons "Upgrade" "websocket")
                               (cons "Connection" "Upgrade")
                               (cons "Sec-WebSocket-Key" "not-base64!")
                               (cons "Sec-WebSocket-Version" "13")))))
    (is-thrown (error)
               (handshake:validate-websocket-request request))))