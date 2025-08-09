;;;; Tests for LSP Protocol Implementation
;;;;
;;;; Tests for the epsilon.lsp.protocol modules

(defpackage #:epsilon.lsp.tests.protocol
  (:use #:common-lisp #:epsilon.test)
  (:local-nicknames
   (#:jsonrpc #:epsilon.lsp.protocol.jsonrpc)
   (#:protocol #:epsilon.lsp.protocol)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)))

(in-package #:epsilon.lsp.tests.protocol)

(deftest json-rpc-request-creation
  "Test JSON-RPC request creation and serialization."
  (let ((request (jsonrpc:make-request 1 "initialize" 
                                       (map:make-map "processId" 12345))))
    (is (jsonrpc:jsonrpc-request-p request))
    (is-equal 1 (jsonrpc:jsonrpc-request-id request))
    (is-equal "initialize" (jsonrpc:jsonrpc-request-method request))
    
    (let ((json (jsonrpc:serialize-message request)))
      (is (stringp json))
      (is (> (length json) 0)))))

(deftest json-rpc-response-creation
  "Test JSON-RPC response creation and serialization."
  (let ((response (jsonrpc:make-response 1 (map:make-map "result" "success"))))
    (is (jsonrpc:jsonrpc-response-p response))
    (is-equal 1 (jsonrpc:jsonrpc-response-id response))
    
    (let ((json (jsonrpc:serialize-message response)))
      (is (stringp json))
      (is (> (length json) 0)))))

(deftest json-rpc-notification-creation
  "Test JSON-RPC notification creation and serialization."
  (let ((notification (jsonrpc:make-notification "initialized" 
                                                 (map:make-map "status" "ready"))))
    (is (jsonrpc:jsonrpc-notification-p notification))
    (is-equal "initialized" (jsonrpc:jsonrpc-notification-method notification))
    
    (let ((json (jsonrpc:serialize-message notification)))
      (is (stringp json))
      (is (> (length json) 0)))))

(deftest json-rpc-error-response-creation
  "Test JSON-RPC error response creation and serialization."
  (let ((error-response (jsonrpc:make-error-response 1 -32601 "Method not found")))
    (is (jsonrpc:jsonrpc-error-p error-response))
    (is-equal 1 (jsonrpc:jsonrpc-error-id error-response))
    (is-equal -32601 (jsonrpc:jsonrpc-error-code error-response))
    (is-equal "Method not found" (jsonrpc:jsonrpc-error-message error-response))
    
    (let ((json (jsonrpc:serialize-message error-response)))
      (is (stringp json))
      (is (> (length json) 0)))))

(deftest protocol-handler-creation
  "Test protocol handler creation."
  (let ((handler (protocol:make-jsonrpc-handler)))
    (is (protocol:protocol-handler-p handler))
    (is-equal :json-rpc (protocol:protocol-handler-type handler))))

(deftest roundtrip-request
  "Test request roundtrip: create -> serialize -> parse."
  (let* ((original (jsonrpc:make-request 42 "test/method" 
                                         (map:make-map "param1" "value1")))
         (json (jsonrpc:serialize-message original))
         (parsed (jsonrpc:parse-message json)))
    
    (is (jsonrpc:jsonrpc-request-p parsed))
    (is-equal 42 (jsonrpc:jsonrpc-request-id parsed))
    (is-equal "test/method" (jsonrpc:jsonrpc-request-method parsed))
    (is-equal "value1" (map:get (jsonrpc:jsonrpc-request-params parsed) "param1"))))

(deftest roundtrip-response
  "Test response roundtrip: create -> serialize -> parse."
  (let* ((original (jsonrpc:make-response 99 (map:make-map "success" t)))
         (json (jsonrpc:serialize-message original))
         (parsed (jsonrpc:parse-message json)))
    
    (is (jsonrpc:jsonrpc-response-p parsed))
    (is-equal 99 (jsonrpc:jsonrpc-response-id parsed))
    (is-equal t (map:get (jsonrpc:jsonrpc-response-result parsed) "success"))))

(deftest message-parsing-invalid-json
  "Test that invalid JSON raises an error."
  (let ((invalid-json "{invalid json"))
    (is-thrown (error) (jsonrpc:parse-message invalid-json))))

(deftest content-length-header-handling
  "Test Content-Length header handling via write-message."
  (let* ((message (jsonrpc:make-request 1 "test" (map:make-map)))
         (output (with-output-to-string (stream)
                   (jsonrpc:write-message message stream))))
    
    (is (stringp output))
    (is (str:contains-p "Content-Length:" output))
    (is (str:contains-p "test" output))))