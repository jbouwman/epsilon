(defpackage #:epsilon.lsp.tests.protocol
  (:use #:common-lisp #:epsilon.test)
  (:local-nicknames
   (#:jsonrpc #:epsilon.lsp.protocol.jsonrpc)
   (#:protocol #:epsilon.lsp.protocol)
   (#:map #:epsilon.map)))

(in-package #:epsilon.lsp.tests.protocol)

(define-test-suite protocol-tests
  "Test suite for LSP protocol implementation.")

(define-test json-rpc-request-creation
  "Test JSON-RPC request creation and serialization."
  (let ((request (jsonrpc:make-request 1 "initialize" 
                                       (map:make-map "processId" 12345))))
    (is (jsonrpc:jsonrpc-request-p request))
    (is (= 1 (jsonrpc:jsonrpc-request-id request)))
    (is (string= "initialize" (jsonrpc:jsonrpc-request-method request)))
    
    (let ((json (jsonrpc:serialize-message request)))
      (is (stringp json))
      (is (> (length json) 0)))))

(define-test json-rpc-response-creation
  "Test JSON-RPC response creation and serialization."
  (let ((response (jsonrpc:make-response 1 (map:make-map "result" "success"))))
    (is (jsonrpc:jsonrpc-response-p response))
    (is (= 1 (jsonrpc:jsonrpc-response-id response)))
    
    (let ((json (jsonrpc:serialize-message response)))
      (is (stringp json))
      (is (> (length json) 0)))))

(define-test json-rpc-notification-creation
  "Test JSON-RPC notification creation and serialization."
  (let ((notification (jsonrpc:make-notification "textDocument/didOpen")))
    (is (jsonrpc:jsonrpc-notification-p notification))
    (is (string= "textDocument/didOpen" 
                 (jsonrpc:jsonrpc-notification-method notification)))
    
    (let ((json (jsonrpc:serialize-message notification)))
      (is (stringp json))
      (is (> (length json) 0)))))

(define-test json-rpc-error-creation
  "Test JSON-RPC error creation and serialization."
  (let ((error (jsonrpc:make-error-response 1 -32601 "Method not found")))
    (is (jsonrpc:jsonrpc-error-p error))
    (is (= 1 (jsonrpc:jsonrpc-error-id error)))
    (is (= -32601 (jsonrpc:jsonrpc-error-code error)))
    (is (string= "Method not found" (jsonrpc:jsonrpc-error-message error)))
    
    (let ((json (jsonrpc:serialize-message error)))
      (is (stringp json))
      (is (> (length json) 0)))))

(define-test json-rpc-parse-request
  "Test parsing JSON-RPC requests."
  (let* ((request-json "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":12345}}")
         (parsed (jsonrpc:parse-message request-json)))
    (is (jsonrpc:jsonrpc-request-p parsed))
    (is (= 1 (jsonrpc:jsonrpc-request-id parsed)))
    (is (string= "initialize" (jsonrpc:jsonrpc-request-method parsed)))))

(define-test json-rpc-parse-response
  "Test parsing JSON-RPC responses."
  (let* ((response-json "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":{}}}")
         (parsed (jsonrpc:parse-message response-json)))
    (is (jsonrpc:jsonrpc-response-p parsed))
    (is (= 1 (jsonrpc:jsonrpc-response-id parsed)))))

(define-test json-rpc-parse-notification
  "Test parsing JSON-RPC notifications."
  (let* ((notification-json "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}")
         (parsed (jsonrpc:parse-message notification-json)))
    (is (jsonrpc:jsonrpc-notification-p parsed))
    (is (string= "initialized" (jsonrpc:jsonrpc-notification-method parsed)))))

(define-test protocol-handler-creation
  "Test protocol handler creation."
  (let ((handler (protocol:make-jsonrpc-handler)))
    (is (protocol:protocol-handler-p handler))
    (is (eq :json-rpc (protocol:protocol-handler-type handler)))))

(define-test roundtrip-request
  "Test request roundtrip: create -> serialize -> parse."
  (let* ((original (jsonrpc:make-request 42 "test/method" 
                                         (map:make-map "param1" "value1")))
         (json (jsonrpc:serialize-message original))
         (parsed (jsonrpc:parse-message json)))
    
    (is (jsonrpc:jsonrpc-request-p parsed))
    (is (= (jsonrpc:jsonrpc-request-id original)
           (jsonrpc:jsonrpc-request-id parsed)))
    (is (string= (jsonrpc:jsonrpc-request-method original)
                 (jsonrpc:jsonrpc-request-method parsed)))))

(run-test-suite 'protocol-tests)
