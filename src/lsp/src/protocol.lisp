(defpackage #:epsilon.lsp.protocol
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map))
  (:export
   #:protocol-handler
   #:protocol-handler-p
   #:protocol-handler-type
   #:protocol-handler-read-fn
   #:protocol-handler-write-fn
   #:protocol-handler-make-request-fn
   #:protocol-handler-make-response-fn
   #:protocol-handler-make-notification-fn
   #:protocol-handler-make-error-fn
   #:make-protocol-handler
   #:make-jsonrpc-handler
   #:read-message
   #:write-message
   #:make-request
   #:make-response
   #:make-notification
   #:make-error-response))

(in-package #:epsilon.lsp.protocol)

(defstruct protocol-handler
  "Abstract protocol handler for different RPC protocols."
  type
  read-fn
  write-fn
  make-request-fn
  make-response-fn
  make-notification-fn
  make-error-fn)

(defun make-jsonrpc-handler ()
  "Create a JSON-RPC protocol handler."
  (make-protocol-handler
   :type :json-rpc
   :read-fn #'epsilon.lsp.protocol.jsonrpc:read-message
   :write-fn #'epsilon.lsp.protocol.jsonrpc:write-message
   :make-request-fn #'epsilon.lsp.protocol.jsonrpc:make-request
   :make-response-fn #'epsilon.lsp.protocol.jsonrpc:make-response
   :make-notification-fn #'epsilon.lsp.protocol.jsonrpc:make-notification
   :make-error-fn #'epsilon.lsp.protocol.jsonrpc:make-error-response))

(defun read-message (handler stream)
  "Read a message using the protocol handler."
  (funcall (protocol-handler-read-fn handler) stream))

(defun write-message (handler message stream)
  "Write a message using the protocol handler."
  (funcall (protocol-handler-write-fn handler) message stream))

(defun make-request (handler id method &optional params)
  "Create a request using the protocol handler."
  (funcall (protocol-handler-make-request-fn handler) id method params))

(defun make-response (handler id result)
  "Create a response using the protocol handler."
  (funcall (protocol-handler-make-response-fn handler) id result))

(defun make-notification (handler method &optional params)
  "Create a notification using the protocol handler."
  (funcall (protocol-handler-make-notification-fn handler) method params))

(defun make-error-response (handler id code message &optional data)
  "Create an error response using the protocol handler."
  (funcall (protocol-handler-make-error-fn handler) id code message data))
