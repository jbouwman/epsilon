(defpackage #:epsilon.lsp.protocol
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:jsonrpc #:epsilon.lsp.protocol.jsonrpc))
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
   #:write-message))

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
   :read-fn #'jsonrpc:read-message
   :write-fn #'jsonrpc:write-message
   :make-request-fn #'jsonrpc:make-request
   :make-response-fn #'jsonrpc:make-response
   :make-notification-fn #'jsonrpc:make-notification
   :make-error-fn #'jsonrpc:make-error-response))

(defun read-message (handler stream)
  "Read a message using the protocol handler."
  (funcall (protocol-handler-read-fn handler) stream))

(defun write-message (handler message stream)
  "Write a message using the protocol handler."
  (funcall (protocol-handler-write-fn handler) message stream))

