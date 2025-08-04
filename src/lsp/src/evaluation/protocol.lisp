;;;; Evaluation Protocol
;;;;
;;;; This module defines the communication protocol between the LSP server
;;;; and evaluation subprocesses. Uses JSON for message serialization with
;;;; support for streaming results and error handling.

(defpackage #:epsilon.lsp.evaluation.protocol
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:json #:epsilon.json)
   (#:uuid #:epsilon.uuid))
  (:export
   ;; Message types
   #:+message-type-request+
   #:+message-type-response+
   #:+message-type-notification+
   #:+message-type-error+
   
   ;; Request types
   #:+request-init+
   #:+request-evaluate+
   #:+request-complete+
   #:+request-inspect+
   #:+request-shutdown+
   
   ;; Message creation
   #:make-request
   #:make-response
   #:make-error-response
   #:make-notification
   #:make-progress-notification
   
   ;; Request creation helpers
   #:make-init-request
   #:make-evaluate-request
   #:make-complete-request
   #:make-inspect-request
   
   ;; Message handling
   #:parse-message
   #:message-type
   #:message-id
   #:message-method
   #:message-params
   #:message-result
   #:message-error
   
   ;; Evaluation results
   #:make-evaluation-result
   #:evaluation-result-value
   #:evaluation-result-output
   #:evaluation-result-duration
   #:evaluation-result-bindings
   
   ;; Error codes
   #:+error-parse-error+
   #:+error-invalid-request+
   #:+error-method-not-found+
   #:+error-invalid-params+
   #:+error-internal-error+
   #:+error-timeout+
   #:+error-evaluation-error+))

(in-package #:epsilon.lsp.evaluation.protocol)

;;; Constants

(defparameter +message-type-request+ "request")
(defparameter +message-type-response+ "response")
(defparameter +message-type-notification+ "notification")
(defparameter +message-type-error+ "error")

(defparameter +request-init+ "init")
(defparameter +request-evaluate+ "evaluate")
(defparameter +request-complete+ "complete")
(defparameter +request-inspect+ "inspect")
(defparameter +request-shutdown+ "shutdown")

;;; Error codes (matching JSON-RPC where applicable)

(defparameter +error-parse-error+ -32700)
(defparameter +error-invalid-request+ -32600)
(defparameter +error-method-not-found+ -32601)
(defparameter +error-invalid-params+ -32602)
(defparameter +error-internal-error+ -32603)
(defparameter +error-timeout+ -32000)
(defparameter +error-evaluation-error+ -32001)

;;; Message Creation

(defun make-request (method &key params id)
  "Create a request message"
  (let ((message (map:make-map
                  "type" +message-type-request+
                  "method" method
                  "id" (or id (uuid:make-v4)))))
    (when params
      (setf message (map:assoc message "params" params)))
    message))

(defun make-response (id result)
  "Create a response message"
  (map:make-map
   "type" +message-type-response+
   "id" id
   "result" result))

(defun make-error-response (id code message &optional data)
  "Create an error response message"
  (let ((error-obj (map:make-map
                    "code" code
                    "message" message)))
    (when data
      (setf error-obj (map:assoc error-obj "data" data)))
    (map:make-map
     "type" +message-type-error+
     "id" id
     "error" error-obj)))

(defun make-notification (method &key params)
  "Create a notification message"
  (let ((message (map:make-map
                  "type" +message-type-notification+
                  "method" method)))
    (when params
      (setf message (map:assoc message "params" params)))
    message))

(defun make-progress-notification (id progress &key message)
  "Create a progress notification for long-running operations"
  (make-notification "progress"
                     :params (map:make-map
                              "id" id
                              "progress" progress
                              "message" message)))

;;; Message Parsing

(defun parse-message (json-string)
  "Parse a JSON message string into a message map"
  (handler-case
      (json:decode json-string)
    (error (e)
      (error "Failed to parse message: ~A" e))))

(defun message-type (message)
  "Get the type of a message"
  (map:get message "type"))

(defun message-id (message)
  "Get the ID of a message"
  (map:get message "id"))

(defun message-method (message)
  "Get the method of a request/notification"
  (map:get message "method"))

(defun message-params (message)
  "Get the params of a request/notification"
  (map:get message "params"))

(defun message-result (message)
  "Get the result of a response"
  (map:get message "result"))

(defun message-error (message)
  "Get the error of an error response"
  (map:get message "error"))

;;; Evaluation Results

(defun make-evaluation-result (&key value output warnings duration bindings)
  "Create an evaluation result object"
  (let ((result (map:make-map)))
    (when value
      (setf result (map:assoc result "value" value)))
    (when output
      (setf result (map:assoc result "output" output)))
    (when warnings
      (setf result (map:assoc result "warnings" warnings)))
    (when duration
      (setf result (map:assoc result "duration" duration)))
    (when bindings
      (setf result (map:assoc result "bindings" bindings)))
    result))

(defun evaluation-result-value (result)
  "Get the value from an evaluation result"
  (map:get result "value"))

(defun evaluation-result-output (result)
  "Get the output from an evaluation result"
  (map:get result "output"))

(defun evaluation-result-duration (result)
  "Get the duration from an evaluation result"
  (map:get result "duration"))

(defun evaluation-result-bindings (result)
  "Get new bindings from an evaluation result"
  (map:get result "bindings"))

;;; Protocol-specific messages

(defun make-init-request (modules restrictions)
  "Create an initialization request"
  (make-request +request-init+
                :params (map:make-map
                         "modules" modules
                         "restrictions" restrictions)))

(defun make-evaluate-request (code &key bindings timeout)
  "Create an evaluation request"
  (let ((params (map:make-map "code" code)))
    (when bindings
      (setf params (map:assoc params "bindings" bindings)))
    (when timeout
      (setf params (map:assoc params "timeout" timeout)))
    (make-request +request-evaluate+ :params params)))

(defun make-complete-request (prefix &key context)
  "Create a completion request"
  (let ((params (map:make-map "prefix" prefix)))
    (when context
      (setf params (map:assoc params "context" context)))
    (make-request +request-complete+ :params params)))

(defun make-inspect-request (object &key verbose)
  "Create an inspection request"
  (make-request +request-inspect+
                :params (map:make-map
                         "object" object
                         "verbose" verbose)))
