(defpackage #:epsilon.lsp.protocol.jsonrpc
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:json #:epsilon.json)
   (#:stream #:epsilon.stream))
  (:export
   #:jsonrpc-request
   #:jsonrpc-request-p
   #:jsonrpc-request-id
   #:jsonrpc-request-method
   #:jsonrpc-request-params
   #:jsonrpc-response
   #:jsonrpc-response-p
   #:jsonrpc-response-id
   #:jsonrpc-response-result
   #:jsonrpc-notification
   #:jsonrpc-notification-p
   #:jsonrpc-notification-method
   #:jsonrpc-notification-params
   #:jsonrpc-error
   #:jsonrpc-error-p
   #:jsonrpc-error-id
   #:jsonrpc-error-code
   #:jsonrpc-error-message
   #:jsonrpc-error-data
   #:make-request
   #:make-response
   #:make-notification
   #:make-error-response
   #:parse-message
   #:serialize-message
   #:read-message
   #:write-message
   #:+parse-error+
   #:+invalid-request+
   #:+method-not-found+
   #:+invalid-params+
   #:+internal-error+
   #:+server-not-initialized+
   #:+unknown-error-code+
   #:add-content-length-header))

(in-package #:epsilon.lsp.protocol.jsonrpc)

;;; JSON-RPC 2.0 Message Types

(defstruct jsonrpc-request
  "JSON-RPC 2.0 request message."
  id
  method
  params)

(defstruct jsonrpc-response
  "JSON-RPC 2.0 response message."
  id
  result)

(defstruct jsonrpc-notification
  "JSON-RPC 2.0 notification message."
  method
  params)

(defstruct jsonrpc-error
  "JSON-RPC 2.0 error response."
  id
  code
  message
  data)

;;; Error Codes (LSP specification)
(defparameter +parse-error+ -32700)
(defparameter +invalid-request+ -32600)
(defparameter +method-not-found+ -32601)
(defparameter +invalid-params+ -32602)
(defparameter +internal-error+ -32603)
(defparameter +server-not-initialized+ -32002)
(defparameter +unknown-error-code+ -32001)

;;; Message Construction

(defun make-request (id method &optional params)
  "Create a JSON-RPC request."
  (make-jsonrpc-request :id id :method method :params params))

(defun make-response (id result)
  "Create a JSON-RPC response."
  (make-jsonrpc-response :id id :result result))

(defun make-notification (method &optional params)
  "Create a JSON-RPC notification."
  (make-jsonrpc-notification :method method :params params))

(defun make-error-response (id code message &optional data)
  "Create a JSON-RPC error response."
  (make-jsonrpc-error :id id :code code :message message :data data))

;;; Message Serialization

(defun serialize-message (message)
  "Serialize a JSON-RPC message to JSON string."
  (let ((json-map (cond
                    ((jsonrpc-request-p message)
                     (map:make-map 
                      "jsonrpc" "2.0"
                      "id" (jsonrpc-request-id message)
                      "method" (jsonrpc-request-method message)
                      "params" (jsonrpc-request-params message)))
                    
                    ((jsonrpc-response-p message)
                     (map:make-map
                      "jsonrpc" "2.0"
                      "id" (jsonrpc-response-id message)
                      "result" (jsonrpc-response-result message)))
                    
                    ((jsonrpc-notification-p message)
                     (let ((base-map (map:make-map
                                      "jsonrpc" "2.0"
                                      "method" (jsonrpc-notification-method message))))
                       (if (jsonrpc-notification-params message)
                           (map:put base-map "params" (jsonrpc-notification-params message))
                           base-map)))
                    
                    ((jsonrpc-error-p message)
                     (let ((error-map (map:make-map
                                       "code" (jsonrpc-error-code message)
                                       "message" (jsonrpc-error-message message))))
                       (when (jsonrpc-error-data message)
                         (setf error-map (map:put error-map "data" (jsonrpc-error-data message))))
                       (map:make-map
                        "jsonrpc" "2.0"
                        "id" (jsonrpc-error-id message)
                        "error" error-map)))
                    
                    (t (error "Unknown message type: ~A" message)))))
    (with-output-to-string (stream)
      (json:encode json-map stream))))

(defun parse-message (json-string)
  "Parse a JSON-RPC message from JSON string."
  (let ((json-map (json:decode json-string)))
    (unless (string= (map:get json-map "jsonrpc") "2.0")
      (error "Invalid JSON-RPC version"))
    
    (cond
      ;; Error response
      ((map:contains-key-p json-map "error")
       (let ((error-map (map:get json-map "error")))
         (make-jsonrpc-error
          :id (map:get json-map "id")
          :code (map:get error-map "code")
          :message (map:get error-map "message")
          :data (map:get error-map "data"))))
      
      ;; Response
      ((map:contains-key-p json-map "result")
       (make-jsonrpc-response
        :id (map:get json-map "id")
        :result (map:get json-map "result")))
      
      ;; Request or Notification
      ((map:contains-key-p json-map "method")
       (if (map:contains-key-p json-map "id")
           ;; Request
           (make-jsonrpc-request
            :id (map:get json-map "id")
            :method (map:get json-map "method")
            :params (map:get json-map "params"))
           ;; Notification
           (make-jsonrpc-notification
            :method (map:get json-map "method")
            :params (map:get json-map "params"))))
      
      (t (error "Invalid JSON-RPC message")))))

;;; LSP Transport Layer

(defun read-message (input-stream)
  "Read an LSP message from stream (with Content-Length header)."
  (let ((content-length nil))
    ;; Read headers
    (loop for line = (read-line input-stream nil nil)
          while (and line (not (string= line "")))
          do (when (string-prefix-p "Content-Length: " line)
               (setf content-length 
                     (parse-integer (subseq line (length "Content-Length: "))))))
    
    (unless content-length
      (error "Missing Content-Length header"))
    
    ;; Read content
    (let ((content (make-string content-length)))
      (read-sequence content input-stream)
      (parse-message content))))

(defun write-message (message output-stream)
  "Write an LSP message to stream (with Content-Length header)."
  (let ((content (serialize-message message)))
    (format output-stream "Content-Length: ~D~C~C~C~C~A"
            (length content) #\Return #\Newline #\Return #\Newline content)
    (force-output output-stream)))

(defun string-prefix-p (prefix string)
  "Check if STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= prefix (subseq string 0 (length prefix)))))
