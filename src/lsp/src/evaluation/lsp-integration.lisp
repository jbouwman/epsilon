;;;; LSP Integration for Evaluation
;;;;
;;;; This module integrates the evaluation subsystem with the LSP server,
;;;; adding custom LSP methods for session management and code evaluation.

(defpackage #:epsilon.lsp.evaluation.integration
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:api #:epsilon.lsp.evaluation.api)
   (#:server #:epsilon.lsp.server))
  (:export
   #:register-evaluation-handlers
   #:evaluation-middleware))

(in-package #:epsilon.lsp.evaluation.integration)

;;; LSP Method Names (using epsilon/ namespace for custom methods)

(defconstant +method-create-session+ "epsilon/evaluation/createSession")
(defconstant +method-list-sessions+ "epsilon/evaluation/listSessions")
(defconstant +method-get-session+ "epsilon/evaluation/getSession")
(defconstant +method-terminate-session+ "epsilon/evaluation/terminateSession")
(defconstant +method-evaluate+ "epsilon/evaluation/evaluate")
(defconstant +method-evaluate-selection+ "epsilon/evaluation/evaluateSelection")
(defconstant +method-complete-in-session+ "epsilon/evaluation/complete")
(defconstant +method-inspect-in-session+ "epsilon/evaluation/inspect")

;;; Registration

(defun register-evaluation-handlers (server)
  "Register evaluation-related handlers with the LSP server"
  (let ((evaluation-service (ensure-evaluation-service server)))
    
    ;; Session management
    (register-handler server +method-create-session+
                      (lambda (params)
                        (handle-create-session evaluation-service params)))
    
    (register-handler server +method-list-sessions+
                      (lambda (params)
                        (handle-list-sessions evaluation-service params)))
    
    (register-handler server +method-get-session+
                      (lambda (params)
                        (handle-get-session evaluation-service params)))
    
    (register-handler server +method-terminate-session+
                      (lambda (params)
                        (handle-terminate-session evaluation-service params)))
    
    ;; Evaluation operations
    (register-handler server +method-evaluate+
                      (lambda (params)
                        (handle-evaluate evaluation-service params)))
    
    (register-handler server +method-evaluate-selection+
                      (lambda (params)
                        (handle-evaluate-selection evaluation-service params)))
    
    (register-handler server +method-complete-in-session+
                      (lambda (params)
                        (handle-complete evaluation-service params)))
    
    (register-handler server +method-inspect-in-session+
                      (lambda (params)
                        (handle-inspect evaluation-service params)))))

(defun ensure-evaluation-service (server)
  "Ensure the server has an evaluation service"
  (or (get-evaluation-service server)
      (let ((service (api:make-evaluation-service)))
        (api:start-evaluation-service service)
        (set-evaluation-service server service)
        service)))

;;; Handler stubs - these need to be implemented based on server structure

(defun register-handler (server method handler)
  "Register a method handler with the server"
  ;; TODO: Integrate with actual server dispatch mechanism
  (declare (ignore server method handler)))

(defun get-evaluation-service (server)
  "Get the evaluation service from the server"
  ;; TODO: Add evaluation-service slot to server
  (declare (ignore server))
  nil)

(defun set-evaluation-service (server service)
  "Set the evaluation service on the server"
  ;; TODO: Add evaluation-service slot to server
  (declare (ignore server service)))

;;; Request Handlers

(defun handle-create-session (service params)
  "Handle epsilon/evaluation/createSession request"
  (let ((name (map:get params "name"))
        (modules (map:get params "modules"))
        (workspace (map:get params "workspace"))
        (restrictions (parse-restrictions (map:get params "restrictions"))))
    
    (api:create-evaluation-session service
                                   :name name
                                   :owner workspace
                                   :modules modules
                                   :restrictions restrictions)))

(defun handle-list-sessions (service params)
  "Handle epsilon/evaluation/listSessions request"
  (let ((workspace (map:get params "workspace")))
    (api:list-evaluation-sessions service :owner workspace)))

(defun handle-get-session (service params)
  "Handle epsilon/evaluation/getSession request"
  (let ((session-id (map:get params "sessionId")))
    (or (api:get-evaluation-session service session-id)
        (error "Session not found: ~A" session-id))))

(defun handle-terminate-session (service params)
  "Handle epsilon/evaluation/terminateSession request"
  (let ((session-id (map:get params "sessionId")))
    (api:terminate-evaluation-session service session-id)
    (map:make-map "status" "terminated")))

(defun handle-evaluate (service params)
  "Handle epsilon/evaluation/evaluate request"
  (let ((session-id (map:get params "sessionId"))
        (code (map:get params "code"))
        (timeout (map:get params "timeout")))
    
    ;; If no session specified, create ephemeral one
    (if session-id
        (api:evaluate service session-id code :timeout timeout)
        (api:evaluate-in-new-session service code
                                      :modules (map:get params "modules")
                                      :owner (map:get params "workspace")))))

(defun handle-evaluate-selection (service params)
  "Handle epsilon/evaluation/evaluateSelection request"
  ;; This is like evaluate but includes document context
  (let ((session-id (map:get params "sessionId"))
        (code (map:get params "code"))
        (uri (map:get params "textDocument" "uri"))
        (range (map:get params "range")))
    
    ;; Add context to evaluation
    (let ((enhanced-code (format nil ";;; Evaluating selection from ~A~%~A"
                                 uri code)))
      (if session-id
          (api:evaluate service session-id enhanced-code)
          (api:evaluate-in-new-session service enhanced-code
                                        :modules (map:get params "modules")
                                        :owner (map:get params "workspace"))))))

(defun handle-complete (service params)
  "Handle epsilon/evaluation/complete request"
  (let ((session-id (map:get params "sessionId"))
        (prefix (map:get params "prefix"))
        (position (map:get params "position"))
        (uri (map:get params "textDocument" "uri")))
    
    (unless session-id
      (error "Session required for completion"))
    
    (let ((completions (api:complete-symbol service session-id prefix
                                            :context (map:make-map
                                                      "position" position
                                                      "uri" uri))))
      ;; Convert to LSP CompletionItem format
      (map:make-map
       "isIncomplete" nil
       "items" (mapcar #'completion-to-lsp completions)))))

(defun handle-inspect (service params)
  "Handle epsilon/evaluation/inspect request"
  (let ((session-id (map:get params "sessionId"))
        (expression (map:get params "expression"))
        (verbose (map:get params "verbose")))
    
    (unless session-id
      (error "Session required for inspection"))
    
    (api:inspect-object service session-id expression :verbose verbose)))

;;; Helper Functions

(defun parse-restrictions (restrictions-map)
  "Parse restrictions from request into restriction struct"
  (when restrictions-map
    (api:make-restrictions
     :max-memory (map:get restrictions-map "maxMemory")
     :max-cpu-time (map:get restrictions-map "maxCpuTime")
     :allow-file-read (map:get restrictions-map "allowFileRead")
     :allow-file-write (map:get restrictions-map "allowFileWrite")
     :allow-network (map:get restrictions-map "allowNetwork"))))

(defun completion-to-lsp (completion)
  "Convert internal completion format to LSP CompletionItem"
  (map:make-map
   "label" (map:get completion "label")
   "kind" (completion-kind-to-lsp (map:get completion "kind"))
   "detail" (map:get completion "detail")
   "documentation" (map:get completion "documentation")))

(defun completion-kind-to-lsp (kind)
  "Convert completion kind to LSP CompletionItemKind"
  (case (intern (string-upcase kind) :keyword)
    (:function 3)    ; Function
    (:variable 6)    ; Variable
    (:macro 3)       ; Function (no specific macro kind)
    (:class 7)       ; Class
    (:method 2)      ; Method
    (:constant 21)   ; Constant
    (t 1)))          ; Text

;;; Middleware for evaluation support

(defun evaluation-middleware (handler)
  "Middleware that adds evaluation context to requests"
  (lambda (server message)
    ;; Check if this is an evaluation-related request
    (let ((method (and (server:jsonrpc-request-p message)
                       (server:jsonrpc-request-method message))))
      (when (and method (string-prefix-p "epsilon/evaluation/" method))
        ;; Add workspace context if not present
        (let ((params (server:jsonrpc-request-params message)))
          (unless (map:get params "workspace")
            (setf params (map:assoc params "workspace" 
                                    (get-current-workspace server))))))
    
    ;; Call original handler
    (funcall handler server message)))

(defun string-prefix-p (prefix string)
  "Check if STRING starts with PREFIX"
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun get-current-workspace (server)
  "Get the current workspace from the server"
  ;; TODO: Implement based on actual server structure
  (declare (ignore server))
  "default-workspace")
