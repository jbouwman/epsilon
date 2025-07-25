;;;; LSP Server Extensions
;;;;
;;;; Additional LSP functionality including diagnostics, document symbols,
;;;; workspace symbols, and references.

(in-package #:epsilon.lsp.server)

;;; Update method dispatch to include new methods

(defun dispatch-method (server method params)
  "Dispatch an LSP method to its handler."
  (cond
    ((string= method "initialize")
     (handle-initialize server params))
    
    ((string= method "shutdown")
     (handle-shutdown server params))
    
    ((string= method "textDocument/definition")
     (handle-definition server params))
    
    ((string= method "textDocument/hover")
     (handle-hover server params))
    
    ((string= method "textDocument/completion")
     (handle-completion server params))
    
    ((string= method "textDocument/documentSymbol")
     (handle-document-symbol server params))
    
    ((string= method "textDocument/references")
     (handle-references server params))
    
    ((string= method "workspace/symbol")
     (handle-workspace-symbol server params))
    
    ;; Evaluation extensions
    ((string= method "epsilon/evaluation/createSession")
     (handle-evaluation-create-session server params))
    
    ((string= method "epsilon/evaluation/listSessions")
     (handle-evaluation-list-sessions server params))
    
    ((string= method "epsilon/evaluation/terminateSession")
     (handle-evaluation-terminate-session server params))
    
    ((string= method "epsilon/evaluation/evaluate")
     (handle-evaluation-evaluate server params))
    
    (t 
     (error "Method not found: ~A" method))))

;;; Diagnostic Support

(defun send-diagnostics (server uri)
  "Send diagnostic information for a document."
  (let* ((doc-analysis (workspace:workspace-get-document 
                        (lsp-server-workspace server) uri))
         (diagnostics (when doc-analysis
                        (convert-errors-to-diagnostics 
                         (analysis:document-analysis-errors doc-analysis))))
         (handler (lsp-server-protocol-handler server))
         (notification (protocol:make-notification 
                        handler 
                        "textDocument/publishDiagnostics"
                        (map:make-map
                         "uri" uri
                         "diagnostics" (or diagnostics '())))))
    
    (protocol:write-message handler notification *standard-output*)))

(defun convert-errors-to-diagnostics (errors)
  "Convert analysis errors to LSP diagnostic format."
  (mapcar (lambda (error)
            (if (analysis:diagnostic-p error)
                (map:make-map
                 "range" (map:make-map
                           "start" (map:make-map "line" 0 "character" 0)
                           "end" (map:make-map "line" 0 "character" 0))
                 "severity" (case (analysis:diagnostic-severity error)
                               (:error 1)
                               (:warning 2)
                               (:information 3)
                               (:hint 4)
                               (t 1))
                 "message" (analysis:diagnostic-message error))
                ;; Handle string errors (legacy)
                (map:make-map
                 "range" (map:make-map
                           "start" (map:make-map "line" 0 "character" 0)
                           "end" (map:make-map "line" 0 "character" 0))
                 "severity" 1
                 "message" (if (stringp error) error (format nil "~A" error)))))
          errors))

;;; Additional LSP Methods

(defun handle-document-symbol (server params)
  "Handle textDocument/documentSymbol request."
  (let* ((text-document (map:get params "textDocument"))
         (uri (map:get text-document "uri"))
         (doc-analysis (workspace:workspace-get-document 
                        (lsp-server-workspace server) uri)))
    
    (if doc-analysis
        (mapcar #'symbol-to-document-symbol 
                (analysis:document-analysis-symbols doc-analysis))
        '())))

(defun symbol-to-document-symbol (symbol)
  "Convert a symbol-info to LSP DocumentSymbol format."
  (let ((pos (analysis:symbol-info-position symbol))
        (range (analysis:symbol-info-range symbol)))
    (map:make-map
     "name" (analysis:symbol-info-name symbol)
     "kind" (symbol-type-to-lsp-kind (analysis:symbol-info-type symbol))
     "range" (map:make-map
               "start" (map:make-map 
                         "line" (1- (car pos))
                         "character" (1- (cdr pos)))
               "end" (map:make-map
                       "line" (1- (car (cdr range)))
                       "character" (cdr (cdr range))))
     "selectionRange" (map:make-map
                        "start" (map:make-map 
                                  "line" (1- (car pos))
                                  "character" (1- (cdr pos)))
                        "end" (map:make-map
                                "line" (1- (car (cdr range)))
                                "character" (cdr (cdr range))))
     "detail" (format nil "~A" (analysis:symbol-info-type symbol)))))

(defun symbol-type-to-lsp-kind (symbol-type)
  "Convert symbol type to LSP SymbolKind."
  (case symbol-type
    (:function 12)      ; Function
    (:variable 13)      ; Variable  
    (:constant 14)      ; Constant
    (:class 5)          ; Class
    (:package 4)        ; Namespace
    (:macro 12)         ; Function (macro)
    (t 1)))             ; File

(defun handle-workspace-symbol (server params)
  "Handle workspace/symbol request."
  (let* ((query (map:get params "query"))
         (workspace (lsp-server-workspace server))
         (all-symbols (workspace:workspace-get-all-symbols workspace)))
    
    (remove-if-not (lambda (symbol-location)
                     (str:contains-p (analysis:symbol-info-name 
                                      (workspace:symbol-location-symbol-info symbol-location))
                                     query))
                   all-symbols)))

(defun handle-references (server params)
  "Handle textDocument/references request."
  (let* ((text-document (map:get params "textDocument"))
         (uri (map:get text-document "uri"))
         (position (map:get params "position"))
         (line (1+ (map:get position "line")))
         (character (1+ (map:get position "character")))
         (doc-analysis (workspace:workspace-get-document 
                        (lsp-server-workspace server) uri)))
    
    (if doc-analysis
        (let ((symbol-at-pos (analysis:symbol-at-position doc-analysis (cons line character))))
          (if symbol-at-pos
              (let ((references (analysis:find-references 
                                 doc-analysis 
                                 (analysis:symbol-info-name symbol-at-pos))))
                (mapcar (lambda (ref)
                          (let ((ref-pos (analysis:symbol-info-position ref)))
                            (map:make-map
                             "uri" uri
                             "range" (map:make-map
                                        "start" (map:make-map
                                                   "line" (1- (car ref-pos))
                                                   "character" (1- (cdr ref-pos)))
                                        "end" (map:make-map
                                                "line" (1- (car ref-pos))
                                                "character" (+ (1- (cdr ref-pos))
                                                                (length (analysis:symbol-info-name ref))))))))
                        references))
              '()))
        '())))

;;; Evaluation Support

(defun ensure-evaluation-service (server)
  "Ensure the server has an evaluation service"
  (or (lsp-server-evaluation-service server)
      (let ((service (make-instance 'epsilon.lsp.evaluation.api:evaluation-service)))
        (epsilon.lsp.evaluation.api:start-evaluation-service service)
        (setf (lsp-server-evaluation-service server) service)
        service)))

(defun handle-evaluation-create-session (server params)
  "Handle epsilon/evaluation/createSession request"
  (let ((service (ensure-evaluation-service server))
        (name (map:get params "name"))
        (modules (map:get params "modules"))
        (restrictions (map:get params "restrictions")))
    (epsilon.lsp.evaluation.api:create-evaluation-session 
     service
     :name name
     :owner (get-workspace-id server)
     :modules modules
     :restrictions (when restrictions
                     (epsilon.lsp.evaluation.session:make-restrictions
                      :max-memory (map:get restrictions "maxMemory")
                      :max-cpu-time (map:get restrictions "maxCpuTime")
                      :allow-file-read (map:get restrictions "allowFileRead")
                      :allow-file-write (map:get restrictions "allowFileWrite")
                      :allow-network (map:get restrictions "allowNetwork"))))))

(defun handle-evaluation-list-sessions (server params)
  "Handle epsilon/evaluation/listSessions request"
  (let ((service (ensure-evaluation-service server)))
    (epsilon.lsp.evaluation.api:list-evaluation-sessions 
     service 
     :owner (get-workspace-id server))))

(defun handle-evaluation-terminate-session (server params)
  "Handle epsilon/evaluation/terminateSession request"
  (let ((service (ensure-evaluation-service server))
        (session-id (map:get params "sessionId")))
    (epsilon.lsp.evaluation.api:terminate-evaluation-session service session-id)
    (map:make-map "status" "terminated")))

(defun handle-evaluation-evaluate (server params)
  "Handle epsilon/evaluation/evaluate request"
  (let ((service (ensure-evaluation-service server))
        (session-id (map:get params "sessionId"))
        (code (map:get params "code"))
        (timeout (map:get params "timeout")))
    (if session-id
        (epsilon.lsp.evaluation.api:evaluate service session-id code :timeout timeout)
        (epsilon.lsp.evaluation.api:evaluate-in-new-session 
         service code
         :modules (map:get params "modules")
         :owner (get-workspace-id server)))))

(defun get-workspace-id (server)
  "Get the current workspace ID"
  (or (first (lsp-server-workspace-folders server))
      "default"))

;;; Enhanced Main Entry Point

(defun main ()
  "Main entry point for LSP server."
  (let ((server (start-lsp-server :protocol :json-rpc)))
    (format t "Enhanced LSP server started. Ready for communication via stdio.~%")
    (format t "Supported features: definition, hover, completion, diagnostics, symbols, references, evaluation~%")
    
    ;; Main message loop
    (loop
      (handler-case
          (let ((message (protocol:read-message 
                          (lsp-server-protocol-handler server) 
                          *standard-input*)))
            (handle-lsp-message server message))
        (end-of-file ()
          (format t "Client disconnected.~%")
          ;; Clean up evaluation service
          (when (lsp-server-evaluation-service server)
            (epsilon.lsp.evaluation.api:stop-evaluation-service 
             (lsp-server-evaluation-service server)))
          (return))
        (error (e)
          (format t "Error handling message: ~A~%" e))))))