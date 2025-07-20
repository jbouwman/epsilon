;;;; Language Server Protocol (LSP) Server Implementation
;;;;
;;;; LSP 3.17 server implementation for Common Lisp development
;;;; environments.

(defpackage #:epsilon.lsp.server
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:json #:epsilon.json)
   (#:protocol #:epsilon.lsp.protocol)
   (#:jsonrpc #:epsilon.lsp.protocol.jsonrpc)
   (#:thread #:epsilon.sys.thread)
   (#:analysis #:epsilon.lsp.analysis)
   (#:workspace #:epsilon.lsp.workspace)
   (#:str #:epsilon.string))
  (:export
   #:lsp-server
   #:start-lsp-server
   #:stop-lsp-server
   #:main))

(in-package #:epsilon.lsp.server)

(defstruct lsp-server
  "LSP server state."
  host
  port
  protocol-handler
  tcp-server
  initialized-p
  client-capabilities
  workspace-folders
  workspace                  ; Workspace instance for symbol management
  open-documents
  evaluation-service)        ; Evaluation service for code execution

(defvar *current-server* nil
  "The currently running LSP server.")

;;; LSP Server Lifecycle

(defun start-lsp-server (&key (host "127.0.0.1") (port 7888) (protocol :json-rpc))
  "Start the LSP server."
  (let* ((protocol-handler (case protocol
                             (:json-rpc (protocol:make-jsonrpc-handler))
                             (t (error "Unsupported protocol: ~A" protocol))))
         (server (make-lsp-server
                  :host host
                  :port port
                  :protocol-handler protocol-handler
                  :initialized-p nil
                  :workspace (workspace:make-workspace)
                  :open-documents (map:make-map))))
    
    ;; Start simple TCP server for LSP communication
    ;; For now, just create a simple stdio-based server
    (format t "LSP server would start on ~A:~A (stdio mode for now)~%" host port)
    
    (setf *current-server* server)
    server))

(defun stop-lsp-server (server)
  "Stop the LSP server."
  (when (eq server *current-server*)
    (setf *current-server* nil))
  (format t "LSP server stopped~%")
  t)

;;; Request Handling (simplified for stdio)

(defun handle-lsp-message (server message)
  "Handle a parsed LSP message."
  (cond
    ((jsonrpc:jsonrpc-request-p message)
     (handle-lsp-method server message))
    
    ((jsonrpc:jsonrpc-notification-p message)
     (handle-lsp-notification server message))
    
    (t 
     (let ((error-response 
            (protocol:make-error-response 
             (lsp-server-protocol-handler server)
             nil
             jsonrpc:+invalid-request+
             "Invalid message type")))
       (protocol:write-message 
        (lsp-server-protocol-handler server)
        error-response 
        *standard-output*)))))

;;; LSP Method Handlers

(defun handle-lsp-method (server request)
  "Handle an LSP method request."
  (let* ((method (jsonrpc:jsonrpc-request-method request))
         (params (jsonrpc:jsonrpc-request-params request))
         (id (jsonrpc:jsonrpc-request-id request))
         (handler (lsp-server-protocol-handler server)))
    
    (handler-case
        (let ((result (dispatch-method server method params)))
          (let ((response (protocol:make-response handler id result)))
            (protocol:write-message handler response *standard-output*)))
      (error (e)
        (let ((error-response 
               (protocol:make-error-response 
                handler id jsonrpc:+internal-error+ 
                (format nil "Internal error: ~A" e))))
          (protocol:write-message handler error-response *standard-output*))))))

(defun handle-lsp-notification (server notification)
  "Handle an LSP notification."
  (let ((method (jsonrpc:jsonrpc-notification-method notification))
        (params (jsonrpc:jsonrpc-notification-params notification)))
    (dispatch-notification server method params)))

;;; Method Dispatch

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
    
    (t 
     (error "Method not found: ~A" method))))

(defun dispatch-notification (server method params)
  "Dispatch an LSP notification to its handler."
  (cond
    ((string= method "initialized")
     (handle-initialized server params))
    
    ((string= method "exit")
     (handle-exit server params))
    
    ((string= method "textDocument/didOpen")
     (handle-did-open server params))
    
    ((string= method "textDocument/didChange")
     (handle-did-change server params))
    
    ((string= method "textDocument/didClose")
     (handle-did-close server params))
    
    (t 
     (format t "Unknown notification: ~A~%" method))))

;;; LSP Method Implementations

(defun handle-initialize (server params)
  "Handle the initialize request."
  (setf (lsp-server-client-capabilities server) 
        (map:get params "capabilities"))
  
  (setf (lsp-server-workspace-folders server)
        (map:get params "workspaceFolders"))
  
  ;; Return server capabilities
  (map:make-map
   "capabilities" (map:make-map
                   "textDocumentSync" 1  ; Full document sync
                   "hoverProvider" t
                   "completionProvider" (map:make-map "triggerCharacters" '("." ":"))
                   "definitionProvider" t
                   "referencesProvider" t
                   "documentSymbolProvider" t
                   "workspaceSymbolProvider" t)))

(defun handle-shutdown (server params)
  "Handle the shutdown request."
  (declare (ignore server params))
  'null)

(defun handle-initialized (server params)
  "Handle the initialized notification."
  (declare (ignore params))
  (setf (lsp-server-initialized-p server) t)
  (format t "LSP server initialized~%"))

(defun handle-exit (server params)
  "Handle the exit notification."
  (declare (ignore params))
  (stop-lsp-server server))

;;; Text Document Synchronization

(defun handle-did-open (server params)
  "Handle textDocument/didOpen notification."
  (let* ((text-document (map:get params "textDocument"))
         (uri (map:get text-document "uri"))
         (text (map:get text-document "text")))
    
    ;; Store in open documents
    (setf (lsp-server-open-documents server)
          (map:put (lsp-server-open-documents server) uri text))
    
    ;; Add to workspace for analysis
    (workspace:workspace-add-document (lsp-server-workspace server) uri text)
    
    ;; Send diagnostics
    (send-diagnostics server uri)
    
    (format t "Opened document: ~A~%" uri)))

(defun handle-did-change (server params)
  "Handle textDocument/didChange notification."
  (let* ((text-document (map:get params "textDocument"))
         (uri (map:get text-document "uri"))
         (changes (map:get params "contentChanges")))
    
    ;; For full document sync, we just replace the entire content
    (when changes
      (let ((new-text (map:get (first changes) "text")))
        ;; Update open documents
        (setf (lsp-server-open-documents server)
              (map:put (lsp-server-open-documents server) uri new-text))
        
        ;; Update workspace analysis
        (workspace:workspace-update-document (lsp-server-workspace server) uri new-text)
        
        ;; Send updated diagnostics
        (send-diagnostics server uri)))
    
    (format t "Changed document: ~A~%" uri)))

(defun handle-did-close (server params)
  "Handle textDocument/didClose notification."
  (let* ((text-document (map:get params "textDocument"))
         (uri (map:get text-document "uri")))
    
    ;; Remove from open documents
    (setf (lsp-server-open-documents server)
          (map:remove (lsp-server-open-documents server) uri))
    
    ;; Remove from workspace
    (workspace:workspace-remove-document (lsp-server-workspace server) uri)
    
    (format t "Closed document: ~A~%" uri)))

;;; Language Features Implementation

(defun handle-definition (server params)
  "Handle textDocument/definition request."
  (let* ((text-document (map:get params "textDocument"))
         (uri (map:get text-document "uri"))
         (position (map:get params "position"))
         (line (1+ (map:get position "line")))  ; LSP is 0-based, we use 1-based
         (character (1+ (map:get position "character")))
         (doc-analysis (workspace:workspace-get-document 
                        (lsp-server-workspace server) uri)))
    
    (if doc-analysis
        (let* ((content (analysis:document-analysis-content doc-analysis))
               (offset (analysis:position-to-offset content line character))
               (symbol-at-pos (analysis:symbol-at-position doc-analysis (cons line character))))
          (if symbol-at-pos
              (let ((definition (analysis:find-definition 
                                 doc-analysis 
                                 (analysis:symbol-info-name symbol-at-pos)
                                 (cons line character))))
                (if definition
                    (let ((def-pos (analysis:symbol-info-position definition))
                          (def-range (analysis:symbol-info-range definition)))
                      (map:make-map
                       "uri" uri
                       "range" (map:make-map
                                 "start" (map:make-map
                                           "line" (1- (car def-pos))
                                           "character" (1- (cdr def-pos)))
                                 "end" (map:make-map
                                         "line" (1- (car (cdr def-range)))
                                         "character" (cdr (cdr def-range))))))
                    'null))
              'null))
        'null)))

(defun handle-hover (server params)
  "Handle textDocument/hover request."
  (let* ((text-document (map:get params "textDocument"))
         (uri (map:get text-document "uri"))
         (position (map:get params "position"))
         (line (1+ (map:get position "line")))
         (character (1+ (map:get position "character")))
         (doc-analysis (workspace:workspace-get-document 
                        (lsp-server-workspace server) uri)))
    
    (if doc-analysis
        (let ((hover-info (analysis:get-hover-info doc-analysis (cons line character))))
          (if hover-info
              (map:make-map
               "contents" (map:make-map
                           "kind" "markdown"
                           "value" hover-info))
              'null))
        'null)))

(defun handle-completion (server params)
  "Handle textDocument/completion request."
  (let* ((text-document (map:get params "textDocument"))
         (uri (map:get text-document "uri"))
         (position (map:get params "position"))
         (line (1+ (map:get position "line")))
         (character (1+ (map:get position "character")))
         (doc-analysis (workspace:workspace-get-document 
                        (lsp-server-workspace server) uri))
         (context (map:get params "context")))
    
    (if doc-analysis
        (let* ((content (analysis:document-analysis-content doc-analysis))
               (prefix (extract-completion-prefix content line character))
               (completions (analysis:get-completions doc-analysis 
                                                       (cons line character) 
                                                       prefix)))
          (map:make-map
           "isIncomplete" nil
           "items" completions))
        (map:make-map
         "isIncomplete" nil
         "items" '()))))

(defun extract-completion-prefix (content line character)
  "Extract the prefix for completion from the current position."
  (let* ((lines (str:split content #\Newline))
         (current-line (when (< (1- line) (length lines))
                         (nth (1- line) lines))))
    (if current-line
        (let* ((prefix-end (min (1- character) (length current-line)))
               (prefix-start (or (position-if (lambda (c)
                                                (or (char= c #\Space)
                                                    (char= c #\()
                                                    (char= c #\)))
                                              current-line
                                              :from-end t
                                              :end prefix-end)
                                 -1)))
          (subseq current-line (1+ prefix-start) prefix-end))
        "")))

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
