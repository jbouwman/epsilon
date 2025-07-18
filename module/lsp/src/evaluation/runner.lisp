;;;; Evaluation Runner
;;;;
;;;; This module runs in the subprocess and handles evaluation requests
;;;; from the parent LSP server. It provides a sandboxed environment for
;;;; code execution with security restrictions.

(defpackage #:epsilon.lsp.evaluation.runner
  (:use #:common-lisp)
  (:export #:main))

(in-package #:epsilon.lsp.evaluation.runner)

;;; Early bootstrap - define minimal dependencies

(defun read-json-line ()
  "Read a line and parse as JSON - minimal implementation"
  (let ((line (read-line *standard-input* nil nil)))
    (when line
      ;; After modules are loaded, use real JSON parser
      (if (find-package "EPSILON.LIB.JSON")
          (funcall (find-symbol "DECODE" "EPSILON.LIB.JSON") line)
          ;; Bootstrap: very basic alist parsing
          (parse-simple-json line)))))

(defun write-json-line (object)
  "Write object as JSON line - minimal implementation"
  (if (find-package "EPSILON.LIB.JSON")
      (progn
        (write-line (funcall (find-symbol "ENCODE" "EPSILON.LIB.JSON") object))
        (force-output))
      ;; Bootstrap: simple alist to JSON
      (progn
        (write-line (alist-to-json object))
        (force-output))))

(defun parse-simple-json (string)
  "Very simple JSON parser for bootstrap - handles basic objects only"
  ;; This is a hack for bootstrap - only handles simple flat objects
  (when (and (char= (char string 0) #\{)
             (char= (char string (1- (length string))) #\}))
    (let ((content (subseq string 1 (1- (length string))))
          (result '()))
      ;; Very naive parsing - won't handle nested objects or arrays
      (loop for pair in (split-string content #\,)
            do (let ((kv (split-string (string-trim " " pair) #\:)))
                 (when (= (length kv) 2)
                   (let ((key (string-trim "\" " (first kv)))
                         (val (string-trim "\" " (second kv))))
                     (push (cons key val) result)))))
      (nreverse result))))

(defun split-string (string delimiter)
  "Split string by delimiter character"
  (let ((result '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
          do (push (subseq string start i) result)
             (setf start (1+ i)))
    (push (subseq string start) result)
    (nreverse result)))

(defun alist-to-json (alist)
  "Convert alist to simple JSON string"
  (format nil "{~{~A~^,~}}"
          (mapcar (lambda (pair)
                    (format nil "\"~A\":\"~A\"" (car pair) (cdr pair)))
                  alist)))

;;; Global state

(defvar *session-modules* nil
  "Modules loaded in this session")

(defvar *session-restrictions* nil
  "Security restrictions for this session")

(defvar *session-bindings* (make-hash-table :test 'equal)
  "Dynamic bindings maintained across evaluations")

(defvar *evaluation-package* nil
  "Package for evaluations")

;;; Main entry point

(defun main ()
  "Main entry point for evaluation runner"
  (handler-case
      (progn
        ;; Disable debugger to prevent hangs
        (setf *debugger-hook* (lambda (c old-hook)
                                (declare (ignore old-hook))
                                (format *error-output* "Debugger invoked: ~A~%" c)
                                (abort)))
        
        ;; Wait for initialization
        (initialize-session)
        
        ;; Main message loop
        (loop
          (let ((message (read-message)))
            (unless message
              (return))
            (handle-message message))))
    
    (error (e)
      (format *error-output* "Runner error: ~A~%" e)
      (sb-ext:exit :code 1))))

;;; Initialization

(defun initialize-session ()
  "Initialize the evaluation session"
  (let ((init-msg (read-message)))
    (unless (and init-msg 
                 (string= (get-field init-msg "type") "request")
                 (string= (get-field init-msg "method") "init"))
      (error "Expected init request"))
    
    (let ((params (get-field init-msg "params")))
      ;; Load requested modules
      (setf *session-modules* (get-field params "modules"))
      (load-modules *session-modules*)
      
      ;; Set restrictions
      (setf *session-restrictions* (get-field params "restrictions"))
      (apply-restrictions *session-restrictions*)
      
      ;; Create evaluation package
      (setf *evaluation-package* (make-evaluation-package))
      
      ;; Send acknowledgment
      (write-message (make-response (get-field init-msg "id")
                                    '(("status" . "initialized")
                                      ("type" . "init-ack")))))))

(defun load-modules (modules)
  "Load requested modules"
  (dolist (module modules)
    (handler-case
        (require (intern (string-upcase module) :keyword))
      (error (e)
        (format *error-output* "Failed to load module ~A: ~A~%" module e)))))

(defun apply-restrictions (restrictions)
  "Apply security restrictions"
  ;; TODO: Implement actual restrictions
  ;; - File system access controls
  ;; - Network access controls
  ;; - CPU/memory limits
  ;; - Forbidden function list
  )

(defun make-evaluation-package ()
  "Create a package for evaluations"
  (let ((pkg (make-package "EPSILON-EVAL" :use '("COMMON-LISP"))))
    ;; Import symbols from loaded modules
    (dolist (module *session-modules*)
      (let ((module-pkg (find-package (string-upcase module))))
        (when module-pkg
          (do-external-symbols (sym module-pkg)
            (import sym pkg)))))
    pkg))

;;; Message handling

(defun handle-message (message)
  "Handle an incoming message"
  (let ((type (get-field message "type")))
    (cond
      ((string= type "request")
       (handle-request message))
      
      ((string= type "notification")
       (handle-notification message))
      
      (t
       (format *error-output* "Unknown message type: ~A~%" type)))))

(defun handle-request (request)
  "Handle a request message"
  (let ((method (get-field request "method"))
        (id (get-field request "id"))
        (params (get-field request "params")))
    
    (handler-case
        (cond
          ((string= method "evaluate")
           (handle-evaluate id params))
          
          ((string= method "complete")
           (handle-complete id params))
          
          ((string= method "inspect")
           (handle-inspect id params))
          
          ((string= method "shutdown")
           (handle-shutdown id)
           (sb-ext:exit :code 0))
          
          (t
           (write-message 
            (make-error-response id -32601 
                                 (format nil "Method not found: ~A" method)))))
      
      (error (e)
        (write-message 
         (make-error-response id -32603 
                              (format nil "Internal error: ~A" e)))))))

(defun handle-notification (notification)
  "Handle a notification message"
  ;; Currently no notifications to handle
  (declare (ignore notification)))

;;; Evaluation

(defun handle-evaluate (id params)
  "Handle an evaluation request"
  (let ((code (get-field params "code"))
        (timeout (or (get-field params "timeout") 30))
        (bindings (get-field params "bindings")))
    
    (multiple-value-bind (result output error-p)
        (safe-evaluate code :timeout timeout :bindings bindings)
      
      (if error-p
          (write-message 
           (make-error-response id -32001 "Evaluation error"
                                `(("error" . ,result)
                                  ("output" . ,output))))
          (write-message 
           (make-response id 
                          `(("value" . ,(format nil "~S" result))
                            ("output" . ,output)
                            ("type" . "evaluation-result"))))))))

(defun safe-evaluate (code &key timeout bindings)
  "Safely evaluate code with timeout and capture output"
  (let ((output-string (make-string-output-stream))
        (*standard-output* output-string)
        (*error-output* output-string)
        (*package* *evaluation-package*)
        (start-time (get-internal-real-time)))
    
    ;; Apply dynamic bindings
    (when bindings
      (apply-bindings bindings))
    
    (handler-case
        (let ((result (with-timeout timeout
                        (eval (read-from-string code)))))
          (values result 
                  (get-output-stream-string output-string)
                  nil))
      
      (error (e)
        (values (format nil "~A" e)
                (get-output-stream-string output-string)
                t)))))

(defmacro with-timeout (seconds &body body)
  "Execute body with a timeout"
  ;; Simple timeout implementation
  ;; TODO: Implement proper timeout with sb-ext:with-timeout
  `(progn ,@body))

(defun apply-bindings (bindings)
  "Apply dynamic bindings from previous evaluations"
  ;; TODO: Implement binding restoration
  )

;;; Completion

(defun handle-complete (id params)
  "Handle a completion request"
  (let ((prefix (get-field params "prefix"))
        (context (get-field params "context")))
    
    (declare (ignore context)) ; TODO: Use context
    
    (let ((completions (find-completions prefix)))
      (write-message 
       (make-response id 
                      `(("completions" . ,completions)))))))

(defun find-completions (prefix)
  "Find symbol completions for prefix"
  (let ((completions '())
        (prefix-upper (string-upcase prefix))
        (prefix-len (length prefix)))
    
    ;; Search in evaluation package
    (do-symbols (sym *evaluation-package*)
      (let ((name (symbol-name sym)))
        (when (and (>= (length name) prefix-len)
                   (string= prefix-upper name :end2 prefix-len))
          (push `(("label" . ,(string-downcase name))
                  ("kind" . ,(symbol-kind sym)))
                completions))))
    
    (nreverse completions)))

(defun symbol-kind (symbol)
  "Determine the kind of a symbol"
  (cond
    ((fboundp symbol) "function")
    ((macro-function symbol) "macro")
    ((boundp symbol) "variable")
    ((find-class symbol nil) "class")
    (t "symbol")))

;;; Inspection

(defun handle-inspect (id params)
  "Handle an inspection request"
  (let ((object-expr (get-field params "object"))
        (verbose (get-field params "verbose")))
    
    (handler-case
        (let* ((object (eval (read-from-string object-expr)))
               (inspection (inspect-object object verbose)))
          (write-message 
           (make-response id inspection)))
      
      (error (e)
        (write-message 
         (make-error-response id -32001 
                              (format nil "Inspection error: ~A" e)))))))

(defun inspect-object (object verbose)
  "Inspect an object and return information"
  `(("type" . ,(type-of object))
    ("class" . ,(class-name (class-of object)))
    ("string" . ,(format nil "~A" object))
    ("details" . ,(when verbose
                    (format nil "~S" (describe object nil))))))

;;; Shutdown

(defun handle-shutdown (id)
  "Handle shutdown request"
  (write-message (make-response id '(("status" . "shutting-down"))))
  (force-output)
  ;; Clean shutdown will be handled by caller
  )

;;; Message I/O (minimal implementation)

(defun read-message ()
  "Read a message from stdin"
  (read-json-line))

(defun write-message (message)
  "Write a message to stdout"
  (write-json-line message))

(defun get-field (alist key)
  "Get a field from an alist"
  (cdr (assoc key alist :test #'string=)))

(defun make-response (id result)
  "Make a response message"
  `(("type" . "response")
    ("id" . ,id)
    ("result" . ,result)))

(defun make-error-response (id code message &optional data)
  "Make an error response"
  `(("type" . "error")
    ("id" . ,id)
    ("error" . (("code" . ,code)
                ("message" . ,message)
                ,@(when data `(("data" . ,data)))))))