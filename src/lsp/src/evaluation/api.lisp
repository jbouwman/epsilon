;;;; Evaluation API
;;;;
;;;; This module provides the high-level API for code evaluation within
;;;; the LSP server. It manages sessions and handles evaluation requests.

(defpackage #:epsilon.lsp.evaluation.api
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:session #:epsilon.lsp.evaluation.session)
   (#:protocol #:epsilon.lsp.evaluation.protocol)
   (#:json #:epsilon.json)
   (#:thread #:epsilon.sys.thread))
  (:export
   ;; Main API
   #:evaluation-service
   #:service-session-manager
   #:make-evaluation-service
   #:start-evaluation-service
   #:stop-evaluation-service
   
   ;; Session operations
   #:create-evaluation-session
   #:get-evaluation-session
   #:list-evaluation-sessions
   #:terminate-evaluation-session
   
   ;; Evaluation operations
   #:evaluate
   #:evaluate-async
   #:complete-symbol
   #:inspect-object
   
   ;; Convenience functions
   #:evaluate-in-new-session
   #:with-evaluation-session))

(in-package #:epsilon.lsp.evaluation.api)

;;; Evaluation Service

(defclass evaluation-service ()
  ((session-manager :initform (make-instance 'session:session-manager)
                    :reader service-session-manager)
   (default-modules :initarg :default-modules
                    :initform '("epsilon.core")
                    :reader default-modules)
   (started-p :initform nil
              :accessor service-started-p)))

(defun make-evaluation-service (&key default-modules)
  "Create a new evaluation service"
  (make-instance 'evaluation-service
                 :default-modules (or default-modules '("epsilon.core"))))

(defmethod start-evaluation-service ((service evaluation-service))
  "Start the evaluation service"
  (setf (service-started-p service) t)
  service)

(defmethod stop-evaluation-service ((service evaluation-service))
  "Stop the evaluation service and terminate all sessions"
  (when (service-started-p service)
    ;; Terminate all sessions
    (dolist (session (session:list-sessions (service-session-manager service)))
      (session:terminate-session (service-session-manager service)
                                 (session:session-id session)))
    (setf (service-started-p service) nil))
  service)

;;; Session Operations

(defmethod create-evaluation-session ((service evaluation-service)
                                      &key name owner modules restrictions)
  "Create a new evaluation session"
  (unless (service-started-p service)
    (error "Evaluation service not started"))
  
  (let ((session (session:create-session 
                  (service-session-manager service)
                  :name name
                  :owner owner
                  :modules (or modules (default-modules service))
                  :restrictions restrictions)))
    
    ;; Return session info
    (map:make-map
     "sessionId" (session:session-id session)
     "name" (session:session-name session)
     "state" (string-downcase (symbol-name (session:session-state session)))
     "modules" (session:session-modules session)
     "createdAt" (session:session-created-at session))))

(defmethod get-evaluation-session ((service evaluation-service) session-id)
  "Get information about a session"
  (let ((session (session:get-session (service-session-manager service) 
                                      session-id)))
    (when session
      (map:make-map
       "sessionId" (session:session-id session)
       "name" (session:session-name session)
       "state" (string-downcase (symbol-name (session:session-state session)))
       "modules" (session:session-modules session)
       "createdAt" (session:session-created-at session)
       "lastUsed" (session:session-last-used session)))))

(defmethod list-evaluation-sessions ((service evaluation-service) &key owner)
  "List all sessions, optionally filtered by owner"
  (mapcar (lambda (session)
            (map:make-map
             "sessionId" (session:session-id session)
             "name" (session:session-name session)
             "state" (string-downcase (symbol-name (session:session-state session)))
             "owner" (session:session-owner session)
             "lastUsed" (session:session-last-used session)))
          (session:list-sessions (service-session-manager service) :owner owner)))

(defmethod terminate-evaluation-session ((service evaluation-service) session-id)
  "Terminate a session"
  (session:terminate-session (service-session-manager service) session-id))

;;; Evaluation Operations

(defmethod evaluate ((service evaluation-service) session-id code &key timeout)
  "Evaluate code in a session and return the result"
  (let ((session (session:get-session (service-session-manager service) 
                                      session-id)))
    (unless session
      (error "Session not found: ~A" session-id))
    
    ;; Update last-used timestamp
    (setf (session:session-last-used session) (get-universal-time))
    
    ;; Create evaluation request
    (let* ((request-id (protocol:message-id 
                        (protocol:make-evaluate-request code :timeout timeout)))
           (request (protocol:make-evaluate-request code :timeout timeout))
           (start-time (get-internal-real-time)))
      
      ;; Send request
      (session:write-message-to-session session request)
      
      ;; Wait for response
      (let ((response (session:read-message-from-session 
                       session 
                       :timeout (or timeout 30))))
        (unless response
          (error "Evaluation timeout"))
        
        ;; Handle response
        (cond
          ((string= (protocol:message-type response) 
                    protocol:+message-type-response+)
           (let* ((result (protocol:message-result response))
                  (end-time (get-internal-real-time))
                  (duration (/ (- end-time start-time) 
                               internal-time-units-per-second)))
             ;; Add duration if not already present
             (unless (protocol:evaluation-result-duration result)
               (setf result (map:assoc result "duration" duration)))
             result))
          
          ((string= (protocol:message-type response)
                    protocol:+message-type-error+)
           (let ((error-info (protocol:message-error response)))
             (error "Evaluation error: ~A" 
                    (map:get error-info "message"))))
          
          (t
           (error "Unexpected response type: ~A" 
                  (protocol:message-type response))))))))

(defmethod evaluate-async ((service evaluation-service) session-id code 
                           &key timeout callback)
  "Evaluate code asynchronously, calling callback with the result"
  (thread:make-thread
   (lambda ()
     (handler-case
         (let ((result (evaluate service session-id code :timeout timeout)))
           (funcall callback :success result))
       (error (e)
         (funcall callback :error (format nil "~A" e)))))
   :name "async-evaluation"))

(defmethod complete-symbol ((service evaluation-service) session-id prefix 
                            &key context)
  "Get symbol completions in a session"
  (let ((session (session:get-session (service-session-manager service) 
                                      session-id)))
    (unless session
      (error "Session not found: ~A" session-id))
    
    ;; Send completion request
    (let ((request (protocol:make-complete-request prefix :context context)))
      (session:write-message-to-session session request)
      
      ;; Wait for response
      (let ((response (session:read-message-from-session session :timeout 5)))
        (unless response
          (error "Completion timeout"))
        
        (if (string= (protocol:message-type response) 
                     protocol:+message-type-response+)
            (protocol:message-result response)
            (error "Completion failed"))))))

(defmethod inspect-object ((service evaluation-service) session-id object 
                           &key verbose)
  "Inspect an object in a session"
  (let ((session (session:get-session (service-session-manager service) 
                                      session-id)))
    (unless session
      (error "Session not found: ~A" session-id))
    
    ;; Send inspection request
    (let ((request (protocol:make-inspect-request object :verbose verbose)))
      (session:write-message-to-session session request)
      
      ;; Wait for response
      (let ((response (session:read-message-from-session session :timeout 5)))
        (unless response
          (error "Inspection timeout"))
        
        (if (string= (protocol:message-type response) 
                     protocol:+message-type-response+)
            (protocol:message-result response)
            (error "Inspection failed"))))))

;;; Convenience Functions

(defun evaluate-in-new-session (service code &key modules owner)
  "Create a temporary session, evaluate code, and return the result"
  (let* ((session-info (create-evaluation-session 
                        service
                        :name "ephemeral"
                        :owner owner
                        :modules modules))
         (session-id (map:get session-info "sessionId")))
    (unwind-protect
         (evaluate service session-id code)
      (terminate-evaluation-session service session-id))))

(defmacro with-evaluation-session ((session-var service &key name modules owner)
                                   &body body)
  "Execute body with a session, ensuring cleanup"
  (let ((session-id-var (gensym "SESSION-ID"))
        (service-var (gensym "SERVICE")))
    `(let* ((,service-var ,service)
            (,session-var (create-evaluation-session 
                           ,service-var
                           :name ,name
                           :modules ,modules
                           :owner ,owner))
            (,session-id-var (map:get ,session-var "sessionId")))
       (unwind-protect
            (progn ,@body)
         (terminate-evaluation-session ,service-var ,session-id-var)))))
