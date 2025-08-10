;;;; Tests for LSP Evaluation System

(defpackage #:epsilon.lsp.evaluation.tests
  (:use #:common-lisp #:epsilon.test)
  (:local-nicknames
   (#:api #:epsilon.lsp.evaluation.api)
   (#:session #:epsilon.lsp.evaluation.session)
   (#:protocol #:epsilon.lsp.evaluation.protocol)
   (#:map #:epsilon.map)))

(in-package #:epsilon.lsp.evaluation.tests)

;;; Session Management Tests

(deftest test-create-session
  "Test creating an evaluation session"
  (let ((service (api:make-evaluation-service)))
    (api:start-evaluation-service service)
    
    (unwind-protect
         (let ((session-info (api:create-evaluation-session 
                              service
                              :name "test-session"
                              :owner "test-owner")))
           (is (map:get session-info "sessionId"))
           (is-equal "test-session" (map:get session-info "name"))
           (is-equal "active" (map:get session-info "state")))
      
      (api:stop-evaluation-service service))))

(deftest test-list-sessions
  "Test listing evaluation sessions"
  (let ((service (api:make-evaluation-service)))
    (api:start-evaluation-service service)
    
    (unwind-protect
         (progn
           ;; Initially empty
           (is-equal 0 (length (api:list-evaluation-sessions service)))
           
           ;; Create some sessions
           (api:create-evaluation-session service :name "session1" :owner "owner1")
           (api:create-evaluation-session service :name "session2" :owner "owner1")
           (api:create-evaluation-session service :name "session3" :owner "owner2")
           
           ;; List all
           (is-equal 3 (length (api:list-evaluation-sessions service)))
           
           ;; List by owner
           (is-equal 2 (length (api:list-evaluation-sessions service :owner "owner1")))
           (is-equal 1 (length (api:list-evaluation-sessions service :owner "owner2"))))
      
      (api:stop-evaluation-service service))))

(deftest test-terminate-session
  "Test terminating a session"
  (let ((service (api:make-evaluation-service)))
    (api:start-evaluation-service service)
    
    (unwind-protect
         (let* ((session-info (api:create-evaluation-session 
                               service
                               :name "temp-session"))
                (session-id (map:get session-info "sessionId")))
           
           ;; Session exists
           (is (api:get-evaluation-session service session-id))
           
           ;; Terminate it
           (api:terminate-evaluation-session service session-id)
           
           ;; Session no longer exists
           (is-not (api:get-evaluation-session service session-id)))
      
      (api:stop-evaluation-service service))))

;;; Evaluation Tests

(deftest test-simple-evaluation
  "Test simple code evaluation"
  (let ((service (api:make-evaluation-service)))
    (api:start-evaluation-service service)
    
    (unwind-protect
         (let* ((session-info (api:create-evaluation-session service))
                (session-id (map:get session-info "sessionId")))
           
           ;; Simple arithmetic
           (let ((result (api:evaluate service session-id "(+ 1 2)")))
             (is-equal "3" (protocol:evaluation-result-value result)))
           
           ;; String operation
           (let ((result (api:evaluate service session-id 
                                       "(string-upcase \"hello\")")))
             (is-equal "\"HELLO\"" (protocol:evaluation-result-value result))))
      
      (api:stop-evaluation-service service))))

(deftest test-stateful-evaluation
  "Test that sessions maintain state"
  (let ((service (api:make-evaluation-service)))
    (api:start-evaluation-service service)
    
    (unwind-protect
         (let* ((session-info (api:create-evaluation-session service))
                (session-id (map:get session-info "sessionId")))
           
           ;; Define a variable
           (api:evaluate service session-id "(defparameter *test-var* 42)")
           
           ;; Reference it in next evaluation
           (let ((result (api:evaluate service session-id "*test-var*")))
             (is-equal "42" (protocol:evaluation-result-value result)))
           
           ;; Modify it
           (api:evaluate service session-id "(setf *test-var* 100)")
           
           ;; Check new value
           (let ((result (api:evaluate service session-id "*test-var*")))
             (is-equal "100" (protocol:evaluation-result-value result))))
      
      (api:stop-evaluation-service service))))

(deftest test-evaluation-error-handling
  "Test error handling in evaluation"
  (let ((service (api:make-evaluation-service)))
    (api:start-evaluation-service service)
    
    (unwind-protect
         (let* ((session-info (api:create-evaluation-session service))
                (session-id (map:get session-info "sessionId")))
           
           ;; Division by zero
           (is-thrown (error)
             (api:evaluate service session-id "(/ 1 0)"))
           
           ;; Undefined variable
           (is-thrown (error)
             (api:evaluate service session-id "*undefined-var*"))
           
           ;; Syntax error
           (is-thrown (error)
             (api:evaluate service session-id "(+ 1 2")))
      
      (api:stop-evaluation-service service))))

(deftest test-ephemeral-evaluation
  "Test evaluation without explicit session"
  (let ((service (api:make-evaluation-service)))
    (api:start-evaluation-service service)
    
    (unwind-protect
         (let ((result (api:evaluate-in-new-session 
                        service 
                        "(* 7 6)")))
           (is-equal "42" (protocol:evaluation-result-value result)))
      
      (api:stop-evaluation-service service))))

;;; Protocol Tests

(deftest test-protocol-messages
  "Test protocol message creation and parsing"
  ;; Request creation
  (let ((request (protocol:make-request "test-method" 
                                        :params (map:make-map "foo" "bar")
                                        :id "test-123")))
    (is-equal protocol:+message-type-request+ (protocol:message-type request))
    (is-equal "test-method" (protocol:message-method request))
    (is-equal "test-123" (protocol:message-id request))
    (is-equal "bar" (map:get (protocol:message-params request) "foo")))
  
  ;; Response creation
  (let ((response (protocol:make-response "test-123" 
                                          (map:make-map "result" "ok"))))
    (is-equal protocol:+message-type-response+ (protocol:message-type response))
    (is-equal "test-123" (protocol:message-id response))
    (is-equal "ok" (map:get (protocol:message-result response) "result")))
  
  ;; Error response
  (let ((error-resp (protocol:make-error-response 
                     "test-123"
                     protocol:+error-evaluation-error+
                     "Something went wrong")))
    (is-equal protocol:+message-type-error+ (protocol:message-type error-resp))
    (is-equal "test-123" (protocol:message-id error-resp))
    (let ((error-info (protocol:message-error error-resp)))
      (is-equal protocol:+error-evaluation-error+ (map:get error-info "code"))
      (is-equal "Something went wrong" (map:get error-info "message")))))

;;; Security Tests

(deftest test-session-restrictions
  "Test security restrictions"
  (let ((restrictions (session:make-restrictions
                       :max-memory "128M"
                       :max-cpu-time 10
                       :allow-file-read nil
                       :allow-file-write nil
                       :allow-network nil)))
    
    (is-equal "128M" (session:restriction-max-memory restrictions))
    (is-equal 10 (session:restriction-max-cpu-time restrictions))
    (is-not (session:restriction-allow-file-read restrictions))
    (is-not (session:restriction-allow-file-write restrictions))
    (is-not (session:restriction-allow-network restrictions))))

(deftest test-session-limits
  "Test session creation limits"
  (let ((service (api:make-evaluation-service))
        (manager (api:service-session-manager service)))
    ;; Set low limit for testing
    (setf (slot-value manager 'session:max-sessions-per-client) 2)
    
    (api:start-evaluation-service service)
    
    (unwind-protect
         (progn
           ;; Create max sessions
           (api:create-evaluation-session service :owner "test-client")
           (api:create-evaluation-session service :owner "test-client")
           
           ;; Should fail on third
           (is-thrown (error)
             (api:create-evaluation-session service :owner "test-client"))
           
           ;; Different owner should work
           (api:create-evaluation-session service :owner "other-client"))
      
      (api:stop-evaluation-service service))))
