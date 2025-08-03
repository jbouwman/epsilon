;;;; WebSocket Integration Tests

(defpackage epsilon.websocket.integration.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (ws epsilon.websocket)
   (thread epsilon.sys.thread)
   (str epsilon.string)))

(in-package epsilon.websocket.integration.tests)

(deftest echo-server-test
  "Test complete echo server functionality"
  (let ((server (ws:make-server :host "localhost" :port 9001))
        (received-messages '())
        (connection-opened nil)
        (connection-closed nil))
    
    ;; Register echo handler
    (ws:register-handler server "/" 
                        (ws:make-websocket-handler
                         :on-connect (lambda (conn)
                                      (declare (ignore conn))
                                      (setf connection-opened t))
                         :on-message (lambda (conn message is-text)
                                      (if is-text
                                          (ws:send-text conn (format nil "Echo: ~A" message))
                                          (ws:send-binary conn message))
                                      (push message received-messages))
                         :on-close (lambda (conn code reason)
                                    (declare (ignore conn code reason))
                                    (setf connection-closed t))))
    
    ;; Start server
    (ws:start-server server)
    
    (unwind-protect
         (progn
           ;; Give server time to start
           (sleep 0.1)
           
           ;; Test text message echo
           (let ((result (ws:echo-client "ws://localhost:9001/" "Hello World")))
             (is-equal result "Echo: Hello World"))
           
           ;; Verify handler was called
           (is connection-opened)
           (is (member "Hello World" received-messages :test #'string=)))
      
      ;; Cleanup
      (ws:stop-server server))))

(deftest client-server-binary-communication
  "Test binary message communication"
  (let ((server (ws:make-server :host "localhost" :port 9002))
        (received-data nil))
    
    ;; Register binary echo handler
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :on-message (lambda (conn message is-text)
                                      (declare (ignore is-text))
                                      (setf received-data message)
                                      (ws:send-binary conn message))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Test binary data
           (let ((test-data #(1 2 3 4 5 255 254 253)))
             (ws:send-binary-message "ws://localhost:9002/" test-data)
             
             ;; Give time for message processing
             (sleep 0.1)
             
             (is-equalp received-data test-data)))
      
      (ws:stop-server server))))

(deftest ping-pong-functionality
  "Test ping/pong mechanism"
  (let ((server (ws:make-server :host "localhost" :port 9003))
        (ping-received nil)
        (pong-received nil))
    
    ;; Register handler that tracks ping/pong
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :on-ping (lambda (conn payload)
                                   (declare (ignore conn))
                                   (setf ping-received payload))
                         :on-pong (lambda (conn payload)
                                   (declare (ignore conn))
                                   (setf pong-received payload))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Connect and send ping
           (ws:with-websocket-connection (conn "ws://localhost:9003/")
             (ws:send-ping conn "ping-data")
             
             ;; Give time for ping/pong exchange
             (sleep 0.1)
             
             (is-equalp ping-received (str:string-to-octets "ping-data"))))
      
      (ws:stop-server server))))

(deftest connection-close-handshake
  "Test proper connection close handshake"
  (let ((server (ws:make-server :host "localhost" :port 9004))
        (close-code nil)
        (close-reason nil))
    
    ;; Register handler that tracks close
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :on-close (lambda (conn code reason)
                                    (declare (ignore conn))
                                    (setf close-code code
                                          close-reason reason))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Connect and close with specific code
           (let ((conn (ws:connect "ws://localhost:9004/")))
             (ws:close-connection conn ws:+close-normal+ "Test close")
             (ws:wait-for-close conn)
             
             ;; Give time for server to process close
             (sleep 0.1)
             
             (is-= close-code ws:+close-normal+)
             (is-equal close-reason "Test close")))
      
      (ws:stop-server server))))

(deftest multiple-client-connections
  "Test server handling multiple simultaneous clients"
  (let ((server (ws:make-server :host "localhost" :port 9005))
        (client-count 0)
        (messages-received '()))
    
    ;; Register handler that counts clients and messages
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :on-connect (lambda (conn)
                                      (declare (ignore conn))
                                      (incf client-count))
                         :on-message (lambda (conn message is-text)
                                      (declare (ignore conn is-text))
                                      (push message messages-received))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Connect multiple clients
           (let ((connections '()))
             (dotimes (i 3)
               (push (ws:connect "ws://localhost:9005/") connections))
             
             ;; Verify all clients connected
             (is-= client-count 3)
             
             ;; Send message from each client
             (dolist (conn connections)
               (ws:send-text conn "Hello from client"))
             
             ;; Give time for message processing
             (sleep 0.1)
             
             ;; Verify all messages received
             (is-= (length messages-received) 3)
             
             ;; Close all connections
             (dolist (conn connections)
               (ws:close-connection conn)
               (ws:wait-for-close conn))))
      
      (ws:stop-server server))))

(deftest broadcast-functionality
  "Test server broadcasting to multiple clients"
  (let ((server (ws:make-server :host "localhost" :port 9006))
        (received-messages (make-hash-table)))
    
    ;; Register handler
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :on-connect (lambda (conn)
                                      (setf (gethash conn received-messages) '()))
                         :on-message (lambda (conn message is-text)
                                      (declare (ignore is-text))
                                      (push message (gethash conn received-messages)))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Connect multiple clients
           (let ((connections '()))
             (dotimes (i 3)
               (push (ws:connect "ws://localhost:9006/") connections))
             
             (sleep 0.1)
             
             ;; Broadcast message to all clients
             (ws:broadcast-text server "Broadcast message")
             
             ;; Give time for message processing
             (sleep 0.1)
             
             ;; Verify all clients received the broadcast
             (dolist (conn connections)
               (let ((messages (gethash conn received-messages)))
                 (is (member "Broadcast message" messages :test #'string=))))
             
             ;; Close connections
             (dolist (conn connections)
               (ws:close-connection conn)
               (ws:wait-for-close conn))))
      
      (ws:stop-server server))))

(deftest fragmented-message-handling
  "Test handling of fragmented messages"
  (skip "Fragmented message support not yet implemented"))

(deftest large-message-handling
  "Test handling of large messages"
  (let ((server (ws:make-server :host "localhost" :port 9007))
        (received-message nil))
    
    ;; Register handler that echoes large messages
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :on-message (lambda (conn message is-text)
                                      (setf received-message message)
                                      (if is-text
                                          (ws:send-text conn message)
                                          (ws:send-binary conn message)))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Test large text message (> 65535 bytes)
           (let* ((large-text (make-string 100000 :initial-element #\A))
                  (result (ws:echo-client "ws://localhost:9007/" large-text)))
             
             (is-equal result large-text)
             (is-equal received-message large-text)))
      
      (ws:stop-server server))))

(deftest connection-timeout-handling
  "Test connection timeout scenarios"
  (skip "Timeout handling not yet implemented"))

(deftest subprotocol-negotiation-integration
  "Test subprotocol negotiation in full client-server scenario"
  (let ((server (ws:make-server :host "localhost" :port 9008))
        (negotiated-protocol nil))
    
    ;; Register handler that supports specific subprotocols
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :subprotocols '("chat" "echo")
                         :on-connect (lambda (conn)
                                      (setf negotiated-protocol 
                                            (ws:connection-subprotocol conn)))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Connect with subprotocols
           (let* ((options (ws:make-client-options :subprotocols '("superchat" "chat")))
                  (conn (ws:connect "ws://localhost:9008/" :options options)))
             
             (sleep 0.1)
             
             ;; Verify correct subprotocol was negotiated
             (is-equal negotiated-protocol "chat")
             (is-equal (ws:connection-subprotocol conn) "chat")
             
             (ws:close-connection conn)
             (ws:wait-for-close conn)))
      
      (ws:stop-server server))))

(deftest error-handling-integration
  "Test error handling in client-server communication"
  (let ((server (ws:make-server :host "localhost" :port 9009))
        (error-occurred nil))
    
    ;; Register handler that generates errors
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :on-message (lambda (conn message is-text)
                                      (declare (ignore is-text))
                                      (when (string= message "ERROR")
                                        (error "Intentional test error"))
                                      (ws:send-text conn message))
                         :on-error (lambda (conn error)
                                    (declare (ignore conn error))
                                    (setf error-occurred t))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Connect and send error-triggering message
           (let ((conn (ws:connect "ws://localhost:9009/")))
             (ws:send-text conn "ERROR")
             
             ;; Give time for error processing
             (sleep 0.1)
             
             ;; Verify error was handled
             (is error-occurred)
             
             ;; Connection should be closed due to error
             (sleep 0.1)
             (is (ws:connection-closed-p conn))))
      
      (ws:stop-server server))))

(deftest concurrent-stress-test
  "Stress test with many concurrent connections and messages"
  (let ((server (ws:make-server :host "localhost" :port 9010))
        (message-count 0)
        (connection-count 0))
    
    ;; Register handler that counts messages and connections
    (ws:register-handler server "/"
                        (ws:make-websocket-handler
                         :on-connect (lambda (conn)
                                      (declare (ignore conn))
                                      (incf connection-count))
                         :on-message (lambda (conn message is-text)
                                      (declare (ignore is-text))
                                      (incf message-count)
                                      (ws:send-text conn (format nil "Echo: ~A" message)))))
    
    (ws:start-server server)
    
    (unwind-protect
         (progn
           (sleep 0.1)
           
           ;; Create multiple concurrent connections
           (let ((threads '()))
             (dotimes (i 5)
               (push (thread:make-thread
                     (lambda ()
                       (let ((conn (ws:connect "ws://localhost:9010/")))
                         ;; Send multiple messages
                         (dotimes (j 10)
                           (ws:send-text conn (format nil "Message ~D from thread ~D" j i)))
                         (ws:close-connection conn)
                         (ws:wait-for-close conn))))
                     threads))
             
             ;; Wait for all threads to complete
             (dolist (thread threads)
               (thread:join-thread thread))
             
             ;; Give server time to process
             (sleep 0.5)
             
             ;; Verify expected counts
             (is-= connection-count 5)
             (is-= message-count 50)))
      
      (ws:stop-server server))))
