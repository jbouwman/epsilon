;;;; WebSocket Examples and Demo Applications
;;;;
;;;; This file contains example applications and usage patterns
;;;; for the WebSocket implementation.

(defpackage epsilon.websocket.examples
  (:use cl)
  (:local-nicknames
   (ws epsilon.websocket)
   (str epsilon.string)
   (thread epsilon.sys.thread)
   (time epsilon.time))
  (:export
   ;; Chat server example
   run-chat-server
   chat-client
   
   ;; Real-time data feed
   run-data-feed-server
   data-feed-client
   
   ;; File transfer example
   run-file-server
   file-client
   
   ;; Gaming/real-time updates
   run-game-server
   game-client
   
   ;; Load testing
   run-load-test
   
   ;; Benchmarking
   benchmark-throughput
   benchmark-latency))

(in-package epsilon.websocket.examples)

;;;; Chat Server Example

(defparameter *chat-rooms* (make-hash-table :test 'equal)
  "Hash table of room-name -> list of connections")

(defun add-to-room (room-name connection)
  "Add connection to chat room"
  (push connection (gethash room-name *chat-rooms* '())))

(defun remove-from-room (room-name connection)
  "Remove connection from chat room"
  (setf (gethash room-name *chat-rooms*)
        (remove connection (gethash room-name *chat-rooms*))))

(defun broadcast-to-room (room-name message &optional exclude-connection)
  "Broadcast message to all connections in room"
  (dolist (conn (gethash room-name *chat-rooms*))
    (when (and (ws:connection-open-p conn)
               (not (eq conn exclude-connection)))
      (handler-case
          (ws:send-text conn message)
        (error (e)
          (format t "Error broadcasting to connection: ~A~%" e)
          (remove-from-room room-name conn))))))

(defun make-chat-handler ()
  "Create chat server handler"
  (ws:make-websocket-handler
   :subprotocols '("chat")
   :on-connect (lambda (connection)
                 (format t "Chat client connected~%")
                 (ws:send-text connection 
                              (format nil "~A" 
                                      '(:type "welcome" 
                                        :message "Welcome to the chat server!"))))
   
   :on-message (lambda (connection message is-text)
                 (when is-text
                   (handler-case
                       (let ((msg (read-from-string message)))
                         (case (getf msg :type)
                           (:join
                            (let ((room (getf msg :room "general")))
                              (add-to-room room connection)
                              (broadcast-to-room room
                                               (format nil "~A" 
                                                       `(:type "user-joined"
                                                         :user ,(getf msg :user)
                                                         :room ,room))
                                               connection)))
                           (:message
                            (let ((room (getf msg :room "general")))
                              (broadcast-to-room room
                                               (format nil "~A" 
                                                       `(:type "chat-message"
                                                         :user ,(getf msg :user)
                                                         :message ,(getf msg :message)
                                                         :timestamp ,(get-universal-time)))
                                               connection)))
                           (:leave
                            (let ((room (getf msg :room "general")))
                              (remove-from-room room connection)
                              (broadcast-to-room room
                                               (format nil "~A" 
                                                       `(:type "user-left"
                                                         :user ,(getf msg :user)
                                                         :room ,room))
                                               connection)))))
                     (error (e)
                       (format t "Error parsing chat message: ~A~%" e)))))
   
   :on-close (lambda (connection code reason)
               (declare (ignore code reason))
               (format t "Chat client disconnected~%")
               ;; Remove from all rooms
               (maphash (lambda (room-name connections)
                         (declare (ignore room-name))
                         (setf connections (remove connection connections)))
                       *chat-rooms*))))

(defun run-chat-server (&key (host "localhost") (port 8080))
  "Run chat server example"
  (let ((server (ws:make-server :host host :port port)))
    (ws:register-handler server "/" (make-chat-handler))
    (ws:start-server server)
    (format t "Chat server running on ws://~A:~A/~%" host port)
    (format t "Press Enter to stop...~%")
    (read-line)
    (ws:stop-server server)))

(defun chat-client (uri username &key (room "general"))
  "Simple chat client"
  (let ((messages-received '()))
    (let ((options (ws:make-client-options
                   :subprotocols '("chat")
                   :on-message (lambda (conn message is-text)
                                (declare (ignore conn))
                                (when is-text
                                  (push message messages-received)
                                  (format t "Received: ~A~%" message))))))
      
      (ws:with-websocket-connection (conn uri :options options)
        ;; Join room
        (ws:send-text conn (format nil "~A" `(:type :join :user ,username :room ,room)))
        
        ;; Send a test message
        (ws:send-text conn (format nil "~A" `(:type :message 
                                             :user ,username 
                                             :room ,room
                                             :message "Hello from Lisp client!")))
        
        ;; Wait a bit for responses
        (sleep 1)
        
        ;; Leave room
        (ws:send-text conn (format nil "~A" `(:type :leave :user ,username :room ,room)))
        
        messages-received))))

;;;; Real-time Data Feed Example

(defparameter *data-feed-active* nil)
(defparameter *data-feed-subscribers* '())

(defun generate-market-data ()
  "Generate simulated market data"
  `(:type "market-data"
    :symbol "AAPL"
    :price ,(+ 150.0 (- (random 10.0) 5.0))
    :volume ,(+ 1000 (random 5000))
    :timestamp ,(get-universal-time)))

(defun start-data-feed ()
  "Start data feed broadcast thread"
  (setf *data-feed-active* t)
  (thread:make-thread
   (lambda ()
     (loop while *data-feed-active*
           do (let ((data (generate-market-data)))
                (dolist (conn *data-feed-subscribers*)
                  (when (ws:connection-open-p conn)
                    (handler-case
                        (ws:send-text conn (format nil "~A" data))
                      (error ()
                        (setf *data-feed-subscribers* 
                              (remove conn *data-feed-subscribers*))))))
                (sleep 0.1)))) ; 10 updates per second
   :name "data-feed"))

(defun stop-data-feed ()
  "Stop data feed"
  (setf *data-feed-active* nil))

(defun make-data-feed-handler ()
  "Create data feed handler"
  (ws:make-websocket-handler
   :subprotocols '("market-data")
   :on-connect (lambda (connection)
                 (push connection *data-feed-subscribers*)
                 (ws:send-text connection 
                              (format nil "~A" '(:type "subscribed" 
                                                 :message "Subscribed to market data")))
                 (format t "Data feed client connected (~D total)~%" 
                         (length *data-feed-subscribers*)))
   
   :on-message (lambda (connection message is-text)
                 (when is-text
                   (let ((msg (read-from-string message)))
                     (case (getf msg :type)
                       (:subscribe
                        (format t "Client subscribed to ~A~%" (getf msg :symbol)))
                       (:unsubscribe
                        (format t "Client unsubscribed from ~A~%" (getf msg :symbol)))))))
   
   :on-close (lambda (connection code reason)
               (declare (ignore code reason))
               (setf *data-feed-subscribers* 
                     (remove connection *data-feed-subscribers*))
               (format t "Data feed client disconnected (~D remaining)~%" 
                       (length *data-feed-subscribers*)))))

(defun run-data-feed-server (&key (host "localhost") (port 8081))
  "Run real-time data feed server"
  (let ((server (ws:make-server :host host :port port)))
    (ws:register-handler server "/" (make-data-feed-handler))
    (ws:start-server server)
    (start-data-feed)
    
    (format t "Data feed server running on ws://~A:~A/~%" host port)
    (format t "Press Enter to stop...~%")
    (read-line)
    
    (stop-data-feed)
    (ws:stop-server server)))

(defun data-feed-client (uri &key (duration 5))
  "Data feed client that collects data for specified duration"
  (let ((data-points '()))
    (let ((options (ws:make-client-options
                   :subprotocols '("market-data")
                   :on-message (lambda (conn message is-text)
                                (declare (ignore conn))
                                (when is-text
                                  (let ((data (read-from-string message)))
                                    (when (string= (getf data :type) "market-data")
                                      (push data data-points))))))))
      
      (ws:with-websocket-connection (conn uri :options options)
        ;; Subscribe to AAPL
        (ws:send-text conn (format nil "~A" '(:type :subscribe :symbol "AAPL")))
        
        ;; Collect data for specified duration
        (sleep duration)
        
        ;; Unsubscribe
        (ws:send-text conn (format nil "~A" '(:type :unsubscribe :symbol "AAPL")))
        
        (format t "Collected ~D data points in ~D seconds~%" 
                (length data-points) duration)
        (reverse data-points)))))

;;;; File Transfer Example

(defun make-file-server-handler ()
  "Create file transfer handler"
  (ws:make-websocket-handler
   :on-connect (lambda (connection)
                 (format t "File client connected~%")
                 (ws:send-text connection "Ready for file transfer"))
   
   :on-message (lambda (connection message is-text)
                 (if is-text
                     ;; Text message - file metadata or command
                     (let ((msg (read-from-string message)))
                       (case (getf msg :type)
                         (:file-request
                          (let ((filename (getf msg :filename)))
                            (format t "Client requested file: ~A~%" filename)
                            (if (probe-file filename)
                                (send-file connection filename)
                                (ws:send-text connection 
                                             (format nil "~A" '(:type :error 
                                                               :message "File not found"))))))
                         (:file-upload
                          (format t "Client uploading file: ~A (~D bytes)~%" 
                                  (getf msg :filename) (getf msg :size)))))
                     ;; Binary message - file data
                     (progn
                       (format t "Received ~D bytes of file data~%" (length message))
                       (ws:send-text connection 
                                    (format nil "~A" '(:type :ack 
                                                      :bytes-received ,(length message)))))))
   
   :on-close (lambda (connection code reason)
               (declare (ignore connection code reason))
               (format t "File client disconnected~%"))))

(defun send-file (connection filename)
  "Send file to client"
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let* ((file-size (file-length stream))
           (buffer (make-array file-size :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      
      ;; Send file metadata
      (ws:send-text connection 
                   (format nil "~A" `(:type :file-data 
                                     :filename ,(file-namestring filename)
                                     :size ,file-size)))
      
      ;; Send file data
      (ws:send-binary connection buffer)
      
      (format t "Sent file ~A (~D bytes)~%" filename file-size))))

(defun run-file-server (&key (host "localhost") (port 8082))
  "Run file transfer server"
  (let ((server (ws:make-server :host host :port port)))
    (ws:register-handler server "/" (make-file-server-handler))
    (ws:start-server server)
    (format t "File server running on ws://~A:~A/~%" host port)
    (format t "Press Enter to stop...~%")
    (read-line)
    (ws:stop-server server)))

;;;; Load Testing

(defun run-load-test (&key (host "localhost") (port 8080) 
                          (clients 10) (messages-per-client 100) (message-size 1024))
  "Run load test against WebSocket server"
  (let ((threads '())
        (start-time (get-internal-real-time))
        (total-messages 0)
        (total-errors 0))
    
    (format t "Starting load test: ~D clients, ~D messages each, ~D bytes per message~%" 
            clients messages-per-client message-size)
    
    ;; Create test message
    (let ((test-message (make-string message-size :initial-element #\A))
          (uri (format nil "ws://~A:~A/" host port)))
      
      ;; Start client threads
      (dotimes (i clients)
        (push (thread:make-thread
               (lambda ()
                 (handler-case
                     (ws:with-websocket-connection (conn uri)
                       (dotimes (j messages-per-client)
                         (ws:send-text conn test-message)
                         (incf total-messages)))
                   (error (e)
                     (format t "Client error: ~A~%" e)
                     (incf total-errors))))
               :name (format nil "load-test-client-~D" i))
              threads))
      
      ;; Wait for all threads to complete
      (dolist (thread threads)
        (thread:join-thread thread))
      
      (let* ((end-time (get-internal-real-time))
             (elapsed (/ (- end-time start-time) internal-time-units-per-second))
             (throughput (/ total-messages elapsed)))
        
        (format t "Load test completed:~%")
        (format t "  Total messages: ~D~%" total-messages)
        (format t "  Total errors: ~D~%" total-errors)
        (format t "  Time: ~,2F seconds~%" elapsed)
        (format t "  Throughput: ~,1F messages/sec~%" throughput)
        (format t "  Data rate: ~,1F KB/sec~%" (/ (* throughput message-size) 1024))))))

;;;; Benchmarking

(defun benchmark-latency (&key (host "localhost") (port 8080) (samples 1000))
  "Benchmark round-trip latency"
  (let ((latencies '())
        (uri (format nil "ws://~A:~A/" host port)))
    
    (ws:with-websocket-connection (conn uri)
      (dotimes (i samples)
        (let ((start-time (get-internal-real-time)))
          (ws:send-text conn "ping")
          ;; Assume immediate echo response
          (let ((end-time (get-internal-real-time)))
            (push (/ (- end-time start-time) internal-time-units-per-second) latencies))))
      
      (let* ((avg-latency (/ (reduce #'+ latencies) (length latencies)))
             (min-latency (reduce #'min latencies))
             (max-latency (reduce #'max latencies)))
        
        (format t "Latency benchmark (~D samples):~%" samples)
        (format t "  Average: ~,3F ms~%" (* avg-latency 1000))
        (format t "  Minimum: ~,3F ms~%" (* min-latency 1000))
        (format t "  Maximum: ~,3F ms~%" (* max-latency 1000))
        
        latencies))))

;;;; Demo Application Runner

(defun run-all-examples ()
  "Run all example applications for demonstration"
  (format t "WebSocket Examples Demonstration~%")
  (format t "================================~%~%")
  
  ;; Simple echo test
  (format t "1. Simple Echo Test~%")
  (let ((result (ws:simple-echo-test :port 9000)))
    (format t "   Result: ~A~%~%" result))
  
  ;; Throughput benchmark
  (format t "2. Throughput Benchmark~%")
  (ws:benchmark-throughput :port 9001 :message-count 100 :message-size 1024)
  (format t "~%")
  
  (format t "Examples completed!~%"))

;;;; Helper Functions

(defun start-example-server (type &key (host "localhost") (port 8080))
  "Start an example server of the specified type"
  (case type
    (:chat (run-chat-server :host host :port port))
    (:data-feed (run-data-feed-server :host host :port port))
    (:file (run-file-server :host host :port port))
    (:echo (let ((server (ws:make-server :host host :port port)))
             (ws:register-handler server "/" (ws:make-echo-handler))
             (ws:start-server server)
             (format t "Echo server running on ws://~A:~A/~%" host port)
             server))
    (t (error "Unknown server type: ~A" type))))

(defun interactive-chat-client (uri)
  "Interactive chat client for testing"
  (format t "WebSocket Chat Client~%")
  (format t "Enter username: ")
  (let ((username (read-line)))
    (let ((options (ws:make-client-options
                   :subprotocols '("chat")
                   :on-message (lambda (conn message is-text)
                                (declare (ignore conn))
                                (when is-text
                                  (format t "~A~%" message))))))
      
      (ws:with-websocket-connection (conn uri :options options)
        ;; Join default room
        (ws:send-text conn (format nil "~A" `(:type :join :user ,username :room "general")))
        
        (format t "Connected to chat! Type messages (or 'quit' to exit):~%")
        (loop
          (let ((input (read-line)))
            (when (string= input "quit")
              (return))
            (ws:send-text conn (format nil "~A" `(:type :message 
                                                 :user ,username 
                                                 :room "general"
                                                 :message ,input)))))
        
        ;; Leave room
        (ws:send-text conn (format nil "~A" `(:type :leave :user ,username :room "general")))))))

(defun websocket-stress-test (&key (host "localhost") (port 8080) (duration 60))
  "Continuous stress test for specified duration"
  (let ((start-time (get-universal-time))
        (message-count 0)
        (error-count 0)
        (active t))
    
    ;; Start monitoring thread
    (thread:make-thread
     (lambda ()
       (loop while active
             do (sleep 10)
                (let ((elapsed (- (get-universal-time) start-time)))
                  (format t "Progress: ~D seconds, ~D messages, ~D errors~%" 
                          elapsed message-count error-count))))
     :name "stress-monitor")
    
    ;; Run stress test
    (let ((end-time (+ start-time duration)))
      (loop while (< (get-universal-time) end-time)
            do (handler-case
                   (ws:send-text-message (format nil "ws://~A:~A/" host port) 
                                        "stress test message")
                   (incf message-count)
                 (error ()
                   (incf error-count)))
               (sleep 0.01))) ; 100 messages per second target
    
    (setf active nil)
    (format t "Stress test completed: ~D messages, ~D errors in ~D seconds~%" 
            message-count error-count duration)))
