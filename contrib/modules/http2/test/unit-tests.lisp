;;;; HTTP/2 Unit Tests
;;;;
;;;; Basic unit tests for HTTP/2 core functionality

(in-package :epsilon.http2)

;;;; Flow Control Tests

(epsilon.test:deftest test-flow-control-basics
  "Test basic flow control operations"
  
  ;; Test connection flow controller
  (let ((controller (epsilon.http2.flow-control:make-connection-flow-controller)))
    (epsilon.test:is (= 65535 (epsilon.http2.flow-control:flow-controller-send-window controller)))
    (epsilon.test:is (= 65535 (epsilon.http2.flow-control:flow-controller-recv-window controller)))
    
    ;; Test window consumption
    (epsilon.http2.flow-control:consume-send-window controller 1000)
    (epsilon.test:is (= 64535 (epsilon.http2.flow-control:flow-controller-send-window controller)))
    
    ;; Test window update
    (epsilon.http2.flow-control:update-send-window controller 500)
    (epsilon.test:is (= 65035 (epsilon.http2.flow-control:flow-controller-send-window controller)))))

(epsilon.test:deftest test-stream-flow-control
  "Test stream flow control"
  
  (let ((stream-controller (epsilon.http2.flow-control:make-stream-flow-controller)))
    (epsilon.test:is (= 65535 (epsilon.http2.flow-control:flow-controller-send-window stream-controller)))
    
    ;; Test can-send check
    (epsilon.test:is (epsilon.http2.flow-control:can-send-p stream-controller 1000))
    (epsilon.test:is (not (epsilon.http2.flow-control:can-send-p stream-controller 70000)))))

;;;; Stream State Tests

(epsilon.test:deftest test-stream-initialization
  "Test stream initialization"
  
  (let ((stream (epsilon.http2.stream:initialize-stream 1 nil)))
    (epsilon.test:is (= 1 (epsilon.http2.stream:stream-id stream)))
    (epsilon.test:is (= epsilon.http2.stream:+stream-idle+ (epsilon.http2.stream:stream-state stream)))
    (epsilon.test:is (not (epsilon.http2.stream:stream-closed-p stream)))))

(epsilon.test:deftest test-stream-state-transitions
  "Test stream state transitions"
  
  (let ((stream (epsilon.http2.stream:initialize-stream 1 nil)))
    ;; HEADERS frame should open stream
    (epsilon.http2.stream:transition-stream-state stream 
                                                  epsilon.http2.frames:+frame-headers+ 0)
    (epsilon.test:is (= epsilon.http2.stream:+stream-open+ (epsilon.http2.stream:stream-state stream)))
    
    ;; Can send and receive in open state
    (epsilon.test:is (epsilon.http2.stream:can-send-frame-p stream epsilon.http2.frames:+frame-data+))
    (epsilon.test:is (epsilon.http2.stream:can-receive-frame-p stream epsilon.http2.frames:+frame-data+))
    
    ;; END_STREAM should half-close
    (epsilon.http2.stream:transition-stream-state stream 
                                                  epsilon.http2.frames:+frame-data+
                                                  epsilon.http2.frames:+flag-end-stream+)
    (epsilon.test:is (= epsilon.http2.stream:+stream-half-closed-remote+ 
                       (epsilon.http2.stream:stream-state stream)))))

;;;; Frame Tests

(epsilon.test:deftest test-frame-creation
  "Test HTTP/2 frame creation"
  
  ;; Test DATA frame
  (let ((data-frame (epsilon.http2.frames:make-data-frame 1 "Hello" :end-stream t)))
    (epsilon.test:is (= epsilon.http2.frames:+frame-data+ (epsilon.http2.frames:http2-frame-type data-frame)))
    (epsilon.test:is (= 1 (epsilon.http2.frames:http2-frame-stream-id data-frame)))
    (epsilon.test:is (= epsilon.http2.frames:+flag-end-stream+ (epsilon.http2.frames:http2-frame-flags data-frame))))
  
  ;; Test SETTINGS frame
  (let ((settings-frame (epsilon.http2.frames:make-settings-frame)))
    (epsilon.test:is (= epsilon.http2.frames:+frame-settings+ (epsilon.http2.frames:http2-frame-type settings-frame)))
    (epsilon.test:is (= 0 (epsilon.http2.frames:http2-frame-stream-id settings-frame))))
  
  ;; Test WINDOW_UPDATE frame
  (let ((window-frame (epsilon.http2.frames:make-window-update-frame 1 1000)))
    (epsilon.test:is (= epsilon.http2.frames:+frame-window-update+ (epsilon.http2.frames:http2-frame-type window-frame)))
    (epsilon.test:is (= 1 (epsilon.http2.frames:http2-frame-stream-id window-frame)))
    (epsilon.test:is (= 4 (epsilon.http2.frames:http2-frame-length window-frame)))))

;;;; Error Handling Tests

(epsilon.test:deftest test-error-code-names
  "Test error code name mapping"
  
  (epsilon.test:is (string= "NO_ERROR" 
                           (epsilon.http2.error:error-code-name epsilon.http2.frames:+error-no-error+)))
  (epsilon.test:is (string= "PROTOCOL_ERROR" 
                           (epsilon.http2.error:error-code-name epsilon.http2.frames:+error-protocol-error+)))
  (epsilon.test:is (string= "FLOW_CONTROL_ERROR" 
                           (epsilon.http2.error:error-code-name epsilon.http2.frames:+error-flow-control-error+))))

(epsilon.test:deftest test-frame-size-validation
  "Test frame size validation"
  
  ;; Valid frame should pass
  (let ((valid-frame (epsilon.http2.frames:make-data-frame 1 "test")))
    (epsilon.test:is (epsilon.http2.frames:valid-frame-p valid-frame)))
  
  ;; Test WINDOW_UPDATE size validation
  (let ((window-frame (epsilon.http2.frames:make-window-update-frame 1 1000)))
    (epsilon.test:is (= 4 (epsilon.http2.frames:http2-frame-length window-frame)))))

;;;; HPACK Tests

(epsilon.test:deftest test-hpack-basic
  "Test basic HPACK functionality"
  
  (let ((encoder (epsilon.http2.hpack:make-encoder))
        (decoder (epsilon.http2.hpack:make-decoder)))
    (epsilon.test:is (not (null encoder)))
    (epsilon.test:is (not (null decoder)))
    
    ;; Test simple encoding/decoding
    (let* ((headers '((":method" . "GET") (":path" . "/test")))
           (encoded (epsilon.http2.hpack:encode-header-list encoder headers))
           (decoded (epsilon.http2.hpack:decode-header-block decoder encoded)))
      (epsilon.test:is (= (length headers) (length decoded)))
      ;; Check that all original headers are in decoded result
      (dolist (header headers)
        (epsilon.test:is (member header decoded :test #'equal))))))

;;;; Priority Tree Tests

(epsilon.test:deftest test-priority-tree-basic
  "Test basic priority tree operations"
  
  (let ((tree (epsilon.http2.priority:make-priority-tree)))
    (epsilon.test:is (not (null tree)))
    
    ;; Add a stream
    (epsilon.http2.priority:add-stream tree 1 :weight 16)
    (epsilon.test:is (member 1 (epsilon.http2.priority:get-stream-children tree 0)))
    
    ;; Add child stream
    (epsilon.http2.priority:add-stream tree 3 :dependency 1 :weight 32)
    (epsilon.test:is (member 3 (epsilon.http2.priority:get-stream-children tree 1)))
    (epsilon.test:is (= 1 (epsilon.http2.priority:get-stream-parent tree 3)))))

;;;; Connection Tests

(epsilon.test:deftest test-connection-creation
  "Test HTTP/2 connection creation"
  
  (let ((conn (make-http2-connection nil :client-p t)))
    (epsilon.test:is (http2-connection-p conn))
    (epsilon.test:is (connection-client-p conn))
    (epsilon.test:is (= 1 (connection-next-stream-id conn))) ; Client starts with odd
    (epsilon.test:is (not (null (http2-connection-flow-controller conn)))))
  
  (let ((server-conn (make-http2-connection nil :client-p nil)))
    (epsilon.test:is (not (connection-client-p server-conn)))
    (epsilon.test:is (= 2 (connection-next-stream-id server-conn))))) ; Server starts with even

;;;; Integration Tests

(epsilon.test:deftest test-stream-creation-on-connection
  "Test creating streams on connection"
  
  (let* ((conn (make-http2-connection nil :client-p t))
         (stream1 (create-stream conn))
         (stream2 (create-stream conn)))
    
    (epsilon.test:is (= 1 (epsilon.http2.stream:stream-id stream1)))
    (epsilon.test:is (= 3 (epsilon.http2.stream:stream-id stream2))) ; Next odd number
    (epsilon.test:is (= 2 (connection-active-streams-count conn)))))

(epsilon.test:deftest test-flow-control-integration
  "Test flow control integration with streams"
  
  (let* ((conn (make-http2-connection nil :client-p t))
         (stream (create-stream conn))
         (conn-controller (http2-connection-flow-controller conn))
         (stream-controller (epsilon.http2.stream:stream-flow-controller stream)))
    
    ;; Both controllers should exist
    (epsilon.test:is (not (null conn-controller)))
    (epsilon.test:is (not (null stream-controller)))
    
    ;; Should be able to send small amounts
    (epsilon.test:is (epsilon.http2.flow-control:can-send-data-p conn-controller stream-controller 1000))))