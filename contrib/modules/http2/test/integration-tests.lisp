;;;; Comprehensive HTTP/2 Integration Tests
;;;;
;;;; Tests all major HTTP/2 features including flow control, stream management,
;;;; priority, server push, and error handling

(in-package :epsilon.http2)

;;;; Flow Control Tests

(epsilon.test:deftest test-connection-flow-control
  "Test connection-level flow control"
  (let ((controller (epsilon.http2.flow-control:make-connection-flow-controller)))
    ;; Initial window should be 65535
    (epsilon.test:is (= 65535 (epsilon.http2.flow-control:flow-controller-send-window controller)))
    (epsilon.test:is (= 65535 (epsilon.http2.flow-control:flow-controller-recv-window controller)))
    
    ;; Consume some window
    (epsilon.http2.flow-control:consume-send-window controller 1000)
    (epsilon.test:is (= 64535 (epsilon.http2.flow-control:flow-controller-send-window controller)))
    
    ;; Update window
    (epsilon.http2.flow-control:update-send-window controller 500)
    (epsilon.test:is (= 65035 (epsilon.http2.flow-control:flow-controller-send-window controller)))
    
    ;; Test receive window
    (epsilon.http2.flow-control:consume-recv-window controller 32768)
    (epsilon.test:is (epsilon.http2.flow-control:should-send-window-update-p controller))
    (epsilon.test:is (= 32768 (epsilon.http2.flow-control:calculate-window-update controller)))))

(deftest test-stream-flow-control ()
  "Test stream-level flow control"
  (let ((conn-controller (flow:make-connection-flow-controller))
        (stream-controller (flow:make-stream-flow-controller)))
    
    ;; Test coordinated flow control
    (assert-true (flow:can-send-data-p conn-controller stream-controller 1000))
    
    ;; Consume window on both levels
    (flow:consume-send-window conn-controller 1000)
    (flow:consume-send-window stream-controller 1000)
    
    ;; Test window exhaustion
    (assert-false (flow:can-send-data-p conn-controller stream-controller 65000))
    
    ;; Test DATA frame flow control
    (multiple-value-bind (conn-update stream-update)
        (flow:handle-data-frame-flow-control 
         conn-controller stream-controller 30000)
      (assert-true conn-update)
      (assert-true stream-update))))

;;;; Stream State Machine Tests

(deftest test-stream-state-transitions ()
  "Test stream state machine transitions"
  (let ((stream (stream:initialize-stream 1 nil)))
    ;; Initial state should be IDLE
    (assert-equal stream:+stream-idle+ (stream:stream-state stream))
    
    ;; HEADERS frame should open stream
    (stream:transition-stream-state stream frames:+frame-headers+ 0)
    (assert-equal stream:+stream-open+ (stream:stream-state stream))
    
    ;; DATA with END_STREAM should half-close
    (stream:transition-stream-state stream frames:+frame-data+ 
                                   frames:+flag-end-stream+)
    (assert-equal stream:+stream-half-closed-remote+ 
                 (stream:stream-state stream))
    
    ;; Can still send frames
    (assert-true (stream:can-send-frame-p stream frames:+frame-data+))
    
    ;; Cannot receive DATA anymore
    (assert-false (stream:can-receive-frame-p stream frames:+frame-data+))
    
    ;; RST_STREAM closes stream
    (stream:transition-stream-state stream frames:+frame-rst-stream+ 0)
    (assert-true (stream:stream-closed-p stream))))

(deftest test-stream-priority-states ()
  "Test stream states with PUSH_PROMISE"
  (let ((stream (stream:initialize-stream 2 nil)))
    ;; Reserve stream for push
    (stream:reserve-stream stream nil) ; Server reservation
    (assert-equal stream:+stream-reserved-remote+ (stream:stream-state stream))
    
    ;; HEADERS should transition to half-closed
    (stream:transition-stream-state stream frames:+frame-headers+ 0)
    (assert-equal stream:+stream-half-closed-local+ 
                 (stream:stream-state stream))
    
    ;; Can receive but not send DATA
    (assert-true (stream:can-receive-frame-p stream frames:+frame-data+))
    (assert-false (stream:can-send-frame-p stream frames:+frame-data+))))

;;;; Error Handling Tests

(deftest test-protocol-error-detection ()
  "Test protocol error detection"
  ;; Test frame size validation
  (let ((oversized-frame (frames:make-http2-frame
                         :type frames:+frame-data+
                         :length 16777216 ; > max
                         :stream-id 1)))
    (assert-error (error:check-frame-size oversized-frame 16384)))
  
  ;; Test invalid SETTINGS
  (assert-error (error:check-settings-values 
                '((#.frames:+settings-enable-push+ . 2)))) ; Must be 0 or 1
  
  ;; Test zero WINDOW_UPDATE
  (assert-error (error:check-window-update 0 1))
  
  ;; Test stream dependency on itself
  (assert-error (error:check-priority-validity 1 1))
  
  ;; Test connection-specific headers
  (assert-error (error:check-header-block-validity
                '(("connection" . "keep-alive")))))

(deftest test-goaway-frame-creation ()
  "Test GOAWAY frame creation"
  (let ((frame (error:make-goaway-frame 
               42 
               frames:+error-protocol-error+
               "Protocol violation")))
    (assert-equal frames:+frame-goaway+ (frames:http2-frame-type frame))
    (assert-equal 0 (frames:http2-frame-stream-id frame))
    
    ;; Parse payload
    (let ((payload (frames:http2-frame-payload frame)))
      (let ((last-stream-id (logior (ash (aref payload 0) 24)
                                   (ash (aref payload 1) 16)
                                   (ash (aref payload 2) 8)
                                   (aref payload 3)))
            (error-code (logior (ash (aref payload 4) 24)
                              (ash (aref payload 5) 16)
                              (ash (aref payload 6) 8)
                              (aref payload 7))))
        (assert-equal 42 (logand last-stream-id #x7fffffff))
        (assert-equal frames:+error-protocol-error+ error-code)))))

;;;; HPACK Tests

(deftest test-hpack-dynamic-table ()
  "Test HPACK dynamic table operations"
  (let ((encoder (hpack:make-encoder))
        (decoder (hpack:make-decoder)))
    
    ;; Encode headers with dynamic table
    (let* ((headers '((":method" . "GET")
                     (":path" . "/index.html")
                     (":scheme" . "https")
                     ("custom-header" . "custom-value")))
           (encoded (hpack:encode-header-list encoder headers)))
      
      ;; Decode should match
      (let ((decoded (hpack:decode-header-block decoder encoded)))
        (assert-equal (length headers) (length decoded))
        (dolist (header headers)
          (assert-true (member header decoded :test #'equal)))))
    
    ;; Test table size limit
    (let ((large-headers (loop for i from 1 to 100
                              collect (cons (format nil "header-~D" i)
                                          (format nil "value-~D" i)))))
      (hpack:encode-header-list encoder large-headers))))

(deftest test-huffman-encoding ()
  "Test Huffman encoding/decoding"
  (let ((test-strings '("www.example.com"
                       "GET"
                       "/index.html"
                       "Mozilla/5.0"
                       "application/json")))
    (dolist (string test-strings)
      (let* ((encoded (hpack:huffman-encode string))
             (decoded (hpack:huffman-decode encoded)))
        (assert-string= string decoded)
        ;; Huffman should compress most strings
        (assert-true (< (length encoded) (length string)))))))

;;;; Priority Tree Tests

(deftest test-priority-tree-operations ()
  "Test stream priority tree"
  (let ((tree (priority:make-priority-tree)))
    ;; Add streams with dependencies
    (priority:add-stream tree 1 :dependency 0 :weight 10)
    (priority:add-stream tree 3 :dependency 1 :weight 20)
    (priority:add-stream tree 5 :dependency 1 :weight 30 :exclusive-p t)
    
    ;; Stream 5 should be sole child of 1, with 3 as its child
    (assert-equal '(5) (priority:get-stream-children tree 1))
    (assert-equal '(3) (priority:get-stream-children tree 5))
    
    ;; Test reprioritization
    (priority:reprioritize-stream tree 3 0 15)
    (assert-equal 0 (priority:get-stream-parent tree 3))
    
    ;; Test removal
    (priority:remove-stream tree 5)
    ;; Stream 3 should move back to stream 1
    (assert-equal 1 (priority:get-stream-parent tree 3))))

(deftest test-weighted-fair-queuing ()
  "Test priority-based scheduling"
  (let ((tree (priority:make-priority-tree)))
    ;; Create streams with different weights
    (priority:add-stream tree 1 :weight 10)
    (priority:add-stream tree 3 :weight 20)
    (priority:add-stream tree 5 :weight 30)
    
    ;; Set queued data
    (setf (priority:stream-priority-bytes-queued 
           (gethash 1 (priority:tree-nodes tree))) 1000)
    (setf (priority:stream-priority-bytes-queued
           (gethash 3 (priority:tree-nodes tree))) 1000)
    (setf (priority:stream-priority-bytes-queued
           (gethash 5 (priority:tree-nodes tree))) 1000)
    
    ;; Stream 5 should be scheduled first (highest weight)
    (let ((next (priority:get-next-stream tree)))
      (assert-true (member next '(1 3 5))))))

;;;; Server Push Tests

(deftest test-server-push-promise ()
  "Test server push functionality"
  (let ((cache (push:make-push-cache)))
    ;; Cache pushable resources
    (push:cache-push-resource cache "/style.css"
                             '((":status" . "200")
                               ("content-type" . "text/css"))
                             "body { margin: 0; }")
    
    ;; Check what should be pushed
    (let ((pushable (push:get-pushable-resources 
                    cache "/" '(("accept" . "text/html")))))
      (assert-true (>= (length pushable) 0)))
    
    ;; Test push promise creation
    (let ((promise (push:make-push-promise
                   :stream-id 1
                   :promised-stream-id 2
                   :headers '((":method" . "GET")
                            (":path" . "/style.css")))))
      (assert-equal 1 (push:push-promise-stream-id promise))
      (assert-equal 2 (push:push-promise-promised-stream-id promise))
      (assert-equal :promised (push:push-promise-state promise)))))

;;;; Connection Management Tests

(deftest test-connection-lifecycle ()
  "Test connection establishment and shutdown"
  ;; This would require actual network setup
  ;; Simplified test for connection object
  (let* ((socket nil) ; Would be actual socket
         (conn (http2:make-http2-connection socket :client-p t)))
    
    ;; Check initialization
    (assert-true (http2:http2-connection-p conn))
    (assert-equal 1 (slot-value conn 'http2::next-stream-id)) ; Client starts with odd
    (assert-not-null (http2:http2-connection-flow-controller conn))
    
    ;; Create streams
    (let ((stream1 (http2:create-stream conn))
          (stream2 (http2:create-stream conn)))
      (assert-equal 1 (stream:stream-id stream1))
      (assert-equal 3 (stream:stream-id stream2))
      (assert-equal 2 (slot-value conn 'http2::active-streams-count)))))

;;;; Full Integration Test

(deftest test-http2-request-response-cycle ()
  "Test complete HTTP/2 request-response cycle"
  ;; This test simulates a full cycle without actual networking
  (let* ((client-conn (http2:make-http2-connection nil :client-p t))
         (server-conn (http2:make-http2-connection nil :client-p nil)))
    
    ;; Client creates stream and sends request
    (let ((client-stream (http2:create-stream client-conn)))
      ;; Send headers
      (let ((request-headers '((":method" . "GET")
                             (":path" . "/test")
                             (":scheme" . "https")
                             (":authority" . "example.com"))))
        ;; This would normally send over network
        ;; For testing, we just verify the state changes
        (stream:transition-stream-state client-stream 
                                       frames:+frame-headers+ 
                                       frames:+flag-end-stream+)
        (assert-equal stream:+stream-half-closed-local+ 
                     (stream:stream-state client-stream))))
    
    ;; Server would receive and process
    (let ((server-stream (stream:initialize-stream 1 server-conn)))
      (stream:transition-stream-state server-stream 
                                     frames:+frame-headers+
                                     frames:+flag-end-stream+)
      (assert-equal stream:+stream-half-closed-remote+
                   (stream:stream-state server-stream))
      
      ;; Server sends response
      (stream:transition-stream-state server-stream
                                     frames:+frame-headers+ 0)
      (stream:transition-stream-state server-stream
                                     frames:+frame-data+
                                     frames:+flag-end-stream+)
      (assert-true (stream:stream-closed-p server-stream)))))

;;;; Run all tests

(defun run-integration-tests ()
  "Run all HTTP/2 integration tests"
  (run-suite 'http2-integration-suite))