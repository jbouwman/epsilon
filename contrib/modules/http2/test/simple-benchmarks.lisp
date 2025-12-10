;;;; Simple HTTP/2 Performance Tests
;;;;
;;;; Basic performance tests for HTTP/2 components

(in-package :epsilon.http2)

;;;; Benchmark Utilities

(defun benchmark-simple (name iterations fn)
  "Simple benchmark helper"
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (funcall fn))
    (let* ((end (get-internal-real-time))
           (elapsed (/ (- end start) internal-time-units-per-second))
           (per-iteration (/ elapsed iterations)))
      (format t "~A: ~,3F seconds total (~,6F per iteration, ~D iterations)~%" 
              name elapsed per-iteration iterations)
      per-iteration)))

;;;; Frame Benchmarks

(epsilon.test:deftest test-frame-creation-performance
  "Test frame creation performance"
  
  (let* ((iterations 10000)
         (data (make-string 100 :initial-element #\X))
         (per-iteration 
          (benchmark-simple "DATA frame creation" iterations
                           (lambda ()
                             (epsilon.http2.frames:make-data-frame 1 data)))))
    
    ;; Should create frames reasonably fast (less than 1ms each)
    (epsilon.test:is (< per-iteration 0.001))))

(epsilon.test:deftest test-settings-frame-performance
  "Test SETTINGS frame creation performance"
  
  (let* ((iterations 50000)
         (per-iteration 
          (benchmark-simple "SETTINGS frame creation" iterations
                           (lambda ()
                             (epsilon.http2.frames:make-settings-frame)))))
    
    ;; SETTINGS frames should be very fast
    (epsilon.test:is (< per-iteration 0.0005))))

;;;; Flow Control Benchmarks

(epsilon.test:deftest test-flow-control-performance
  "Test flow control operation performance"
  
  (let ((controller (epsilon.http2.flow-control:make-connection-flow-controller))
        (iterations 100000))
    
    ;; Test window checks
    (let ((per-iteration
           (benchmark-simple "Flow control checks" iterations
                            (lambda ()
                              (epsilon.http2.flow-control:can-send-p controller 1000)))))
      (epsilon.test:is (< per-iteration 0.00001))) ; Should be very fast
    
    ;; Test window updates
    (let ((per-iteration
           (benchmark-simple "Window updates" 10000
                            (lambda ()
                              (epsilon.http2.flow-control:consume-send-window controller 100)
                              (epsilon.http2.flow-control:update-send-window controller 100)))))
      (epsilon.test:is (< per-iteration 0.0001)))))

;;;; HPACK Benchmarks

(epsilon.test:deftest test-hpack-performance
  "Test HPACK encoding/decoding performance"
  
  (let ((encoder (epsilon.http2.hpack:make-encoder))
        (decoder (epsilon.http2.hpack:make-decoder))
        (headers '((":method" . "GET")
                  (":path" . "/index.html")
                  (":scheme" . "https")
                  ("user-agent" . "test-client/1.0")
                  ("accept" . "text/html"))))
    
    ;; Test encoding performance
    (let ((per-iteration
           (benchmark-simple "HPACK encoding" 1000
                            (lambda ()
                              (epsilon.http2.hpack:encode-header-list encoder headers)))))
      (epsilon.test:is (< per-iteration 0.01)))
    
    ;; Test round-trip performance
    (let ((encoded (epsilon.http2.hpack:encode-header-list encoder headers))
          (per-iteration
           (benchmark-simple "HPACK decoding" 1000
                            (lambda ()
                              (epsilon.http2.hpack:decode-header-block decoder encoded)))))
      (epsilon.test:is (< per-iteration 0.01)))))

;;;; Stream State Benchmarks

(epsilon.test:deftest test-stream-state-performance
  "Test stream state transition performance"
  
  (let ((streams (loop for i from 1 to 1000 by 2
                      collect (epsilon.http2.stream:initialize-stream i nil))))
    
    ;; Test state transitions
    (let ((per-iteration
           (benchmark-simple "Stream state transitions" 1000
                            (lambda ()
                              (dolist (stream streams)
                                (epsilon.http2.stream:transition-stream-state 
                                 stream epsilon.http2.frames:+frame-headers+ 0))))))
      (epsilon.test:is (< per-iteration 0.01)))
    
    ;; Test state queries
    (let ((per-iteration
           (benchmark-simple "Stream state queries" 10000
                            (lambda ()
                              (dolist (stream streams)
                                (epsilon.http2.stream:stream-open-p stream))))))
      (epsilon.test:is (< per-iteration 0.001)))))

;;;; Memory Usage Test

(epsilon.test:deftest test-memory-usage
  "Test memory usage of core components"
  
  (let ((initial-bytes #+sbcl (sb-ext:get-bytes-consed)
                       #-sbcl 0))
    
    ;; Create multiple connections and streams
    (let ((connections (loop for i from 1 to 10
                           collect (make-http2-connection nil :client-p (oddp i))))
          (streams nil))
      
      ;; Create streams on connections
      (dolist (conn connections)
        (loop for j from 1 to 10
              do (push (create-stream conn) streams)))
      
      (let ((final-bytes #+sbcl (sb-ext:get-bytes-consed)
                         #-sbcl 0))
        (format t "Memory allocated: ~:D bytes for 10 connections + 100 streams~%"
                (- final-bytes initial-bytes))
        
        ;; Cleanup
        (setf connections nil streams nil)))))

;;;; Stress Tests

(epsilon.test:deftest test-concurrent-stream-simulation
  "Simulate concurrent stream operations"
  
  (let* ((conn (make-http2-connection nil :client-p t))
         (num-streams 50)
         (streams (loop for i from 1 to num-streams
                       collect (create-stream conn))))
    
    ;; Simulate activity on all streams
    (let ((per-iteration
           (benchmark-simple "Concurrent stream operations" 100
                            (lambda ()
                              (dolist (stream streams)
                                ;; Simulate headers
                                (epsilon.http2.stream:transition-stream-state 
                                 stream epsilon.http2.frames:+frame-headers+ 0)
                                ;; Check flow control
                                (epsilon.http2.flow-control:can-send-data-p
                                 (http2-connection-flow-controller conn)
                                 (epsilon.http2.stream:stream-flow-controller stream)
                                 1000))))))
      
      (epsilon.test:is (< per-iteration 0.1)) ; Should handle 50 streams reasonably fast
      (epsilon.test:is (= num-streams (connection-active-streams-count conn))))))

;;;; Run All Benchmarks

(defun run-performance-tests ()
  "Run all performance tests"
  (format t "~%=== HTTP/2 Performance Tests ===~%")
  (epsilon.test:run-tests :pattern "performance\\|benchmark")
  (format t "~%=== Performance Tests Complete ===~%"))