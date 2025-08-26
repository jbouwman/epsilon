;;;; HTTP/2 Module Tests
;;;;
;;;; Main test suite for HTTP/2 implementation

(in-package :epsilon.http2)

(epsilon.test:deftest test-connection-preface
  "Test HTTP/2 connection preface"
  
  (let ((preface #(80 82 73 32 42 32 72 84 84 80 47 50 46 48 13 10 13 10 83 77 13 10 13 10))
        (preface-string "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"))
    
    ;; Test preface bytes
    (epsilon.test:is (= (length preface) 24))
    (epsilon.test:is (= (aref preface 0) 80)) ; 'P'
    (epsilon.test:is (= (aref preface 1) 82)) ; 'R'
    (epsilon.test:is (= (aref preface 2) 73)) ; 'I'
    
    ;; Test parameter value
    (epsilon.test:is (equalp +http2-preface+ preface-string))))

(epsilon.test:deftest test-make-http2-connection
  "Test HTTP/2 connection creation"
  
  ;; Mock socket for testing
  (let* ((mock-socket :mock-socket)
         (conn (make-http2-connection mock-socket :client-p t)))
    
    (epsilon.test:is (http2-connection-p conn))
    (epsilon.test:is (eq (http2-connection-socket conn) mock-socket))
    (epsilon.test:is (http2-connection-client-p conn))
    
    ;; Check HPACK encoder/decoder are created
    (epsilon.test:is (not (null (http2-connection-hpack-encoder conn))))
    (epsilon.test:is (not (null (http2-connection-hpack-decoder conn))))))

(epsilon.test:deftest test-stream-creation
  "Test HTTP/2 stream creation"
  
  (let* ((mock-socket :mock-socket)
         (conn (make-http2-connection mock-socket))
         (stream (create-stream conn)))
    
    (epsilon.test:is (http2-stream-p stream))
    (epsilon.test:is (eq (stream-connection stream) conn))
    (epsilon.test:is (> (stream-id stream) 0))
    
    ;; Client streams should be odd
    (when (http2-connection-client-p conn)
      (epsilon.test:is (oddp (stream-id stream))))))

(epsilon.test:deftest test-frame-flag-operations
  "Test frame flag operations"
  
  (let ((frame (make-http2-frame :type +frame-headers+
                                 :flags (logior +flag-end-stream+ 
                                              +flag-end-headers+)
                                 :stream-id 1)))
    
    ;; Test flag checking
    (epsilon.test:is (frame-flag-set-p frame +flag-end-stream+))
    (epsilon.test:is (frame-flag-set-p frame +flag-end-headers+))
    (epsilon.test:is (not (frame-flag-set-p frame +flag-padded+)))))

(epsilon.test:deftest test-settings-frame-creation
  "Test SETTINGS frame creation"
  
  ;; Test empty settings (ACK)
  (let ((ack-frame (make-settings-frame :ack t)))
    (epsilon.test:is (= (http2-frame-type ack-frame) +frame-settings+))
    (epsilon.test:is (= (http2-frame-flags ack-frame) +flag-ack+))
    (epsilon.test:is (= (http2-frame-length ack-frame) 0)))
  
  ;; Test with initial settings
  (let ((settings-frame (make-settings-frame 
                        :initial-settings '((#x3 . 100)
                                          (#x4 . 65535)))))
    (epsilon.test:is (= (http2-frame-type settings-frame) +frame-settings+))
    (epsilon.test:is (= (http2-frame-flags settings-frame) 0))
    (epsilon.test:is (= (http2-frame-length settings-frame) 12)))) ; 2 settings * 6 bytes

(epsilon.test:deftest test-ping-frame-creation
  "Test PING frame creation"
  
  ;; Test PING without ACK
  (let ((ping-frame (make-ping-frame)))
    (epsilon.test:is (= (http2-frame-type ping-frame) +frame-ping+))
    (epsilon.test:is (= (http2-frame-flags ping-frame) 0))
    (epsilon.test:is (= (http2-frame-length ping-frame) 8)))
  
  ;; Test PING with ACK
  (let ((ping-ack-frame (make-ping-frame :ack t)))
    (epsilon.test:is (= (http2-frame-flags ping-ack-frame) +flag-ack+))))

(epsilon.test:deftest test-goaway-frame-creation
  "Test GOAWAY frame creation"
  
  (let ((goaway-frame (make-goaway-frame 31 +error-protocol-error+ "Protocol error")))
    (epsilon.test:is (= (http2-frame-type goaway-frame) +frame-goaway+))
    (epsilon.test:is (= (http2-frame-stream-id goaway-frame) 0)) ; Connection-level frame
    
    ;; Check payload structure
    (let ((payload (http2-frame-payload goaway-frame)))
      (epsilon.test:is (>= (length payload) 8))
      
      ;; Extract last stream ID
      (let ((last-stream-id (logior (ash (logand #x7f (aref payload 0)) 24)
                                    (ash (aref payload 1) 16)
                                    (ash (aref payload 2) 8)
                                    (aref payload 3))))
        (epsilon.test:is (= last-stream-id 31)))
      
      ;; Extract error code
      (let ((error-code (logior (ash (aref payload 4) 24)
                               (ash (aref payload 5) 16)
                               (ash (aref payload 6) 8)
                               (aref payload 7))))
        (epsilon.test:is (= error-code +error-protocol-error+))))))

(epsilon.test:deftest test-rst-stream-frame
  "Test RST_STREAM frame creation"
  
  (let ((rst-frame (make-rst-stream-frame 3 +error-cancel+)))
    (epsilon.test:is (= (http2-frame-type rst-frame) +frame-rst-stream+))
    (epsilon.test:is (= (http2-frame-stream-id rst-frame) 3))
    (epsilon.test:is (= (http2-frame-length rst-frame) 4))
    
    ;; Check error code in payload
    (let* ((payload (http2-frame-payload rst-frame))
           (error-code (logior (ash (aref payload 0) 24)
                              (ash (aref payload 1) 16)
                              (ash (aref payload 2) 8)
                              (aref payload 3))))
      (epsilon.test:is (= error-code +error-cancel+)))))

;; Test runner
(defun run-http2-tests ()
  "Run all HTTP/2 module tests"
  (format t "~%Running HTTP/2 Module Tests~%")
  (format t "============================~%~%")
  
  (let ((results (epsilon.test:run-tests 
                  :package :epsilon.http2
                  :verbose t)))
    
    (format t "~%Test Results:~%")
    (format t "  Passed: ~D~%" (epsilon.test:passed-count results))
    (format t "  Failed: ~D~%" (epsilon.test:failed-count results))
    (format t "  Total:  ~D~%" (epsilon.test:total-count results))
    
    results))

(export 'run-http2-tests)