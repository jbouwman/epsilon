;;;; HTTP/2 Frame Tests
;;;;
;;;; Tests for frame parsing and serialization

(in-package :epsilon.http2)

(epsilon.test:deftest test-frame-creation
  "Test creating various frame types"
  
  ;; Test SETTINGS frame
  (let ((settings-frame (make-settings-frame 
                         :initial-settings '((#x3 . 100)
                                           (#x4 . 65535)))))
    (epsilon.test:is (= (http2-frame-type settings-frame) +frame-settings+))
    (epsilon.test:is (= (http2-frame-stream-id settings-frame) 0))
    (epsilon.test:is (> (http2-frame-length settings-frame) 0)))
  
  ;; Test PING frame
  (let ((ping-frame (make-ping-frame :ack nil)))
    (epsilon.test:is (= (http2-frame-type ping-frame) +frame-ping+))
    (epsilon.test:is (= (http2-frame-length ping-frame) 8)))
  
  ;; Test GOAWAY frame
  (let ((goaway-frame (make-goaway-frame 0 +error-no-error+)))
    (epsilon.test:is (= (http2-frame-type goaway-frame) +frame-goaway+))
    (epsilon.test:is (>= (http2-frame-length goaway-frame) 8))))

(epsilon.test:deftest test-frame-serialization
  "Test frame serialization and deserialization"
  
  ;; Create a test frame
  (let* ((original-frame (make-settings-frame :ack t))
         (output-stream (make-string-output-stream))
         (binary-stream (make-instance 'epsilon.stream:binary-output-stream
                                       :stream output-stream)))
    
    ;; Write frame
    (write-frame binary-stream original-frame)
    
    ;; Read it back
    (let* ((data (get-output-stream-string output-stream))
           (input-stream (make-string-input-stream data))
           (binary-input (make-instance 'epsilon.stream:binary-input-stream
                                        :stream input-stream))
           (read-frame (read-frame binary-input)))
      
      (epsilon.test:is (= (http2-frame-type original-frame)
                          (http2-frame-type read-frame)))
      (epsilon.test:is (= (http2-frame-flags original-frame)
                          (http2-frame-flags read-frame))))))

(epsilon.test:deftest test-frame-validation
  "Test frame validation"
  
  ;; Valid frame
  (let ((valid-frame (make-http2-frame :type +frame-data+
                                       :length 100
                                       :stream-id 1)))
    (epsilon.test:is (valid-frame-p valid-frame)))
  
  ;; Invalid frame (length too large)
  (let ((invalid-frame (make-http2-frame :type +frame-data+
                                         :length #x1000000
                                         :stream-id 1)))
    (epsilon.test:is (not (valid-frame-p invalid-frame)))))

(epsilon.test:deftest test-window-update-frame
  "Test WINDOW_UPDATE frame creation"
  
  (let ((window-frame (make-window-update-frame 1 65535)))
    (epsilon.test:is (= (http2-frame-type window-frame) +frame-window-update+))
    (epsilon.test:is (= (http2-frame-stream-id window-frame) 1))
    (epsilon.test:is (= (http2-frame-length window-frame) 4))
    
    ;; Check payload encoding
    (let ((payload (http2-frame-payload window-frame)))
      (epsilon.test:is (= (length payload) 4))
      ;; Window increment should be 65535
      (let ((increment (logior (ash (logand #x7f (aref payload 0)) 24)
                              (ash (aref payload 1) 16)
                              (ash (aref payload 2) 8)
                              (aref payload 3))))
        (epsilon.test:is (= increment 65535))))))

(epsilon.test:deftest test-headers-frame
  "Test HEADERS frame with HPACK encoding"
  
  (let* ((headers '((":status" . "200")
                   ("content-type" . "text/plain")))
         (headers-frame (make-headers-frame 1 headers 
                                           :end-stream t 
                                           :end-headers t)))
    
    (epsilon.test:is (= (http2-frame-type headers-frame) +frame-headers+))
    (epsilon.test:is (= (http2-frame-stream-id headers-frame) 1))
    (epsilon.test:is (logtest (http2-frame-flags headers-frame) +flag-end-stream+))
    (epsilon.test:is (logtest (http2-frame-flags headers-frame) +flag-end-headers+))
    
    ;; Verify we can decode the headers back
    (let ((decoded (decode-headers-from-payload (http2-frame-payload headers-frame))))
      (epsilon.test:is (equal (length decoded) (length headers))))))

(epsilon.test:deftest test-data-frame
  "Test DATA frame creation"
  
  (let* ((data "Hello, World!")
         (data-frame (make-data-frame 1 data :end-stream t)))
    
    (epsilon.test:is (= (http2-frame-type data-frame) +frame-data+))
    (epsilon.test:is (= (http2-frame-stream-id data-frame) 1))
    (epsilon.test:is (logtest (http2-frame-flags data-frame) +flag-end-stream+))
    
    ;; Check payload
    (let ((payload (http2-frame-payload data-frame)))
      (epsilon.test:is (equalp payload (epsilon.string:string-to-octets data))))))