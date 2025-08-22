;;;; HTTP/2 Conformance Tests
;;;;
;;;; Tests to verify RFC 7540 compliance

(defpackage :epsilon.http2.conformance-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:h2 #:epsilon.http2)
   (#:hpack #:epsilon.http2.hpack)))

(in-package :epsilon.http2.conformance-tests)

;;;; Frame Format Tests (RFC 7540 Section 4)

(deftest test-frame-header-format
  "Test that frame headers are 9 bytes"
  (let ((frame (h2::make-http2-frame 
               :length 0
               :type h2::+frame-settings+
               :flags 0
               :stream-id 0)))
    (let ((serialized (h2::serialize-frame frame)))
      (is-= 9 (length serialized)))))

(deftest test-frame-length-limits
  "Test 24-bit length field limits"
  ;; Maximum frame length is 2^24 - 1
  (let ((max-length (1- (ash 1 24))))
    (is-= #xffffff max-length)
    
    ;; Test frame with max length
    (let ((frame (h2::make-http2-frame
                 :length max-length
                 :type h2::+frame-data+
                 :flags 0
                 :stream-id 1)))
      (is-= max-length (h2::http2-frame-length frame)))))

(deftest test-stream-id-limits
  "Test 31-bit stream ID limits"
  ;; Maximum stream ID is 2^31 - 1
  (let ((max-stream-id (1- (ash 1 31))))
    (is-= #x7fffffff max-stream-id)
    
    ;; Test frame with max stream ID
    (let ((frame (h2::make-http2-frame
                 :length 0
                 :type h2::+frame-data+
                 :flags 0
                 :stream-id max-stream-id)))
      (is-= max-stream-id (h2::http2-frame-stream-id frame)))))

;;;; Connection Preface Tests (RFC 7540 Section 3.5)

(deftest test-connection-preface
  "Test HTTP/2 connection preface"
  (is-equal "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" h2::+http2-preface+)
  (is-= 24 (length h2::+http2-preface+)))

;;;; Frame Type Tests (RFC 7540 Section 6)

(deftest test-frame-types
  "Test frame type constants"
  (is-= #x0 h2::+frame-data+)
  (is-= #x1 h2::+frame-headers+)
  (is-= #x2 h2::+frame-priority+)
  (is-= #x3 h2::+frame-rst-stream+)
  (is-= #x4 h2::+frame-settings+)
  (is-= #x5 h2::+frame-push-promise+)
  (is-= #x6 h2::+frame-ping+)
  (is-= #x7 h2::+frame-goaway+)
  (is-= #x8 h2::+frame-window-update+)
  (is-= #x9 h2::+frame-continuation+))

;;;; Settings Tests (RFC 7540 Section 6.5)

(deftest test-settings-parameters
  "Test settings parameter IDs"
  (is-= #x1 h2::+settings-header-table-size+)
  (is-= #x2 h2::+settings-enable-push+)
  (is-= #x3 h2::+settings-max-concurrent-streams+)
  (is-= #x4 h2::+settings-initial-window-size+)
  (is-= #x5 h2::+settings-max-frame-size+)
  (is-= #x6 h2::+settings-max-header-list-size+))

(deftest test-settings-frame-format
  "Test SETTINGS frame format"
  ;; Empty settings (ACK)
  (let ((ack-frame (h2::make-settings-frame :ack t)))
    (is-= h2::+frame-settings+ (h2::http2-frame-type ack-frame))
    (is-= h2::+flag-ack+ (h2::http2-frame-flags ack-frame))
    (is-= 0 (h2::http2-frame-length ack-frame))
    (is-= 0 (h2::http2-frame-stream-id ack-frame)))
  
  ;; Settings with parameters
  (let ((settings-frame (h2::make-settings-frame 
                        :initial-settings
                        (list (cons h2::+settings-initial-window-size+ 65535)
                              (cons h2::+settings-max-frame-size+ 16384)))))
    (is-= h2::+frame-settings+ (h2::http2-frame-type settings-frame))
    (is-= 0 (h2::http2-frame-flags settings-frame))
    (is-= 12 (h2::http2-frame-length settings-frame)) ; 2 settings * 6 bytes each
    (is-= 0 (h2::http2-frame-stream-id settings-frame))))

;;;; PING Frame Tests (RFC 7540 Section 6.7)

(deftest test-ping-frame-format
  "Test PING frame must be 8 bytes"
  (let ((ping-frame (h2::make-ping-frame)))
    (is-= h2::+frame-ping+ (h2::http2-frame-type ping-frame))
    (is-= 8 (h2::http2-frame-length ping-frame))
    (is-= 0 (h2::http2-frame-stream-id ping-frame))))

(deftest test-ping-ack
  "Test PING ACK frame"
  (let ((data (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4 5 6 7 8))))
    (let ((ping-ack (h2::make-ping-frame :ack t :data data)))
      (is-= h2::+flag-ack+ (h2::http2-frame-flags ping-ack))
      (is-equalp data (h2::http2-frame-payload ping-ack)))))

;;;; GOAWAY Frame Tests (RFC 7540 Section 6.8)

(deftest test-goaway-frame-format
  "Test GOAWAY frame format"
  (let ((goaway-frame (h2::make-goaway-frame 
                      42 
                      h2::+error-protocol-error+
                      "Test error")))
    (is-= h2::+frame-goaway+ (h2::http2-frame-type goaway-frame))
    (is-= 0 (h2::http2-frame-stream-id goaway-frame))
    (is-true (>= (h2::http2-frame-length goaway-frame) 8)))) ; At least 8 bytes

;;;; WINDOW_UPDATE Frame Tests (RFC 7540 Section 6.9)

(deftest test-window-update-frame-format
  "Test WINDOW_UPDATE frame format"
  (let ((window-frame (h2::make-window-update-frame 1 1024)))
    (is-= h2::+frame-window-update+ (h2::http2-frame-type window-frame))
    (is-= 4 (h2::http2-frame-length window-frame))
    (is-= 1 (h2::http2-frame-stream-id window-frame))))

;;;; Error Code Tests (RFC 7540 Section 7)

(deftest test-error-codes
  "Test error code constants"
  (is-= #x0 h2::+error-no-error+)
  (is-= #x1 h2::+error-protocol-error+)
  (is-= #x2 h2::+error-internal-error+)
  (is-= #x3 h2::+error-flow-control-error+)
  (is-= #x4 h2::+error-settings-timeout+)
  (is-= #x5 h2::+error-stream-closed+)
  (is-= #x6 h2::+error-frame-size-error+)
  (is-= #x7 h2::+error-refused-stream+)
  (is-= #x8 h2::+error-cancel+)
  (is-= #x9 h2::+error-compression-error+)
  (is-= #xa h2::+error-connect-error+)
  (is-= #xb h2::+error-enhance-your-calm+)
  (is-= #xc h2::+error-inadequate-security+)
  (is-= #xd h2::+error-http-1-1-required+))

;;;; Flow Control Tests (RFC 7540 Section 5.2)

(deftest test-initial-window-size
  "Test default initial window size"
  (is-= 65535 h2::+default-initial-window-size+))

(deftest test-max-window-size
  "Test maximum window size"
  (is-= (1- (ash 1 31)) h2::+max-window-size+))

(deftest test-flow-control-operations
  "Test flow control window operations"
  (let ((controller (h2::make-connection-flow-controller)))
    ;; Initial state
    (is-= 65535 (h2::flow-controller-send-window controller))
    
    ;; Consume window
    (h2::consume-send-window controller 1000)
    (is-= 64535 (h2::flow-controller-send-window controller))
    
    ;; Update window
    (h2::update-send-window controller 500)
    (is-= 65035 (h2::flow-controller-send-window controller))
    
    ;; Check capacity
    (is-true (h2::can-send-p controller 65000))
    (is-not (h2::can-send-p controller 66000))))

;;;; HPACK Tests (RFC 7541)

(deftest test-hpack-encoding-decoding
  "Test HPACK header compression round-trip"
  (let ((encoder (hpack:create-encoder))
        (decoder (hpack:create-decoder))
        (headers '((":method" . "GET")
                  (":path" . "/")
                  (":scheme" . "https")
                  ("host" . "example.com"))))
    
    (let* ((encoded (hpack:encode-headers encoder headers))
           (decoded (hpack:decode-headers decoder encoded)))
      ;; Should get same headers back
      (is-= (length headers) (length decoded))
      (dolist (header headers)
        (is-not-null (member header decoded :test #'equal))))))

(deftest test-hpack-static-table
  "Test HPACK static table entries"
  ;; Test some well-known static table entries
  (let ((encoder (hpack:create-encoder))
        (decoder (hpack:create-decoder)))
    
    ;; :method GET is index 2
    (let* ((headers '((":method" . "GET")))
           (encoded (hpack:encode-headers encoder headers)))
      ;; Should be very small (just the index)
      (is-true (< (length encoded) 5))
      
      ;; Should decode correctly
      (let ((decoded (hpack:decode-headers decoder encoded)))
        (is-equal headers decoded)))))

;;;; Stream State Tests (RFC 7540 Section 5.1)

(deftest test-stream-states
  "Test stream state transitions"
  (let* ((conn (make-instance 'h2::http2-connection
                              :socket nil
                              :client-p t))
         (stream (h2::create-stream conn)))
    
    ;; Initial state
    (is-eq :idle (h2::http2-stream-state stream))
    
    ;; State transitions
    (setf (h2::http2-stream-state stream) :open)
    (is-eq :open (h2::http2-stream-state stream))
    
    (setf (h2::http2-stream-state stream) :half-closed-local)
    (is-eq :half-closed-local (h2::http2-stream-state stream))
    
    (setf (h2::http2-stream-state stream) :closed)
    (is-eq :closed (h2::http2-stream-state stream))))

(deftest test-stream-id-assignment
  "Test proper stream ID assignment"
  (let ((client-conn (make-instance 'h2::http2-connection
                                    :socket nil
                                    :client-p t))
        (server-conn (make-instance 'h2::http2-connection
                                    :socket nil
                                    :client-p nil)))
    
    ;; Client uses odd stream IDs
    (let ((stream1 (h2::create-stream client-conn))
          (stream2 (h2::create-stream client-conn)))
      (is-= 1 (h2::stream-id stream1))
      (is-= 3 (h2::stream-id stream2)))
    
    ;; Server would use even stream IDs (for push)
    ;; Note: Current implementation doesn't differentiate
    ))

;;;; Connection Tests

(deftest test-connection-creation
  "Test HTTP/2 connection creation"
  (let ((conn (make-instance 'h2::http2-connection
                            :socket :mock-socket
                            :client-p t)))
    (is-not-null conn)
    (is-true (h2::http2-connection-p conn))
    (is-true (h2::connection-client-p conn))
    (is-not-null (h2::connection-streams conn))
    (is-= 65535 (h2::connection-send-window conn))
    (is-= 65535 (h2::connection-recv-window conn))))

;;;; Frame Serialization Tests

(deftest test-frame-serialization
  "Test frame serialization and parsing"
  (let ((frame (h2::make-http2-frame
               :length 10
               :type h2::+frame-data+
               :flags h2::+flag-end-stream+
               :stream-id 1
               :payload (make-array 10 :element-type '(unsigned-byte 8)
                                      :initial-element 42))))
    
    (let ((serialized (h2::serialize-frame frame)))
      ;; Check total size
      (is-= 19 (length serialized)) ; 9 byte header + 10 byte payload
      
      ;; Check header bytes
      (is-= 0 (aref serialized 0))  ; Length high byte
      (is-= 0 (aref serialized 1))  ; Length mid byte
      (is-= 10 (aref serialized 2)) ; Length low byte
      (is-= h2::+frame-data+ (aref serialized 3)) ; Type
      (is-= h2::+flag-end-stream+ (aref serialized 4)) ; Flags
      (is-= 0 (aref serialized 5))  ; Stream ID high byte
      (is-= 0 (aref serialized 6))
      (is-= 0 (aref serialized 7))
      (is-= 1 (aref serialized 8))  ; Stream ID low byte
      
      ;; Check payload
      (loop for i from 9 below 19
            do (is-= 42 (aref serialized i))))))