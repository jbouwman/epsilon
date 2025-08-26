;;;; HTTP/2 Conformance Tests
;;;;
;;;; Tests to verify RFC 7540 compliance

(defpackage :epsilon.http2.conformance-tests
  (:use :cl :epsilon.test))

(in-package :epsilon.http2.conformance-tests)

;;;; Frame Format Tests (RFC 7540 Section 4)

(deftest test-frame-header-format
  "Test that frame headers are 9 bytes"
  (skip "serialize-frame not yet implemented"))

(deftest test-frame-length-limits
  "Test 24-bit length field limits"
  ;; Maximum frame length is 2^24 - 1
  (let ((max-length (1- (ash 1 24))))
    (is-= #xffffff max-length)
    
    ;; Test frame with max length
    (let ((frame (epsilon.http2.frames:make-http2-frame
                 :length max-length
                 :type epsilon.http2.frames:+frame-data+
                 :flags 0
                 :stream-id 1)))
      (is-= max-length (epsilon.http2.frames:http2-frame-length frame)))))

(deftest test-stream-id-limits
  "Test 31-bit stream ID limits"
  ;; Maximum stream ID is 2^31 - 1
  (let ((max-stream-id (1- (ash 1 31))))
    (is-= #x7fffffff max-stream-id)
    
    ;; Test frame with max stream ID
    (let ((frame (epsilon.http2.frames:make-http2-frame
                 :length 0
                 :type epsilon.http2.frames:+frame-data+
                 :flags 0
                 :stream-id max-stream-id)))
      (is-= max-stream-id (epsilon.http2.frames:http2-frame-stream-id frame)))))

;;;; Connection Preface Tests (RFC 7540 Section 3.5)

(deftest test-connection-preface
  "Test HTTP/2 connection preface"
  (is-equal (format nil "PRI * HTTP/2.0~C~C~C~CSM~C~C~C~C" 
                     #\Return #\Newline #\Return #\Newline
                     #\Return #\Newline #\Return #\Newline)
            epsilon.http2::+http2-preface+)
  (is-= 24 (length epsilon.http2::+http2-preface+)))

;;;; Frame Type Tests (RFC 7540 Section 6)

(deftest test-frame-types
  "Test frame type constants"
  (is-= #x0 epsilon.http2.frames:+frame-data+)
  (is-= #x1 epsilon.http2.frames:+frame-headers+)
  (is-= #x2 epsilon.http2.frames:+frame-priority+)
  (is-= #x3 epsilon.http2.frames:+frame-rst-stream+)
  (is-= #x4 epsilon.http2.frames:+frame-settings+)
  (is-= #x5 epsilon.http2.frames:+frame-push-promise+)
  (is-= #x6 epsilon.http2.frames:+frame-ping+)
  (is-= #x7 epsilon.http2.frames:+frame-goaway+)
  (is-= #x8 epsilon.http2.frames:+frame-window-update+)
  (is-= #x9 epsilon.http2.frames:+frame-continuation+))

;;;; Settings Tests (RFC 7540 Section 6.5)

(deftest test-settings-parameters
  "Test settings parameter IDs"
  (is-= #x1 epsilon.http2.frames:+settings-header-table-size+)
  (is-= #x2 epsilon.http2.frames:+settings-enable-push+)
  (is-= #x3 epsilon.http2.frames:+settings-max-concurrent-streams+)
  (is-= #x4 epsilon.http2.frames:+settings-initial-window-size+)
  (is-= #x5 epsilon.http2.frames:+settings-max-frame-size+)
  (is-= #x6 epsilon.http2.frames:+settings-max-header-list-size+))

(deftest test-settings-frame-format
  "Test SETTINGS frame format"
  ;; Empty settings (ACK)
  (let ((ack-frame (epsilon.http2.frames:make-settings-frame :ack t)))
    (is-= epsilon.http2.frames:+frame-settings+ (epsilon.http2.frames:http2-frame-type ack-frame))
    (is-= epsilon.http2.frames:+flag-ack+ (epsilon.http2.frames:http2-frame-flags ack-frame))
    (is-= 0 (epsilon.http2.frames:http2-frame-length ack-frame))
    (is-= 0 (epsilon.http2.frames:http2-frame-stream-id ack-frame)))
  
  ;; Settings with parameters
  (let ((settings-frame (epsilon.http2.frames:make-settings-frame 
                        :initial-settings
                        (list (cons epsilon.http2.frames:+settings-initial-window-size+ 65535)
                              (cons epsilon.http2.frames:+settings-max-frame-size+ 16384)))))
    (is-= epsilon.http2.frames:+frame-settings+ (epsilon.http2.frames:http2-frame-type settings-frame))
    (is-= 0 (epsilon.http2.frames:http2-frame-flags settings-frame))
    (is-= 12 (epsilon.http2.frames:http2-frame-length settings-frame)) ; 2 settings * 6 bytes each
    (is-= 0 (epsilon.http2.frames:http2-frame-stream-id settings-frame))))

;;;; PING Frame Tests (RFC 7540 Section 6.7)

(deftest test-ping-frame-format
  "Test PING frame must be 8 bytes"
  (let ((ping-frame (epsilon.http2.frames:make-ping-frame)))
    (is-= epsilon.http2.frames:+frame-ping+ (epsilon.http2.frames:http2-frame-type ping-frame))
    (is-= 8 (epsilon.http2.frames:http2-frame-length ping-frame))
    (is-= 0 (epsilon.http2.frames:http2-frame-stream-id ping-frame))))

(deftest test-ping-ack
  "Test PING ACK frame"
  (let ((data (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4 5 6 7 8))))
    (let ((ping-ack (epsilon.http2.frames:make-ping-frame :ack t :data data)))
      (is-= epsilon.http2.frames:+flag-ack+ (epsilon.http2.frames:http2-frame-flags ping-ack))
      (is-equalp data (epsilon.http2.frames:http2-frame-payload ping-ack)))))

;;;; GOAWAY Frame Tests (RFC 7540 Section 6.8)

(deftest test-goaway-frame-format
  "Test GOAWAY frame format"
  (let ((goaway-frame (epsilon.http2.frames:make-goaway-frame 
                      42 
                      epsilon.http2.frames:+error-protocol-error+
                      "Test error")))
    (is-= epsilon.http2.frames:+frame-goaway+ (epsilon.http2.frames:http2-frame-type goaway-frame))
    (is-= 0 (epsilon.http2.frames:http2-frame-stream-id goaway-frame))
    (is-true (>= (epsilon.http2.frames:http2-frame-length goaway-frame) 8)))) ; At least 8 bytes

;;;; WINDOW_UPDATE Frame Tests (RFC 7540 Section 6.9)

(deftest test-window-update-frame-format
  "Test WINDOW_UPDATE frame format"
  (let ((window-frame (epsilon.http2.frames:make-window-update-frame 1 1024)))
    (is-= epsilon.http2.frames:+frame-window-update+ (epsilon.http2.frames:http2-frame-type window-frame))
    (is-= 4 (epsilon.http2.frames:http2-frame-length window-frame))
    (is-= 1 (epsilon.http2.frames:http2-frame-stream-id window-frame))))

;;;; Error Code Tests (RFC 7540 Section 7)

(deftest test-error-codes
  "Test error code constants"
  (is-= #x0 epsilon.http2.frames:+error-no-error+)
  (is-= #x1 epsilon.http2.frames:+error-protocol-error+)
  (is-= #x2 epsilon.http2.frames:+error-internal-error+)
  (is-= #x3 epsilon.http2.frames:+error-flow-control-error+)
  (is-= #x4 epsilon.http2.frames:+error-settings-timeout+)
  (is-= #x5 epsilon.http2.frames:+error-stream-closed+)
  (is-= #x6 epsilon.http2.frames:+error-frame-size-error+)
  (is-= #x7 epsilon.http2.frames:+error-refused-stream+)
  (is-= #x8 epsilon.http2.frames:+error-cancel+)
  (is-= #x9 epsilon.http2.frames:+error-compression-error+)
  (is-= #xa epsilon.http2.frames:+error-connect-error+)
  (is-= #xb epsilon.http2.frames:+error-enhance-your-calm+)
  (is-= #xc epsilon.http2.frames:+error-inadequate-security+)
  (is-= #xd epsilon.http2.frames:+error-http-1-1-required+))

;;;; Flow Control Tests (RFC 7540 Section 5.2)

(deftest test-initial-window-size
  "Test default initial window size"
  (is-= 65535 epsilon.http2.flow-control:+default-initial-window-size+))

(deftest test-max-window-size
  "Test maximum window size"
  (is-= (1- (ash 1 31)) epsilon.http2.flow-control:+max-window-size+))

(deftest test-flow-control-operations
  "Test flow control window operations"
  (let ((controller (epsilon.http2.flow-control:make-connection-flow-controller)))
    ;; Initial state
    (is-= 65535 (epsilon.http2.flow-control:flow-controller-send-window controller))
    
    ;; Consume window
    (epsilon.http2.flow-control:consume-send-window controller 1000)
    (is-= 64535 (epsilon.http2.flow-control:flow-controller-send-window controller))
    
    ;; Update window
    (epsilon.http2.flow-control:update-send-window controller 500)
    (is-= 65035 (epsilon.http2.flow-control:flow-controller-send-window controller))
    
    ;; Check capacity
    (is-true (epsilon.http2.flow-control:can-send-p controller 65000))
    (is-not (epsilon.http2.flow-control:can-send-p controller 66000))))

;;;; HPACK Tests (RFC 7541)

(deftest test-hpack-encoding-decoding
  "Test HPACK header compression round-trip"
  (skip "HPACK functions not yet fully implemented"))

(deftest test-hpack-static-table
  "Test HPACK static table entries"
  (skip "HPACK functions not yet fully implemented"))

;;;; Stream State Tests (RFC 7540 Section 5.1)

(deftest test-stream-states
  "Test stream state transitions"
  (let* ((conn (make-instance 'epsilon.http2::http2-connection
                              :socket nil
                              :client-p t))
         (stream (epsilon.http2::create-stream conn)))
    
    ;; Initial state - use numeric constants
    (is-= epsilon.http2.stream:+stream-idle+ 
          (epsilon.http2.stream:http2-stream-state stream))
    
    ;; State transitions
    (setf (epsilon.http2.stream:http2-stream-state stream) 
          epsilon.http2.stream:+stream-open+)
    (is-= epsilon.http2.stream:+stream-open+ 
          (epsilon.http2.stream:http2-stream-state stream))
    
    (setf (epsilon.http2.stream:http2-stream-state stream) 
          epsilon.http2.stream:+stream-half-closed-local+)
    (is-= epsilon.http2.stream:+stream-half-closed-local+ 
          (epsilon.http2.stream:http2-stream-state stream))
    
    (setf (epsilon.http2.stream:http2-stream-state stream) 
          epsilon.http2.stream:+stream-closed+)
    (is-= epsilon.http2.stream:+stream-closed+ 
          (epsilon.http2.stream:http2-stream-state stream))))

(deftest test-stream-id-assignment
  "Test proper stream ID assignment"
  (let ((client-conn (make-instance 'epsilon.http2::http2-connection
                                    :socket nil
                                    :client-p t))
        (server-conn (make-instance 'epsilon.http2::http2-connection
                                    :socket nil
                                    :client-p nil)))
    
    ;; Client uses odd stream IDs
    (let ((stream1 (epsilon.http2::create-stream client-conn))
          (stream2 (epsilon.http2::create-stream client-conn)))
      (is-= 1 (epsilon.http2::stream-id stream1))
      (is-= 3 (epsilon.http2::stream-id stream2)))
    
    ;; Server would use even stream IDs (for push)
    ;; Note: Current implementation doesn't differentiate
    ))

;;;; Connection Tests

(deftest test-connection-creation
  "Test HTTP/2 connection creation"
  (let ((conn (make-instance 'epsilon.http2::http2-connection
                            :socket :mock-socket
                            :client-p t)))
    (is-not-null conn)
    (is-true (epsilon.http2::http2-connection-p conn))
    (is-true (epsilon.http2::connection-client-p conn))
    (is-not-null (epsilon.http2::connection-streams conn))
    (is-= 65535 (epsilon.http2::connection-send-window conn))
    (is-= 65535 (epsilon.http2::connection-recv-window conn))))

;;;; Frame Serialization Tests

(deftest test-frame-serialization
  "Test frame serialization and parsing"
  (skip "serialize-frame not yet implemented"))
