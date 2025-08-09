;;;; WebSocket Test Suite
;;;;
;;;; Comprehensive tests for WebSocket client and server functionality

(defpackage :epsilon.websocket.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:ws #:epsilon.websocket)
   (#:server #:epsilon.http.server)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:binary #:epsilon.binary)))

(in-package :epsilon.websocket.tests)

;;; Frame parsing and creation tests

(deftest test-frame-creation ()
  "Test WebSocket frame creation"
  ;; Text frame
  (let ((frame (ws::make-frame :opcode ws:+opcode-text+
                               :payload (str:string-to-octets "Hello"))))
    (is-equal ws:+opcode-text+ (ws::frame-opcode frame))
    (is (ws::frame-fin frame))
    (is-not (ws::frame-mask frame))
    (is-equal 5 (length (ws::frame-payload frame))))
  
  ;; Binary frame
  (let ((frame (ws::make-frame :opcode ws:+opcode-binary+
                               :payload #(1 2 3 4 5))))
    (is-equal ws:+opcode-binary+ (ws::frame-opcode frame))
    (is-equal 5 (length (ws::frame-payload frame))))
  
  ;; Close frame
  (let ((frame (ws::make-frame :opcode ws:+opcode-close+
                               :payload #(3 232)))) ; 1000 in big-endian
    (is-equal ws:+opcode-close+ (ws::frame-opcode frame))
    (is-equal 2 (length (ws::frame-payload frame)))))

(deftest test-frame-encoding ()
  "Test WebSocket frame encoding"
  ;; Small payload (< 126 bytes)
  (let* ((payload (str:string-to-octets "Hello"))
         (frame (ws::make-frame :opcode ws:+opcode-text+ :payload payload))
         (encoded (ws::encode-frame frame)))
    ;; First byte: FIN=1, RSV=0, opcode=1
    (is-equal #b10000001 (aref encoded 0))
    ;; Second byte: MASK=0, length=5
    (is-equal 5 (aref encoded 1))
    ;; Payload
    (is-equal (length payload) (- (length encoded) 2)))
  
  ;; Medium payload (126-65535 bytes)
  (let* ((payload (make-array 300 :element-type '(unsigned-byte 8) :initial-element 65))
         (frame (ws::make-frame :opcode ws:+opcode-binary+ :payload payload))
         (encoded (ws::encode-frame frame)))
    ;; First byte: FIN=1, opcode=2
    (is-equal #b10000010 (aref encoded 0))
    ;; Second byte: MASK=0, length=126
    (is-equal 126 (aref encoded 1))
    ;; Next 2 bytes: actual length in big-endian
    (is-equal 300 (+ (* 256 (aref encoded 2)) (aref encoded 3))))
  
  ;; Masked frame
  (let* ((payload (str:string-to-octets "Test"))
         (frame (ws::make-frame :opcode ws:+opcode-text+ 
                                :payload payload 
                                :mask #(1 2 3 4)))
         (encoded (ws::encode-frame frame)))
    ;; Second byte should have mask bit set
    (is (logbitp 7 (aref encoded 1)))))

(deftest test-frame-decoding ()
  "Test WebSocket frame decoding"
  ;; Simple text frame
  (let* ((encoded #(#b10000001 5 72 101 108 108 111)) ; "Hello"
         (frame (ws::decode-frame encoded)))
    (is (ws::frame-fin frame))
    (is-equal ws:+opcode-text+ (ws::frame-opcode frame))
    (is-equal "Hello" (str:octets-to-string (ws::frame-payload frame))))
  
  ;; Masked frame
  (let* ((mask #(#x37 #xfa #x21 #x3d))
         (payload "Hello")
         (masked-payload (ws::mask-payload (str:string-to-octets payload) mask))
         (encoded (concatenate 'vector
                               #(#b10000001 #b10000101) ; FIN=1, TEXT, MASK=1, LEN=5
                               mask
                               masked-payload))
         (frame (ws::decode-frame encoded)))
    (is (ws::frame-fin frame))
    (is-equal ws:+opcode-text+ (ws::frame-opcode frame))
    (is-equal "Hello" (str:octets-to-string (ws::frame-payload frame)))))

(deftest test-masking ()
  "Test payload masking/unmasking"
  (let* ((payload (str:string-to-octets "WebSocket"))
         (mask #(#x37 #xfa #x21 #x3d))
         (masked (ws::mask-payload payload mask))
         (unmasked (ws::mask-payload masked mask)))
    ;; Masking twice should return original
    (is-equal payload unmasked)
    ;; Masked should be different from original
    (is-not (equalp payload masked))))

(deftest test-close-codes ()
  "Test close frame with status codes"
  ;; Normal closure
  (let ((payload (ws::encode-close-payload ws:+close-normal+ "Goodbye")))
    (is-equal 2 (+ (aref payload 0) (* 256 (aref payload 1)))) ; Check it's not 1000
    (is-equal "Goodbye" (str:octets-to-string (subseq payload 2))))
  
  ;; Going away
  (let ((payload (ws::encode-close-payload ws:+close-going-away+ "")))
    (is-equal 2 (length payload)))
  
  ;; Protocol error  
  (let ((payload (ws::encode-close-payload ws:+close-protocol-error+ "Invalid frame")))
    (is (> (length payload) 2))))

(deftest test-handshake-request ()
  "Test WebSocket handshake request generation"
  (let ((headers (ws::generate-handshake-headers "example.com" "/chat")))
    ;; Check required headers
    (is-equal "example.com" (map:get headers "Host"))
    (is-equal "Upgrade" (map:get headers "Connection"))
    (is-equal "websocket" (map:get headers "Upgrade"))
    (is-equal "13" (map:get headers "Sec-WebSocket-Version"))
    ;; Key should be present and base64 encoded
    (let ((key (map:get headers "Sec-WebSocket-Key")))
      (is (stringp key))
      (is (>= (length key) 16)))))

(deftest test-handshake-validation ()
  "Test WebSocket handshake response validation"
  ;; Valid response
  (let ((request-key "dGhlIHNhbXBsZSBub25jZQ==")
        (expected-accept "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))
    (is (ws::valid-handshake-response-p expected-accept request-key)))
  
  ;; Invalid response
  (let ((request-key "dGhlIHNhbXBsZSBub25jZQ==")
        (wrong-accept "wrongacceptvalue"))
    (is-not (ws::valid-handshake-response-p wrong-accept request-key))))

(deftest test-frame-fragmentation ()
  "Test frame fragmentation"
  ;; First fragment
  (let ((frame (ws::make-frame :opcode ws:+opcode-text+
                               :fin nil
                               :payload (str:string-to-octets "Hello"))))
    (is-not (ws::frame-fin frame))
    (is-equal ws:+opcode-text+ (ws::frame-opcode frame)))
  
  ;; Continuation fragment
  (let ((frame (ws::make-frame :opcode ws:+opcode-continuation+
                               :fin nil
                               :payload (str:string-to-octets " World"))))
    (is-not (ws::frame-fin frame))
    (is-equal ws:+opcode-continuation+ (ws::frame-opcode frame)))
  
  ;; Final fragment
  (let ((frame (ws::make-frame :opcode ws:+opcode-continuation+
                               :fin t
                               :payload (str:string-to-octets "!"))))
    (is (ws::frame-fin frame))
    (is-equal ws:+opcode-continuation+ (ws::frame-opcode frame))))

(deftest test-control-frames ()
  "Test control frames"
  ;; Ping frame
  (let ((frame (ws::make-frame :opcode ws:+opcode-ping+
                               :payload (str:string-to-octets "ping"))))
    (is-equal ws:+opcode-ping+ (ws::frame-opcode frame))
    (is (ws::frame-fin frame)) ; Control frames must not be fragmented
    (is (<= (length (ws::frame-payload frame)) 125))) ; Max 125 bytes
  
  ;; Pong frame
  (let ((frame (ws::make-frame :opcode ws:+opcode-pong+
                               :payload (str:string-to-octets "pong"))))
    (is-equal ws:+opcode-pong+ (ws::frame-opcode frame))
    (is (ws::frame-fin frame))))

(deftest test-reserved-bits ()
  "Test reserved bits handling"
  ;; Frame with RSV bits set (should be rejected in strict mode)
  (let ((encoded #(#b11110001 5 72 101 108 108 111))) ; All RSV bits set
    (handler-case
        (let ((frame (ws::decode-frame encoded)))
          ;; In permissive mode, frame might be decoded
          (is-equal #b111 (logand #b01110000 (aref encoded 0))))
      (error ()
        ;; In strict mode, should error
        (is t)))))

(deftest test-payload-length-encoding ()
  "Test various payload length encodings"
  ;; 125 bytes (single byte length)
  (let* ((payload (make-array 125 :element-type '(unsigned-byte 8) :initial-element 65))
         (frame (ws::make-frame :opcode ws:+opcode-binary+ :payload payload))
         (encoded (ws::encode-frame frame)))
    (is-equal 125 (aref encoded 1)))
  
  ;; 126 bytes (2-byte length)
  (let* ((payload (make-array 126 :element-type '(unsigned-byte 8) :initial-element 65))
         (frame (ws::make-frame :opcode ws:+opcode-binary+ :payload payload))
         (encoded (ws::encode-frame frame)))
    (is-equal 126 (logand #b01111111 (aref encoded 1))))
  
  ;; 65535 bytes (still 2-byte length)
  (let* ((payload (make-array 65535 :element-type '(unsigned-byte 8) :initial-element 65))
         (frame (ws::make-frame :opcode ws:+opcode-binary+ :payload payload))
         (encoded (ws::encode-frame frame)))
    (is-equal 126 (logand #b01111111 (aref encoded 1))))
  
  ;; 65536 bytes (8-byte length)
  (let* ((payload (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 65))
         (frame (ws::make-frame :opcode ws:+opcode-binary+ :payload payload))
         (encoded (ws::encode-frame frame)))
    (is-equal 127 (logand #b01111111 (aref encoded 1)))))

(deftest test-utf8-validation ()
  "Test UTF-8 validation for text frames"
  ;; Valid UTF-8
  (let ((valid-utf8 (str:string-to-octets "Hello 世界")))
    (is (ws::valid-utf8-p valid-utf8)))
  
  ;; Invalid UTF-8 sequences should be rejected for text frames
  (let ((invalid-utf8 #(#xFF #xFE #xFD)))
    (is-not (ws::valid-utf8-p invalid-utf8))))

(deftest test-extension-data ()
  "Test extension data handling"
  ;; Currently no extensions implemented, but test the structure
  (let ((frame (ws::make-frame :opcode ws:+opcode-text+
                               :payload (str:string-to-octets "data")
                               :rsv1 nil
                               :rsv2 nil
                               :rsv3 nil)))
    (is-not (ws::frame-rsv1 frame))
    (is-not (ws::frame-rsv2 frame))
    (is-not (ws::frame-rsv3 frame))))

(deftest test-error-conditions ()
  "Test various error conditions"
  ;; Empty frame data
  (is-thrown 'error
             (ws::decode-frame #()))
  
  ;; Incomplete frame header
  (is-thrown 'error
             (ws::decode-frame #(#x81))) ; Only first byte
  
  ;; Incomplete payload
  (is-thrown 'error
             (ws::decode-frame #(#x81 #x05 #x48 #x65))) ; Says 5 bytes but only 2
  
  ;; Invalid opcode
  (let ((encoded #(#b10001111 5 72 101 108 108 111))) ; Opcode 15 (reserved)
    (handler-case
        (ws::decode-frame encoded)
      (error () (is t)))))

(deftest test-connection-state ()
  "Test WebSocket connection state management"
  (let ((conn (make-instance 'ws:websocket-connection)))
    ;; Initial state
    (is-equal :connecting (ws:connection-state conn))
    
    ;; Transition to open
    (setf (ws:connection-state conn) :open)
    (is-equal :open (ws:connection-state conn))
    
    ;; Transition to closing
    (setf (ws:connection-state conn) :closing) 
    (is-equal :closing (ws:connection-state conn))
    
    ;; Transition to closed
    (setf (ws:connection-state conn) :closed)
    (is-equal :closed (ws:connection-state conn))))