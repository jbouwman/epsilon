;;;; WebSocket Frame Tests

(defpackage epsilon.websocket.frame.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (frame epsilon.websocket.frame)
   (str epsilon.string)
   (bin epsilon.binary)
   (stream epsilon.stream)
   (map epsilon.map)))

(in-package epsilon.websocket.frame.tests)

;;; Test helper - convert octets to stream for testing
(defun octets-to-stream (octets)
  "Convert byte array to input stream for testing"
  (stream:make-input-stream octets))

(deftest frame-creation
  "Test basic frame creation"
  (let ((text-frame (frame:make-text-frame "Hello")))
    (is (frame:websocket-frame-p text-frame))
    (is (frame:websocket-frame-fin text-frame))
    (is-= (frame:websocket-frame-opcode text-frame) frame:+opcode-text+)
    (is-equal "Hello" (frame:frame-text text-frame)))
  
  (let ((binary-frame (frame:make-binary-frame #(1 2 3 4))))
    (is (frame:websocket-frame-p binary-frame))
    (is-= (frame:websocket-frame-opcode binary-frame) frame:+opcode-binary+)
    (is-equalp #(1 2 3 4) (frame:websocket-frame-payload binary-frame)))
  
  (let ((close-frame (frame:make-close-frame frame:+close-normal+ "Goodbye")))
    (is (frame:websocket-frame-p close-frame))
    (is-= (frame:websocket-frame-opcode close-frame) frame:+opcode-close+)))

(deftest frame-serialization
  "Test frame serialization and parsing"
  (let* ((original-frame (frame:make-text-frame "Test message"))
         (serialized (frame:serialize-frame original-frame))
         (parsed-frame (frame:parse-frame (octets-to-stream serialized))))
    
    (is (frame:websocket-frame-p parsed-frame))
    (is-equal (frame:websocket-frame-fin original-frame)
              (frame:websocket-frame-fin parsed-frame))
    (is-= (frame:websocket-frame-opcode original-frame)
          (frame:websocket-frame-opcode parsed-frame))
    (is-equalp (frame:websocket-frame-payload original-frame)
               (frame:websocket-frame-payload parsed-frame))))

(deftest frame-masking
  "Test frame masking and unmasking"
  (let* ((payload #(72 101 108 108 111)) ; "Hello"
         (mask #(1 2 3 4))
         (masked (frame:mask-payload payload mask))
         (unmasked (frame:unmask-payload masked mask)))
    
    (is-not (equalp payload masked))
    (is-equalp payload unmasked)))

(deftest client-side-masking
  "Test that client-side frames are masked"
  (let* ((frame (frame:make-text-frame "Hello"))
         (serialized (frame:serialize-frame frame :client-side t)))
    
    ;; Check mask bit is set (second byte, bit 7)
    (is (logbitp 7 (aref serialized 1)))))

(deftest server-side-no-masking
  "Test that server-side frames are not masked"
  (let* ((frame (frame:make-text-frame "Hello"))
         (serialized (frame:serialize-frame frame :client-side nil)))
    
    ;; Check mask bit is not set (second byte, bit 7)
    (is-not (logbitp 7 (aref serialized 1)))))

(deftest extended-payload-length
  "Test extended payload length encoding"
  ;; Test 16-bit extended length (126-65535 bytes)
  (let* ((large-payload (make-array 1000 :element-type '(unsigned-byte 8) :initial-element 65))
         (frame (frame:make-binary-frame large-payload))
         (serialized (frame:serialize-frame frame))
         (parsed (frame:parse-frame (octets-to-stream serialized))))
    
    (is-= (length (frame:websocket-frame-payload parsed)) 1000)
    (is-equalp large-payload (frame:websocket-frame-payload parsed)))
  
  ;; Test normal length (< 126 bytes)
  (let* ((small-payload (make-array 50 :element-type '(unsigned-byte 8) :initial-element 66))
         (frame (frame:make-binary-frame small-payload))
         (serialized (frame:serialize-frame frame))
         (parsed (frame:parse-frame (octets-to-stream serialized))))
    
    (is-= (length (frame:websocket-frame-payload parsed)) 50)
    (is-equalp small-payload (frame:websocket-frame-payload parsed))))

(deftest close-frame-parsing
  "Test close frame creation and parsing"
  (let* ((close-frame (frame:make-close-frame frame:+close-normal+ "Test reason"))
         (serialized (frame:serialize-frame close-frame))
         (parsed (frame:parse-frame (octets-to-stream serialized))))
    
    (multiple-value-bind (code reason)
        (frame:parse-close-frame parsed)
      (is-= code frame:+close-normal+)
      (is-equal reason "Test reason"))))

(deftest ping-pong-frames
  "Test ping and pong frame creation"
  (let* ((ping-payload "ping data")
         (ping-frame (frame:make-ping-frame ping-payload))
         (pong-frame (frame:make-pong-frame ping-payload)))
    
    (is-= (frame:websocket-frame-opcode ping-frame) frame:+opcode-ping+)
    (is-= (frame:websocket-frame-opcode pong-frame) frame:+opcode-pong+)
    (is-equal ping-payload (str:octets-to-string (frame:websocket-frame-payload ping-frame)))
    (is-equal ping-payload (str:octets-to-string (frame:websocket-frame-payload pong-frame)))))

(deftest frame-validation
  "Test frame validation"
  (let ((valid-frame (frame:make-text-frame "Valid")))
    (is (frame:validate-frame valid-frame)))
  
  ;; Test control frame with large payload should fail
  (let ((invalid-close (frame:make-websocket-frame
                       :fin t
                       :opcode frame:+opcode-close+
                       :payload (make-array 200 :element-type '(unsigned-byte 8)))))
    (is-thrown (error) (frame:validate-frame invalid-close))))

(deftest fragmented-message
  "Test fragmented message handling"
  (let* ((message "This is a long message that will be fragmented")
         (part1 (subseq message 0 20))
         (part2 (subseq message 20))
         
         ;; Create fragmented frames
         (first-frame (frame:make-text-frame part1 :fin nil))
         (final-frame (frame:make-websocket-frame
                      :fin t
                      :opcode frame:+opcode-continuation+
                      :payload (str:string-to-octets part2))))
    
    (is-not (frame:websocket-frame-fin first-frame))
    (is-= (frame:websocket-frame-opcode first-frame) frame:+opcode-text+)
    
    (is (frame:websocket-frame-fin final-frame))
    (is-= (frame:websocket-frame-opcode final-frame) frame:+opcode-continuation+)))

(deftest binary-frame-handling
  "Test binary frame operations"
  (let* ((binary-data #(0 1 2 3 4 5 255 254 253))
         (frame (frame:make-binary-frame binary-data))
         (serialized (frame:serialize-frame frame))
         (parsed (frame:parse-frame (octets-to-stream serialized))))
    
    (is-= (frame:websocket-frame-opcode parsed) frame:+opcode-binary+)
    (is-equalp binary-data (frame:websocket-frame-payload parsed))))

(deftest empty-payload
  "Test frames with empty payloads"
  (let* ((empty-text (frame:make-text-frame ""))
         (empty-binary (frame:make-binary-frame #()))
         (empty-ping (frame:make-ping-frame)))
    
    (is-= (length (frame:websocket-frame-payload empty-text)) 0)
    (is-= (length (frame:websocket-frame-payload empty-binary)) 0)
    (is-= (length (frame:websocket-frame-payload empty-ping)) 0)
    
    ;; Test serialization/parsing of empty frames
    (dolist (frame (list empty-text empty-binary empty-ping))
      (let* ((serialized (frame:serialize-frame frame))
             (parsed (frame:parse-frame (octets-to-stream serialized))))
        (is-= (length (frame:websocket-frame-payload parsed)) 0)))))

(deftest reserved-bits
  "Test reserved bit handling"
  ;; Reserved bits should be 0 and cause error if set
  (let ((frame-with-rsv (frame:make-websocket-frame
                        :fin t
                        :rsv1 t  ; Set reserved bit
                        :opcode frame:+opcode-text+
                        :payload (str:string-to-octets "test"))))
    
    (is-thrown (error) (frame:validate-frame frame-with-rsv))))

(deftest opcode-validation
  "Test opcode validation"
  ;; Test known opcodes
  (dolist (opcode (list frame:+opcode-continuation+
                       frame:+opcode-text+
                       frame:+opcode-binary+
                       frame:+opcode-close+
                       frame:+opcode-ping+
                       frame:+opcode-pong+))
    (let ((frame (frame:make-websocket-frame
                 :fin t
                 :opcode opcode
                 :payload (make-array 0 :element-type '(unsigned-byte 8)))))
      (is (frame:validate-frame frame))))
  
  ;; Test unknown opcode should fail
  (let ((frame (frame:make-websocket-frame
               :fin t
               :opcode #xF  ; Unknown opcode
               :payload (make-array 0 :element-type '(unsigned-byte 8)))))
    (is-thrown (error) (frame:validate-frame frame))))
