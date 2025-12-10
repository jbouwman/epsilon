;;;; Frame Serialization Tests
;;;;
;;;; Test-first approach for frame serialization functionality

(defpackage :epsilon.http2.frame-serialization-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:frames #:epsilon.http2.frames)
   (#:http2 #:epsilon.http2)))

(in-package :epsilon.http2.frame-serialization-tests)

;;;; Helper Functions

(defun bytes-equal-p (bytes1 bytes2)
  "Compare two byte arrays"
  (and (= (length bytes1) (length bytes2))
       (every #'= bytes1 bytes2)))

;;;; Frame Header Tests

(deftest test-frame-header-serialization ()
  "Test that frame headers serialize to exactly 9 bytes"
  ;; DATA frame header: length=10, type=0, flags=0, stream=1
  (let* ((frame (frames:make-data-frame 1 (make-array 10 :element-type '(unsigned-byte 8))))
         (serialized (http2:serialize-frame frame)))
    (is (= 19 (length serialized))) ; 9 byte header + 10 byte payload
    
    ;; Check header bytes
    (is (= 0 (aref serialized 0))) ; Length high byte
    (is (= 0 (aref serialized 1))) ; Length mid byte  
    (is (= 10 (aref serialized 2))) ; Length low byte
    (is (= frames:+frame-data+ (aref serialized 3))) ; Type
    (is (= 0 (aref serialized 4))) ; Flags
    (is (= 0 (aref serialized 5))) ; Stream ID high byte
    (is (= 0 (aref serialized 6))) 
    (is (= 0 (aref serialized 7)))
    (is (= 1 (aref serialized 8))))) ; Stream ID low byte

(deftest test-frame-length-encoding ()
  "Test 24-bit length field encoding"
  ;; Test maximum length (2^24 - 1)
  (let* ((max-length #xFFFFFF)
         (payload (make-array max-length :element-type '(unsigned-byte 8)))
         (frame (frames:make-data-frame 1 payload))
         (serialized (http2:serialize-frame frame)))
    ;; Check length bytes
    (is (= #xFF (aref serialized 0)))
    (is (= #xFF (aref serialized 1)))
    (is (= #xFF (aref serialized 2)))))

(deftest test-stream-id-encoding ()
  "Test 31-bit stream ID encoding with reserved bit"
  ;; Test maximum stream ID (2^31 - 1)
  (let* ((max-stream-id #x7FFFFFFF)
         (frame (frames:make-data-frame max-stream-id (make-array 0 :element-type '(unsigned-byte 8))))
         (serialized (http2:serialize-frame frame)))
    ;; Check stream ID bytes (bit 31 must be 0)
    (is (= #x7F (aref serialized 5))) ; High byte with reserved bit clear
    (is (= #xFF (aref serialized 6)))
    (is (= #xFF (aref serialized 7)))
    (is (= #xFF (aref serialized 8)))))

;;;; Frame Type Tests

(deftest test-data-frame-serialization ()
  "Test DATA frame serialization"
  (let* ((data #(72 101 108 108 111)) ; "Hello"
         (frame (frames:make-data-frame 1 data :end-stream t))
         (serialized (http2:serialize-frame frame)))
    ;; Check header
    (is (= 5 (logior (ash (aref serialized 0) 16)
                     (ash (aref serialized 1) 8)
                     (aref serialized 2)))) ; Length
    (is (= frames:+frame-data+ (aref serialized 3))) ; Type
    (is (= frames:+flag-end-stream+ (aref serialized 4))) ; Flags
    
    ;; Check payload
    (is (bytes-equal-p data (subseq serialized 9)))))

(deftest test-headers-frame-serialization ()
  "Test HEADERS frame serialization"
  (let* ((headers #(130 132)) ; Encoded headers
         (frame (frames:make-headers-frame 1 headers 
                                          :end-stream t 
                                          :end-headers t))
         (serialized (http2:serialize-frame frame)))
    ;; Check type and flags
    (is (= frames:+frame-headers+ (aref serialized 3)))
    (is (= (logior frames:+flag-end-stream+ frames:+flag-end-headers+)
           (aref serialized 4)))))

(deftest test-priority-frame-serialization ()
  "Test PRIORITY frame serialization (always 5 bytes)"
  (let* ((frame (frames:make-priority-frame 3 
                                           :dependency 1 
                                           :weight 16 
                                           :exclusive t))
         (serialized (http2:serialize-frame frame)))
    ;; Check frame size
    (is (= 5 (logior (ash (aref serialized 0) 16)
                     (ash (aref serialized 1) 8)
                     (aref serialized 2))))
    
    ;; Check payload format
    (let ((payload (subseq serialized 9)))
      ;; First bit is exclusive flag, rest is dependency
      (is (= #x80 (logand #x80 (aref payload 0)))) ; Exclusive bit set
      (is (= 1 (logior (ash (logand #x7F (aref payload 0)) 24)
                       (ash (aref payload 1) 16)
                       (ash (aref payload 2) 8)
                       (aref payload 3))))
      ;; Weight is last byte (actual weight - 1)
      (is (= 15 (aref payload 4)))))) ; 16 - 1

(deftest test-rst-stream-frame-serialization ()
  "Test RST_STREAM frame serialization (always 4 bytes)"
  (let* ((frame (frames:make-rst-stream-frame 1 frames:+error-protocol-error+))
         (serialized (http2:serialize-frame frame)))
    ;; Check frame size
    (is (= 4 (logior (ash (aref serialized 0) 16)
                     (ash (aref serialized 1) 8)
                     (aref serialized 2))))
    
    ;; Check error code
    (let ((error-code (logior (ash (aref serialized 9) 24)
                              (ash (aref serialized 10) 16)
                              (ash (aref serialized 11) 8)
                              (aref serialized 12))))
      (is (= frames:+error-protocol-error+ error-code)))))

(deftest test-settings-frame-serialization ()
  "Test SETTINGS frame serialization"
  ;; Empty settings (ACK)
  (let* ((ack-frame (frames:make-settings-frame :ack t))
         (serialized (http2:serialize-frame ack-frame)))
    (is (= 0 (logior (ash (aref serialized 0) 16)
                     (ash (aref serialized 1) 8)
                     (aref serialized 2)))) ; Length = 0
    (is (= frames:+flag-ack+ (aref serialized 4))))
  
  ;; Settings with parameters (6 bytes each)
  (let* ((settings `((,frames:+settings-initial-window-size+ . 65535)
                    (,frames:+settings-max-frame-size+ . 16384)))
         (frame (frames:make-settings-frame :initial-settings settings))
         (serialized (http2:serialize-frame frame)))
    (is (= 12 (logior (ash (aref serialized 0) 16)
                      (ash (aref serialized 1) 8)
                      (aref serialized 2)))) ; 2 settings * 6 bytes
    
    ;; Check first setting
    (is (= frames:+settings-initial-window-size+
           (logior (ash (aref serialized 9) 8)
                   (aref serialized 10))))
    (is (= 65535 (logior (ash (aref serialized 11) 24)
                         (ash (aref serialized 12) 16)
                         (ash (aref serialized 13) 8)
                         (aref serialized 14))))))

(deftest test-ping-frame-serialization ()
  "Test PING frame serialization (always 8 bytes)"
  (let* ((ping-data #(1 2 3 4 5 6 7 8))
         (frame (frames:make-ping-frame :data ping-data))
         (serialized (http2:serialize-frame frame)))
    ;; Check frame size
    (is (= 8 (logior (ash (aref serialized 0) 16)
                     (ash (aref serialized 1) 8)
                     (aref serialized 2))))
    
    ;; Check payload
    (is (bytes-equal-p ping-data (subseq serialized 9)))))

(deftest test-goaway-frame-serialization ()
  "Test GOAWAY frame serialization"
  (let* ((debug-data "Connection error")
         (debug-bytes (epsilon.string:string-to-octets debug-data))
         (frame (frames:make-goaway-frame 31 
                                         frames:+error-protocol-error+ 
                                         debug-data))
         (serialized (http2:serialize-frame frame)))
    ;; Frame must be at least 8 bytes (stream ID + error code)
    (let ((length (logior (ash (aref serialized 0) 16)
                          (ash (aref serialized 1) 8)
                          (aref serialized 2))))
      (is (>= length 8))
      
      ;; Check last stream ID
      (is (= 31 (logior (ash (logand #x7F (aref serialized 9)) 24)
                        (ash (aref serialized 10) 16)
                        (ash (aref serialized 11) 8)
                        (aref serialized 12))))
      
      ;; Check error code
      (is (= frames:+error-protocol-error+
             (logior (ash (aref serialized 13) 24)
                     (ash (aref serialized 14) 16)
                     (ash (aref serialized 15) 8)
                     (aref serialized 16))))
      
      ;; Check debug data if present
      (when (> length 8)
        (is (bytes-equal-p debug-bytes (subseq serialized 17)))))))

(deftest test-window-update-frame-serialization ()
  "Test WINDOW_UPDATE frame serialization (always 4 bytes)"
  (let* ((frame (frames:make-window-update-frame 1 65535))
         (serialized (http2:serialize-frame frame)))
    ;; Check frame size
    (is (= 4 (logior (ash (aref serialized 0) 16)
                     (ash (aref serialized 1) 8)
                     (aref serialized 2))))
    
    ;; Check increment (bit 31 must be 0)
    (is (= 0 (logand #x80 (aref serialized 9)))) ; Reserved bit clear
    (is (= 65535 (logior (ash (logand #x7F (aref serialized 9)) 24)
                         (ash (aref serialized 10) 16)
                         (ash (aref serialized 11) 8)
                         (aref serialized 12))))))

(deftest test-continuation-frame-serialization ()
  "Test CONTINUATION frame serialization"
  (let* ((headers #(130 132 133))
         (frame (frames:make-continuation-frame 1 headers :end-headers t))
         (serialized (http2:serialize-frame frame)))
    ;; Check type and flags
    (is (= frames:+frame-continuation+ (aref serialized 3)))
    (is (= frames:+flag-end-headers+ (aref serialized 4)))
    
    ;; Check payload
    (is (bytes-equal-p headers (subseq serialized 9)))))

;;;; Round-trip Tests

(deftest test-frame-serialization-round-trip ()
  "Test that frames can be serialized and deserialized"
  ;; Test each frame type
  (let ((test-frames
         (list
          (frames:make-data-frame 1 #(1 2 3 4 5))
          (frames:make-headers-frame 3 #(130 132))
          (frames:make-priority-frame 5 :dependency 3 :weight 32)
          (frames:make-rst-stream-frame 7 frames:+error-cancel+)
          (frames:make-settings-frame :initial-settings 
                                     `((,frames:+settings-max-frame-size+ . 32768)))
          (frames:make-ping-frame :data #(8 7 6 5 4 3 2 1))
          (frames:make-goaway-frame 9 frames:+error-no-error+ "Goodbye")
          (frames:make-window-update-frame 11 4096)
          (frames:make-continuation-frame 13 #(135 136)))))
    
    (dolist (original test-frames)
      (let* ((serialized (http2:serialize-frame original))
             (deserialized (http2:deserialize-frame serialized)))
        ;; Check that key properties match
        (is (= (frames:http2-frame-type original)
               (frames:http2-frame-type deserialized)))
        (is (= (frames:http2-frame-flags original)
               (frames:http2-frame-flags deserialized)))
        (is (= (frames:http2-frame-stream-id original)
               (frames:http2-frame-stream-id deserialized)))
        (is (bytes-equal-p (frames:http2-frame-payload original)
                           (frames:http2-frame-payload deserialized)))))))