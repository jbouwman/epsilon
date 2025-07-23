;;;; WebSocket Frame Handling (RFC 6455)
;;;;
;;;; This module implements WebSocket frame parsing and generation according to RFC 6455.
;;;; 
;;;; Frame Format:
;;;;      0                   1                   2                   3
;;;;      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
;;;;     +-+-+-+-+-------+-+-------------+-------------------------------+
;;;;     |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
;;;;     |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
;;;;     |N|V|V|V|       |S|             |   (if payload len==126/127)   |
;;;;     | |1|2|3|       |K|             |                               |
;;;;     +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
;;;;     |     Extended payload length continued, if payload len == 127  |
;;;;     + - - - - - - - - - - - - - - - +-------------------------------+
;;;;     |                               |Masking-key, if MASK set to 1  |
;;;;     +-------------------------------+-------------------------------+
;;;;     | Masking-key (continued)       |          Payload Data         |
;;;;     +-------------------------------- - - - - - - - - - - - - - - - +
;;;;     :                     Payload Data continued ...                :
;;;;     + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
;;;;     |                     Payload Data continued ...                |
;;;;     +---------------------------------------------------------------+

(defpackage epsilon.websocket.frame
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (bin epsilon.binary)
   (str epsilon.string))
  (:export
   ;; Frame structure
   websocket-frame
   make-frame
   frame-fin
   frame-opcode
   frame-masked
   frame-payload
   
   ;; Opcodes
   +opcode-continuation+
   +opcode-text+
   +opcode-binary+ 
   +opcode-close+
   +opcode-ping+
   +opcode-pong+
   
   ;; Frame operations
   parse-frame
   serialize-frame
   mask-payload
   unmask-payload
   
   ;; Close codes
   +close-normal+
   +close-going-away+
   +close-protocol-error+
   +close-unsupported-data+
   +close-invalid-frame-payload+
   +close-policy-violation+
   +close-message-too-big+
   +close-mandatory-extension+
   +close-internal-error+))

(in-package epsilon.websocket.frame)

;;; Constants

(defconstant +opcode-continuation+ #x0 "Continuation frame")
(defconstant +opcode-text+         #x1 "Text frame")
(defconstant +opcode-binary+       #x2 "Binary frame")
(defconstant +opcode-close+        #x8 "Connection close")
(defconstant +opcode-ping+         #x9 "Ping")
(defconstant +opcode-pong+         #xa "Pong")

;;; Close codes (RFC 6455 Section 7.4.1)
(defconstant +close-normal+                1000 "Normal closure")
(defconstant +close-going-away+            1001 "Endpoint going away")
(defconstant +close-protocol-error+        1002 "Protocol error") 
(defconstant +close-unsupported-data+      1003 "Unsupported data type")
(defconstant +close-invalid-frame-payload+ 1007 "Invalid frame payload")
(defconstant +close-policy-violation+      1008 "Policy violation")
(defconstant +close-message-too-big+       1009 "Message too big")
(defconstant +close-mandatory-extension+   1010 "Mandatory extension missing")
(defconstant +close-internal-error+        1011 "Internal server error")

;;; Frame structure

(defstruct websocket-frame
  "WebSocket frame structure"
  (fin t :type boolean)                    ; Final fragment
  (rsv1 nil :type boolean)                 ; Reserved bit 1
  (rsv2 nil :type boolean)                 ; Reserved bit 2  
  (rsv3 nil :type boolean)                 ; Reserved bit 3
  (opcode 0 :type (unsigned-byte 4))       ; Frame opcode
  (masked nil :type boolean)               ; Payload is masked
  (mask nil :type (or null (simple-array (unsigned-byte 8) (4)))) ; Masking key
  (payload #() :type (simple-array (unsigned-byte 8) (*))))       ; Frame payload

(defun make-frame (&key (fin t) (opcode +opcode-text+) (masked nil) payload)
  "Create a WebSocket frame"
  (make-websocket-frame
   :fin fin
   :opcode opcode
   :masked masked
   :payload (if (stringp payload)
                (str:string-to-octets payload)
                (or payload #()))))

;;; Masking operations

(defun generate-mask ()
  "Generate a 4-byte random masking key"
  (let ((mask (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 mask)
      (setf (aref mask i) (random 256)))))

(defun mask-payload (payload mask)
  "Apply masking to payload using 4-byte mask"
  (when (and payload mask)
    (let ((masked (make-array (length payload) :element-type '(unsigned-byte 8))))
      (dotimes (i (length payload) masked)
        (setf (aref masked i)
              (logxor (aref payload i) (aref mask (mod i 4))))))))

(defun unmask-payload (masked-payload mask)
  "Remove masking from payload using 4-byte mask"
  (mask-payload masked-payload mask)) ; XOR is its own inverse

;;; Frame parsing

(defun parse-frame (stream)
  "Parse a WebSocket frame from stream. Returns a websocket-frame or nil if incomplete."
  (let ((first-bytes (make-array 2 :element-type '(unsigned-byte 8))))
    ;; Read first 2 bytes
    (unless (= 2 (read-sequence first-bytes stream))
      (return-from parse-frame nil))
    
    (let* ((byte1 (aref first-bytes 0))
           (byte2 (aref first-bytes 1))
           (fin (logbitp 7 byte1))
           (rsv1 (logbitp 6 byte1))
           (rsv2 (logbitp 5 byte1)) 
           (rsv3 (logbitp 4 byte1))
           (opcode (logand byte1 #x0f))
           (masked (logbitp 7 byte2))
           (payload-len (logand byte2 #x7f)))
      
      ;; Validate reserved bits (must be 0 unless extension negotiated)
      (when (or rsv1 rsv2 rsv3)
        (error "Reserved bits set in WebSocket frame"))
      
      ;; Parse extended payload length
      (let ((actual-payload-len
              (cond
                ((< payload-len 126) payload-len)
                ((= payload-len 126)
                 (let ((extended-len (make-array 2 :element-type '(unsigned-byte 8))))
                   (unless (= 2 (read-sequence extended-len stream))
                     (error "Incomplete extended payload length"))
                   (bin:octets-to-uint16 extended-len :big-endian)))
                ((= payload-len 127)
                 (let ((extended-len (make-array 8 :element-type '(unsigned-byte 8))))
                   (unless (= 8 (read-sequence extended-len stream))
                     (error "Incomplete extended payload length"))
                   (bin:octets-to-uint64 extended-len :big-endian)))
                (t (error "Invalid payload length: ~D" payload-len)))))
        
        ;; Read masking key if present
        (let ((mask (when masked
                      (let ((mask-bytes (make-array 4 :element-type '(unsigned-byte 8))))
                        (unless (= 4 (read-sequence mask-bytes stream))
                          (error "Incomplete masking key"))
                        mask-bytes))))
          
          ;; Read payload
          (let ((payload (make-array actual-payload-len :element-type '(unsigned-byte 8))))
            (unless (= actual-payload-len (read-sequence payload stream))
              (error "Incomplete payload"))
            
            ;; Unmask payload if needed
            (when masked
              (setf payload (unmask-payload payload mask)))
            
            ;; Return parsed frame
            (make-websocket-frame
             :fin fin
             :rsv1 rsv1
             :rsv2 rsv2
             :rsv3 rsv3
             :opcode opcode
             :masked masked
             :mask mask
             :payload payload)))))))

;;; Frame serialization

(defun serialize-frame (frame &key (client-side nil))
  "Serialize a WebSocket frame to octets. If client-side is true, applies masking."
  (let* ((payload (websocket-frame-payload frame))
         (payload-len (length payload))
         (needs-masking (or (websocket-frame-masked frame) client-side))
         (mask (when needs-masking
                 (or (websocket-frame-mask frame) (generate-mask))))
         (masked-payload (if needs-masking
                             (mask-payload payload mask)
                             payload)))
    
    (bin:with-output-to-octets (stream)
      ;; First byte: FIN + RSV + Opcode
      (let ((byte1 (websocket-frame-opcode frame)))
        (when (websocket-frame-fin frame)
          (setf byte1 (logior byte1 #x80)))
        (when (websocket-frame-rsv1 frame)
          (setf byte1 (logior byte1 #x40)))
        (when (websocket-frame-rsv2 frame)
          (setf byte1 (logior byte1 #x20)))
        (when (websocket-frame-rsv3 frame)
          (setf byte1 (logior byte1 #x10)))
        (write-byte byte1 stream))
      
      ;; Second byte: MASK + Payload length
      (let ((byte2 (cond
                     ((< payload-len 126) payload-len)
                     ((< payload-len 65536) 126)
                     (t 127))))
        (when needs-masking
          (setf byte2 (logior byte2 #x80)))
        (write-byte byte2 stream))
      
      ;; Extended payload length
      (cond
        ((< payload-len 126)
         ;; No extended length needed
         )
        ((< payload-len 65536)
         ;; 16-bit extended length
         (let ((len-bytes (bin:uint16-to-octets payload-len :big-endian)))
           (write-sequence len-bytes stream)))
        (t
         ;; 64-bit extended length
         (let ((len-bytes (bin:uint64-to-octets payload-len :big-endian)))
           (write-sequence len-bytes stream))))
      
      ;; Masking key
      (when needs-masking
        (write-sequence mask stream))
      
      ;; Payload
      (write-sequence masked-payload stream))))

;;; Frame validation

(defun control-frame-p (opcode)
  "Check if opcode represents a control frame"
  (>= opcode #x8))

(defun data-frame-p (opcode)
  "Check if opcode represents a data frame"
  (< opcode #x8))

(defun validate-frame (frame)
  "Validate WebSocket frame according to RFC 6455"
  (let ((opcode (websocket-frame-opcode frame))
        (payload-len (length (websocket-frame-payload frame)))
        (fin (websocket-frame-fin frame)))
    
    ;; Control frames validation
    (when (control-frame-p opcode)
      ;; Control frames must not be fragmented
      (unless fin
        (error "Control frames must not be fragmented"))
      ;; Control frames must have payload <= 125 bytes
      (when (> payload-len 125)
        (error "Control frame payload too large: ~D bytes" payload-len)))
    
    ;; Reserved opcodes
    (when (and (not (member opcode (list +opcode-continuation+
                                         +opcode-text+
                                         +opcode-binary+
                                         +opcode-close+
                                         +opcode-ping+
                                         +opcode-pong+))))
      (error "Unknown opcode: ~D" opcode))
    
    t))

;;; Close frame handling

(defun make-close-frame (code &optional reason)
  "Create a close frame with status code and optional reason"
  (let ((payload (bin:with-output-to-octets (stream)
                   (let ((code-bytes (bin:uint16-to-octets code :big-endian)))
                     (write-sequence code-bytes stream))
                   (when reason
                     (let ((reason-bytes (str:string-to-octets reason)))
                       (write-sequence reason-bytes stream))))))
    (make-websocket-frame
     :fin t
     :opcode +opcode-close+
     :payload payload)))

(defun parse-close-frame (frame)
  "Parse close frame payload. Returns (values code reason)"
  (let ((payload (websocket-frame-payload frame)))
    (if (zerop (length payload))
        (values +close-normal+ nil)
        (let ((code (bin:octets-to-uint16 (subseq payload 0 2) :big-endian))
              (reason (when (> (length payload) 2)
                        (str:octets-to-string (subseq payload 2)))))
          (values code reason)))))

;;; Text frame utilities

(defun make-text-frame (text &key (fin t))
  "Create a text frame from string"
  (make-websocket-frame
   :fin fin
   :opcode +opcode-text+
   :payload (str:string-to-octets text)))

(defun make-binary-frame (data &key (fin t))
  "Create a binary frame from octets"
  (make-websocket-frame
   :fin fin
   :opcode +opcode-binary+
   :payload data))

(defun frame-text (frame)
  "Extract text from a text frame"
  (when (= (websocket-frame-opcode frame) +opcode-text+)
    (str:octets-to-string (websocket-frame-payload frame))))

(defun make-ping-frame (&optional payload)
  "Create a ping frame with optional payload"
  (make-websocket-frame
   :fin t
   :opcode +opcode-ping+
   :payload (if (stringp payload)
                (str:string-to-octets payload)
                (or payload #()))))

(defun make-pong-frame (&optional payload)
  "Create a pong frame with optional payload"
  (make-websocket-frame
   :fin t
   :opcode +opcode-pong+
   :payload (if (stringp payload)
                (str:string-to-octets payload)
                (or payload #()))))
