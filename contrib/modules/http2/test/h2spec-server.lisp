;;;; Simple HTTP/2 Test Server for h2spec
;;;;
;;;; A minimal HTTP/2 server that can be tested with h2spec conformance tool

(format t "Loading HTTP/2 test server...~%")

;; Check if network module is loaded
(unless (find-package :epsilon.net)
  (let ((platform (string-downcase (symbol-name (epsilon.sys.env:platform)))))
    (format t "Detected platform: ~A~%" platform)
    (error "Please run with platform module loaded: ./epsilon --module epsilon.~A --module epsilon.crypto --eval \"(load \\\"test-http2-server.lisp\\\")\"" platform)))

;;;; Simple HTTP/2 Server Implementation

(defparameter *server-port* 8080)
(defparameter *server-running* nil)
(defparameter *server-thread* nil)

(defun handle-http2-connection (client-socket)
  "Handle an HTTP/2 connection"
  (format t "New connection accepted~%")
  
  (handler-case
      (let ((input-stream (epsilon.net:tcp-stream-reader client-socket))
            (output-stream (epsilon.net:tcp-stream-writer client-socket)))
        
        ;; Read client preface (24 bytes)
        (let ((preface (make-array 24 :element-type '(unsigned-byte 8)))
              (expected #(80 82 73 32 42 32 72 84 84 80 47 50 46 48 13 10 13 10 83 77 13 10 13 10)))
          (let ((bytes-read (read-sequence preface input-stream)))
            (unless (and (= bytes-read 24)
                        (equalp preface expected))
              (error "Invalid client preface: got ~S" preface))))
        
        (format t "Client preface received~%")
        
        ;; Send server preface (SETTINGS frame)
        (send-initial-settings output-stream)
        
        ;; Main frame processing loop
        (loop while *server-running*
              do (let ((frame (read-frame-simple input-stream)))
                   (when frame
                     (handle-frame-simple output-stream frame)))))
    
    (error (e)
      (format t "Connection error: ~A~%" e)))
  
  (handler-case
      (epsilon.net:tcp-shutdown client-socket :how :both)
    (error (e)
      (format t "Error closing connection: ~A~%" e))))

(defun send-initial-settings (stream)
  "Send initial SETTINGS frame"
  ;; SETTINGS frame: length=18, type=4, flags=0, stream=0
  (let ((frame (make-array 27 :element-type '(unsigned-byte 8))))
    ;; Header
    (setf (aref frame 0) 0)   ; Length high
    (setf (aref frame 1) 0)   ; Length mid  
    (setf (aref frame 2) 18)  ; Length low (3 settings * 6 bytes)
    (setf (aref frame 3) 4)   ; Type = SETTINGS
    (setf (aref frame 4) 0)   ; Flags
    (setf (aref frame 5) 0)   ; Stream ID
    (setf (aref frame 6) 0)
    (setf (aref frame 7) 0)
    (setf (aref frame 8) 0)
    
    ;; Setting 1: MAX_CONCURRENT_STREAMS = 100
    (setf (aref frame 9) 0)
    (setf (aref frame 10) 3)  ; ID = 3
    (setf (aref frame 11) 0)
    (setf (aref frame 12) 0)
    (setf (aref frame 13) 0)
    (setf (aref frame 14) 100) ; Value = 100
    
    ;; Setting 2: INITIAL_WINDOW_SIZE = 65535
    (setf (aref frame 15) 0)
    (setf (aref frame 16) 4)  ; ID = 4
    (setf (aref frame 17) 0)
    (setf (aref frame 18) 0)
    (setf (aref frame 19) #xff)
    (setf (aref frame 20) #xff) ; Value = 65535
    
    ;; Setting 3: MAX_FRAME_SIZE = 16384
    (setf (aref frame 21) 0)
    (setf (aref frame 22) 5)  ; ID = 5
    (setf (aref frame 23) 0)
    (setf (aref frame 24) 0)
    (setf (aref frame 25) #x40)
    (setf (aref frame 26) 0)   ; Value = 16384
    
    (write-sequence frame stream)
    (force-output stream)
    (format t "Sent initial SETTINGS frame~%")))

(defun read-frame-simple (stream)
  "Read a simple frame header"
  (let ((header (make-array 9 :element-type '(unsigned-byte 8))))
    (let ((bytes-read (read-sequence header stream)))
      (when (= bytes-read 9)
        (let* ((length (logior (ash (aref header 0) 16)
                              (ash (aref header 1) 8)
                              (aref header 2)))
               (type (aref header 3))
               (flags (aref header 4))
               (stream-id (logior (ash (logand #x7f (aref header 5)) 24)
                                 (ash (aref header 6) 16)
                                 (ash (aref header 7) 8)
                                 (aref header 8)))
               (payload (when (> length 0)
                         (let ((p (make-array length :element-type '(unsigned-byte 8))))
                           (read-sequence p stream)
                           p))))
          (list :length length
                :type type
                :flags flags
                :stream-id stream-id
                :payload payload))))))

(defun handle-frame-simple (output-stream frame)
  "Handle incoming frame"
  (let ((type (getf frame :type))
        (flags (getf frame :flags))
        (stream-id (getf frame :stream-id)))
    
    (format t "Received frame: type=~D, flags=~D, stream=~D, length=~D~%"
            type flags stream-id (getf frame :length))
    
    (case type
      ;; SETTINGS (type 4)
      (4 (if (logtest flags 1)  ; ACK flag
             (format t "  SETTINGS ACK received~%")
             (progn
               (format t "  SETTINGS received, sending ACK~%")
               (send-settings-ack output-stream))))
      
      ;; PING (type 6)
      (6 (unless (logtest flags 1)  ; Not ACK
           (format t "  PING received, sending ACK~%")
           (send-ping-ack output-stream (getf frame :payload))))
      
      ;; WINDOW_UPDATE (type 8)
      (8 (format t "  WINDOW_UPDATE received~%"))
      
      ;; HEADERS (type 1)
      (1 (progn
           (format t "  HEADERS received~%")
           ;; Always send response when END_HEADERS is set
           (when (logtest flags 4) ; END_HEADERS flag
             (send-simple-response output-stream stream-id))))
      
      ;; DATA (type 0)
      (0 (format t "  DATA received~%"))
      
      ;; GOAWAY (type 7)
      (7 (progn
           (format t "  GOAWAY received, closing~%")
           (setf *server-running* nil)))
      
      ;; PRIORITY (type 2)
      (2 (format t "  PRIORITY received~%"))
      
      ;; RST_STREAM (type 3)
      (3 (format t "  RST_STREAM received~%"))
      
      ;; PUSH_PROMISE (type 5)
      (5 (format t "  PUSH_PROMISE received (not supported)~%"))
      
      ;; CONTINUATION (type 9)
      (9 (format t "  CONTINUATION received~%"))
      
      ;; Unknown
      (t (format t "  Unknown frame type: ~D~%" type)))))

(defun send-settings-ack (stream)
  "Send SETTINGS ACK frame"
  (let ((frame (make-array 9 :element-type '(unsigned-byte 8))))
    ;; Header for empty SETTINGS with ACK flag
    (setf (aref frame 0) 0)   ; Length = 0
    (setf (aref frame 1) 0)
    (setf (aref frame 2) 0)
    (setf (aref frame 3) 4)   ; Type = SETTINGS
    (setf (aref frame 4) 1)   ; Flags = ACK
    (setf (aref frame 5) 0)   ; Stream ID = 0
    (setf (aref frame 6) 0)
    (setf (aref frame 7) 0)
    (setf (aref frame 8) 0)
    
    (write-sequence frame stream)
    (force-output stream)))

(defun send-ping-ack (stream payload)
  "Send PING ACK frame"
  (let ((frame (make-array 17 :element-type '(unsigned-byte 8))))
    ;; Header
    (setf (aref frame 0) 0)   ; Length = 8
    (setf (aref frame 1) 0)
    (setf (aref frame 2) 8)
    (setf (aref frame 3) 6)   ; Type = PING
    (setf (aref frame 4) 1)   ; Flags = ACK
    (setf (aref frame 5) 0)   ; Stream ID = 0
    (setf (aref frame 6) 0)
    (setf (aref frame 7) 0)
    (setf (aref frame 8) 0)
    
    ;; Copy payload (8 bytes)
    (if payload
        (replace frame payload :start1 9 :end1 17 :end2 8)
        (loop for i from 9 to 16 do (setf (aref frame i) 0)))
    
    (write-sequence frame stream)
    (force-output stream)))

(defun send-simple-response (stream stream-id)
  "Send a simple HTTP/2 response"
  ;; Send HEADERS frame with :status 200
  ;; Using literal header field without indexing (simplified HPACK)
  (let ((headers-payload (make-array 13 :element-type '(unsigned-byte 8))))
    ;; Literal header field without indexing - new name
    (setf (aref headers-payload 0) #x00)  ; Literal without indexing
    (setf (aref headers-payload 1) #x07)  ; Name length = 7
    ;; ":status" in ASCII
    (setf (aref headers-payload 2) (char-code #\:))
    (setf (aref headers-payload 3) (char-code #\s))
    (setf (aref headers-payload 4) (char-code #\t))
    (setf (aref headers-payload 5) (char-code #\a))
    (setf (aref headers-payload 6) (char-code #\t))
    (setf (aref headers-payload 7) (char-code #\u))
    (setf (aref headers-payload 8) (char-code #\s))
    (setf (aref headers-payload 9) #x03)  ; Value length = 3
    ;; "200" in ASCII
    (setf (aref headers-payload 10) (char-code #\2))
    (setf (aref headers-payload 11) (char-code #\0))
    (setf (aref headers-payload 12) (char-code #\0))
    
    ;; Send HEADERS frame
    (let ((frame (make-array (+ 9 13) :element-type '(unsigned-byte 8))))
      ;; Header
      (setf (aref frame 0) 0)   ; Length high
      (setf (aref frame 1) 0)   ; Length mid
      (setf (aref frame 2) 13)  ; Length low
      (setf (aref frame 3) 1)   ; Type = HEADERS
      (setf (aref frame 4) 4)   ; Flags = END_HEADERS only
      ;; Stream ID
      (setf (aref frame 5) (logand #x7f (ash stream-id -24)))
      (setf (aref frame 6) (logand #xff (ash stream-id -16)))
      (setf (aref frame 7) (logand #xff (ash stream-id -8)))
      (setf (aref frame 8) (logand #xff stream-id))
      
      ;; Copy headers payload
      (replace frame headers-payload :start1 9)
      
      (write-sequence frame stream)
      (force-output stream)))
  
  ;; Send DATA frame with response body
  (let ((data-payload (epsilon.string:string-to-octets "OK"))
        (frame (make-array (+ 9 2) :element-type '(unsigned-byte 8))))
    ;; Header
    (setf (aref frame 0) 0)   ; Length high
    (setf (aref frame 1) 0)   ; Length mid
    (setf (aref frame 2) 2)   ; Length low (2 bytes for "OK")
    (setf (aref frame 3) 0)   ; Type = DATA
    (setf (aref frame 4) 1)   ; Flags = END_STREAM
    ;; Stream ID
    (setf (aref frame 5) (logand #x7f (ash stream-id -24)))
    (setf (aref frame 6) (logand #xff (ash stream-id -16)))
    (setf (aref frame 7) (logand #xff (ash stream-id -8)))
    (setf (aref frame 8) (logand #xff stream-id))
    ;; Payload
    (setf (aref frame 9) (aref data-payload 0))
    (setf (aref frame 10) (aref data-payload 1))
    
    (write-sequence frame stream)
    (force-output stream)))

(defun start-test-server (&key (port 8080))
  "Start the HTTP/2 test server"
  (setf *server-port* port)
  (setf *server-running* t)
  
  (format t "~%Starting HTTP/2 test server on port ~D~%" port)
  (format t "Test with: docker run --rm --network host summerwind/h2spec -p ~D~%~%" port)
  
  (let* ((address (epsilon.net:make-socket-address "0.0.0.0" port))
         (server-socket (epsilon.net:tcp-bind address :backlog 10)))
    
    (setf *server-thread*
          (sb-thread:make-thread
           (lambda ()
             (unwind-protect
                  (loop while *server-running*
                        do (let ((client (epsilon.net:tcp-accept server-socket)))
                             (sb-thread:make-thread
                              (lambda () (handle-http2-connection client))
                              :name "http2-client")))
               (handler-case
                   (epsilon.net:tcp-shutdown server-socket :how :both)
                 (error (e)
                   (format t "Error closing server socket: ~A~%" e)))))
           :name "http2-server"))
    
    (format t "Server started. Press Ctrl+C to stop.~%")
    
    ;; Wait for interrupt
    (handler-case
        (loop while *server-running*
              do (sleep 1))
      (sb-sys:interactive-interrupt ()
        (format t "~%Shutting down server...~%")
        (setf *server-running* nil)
        (sleep 1)))))

;; Start the server
(start-test-server :port 8080)