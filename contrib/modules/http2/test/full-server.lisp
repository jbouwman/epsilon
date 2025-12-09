;;;; Full HTTP/2 Server Implementation
;;;;
;;;; A complete HTTP/2 server for h2spec compliance testing

(format t "Loading HTTP/2 server...~%")

;; Load required modules
(unless (find-package :epsilon.net)
  (let ((platform (string-downcase (symbol-name (epsilon.sys.env:platform)))))
    (error "Please run with: ./epsilon --module epsilon.~A --module epsilon.crypto --module epsilon.http2 --load \"http2-server.lisp\"" platform)))

(unless (find-package :epsilon.http2)
  (error "HTTP/2 module not loaded. Run with --module epsilon.http2"))

;;;; Server Implementation

(defparameter *server-port* 8080)
(defparameter *server-running* nil)
(defparameter *server-socket* nil)

(defstruct h2-connection
  socket
  input-stream
  output-stream
  decoder
  encoder
  streams
  last-stream-id
  local-settings
  remote-settings
  recv-window
  send-window)

(defun make-h2-connection (socket)
  (make-instance 'h2-connection
                 :socket socket
                 :input-stream (epsilon.net:tcp-stream-reader socket)
                 :output-stream (epsilon.net:tcp-stream-writer socket)
                 :decoder (epsilon.http2.hpack:create-decoder)
                 :encoder (epsilon.http2.hpack:create-encoder)
                 :streams (make-hash-table)
                 :last-stream-id 0
                 :recv-window 65535
                 :send-window 65535))

(defun handle-h2-connection (client-socket)
  "Handle an HTTP/2 connection with full protocol support"
  (format t "~%[~A] New connection~%"
          (get-universal-time))
  
  (let ((conn (make-h2-connection client-socket)))
    (handler-case
        (progn
          ;; Read and validate client preface
          (unless (read-client-preface conn)
            (error "Invalid client preface"))
          
          ;; Send server preface (SETTINGS frame)
          (send-settings conn)
          
          ;; Main frame processing loop
          (loop while *server-running*
                do (let ((frame (read-frame conn)))
                     (when frame
                       (handle-frame conn frame)))))
      
      (error (e)
        (format t "[~A] Connection error: ~A~%" (get-universal-time) e)))
    
    ;; Clean shutdown
    (handler-case
        (progn
          (force-output (h2-connection-output-stream conn))
          (epsilon.net:tcp-shutdown client-socket :how :both))
      (error (e)
        (declare (ignore e))))))

(defun read-client-preface (conn)
  "Read and validate HTTP/2 client preface"
  (let ((preface (make-array 24 :element-type '(unsigned-byte 8)))
        (expected #(80 82 73 32 42 32 72 84 84 80 47 50 46 48 13 10 13 10 83 77 13 10 13 10)))
    (let ((bytes-read (read-sequence preface (h2-connection-input-stream conn))))
      (when (and (= bytes-read 24)
                 (equalp preface expected))
        (format t "[~A] Client preface OK~%" (get-universal-time))
        t))))

(defun send-settings (conn)
  "Send initial SETTINGS frame"
  (let ((settings-frame (epsilon.http2:make-settings-frame
                         :initial-settings
                         '((#x3 . 100)    ; MAX_CONCURRENT_STREAMS
                           (#x4 . 65535)  ; INITIAL_WINDOW_SIZE
                           (#x5 . 16384)  ; MAX_FRAME_SIZE
                           (#x6 . 8192))))) ; MAX_HEADER_LIST_SIZE
    (write-frame conn settings-frame)
    (format t "[~A] Sent SETTINGS frame~%" (get-universal-time))))

(defun read-frame (conn)
  "Read an HTTP/2 frame"
  (handler-case
      (let ((header (make-array 9 :element-type '(unsigned-byte 8))))
        (let ((bytes-read (read-sequence header (h2-connection-input-stream conn))))
          (when (= bytes-read 9)
            (let* ((length (logior (ash (aref header 0) 16)
                                  (ash (aref header 1) 8)
                                  (aref header 2)))
                   (type (aref header 3))
                   (flags (aref header 4))
                   (stream-id (logand #x7fffffff
                                     (logior (ash (aref header 5) 24)
                                            (ash (aref header 6) 16)
                                            (ash (aref header 7) 8)
                                            (aref header 8))))
                   (payload (when (> length 0)
                             (let ((p (make-array length :element-type '(unsigned-byte 8))))
                               (read-sequence p (h2-connection-input-stream conn))
                               p))))
              (make-instance 'epsilon.http2:http2-frame
                            :length length
                            :type type
                            :flags flags
                            :stream-id stream-id
                            :payload payload)))))
    (end-of-file ()
      nil)))

(defun write-frame (conn frame)
  "Write an HTTP/2 frame"
  (epsilon.http2:write-frame (h2-connection-output-stream conn) frame))

(defun handle-frame (conn frame)
  "Handle incoming HTTP/2 frame"
  (let ((type (epsilon.http2:http2-frame-type frame))
        (flags (epsilon.http2:http2-frame-flags frame))
        (stream-id (epsilon.http2:http2-frame-stream-id frame)))
    
    (format t "[~A] Frame: type=~D flags=~D stream=~D len=~D~%"
            (get-universal-time) type flags stream-id
            (epsilon.http2:http2-frame-length frame))
    
    (case type
      ;; DATA (0x0)
      (0 (handle-data-frame conn frame))
      
      ;; HEADERS (0x1)
      (1 (handle-headers-frame conn frame))
      
      ;; PRIORITY (0x2)
      (2 (handle-priority-frame conn frame))
      
      ;; RST_STREAM (0x3)
      (3 (handle-rst-stream-frame conn frame))
      
      ;; SETTINGS (0x4)
      (4 (handle-settings-frame conn frame))
      
      ;; PUSH_PROMISE (0x5)
      (5 (handle-push-promise-frame conn frame))
      
      ;; PING (0x6)
      (6 (handle-ping-frame conn frame))
      
      ;; GOAWAY (0x7)
      (7 (handle-goaway-frame conn frame))
      
      ;; WINDOW_UPDATE (0x8)
      (8 (handle-window-update-frame conn frame))
      
      ;; CONTINUATION (0x9)
      (9 (handle-continuation-frame conn frame))
      
      ;; Unknown
      (t (format t "  [WARN] Unknown frame type: ~D~%" type)))))

(defun handle-data-frame (conn frame)
  "Handle DATA frame"
  (let ((stream-id (epsilon.http2:http2-frame-stream-id frame))
        (end-stream (logtest (epsilon.http2:http2-frame-flags frame) 
                            epsilon.http2:+flag-end-stream+)))
    (format t "  DATA: stream=~D end-stream=~A~%" stream-id end-stream)
    
    ;; Update flow control windows
    (when (> (epsilon.http2:http2-frame-length frame) 0)
      (decf (h2-connection-recv-window conn) 
            (epsilon.http2:http2-frame-length frame))
      
      ;; Send WINDOW_UPDATE if needed
      (when (< (h2-connection-recv-window conn) 32768)
        (let ((increment 65535))
          (incf (h2-connection-recv-window conn) increment)
          (write-frame conn (epsilon.http2:make-window-update-frame 0 increment))
          (format t "  Sent WINDOW_UPDATE: increment=~D~%" increment))))))

(defun handle-headers-frame (conn frame)
  "Handle HEADERS frame"
  (let* ((stream-id (epsilon.http2:http2-frame-stream-id frame))
         (end-stream (logtest (epsilon.http2:http2-frame-flags frame)
                             epsilon.http2:+flag-end-stream+))
         (end-headers (logtest (epsilon.http2:http2-frame-flags frame)
                              epsilon.http2:+flag-end-headers+))
         (headers (when end-headers
                   (epsilon.http2.hpack:decode-headers
                    (h2-connection-decoder conn)
                    (epsilon.http2:http2-frame-payload frame)))))
    
    (format t "  HEADERS: stream=~D end-stream=~A end-headers=~A~%" 
            stream-id end-stream end-headers)
    
    (when headers
      (format t "  Headers: ~S~%" headers))
    
    ;; Send response
    (when end-stream
      (send-response conn stream-id))))

(defun handle-priority-frame (conn frame)
  "Handle PRIORITY frame"
  (declare (ignore conn))
  (let ((stream-id (epsilon.http2:http2-frame-stream-id frame)))
    (format t "  PRIORITY: stream=~D~%" stream-id)))

(defun handle-rst-stream-frame (conn frame)
  "Handle RST_STREAM frame"
  (declare (ignore conn))
  (let ((stream-id (epsilon.http2:http2-frame-stream-id frame)))
    (format t "  RST_STREAM: stream=~D~%" stream-id)))

(defun handle-settings-frame (conn frame)
  "Handle SETTINGS frame"
  (let ((ack (logtest (epsilon.http2:http2-frame-flags frame)
                     epsilon.http2:+flag-ack+)))
    (if ack
        (format t "  SETTINGS ACK~%")
        (progn
          (format t "  SETTINGS~%")
          ;; Parse and apply settings
          (let ((payload (epsilon.http2:http2-frame-payload frame)))
            (when payload
              (loop for i from 0 below (length payload) by 6
                    do (let ((id (logior (ash (aref payload i) 8)
                                        (aref payload (1+ i))))
                             (value (logior (ash (aref payload (+ i 2)) 24)
                                          (ash (aref payload (+ i 3)) 16)
                                          (ash (aref payload (+ i 4)) 8)
                                          (aref payload (+ i 5)))))
                         (format t "    Setting ~D = ~D~%" id value)))))
          ;; Send ACK
          (write-frame conn (epsilon.http2:make-settings-frame :ack t))
          (format t "  Sent SETTINGS ACK~%")))))

(defun handle-push-promise-frame (conn frame)
  "Handle PUSH_PROMISE frame"
  (declare (ignore conn frame))
  (format t "  PUSH_PROMISE (not supported)~%"))

(defun handle-ping-frame (conn frame)
  "Handle PING frame"
  (let ((ack (logtest (epsilon.http2:http2-frame-flags frame)
                     epsilon.http2:+flag-ack+)))
    (if ack
        (format t "  PING ACK~%")
        (progn
          (format t "  PING~%")
          ;; Send PING ACK with same payload
          (write-frame conn (epsilon.http2:make-ping-frame 
                           :ack t 
                           :data (epsilon.http2:http2-frame-payload frame)))
          (format t "  Sent PING ACK~%")))))

(defun handle-goaway-frame (conn frame)
  "Handle GOAWAY frame"
  (declare (ignore conn frame))
  (format t "  GOAWAY - closing connection~%")
  (setf *server-running* nil))

(defun handle-window-update-frame (conn frame)
  "Handle WINDOW_UPDATE frame"
  (let ((stream-id (epsilon.http2:http2-frame-stream-id frame))
        (payload (epsilon.http2:http2-frame-payload frame)))
    (when (and payload (>= (length payload) 4))
      (let ((increment (logior (ash (logand #x7f (aref payload 0)) 24)
                              (ash (aref payload 1) 16)
                              (ash (aref payload 2) 8)
                              (aref payload 3))))
        (format t "  WINDOW_UPDATE: stream=~D increment=~D~%" stream-id increment)
        
        ;; Update send window
        (when (zerop stream-id)
          (incf (h2-connection-send-window conn) increment))))))

(defun handle-continuation-frame (conn frame)
  "Handle CONTINUATION frame"
  (declare (ignore conn))
  (let ((stream-id (epsilon.http2:http2-frame-stream-id frame)))
    (format t "  CONTINUATION: stream=~D~%" stream-id)))

(defun send-response (conn stream-id)
  "Send HTTP/2 response"
  (format t "  Sending response on stream ~D~%" stream-id)
  
  ;; Send HEADERS frame with status 200
  (let* ((headers '((":status" . "200")
                   ("content-type" . "text/plain")
                   ("server" . "epsilon-http2")))
         (encoded (epsilon.http2.hpack:encode-headers
                  (h2-connection-encoder conn)
                  headers))
         (headers-frame (make-instance 'epsilon.http2:http2-frame
                                       :type 1  ; HEADERS
                                       :flags 4 ; END_HEADERS
                                       :stream-id stream-id
                                       :length (length encoded)
                                       :payload encoded)))
    (write-frame conn headers-frame))
  
  ;; Send DATA frame with response body
  (let* ((body (epsilon.string:string-to-octets "Hello from Epsilon HTTP/2 server!\n"))
         (data-frame (make-instance 'epsilon.http2:http2-frame
                                    :type 0  ; DATA
                                    :flags 1 ; END_STREAM
                                    :stream-id stream-id
                                    :length (length body)
                                    :payload body)))
    (write-frame conn data-frame))
  
  (format t "  Response sent~%"))

(defun start-h2-server (&key (port 8080))
  "Start HTTP/2 server"
  (setf *server-port* port)
  (setf *server-running* t)
  
  (format t "~%Starting HTTP/2 server on port ~D~%" port)
  (format t "Test with: docker run --rm --network host summerwind/h2spec -p ~D~%~%" port)
  
  (let* ((address (epsilon.net:make-socket-address "0.0.0.0" port))
         (server-socket (epsilon.net:tcp-bind address :backlog 10)))
    
    (setf *server-socket* server-socket)
    
    (unwind-protect
         (loop while *server-running*
               do (handler-case
                      (let ((client (epsilon.net:tcp-accept server-socket)))
                        (sb-thread:make-thread
                         (lambda () 
                           (handler-case
                               (handle-h2-connection client)
                             (error (e)
                               (format t "Client handler error: ~A~%" e))))
                         :name (format nil "h2-client-~D" (get-universal-time))))
                    (error (e)
                      (unless *server-running*
                        (return))
                      (format t "Accept error: ~A~%" e))))
      
      ;; Cleanup
      (handler-case
          (epsilon.net:tcp-shutdown server-socket :how :both)
        (error (e)
          (declare (ignore e)))))))

;; Start server
(handler-case
    (start-h2-server :port 8080)
  (sb-sys:interactive-interrupt ()
    (format t "~%Shutting down server...~%")
    (setf *server-running* nil)))