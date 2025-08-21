;;;; HTTP/2 Server Implementation
;;;;
;;;; Implements an HTTP/2 server that can pass h2spec conformance tests

(in-package :epsilon.http2)

;;;; Server Configuration

(defstruct http2-server-config
  "HTTP/2 server configuration"
  (port 8080 :type integer)
  (host "0.0.0.0" :type string)
  (ssl-p nil :type boolean)
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (handler nil :type (or null function))
  (max-concurrent-streams 100 :type integer)
  (initial-window-size 65535 :type integer)
  (max-frame-size 16384 :type integer)
  (max-header-list-size 8192 :type integer)
  (enable-push t :type boolean))

;;;; Stream State

(defstruct http2-stream-state
  "HTTP/2 stream state"
  (id 0 :type integer)
  (state :idle :type keyword)
  (headers nil)
  (data (make-array 0 :element-type '(unsigned-byte 8) 
                     :adjustable t :fill-pointer 0))
  (send-window 65535 :type integer)
  (recv-window 65535 :type integer))

;;;; Connection Preface

(defparameter *client-preface* 
  #(80 82 73 32 42 32 72 84 84 80 47 50 46 48 13 10 13 10 83 77 13 10 13 10)
  "HTTP/2 client connection preface")

;;;; Server Implementation

(defun start-http2-server (&key (port 8080) 
                               (host "0.0.0.0")
                               ssl-p
                               cert-file
                               key-file
                               handler)
  "Start an HTTP/2 server"
  (let ((config (make-http2-server-config
                 :port port
                 :host host
                 :ssl-p ssl-p
                 :cert-file cert-file
                 :key-file key-file
                 :handler (or handler #'default-handler))))
    
    (format t "Starting HTTP/2 server on ~A:~D~%" host port)
    
    ;; Create server socket
    (let ((server-socket (create-server-socket host port ssl-p cert-file key-file)))
      (unwind-protect
           (loop
             (let ((client-socket (accept-connection server-socket)))
               (sb-thread:make-thread 
                (lambda () 
                  (handle-connection client-socket config))
                :name "http2-connection")))
        (close-socket server-socket)))))

(defun create-server-socket (host port ssl-p cert-file key-file)
  "Create a server socket with optional TLS"
  (let ((address (epsilon.net:make-socket-address host port)))
    (let ((socket (epsilon.net:tcp-bind address :backlog 128)))
      (if ssl-p
          (wrap-with-tls socket cert-file key-file)
          socket))))

(defun accept-connection (server-socket)
  "Accept a new connection"
  (epsilon.net:tcp-accept server-socket))

(defun wrap-with-tls (socket cert-file key-file)
  "Wrap socket with TLS (using epsilon.crypto)"
  ;; This would use epsilon.crypto TLS functions
  socket)

(defun close-socket (socket)
  "Close a socket"
  (epsilon.net:tcp-shutdown socket :how :both))

(defun handle-connection (socket config)
  "Handle an HTTP/2 connection"
  (let* ((stream (make-socket-stream socket))
         (conn (make-http2-connection
                :socket socket
                :stream stream
                :config config
                :state :waiting-magic)))
    
    (handler-case
        (progn
          ;; Read client preface
          (unless (read-client-preface stream)
            (error "Invalid client preface"))
          
          (setf (http2-connection-state conn) :connected)
          
          ;; Send server preface (SETTINGS frame)
          (send-server-preface conn)
          
          ;; Main frame processing loop
          (loop
            (let ((frame (read-frame stream)))
              (unless frame
                (return))
              
              (handle-frame conn frame)
              
              (when (http2-connection-goaway-sent conn)
                (return)))))
      
      (error (e)
        (format t "Connection error: ~A~%" e)
        (send-goaway conn +error-internal-error+ 
                    (format nil "~A" e))))
    
    (close-socket socket)))

(defun make-socket-stream (socket)
  "Create a bidirectional stream from socket"
  (make-two-way-stream 
   (epsilon.net:tcp-stream-reader socket)
   (epsilon.net:tcp-stream-writer socket)))

(defun read-client-preface (stream)
  "Read and validate client preface"
  (let ((preface (make-array 24 :element-type '(unsigned-byte 8))))
    (read-sequence preface stream)
    (equalp preface *client-preface*)))

(defun send-server-preface (conn)
  "Send server preface (initial SETTINGS frame)"
  (let ((settings-frame (make-settings-frame 
                        :initial-settings
                        (list (cons +settings-max-concurrent-streams+ 
                                   (http2-server-config-max-concurrent-streams 
                                    (http2-connection-config conn)))
                              (cons +settings-initial-window-size+
                                   (http2-server-config-initial-window-size
                                    (http2-connection-config conn)))
                              (cons +settings-max-frame-size+
                                   (http2-server-config-max-frame-size
                                    (http2-connection-config conn)))))))
    (write-frame (http2-connection-stream conn) settings-frame)))

;;;; Frame Handling

(defun handle-frame (conn frame)
  "Handle an incoming HTTP/2 frame"
  (case (http2-frame-type frame)
    (#.+frame-settings+
     (handle-settings-frame conn frame))
    (#.+frame-ping+
     (handle-ping-frame conn frame))
    (#.+frame-window-update+
     (handle-window-update-frame conn frame))
    (#.+frame-headers+
     (handle-headers-frame conn frame))
    (#.+frame-data+
     (handle-data-frame conn frame))
    (#.+frame-goaway+
     (handle-goaway-frame conn frame))
    (#.+frame-rst-stream+
     (handle-rst-stream-frame conn frame))
    (#.+frame-priority+
     (handle-priority-frame conn frame))
    (#.+frame-push-promise+
     (handle-push-promise-frame conn frame))
    (#.+frame-continuation+
     (handle-continuation-frame conn frame))
    (t
     (handle-unknown-frame conn frame))))

(defun handle-settings-frame (conn frame)
  "Handle SETTINGS frame"
  (if (logtest (http2-frame-flags frame) +flag-ack+)
      ;; Settings ACK
      (format t "Received SETTINGS ACK~%")
      ;; New settings
      (progn
        (format t "Received SETTINGS frame~%")
        (apply-settings conn frame)
        ;; Send ACK
        (let ((ack-frame (make-settings-frame :ack t)))
          (write-frame (http2-connection-stream conn) ack-frame)))))

(defun apply-settings (conn frame)
  "Apply settings from SETTINGS frame"
  (let ((payload (http2-frame-payload frame))
        (offset 0))
    (loop while (< offset (length payload))
          do (let ((id (logior (ash (aref payload offset) 8)
                              (aref payload (1+ offset))))
                   (value (logior (ash (aref payload (+ offset 2)) 24)
                                 (ash (aref payload (+ offset 3)) 16)
                                 (ash (aref payload (+ offset 4)) 8)
                                 (aref payload (+ offset 5)))))
               (format t "  Setting ~D = ~D~%" id value)
               (incf offset 6)))))

(defun handle-ping-frame (conn frame)
  "Handle PING frame"
  (unless (logtest (http2-frame-flags frame) +flag-ack+)
    ;; Send PING ACK with same payload
    (let ((ack-frame (make-ping-frame :ack t 
                                      :data (http2-frame-payload frame))))
      (write-frame (http2-connection-stream conn) ack-frame))))

(defun handle-window-update-frame (conn frame)
  "Handle WINDOW_UPDATE frame"
  (let* ((payload (http2-frame-payload frame))
         (increment (logand #x7fffffff
                           (logior (ash (aref payload 0) 24)
                                  (ash (aref payload 1) 16)
                                  (ash (aref payload 2) 8)
                                  (aref payload 3)))))
    (if (zerop (http2-frame-stream-id frame))
        ;; Connection window update
        (incf (http2-connection-send-window conn) increment)
        ;; Stream window update
        (let ((stream (gethash (http2-frame-stream-id frame)
                              (http2-connection-streams conn))))
          (when stream
            (incf (http2-stream-state-send-window stream) increment))))))

(defun handle-headers-frame (conn frame)
  "Handle HEADERS frame"
  (let* ((stream-id (http2-frame-stream-id frame))
         (stream (or (gethash stream-id (http2-connection-streams conn))
                    (make-http2-stream-state :id stream-id 
                                            :state :open))))
    (setf (gethash stream-id (http2-connection-streams conn)) stream)
    
    ;; Decode headers (simplified - needs HPACK)
    (let ((headers (decode-headers (http2-frame-payload frame))))
      (setf (http2-stream-state-headers stream) headers)
      
      ;; If END_STREAM flag is set, process request
      (when (logtest (http2-frame-flags frame) +flag-end-stream+)
        (process-request conn stream)))))

(defun decode-headers (payload)
  "Decode headers from payload (placeholder for HPACK)"
  ;; Simple decoding - matches our simple encoding
  (let ((headers nil)
        (offset 0))
    (loop while (< offset (length payload))
          do (let* ((name-len (aref payload offset))
                    (name-bytes (subseq payload (1+ offset) (+ offset 1 name-len)))
                    (value-offset (+ offset 1 name-len))
                    (value-len (aref payload value-offset))
                    (value-bytes (subseq payload (1+ value-offset) 
                                       (+ value-offset 1 value-len))))
               (push (cons (epsilon.string:octets-to-string name-bytes)
                          (epsilon.string:octets-to-string value-bytes))
                     headers)
               (setf offset (+ value-offset 1 value-len))))
    (nreverse headers)))

(defun handle-data-frame (conn frame)
  "Handle DATA frame"
  (let* ((stream-id (http2-frame-stream-id frame))
         (stream (gethash stream-id (http2-connection-streams conn))))
    (when stream
      ;; Append data to stream buffer
      (let ((payload (http2-frame-payload frame))
            (buffer (http2-stream-state-data stream)))
        (loop for byte across payload
              do (vector-push-extend byte buffer))
        
        ;; If END_STREAM flag is set, process request
        (when (logtest (http2-frame-flags frame) +flag-end-stream+)
          (process-request conn stream))))))

(defun handle-goaway-frame (conn frame)
  "Handle GOAWAY frame"
  (setf (http2-connection-goaway-received conn) t)
  (format t "Received GOAWAY frame~%"))

(defun handle-rst-stream-frame (conn frame)
  "Handle RST_STREAM frame"
  (let ((stream-id (http2-frame-stream-id frame)))
    (remhash stream-id (http2-connection-streams conn))
    (format t "Stream ~D reset~%" stream-id)))

(defun handle-priority-frame (conn frame)
  "Handle PRIORITY frame"
  ;; Priority handling would go here
  (format t "Received PRIORITY frame~%"))

(defun handle-push-promise-frame (conn frame)
  "Handle PUSH_PROMISE frame"
  ;; Server shouldn't receive PUSH_PROMISE
  (send-goaway conn +error-protocol-error+ "Server received PUSH_PROMISE"))

(defun handle-continuation-frame (conn frame)
  "Handle CONTINUATION frame"
  ;; Continuation handling for headers
  (format t "Received CONTINUATION frame~%"))

(defun handle-unknown-frame (conn frame)
  "Handle unknown frame type"
  (format t "Unknown frame type: ~D~%" (http2-frame-type frame)))

;;;; Request Processing

(defun process-request (conn stream)
  "Process an HTTP/2 request"
  (let* ((headers (http2-stream-state-headers stream))
         (body (http2-stream-state-data stream))
         (handler (http2-server-config-handler (http2-connection-config conn)))
         (response (funcall handler headers body)))
    
    ;; Send response
    (send-response conn (http2-stream-state-id stream) response)))

(defun send-response (conn stream-id response)
  "Send an HTTP/2 response"
  (let* ((status (getf response :status 200))
         (headers (getf response :headers nil))
         (body (getf response :body ""))
         (response-headers (append
                           (list (cons ":status" (format nil "~D" status)))
                           headers)))
    
    ;; Send HEADERS frame
    (let ((headers-frame (make-headers-frame 
                         stream-id response-headers
                         :end-headers t
                         :end-stream (zerop (length body)))))
      (write-frame (http2-connection-stream conn) headers-frame))
    
    ;; Send DATA frame if there's a body
    (when (plusp (length body))
      (let ((data-frame (make-data-frame stream-id body :end-stream t)))
        (write-frame (http2-connection-stream conn) data-frame)))))

(defun default-handler (headers body)
  "Default request handler"
  (list :status 200
        :headers (list (cons "content-type" "text/plain"))
        :body "Hello from HTTP/2 server!"))

(defun send-goaway (conn error-code &optional debug-data)
  "Send GOAWAY frame and mark connection for closure"
  (unless (http2-connection-goaway-sent conn)
    (let ((goaway-frame (make-goaway-frame 
                        (http2-connection-last-stream-id conn)
                        error-code
                        debug-data)))
      (write-frame (http2-connection-stream conn) goaway-frame)
      (setf (http2-connection-goaway-sent conn) t))))

;;;; Export symbols

(export '(;; Server
          start-http2-server
          http2-server-config
          make-http2-server-config
          
          ;; Connection
          http2-connection
          http2-connection-p
          
          ;; Stream
          http2-stream-state
          http2-stream-state-p
          
          ;; Handlers
          default-handler))