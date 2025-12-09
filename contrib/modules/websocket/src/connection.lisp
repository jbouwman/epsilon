;;;; WebSocket Connection Management
;;;;
;;;; This module manages WebSocket connections including message handling,
;;;; connection state, and the close handshake.

(defpackage epsilon.websocket.connection
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (frame epsilon.websocket.frame)
   (str epsilon.string)
   (thread epsilon.sys.thread)
   (lock epsilon.sys.lock))
  (:export
   ;; Connection structure
   websocket-connection
   make-connection
   websocket-connection-stream
   websocket-connection-state
   websocket-connection-subprotocol
   websocket-connection-extensions
   websocket-connection-client-side
   websocket-connection-close-code
   websocket-connection-close-reason
   websocket-connection-ping-payload
   websocket-connection-message-buffer
   websocket-connection-message-type
   websocket-connection-lock
   websocket-connection-on-open
   websocket-connection-on-message
   websocket-connection-on-close
   websocket-connection-on-error
   websocket-connection-on-ping
   websocket-connection-on-pong
   
   ;; Connection states
   +state-connecting+
   +state-open+
   +state-closing+
   +state-closed+
   
   ;; Connection operations
   send-text
   send-binary
   send-ping
   send-pong
   close-connection
   
   ;; Message handling
   handle-frame
   read-message
   
   ;; Event callbacks
   on-open
   on-message
   on-close
   on-error
   on-ping
   on-pong
   
   ;; Connection management
   connection-open-p
   connection-closed-p
   wait-for-close
   
   ;; Errors
   timeout-error
   timeout-error-message))

(in-package epsilon.websocket.connection)

;;; Error conditions

(define-condition timeout-error (error)
  ((message :initarg :message :reader timeout-error-message))
  (:report (lambda (condition stream)
             (format stream "WebSocket timeout: ~A" 
                     (timeout-error-message condition)))))

;;; Connection states

(defconstant +state-connecting+ :connecting "Connection in progress")
(defconstant +state-open+ :open "Connection established")
(defconstant +state-closing+ :closing "Close handshake initiated")
(defconstant +state-closed+ :closed "Connection closed")

;;; Connection structure

(defstruct websocket-connection
  "WebSocket connection state"
  (stream nil :type (or null stream))                    ; Underlying stream
  (state +state-connecting+ :type keyword)               ; Connection state
  (client-side nil :type boolean)                        ; True if client connection
  (subprotocol nil :type (or null string))              ; Negotiated subprotocol
  (extensions nil :type list)                            ; Negotiated extensions
  (close-code nil :type (or null integer))              ; Close status code
  (close-reason nil :type (or null string))             ; Close reason
  (ping-payload nil :type (or null vector))             ; Last ping payload
  (message-buffer nil :type list)                       ; Partial message fragments
  (message-type nil :type (or null integer))            ; Type of partial message
  (lock (lock:make-lock) :type lock:lock)               ; Thread safety lock
  
  ;; Event callbacks
  (on-open nil :type (or null function))                ; Connection opened
  (on-message nil :type (or null function))             ; Message received
  (on-close nil :type (or null function))               ; Connection closed
  (on-error nil :type (or null function))               ; Error occurred
  (on-ping nil :type (or null function))                ; Ping received
  (on-pong nil :type (or null function)))               ; Pong received

(defun make-connection (stream &key client-side subprotocol extensions
                               on-open on-message on-close on-error on-ping on-pong)
  "Create a new WebSocket connection"
  (make-websocket-connection
   :stream stream
   :state +state-open+
   :client-side client-side
   :subprotocol subprotocol
   :extensions extensions
   :on-open on-open
   :on-message on-message
   :on-close on-close
   :on-error on-error
   :on-ping on-ping
   :on-pong on-pong))

;;; State predicates

(defun connection-open-p (connection)
  "Check if connection is open"
  (eq (websocket-connection-state connection) +state-open+))

(defun connection-closed-p (connection)
  "Check if connection is closed"
  (eq (websocket-connection-state connection) +state-closed+))

;;; Frame sending

(defun send-frame (connection frame)
  "Send a frame over the connection"
  (lock:with-lock ((websocket-connection-lock connection))
    (unless (connection-open-p connection)
      (error "Cannot send frame: connection not open"))
    
    (let ((stream (websocket-connection-stream connection))
          (client-side (websocket-connection-client-side connection)))
      ;; Serialize and send frame
      (let ((frame-data (frame:serialize-frame frame :client-side client-side)))
        (write-sequence frame-data stream)
        (force-output stream)))))

(defun send-text (connection text)
  "Send a text message"
  (send-frame connection (frame:make-text-frame text)))

(defun send-binary (connection data)
  "Send binary data"
  (send-frame connection (frame:make-binary-frame data)))

(defun send-ping (connection &optional payload)
  "Send a ping frame"
  (let ((ping-frame (frame:make-ping-frame payload)))
    ;; Store ping payload for pong validation
    (setf (websocket-connection-ping-payload connection)
          (frame:websocket-frame-payload ping-frame))
    (send-frame connection ping-frame)))

(defun send-pong (connection &optional payload)
  "Send a pong frame"
  (send-frame connection (frame:make-pong-frame payload)))

;;; Connection close

(defun close-connection (connection &optional (code frame:+close-normal+) reason)
  "Initiate connection close handshake"
  (lock:with-lock ((websocket-connection-lock connection))
    (when (connection-open-p connection)
      ;; Send close frame
      (setf (websocket-connection-state connection) +state-closing+
            (websocket-connection-close-code connection) code
            (websocket-connection-close-reason connection) reason)
      
      (send-frame connection (frame:make-close-frame code reason)))))

(defun force-close (connection)
  "Force close connection without handshake"
  (lock:with-lock ((websocket-connection-lock connection))
    (setf (websocket-connection-state connection) +state-closed+)
    (when (websocket-connection-stream connection)
      (close (websocket-connection-stream connection)))))

;;; Frame handling

(defun handle-frame (connection frame)
  "Handle received WebSocket frame"
  (frame:validate-frame frame)
  
  (let ((opcode (frame:websocket-frame-opcode frame)))
    (cond
      ;; Text/Binary/Continuation frames
      ((member opcode (list frame:+opcode-text+ 
                           frame:+opcode-binary+ 
                           frame:+opcode-continuation+))
       (handle-data-frame connection frame))
      
      ;; Close frame
      ((= opcode frame:+opcode-close+)
       (handle-close-frame connection frame))
      
      ;; Ping frame
      ((= opcode frame:+opcode-ping+)
       (handle-ping-frame connection frame))
      
      ;; Pong frame
      ((= opcode frame:+opcode-pong+)
       (handle-pong-frame connection frame))
      
      (t
       (error "Unknown frame opcode: ~D" opcode)))))

(defun handle-data-frame (connection frame)
  "Handle text/binary/continuation frame"
  (let ((opcode (frame:websocket-frame-opcode frame))
        (fin (frame:websocket-frame-fin frame))
        (payload (frame:websocket-frame-payload frame)))
    
    (cond
      ;; Start of new message
      ((member opcode (list frame:+opcode-text+ frame:+opcode-binary+))
       (setf (websocket-connection-message-type connection) opcode
             (websocket-connection-message-buffer connection) (list payload)))
      
      ;; Continuation frame
      ((= opcode frame:+opcode-continuation+)
       (unless (websocket-connection-message-type connection)
         (error "Unexpected continuation frame"))
       (push payload (websocket-connection-message-buffer connection)))
      
      (t
       (error "Invalid data frame opcode: ~D" opcode)))
    
    ;; If final frame, assemble complete message
    (when fin
      (let ((complete-payload (apply #'concatenate 'vector
                                    (reverse (websocket-connection-message-buffer connection))))
            (message-type (websocket-connection-message-type connection)))
        
        ;; Clear message state
        (setf (websocket-connection-message-type connection) nil
              (websocket-connection-message-buffer connection) nil)
        
        ;; Call message callback
        (when (websocket-connection-on-message connection)
          (funcall (websocket-connection-on-message connection)
                   connection
                   (if (= message-type frame:+opcode-text+)
                       (str:octets-to-string complete-payload)
                       complete-payload)
                   (= message-type frame:+opcode-text+)))))))

(defun handle-close-frame (connection frame)
  "Handle close frame"
  (multiple-value-bind (code reason)
      (frame:parse-close-frame frame)
    
    (lock:with-lock ((websocket-connection-lock connection))
      (setf (websocket-connection-close-code connection) code
            (websocket-connection-close-reason connection) reason)
      
      (cond
        ;; Received close while open - send close response
        ((connection-open-p connection)
         (setf (websocket-connection-state connection) +state-closing+)
         (send-frame connection (frame:make-close-frame code reason))
         (setf (websocket-connection-state connection) +state-closed+))
        
        ;; Received close response while closing
        ((eq (websocket-connection-state connection) +state-closing+)
         (setf (websocket-connection-state connection) +state-closed+))
        
        (t
         ;; Already closed - ignore
         )))
    
    ;; Call close callback
    (when (websocket-connection-on-close connection)
      (funcall (websocket-connection-on-close connection)
               connection code reason))))

(defun handle-ping-frame (connection frame)
  "Handle ping frame - send pong response"
  (let ((payload (frame:websocket-frame-payload frame)))
    ;; Send pong with same payload
    (send-pong connection payload)
    
    ;; Call ping callback
    (when (websocket-connection-on-ping connection)
      (funcall (websocket-connection-on-ping connection)
               connection payload))))

(defun handle-pong-frame (connection frame)
  "Handle pong frame"
  (let ((payload (frame:websocket-frame-payload frame))
        (expected-payload (websocket-connection-ping-payload connection)))
    
    ;; Validate pong payload matches last ping
    (when (and expected-payload
               (not (equalp payload expected-payload)))
      (error "Pong payload does not match ping"))
    
    ;; Clear ping payload
    (setf (websocket-connection-ping-payload connection) nil)
    
    ;; Call pong callback
    (when (websocket-connection-on-pong connection)
      (funcall (websocket-connection-on-pong connection)
               connection payload))))

;;; Message reading

(defun read-message (connection &key (timeout nil))
  "Read next complete message from connection with optional timeout"
  (unless (connection-open-p connection)
    (error "Cannot read message: connection not open"))
  
  (let ((deadline (when timeout
                    (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second)))))
    (loop
      ;; Check timeout
      (when (and deadline (> (get-internal-real-time) deadline))
        (error 'timeout-error :message "Read timeout exceeded"))
      
      ;; Try to read frame with non-blocking check
      (let ((stream (websocket-connection-stream connection)))
        ;; Check if data is available (implementation-specific)
        (when (listen stream)
          (let ((frame (frame:parse-frame stream)))
            (when frame
              (handle-frame connection frame)
              
              ;; Return if we got a complete data message
              (when (and (websocket-connection-message-buffer connection)
                         (frame:websocket-frame-fin frame))
                (return-from read-message t)))))
        
        ;; Brief sleep to avoid busy-waiting
        (when timeout
          (sleep 0.01))))))

;;; Event handling

(defun trigger-event (connection event &rest args)
  "Trigger connection event callback"
  (let ((callback (case event
                    (:open (websocket-connection-on-open connection))
                    (:message (websocket-connection-on-message connection))
                    (:close (websocket-connection-on-close connection))
                    (:error (websocket-connection-on-error connection))
                    (:ping (websocket-connection-on-ping connection))
                    (:pong (websocket-connection-on-pong connection)))))
    (when callback
      (apply callback connection args))))

;;; Utilities

(defun wait-for-close (connection &key (timeout 5))
  "Wait for connection to close (with timeout)"
  (let ((start-time (get-internal-real-time)))
    (loop while (not (connection-closed-p connection))
          do (sleep 0.1)
             (when (and timeout
                        (> (/ (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)
                           timeout))
               (force-close connection)
               (return-from wait-for-close nil)))
    (connection-closed-p connection)))
