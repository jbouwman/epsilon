;;;; HTTP/2 Flow Control Implementation
;;;;
;;;; Implements connection and stream-level flow control per RFC 7540

(in-package :epsilon.http2)

;;;; Constants

(defconstant +default-initial-window-size+ 65535
  "Default initial window size for flow control")

(defconstant +max-window-size+ (1- (ash 1 31))
  "Maximum allowed window size (2^31 - 1)")

;;;; Flow Control State

(defstruct flow-controller
  "Manages flow control for a connection or stream"
  (send-window +default-initial-window-size+ :type integer)
  (recv-window +default-initial-window-size+ :type integer)
  (initial-send-window +default-initial-window-size+ :type integer)
  (initial-recv-window +default-initial-window-size+ :type integer))

(defun make-connection-flow-controller ()
  "Create flow controller for connection-level flow control"
  (make-flow-controller))

(defun make-stream-flow-controller (connection-settings)
  "Create flow controller for stream-level flow control"
  (let ((initial-window (or (cdr (assoc +settings-initial-window-size+ 
                                        connection-settings))
                            +default-initial-window-size+)))
    (make-flow-controller :send-window initial-window
                         :recv-window initial-window
                         :initial-send-window initial-window
                         :initial-recv-window initial-window)))

;;;; Window Management

(defun can-send-p (controller bytes)
  "Check if we can send the specified number of bytes"
  (>= (flow-controller-send-window controller) bytes))

(defun consume-send-window (controller bytes)
  "Consume bytes from send window"
  (when (> bytes (flow-controller-send-window controller))
    (error "Insufficient send window: have ~D, need ~D"
           (flow-controller-send-window controller) bytes))
  (decf (flow-controller-send-window controller) bytes))

(defun update-send-window (controller increment)
  "Update send window with WINDOW_UPDATE increment"
  (let ((new-window (+ (flow-controller-send-window controller) increment)))
    (when (> new-window +max-window-size+)
      (error "Window overflow: ~D > ~D" new-window +max-window-size+))
    (setf (flow-controller-send-window controller) new-window)))

(defun consume-recv-window (controller bytes)
  "Consume bytes from receive window"
  (when (> bytes (flow-controller-recv-window controller))
    (error "Flow control error: received ~D bytes, window is ~D"
           bytes (flow-controller-recv-window controller)))
  (decf (flow-controller-recv-window controller) bytes))

(defun should-send-window-update-p (controller &optional (threshold 0.5))
  "Check if we should send a WINDOW_UPDATE"
  (let ((consumed (- (flow-controller-initial-recv-window controller)
                    (flow-controller-recv-window controller))))
    (> consumed (* threshold (flow-controller-initial-recv-window controller)))))

(defun calculate-window-update (controller)
  "Calculate window update size"
  (- (flow-controller-initial-recv-window controller)
     (flow-controller-recv-window controller)))

(defun update-recv-window (controller increment)
  "Update receive window after sending WINDOW_UPDATE"
  (let ((new-window (+ (flow-controller-recv-window controller) increment)))
    (when (> new-window +max-window-size+)
      (error "Window overflow: ~D > ~D" new-window +max-window-size+))
    (setf (flow-controller-recv-window controller) new-window)))

;;;; Settings Updates

(defun apply-settings-to-flow-control (controller settings)
  "Apply SETTINGS frame changes to flow control"
  (let ((new-initial-window (cdr (assoc +settings-initial-window-size+ settings))))
    (when new-initial-window
      ;; Update initial window size
      (let ((delta (- new-initial-window 
                     (flow-controller-initial-send-window controller))))
        ;; Apply delta to current window
        (incf (flow-controller-send-window controller) delta)
        (setf (flow-controller-initial-send-window controller) new-initial-window)))))

;;;; Frame Handling with Flow Control

(defun handle-data-frame-flow-control (connection stream-id data-length)
  "Handle flow control for incoming DATA frame"
  (let ((conn-controller (connection-flow-controller connection))
        (stream (get-stream connection stream-id)))
    
    ;; Check connection-level window
    (consume-recv-window conn-controller data-length)
    
    ;; Check stream-level window if stream exists
    (when stream
      (consume-recv-window (stream-flow-controller stream) data-length))
    
    ;; Send WINDOW_UPDATE if needed
    (when (should-send-window-update-p conn-controller)
      (let ((increment (calculate-window-update conn-controller)))
        (send-window-update connection 0 increment)
        (update-recv-window conn-controller increment)))
    
    (when (and stream (should-send-window-update-p (stream-flow-controller stream)))
      (let ((increment (calculate-window-update (stream-flow-controller stream))))
        (send-window-update connection stream-id increment)
        (update-recv-window (stream-flow-controller stream) increment)))))

(defun handle-window-update-frame (connection stream-id increment)
  "Handle incoming WINDOW_UPDATE frame"
  (when (zerop increment)
    (signal-protocol-error connection +error-protocol-error+ 
                          "WINDOW_UPDATE with zero increment"))
  
  (if (zerop stream-id)
      ;; Connection-level update
      (update-send-window (connection-flow-controller connection) increment)
      ;; Stream-level update
      (let ((stream (get-stream connection stream-id)))
        (when stream
          (update-send-window (stream-flow-controller stream) increment)))))

(defun can-send-data-p (connection stream-id bytes)
  "Check if we can send data on a stream"
  (and (can-send-p (connection-flow-controller connection) bytes)
       (let ((stream (get-stream connection stream-id)))
         (and stream 
              (can-send-p (stream-flow-controller stream) bytes)))))

(defun send-data-with-flow-control (connection stream-id data)
  "Send DATA frame with flow control"
  (let* ((data-bytes (if (stringp data)
                         (epsilon.string:string-to-octets data)
                         data))
         (data-length (length data-bytes))
         (conn-controller (connection-flow-controller connection))
         (stream (get-stream connection stream-id)))
    
    (unless stream
      (error "Stream ~D does not exist" stream-id))
    
    (let ((stream-controller (stream-flow-controller stream)))
      ;; Check available window
      (unless (can-send-p conn-controller data-length)
        (error "Insufficient connection window for ~D bytes" data-length))
      (unless (can-send-p stream-controller data-length)
        (error "Insufficient stream window for ~D bytes" data-length))
      
      ;; Consume window
      (consume-send-window conn-controller data-length)
      (consume-send-window stream-controller data-length)
      
      ;; Send frame
      (let ((frame (make-data-frame stream-id data-bytes)))
        (connection-send-frame connection frame)))))

;;;; Window Update Helpers

(defun send-window-update (connection stream-id increment)
  "Send a WINDOW_UPDATE frame"
  (let ((frame (make-window-update-frame stream-id increment)))
    (connection-send-frame connection frame)))

(defun signal-protocol-error (connection error-code message)
  "Signal a protocol error"
  (format t "Protocol error: ~A~%" message)
  ;; Send GOAWAY frame
  (let ((frame (make-goaway-frame 
                (connection-last-stream-id connection)
                error-code
                message)))
    (connection-send-frame connection frame))
  ;; Close connection
  (connection-close connection))

;;;; Integration with Connection

(defun connection-flow-controller (connection)
  "Get or create connection flow controller"
  (or (http2-connection-flow-controller connection)
      (setf (http2-connection-flow-controller connection)
            (make-connection-flow-controller))))

(defun stream-flow-controller (stream)
  "Get or create stream flow controller"
  (or (http2-stream-flow-controller stream)
      (setf (http2-stream-flow-controller stream)
            (make-stream-flow-controller 
             (http2-connection-local-settings 
              (http2-stream-connection stream))))))

(defun get-stream (connection stream-id)
  "Get stream by ID"
  (gethash stream-id (http2-connection-streams connection)))

(defun connection-last-stream-id (connection)
  "Get last stream ID"
  (http2-connection-last-stream-id connection))

;;;; Export symbols

(export '(make-connection-flow-controller
          make-stream-flow-controller
          flow-controller
          flow-controller-p
          can-send-p
          handle-data-frame-flow-control
          handle-window-update-frame
          can-send-data-p
          send-data-with-flow-control
          +default-initial-window-size+
          +max-window-size+))