;;;; HTTP/2 Flow Control Implementation
;;;;
;;;; Implements connection and stream-level flow control per RFC 7540

(defpackage :epsilon.http2.flow-control
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string))
  (:export
   ;; Constants
   #:+default-initial-window-size+
   #:+max-window-size+
   
   ;; Flow controller
   #:flow-controller
   #:make-flow-controller
   #:flow-controller-p
   #:flow-controller-send-window
   #:flow-controller-recv-window
   #:flow-controller-initial-send-window
   #:flow-controller-initial-recv-window
   #:make-connection-flow-controller
   #:make-stream-flow-controller
   
   ;; Window management
   #:can-send-p
   #:consume-send-window
   #:update-send-window
   #:consume-recv-window
   #:should-send-window-update-p
   #:calculate-window-update
   #:update-recv-window
   
   ;; Settings
   #:apply-settings-to-flow-control
   
   ;; Frame handling
   #:handle-data-frame-flow-control
   #:handle-window-update-frame
   #:can-send-data-p
   #:send-data-with-flow-control))

(in-package :epsilon.http2.flow-control)

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

(defun make-stream-flow-controller (&optional initial-window-size)
  "Create flow controller for stream-level flow control"
  (let ((initial-window (or initial-window-size +default-initial-window-size+)))
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

(defun apply-settings-to-flow-control (controller settings-alist)
  "Apply SETTINGS frame changes to flow control"
  ;; The settings ID for initial window size is 4 (0x4)
  (let ((new-initial-window (cdr (assoc 4 settings-alist))))
    (when new-initial-window
      ;; Update initial window size
      (let ((delta (- new-initial-window 
                     (flow-controller-initial-send-window controller))))
        ;; Apply delta to current window
        (incf (flow-controller-send-window controller) delta)
        (setf (flow-controller-initial-send-window controller) new-initial-window)))))

;;;; Simplified Frame Handling
;; These functions provide flow control logic but don't directly
;; manipulate connection/stream objects to avoid circular dependencies

(defun handle-data-frame-flow-control (conn-controller stream-controller data-length)
  "Handle flow control for incoming DATA frame"
  ;; Check connection-level window
  (consume-recv-window conn-controller data-length)
  
  ;; Check stream-level window if provided
  (when stream-controller
    (consume-recv-window stream-controller data-length))
  
  ;; Return window update recommendations
  (values (when (should-send-window-update-p conn-controller)
            (calculate-window-update conn-controller))
          (when (and stream-controller 
                     (should-send-window-update-p stream-controller))
            (calculate-window-update stream-controller))))

(defun handle-window-update-frame (controller increment)
  "Handle incoming WINDOW_UPDATE frame"
  (when (zerop increment)
    (error "WINDOW_UPDATE with zero increment"))
  
  (update-send-window controller increment))

(defun can-send-data-p (conn-controller stream-controller bytes)
  "Check if we can send data on a stream"
  (and (can-send-p conn-controller bytes)
       (or (null stream-controller)
           (can-send-p stream-controller bytes))))

(defun send-data-with-flow-control (conn-controller stream-controller data)
  "Check flow control before sending DATA frame"
  (let* ((data-bytes (if (stringp data)
                         (str:string-to-octets data)
                         data))
         (data-length (length data-bytes)))
    
    ;; Check available window
    (unless (can-send-p conn-controller data-length)
      (error "Insufficient connection window for ~D bytes" data-length))
    (when stream-controller
      (unless (can-send-p stream-controller data-length)
        (error "Insufficient stream window for ~D bytes" data-length)))
    
    ;; Consume window
    (consume-send-window conn-controller data-length)
    (when stream-controller
      (consume-send-window stream-controller data-length))
    
    ;; Return the data to send
    data-bytes))