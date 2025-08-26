;;;; HTTP/2 Stream Management
;;;;
;;;; Implements stream lifecycle and state management per RFC 7540

(defpackage :epsilon.http2.stream
  (:use :cl)
  (:local-nicknames
   (#:flow #:epsilon.http2.flow-control)
   (#:frames #:epsilon.http2.frames))
  (:export
   ;; Stream states
   #:+stream-idle+
   #:+stream-reserved-local+
   #:+stream-reserved-remote+
   #:+stream-open+
   #:+stream-half-closed-local+
   #:+stream-half-closed-remote+
   #:+stream-closed+
   
   ;; Stream structure
   #:http2-stream
   #:make-http2-stream
   #:stream-id
   #:stream-state
   #:stream-connection
   #:stream-flow-controller
   #:stream-dependency
   #:stream-weight
   #:stream-headers-buffer
   #:stream-data-buffer
   #:stream-priority
   
   ;; State transitions
   #:transition-stream-state
   #:can-receive-frame-p
   #:can-send-frame-p
   #:stream-closed-p
   #:stream-open-p
   
   ;; Stream operations
   #:initialize-stream
   #:reserve-stream
   #:open-stream
   #:close-stream
   #:reset-stream
   
   ;; Frame handling
   #:process-frame-for-stream
   #:validate-frame-for-state))

(in-package :epsilon.http2.stream)

;;;; Stream States (RFC 7540 Section 5.1)

(defconstant +stream-idle+ 0 "Stream not yet created")
(defconstant +stream-reserved-local+ 1 "Reserved by local PUSH_PROMISE")
(defconstant +stream-reserved-remote+ 2 "Reserved by remote PUSH_PROMISE")
(defconstant +stream-open+ 3 "Stream is open for both directions")
(defconstant +stream-half-closed-local+ 4 "Local endpoint has sent END_STREAM")
(defconstant +stream-half-closed-remote+ 5 "Remote endpoint has sent END_STREAM")
(defconstant +stream-closed+ 6 "Stream is fully closed")

;;;; Stream Structure

(defstruct http2-stream
  "HTTP/2 stream representation"
  (id 0 :type (unsigned-byte 31))
  (state +stream-idle+ :type fixnum)
  (connection nil)
  (flow-controller nil :type (or null flow:flow-controller))
  
  ;; Priority fields (RFC 7540 Section 5.3)
  (dependency 0 :type (unsigned-byte 31))
  (weight 16 :type (unsigned-byte 8))
  (exclusive-p nil :type boolean)
  
  ;; Buffers for received data
  (headers-buffer nil :type list)
  (data-buffer nil :type list)
  
  ;; Tracking flags
  (local-end-stream-p nil :type boolean)
  (remote-end-stream-p nil :type boolean)
  (rst-sent-p nil :type boolean)
  (rst-received-p nil :type boolean)
  
  ;; Error tracking
  (error-code nil :type (or null fixnum))
  
  ;; Timestamps
  (created-at (get-internal-real-time) :type integer)
  (last-activity (get-internal-real-time) :type integer))

;;;; Stream Initialization

(defun initialize-stream (id connection &key (initial-window-size 65535))
  "Initialize a new stream"
  (make-http2-stream
   :id id
   :state +stream-idle+
   :connection connection
   :flow-controller (flow:make-stream-flow-controller initial-window-size)))

(defun reserve-stream (stream local-p)
  "Reserve a stream for server push"
  (setf (http2-stream-state stream)
        (if local-p
            +stream-reserved-local+
            +stream-reserved-remote+))
  stream)

(defun open-stream (stream)
  "Open a stream for bidirectional communication"
  (case (http2-stream-state stream)
    (#.+stream-idle+
     (setf (http2-stream-state stream) +stream-open+))
    ((#.+stream-reserved-local+ #.+stream-reserved-remote+)
     (setf (http2-stream-state stream) +stream-half-closed-remote+))
    (otherwise
     (error "Cannot open stream in state ~A" (http2-stream-state stream))))
  stream)

;;;; State Transitions

(defun transition-stream-state (stream frame-type flags)
  "Transition stream state based on frame type and flags"
  (let ((end-stream-p (logtest flags frames:+flag-end-stream+))
        (current-state (http2-stream-state stream)))
    
    (case frame-type
      ;; HEADERS frame
      (#.frames:+frame-headers+
       (case current-state
         (#.+stream-idle+
          (setf (http2-stream-state stream) 
                (if end-stream-p 
                    +stream-half-closed-remote+
                    +stream-open+)))
         (#.+stream-reserved-local+
          (setf (http2-stream-state stream) +stream-half-closed-remote+))
         (#.+stream-reserved-remote+
          (setf (http2-stream-state stream) +stream-half-closed-local+))
         (#.+stream-open+
          (when end-stream-p
            (setf (http2-stream-state stream) +stream-half-closed-remote+
                  (http2-stream-remote-end-stream-p stream) t)))
         (#.+stream-half-closed-local+
          (when end-stream-p
            (setf (http2-stream-state stream) +stream-closed+
                  (http2-stream-remote-end-stream-p stream) t)))))
      
      ;; DATA frame
      (#.frames:+frame-data+
       (case current-state
         (#.+stream-open+
          (when end-stream-p
            (setf (http2-stream-state stream) +stream-half-closed-remote+
                  (http2-stream-remote-end-stream-p stream) t)))
         (#.+stream-half-closed-local+
          (when end-stream-p
            (setf (http2-stream-state stream) +stream-closed+
                  (http2-stream-remote-end-stream-p stream) t)))))
      
      ;; RST_STREAM frame
      (#.frames:+frame-rst-stream+
       (setf (http2-stream-state stream) +stream-closed+
             (http2-stream-rst-received-p stream) t))
      
      ;; PUSH_PROMISE frame
      (#.frames:+frame-push-promise+
       (when (= current-state +stream-idle+)
         (setf (http2-stream-state stream) +stream-reserved-remote+))))
    
    ;; Update last activity
    (setf (http2-stream-last-activity stream) (get-internal-real-time))
    
    stream))

;;;; State Validation

(defun can-receive-frame-p (stream frame-type)
  "Check if stream can receive a frame of given type"
  (let ((state (http2-stream-state stream)))
    (case frame-type
      ;; HEADERS can be received in most states
      (#.frames:+frame-headers+
       (member state '(#.+stream-idle+ 
                      #.+stream-reserved-remote+
                      #.+stream-open+ 
                      #.+stream-half-closed-local+)))
      
      ;; DATA frames
      (#.frames:+frame-data+
       (member state '(#.+stream-open+ 
                      #.+stream-half-closed-local+)))
      
      ;; PRIORITY can be received in any state
      (#.frames:+frame-priority+ t)
      
      ;; RST_STREAM can be received in most states
      (#.frames:+frame-rst-stream+
       (not (= state +stream-idle+)))
      
      ;; PUSH_PROMISE 
      (#.frames:+frame-push-promise+
       (member state '(#.+stream-idle+ 
                      #.+stream-open+
                      #.+stream-half-closed-local+)))
      
      ;; WINDOW_UPDATE
      (#.frames:+frame-window-update+
       (member state '(#.+stream-open+
                      #.+stream-half-closed-local+
                      #.+stream-half-closed-remote+)))
      
      ;; CONTINUATION must follow HEADERS
      (#.frames:+frame-continuation+
       (member state '(#.+stream-open+
                      #.+stream-half-closed-local+
                      #.+stream-half-closed-remote+)))
      
      ;; Unknown frame types
      (otherwise nil))))

(defun can-send-frame-p (stream frame-type)
  "Check if we can send a frame of given type on this stream"
  (let ((state (http2-stream-state stream)))
    (case frame-type
      ;; HEADERS
      (#.frames:+frame-headers+
       (member state '(#.+stream-idle+
                      #.+stream-reserved-local+
                      #.+stream-open+
                      #.+stream-half-closed-remote+)))
      
      ;; DATA
      (#.frames:+frame-data+
       (member state '(#.+stream-open+
                      #.+stream-half-closed-remote+)))
      
      ;; PRIORITY
      (#.frames:+frame-priority+ t)
      
      ;; RST_STREAM
      (#.frames:+frame-rst-stream+
       (not (= state +stream-closed+)))
      
      ;; PUSH_PROMISE (server only)
      (#.frames:+frame-push-promise+
       (member state '(#.+stream-open+
                      #.+stream-half-closed-remote+)))
      
      ;; WINDOW_UPDATE
      (#.frames:+frame-window-update+
       (not (= state +stream-closed+)))
      
      (otherwise nil))))

;;;; Stream State Queries

(defun stream-closed-p (stream)
  "Check if stream is closed"
  (= (http2-stream-state stream) +stream-closed+))

(defun stream-open-p (stream)
  "Check if stream is open for sending data"
  (member (http2-stream-state stream)
          '(#.+stream-open+ #.+stream-half-closed-remote+)))

;;;; Stream Operations

(defun close-stream (stream)
  "Close a stream"
  (setf (http2-stream-state stream) +stream-closed+)
  stream)

(defun reset-stream (stream error-code)
  "Reset a stream with error code"
  (setf (http2-stream-state stream) +stream-closed+
        (http2-stream-error-code stream) error-code
        (http2-stream-rst-sent-p stream) t)
  stream)

;;;; Frame Processing

(defun process-frame-for-stream (stream frame)
  "Process a frame for this stream"
  (unless (can-receive-frame-p stream (frames:http2-frame-type frame))
    (error "Cannot receive frame type ~A in state ~A"
           (frames:http2-frame-type frame)
           (http2-stream-state stream)))
  
  ;; Transition state based on frame
  (transition-stream-state stream 
                           (frames:http2-frame-type frame)
                           (frames:http2-frame-flags frame))
  
  ;; Store frame data if needed
  (case (frames:http2-frame-type frame)
    (#.frames:+frame-headers+
     (push frame (http2-stream-headers-buffer stream)))
    (#.frames:+frame-data+
     (push frame (http2-stream-data-buffer stream))))
  
  stream)

(defun validate-frame-for-state (stream frame-type)
  "Validate that a frame type can be sent/received in current state"
  (can-receive-frame-p stream frame-type))