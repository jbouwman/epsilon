;;;; HTTP/2 Error Handling
;;;;
;;;; Implements error detection and handling per RFC 7540

(defpackage :epsilon.http2.error
  (:use :cl)
  (:local-nicknames
   (#:frames #:epsilon.http2.frames))
  (:export
   ;; Error conditions
   #:http2-error
   #:http2-connection-error
   #:http2-stream-error
   #:http2-compression-error
   #:http2-flow-control-error
   #:http2-protocol-error
   #:http2-frame-size-error
   #:http2-settings-timeout-error
   
   ;; Error handling
   #:make-error-response
   #:error-code-name
   #:error-code-description
   #:should-close-connection-p
   
   ;; Protocol violations
   #:check-frame-size
   #:check-stream-id-validity
   #:check-padding
   #:check-settings-values
   #:check-window-update
   #:check-priority-validity
   #:check-header-block-validity
   
   ;; Error frame generation
   #:make-rst-stream-frame
   #:make-goaway-frame))

(in-package :epsilon.http2.error)

;;;; Error Conditions

(define-condition http2-error (error)
  ((error-code :initarg :error-code :reader error-code)
   (message :initarg :message :reader error-message)
   (stream-id :initarg :stream-id :reader error-stream-id :initform 0))
  (:report (lambda (condition stream)
             (format stream "HTTP/2 Error (~A): ~A"
                     (error-code-name (error-code condition))
                     (error-message condition)))))

(define-condition http2-connection-error (http2-error)
  ()
  (:documentation "Error that requires connection termination"))

(define-condition http2-stream-error (http2-error)
  ()
  (:documentation "Error that affects only a single stream"))

(define-condition http2-compression-error (http2-connection-error)
  ((error-code :initform frames:+error-compression-error+))
  (:documentation "HPACK compression error"))

(define-condition http2-flow-control-error (http2-error)
  ((error-code :initform frames:+error-flow-control-error+))
  (:documentation "Flow control protocol violated"))

(define-condition http2-protocol-error (http2-error)
  ((error-code :initform frames:+error-protocol-error+))
  (:documentation "Protocol error detected"))

(define-condition http2-frame-size-error (http2-connection-error)
  ((error-code :initform frames:+error-frame-size-error+))
  (:documentation "Frame size incorrect"))

(define-condition http2-settings-timeout-error (http2-connection-error)
  ((error-code :initform frames:+error-settings-timeout+))
  (:documentation "Settings not acknowledged in time"))

;;;; Error Code Information

(defun error-code-name (code)
  "Get human-readable name for error code"
  (case code
    (#.frames:+error-no-error+ "NO_ERROR")
    (#.frames:+error-protocol-error+ "PROTOCOL_ERROR")
    (#.frames:+error-internal-error+ "INTERNAL_ERROR")
    (#.frames:+error-flow-control-error+ "FLOW_CONTROL_ERROR")
    (#.frames:+error-settings-timeout+ "SETTINGS_TIMEOUT")
    (#.frames:+error-stream-closed+ "STREAM_CLOSED")
    (#.frames:+error-frame-size-error+ "FRAME_SIZE_ERROR")
    (#.frames:+error-refused-stream+ "REFUSED_STREAM")
    (#.frames:+error-cancel+ "CANCEL")
    (#.frames:+error-compression-error+ "COMPRESSION_ERROR")
    (#.frames:+error-connect-error+ "CONNECT_ERROR")
    (#.frames:+error-enhance-your-calm+ "ENHANCE_YOUR_CALM")
    (#.frames:+error-inadequate-security+ "INADEQUATE_SECURITY")
    (#.frames:+error-http-1-1-required+ "HTTP_1_1_REQUIRED")
    (otherwise (format nil "UNKNOWN(~A)" code))))

(defun error-code-description (code)
  "Get description for error code"
  (case code
    (#.frames:+error-no-error+ 
     "Graceful shutdown")
    (#.frames:+error-protocol-error+ 
     "Protocol error detected")
    (#.frames:+error-internal-error+ 
     "Implementation fault")
    (#.frames:+error-flow-control-error+ 
     "Flow-control protocol violated")
    (#.frames:+error-settings-timeout+ 
     "Settings not acknowledged")
    (#.frames:+error-stream-closed+ 
     "Frame received for closed stream")
    (#.frames:+error-frame-size-error+ 
     "Frame size incorrect")
    (#.frames:+error-refused-stream+ 
     "Stream not processed")
    (#.frames:+error-cancel+ 
     "Stream cancelled")
    (#.frames:+error-compression-error+ 
     "Compression state not updated")
    (#.frames:+error-connect-error+ 
     "TCP connection error for CONNECT method")
    (#.frames:+error-enhance-your-calm+ 
     "Processing capacity exceeded")
    (#.frames:+error-inadequate-security+ 
     "Negotiated TLS parameters not acceptable")
    (#.frames:+error-http-1-1-required+ 
     "Use HTTP/1.1 for the request")
    (otherwise "Unknown error")))

(defun should-close-connection-p (error-code)
  "Check if error requires connection closure"
  (member error-code 
          '(#.frames:+error-protocol-error+
            #.frames:+error-internal-error+
            #.frames:+error-flow-control-error+
            #.frames:+error-settings-timeout+
            #.frames:+error-frame-size-error+
            #.frames:+error-compression-error+
            #.frames:+error-inadequate-security+)))

;;;; Protocol Violation Checks

(defun check-frame-size (frame max-frame-size)
  "Check if frame size is valid"
  (let ((size (frames:http2-frame-length frame)))
    (when (> size max-frame-size)
      (error 'http2-frame-size-error
             :message (format nil "Frame size ~A exceeds maximum ~A" 
                            size max-frame-size)
             :stream-id (frames:http2-frame-stream-id frame)))
    
    ;; Check specific frame type requirements
    (case (frames:http2-frame-type frame)
      ;; SETTINGS frames must be multiple of 6 bytes
      (#.frames:+frame-settings+
       (unless (zerop (mod size 6))
         (error 'http2-frame-size-error
                :message "SETTINGS frame size not multiple of 6"
                :stream-id 0)))
      
      ;; WINDOW_UPDATE must be exactly 4 bytes
      (#.frames:+frame-window-update+
       (unless (= size 4)
         (error 'http2-frame-size-error
                :message "WINDOW_UPDATE frame must be 4 bytes"
                :stream-id (frames:http2-frame-stream-id frame))))
      
      ;; PRIORITY must be exactly 5 bytes
      (#.frames:+frame-priority+
       (unless (= size 5)
         (error 'http2-stream-error
                :error-code frames:+error-frame-size-error+
                :message "PRIORITY frame must be 5 bytes"
                :stream-id (frames:http2-frame-stream-id frame))))
      
      ;; RST_STREAM must be exactly 4 bytes
      (#.frames:+frame-rst-stream+
       (unless (= size 4)
         (error 'http2-connection-error
                :error-code frames:+error-frame-size-error+
                :message "RST_STREAM frame must be 4 bytes"
                :stream-id 0))))
      
      ;; PING must be exactly 8 bytes
      (#.frames:+frame-ping+
       (unless (= size 8)
         (error 'http2-connection-error
                :error-code frames:+error-frame-size-error+
                :message "PING frame must be 8 bytes"
                :stream-id 0)))))

(defun check-stream-id-validity (frame)
  "Check if stream ID is valid for frame type"
  (let ((stream-id (frames:http2-frame-stream-id frame))
        (frame-type (frames:http2-frame-type frame)))
    
    ;; Connection-level frames must have stream ID 0
    (when (and (member frame-type 
                       '(#.frames:+frame-settings+
                         #.frames:+frame-ping+
                         #.frames:+frame-goaway+))
               (not (zerop stream-id)))
      (error 'http2-protocol-error
             :message (format nil "Frame type ~A must have stream ID 0" 
                            frame-type)
             :stream-id 0))
    
    ;; Stream-level frames must have non-zero stream ID
    (when (and (member frame-type
                       '(#.frames:+frame-data+
                         #.frames:+frame-headers+
                         #.frames:+frame-priority+
                         #.frames:+frame-rst-stream+
                         #.frames:+frame-push-promise+
                         #.frames:+frame-continuation+))
               (zerop stream-id))
      (error 'http2-protocol-error
             :message (format nil "Frame type ~A must have non-zero stream ID" 
                            frame-type)
             :stream-id 0))))

(defun check-padding (frame)
  "Check padding validity for padded frames"
  (when (logtest (frames:http2-frame-flags frame) frames:+flag-padded+)
    (let* ((payload (frames:http2-frame-payload frame))
           (pad-length (when payload (aref payload 0))))
      (when (and pad-length (>= pad-length (length payload)))
        (error 'http2-protocol-error
               :message "Padding length exceeds frame payload"
               :stream-id (frames:http2-frame-stream-id frame))))))

(defun check-settings-values (settings-alist)
  "Validate SETTINGS parameters"
  (dolist (setting settings-alist)
    (let ((id (car setting))
          (value (cdr setting)))
      (case id
        ;; ENABLE_PUSH must be 0 or 1
        (#.frames:+settings-enable-push+
         (unless (member value '(0 1))
           (error 'http2-protocol-error
                  :message (format nil "ENABLE_PUSH must be 0 or 1, got ~A" value)
                  :stream-id 0)))
        
        ;; INITIAL_WINDOW_SIZE must not exceed 2^31-1
        (#.frames:+settings-initial-window-size+
         (when (> value #x7fffffff)
           (error 'http2-flow-control-error
                  :message (format nil "INITIAL_WINDOW_SIZE too large: ~A" value)
                  :stream-id 0)))
        
        ;; MAX_FRAME_SIZE must be between 16384 and 16777215
        (#.frames:+settings-max-frame-size+
         (unless (<= 16384 value 16777215)
           (error 'http2-protocol-error
                  :message (format nil "MAX_FRAME_SIZE out of range: ~A" value)
                  :stream-id 0)))))))

(defun check-window-update (increment stream-id)
  "Check WINDOW_UPDATE increment validity"
  (when (zerop increment)
    (if (zerop stream-id)
        (error 'http2-protocol-error
               :message "WINDOW_UPDATE increment cannot be 0 on connection"
               :stream-id 0)
        (error 'http2-stream-error
               :error-code frames:+error-protocol-error+
               :message "WINDOW_UPDATE increment cannot be 0 on stream"
               :stream-id stream-id)))
  
  (when (> increment #x7fffffff)
    (error 'http2-flow-control-error
           :message (format nil "WINDOW_UPDATE increment too large: ~A" increment)
           :stream-id stream-id)))

(defun check-priority-validity (stream-id dependency)
  "Check PRIORITY frame validity"
  (when (= stream-id dependency)
    (error 'http2-stream-error
           :error-code frames:+error-protocol-error+
           :message "Stream cannot depend on itself"
           :stream-id stream-id)))

(defun check-header-block-validity (headers)
  "Check header block validity"
  ;; Check for mandatory pseudo-headers
  (let ((has-method nil)
        (has-scheme nil)
        (has-path nil)
        (pseudo-after-regular nil))
    
    (loop for (name . value) in headers
          for is-pseudo = (char= (char name 0) #\:)
          do (cond
               ;; Check pseudo-headers come first
               ((and is-pseudo has-regular)
                (setf pseudo-after-regular t))
               
               ;; Track regular headers
               ((not is-pseudo)
                (setf has-regular t))
               
               ;; Track mandatory pseudo-headers
               ((string= name ":method") (setf has-method t))
               ((string= name ":scheme") (setf has-scheme t))
               ((string= name ":path") (setf has-path t))
               
               ;; Check for connection-specific headers
               ((member name '("connection" "keep-alive" "proxy-connection"
                              "transfer-encoding" "upgrade")
                       :test #'string-equal)
                (error 'http2-protocol-error
                       :message (format nil "Connection-specific header not allowed: ~A" name)
                       :stream-id 0))))
    
    (when pseudo-after-regular
      (error 'http2-protocol-error
             :message "Pseudo-header fields must appear before regular fields"
             :stream-id 0))
    
    ;; For requests, check mandatory headers
    (when (and has-method (not (and has-scheme has-path)))
      (error 'http2-protocol-error
             :message "Request missing mandatory pseudo-headers"
             :stream-id 0))))

;;;; Error Frame Generation

(defun make-rst-stream-frame (stream-id error-code)
  "Create RST_STREAM frame"
  (let ((payload (make-array 4 :element-type '(unsigned-byte 8))))
    ;; Error code (32 bits)
    (setf (aref payload 0) (logand #xff (ash error-code -24)))
    (setf (aref payload 1) (logand #xff (ash error-code -16)))
    (setf (aref payload 2) (logand #xff (ash error-code -8)))
    (setf (aref payload 3) (logand #xff error-code))
    
    (frames:make-http2-frame
     :length 4
     :type frames:+frame-rst-stream+
     :flags 0
     :stream-id stream-id
     :payload payload)))

(defun make-goaway-frame (last-stream-id error-code &optional debug-data)
  "Create GOAWAY frame"
  (let* ((debug-bytes (when debug-data
                        (if (stringp debug-data)
                            (map 'vector #'char-code debug-data)
                            debug-data)))
         (debug-length (if debug-bytes (length debug-bytes) 0))
         (payload (make-array (+ 8 debug-length) :element-type '(unsigned-byte 8))))
    
    ;; Last stream ID (31 bits with reserved bit)
    (setf (aref payload 0) (logand #x7f (ash last-stream-id -24)))
    (setf (aref payload 1) (logand #xff (ash last-stream-id -16)))
    (setf (aref payload 2) (logand #xff (ash last-stream-id -8)))
    (setf (aref payload 3) (logand #xff last-stream-id))
    
    ;; Error code (32 bits)
    (setf (aref payload 4) (logand #xff (ash error-code -24)))
    (setf (aref payload 5) (logand #xff (ash error-code -16)))
    (setf (aref payload 6) (logand #xff (ash error-code -8)))
    (setf (aref payload 7) (logand #xff error-code))
    
    ;; Debug data if present
    (when debug-bytes
      (replace payload debug-bytes :start1 8))
    
    (frames:make-http2-frame
     :length (+ 8 debug-length)
     :type frames:+frame-goaway+
     :flags 0
     :stream-id 0
     :payload payload)))