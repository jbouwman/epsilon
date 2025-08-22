;;;; HTTP/2 Frame Implementation
;;;;
;;;; Implements HTTP/2 frame parsing and serialization according to RFC 7540

(defpackage :epsilon.http2.frames
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:hpack #:epsilon.http2.hpack))
  (:export
   ;; Frame types
   #:+frame-data+
   #:+frame-headers+
   #:+frame-priority+
   #:+frame-rst-stream+
   #:+frame-settings+
   #:+frame-push-promise+
   #:+frame-ping+
   #:+frame-goaway+
   #:+frame-window-update+
   #:+frame-continuation+
   
   ;; Frame flags
   #:+flag-end-stream+
   #:+flag-end-headers+
   #:+flag-padded+
   #:+flag-priority+
   #:+flag-ack+
   
   ;; Settings
   #:+settings-header-table-size+
   #:+settings-enable-push+
   #:+settings-max-concurrent-streams+
   #:+settings-initial-window-size+
   #:+settings-max-frame-size+
   #:+settings-max-header-list-size+
   
   ;; Error codes
   #:+error-no-error+
   #:+error-protocol-error+
   #:+error-internal-error+
   #:+error-flow-control-error+
   #:+error-settings-timeout+
   #:+error-stream-closed+
   #:+error-frame-size-error+
   #:+error-refused-stream+
   #:+error-cancel+
   #:+error-compression-error+
   #:+error-connect-error+
   #:+error-enhance-your-calm+
   #:+error-inadequate-security+
   #:+error-http-1-1-required+
   
   ;; Frame structure
   #:http2-frame
   #:make-http2-frame
   #:http2-frame-p
   #:http2-frame-length
   #:http2-frame-type
   #:http2-frame-flags
   #:http2-frame-stream-id
   #:http2-frame-payload
   
   ;; Frame I/O
   #:read-frame
   #:write-frame
   #:read-frame-header
   #:write-frame-header
   
   ;; Frame creation
   #:make-settings-frame
   #:make-ping-frame
   #:make-goaway-frame
   #:make-window-update-frame
   #:make-rst-stream-frame
   #:make-data-frame
   #:make-headers-frame
   
   ;; Header processing
   #:decode-headers-from-payload
   
   ;; Validation
   #:valid-frame-p))

(in-package :epsilon.http2.frames)

;;;; Frame Types (RFC 7540 Section 6)

(defconstant +frame-data+ #x0)
(defconstant +frame-headers+ #x1)
(defconstant +frame-priority+ #x2)
(defconstant +frame-rst-stream+ #x3)
(defconstant +frame-settings+ #x4)
(defconstant +frame-push-promise+ #x5)
(defconstant +frame-ping+ #x6)
(defconstant +frame-goaway+ #x7)
(defconstant +frame-window-update+ #x8)
(defconstant +frame-continuation+ #x9)

;;;; Frame Flags

(defconstant +flag-end-stream+ #x1)
(defconstant +flag-end-headers+ #x4)
(defconstant +flag-padded+ #x8)
(defconstant +flag-priority+ #x20)
(defconstant +flag-ack+ #x1)

;;;; Settings Parameters (RFC 7540 Section 6.5.2)

(defconstant +settings-header-table-size+ #x1)
(defconstant +settings-enable-push+ #x2)
(defconstant +settings-max-concurrent-streams+ #x3)
(defconstant +settings-initial-window-size+ #x4)
(defconstant +settings-max-frame-size+ #x5)
(defconstant +settings-max-header-list-size+ #x6)

;;;; Error Codes (RFC 7540 Section 7)

(defconstant +error-no-error+ #x0)
(defconstant +error-protocol-error+ #x1)
(defconstant +error-internal-error+ #x2)
(defconstant +error-flow-control-error+ #x3)
(defconstant +error-settings-timeout+ #x4)
(defconstant +error-stream-closed+ #x5)
(defconstant +error-frame-size-error+ #x6)
(defconstant +error-refused-stream+ #x7)
(defconstant +error-cancel+ #x8)
(defconstant +error-compression-error+ #x9)
(defconstant +error-connect-error+ #xa)
(defconstant +error-enhance-your-calm+ #xb)
(defconstant +error-inadequate-security+ #xc)
(defconstant +error-http-1-1-required+ #xd)

;;;; Frame Structure

(defstruct http2-frame
  "HTTP/2 frame structure"
  (length 0 :type (unsigned-byte 24))
  (type 0 :type (unsigned-byte 8))
  (flags 0 :type (unsigned-byte 8))
  (stream-id 0 :type (unsigned-byte 31))
  (payload nil :type (or null (vector (unsigned-byte 8)))))

;;;; Frame Parsing

(defun read-frame-header (stream)
  "Read a 9-byte frame header from stream"
  (let ((header (make-array 9 :element-type '(unsigned-byte 8))))
    (read-sequence header stream)
    (let* ((length (logior (ash (aref header 0) 16)
                          (ash (aref header 1) 8)
                          (aref header 2)))
           (type (aref header 3))
           (flags (aref header 4))
           (stream-id (logand #x7fffffff
                             (logior (ash (aref header 5) 24)
                                    (ash (aref header 6) 16)
                                    (ash (aref header 7) 8)
                                    (aref header 8)))))
      (make-http2-frame :length length
                        :type type
                        :flags flags
                        :stream-id stream-id))))

(defun read-frame (stream)
  "Read a complete HTTP/2 frame from stream"
  (let ((frame (read-frame-header stream)))
    (when (> (http2-frame-length frame) 0)
      (let ((payload (make-array (http2-frame-length frame) 
                                :element-type '(unsigned-byte 8))))
        (read-sequence payload stream)
        (setf (http2-frame-payload frame) payload)))
    frame))

(defun write-frame-header (stream frame)
  "Write a 9-byte frame header to stream"
  (let ((header (make-array 9 :element-type '(unsigned-byte 8))))
    ;; Length (24 bits)
    (setf (aref header 0) (logand #xff (ash (http2-frame-length frame) -16)))
    (setf (aref header 1) (logand #xff (ash (http2-frame-length frame) -8)))
    (setf (aref header 2) (logand #xff (http2-frame-length frame)))
    ;; Type (8 bits)
    (setf (aref header 3) (http2-frame-type frame))
    ;; Flags (8 bits)
    (setf (aref header 4) (http2-frame-flags frame))
    ;; Stream ID (31 bits with reserved bit)
    (setf (aref header 5) (logand #x7f (ash (http2-frame-stream-id frame) -24)))
    (setf (aref header 6) (logand #xff (ash (http2-frame-stream-id frame) -16)))
    (setf (aref header 7) (logand #xff (ash (http2-frame-stream-id frame) -8)))
    (setf (aref header 8) (logand #xff (http2-frame-stream-id frame)))
    (write-sequence header stream)))

(defun write-frame (stream frame)
  "Write a complete HTTP/2 frame to stream"
  (write-frame-header stream frame)
  (when (http2-frame-payload frame)
    (write-sequence (http2-frame-payload frame) stream))
  (force-output stream))

;;;; Frame Creation Helpers

(defun make-settings-frame (&key ack initial-settings)
  "Create a SETTINGS frame"
  (if ack
      (make-http2-frame :type +frame-settings+
                        :flags +flag-ack+
                        :stream-id 0
                        :length 0)
      (let* ((settings (or initial-settings
                          (list (cons +settings-initial-window-size+ 65535)
                                (cons +settings-max-frame-size+ 16384)
                                (cons +settings-max-concurrent-streams+ 100))))
             (payload-size (* 6 (length settings)))
             (payload (make-array payload-size :element-type '(unsigned-byte 8)))
             (offset 0))
        (dolist (setting settings)
          ;; Identifier (16 bits)
          (setf (aref payload offset) (logand #xff (ash (car setting) -8)))
          (setf (aref payload (1+ offset)) (logand #xff (car setting)))
          ;; Value (32 bits)
          (setf (aref payload (+ offset 2)) (logand #xff (ash (cdr setting) -24)))
          (setf (aref payload (+ offset 3)) (logand #xff (ash (cdr setting) -16)))
          (setf (aref payload (+ offset 4)) (logand #xff (ash (cdr setting) -8)))
          (setf (aref payload (+ offset 5)) (logand #xff (cdr setting)))
          (incf offset 6))
        (make-http2-frame :type +frame-settings+
                          :flags 0
                          :stream-id 0
                          :length payload-size
                          :payload payload))))

(defun make-ping-frame (&key ack data)
  "Create a PING frame"
  (let ((payload (or data (make-array 8 :element-type '(unsigned-byte 8) 
                                        :initial-element 0))))
    (make-http2-frame :type +frame-ping+
                      :flags (if ack +flag-ack+ 0)
                      :stream-id 0
                      :length 8
                      :payload payload)))

(defun make-goaway-frame (last-stream-id error-code &optional debug-data)
  "Create a GOAWAY frame"
  (let* ((debug-bytes (when debug-data
                        (str:string-to-octets debug-data)))
         (payload-size (+ 8 (if debug-bytes (length debug-bytes) 0)))
         (payload (make-array payload-size :element-type '(unsigned-byte 8))))
    ;; Last Stream ID (31 bits with reserved bit)
    (setf (aref payload 0) (logand #x7f (ash last-stream-id -24)))
    (setf (aref payload 1) (logand #xff (ash last-stream-id -16)))
    (setf (aref payload 2) (logand #xff (ash last-stream-id -8)))
    (setf (aref payload 3) (logand #xff last-stream-id))
    ;; Error Code (32 bits)
    (setf (aref payload 4) (logand #xff (ash error-code -24)))
    (setf (aref payload 5) (logand #xff (ash error-code -16)))
    (setf (aref payload 6) (logand #xff (ash error-code -8)))
    (setf (aref payload 7) (logand #xff error-code))
    ;; Debug data if present
    (when debug-bytes
      (replace payload debug-bytes :start1 8))
    (make-http2-frame :type +frame-goaway+
                      :flags 0
                      :stream-id 0
                      :length payload-size
                      :payload payload)))

(defun make-window-update-frame (stream-id increment)
  "Create a WINDOW_UPDATE frame"
  (let ((payload (make-array 4 :element-type '(unsigned-byte 8))))
    ;; Window Size Increment (31 bits with reserved bit)
    (setf (aref payload 0) (logand #x7f (ash increment -24)))
    (setf (aref payload 1) (logand #xff (ash increment -16)))
    (setf (aref payload 2) (logand #xff (ash increment -8)))
    (setf (aref payload 3) (logand #xff increment))
    (make-http2-frame :type +frame-window-update+
                      :flags 0
                      :stream-id stream-id
                      :length 4
                      :payload payload)))

(defun make-rst-stream-frame (stream-id error-code)
  "Create a RST_STREAM frame"
  (let ((payload (make-array 4 :element-type '(unsigned-byte 8))))
    ;; Error Code (32 bits)
    (setf (aref payload 0) (logand #xff (ash error-code -24)))
    (setf (aref payload 1) (logand #xff (ash error-code -16)))
    (setf (aref payload 2) (logand #xff (ash error-code -8)))
    (setf (aref payload 3) (logand #xff error-code))
    (make-http2-frame :type +frame-rst-stream+
                      :flags 0
                      :stream-id stream-id
                      :length 4
                      :payload payload)))

(defun make-data-frame (stream-id data &key end-stream padded)
  "Create a DATA frame"
  (let* ((data-bytes (if (stringp data)
                        (str:string-to-octets data)
                        data))
         (flags (logior (if end-stream +flag-end-stream+ 0)
                       (if padded +flag-padded+ 0)))
         (pad-length (if padded (random 16) 0))
         (payload-size (+ (length data-bytes)
                         (if padded (1+ pad-length) 0)))
         (payload (make-array payload-size :element-type '(unsigned-byte 8))))
    (if padded
        (progn
          (setf (aref payload 0) pad-length)
          (replace payload data-bytes :start1 1)
          ;; Fill padding with zeros
          (loop for i from (1+ (length data-bytes)) below payload-size
                do (setf (aref payload i) 0)))
        (replace payload data-bytes))
    (make-http2-frame :type +frame-data+
                      :flags flags
                      :stream-id stream-id
                      :length payload-size
                      :payload payload)))

(defun make-headers-frame (stream-id headers &key end-stream end-headers priority)
  "Create a HEADERS frame with HPACK encoded headers"
  ;; Use HPACK encoder
  (let* ((encoder (hpack:create-encoder))
         (header-block (hpack:encode-headers encoder headers))
         (flags (logior (if end-stream +flag-end-stream+ 0)
                       (if end-headers +flag-end-headers+ 0)
                       (if priority +flag-priority+ 0)))
         (payload header-block))
    (make-http2-frame :type +frame-headers+
                      :flags flags
                      :stream-id stream-id
                      :length (length payload)
                      :payload payload)))

(defun decode-headers-from-payload (payload)
  "Decode headers from frame payload using HPACK"
  (let ((decoder (hpack:create-decoder)))
    (hpack:decode-headers decoder payload)))

;;;; Frame Validation

(defun valid-frame-p (frame)
  "Validate frame according to RFC 7540"
  (and (<= (http2-frame-length frame) #xffffff)  ; Max 24-bit value
       (<= (http2-frame-type frame) +frame-continuation+)
       (or (zerop (http2-frame-stream-id frame))  ; Connection frames
           (plusp (http2-frame-stream-id frame))))) ; Stream frames

