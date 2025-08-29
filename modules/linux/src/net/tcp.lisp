;;;; TCP Operations Wrapper
;;;;
;;;; This module provides the high-level TCP API by delegating to the sockets module.

(defpackage epsilon.net.tcp
  (:use cl)
  (:local-nicknames
   (sockets epsilon.net.sockets)
   (types epsilon.net.types))
  (:export
   ;; TCP Listener operations
   #:tcp-bind
   #:tcp-accept
   #:tcp-incoming
   #:tcp-try-accept
   #:tcp-poll-accept
   #:tcp-local-addr
   
   ;; TCP Stream operations
   #:tcp-connect
   #:tcp-read
   #:tcp-write
   #:tcp-write-all
   #:tcp-write-line
   #:tcp-read-line
   #:tcp-flush
   #:tcp-try-read
   #:tcp-try-write
   #:tcp-poll-read
   #:tcp-poll-write
   #:tcp-peer-addr
   #:tcp-shutdown
   #:tcp-stream-reader
   #:tcp-stream-writer
   #:tcp-connected-p
   
   ;; Macros
   #:with-tcp-server
   #:with-tcp-connection))

(in-package epsilon.net.tcp)

;;; ============================================================================
;;; TCP Listener Operations
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address"
  (sockets:tcp-bind address :backlog backlog :reuse-addr reuse-addr))

(defun tcp-accept (listener &key (timeout nil))
  "Accept a connection, with optional timeout in seconds"
  (sockets:tcp-accept listener :timeout timeout))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (sockets:tcp-try-accept listener))

(defun tcp-poll-accept (listener timeout-ms)
  "Poll for incoming connections with timeout"
  (sockets:tcp-poll-accept listener timeout-ms))

(defun tcp-local-addr (listener)
  "Get the local address of a TCP listener"
  (sockets:tcp-local-addr listener))

(defun tcp-incoming (listener)
  "Return a list of incoming connections"
  (sockets:tcp-incoming listener))

;;; ============================================================================
;;; TCP Stream Operations
;;; ============================================================================

(defun tcp-connect (address &key (timeout nil))
  "Connect to a TCP server with optional timeout"
  (sockets:tcp-connect address :timeout timeout))

(defun tcp-read (stream buffer &key (start 0) (end nil) (timeout nil))
  "Read data from TCP stream into buffer with optional timeout"
  (sockets:tcp-read stream buffer :start start :end end :timeout timeout))

(defun tcp-write (stream data &key (start 0) (end nil) (timeout nil))
  "Write data to TCP stream with optional timeout"
  (sockets:tcp-write stream data :start start :end end :timeout timeout))

(defun tcp-write-all (stream data &key (start 0) (end nil))
  "Write all data to TCP stream"
  (sockets:tcp-write-all stream data :start start :end end))

(defun tcp-write-line (stream line &key (timeout nil))
  "Write a line to TCP stream"
  (declare (ignore timeout))
  (let* ((data (if (stringp line)
                   (concatenate 'string line (string #\Newline))
                   (concatenate 'vector line #(10))))
         (bytes (if (stringp data)
                    (sb-ext:string-to-octets data)
                    data)))
    (tcp-write-all stream bytes)))

(defun tcp-read-line (stream &key (timeout nil))
  "Read a line from TCP stream"
  (declare (ignore timeout))
  (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
        (line-buffer (make-array 0 :element-type '(unsigned-byte 8) 
                                 :adjustable t :fill-pointer 0)))
    (loop
      (let ((bytes-read (tcp-read stream buffer :end 1024)))
        (when (zerop bytes-read)
          ;; Connection closed
          (return (if (> (length line-buffer) 0)
                      (sb-ext:octets-to-string line-buffer)
                      nil)))
        ;; Look for newline
        (loop for i from 0 below bytes-read
              for byte = (aref buffer i)
              do (if (= byte 10) ; newline
                     (return-from tcp-read-line 
                       (sb-ext:octets-to-string line-buffer))
                     (vector-push-extend byte line-buffer)))))))

(defun tcp-flush (stream)
  "Flush any buffered output"
  (sockets:tcp-flush stream))

(defun tcp-try-read (stream buffer &key (start 0) (end nil))
  "Try to read without blocking"
  (sockets:tcp-try-read stream buffer :start start :end end))

(defun tcp-try-write (stream data &key (start 0) (end nil))
  "Try to write without blocking"
  (sockets:tcp-try-write stream data :start start :end end))

(defun tcp-poll-read (stream timeout-ms)
  "Poll for readable data with timeout"
  (sockets:tcp-poll-read stream timeout-ms))

(defun tcp-poll-write (stream timeout-ms)
  "Poll for writability with timeout"
  (sockets:tcp-poll-write stream timeout-ms))

(defun tcp-peer-addr (stream)
  "Get peer address of TCP stream"
  (sockets:tcp-peer-addr stream))

(defun tcp-shutdown (stream &key (how :both))
  "Shutdown TCP stream"
  (sockets:tcp-shutdown stream :how how))

(defun tcp-stream-reader (stream)
  "Get input stream for TCP stream"
  (sockets:tcp-stream-reader stream))

(defun tcp-stream-writer (stream)
  "Get output stream for TCP stream"
  (sockets:tcp-stream-writer stream))

(defun tcp-connected-p (stream)
  "Check if TCP stream is connected"
  (sockets:tcp-connected-p stream))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-tcp-server ((server address &key (backlog 128) (reuse-addr t)) 
                           &body body)
  "Execute body with a TCP server bound to address"
  `(let ((,server (tcp-bind ,address :backlog ,backlog :reuse-addr ,reuse-addr)))
     (unwind-protect
          (progn ,@body)
       (when ,server
         (handler-case
             (sockets:tcp-shutdown ,server)
           (error ()
             ;; Ignore errors during cleanup
             nil))))))

(defmacro with-tcp-connection ((connection address &key (timeout nil)) 
                               &body body)
  "Execute body with a TCP connection to address"
  `(let ((,connection (tcp-connect ,address :timeout ,timeout)))
     (unwind-protect
          (progn ,@body)
       (when ,connection
         (handler-case
             (tcp-shutdown ,connection)
           (error ()
             ;; Ignore errors during cleanup
             nil))))))