;;;; TCP High-Level Operations for Linux
;;;;
;;;; TCP-specific high-level operations using unified socket utilities

(defpackage epsilon.net.tcp
  (:use cl)
  (:local-nicknames
   (sockets epsilon.net.sockets)
   (types epsilon.net.types)
   (errors epsilon.net.errors))
  (:export
   ;; Re-export socket operations for TCP
   #:tcp-bind
   #:tcp-accept
   #:tcp-incoming
   #:tcp-try-accept
   #:tcp-poll-accept
   #:tcp-local-addr
   #:tcp-connect
   #:tcp-read
   #:tcp-write
   #:tcp-write-all
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
   
   ;; TCP-specific high-level utilities
   #:with-tcp-server
   #:with-tcp-connection
   #:tcp-echo-server
   #:tcp-read-line
   #:tcp-write-line))

(in-package epsilon.net.tcp)

;;; ============================================================================
;;; Re-exports from socket operations
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address"
  (sockets:tcp-bind address :backlog backlog :reuse-addr reuse-addr))

(defun tcp-accept (listener &key (timeout nil))
  "Accept a new incoming connection"
  (sockets:tcp-accept listener :timeout timeout))

(defun tcp-incoming (listener)
  "Return a list of pending incoming connections"
  (sockets:tcp-incoming listener))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (sockets:tcp-try-accept listener))

(defun tcp-poll-accept (listener timeout-ms)
  "Poll for incoming connections with timeout"
  (sockets:tcp-poll-accept listener timeout-ms))

(defun tcp-local-addr (listener)
  "Get the local address of a TCP listener"
  (sockets:tcp-local-addr listener))

(defun tcp-connect (address &key (timeout nil))
  "Connect to a TCP server"
  (sockets:tcp-connect address :timeout timeout))

(defun tcp-read (stream buffer &key (start 0) (end (length buffer)) (timeout nil))
  "Read data from TCP stream"
  (sockets:tcp-read stream buffer :start start :end end :timeout timeout))

(defun tcp-write (stream data &key (start 0) (end nil) (timeout nil))
  "Write data to TCP stream"
  (sockets:tcp-write stream data :start start :end end :timeout timeout))

(defun tcp-write-all (stream data &key (start 0) (end nil))
  "Write all data to TCP stream"
  (sockets:tcp-write-all stream data :start start :end end))

(defun tcp-flush (stream)
  "Flush any buffered output"
  (sockets:tcp-flush stream))

(defun tcp-try-read (stream buffer &key (start 0) (end (length buffer)))
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
;;; TCP-Specific High-Level Utilities
;;; ============================================================================

(defmacro with-tcp-server ((server address &key (backlog 128) (reuse-addr t)) &body body)
  "Execute body with a TCP server bound to address"
  `(let ((,server (tcp-bind ,address :backlog ,backlog :reuse-addr ,reuse-addr)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (sockets:close-socket (types:tcp-listener-handle ,server))))))

(defmacro with-tcp-connection ((connection address &key (timeout nil)) &body body)
  "Execute body with a TCP connection to address"
  `(let ((,connection (tcp-connect ,address :timeout ,timeout)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (tcp-shutdown ,connection :how :both)))))

(defun tcp-echo-server (listener &key (thread-per-client t))
  "Start a simple echo server that echoes back received data"
  (if thread-per-client
      ;; Multi-threaded version
      (loop
       (let ((client (tcp-accept listener)))
         (when client
           (sb-thread:make-thread
            (lambda ()
              (unwind-protect
                   (tcp-echo-client-handler client)
                (ignore-errors (tcp-shutdown client :how :both))))
            :name "tcp-echo-client"))))
      ;; Single-threaded version
      (loop
       (let ((clients (tcp-incoming listener)))
         (dolist (client clients)
           (tcp-echo-client-handler client))))))

(defun tcp-echo-client-handler (client)
  "Handle a single client for echo server"
  (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
    (loop
     (let ((bytes-read (tcp-try-read client buffer)))
       (cond
         ((zerop bytes-read)
          ;; No data or connection closed
          (unless (tcp-connected-p client)
            (return)))
         (t
          ;; Echo back the data
          (tcp-write-all client buffer :end bytes-read)))))))

(defun tcp-read-line (stream &key (timeout nil) (max-length 4096))
  "Read a line from TCP stream (terminated by newline)"
  (let ((buffer (make-array max-length :element-type '(unsigned-byte 8)))
        (pos 0))
    (loop
     (when (>= pos max-length)
       (error 'errors:network-error :message "Line too long"))
     
     (let ((bytes-read (tcp-read stream buffer :start pos :end (1+ pos) :timeout timeout)))
       (cond
         ((zerop bytes-read)
          ;; Connection closed or timeout
          (if (zerop pos)
              (return nil) ; No data read
              (return (sb-ext:octets-to-string (subseq buffer 0 pos)))))
         (t
          (let ((byte (aref buffer pos)))
            (incf pos)
            (when (= byte 10) ; newline
              (return (sb-ext:octets-to-string 
                       (subseq buffer 0 (if (and (> pos 1) (= (aref buffer (- pos 2)) 13))
                                             (- pos 2) ; Remove CRLF
                                             (- pos 1)))))))))))) ; Remove LF

(defun tcp-write-line (stream line &key (timeout nil))
  "Write a line to TCP stream (adds newline)"
  (let ((data (sb-ext:string-to-octets (concatenate 'string line (string #\Newline)))))
    (tcp-write-all stream data)))