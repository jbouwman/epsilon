;;;; TCP Operations Wrapper
;;;;
;;;; This module provides the high-level TCP API by delegating to the sockets module.

(defpackage epsilon.net.tcp
  (:use cl)
  (:local-nicknames
   (sockets epsilon.net.sockets)
   (types epsilon.net.types))
  ;; Re-export most functions directly from sockets module
  (:import-from epsilon.net.sockets
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
   #:tcp-connected-p)
  (:export
   ;; Re-exported from sockets
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
   
   ;; Value-added functions defined here
   #:tcp-write-line
   #:tcp-read-line
   
   ;; Macros
   #:with-tcp-server
   #:with-tcp-connection))

(in-package epsilon.net.tcp)

;;; ============================================================================
;;; Value-Added TCP Operations
;;; ============================================================================

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