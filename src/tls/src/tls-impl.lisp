;;;; TLS Implementation Selection
;;;;
;;;; Integrates OpenSSL-based TLS with the epsilon.tls interface

(in-package :epsilon.tls)

;; Try to load OpenSSL implementation
(defvar *tls-implementation* nil)

(defun load-tls-implementation ()
  "Load the TLS implementation"
  (handler-case
      (progn
        ;; OpenSSL implementation is compiled as part of the TLS module
        ;; Check if we can actually use it by trying to initialize
        (when (fboundp 'epsilon.tls.openssl::%ssl-library-init)
          (handler-case
              (progn
                (epsilon.tls.openssl::%ssl-library-init)
                (setf *tls-implementation* :openssl)
                t)
            (error (e)
              (warn "OpenSSL library not available: ~A" e)
              (setf *tls-implementation* :stub)
              nil)))
        (unless *tls-implementation*
          (setf *tls-implementation* :stub)
          nil))
    (error (e)
      (warn "Failed to load TLS implementation: ~A" e)
      (setf *tls-implementation* :stub)
      nil)))

;; Load implementation at compile/load time
(load-tls-implementation)

;;;; Implementation dispatch

(defun implementation-available-p ()
  "Check if a real TLS implementation is available"
  (eq *tls-implementation* :openssl))

;;;; Updated TLS Context functions

(defun create-tls-context (&key server-p)
  "Create a new TLS context"
  (if (implementation-available-p)
      ;; Use OpenSSL implementation
      (epsilon.tls.openssl:make-openssl-context :server-p server-p)
      ;; Fall back to stub
      (make-tls-context :server-p server-p)))

(defun load-cert-file (context cert-file)
  "Load certificate file into TLS context"
  (cond
    ((and (implementation-available-p)
          (typep context 'epsilon.tls.openssl:openssl-context))
     (epsilon.tls.openssl:load-cert-file context cert-file))
    ((tls-context-p context)
     ;; Stub implementation
     (setf (tls-context-cert-file context) cert-file)
     context)
    (t (error "Invalid TLS context"))))

(defun load-key-file (context key-file)
  "Load private key file into TLS context"
  (cond
    ((and (implementation-available-p)
          (typep context 'epsilon.tls.openssl:openssl-context))
     (epsilon.tls.openssl:load-key-file context key-file))
    ((tls-context-p context)
     ;; Stub implementation
     (setf (tls-context-key-file context) key-file)
     context)
    (t (error "Invalid TLS context"))))

(defun set-verify-mode (context mode)
  "Set certificate verification mode"
  (cond
    ((and (implementation-available-p)
          (typep context 'epsilon.tls.openssl:openssl-context))
     (epsilon.tls.openssl:set-verify-mode context mode))
    ((tls-context-p context)
     ;; Stub implementation
     (setf (tls-context-verify-mode context) mode)
     context)
    (t (error "Invalid TLS context"))))

;;;; Updated Connection Management

(defun tls-connect (socket context)
  "Establish TLS connection over existing socket"
  (cond
    ((and (implementation-available-p)
          (typep context 'epsilon.tls.openssl:openssl-context))
     (epsilon.tls.openssl:openssl-connect context socket))
    ((tls-context-p context)
     ;; Stub implementation
     (make-tls-connection :socket socket
                          :context context
                          :connected-p t))
    (t (error "Invalid TLS context"))))

(defun tls-accept (socket context)
  "Accept TLS connection on socket"
  (cond
    ((and (implementation-available-p)
          (typep context 'epsilon.tls.openssl:openssl-context))
     (epsilon.tls.openssl:openssl-accept context socket))
    ((tls-context-p context)
     ;; Stub implementation
     (make-tls-connection :socket socket
                          :context context
                          :connected-p t))
    (t (error "Invalid TLS context"))))

(defun tls-close (connection)
  "Close TLS connection"
  (cond
    ((and (implementation-available-p)
          (typep connection 'epsilon.tls.openssl:openssl-connection))
     (epsilon.tls.openssl:openssl-close connection))
    ((tls-connection-p connection)
     ;; Stub implementation
     (when (tls-connection-socket connection)
       (ignore-errors
         (close (tls-connection-socket connection)))
       (setf (tls-connection-socket connection) nil
             (tls-connection-connected-p connection) nil)))
    (t (error "Invalid TLS connection"))))

;;;; Updated I/O Operations

(defun tls-read (connection buffer &optional (start 0) end)
  "Read data from TLS connection"
  (cond
    ((and (implementation-available-p)
          (typep connection 'epsilon.tls.openssl:openssl-connection))
     (let* ((actual-end (or end (length buffer)))
            (length (- actual-end start))
            (temp-buffer (make-array length :element-type '(unsigned-byte 8))))
       (let ((bytes-read (epsilon.tls.openssl:openssl-read connection temp-buffer length)))
         (when (numberp bytes-read)
           ;; Copy to target buffer
           (dotimes (i bytes-read)
             (setf (aref buffer (+ start i)) (aref temp-buffer i))))
         bytes-read)))
    ((tls-connection-p connection)
     ;; Stub implementation - just read from socket
     (unless (tls-connection-handshake-complete-p connection)
       (error "TLS handshake not complete"))
     (let ((socket (tls-connection-socket connection)))
       (sb-bsd-sockets:socket-receive socket buffer (or end (length buffer)))))
    (t (error "Invalid TLS connection"))))

(defun tls-write (connection data &optional (start 0) end)
  "Write data to TLS connection"
  (cond
    ((and (implementation-available-p)
          (typep connection 'epsilon.tls.openssl:openssl-connection))
     (epsilon.tls.openssl:openssl-write connection data :start start :end (or end (length data))))
    ((tls-connection-p connection)
     ;; Stub implementation - just write to socket
     (unless (tls-connection-handshake-complete-p connection)
       (error "TLS handshake not complete"))
     (let ((socket (tls-connection-socket connection))
           (actual-end (or end (length data))))
       (sb-bsd-sockets:socket-send socket (subseq data start actual-end) 
                                   (- actual-end start))))
    (t (error "Invalid TLS connection"))))

;;;; Stream Interface

(defun tls-stream (connection)
  "Get stream interface for TLS connection"
  (cond
    ((and (implementation-available-p)
          (typep connection 'epsilon.tls.openssl:openssl-connection))
     (epsilon.tls.openssl:openssl-stream connection))
    ((tls-connection-p connection)
     ;; Stub implementation
     (sb-bsd-sockets:socket-make-stream (tls-connection-socket connection)
                                        :input t :output t
                                        :element-type 'character))
    (t (error "Invalid TLS connection"))))

;;;; Information Functions

(defun tls-version (connection)
  "Get TLS version for connection"
  (cond
    ((and (implementation-available-p)
          (typep connection 'epsilon.tls.openssl:openssl-connection))
     (epsilon.tls.openssl:openssl-get-version connection))
    ((tls-connection-p connection)
     "TLS 1.2 (simulated)")
    (t (error "Invalid TLS connection"))))

(defun tls-cipher (connection)
  "Get cipher suite for connection"
  (cond
    ((and (implementation-available-p)
          (typep connection 'epsilon.tls.openssl:openssl-connection))
     (epsilon.tls.openssl:openssl-get-cipher connection))
    ((tls-connection-p connection)
     "AES256-GCM-SHA384 (simulated)")
    (t (error "Invalid TLS connection"))))

(defun get-peer-certificate (connection)
  "Get peer certificate information"
  (cond
    ((and (implementation-available-p)
          (typep connection 'epsilon.tls.openssl:openssl-connection))
     (epsilon.tls.openssl:openssl-get-peer-certificate connection))
    ((tls-connection-p connection)
     ;; Stub implementation
     (map:make-map "subject" "CN=localhost"
                   "issuer" "CN=Test CA"
                   "valid-from" "2024-01-01"
                   "valid-to" "2025-12-31"))
    (t (error "Invalid TLS connection"))))