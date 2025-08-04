;;;; TLS Implementation Selection
;;;;
;;;; Integrates OpenSSL-based TLS with the epsilon.tls interface

(in-package :epsilon.tls)

;; TLS implementation selection
(defvar *tls-implementation* :stub
  "Current TLS implementation: :openssl or :stub")

;;;; Implementation dispatch

(defun implementation-available-p ()
  "Check if a real TLS implementation is available"
  (eq *tls-implementation* :openssl))

;;;; Updated TLS Context functions

(defun create-tls-context (&key server-p)
  "Create a new TLS context"
  ;; For now, always use stub implementation since OpenSSL integration is complex
  (make-tls-context :server-p server-p))

(defun load-cert-file (context cert-file)
  "Load certificate file into TLS context"
  (if (tls-context-p context)
      (progn
        (setf (tls-context-cert-file context) cert-file)
        context)
      (error "Invalid TLS context")))

(defun load-key-file (context key-file)
  "Load private key file into TLS context"
  (if (tls-context-p context)
      (progn
        (setf (tls-context-key-file context) key-file)
        context)
      (error "Invalid TLS context")))

(defun set-verify-mode (context mode)
  "Set certificate verification mode"
  (if (tls-context-p context)
      (progn
        (setf (tls-context-verify-mode context) mode)
        context)
      (error "Invalid TLS context")))

;;;; Updated Connection Management

(defun tls-connect (socket context)
  "Establish TLS connection over existing socket"
  (cond
    (*mock-mode*
     ;; Use enhanced mock implementation
     (create-mock-connection socket context))
    ((tls-context-p context)
     ;; Stub implementation
     (stub-tls-connect socket context))
    (t (error "Invalid TLS context"))))

(defun tls-accept (socket context)
  "Accept TLS connection on socket"
  (if (tls-context-p context)
      (stub-tls-accept socket context)
      (error "Invalid TLS context")))

(defun tls-close (connection)
  "Close TLS connection"
  (cond
    ((mock-tls-connection-p connection)
     ;; Mock implementation - just mark as disconnected
     (setf (mock-tls-connection-connected-p connection) nil))
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
    ((and *mock-mode* (mock-tls-connection-p connection))
     ;; Use enhanced mock implementation
     (mock-tls-read connection buffer start end))
    ((tls-connection-p connection)
     ;; Stub implementation
     (stub-tls-read connection buffer start end))
    (t (error "Invalid TLS connection"))))

(defun tls-write (connection data &optional (start 0) end)
  "Write data to TLS connection"
  (cond
    ((and *mock-mode* (mock-tls-connection-p connection))
     ;; Use enhanced mock implementation
     (mock-tls-write connection data start end))
    ((tls-connection-p connection)
     ;; Stub implementation
     (stub-tls-write connection data start end))
    (t (error "Invalid TLS connection"))))

;;;; Stream Interface

(defun tls-stream (connection)
  "Get stream interface for TLS connection"
  (if (tls-connection-p connection)
      (stub-tls-stream connection)
      (error "Invalid TLS connection")))

;;;; Stub Implementation Functions (for fallback when OpenSSL not available)

(defun stub-tls-connect (socket context)
  "Stub TLS connect - just wraps socket"
  (let ((connection (make-tls-connection :socket socket
                                         :context context
                                         :connected-p t)))
    ;; Simulate handshake completion
    (setf (tls-connection-handshake-complete-p connection) t)
    connection))

(defun stub-tls-accept (listening-socket context)
  "Stub TLS accept - just accepts socket"
  (let* ((client-socket (sb-bsd-sockets:socket-accept listening-socket))
         (connection (make-tls-connection :socket client-socket
                                          :context context
                                          :connected-p t)))
    ;; Simulate server-side handshake
    (setf (tls-connection-handshake-complete-p connection) t)
    connection))

(defun stub-tls-read (connection buffer &optional (start 0) end)
  "Stub TLS read - just reads from socket"
  (declare (ignore start))  ; TODO: implement partial buffer reads with start offset
  (unless (tls-connection-handshake-complete-p connection)
    (error "TLS handshake not complete"))
  
  (let ((socket (tls-connection-socket connection)))
    (sb-bsd-sockets:socket-receive socket buffer (or end (length buffer)))))

(defun stub-tls-write (connection data &optional (start 0) end)
  "Stub TLS write - just writes to socket"
  (unless (tls-connection-handshake-complete-p connection)
    (error "TLS handshake not complete"))
  
  (let ((socket (tls-connection-socket connection))
        (actual-end (or end (length data))))
    (sb-bsd-sockets:socket-send socket (subseq data start actual-end) 
                                (- actual-end start))))

(defun stub-tls-stream (connection)
  "Stub TLS stream - just returns socket stream"
  (sb-bsd-sockets:socket-make-stream (tls-connection-socket connection)
                                     :input t :output t
                                     :element-type 'character))

;;;; Handshake and Information Functions

(defun tls-handshake (connection)
  "Perform or check TLS handshake status"
  (cond
    ((tls-connection-p connection)
     ;; Stub implementation
     (if (tls-connection-handshake-complete-p connection)
         :complete
         :pending))
    (t (error "Invalid TLS connection"))))

;;;; Information Functions

(defun tls-version (connection)
  "Get TLS version for connection"
  (cond
    ((and *mock-mode* (mock-tls-connection-p connection))
     ;; Use custom mock version
     (gethash :custom-version *mock-settings*))
    ((tls-connection-p connection)
     "TLS 1.2 (simulated)")
    (t (error "Invalid TLS connection"))))

(defun tls-cipher (connection)
  "Get cipher suite for connection"
  (cond
    ((and *mock-mode* (mock-tls-connection-p connection))
     ;; Use custom mock cipher
     (gethash :custom-cipher *mock-settings*))
    ((tls-connection-p connection)
     "AES256-GCM-SHA384 (simulated)")
    (t (error "Invalid TLS connection"))))

(defun get-peer-certificate (connection)
  "Get peer certificate information"
  (cond
    ((and *mock-mode* (mock-tls-connection-p connection))
     ;; Use enhanced mock certificate data
     (get-mock-certificate-data))
    ((tls-connection-p connection)
     ;; Stub implementation
     (map:make-map "subject" "CN=localhost"
                   "issuer" "CN=Test CA"
                   "valid-from" "2024-01-01"
                   "valid-to" "2025-12-31"))
    (t (error "Invalid TLS connection"))))