;;;; Mock TLS Implementation for Testing
;;;;
;;;; Provides mock TLS functionality for testing without actual network operations

(in-package :epsilon.crypto)

;;;; Mock Mode State

(defparameter *mock-mode-enabled* nil
  "Whether mock TLS mode is enabled")

;;;; Mock Connection Structure

(defstruct mock-tls-connection
  "Mock TLS connection for testing"
  socket
  context
  handshake-complete-p
  peer-certificate
  selected-alpn-protocol
  read-buffer
  write-buffer
  closed-p)

;;;; Mock Mode Control

(defun enable-mock-mode ()
  "Enable mock TLS mode for testing"
  (setf *mock-mode-enabled* t))

(defun disable-mock-mode ()
  "Disable mock TLS mode"
  (setf *mock-mode-enabled* nil))

(defun mock-mode-p ()
  "Check if mock mode is enabled"
  *mock-mode-enabled*)

;;;; Mock Connection Operations

(defun create-mock-connection (socket context)
  "Create a mock TLS connection"
  (make-mock-tls-connection
   :socket socket
   :context context
   :handshake-complete-p nil
   :peer-certificate nil
   :selected-alpn-protocol nil
   :read-buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
   :write-buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
   :closed-p nil))

(defun simulate-tls-handshake (connection &key client-cert selected-protocol)
  "Simulate TLS handshake completion"
  (when (mock-tls-connection-p connection)
    (setf (mock-tls-connection-handshake-complete-p connection) t)
    (when client-cert
      (setf (mock-tls-connection-peer-certificate connection) client-cert))
    (when selected-protocol
      (setf (mock-tls-connection-selected-alpn-protocol connection) selected-protocol))
    t))

(defun mock-tls-write (connection data)
  "Write data to mock TLS connection"
  (when (and (mock-tls-connection-p connection)
             (not (mock-tls-connection-closed-p connection)))
    (let ((buffer (mock-tls-connection-write-buffer connection)))
      (loop for byte across data
            do (vector-push-extend byte buffer))
      (length data))))

(defun mock-tls-read (connection &optional (max-bytes 4096))
  "Read data from mock TLS connection"
  (when (and (mock-tls-connection-p connection)
             (not (mock-tls-connection-closed-p connection)))
    (let* ((buffer (mock-tls-connection-read-buffer connection))
           (available (length buffer))
           (to-read (min available max-bytes)))
      (when (> to-read 0)
        (let ((result (subseq buffer 0 to-read)))
          ;; Remove read bytes from buffer
          (setf (mock-tls-connection-read-buffer connection)
                (subseq buffer to-read))
          result)))))

(defun mock-tls-close (connection)
  "Close mock TLS connection"
  (when (mock-tls-connection-p connection)
    (setf (mock-tls-connection-closed-p connection) t)))

(defun mock-add-read-data (connection data)
  "Add data to the read buffer for testing"
  (when (mock-tls-connection-p connection)
    (let ((buffer (mock-tls-connection-read-buffer connection)))
      (loop for byte across data
            do (vector-push-extend byte buffer)))))

(defun mock-get-write-data (connection)
  "Get data written to the connection for testing"
  (when (mock-tls-connection-p connection)
    (mock-tls-connection-write-buffer connection)))

;;;; Export Mock Functions

(export '(enable-mock-mode
          disable-mock-mode
          mock-mode-p
          mock-tls-connection
          mock-tls-connection-p
          mock-tls-connection-handshake-complete-p
          mock-tls-connection-peer-certificate
          mock-tls-connection-selected-alpn-protocol
          create-mock-connection
          simulate-tls-handshake
          mock-tls-write
          mock-tls-read
          mock-tls-close
          mock-add-read-data
          mock-get-write-data))