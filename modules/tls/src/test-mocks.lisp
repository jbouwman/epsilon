;;;; TLS Test Mocking Infrastructure
;;;;
;;;; Enhanced test mocking for TLS functionality

(in-package :epsilon.tls)

;;; Mock Configuration

(defvar *mock-mode* nil
  "Enable mock mode for testing")

(defvar *mock-settings* (make-hash-table :test 'eq)
  "Mock settings for different test scenarios")

(defun enable-mock-mode (&key 
                          (simulate-handshake-failure nil)
                          (simulate-cert-error nil)
                          (simulate-network-error nil)
                          (custom-cert-data nil)
                          (custom-cipher "AES256-GCM-SHA384")
                          (custom-version "TLS 1.3"))
  "Enable mock mode with specific test settings"
  (setf *mock-mode* t)
  (clrhash *mock-settings*)
  (setf (gethash :simulate-handshake-failure *mock-settings*) simulate-handshake-failure
        (gethash :simulate-cert-error *mock-settings*) simulate-cert-error
        (gethash :simulate-network-error *mock-settings*) simulate-network-error
        (gethash :custom-cert-data *mock-settings*) custom-cert-data
        (gethash :custom-cipher *mock-settings*) custom-cipher
        (gethash :custom-version *mock-settings*) custom-version))

(defun disable-mock-mode ()
  "Disable mock mode and return to normal operation"
  (setf *mock-mode* nil)
  (clrhash *mock-settings*))

(defmacro with-mock-tls ((&key simulate-handshake-failure
                               simulate-cert-error
                               simulate-network-error
                               custom-cert-data
                               custom-cipher
                               custom-version) &body body)
  "Execute body with TLS mocking enabled"
  `(let ((*mock-mode* t)
         (*mock-settings* (make-hash-table :test 'eq)))
     (setf (gethash :simulate-handshake-failure *mock-settings*) ,simulate-handshake-failure
           (gethash :simulate-cert-error *mock-settings*) ,simulate-cert-error
           (gethash :simulate-network-error *mock-settings*) ,simulate-network-error
           (gethash :custom-cert-data *mock-settings*) ,custom-cert-data
           (gethash :custom-cipher *mock-settings*) (or ,custom-cipher "AES256-GCM-SHA384")
           (gethash :custom-version *mock-settings*) (or ,custom-version "TLS 1.3"))
     (unwind-protect
          (progn ,@body)
       (setf *mock-mode* nil))))

;;; Enhanced Mock Connection

(defstruct mock-tls-connection
  socket
  context
  connected-p
  handshake-complete-p
  bytes-read
  bytes-written
  read-buffer
  write-buffer
  error-state)

(defun create-mock-connection (socket context)
  "Create a mock TLS connection with enhanced capabilities"
  (let ((conn (make-mock-tls-connection
               :socket socket
               :context context
               :connected-p t
               :handshake-complete-p (not (gethash :simulate-handshake-failure *mock-settings*))
               :bytes-read 0
               :bytes-written 0
               :read-buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)
               :write-buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)
               :error-state nil)))
    
    ;; Simulate network errors
    (when (gethash :simulate-network-error *mock-settings*)
      (setf (mock-tls-connection-error-state conn) :network-error
            (mock-tls-connection-connected-p conn) nil))
    
    ;; Simulate certificate errors
    (when (gethash :simulate-cert-error *mock-settings*)
      (setf (mock-tls-connection-error-state conn) :cert-error))
    
    conn))

;;; Mock I/O Operations

(defun mock-tls-read (connection buffer &optional (start 0) end)
  "Mock TLS read operation with configurable behavior"
  (when (gethash :simulate-network-error *mock-settings*)
    (error "Mock network error: Connection lost"))
  
  (let* ((conn (mock-tls-connection-read-buffer connection))
         (actual-end (or end (length buffer)))
         (available (length conn))
         (to-read (min available (- actual-end start))))
    
    ;; Copy data from mock buffer to target buffer
    (dotimes (i to-read)
      (setf (aref buffer (+ start i)) (aref conn i)))
    
    ;; Remove read data from mock buffer
    (when (> to-read 0)
      (setf (mock-tls-connection-read-buffer connection)
            (make-array (- available to-read)
                        :element-type '(unsigned-byte 8)
                        :initial-contents (subseq conn to-read))))
    
    (incf (mock-tls-connection-bytes-read connection) to-read)
    to-read))

(defun mock-tls-write (connection data &optional (start 0) end)
  "Mock TLS write operation with configurable behavior"
  (when (gethash :simulate-network-error *mock-settings*)
    (error "Mock network error: Connection lost"))
  
  (let* ((actual-end (or end (length data)))
         (to-write (- actual-end start))
         (write-buffer (mock-tls-connection-write-buffer connection)))
    
    ;; Append data to write buffer
    (dotimes (i to-write)
      (vector-push-extend (aref data (+ start i)) write-buffer))
    
    (incf (mock-tls-connection-bytes-written connection) to-write)
    to-write))

;;; Mock Data Injection

(defun inject-mock-data (connection data)
  "Inject data into mock connection's read buffer for testing"
  (let ((read-buffer (mock-tls-connection-read-buffer connection)))
    (if (stringp data)
        ;; Convert string to bytes
        (map nil (lambda (char)
                   (vector-push-extend (char-code char) read-buffer))
             data)
        ;; Assume data is already byte array
        (map nil (lambda (byte)
                   (vector-push-extend byte read-buffer))
             data))))

(defun get-mock-written-data (connection &key as-string)
  "Get data written to mock connection"
  (let ((data (mock-tls-connection-write-buffer connection)))
    (if as-string
        (map 'string #'code-char data)
        (copy-seq data))))

(defun clear-mock-buffers (connection)
  "Clear both read and write buffers of mock connection"
  (setf (fill-pointer (mock-tls-connection-read-buffer connection)) 0
        (fill-pointer (mock-tls-connection-write-buffer connection)) 0))

;;; Mock Certificate Data

(defun get-mock-certificate-data ()
  "Get mock certificate data for testing"
  (or (gethash :custom-cert-data *mock-settings*)
      (map:make-map 
       "subject" "CN=mock.example.com,O=Test Organization,C=US"
       "issuer" "CN=Mock Test CA,O=Test CA Organization,C=US"
       "serial" "123456789"
       "not-before" "2024-01-01T00:00:00Z"
       "not-after" "2025-12-31T23:59:59Z"
       "fingerprint" "aa:bb:cc:dd:ee:ff:00:11:22:33:44:55:66:77:88:99:aa:bb:cc:dd"
       "public-key-algorithm" "RSA"
       "signature-algorithm" "SHA256WithRSA"
       "key-size" "2048"
       "extensions" (map:make-map 
                     "subjectAltName" "DNS:mock.example.com,DNS:*.mock.example.com"
                     "keyUsage" "Digital Signature, Key Encipherment"
                     "extendedKeyUsage" "Server Auth, Client Auth"))))

;;; Test Utilities

(defun simulate-tls-handshake (connection)
  "Simulate a TLS handshake process for testing"
  (cond
    ((gethash :simulate-handshake-failure *mock-settings*)
     (setf (mock-tls-connection-handshake-complete-p connection) nil
           (mock-tls-connection-error-state connection) :handshake-failure)
     :failed)
    ((gethash :simulate-cert-error *mock-settings*)
     (setf (mock-tls-connection-handshake-complete-p connection) nil
           (mock-tls-connection-error-state connection) :cert-verification-failed)
     :cert-error)
    (t
     (setf (mock-tls-connection-handshake-complete-p connection) t)
     :complete)))

(defun mock-performance-metrics (connection)
  "Get mock performance metrics for testing"
  (map:make-map
   "bytes-read" (mock-tls-connection-bytes-read connection)
   "bytes-written" (mock-tls-connection-bytes-written connection)
   "handshake-time-ms" 42
   "cipher-negotiation-time-ms" 15
   "certificate-verification-time-ms" 28))