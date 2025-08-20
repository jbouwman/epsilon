;;;; OpenSSL Initialization and Security Functions
;;;;
;;;; Ensures proper OpenSSL initialization and provides security utilities

(defpackage :epsilon.crypto.init
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi))
  (:import-from :epsilon.crypto.ffi
                #:crypto-error)
  (:export #:ensure-openssl-initialized
           #:openssl-version
           #:openssl-available-p
           #:secure-zero-memory
           #:constant-time-compare))

(in-package :epsilon.crypto.init)

(defvar *openssl-initialized* nil
  "Flag indicating whether OpenSSL has been initialized")

(defvar *initialization-lock* (sb-thread:make-mutex :name "openssl-init-lock")
  "Lock for thread-safe initialization")

(defun ensure-openssl-initialized ()
  "Ensure OpenSSL library is properly initialized.
   
   This function is thread-safe and idempotent. It initializes:
   - OpenSSL algorithms
   - Error strings
   - Random number generator
   
   Returns:
     T if initialization successful
   
   Security Notes:
     - Must be called before any crypto operations
     - Automatically called by high-level functions
     - Thread-safe through mutex protection"
  (sb-thread:with-mutex (*initialization-lock*)
    (unless *openssl-initialized*
      ;; Initialize OpenSSL library
      (let ((result (ffi:%openssl-init-crypto 
                     #x00000002  ; OPENSSL_INIT_LOAD_CRYPTO_STRINGS
                     (sb-sys:int-sap 0))))
        (when (zerop result)
          (error 'crypto-error 
                 :code (ffi:%err-get-error)
                 :message "Failed to initialize OpenSSL"))
        
        ;; Initialize error strings
        (ffi:%err-load-crypto-strings)
        
        ;; Seed random number generator if needed
        (unless (= 1 (ffi:%rand-status))
          ;; Try to seed with system entropy
          (let ((seed-bytes (make-array 32 :element-type '(unsigned-byte 8))))
            ;; Read from /dev/urandom or equivalent
            (with-open-file (random "/dev/urandom" 
                                   :element-type '(unsigned-byte 8)
                                   :if-does-not-exist nil)
              (when random
                (read-sequence seed-bytes random)
                (sb-sys:with-pinned-objects (seed-bytes)
                  (ffi:%rand-seed (sb-sys:vector-sap seed-bytes) 32))))))
        
        (setf *openssl-initialized* t))))
  t)

(defun openssl-version ()
  "Get OpenSSL version information.
   
   Returns:
     String containing OpenSSL version"
  (ensure-openssl-initialized)
  (let ((version-ptr (ffi:%openssl-version 0)))  ; OPENSSL_VERSION = 0
    (unless (zerop (sb-sys:sap-int version-ptr))
      (sb-alien:cast version-ptr sb-alien:c-string))))

(defun openssl-available-p ()
  "Check if OpenSSL is available and functional.
   
   Returns:
     T if OpenSSL is available and can be initialized"
  (handler-case
      (progn
        (ensure-openssl-initialized)
        t)
    (error ()
      nil)))

(defun secure-zero-memory (data)
  "Securely zero memory containing sensitive data.
   
   Uses OpenSSL's OPENSSL_cleanse to prevent compiler optimization
   from removing the memory clearing operation.
   
   Parameters:
     data (vector): Byte vector to clear
   
   Security Notes:
     - Prevents sensitive data from remaining in memory
     - Resistant to compiler optimizations
     - Should be called on all key material after use"
  (when (and data (> (length data) 0))
    (sb-sys:with-pinned-objects (data)
      (ffi:%openssl-cleanse (sb-sys:vector-sap data) (length data)))
    ;; Also clear the Lisp array
    (dotimes (i (length data))
      (setf (aref data i) 0)))
  nil)

(defun constant-time-compare (data1 data2)
  "Compare two byte sequences in constant time.
   
   Prevents timing attacks by ensuring comparison takes the same
   time regardless of where the first difference occurs.
   
   Parameters:
     data1 (vector): First byte sequence
     data2 (vector): Second byte sequence
   
   Returns:
     T if sequences are equal, NIL otherwise
   
   Security Notes:
     - Use for comparing MACs, hashes, and other sensitive data
     - Prevents timing side-channel attacks
     - Always compares full length even if early difference found"
  (let ((len1 (length data1))
        (len2 (length data2)))
    (if (/= len1 len2)
        nil  ; Different lengths, not equal
        (if (zerop len1)
            t  ; Both empty, equal
            (sb-sys:with-pinned-objects (data1 data2)
              (= 0 (ffi:%crypto-memcmp 
                    (sb-sys:vector-sap data1)
                    (sb-sys:vector-sap data2)
                    len1)))))))

;; Initialize OpenSSL when module loads
(eval-when (:load-toplevel :execute)
  (handler-case
      (ensure-openssl-initialized)
    (error (e)
      (warn "Failed to initialize OpenSSL: ~A" e))))