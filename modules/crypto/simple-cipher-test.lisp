;;;; Simple Cipher Test to Isolate AEAD Hanging

(defpackage :simple-cipher-test
  (:use :cl)
  (:local-nicknames 
   (#:ffi #:epsilon.crypto.ffi))
  (:export #:run-test))

(in-package :simple-cipher-test)

(defun test-cipher-functions ()
  "Test each OpenSSL cipher function step by step"
  (format t "=== Testing OpenSSL Cipher Functions ===~%")
  
  ;; Test 1: Get cipher by name
  (format t "1. Testing EVP_get_cipherbyname...~%")
  (let ((cipher (ffi:%evp-get-cipherbyname "aes-128-gcm")))
    (if (sb-sys:sap= cipher (sb-sys:int-sap 0))
        (format t "   FAIL: AES-128-GCM not available~%")
        (format t "   OK: AES-128-GCM cipher available~%")))
  
  ;; Test 2: Create cipher context
  (format t "2. Testing EVP_CIPHER_CTX_new...~%")
  (let ((ctx (ffi:%evp-cipher-ctx-new)))
    (if (sb-sys:sap= ctx (sb-sys:int-sap 0))
        (format t "   FAIL: Could not create cipher context~%")
        (progn
          (format t "   OK: Cipher context created~%")
          
          ;; Test 3: Try to initialize encryption
          (format t "3. Testing EVP_EncryptInit_ex (this might hang)...~%")
          (force-output)
          
          (let* ((cipher (ffi:%evp-get-cipherbyname "aes-128-gcm"))
                 (key (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42))
                 (iv (make-array 12 :element-type '(unsigned-byte 8) :initial-element 1)))
            
            (sb-sys:with-pinned-objects (key iv)
              (handler-case
                  (let ((result (ffi:%evp-encryptinit-ex ctx cipher (sb-sys:int-sap 0)
                                                        (sb-sys:vector-sap key)
                                                        (sb-sys:vector-sap iv))))
                    (if (zerop result)
                        (format t "   FAIL: EVP_EncryptInit_ex returned 0~%")
                        (format t "   OK: EVP_EncryptInit_ex succeeded~%")))
                (error (e)
                  (format t "   ERROR: Exception during EVP_EncryptInit_ex: ~A~%" e)))))
          
          ;; Cleanup
          (format t "4. Testing cleanup...~%")
          (ffi:%evp-cipher-ctx-free ctx)
          (format t "   OK: Cipher context freed~%"))))

(defun run-test ()
  "Run cipher function tests"
  (test-cipher-functions)
  (format t "=== Test Complete ===~%"))