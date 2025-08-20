;;;; Minimal AEAD Test to Isolate Hanging Issue

(defpackage :minimal-aead-test
  (:use :cl)
  (:local-nicknames 
   (#:ffi #:epsilon.crypto.ffi)
   (#:crypto #:epsilon.crypto))
  (:export #:run-minimal-test))

(in-package :minimal-aead-test)

(defun test-cipher-availability ()
  "Test what cipher algorithms are available"
  (format t "Testing cipher availability:~%")
  (dolist (cipher-name '("aes-128-gcm" "aes-256-gcm" "chacha20-poly1305"))
    (let ((cipher (ffi:%evp-get-cipherbyname cipher-name)))
      (if (sb-sys:sap= cipher (sb-sys:int-sap 0))
          (format t "  ~A: NOT AVAILABLE~%" cipher-name)
          (format t "  ~A: AVAILABLE~%" cipher-name)))))

(defun test-minimal-aes-gcm ()
  "Minimal AES-GCM test with simple parameters"
  (format t "~%Starting minimal AES-GCM test...~%")
  
  (let* ((plaintext-str "test")
         (plaintext-bytes (sb-ext:string-to-octets plaintext-str :external-format :utf-8))
         (key (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42))
         (iv (make-array 12 :element-type '(unsigned-byte 8) :initial-element 1))
         (ciphertext (make-array (length plaintext-bytes) :element-type '(unsigned-byte 8)))
         (tag (make-array 16 :element-type '(unsigned-byte 8))))
    
    (format t "Input: plaintext='~A', key-length=~D, iv-length=~D~%" 
            plaintext-str (length key) (length iv))
    
    ;; Get AES-128-GCM cipher
    (let ((cipher (ffi:%evp-get-cipherbyname "aes-128-gcm"))
          (ctx (ffi:%evp-cipher-ctx-new)))
      
      (if (sb-sys:sap= cipher (sb-sys:int-sap 0))
          (format t "ERROR: Failed to get aes-128-gcm cipher~%")
          (progn
            (format t "Successfully got AES-128-GCM cipher~%")
            
            (if (sb-sys:sap= ctx (sb-sys:int-sap 0))
                (format t "ERROR: Failed to create cipher context~%")
                (progn
                  (format t "Successfully created cipher context~%")
                  
                  ;; Try AES-GCM encryption step by step
                  (format t "About to initialize encryption...~%")
                  (force-output)
                  
                  (unwind-protect
                      (handler-case
                          (progn
                            ;; Initialize encryption
                            (sb-sys:with-pinned-objects (key iv)
                              (let ((result (ffi:%evp-encryptinit-ex ctx cipher (sb-sys:int-sap 0)
                                                                    (sb-sys:vector-sap key)
                                                                    (sb-sys:vector-sap iv))))
                                (if (zerop result)
                                    (format t "ERROR: Failed to initialize encryption~%")
                                    (format t "SUCCESS: AES-GCM initialization completed~%"))))
                            
                            ;; If we get here, try a simple encrypt operation
                            (format t "About to encrypt plaintext...~%")
                            (force-output)
                            
                            (sb-sys:with-pinned-objects (plaintext-bytes ciphertext)
                              (sb-alien:with-alien ((outlen sb-alien:int))
                                (let ((result (ffi:%evp-encryptupdate ctx
                                                                      (sb-sys:vector-sap ciphertext)
                                                                      (sb-alien:addr outlen)
                                                                      (sb-sys:vector-sap plaintext-bytes)
                                                                      (length plaintext-bytes))))
                                  (if (zerop result)
                                      (format t "ERROR: Failed to encrypt data~%")
                                      (format t "SUCCESS: Data encryption completed~%"))))))
                        
                        (error (e)
                          (format t "ERROR during AES-GCM operation: ~A~%" e)))
                    
                    ;; Cleanup
                    (ffi:%evp-cipher-ctx-free ctx))))))))

(defun test-simple-aes-gcm-call ()
  "Test the high-level AES-GCM function"
  (format t "~%Testing high-level AES-GCM function...~%")
  (handler-case
      (let* ((key (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42))
             (result (crypto:aes-gcm-encrypt "test" key)))
        (format t "SUCCESS: High-level AES-GCM call completed~%")
        (format t "Result keys: ~A~%" (loop for (k v) on result by #'cddr collect k)))
    (error (e)
      (format t "ERROR in high-level AES-GCM: ~A~%" e))))

(defun run-minimal-test ()
  "Run the minimal AEAD test suite"
  (format t "=== Minimal AEAD Debug Test ===~%")
  (test-cipher-availability)
  (test-minimal-aes-gcm)
  (test-simple-aes-gcm-call)
  (format t "=== Test Complete ===~%"))