;;;; Test high-level AEAD functions after FFI fixes

(defpackage :test-aead-high-level
  (:use :cl)
  (:local-nicknames
   (#:aead #:epsilon.crypto.aead)
   (#:crypto #:epsilon.crypto))
  (:export #:run-test))

(in-package :test-aead-high-level)

(defun test-aes-gcm ()
  "Test AES-GCM encryption and decryption"
  (format t "Testing AES-GCM high-level functions...~%")
  
  (let* ((key (crypto:crypto-random-bytes 32))
         (plaintext "Hello, World! This is a test message."))
    
    (format t "Key: ~{~2,'0X~^ ~}~%" (coerce key 'list))
    (format t "Plaintext: ~A~%" plaintext)
    
    ;; Test encryption
    (format t "Attempting encryption...~%")
    (force-output)
    
    (handler-case
        (let ((result (aead:aes-gcm-encrypt plaintext key)))
          (format t "Encryption succeeded!~%")
          (format t "Ciphertext length: ~D~%" (length (getf result :ciphertext)))
          (format t "Tag: ~{~2,'0X~^ ~}~%" (coerce (getf result :tag) 'list))
          (format t "IV: ~{~2,'0X~^ ~}~%" (coerce (getf result :iv) 'list))
          
          ;; Test decryption
          (format t "~%Attempting decryption...~%")
          (force-output)
          
          (let ((decrypted (aead:aes-gcm-decrypt 
                           (getf result :ciphertext)
                           key
                           (getf result :tag)
                           (getf result :iv))))
            (format t "Decryption succeeded!~%")
            (let ((decrypted-string (sb-ext:octets-to-string decrypted :external-format :utf-8)))
              (format t "Decrypted: ~A~%" decrypted-string)
              (if (string= decrypted-string plaintext)
                  (format t "SUCCESS: Decrypted text matches original!~%")
                  (format t "FAIL: Decrypted text doesn't match!~%")))))
      (error (e)
        (format t "ERROR: ~A~%" e)))))

(defun test-chacha20-poly1305 ()
  "Test ChaCha20-Poly1305 encryption and decryption"
  (format t "~%Testing ChaCha20-Poly1305 high-level functions...~%")
  
  (let* ((key (crypto:crypto-random-bytes 32))
         (plaintext "Another test with ChaCha20-Poly1305!"))
    
    (format t "Plaintext: ~A~%" plaintext)
    
    ;; Test encryption
    (format t "Attempting encryption...~%")
    (force-output)
    
    (handler-case
        (let ((result (aead:chacha20-poly1305-encrypt plaintext key)))
          (format t "Encryption succeeded!~%")
          
          ;; Test decryption
          (format t "Attempting decryption...~%")
          (let ((decrypted (aead:chacha20-poly1305-decrypt
                           (getf result :ciphertext)
                           key
                           (getf result :tag)
                           (getf result :nonce))))
            (format t "Decryption succeeded!~%")
            (let ((decrypted-string (sb-ext:octets-to-string decrypted :external-format :utf-8)))
              (format t "Decrypted: ~A~%" decrypted-string)
              (if (string= decrypted-string plaintext)
                  (format t "SUCCESS: Decrypted text matches original!~%")
                  (format t "FAIL: Decrypted text doesn't match!~%")))))
      (error (e)
        (format t "ERROR: ~A~%" e)))))

(defun run-test ()
  "Run all AEAD tests"
  (format t "=== Testing High-Level AEAD Functions ===~%~%")
  (test-aes-gcm)
  (test-chacha20-poly1305)
  (format t "~%=== Test Complete ===~%"))