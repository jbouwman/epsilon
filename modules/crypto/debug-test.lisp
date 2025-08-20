;;;; Debug Test for Crypto Functions
;;;;
;;;; Simple tests with extensive logging to identify timeout issues

(defpackage :epsilon.crypto.debug-test
  (:use :cl)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)))

(in-package :epsilon.crypto.debug-test)

(defun test-basic-functions ()
  "Test basic crypto functions with logging"
  (format t "=== CRYPTO DEBUG TESTS ===~%")
  
  ;; Test 1: Random bytes generation
  (format t "~%--- Testing crypto-random-bytes ---~%")
  (handler-case
      (let ((bytes (crypto:crypto-random-bytes 16)))
        (format t "SUCCESS: Generated ~D random bytes~%" (length bytes)))
    (error (e)
      (format t "ERROR in crypto-random-bytes: ~A~%" e)))
  
  ;; Test 2: PBKDF2
  (format t "~%--- Testing PBKDF2 ---~%")
  (handler-case
      (let ((key (crypto:pbkdf2 "test" "salt" :iterations 1000 :key-length 16)))
        (format t "SUCCESS: PBKDF2 generated ~D byte key~%" (length key)))
    (error (e)
      (format t "ERROR in PBKDF2: ~A~%" e)))
  
  ;; Test 3: BLAKE2b
  (format t "~%--- Testing BLAKE2b ---~%")
  (handler-case
      (let ((hash (crypto:blake2b "test data" :output-length 16)))
        (format t "SUCCESS: BLAKE2b generated ~D byte hash~%" (length hash)))
    (error (e)
      (format t "ERROR in BLAKE2b: ~A~%" e)))
  
  ;; Test 4: AES-GCM (the problematic one)
  (format t "~%--- Testing AES-GCM ---~%")
  (handler-case
      (let* ((key (crypto:crypto-random-bytes 32))
             (result (crypto:aes-gcm-encrypt "test message" key)))
        (format t "SUCCESS: AES-GCM encrypted message~%")
        (format t "  Ciphertext length: ~D~%" (length (getf result :ciphertext)))
        (format t "  Tag length: ~D~%" (length (getf result :tag)))
        (format t "  IV length: ~D~%" (length (getf result :iv))))
    (error (e)
      (format t "ERROR in AES-GCM: ~A~%" e)))
  
  (format t "~%=== DEBUG TESTS COMPLETE ===~%"))

;; Run the debug test when this file is loaded
(test-basic-functions)