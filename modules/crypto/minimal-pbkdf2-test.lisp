;;;; Minimal PBKDF2 Test to Isolate Hanging Issue

(defpackage :minimal-pbkdf2-test
  (:use :cl)
  (:local-nicknames 
   (#:ffi #:epsilon.crypto.ffi))
  (:export #:run-minimal-test))

(in-package :minimal-pbkdf2-test)

(defun test-digest-availability ()
  "Test what digest algorithms are available"
  (format t "Testing digest availability:~%")
  (dolist (digest-name '("SHA1" "SHA256" "SHA384" "SHA512" "MD5"))
    (let ((md (ffi:%evp-get-digestbyname digest-name)))
      (if (sb-sys:sap= md (sb-sys:int-sap 0))
          (format t "  ~A: NOT AVAILABLE~%" digest-name)
          (format t "  ~A: AVAILABLE~%" digest-name)))))

(defun test-minimal-pbkdf2 ()
  "Minimal PBKDF2 test with simple parameters and no debug output"
  (format t "Starting minimal PBKDF2 test...~%")
  
  ;; Very simple parameters
  (let* ((password-str "test")
         (salt-str "salt")
         (password-bytes (sb-ext:string-to-octets password-str :external-format :utf-8))
         (salt-bytes (sb-ext:string-to-octets salt-str :external-format :utf-8))
         (iterations 1000)
         (key-length 16)
         (output (make-array key-length :element-type '(unsigned-byte 8))))
    
    (format t "Input: password='~A', salt='~A', iterations=~D, key-length=~D~%" 
            password-str salt-str iterations key-length)
    
    ;; Get SHA256 digest
    (let ((md (ffi:%evp-get-digestbyname "SHA256")))
      (if (sb-sys:sap= md (sb-sys:int-sap 0))
          (format t "ERROR: Failed to get SHA256 digest~%")
          (progn
            (format t "Successfully got SHA256 digest~%")
            
            ;; Try the PBKDF2 call with timeout protection
            (format t "About to call PKCS5_PBKDF2_HMAC...~%")
            (force-output)
            
            (sb-sys:with-pinned-objects (password-bytes salt-bytes output)
              (let ((start-time (get-internal-real-time)))
                (handler-case
                    (let ((result (ffi:%pkcs5-pbkdf2-hmac
                                   (sb-sys:vector-sap password-bytes)
                                   (length password-bytes)
                                   (sb-sys:vector-sap salt-bytes) 
                                   (length salt-bytes)
                                   iterations
                                   md
                                   key-length
                                   (sb-sys:vector-sap output))))
                      (let ((elapsed-time (- (get-internal-real-time) start-time)))
                        (format t "PBKDF2 call completed in ~D internal time units~%" elapsed-time)
                        (if (zerop result)
                            (format t "ERROR: PBKDF2 returned 0 (failure)~%")
                            (progn
                              (format t "SUCCESS: PBKDF2 returned ~D~%" result)
                              (format t "Generated key: ~{~2,'0X~}~%" (coerce output 'list))))))
                  (error (e)
                    (format t "ERROR during PBKDF2 call: ~A~%" e))))))))))

(defun run-minimal-test ()
  "Run the minimal test suite"
  (format t "=== Minimal PBKDF2 Debug Test ===~%")
  (test-digest-availability)
  (format t "~%")
  (test-minimal-pbkdf2)
  (format t "=== Test Complete ===~%"))