;;;; OpenSSL 3.0 Modern Crypto Implementation
;;;; Clean implementation using only OpenSSL 3.0 APIs

(defpackage :epsilon.crypto.openssl3
  (:use :cl)
  (:local-nicknames
   (#:lib #:epsilon.foreign))
  (:export
   ;; Key generation
   #:generate-rsa-key
   #:generate-ec-key
   #:generate-ed25519-key
   ;; Digest functions
   #:digest
   ;; Random
   #:random-bytes))

(in-package :epsilon.crypto.openssl3)

;;;; Modern OpenSSL 3.0 FFI Bindings

;; Context management
(lib:defshared %evp-pkey-ctx-new-from-name "EVP_PKEY_CTX_new_from_name" "libcrypto" :pointer
  (libctx :pointer) (name :string) (propquery :pointer))

(lib:defshared %evp-pkey-ctx-free "EVP_PKEY_CTX_free" "libcrypto" :void
  (ctx :pointer))

;; Key generation
(lib:defshared %evp-pkey-keygen-init "EVP_PKEY_keygen_init" "libcrypto" :int
  (ctx :pointer))

(lib:defshared %evp-pkey-generate "EVP_PKEY_generate" "libcrypto" :int
  (ctx :pointer) (ppkey :pointer))

(lib:defshared %evp-pkey-ctx-set-rsa-keygen-bits "EVP_PKEY_CTX_set_rsa_keygen_bits" "libcrypto" :int
  (ctx :pointer) (bits :int))

;; Key management
(lib:defshared %evp-pkey-new "EVP_PKEY_new" "libcrypto" :pointer ())
(lib:defshared %evp-pkey-free "EVP_PKEY_free" "libcrypto" :void (pkey :pointer))
(lib:defshared %evp-pkey-bits "EVP_PKEY_get_bits" "libcrypto" :int (pkey :pointer))
(lib:defshared %evp-pkey-id "EVP_PKEY_get_id" "libcrypto" :int (pkey :pointer))

;; Digest functions
(lib:defshared %evp-md-ctx-new "EVP_MD_CTX_new" "libcrypto" :pointer ())
(lib:defshared %evp-md-ctx-free "EVP_MD_CTX_free" "libcrypto" :void (ctx :pointer))
(lib:defshared %evp-get-digestbyname "EVP_get_digestbyname" "libcrypto" :pointer (name :string))
(lib:defshared %evp-digestinit-ex "EVP_DigestInit_ex" "libcrypto" :int
  (ctx :pointer) (type :pointer) (impl :pointer))
(lib:defshared %evp-digestupdate "EVP_DigestUpdate" "libcrypto" :int
  (ctx :pointer) (d :pointer) (cnt :unsigned-long))
(lib:defshared %evp-digestfinal-ex "EVP_DigestFinal_ex" "libcrypto" :int
  (ctx :pointer) (md :pointer) (s :pointer))

;; Random number generation
(lib:defshared %rand-bytes "RAND_bytes" "libcrypto" :int
  (buf :pointer) (num :int))

;; Error handling
(lib:defshared %err-get-error "ERR_get_error" "libcrypto" :unsigned-long ())
(lib:defshared %err-error-string "ERR_error_string" "libcrypto" :pointer
  (e :unsigned-long) (buf :pointer))

;;;; Helper Functions

(defun get-error-string ()
  "Get the last OpenSSL error as a string"
  (let ((err (%err-get-error)))
    (if (zerop err)
        "No error"
        (let ((str-ptr (%err-error-string err (sb-sys:int-sap 0))))
          (if (zerop (sb-sys:sap-int str-ptr))
              (format nil "Error code: ~A" err)
              (sb-alien:cast str-ptr sb-alien:c-string))))))

;;;; Key Generation Functions

(defun generate-rsa-key (bits)
  "Generate an RSA key pair using OpenSSL 3.0 API"
  (unless (member bits '(2048 3072 4096))
    (error "RSA key size must be 2048, 3072, or 4096 bits"))
  
  (let* ((ctx (%evp-pkey-ctx-new-from-name 
                (sb-sys:int-sap 0) "RSA" (sb-sys:int-sap 0)))
         (pkey-ptr nil))
    (when (zerop (sb-sys:sap-int ctx))
      (error "Failed to create RSA context: ~A" (get-error-string)))
    
    (unwind-protect
         (progn
           ;; Initialize key generation
           (when (zerop (%evp-pkey-keygen-init ctx))
             (error "Failed to initialize RSA key generation: ~A" (get-error-string)))
           
           ;; Set RSA key size
           (when (zerop (%evp-pkey-ctx-set-rsa-keygen-bits ctx bits))
             (error "Failed to set RSA key size: ~A" (get-error-string)))
           
           ;; Generate the key
           (sb-alien:with-alien ((pkey-holder sb-alien:system-area-pointer))
             (setf (sb-alien:deref pkey-holder) (sb-sys:int-sap 0))
             (when (zerop (%evp-pkey-generate ctx 
                                              (sb-alien:alien-sap 
                                               (sb-alien:addr pkey-holder))))
               (error "Failed to generate RSA key: ~A" (get-error-string)))
             (setf pkey-ptr (sb-alien:deref pkey-holder)))
           
           ;; Return the key pointer
           pkey-ptr)
      ;; Cleanup
      (when ctx
        (%evp-pkey-ctx-free ctx)))))

(defun generate-ec-key (curve)
  "Generate an EC key pair using OpenSSL 3.0 API"
  (declare (ignore curve)) ;; TODO: Implement curve parameter
  ;; For now, use the same pattern as RSA but with "EC" algorithm
  (let* ((ctx (%evp-pkey-ctx-new-from-name 
                (sb-sys:int-sap 0) "EC" (sb-sys:int-sap 0)))
         (pkey-ptr nil))
    (when (zerop (sb-sys:sap-int ctx))
      (error "Failed to create EC context: ~A" (get-error-string)))
    
    (unwind-protect
         (progn
           ;; Initialize key generation
           (when (zerop (%evp-pkey-keygen-init ctx))
             (error "Failed to initialize EC key generation: ~A" (get-error-string)))
           
           ;; TODO: Set EC curve parameters
           ;; This requires OSSL_PARAM API which needs more work
           
           ;; Generate the key
           (sb-alien:with-alien ((pkey-holder sb-alien:system-area-pointer))
             (setf (sb-alien:deref pkey-holder) (sb-sys:int-sap 0))
             (when (zerop (%evp-pkey-generate ctx 
                                              (sb-alien:alien-sap 
                                               (sb-alien:addr pkey-holder))))
               (error "Failed to generate EC key: ~A" (get-error-string)))
             (setf pkey-ptr (sb-alien:deref pkey-holder)))
           
           ;; Return the key pointer
           pkey-ptr)
      ;; Cleanup
      (when ctx
        (%evp-pkey-ctx-free ctx)))))

(defun generate-ed25519-key ()
  "Generate an Ed25519 key pair using OpenSSL 3.0 API"
  (let* ((ctx (%evp-pkey-ctx-new-from-name 
                (sb-sys:int-sap 0) "ED25519" (sb-sys:int-sap 0)))
         (pkey-ptr nil))
    (when (zerop (sb-sys:sap-int ctx))
      (error "Failed to create Ed25519 context: ~A" (get-error-string)))
    
    (unwind-protect
         (progn
           ;; Initialize key generation
           (when (zerop (%evp-pkey-keygen-init ctx))
             (error "Failed to initialize Ed25519 key generation: ~A" (get-error-string)))
           
           ;; Ed25519 doesn't need parameters
           
           ;; Generate the key
           (sb-alien:with-alien ((pkey-holder sb-alien:system-area-pointer))
             (setf (sb-alien:deref pkey-holder) (sb-sys:int-sap 0))
             (when (zerop (%evp-pkey-generate ctx 
                                              (sb-alien:alien-sap 
                                               (sb-alien:addr pkey-holder))))
               (error "Failed to generate Ed25519 key: ~A" (get-error-string)))
             (setf pkey-ptr (sb-alien:deref pkey-holder)))
           
           ;; Return the key pointer
           pkey-ptr)
      ;; Cleanup
      (when ctx
        (%evp-pkey-ctx-free ctx)))))

;;;; Digest Functions

(defun digest (algorithm data)
  "Compute digest of data using specified algorithm (e.g., \"SHA256\")"
  (let ((ctx (%evp-md-ctx-new))
        (md-type (%evp-get-digestbyname algorithm)))
    
    (when (zerop (sb-sys:sap-int ctx))
      (error "Failed to create digest context"))
    
    (when (zerop (sb-sys:sap-int md-type))
      (error "Unknown digest algorithm: ~A" algorithm))
    
    (unwind-protect
         (sb-alien:with-alien ((md-buf (sb-alien:array sb-alien:unsigned-char 64))
                               (md-len sb-alien:unsigned-int))
           ;; Initialize digest
           (when (zerop (%evp-digestinit-ex ctx md-type (sb-sys:int-sap 0)))
             (error "Failed to initialize digest: ~A" (get-error-string)))
           
           ;; Update with data
           (if (stringp data)
               ;; String data
               (sb-alien:with-alien ((data-buf sb-alien:c-string))
                 (setf data-buf data)
                 (when (zerop (%evp-digestupdate ctx 
                                                 (sb-alien:alien-sap data-buf)
                                                 (length data)))
                   (error "Failed to update digest: ~A" (get-error-string))))
               ;; Byte array data
               (sb-alien:with-alien ((data-buf (sb-alien:array sb-alien:unsigned-char 
                                                               #.(expt 2 16))))
                 (loop for i from 0 below (min (length data) #.(expt 2 16))
                       do (setf (sb-alien:deref data-buf i) (aref data i)))
                 (when (zerop (%evp-digestupdate ctx 
                                                 (sb-alien:alien-sap data-buf)
                                                 (length data)))
                   (error "Failed to update digest: ~A" (get-error-string)))))
           
           ;; Finalize digest
           (when (zerop (%evp-digestfinal-ex ctx 
                                             (sb-alien:alien-sap md-buf)
                                             (sb-alien:alien-sap (sb-alien:addr md-len))))
             (error "Failed to finalize digest: ~A" (get-error-string)))
           
           ;; Convert to byte array
           (let ((result (make-array (sb-alien:deref md-len) 
                                    :element-type '(unsigned-byte 8))))
             (loop for i from 0 below (sb-alien:deref md-len)
                   do (setf (aref result i) (sb-alien:deref md-buf i)))
             result))
      ;; Cleanup
      (when ctx
        (%evp-md-ctx-free ctx)))))

;;;; Random Number Generation

(defun random-bytes (count)
  "Generate COUNT random bytes"
  (let ((result (make-array count :element-type '(unsigned-byte 8))))
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 256)))
      (loop for offset from 0 below count by 256
            for chunk-size = (min 256 (- count offset))
            do (progn
                 (when (zerop (%rand-bytes (sb-alien:alien-sap buf) chunk-size))
                   (error "Failed to generate random bytes: ~A" (get-error-string)))
                 (loop for i from 0 below chunk-size
                       do (setf (aref result (+ offset i)) 
                               (sb-alien:deref buf i))))))
    result))