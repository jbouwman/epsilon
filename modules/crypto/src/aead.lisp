;;;; Authenticated Encryption with Associated Data (AEAD)
;;;;
;;;; This file implements modern AEAD ciphers: AES-GCM and ChaCha20-Poly1305

(defpackage :epsilon.crypto.aead
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi)
   (#:utils #:epsilon.crypto.utils))
  (:import-from :epsilon.crypto.ffi
                #:crypto-error)
  (:export #:aes-gcm-encrypt #:aes-gcm-decrypt 
           #:chacha20-poly1305-encrypt #:chacha20-poly1305-decrypt))

(in-package :epsilon.crypto.aead)

;;;; Constants

(defconstant +aes-128-gcm-key-size+ 16)
(defconstant +aes-256-gcm-key-size+ 32)
(defconstant +aes-gcm-iv-size+ 12)
(defconstant +aes-gcm-tag-size+ 16)

(defconstant +chacha20-poly1305-key-size+ 32)
(defconstant +chacha20-poly1305-nonce-size+ 12)
(defconstant +chacha20-poly1305-tag-size+ 16)

;;;; AES-GCM Implementation

(defun aes-gcm-encrypt (plaintext key &key
                                  (iv nil)
                                  (aad nil))
  "Encrypt data using AES-GCM authenticated encryption.
   
   AES-GCM provides both confidentiality and authenticity, protecting against
   both eavesdropping and tampering. It's widely used in TLS, IPsec, and
   other security protocols.
   
   Parameters:
     plaintext (string or bytes): Data to encrypt
     key (bytes): Encryption key (16 bytes for AES-128, 32 for AES-256)
     iv (bytes): Initialization vector (12 bytes, random if not provided)
     aad (bytes): Additional authenticated data (not encrypted, only authenticated)
   
   Returns:
     Property list with:
       :ciphertext - Encrypted data (bytes)
       :tag - Authentication tag (16 bytes)
       :iv - Initialization vector used (12 bytes)
   
   Security Guidelines:
     - NEVER reuse an IV with the same key
     - Generate random IVs for each encryption (default behavior)
     - Store/transmit IV with ciphertext (it's not secret)
     - Verify tag during decryption to detect tampering
   
   Performance:
     - Hardware accelerated on modern CPUs (AES-NI)
     - Typically 1-3 GB/s on modern hardware
     - Faster than separate encryption + MAC
   
   Example - Basic encryption:
     (let* ((key (crypto:crypto-random-bytes 32))
            (result (aes-gcm-encrypt \"Secret message\" key)))
       ;; result contains :ciphertext, :tag, and :iv
       (list (getf result :ciphertext)
             (getf result :tag)
             (getf result :iv)))
   
   Example - With additional authenticated data:
     (aes-gcm-encrypt message key
                     :aad (sb-ext:string-to-octets \"metadata\"))
   
   Errors:
     Signals CRYPTO-ERROR if encryption fails"
  (declare (type (or string (vector (unsigned-byte 8))) plaintext)
           (type (vector (unsigned-byte 8)) key)
           (type (or null (vector (unsigned-byte 8))) iv aad))
  
  ;; Validate key size
  (unless (or (= (length key) +aes-128-gcm-key-size+)
              (= (length key) +aes-256-gcm-key-size+))
    (error 'crypto-error :code -1
           :message "AES-GCM key must be 16 or 32 bytes"))
  
  ;; Generate IV if not provided
  (let ((iv (or iv (utils:crypto-random-bytes +aes-gcm-iv-size+))))
    
    ;; Validate IV size
    (unless (= (length iv) +aes-gcm-iv-size+)
      (error 'crypto-error :code -1
             :message "AES-GCM IV must be 12 bytes"))
    
    (let* ((plaintext-bytes (if (stringp plaintext)
                                (sb-ext:string-to-octets plaintext :external-format :utf-8)
                                plaintext))
           (cipher-name (if (= (length key) +aes-128-gcm-key-size+)
                           "aes-128-gcm"
                           "aes-256-gcm"))
           (cipher (ffi:%evp-get-cipherbyname cipher-name))
           (ctx (ffi:%evp-cipher-ctx-new)))
      
      (when (sb-sys:sap= cipher (sb-sys:int-sap 0))
        (error 'crypto-error :code -1
               :message (format nil "Cipher ~A not available" cipher-name)))
      
      (when (sb-sys:sap= ctx (sb-sys:int-sap 0))
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to create cipher context"))
      
      (unwind-protect
          (let ((ciphertext (make-array (length plaintext-bytes)
                                       :element-type '(unsigned-byte 8)))
                (tag (make-array +aes-gcm-tag-size+
                                :element-type '(unsigned-byte 8))))
            
            ;; Initialize encryption
            (sb-sys:with-pinned-objects (key iv)
              (when (zerop (ffi:%evp-encryptinit-ex ctx cipher (sb-sys:int-sap 0)
                                                    (sb-sys:vector-sap key)
                                                    (sb-sys:vector-sap iv)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to initialize AES-GCM encryption")))
            
            ;; Process AAD if provided
            (when aad
              (sb-sys:with-pinned-objects (aad)
                (sb-alien:with-alien ((outlen sb-alien:int))
                  (when (zerop (ffi:%evp-encryptupdate ctx (sb-sys:int-sap 0)
                                                       (sb-alien:alien-sap (sb-alien:addr outlen))
                                                       (sb-sys:vector-sap aad)
                                                       (length aad)))
                    (error 'crypto-error :code (ffi:%err-get-error)
                           :message "Failed to process AAD")))))
            
            ;; Encrypt plaintext
            (sb-sys:with-pinned-objects (plaintext-bytes ciphertext)
              (sb-alien:with-alien ((outlen sb-alien:int))
                (when (zerop (ffi:%evp-encryptupdate ctx
                                                     (sb-sys:vector-sap ciphertext)
                                                     (sb-alien:alien-sap (sb-alien:addr outlen))
                                                     (sb-sys:vector-sap plaintext-bytes)
                                                     (length plaintext-bytes)))
                  (error 'crypto-error :code (ffi:%err-get-error)
                         :message "Failed to encrypt data"))))
            
            ;; Finalize encryption
            (sb-alien:with-alien ((outlen sb-alien:int))
              (when (zerop (ffi:%evp-encryptfinal-ex ctx
                                                     (sb-sys:int-sap 0)
                                                     (sb-alien:alien-sap (sb-alien:addr outlen))))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to finalize encryption")))
            
            ;; Get authentication tag
            (sb-sys:with-pinned-objects (tag)
              (when (zerop (ffi:%evp-cipher-ctx-ctrl ctx
                                                     16 ; EVP_CTRL_GCM_GET_TAG
                                                     +aes-gcm-tag-size+
                                                     (sb-sys:vector-sap tag)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to get authentication tag")))
            
            ;; Return results
            (list :ciphertext ciphertext
                  :tag tag
                  :iv iv))
        
        ;; Cleanup
        (ffi:%evp-cipher-ctx-free ctx)))))

(defun aes-gcm-decrypt (ciphertext key tag iv &key (aad nil))
  "Decrypt and verify AES-GCM encrypted data.
   
   Parameters:
     ciphertext (bytes): Encrypted data
     key (bytes): Decryption key (same as used for encryption)
     tag (bytes): Authentication tag (16 bytes)
     iv (bytes): Initialization vector (12 bytes)
     aad (bytes): Additional authenticated data (must match encryption)
   
   Returns:
     Decrypted plaintext as byte vector
   
   Security Notes:
     - Verifies authenticity before returning plaintext
     - Returns error if tag verification fails (tampering detected)
     - Do not ignore authentication failures
   
   Example:
     (aes-gcm-decrypt (getf result :ciphertext)
                     key
                     (getf result :tag)
                     (getf result :iv))
   
   Errors:
     Signals CRYPTO-ERROR if decryption or authentication fails"
  (declare (type (vector (unsigned-byte 8)) ciphertext key tag iv)
           (type (or null (vector (unsigned-byte 8))) aad))
  
  ;; Validate parameters
  (unless (or (= (length key) +aes-128-gcm-key-size+)
              (= (length key) +aes-256-gcm-key-size+))
    (error 'crypto-error :code -1
           :message "Invalid key size"))
  
  (unless (= (length tag) +aes-gcm-tag-size+)
    (error 'crypto-error :code -1
           :message "Invalid tag size"))
  
  (unless (= (length iv) +aes-gcm-iv-size+)
    (error 'crypto-error :code -1
           :message "Invalid IV size"))
  
  (let* ((cipher-name (if (= (length key) +aes-128-gcm-key-size+)
                          "aes-128-gcm"
                          "aes-256-gcm"))
         (cipher (ffi:%evp-get-cipherbyname cipher-name))
         (ctx (ffi:%evp-cipher-ctx-new)))
    
    (when (sb-sys:sap= cipher (sb-sys:int-sap 0))
      (error 'crypto-error :code -1
             :message (format nil "Cipher ~A not available" cipher-name)))
    
    (when (sb-sys:sap= ctx (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create cipher context"))
    
    (unwind-protect
        (let ((plaintext (make-array (length ciphertext)
                                    :element-type '(unsigned-byte 8))))
          
          ;; Initialize decryption
          (sb-sys:with-pinned-objects (key iv)
            (when (zerop (ffi:%evp-decryptinit-ex ctx cipher (sb-sys:int-sap 0)
                                                  (sb-sys:vector-sap key)
                                                  (sb-sys:vector-sap iv)))
              (error 'crypto-error :code (ffi:%err-get-error)
                     :message "Failed to initialize AES-GCM decryption")))
          
          ;; Process AAD if provided
          (when aad
            (sb-sys:with-pinned-objects (aad)
              (sb-alien:with-alien ((outlen sb-alien:int))
                (when (zerop (ffi:%evp-decryptupdate ctx (sb-sys:int-sap 0)
                                                     (sb-alien:alien-sap (sb-alien:addr outlen))
                                                     (sb-sys:vector-sap aad)
                                                     (length aad)))
                  (error 'crypto-error :code (ffi:%err-get-error)
                         :message "Failed to process AAD")))))
          
          ;; Decrypt ciphertext
          (sb-sys:with-pinned-objects (ciphertext plaintext)
            (sb-alien:with-alien ((outlen sb-alien:int))
              (when (zerop (ffi:%evp-decryptupdate ctx
                                                   (sb-sys:vector-sap plaintext)
                                                   (sb-alien:alien-sap (sb-alien:addr outlen))
                                                   (sb-sys:vector-sap ciphertext)
                                                   (length ciphertext)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to decrypt data"))))
          
          ;; Set expected tag
          (sb-sys:with-pinned-objects (tag)
            (when (zerop (ffi:%evp-cipher-ctx-ctrl ctx
                                                   17 ; EVP_CTRL_GCM_SET_TAG
                                                   +aes-gcm-tag-size+
                                                   (sb-sys:vector-sap tag)))
              (error 'crypto-error :code (ffi:%err-get-error)
                     :message "Failed to set authentication tag")))
          
          ;; Finalize and verify
          (sb-alien:with-alien ((outlen sb-alien:int))
            (when (zerop (ffi:%evp-decryptfinal-ex ctx
                                                   (sb-sys:int-sap 0)
                                                   (sb-alien:alien-sap (sb-alien:addr outlen))))
              (error 'crypto-error :code (ffi:%err-get-error)
                     :message "Authentication failed - data may be tampered")))
          
          plaintext)
      
      ;; Cleanup
      (ffi:%evp-cipher-ctx-free ctx))))

;;;; ChaCha20-Poly1305 Implementation

(defun chacha20-poly1305-encrypt (plaintext key &key
                                            (nonce nil)
                                            (aad nil))
  "Encrypt data using ChaCha20-Poly1305 AEAD.
   
   ChaCha20-Poly1305 is a modern AEAD cipher combining the ChaCha20 stream
   cipher with Poly1305 MAC. It's faster than AES-GCM on platforms without
   AES hardware acceleration.
   
   Parameters:
     plaintext (string or bytes): Data to encrypt
     key (bytes): Encryption key (32 bytes)
     nonce (bytes): Nonce/IV (12 bytes, random if not provided)
     aad (bytes): Additional authenticated data
   
   Returns:
     Property list with:
       :ciphertext - Encrypted data (bytes)
       :tag - Authentication tag (16 bytes)
       :nonce - Nonce used (12 bytes)
   
   Security Guidelines:
     - NEVER reuse a nonce with the same key
     - Nonce doesn't need to be secret, just unique
     - Maximum 2^64 messages with same key
   
   Performance:
     - Faster than AES-GCM without hardware acceleration
     - Constant-time implementation prevents timing attacks
     - Used in TLS 1.3, WireGuard, and other modern protocols
   
   Example:
     (let* ((key (crypto:crypto-random-bytes 32))
            (result (chacha20-poly1305-encrypt \"Secret\" key)))
       (list (getf result :ciphertext)
             (getf result :tag)
             (getf result :nonce)))
   
   Errors:
     Signals CRYPTO-ERROR if encryption fails"
  (declare (type (or string (vector (unsigned-byte 8))) plaintext)
           (type (vector (unsigned-byte 8)) key)
           (type (or null (vector (unsigned-byte 8))) nonce aad))
  
  ;; Validate key size
  (unless (= (length key) +chacha20-poly1305-key-size+)
    (error 'crypto-error :code -1
           :message "ChaCha20-Poly1305 key must be 32 bytes"))
  
  ;; Generate nonce if not provided
  (let ((nonce (or nonce (utils:crypto-random-bytes +chacha20-poly1305-nonce-size+))))
    
    ;; Validate nonce size
    (unless (= (length nonce) +chacha20-poly1305-nonce-size+)
      (error 'crypto-error :code -1
             :message "ChaCha20-Poly1305 nonce must be 12 bytes"))
    
    (let* ((plaintext-bytes (if (stringp plaintext)
                                (sb-ext:string-to-octets plaintext :external-format :utf-8)
                                plaintext))
           (cipher (ffi:%evp-get-cipherbyname "chacha20-poly1305"))
           (ctx (ffi:%evp-cipher-ctx-new)))
      
      (when (sb-sys:sap= cipher (sb-sys:int-sap 0))
        (error 'crypto-error :code -1
               :message "ChaCha20-Poly1305 not available in this OpenSSL version"))
      
      (when (sb-sys:sap= ctx (sb-sys:int-sap 0))
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to create cipher context"))
      
      (unwind-protect
          (let ((ciphertext (make-array (length plaintext-bytes)
                                       :element-type '(unsigned-byte 8)))
                (tag (make-array +chacha20-poly1305-tag-size+
                                :element-type '(unsigned-byte 8))))
            
            ;; Initialize encryption
            (sb-sys:with-pinned-objects (key nonce)
              (when (zerop (ffi:%evp-encryptinit-ex ctx cipher (sb-sys:int-sap 0)
                                                    (sb-sys:vector-sap key)
                                                    (sb-sys:vector-sap nonce)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to initialize ChaCha20-Poly1305")))
            
            ;; Process AAD if provided
            (when aad
              (sb-sys:with-pinned-objects (aad)
                (sb-alien:with-alien ((outlen sb-alien:int))
                  (when (zerop (ffi:%evp-encryptupdate ctx (sb-sys:int-sap 0)
                                                       (sb-alien:alien-sap (sb-alien:addr outlen))
                                                       (sb-sys:vector-sap aad)
                                                       (length aad)))
                    (error 'crypto-error :code (ffi:%err-get-error)
                           :message "Failed to process AAD")))))
            
            ;; Encrypt plaintext
            (sb-sys:with-pinned-objects (plaintext-bytes ciphertext)
              (sb-alien:with-alien ((outlen sb-alien:int))
                (when (zerop (ffi:%evp-encryptupdate ctx
                                                     (sb-sys:vector-sap ciphertext)
                                                     (sb-alien:alien-sap (sb-alien:addr outlen))
                                                     (sb-sys:vector-sap plaintext-bytes)
                                                     (length plaintext-bytes)))
                  (error 'crypto-error :code (ffi:%err-get-error)
                         :message "Failed to encrypt data"))))
            
            ;; Finalize encryption
            (sb-alien:with-alien ((outlen sb-alien:int))
              (when (zerop (ffi:%evp-encryptfinal-ex ctx
                                                     (sb-sys:int-sap 0)
                                                     (sb-alien:alien-sap (sb-alien:addr outlen))))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to finalize encryption")))
            
            ;; Get authentication tag
            (sb-sys:with-pinned-objects (tag)
              (when (zerop (ffi:%evp-cipher-ctx-ctrl ctx
                                                     16 ; EVP_CTRL_AEAD_GET_TAG
                                                     +chacha20-poly1305-tag-size+
                                                     (sb-sys:vector-sap tag)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to get authentication tag")))
            
            ;; Return results
            (list :ciphertext ciphertext
                  :tag tag
                  :nonce nonce))
        
        ;; Cleanup
        (ffi:%evp-cipher-ctx-free ctx)))))

(defun chacha20-poly1305-decrypt (ciphertext key tag nonce &key (aad nil))
  "Decrypt and verify ChaCha20-Poly1305 encrypted data.
   
   Parameters:
     ciphertext (bytes): Encrypted data
     key (bytes): Decryption key (32 bytes)
     tag (bytes): Authentication tag (16 bytes)
     nonce (bytes): Nonce used for encryption (12 bytes)
     aad (bytes): Additional authenticated data
   
   Returns:
     Decrypted plaintext as byte vector
   
   Example:
     (chacha20-poly1305-decrypt 
       (getf result :ciphertext)
       key
       (getf result :tag)
       (getf result :nonce))
   
   Errors:
     Signals CRYPTO-ERROR if decryption or authentication fails"
  (declare (type (vector (unsigned-byte 8)) ciphertext key tag nonce)
           (type (or null (vector (unsigned-byte 8))) aad))
  
  ;; Validate parameters
  (unless (= (length key) +chacha20-poly1305-key-size+)
    (error 'crypto-error :code -1
           :message "Invalid key size"))
  
  (unless (= (length tag) +chacha20-poly1305-tag-size+)
    (error 'crypto-error :code -1
           :message "Invalid tag size"))
  
  (unless (= (length nonce) +chacha20-poly1305-nonce-size+)
    (error 'crypto-error :code -1
           :message "Invalid nonce size"))
  
  (let* ((cipher (ffi:%evp-get-cipherbyname "chacha20-poly1305"))
         (ctx (ffi:%evp-cipher-ctx-new)))
    
    (when (sb-sys:sap= cipher (sb-sys:int-sap 0))
      (error 'crypto-error :code -1
             :message "ChaCha20-Poly1305 not available"))
    
    (when (sb-sys:sap= ctx (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create cipher context"))
    
    (unwind-protect
        (let ((plaintext (make-array (length ciphertext)
                                    :element-type '(unsigned-byte 8))))
          
          ;; Initialize decryption
          (sb-sys:with-pinned-objects (key nonce)
            (when (zerop (ffi:%evp-decryptinit-ex ctx cipher (sb-sys:int-sap 0)
                                                  (sb-sys:vector-sap key)
                                                  (sb-sys:vector-sap nonce)))
              (error 'crypto-error :code (ffi:%err-get-error)
                     :message "Failed to initialize decryption")))
          
          ;; Process AAD if provided
          (when aad
            (sb-sys:with-pinned-objects (aad)
              (sb-alien:with-alien ((outlen sb-alien:int))
                (when (zerop (ffi:%evp-decryptupdate ctx (sb-sys:int-sap 0)
                                                     (sb-alien:alien-sap (sb-alien:addr outlen))
                                                     (sb-sys:vector-sap aad)
                                                     (length aad)))
                  (error 'crypto-error :code (ffi:%err-get-error)
                         :message "Failed to process AAD")))))
          
          ;; Decrypt ciphertext
          (sb-sys:with-pinned-objects (ciphertext plaintext)
            (sb-alien:with-alien ((outlen sb-alien:int))
              (when (zerop (ffi:%evp-decryptupdate ctx
                                                   (sb-sys:vector-sap plaintext)
                                                   (sb-alien:alien-sap (sb-alien:addr outlen))
                                                   (sb-sys:vector-sap ciphertext)
                                                   (length ciphertext)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to decrypt data"))))
          
          ;; Set expected tag
          (sb-sys:with-pinned-objects (tag)
            (when (zerop (ffi:%evp-cipher-ctx-ctrl ctx
                                                   17 ; EVP_CTRL_AEAD_SET_TAG
                                                   +chacha20-poly1305-tag-size+
                                                   (sb-sys:vector-sap tag)))
              (error 'crypto-error :code (ffi:%err-get-error)
                     :message "Failed to set authentication tag")))
          
          ;; Finalize and verify
          (sb-alien:with-alien ((outlen sb-alien:int))
            (when (zerop (ffi:%evp-decryptfinal-ex ctx
                                                   (sb-sys:int-sap 0)
                                                   (sb-alien:alien-sap (sb-alien:addr outlen))))
              (error 'crypto-error :code (ffi:%err-get-error)
                     :message "Authentication failed - data may be tampered")))
          
          plaintext)
      
      ;; Cleanup
      (ffi:%evp-cipher-ctx-free ctx))))

