;;;; Key Derivation Functions (KDF)
;;;;
;;;; This file implements various key derivation functions including
;;;; PBKDF2, HKDF, and other password-based cryptographic functions

(defpackage :epsilon.crypto.kdf
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi))
  (:import-from :epsilon.crypto.ffi
                #:crypto-error)
  (:import-from :epsilon.crypto
                #:+digest-sha256+
                #:+digest-sha384+
                #:+digest-sha512+)
  (:export #:pbkdf2 #:hkdf #:scrypt #:argon2))

(in-package :epsilon.crypto.kdf)

;;;; PBKDF2 - Password-Based Key Derivation Function 2

(defun pbkdf2 (password salt &key 
                        (iterations 100000)
                        (key-length 32)
                        (digest +digest-sha256+))
  "Derive a key from a password using PBKDF2 (RFC 2898).
   
   PBKDF2 applies a pseudorandom function (PRF) repeatedly to create
   a derived key from a password and salt. This makes password cracking
   significantly more expensive computationally.
   
   Parameters:
     password (string or bytes): Password to derive key from
     salt (string or bytes): Random salt (minimum 16 bytes recommended)
     iterations (integer): Number of iterations (default: 100000)
     key-length (integer): Desired output key length in bytes (default: 32)
     digest (string): Hash algorithm to use (default: SHA-256)
   
   Returns:
     Byte vector containing the derived key
   
   Security Guidelines:
     - Use minimum 100,000 iterations (2023 standard)
     - Consider 200,000+ iterations for high-security applications
     - Salt must be unique per password (use crypto-random-bytes)
     - Salt should be at least 16 bytes (128 bits)
     - Store salt alongside the derived key
   
   Iteration Recommendations by Year:
     - 2015: 50,000 iterations
     - 2020: 100,000 iterations  
     - 2023: 100,000-200,000 iterations
     - 2025: 200,000-500,000 iterations (projected)
   
   Performance:
     - 100,000 iterations with SHA-256: ~50-200ms
     - 200,000 iterations with SHA-256: ~100-400ms
     - 500,000 iterations with SHA-256: ~250-1000ms
   
   Example - Password storage:
     (let* ((salt (crypto:crypto-random-bytes 16))
            (derived-key (pbkdf2 \"user-password\" salt 
                                :iterations 200000
                                :key-length 32)))
       ;; Store salt and derived-key in database
       )
   
   Example - Key derivation for encryption:
     (let* ((salt (crypto:crypto-random-bytes 32))
            (key (pbkdf2 \"passphrase\" salt
                        :iterations 100000
                        :key-length 32  ; for AES-256
                        :digest crypto:+digest-sha512+)))
       ;; Use key for encryption
       )
   
   Errors:
     Signals CRYPTO-ERROR if key derivation fails"
  (declare (type (or string (vector (unsigned-byte 8))) password salt)
           (type (integer 1 *) iterations)
           (type (integer 1 512) key-length)
           (type string digest))
  
  ;; Convert strings to bytes if needed
  (let ((password-bytes (if (stringp password)
                            (sb-ext:string-to-octets password :external-format :utf-8)
                            password))
        (salt-bytes (if (stringp salt)
                        (sb-ext:string-to-octets salt :external-format :utf-8)
                        salt))
        (output (make-array key-length :element-type '(unsigned-byte 8))))
    
    ;; Get the digest function
    (let ((md (ffi:%evp-get-digestbyname digest)))
      (when (sb-sys:sap= md (sb-sys:int-sap 0))
        (error 'crypto-error :code -1 
               :message (format nil "Unknown digest algorithm: ~A" digest)))
      
      ;; Call OpenSSL's PKCS5_PBKDF2_HMAC
      (sb-sys:with-pinned-objects (password-bytes salt-bytes output)
        (let ((result (ffi:%pkcs5-pbkdf2-hmac
                       (sb-sys:vector-sap password-bytes)
                       (length password-bytes)
                       (sb-sys:vector-sap salt-bytes)
                       (length salt-bytes)
                       iterations
                       md
                       key-length
                       (sb-sys:vector-sap output))))
          (when (zerop result)
            (error 'crypto-error :code (ffi:%err-get-error)
                   :message "PBKDF2 key derivation failed"))))
      
      output)))

;;;; HKDF - HMAC-based Key Derivation Function

(defun hkdf (input-key-material &key
                                (salt nil)
                                (info nil)
                                (length 32)
                                (digest +digest-sha256+))
  "Derive keys using HKDF (RFC 5869).
   
   HKDF is a key derivation function based on HMAC. It consists of two
   stages: Extract (concentrate randomness) and Expand (generate output keys).
   Useful for deriving multiple keys from a single master key.
   
   Parameters:
     input-key-material (bytes): Input keying material (IKM)
     salt (bytes): Optional salt value (defaults to zeros)
     info (bytes): Optional context/application-specific info
     length (integer): Length of output key material in bytes
     digest (string): Hash algorithm to use (default: SHA-256)
   
   Returns:
     Byte vector containing the derived key material
   
   Use Cases:
     - Deriving encryption and MAC keys from a shared secret
     - Key diversification in protocols
     - Converting non-uniform randomness to uniform keys
   
   Security Notes:
     - Salt adds randomness and prevents rainbow table attacks
     - Info parameter provides domain separation
     - Output length limited by hash output size * 255
   
   Example - Derive encryption and MAC keys:
     (let* ((shared-secret (ecdh-key-exchange alice-key bob-key))
            (salt (crypto:crypto-random-bytes 32))
            (key-material (hkdf shared-secret
                               :salt salt
                               :info \"encryption+mac\"
                               :length 64)))
       (values (subseq key-material 0 32)   ; encryption key
               (subseq key-material 32 64))) ; MAC key
   
   Errors:
     Signals CRYPTO-ERROR if derivation fails or length exceeds limits"
  (declare (type (vector (unsigned-byte 8)) input-key-material)
           (type (or null (vector (unsigned-byte 8))) salt info)
           (type (integer 1 8160) length)  ; 255 * 32 (SHA-256 output)
           (type string digest))
  
  (let ((salt-bytes (or salt (make-array 0 :element-type '(unsigned-byte 8))))
        (info-bytes (or info (make-array 0 :element-type '(unsigned-byte 8))))
        (output (make-array length :element-type '(unsigned-byte 8))))
    
    ;; Get EVP context for HKDF
    (let* ((ctx (ffi:%evp-pkey-ctx-new-id ffi:+evp-pkey-hkdf+ (sb-sys:int-sap 0))))
      (when (sb-sys:sap= ctx (sb-sys:int-sap 0))
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to create HKDF context"))
      
      (unwind-protect
          (progn
            ;; Initialize HKDF
            (when (zerop (ffi:%evp-pkey-derive-init ctx))
              (error 'crypto-error :code (ffi:%err-get-error)
                     :message "Failed to initialize HKDF"))
            
            ;; Set digest
            (let ((md (ffi:%evp-get-digestbyname digest)))
              (when (sb-sys:sap= md (sb-sys:int-sap 0))
                (error 'crypto-error :code -1
                       :message (format nil "Unknown digest: ~A" digest)))
              (when (zerop (ffi:%evp-pkey-ctx-set-hkdf-md ctx md))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to set HKDF digest")))
            
            ;; Set salt
            (sb-sys:with-pinned-objects (salt-bytes)
              (when (zerop (ffi:%evp-pkey-ctx-set1-hkdf-salt ctx
                                                              (sb-sys:vector-sap salt-bytes)
                                                              (length salt-bytes)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to set HKDF salt")))
            
            ;; Set key
            (sb-sys:with-pinned-objects (input-key-material)
              (when (zerop (ffi:%evp-pkey-ctx-set1-hkdf-key ctx
                                                            (sb-sys:vector-sap input-key-material)
                                                            (length input-key-material)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to set HKDF key")))
            
            ;; Set info
            (when info
              (sb-sys:with-pinned-objects (info-bytes)
                (when (zerop (ffi:%evp-pkey-ctx-add1-hkdf-info ctx
                                                               (sb-sys:vector-sap info-bytes)
                                                               (length info-bytes)))
                  (error 'crypto-error :code (ffi:%err-get-error)
                         :message "Failed to set HKDF info"))))
            
            ;; Derive key
            (sb-sys:with-pinned-objects (output)
              (let ((outlen length))
                (sb-alien:with-alien ((len-holder sb-alien:size-t outlen))
                  (when (zerop (ffi:%evp-pkey-derive ctx 
                                                     (sb-sys:vector-sap output)
                                                     (sb-alien:alien-sap (sb-alien:addr len-holder))))
                    (error 'crypto-error :code (ffi:%err-get-error)
                           :message "HKDF derivation failed")))))
            
            output)
        
        ;; Cleanup
        (ffi:%evp-pkey-ctx-free ctx)))))

;;;; Scrypt - Memory-hard KDF

(defun scrypt (password salt &key
                       (n 16384)      ; CPU/memory cost
                       (r 8)          ; Block size
                       (p 1)          ; Parallelization
                       (key-length 32))
  "Derive a key using scrypt memory-hard function.
   
   Scrypt is designed to be expensive in both time and memory, making
   hardware brute-force attacks costly. Recommended for password storage
   where memory-hardness is important.
   
   Parameters:
     password (string or bytes): Password to derive from
     salt (string or bytes): Random salt (minimum 16 bytes)
     n (integer): CPU/memory cost parameter (must be power of 2)
     r (integer): Block size parameter (default: 8)
     p (integer): Parallelization parameter (default: 1)
     key-length (integer): Output length in bytes (default: 32)
   
   Returns:
     Byte vector containing derived key
   
   Parameter Guidelines:
     - Minimum: N=16384, r=8, p=1 (uses ~16MB RAM)
     - Recommended: N=32768, r=8, p=1 (uses ~32MB RAM)
     - High security: N=65536, r=8, p=1 (uses ~64MB RAM)
   
   Security Notes:
     - N must be a power of 2 (e.g., 16384, 32768, 65536)
     - Higher N increases both time and memory requirements
     - Memory usage ≈ 128 * N * r bytes
   
   Performance:
     - N=16384: ~50-100ms, 16MB RAM
     - N=32768: ~100-200ms, 32MB RAM
     - N=65536: ~200-400ms, 64MB RAM
   
   Errors:
     Signals CRYPTO-ERROR if derivation fails"
  (declare (type (or string (vector (unsigned-byte 8))) password salt)
           (type (integer 2 *) n)
           (type (integer 1 *) r p)
           (type (integer 1 512) key-length))
  
  ;; Verify N is a power of 2
  (unless (= n (ash 1 (integer-length (1- n))))
    (error 'crypto-error :code -1
           :message "Scrypt parameter N must be a power of 2"))
  
  (let ((password-bytes (if (stringp password)
                            (sb-ext:string-to-octets password :external-format :utf-8)
                            password))
        (salt-bytes (if (stringp salt)
                        (sb-ext:string-to-octets salt :external-format :utf-8)
                        salt))
        (output (make-array key-length :element-type '(unsigned-byte 8))))
    
    (sb-sys:with-pinned-objects (password-bytes salt-bytes output)
      (let ((result (ffi:%evp-pbe-scrypt
                     (sb-sys:vector-sap password-bytes)
                     (length password-bytes)
                     (sb-sys:vector-sap salt-bytes)
                     (length salt-bytes)
                     n r p
                     (* 1024 1024 32)  ; max memory = 32MB
                     (sb-sys:vector-sap output)
                     key-length)))
        (when (zerop result)
          (error 'crypto-error :code (ffi:%err-get-error)
                 :message "Scrypt key derivation failed"))))
    
    output))

;;;; Argon2 - Modern memory-hard KDF

(defun argon2 (password salt &key
                       (variant :argon2id)
                       (memory 65536)      ; KB
                       (iterations 3)
                       (parallelism 4)
                       (key-length 32))
  "Derive a key using Argon2 (winner of Password Hashing Competition).
   
   Argon2 is the recommended KDF for new applications, providing
   strong resistance against both time-memory trade-offs and side-channel attacks.
   
   Parameters:
     password (string or bytes): Password to derive from
     salt (string or bytes): Random salt (minimum 16 bytes)
     variant (keyword): :argon2i, :argon2d, or :argon2id (default)
     memory (integer): Memory usage in KB (default: 64MB)
     iterations (integer): Time cost (default: 3)
     parallelism (integer): Parallelism degree (default: 4)
     key-length (integer): Output length in bytes (default: 32)
   
   Variants:
     :argon2i - Optimized against side-channel attacks
     :argon2d - Optimized against GPU attacks
     :argon2id - Hybrid mode (recommended)
   
   Parameter Guidelines:
     - Minimum: memory=64MB, iterations=3
     - Recommended: memory=128MB, iterations=3
     - High security: memory=256MB, iterations=4
   
   Security Notes:
     - Use :argon2id for password storage (best of both worlds)
     - Minimum 16-byte salt, prefer 32 bytes
     - Adjust parameters based on target hardware
   
   Performance:
     - 64MB, t=3: ~100-200ms
     - 128MB, t=3: ~200-400ms
     - 256MB, t=4: ~500-1000ms
   
   Note: This function requires OpenSSL 3.2+ or libargon2.
         Returns NIL if Argon2 is not available.
   
   Errors:
     Signals CRYPTO-ERROR if derivation fails"
  ;; Note: Argon2 support requires OpenSSL 3.2+ or separate binding
  ;; This is a placeholder for future implementation
  (declare (ignore password salt variant memory iterations parallelism key-length))
  (error 'crypto-error :code -1
         :message "Argon2 not yet implemented - requires OpenSSL 3.2+ or libargon2"))

