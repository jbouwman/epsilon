;;;; Cryptography Benchmarks
;;;;
;;;; This module provides benchmarks for cryptographic operations

(defpackage epsilon.tool.benchmark.crypto
  (:use cl)
  (:local-nicknames
   (benchmark epsilon.tool.benchmark)
   (suites epsilon.tool.benchmark.suites)
   (crypto epsilon.crypto)
   (blake2 epsilon.crypto.blake2)
   (aead epsilon.crypto.aead))
  (:export
   register-crypto-benchmarks))

(in-package epsilon.tool.benchmark.crypto)

;;; Hash Function Benchmarks

(defun register-hash-benchmarks ()
  "Register hash function benchmarks"
  
  ;; SHA-256 benchmark
  (benchmark:defbenchmark crypto-sha256-1kb ()
    (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
      (crypto:digest data crypto:+digest-sha256+)))
  
  (benchmark:defbenchmark crypto-sha256-64kb ()
    (let ((data (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
      (crypto:digest data crypto:+digest-sha256+)))
  
  ;; SHA-512 benchmark
  (benchmark:defbenchmark crypto-sha512-1kb ()
    (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
      (crypto:digest data crypto:+digest-sha512+)))
  
  ;; SHA3-256 benchmark
  (when (boundp 'crypto:+digest-sha3-256+)
    (benchmark:defbenchmark crypto-sha3-256-1kb ()
      (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
        (crypto:digest data crypto:+digest-sha3-256+))))
  
  ;; BLAKE2b benchmarks
  (when (find-package "EPSILON.CRYPTO.BLAKE2")
    (benchmark:defbenchmark crypto-blake2b-1kb ()
      (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
        (blake2:blake2b data :output-length 32)))
    
    (benchmark:defbenchmark crypto-blake2b-64kb ()
      (let ((data (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
        (blake2:blake2b data :output-length 32)))
    
    (benchmark:defbenchmark crypto-blake2s-1kb ()
      (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
        (blake2:blake2s data)))))

;;; KDF Benchmarks

(defun register-kdf-benchmarks ()
  "Register Key Derivation Function benchmarks"
  
  ;; PBKDF2 with various iteration counts
  (benchmark:defbenchmark crypto-pbkdf2-1k-iterations ()
    (let ((password "benchmark-password")
          (salt (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42)))
      (crypto:pbkdf2 password salt 
                     :iterations 1000 
                     :key-length 32
                     :digest crypto:+digest-sha256+)))
  
  (benchmark:defbenchmark crypto-pbkdf2-10k-iterations ()
    (let ((password "benchmark-password")
          (salt (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42)))
      (crypto:pbkdf2 password salt 
                     :iterations 10000 
                     :key-length 32
                     :digest crypto:+digest-sha256+)))
  
  ;; HKDF
  (when (fboundp 'crypto:hkdf)
    (benchmark:defbenchmark crypto-hkdf-expand ()
      (let ((key-material (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
            (info (make-array 16 :element-type '(unsigned-byte 8) :initial-element 2)))
        (crypto:hkdf key-material 
                     :info info 
                     :length 64
                     :digest crypto:+digest-sha256+))))
  
  ;; Scrypt (if available)
  (when (fboundp 'crypto:scrypt)
    (benchmark:defbenchmark crypto-scrypt-standard ()
      (let ((password "benchmark-password")
            (salt (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42)))
        (crypto:scrypt password salt 
                       :n 16384 :r 8 :p 1 
                       :key-length 32)))))

;;; Symmetric Encryption Benchmarks

(defun register-symmetric-benchmarks ()
  "Register symmetric encryption benchmarks"
  
  ;; AES-256-GCM
  (benchmark:defbenchmark crypto-aes256-gcm-encrypt-1kb ()
    (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
          (data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
      (aead:aead-encrypt :aes-256-gcm key data)))
  
  (benchmark:defbenchmark crypto-aes256-gcm-encrypt-64kb ()
    (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
          (data (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
      (aead:aead-encrypt :aes-256-gcm key data)))
  
  ;; ChaCha20-Poly1305
  (when (member :chacha20-poly1305 (aead:available-ciphers))
    (benchmark:defbenchmark crypto-chacha20-poly1305-encrypt-1kb ()
      (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
            (data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
        (aead:aead-encrypt :chacha20-poly1305 key data)))
    
    (benchmark:defbenchmark crypto-chacha20-poly1305-encrypt-64kb ()
      (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
            (data (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
        (aead:aead-encrypt :chacha20-poly1305 key data)))))

;;; Asymmetric Cryptography Benchmarks

(defun register-asymmetric-benchmarks ()
  "Register asymmetric cryptography benchmarks"
  
  ;; RSA operations
  (let ((rsa-2048-key nil)
        (rsa-4096-key nil))
    
    ;; Key generation benchmarks
    (benchmark:defbenchmark crypto-rsa-2048-keygen ()
      (crypto:generate-rsa-key :bits 2048))
    
    (benchmark:defbenchmark crypto-rsa-4096-keygen ()
      (crypto:generate-rsa-key :bits 4096))
    
    ;; Initialize keys for other benchmarks
    (handler-case
        (progn
          (setf rsa-2048-key (crypto:generate-rsa-key :bits 2048))
          (setf rsa-4096-key (crypto:generate-rsa-key :bits 4096))
          
          ;; RSA signing benchmarks
          (benchmark:defbenchmark crypto-rsa-2048-sign ()
            (crypto:sign-message rsa-2048-key "Benchmark message for signing"))
          
          (benchmark:defbenchmark crypto-rsa-4096-sign ()
            (crypto:sign-message rsa-4096-key "Benchmark message for signing"))
          
          ;; RSA encryption benchmarks
          (benchmark:defbenchmark crypto-rsa-2048-encrypt ()
            (crypto:encrypt rsa-2048-key "Short message"))
          
          (benchmark:defbenchmark crypto-rsa-4096-encrypt ()
            (crypto:encrypt rsa-4096-key "Short message")))
      (error (e) 
        (format t "Warning: Could not initialize RSA benchmarks: ~A~%" e))))
  
  ;; ECC operations
  (let ((ec-p256-key nil)
        (ec-p384-key nil))
    
    ;; Key generation
    (benchmark:defbenchmark crypto-ec-p256-keygen ()
      (crypto:generate-ec-key :curve :p256))
    
    (benchmark:defbenchmark crypto-ec-p384-keygen ()
      (crypto:generate-ec-key :curve :p384))
    
    ;; Initialize keys for other benchmarks
    (handler-case
        (progn
          (setf ec-p256-key (crypto:generate-ec-key :curve :p256))
          (setf ec-p384-key (crypto:generate-ec-key :curve :p384))
          
          ;; ECC signing benchmarks
          (benchmark:defbenchmark crypto-ec-p256-sign ()
            (crypto:sign-message ec-p256-key "Benchmark message for signing"))
          
          (benchmark:defbenchmark crypto-ec-p384-sign ()
            (crypto:sign-message ec-p384-key "Benchmark message for signing")))
      (error (e) 
        (format t "Warning: Could not initialize ECC benchmarks: ~A~%" e))))
  
  ;; Ed25519 operations
  (let ((ed25519-key nil))
    
    (benchmark:defbenchmark crypto-ed25519-keygen ()
      (crypto:generate-ed25519-key))
    
    ;; Initialize key for other benchmarks
    (handler-case
        (progn
          (setf ed25519-key (crypto:generate-ed25519-key))
          
          (benchmark:defbenchmark crypto-ed25519-sign ()
            (crypto:sign-message ed25519-key "Benchmark message for signing")))
      (error (e) 
        (format t "Warning: Could not initialize Ed25519 benchmarks: ~A~%" e)))))

;;; Random Number Generation Benchmarks

(defun register-random-benchmarks ()
  "Register random number generation benchmarks"
  
  (benchmark:defbenchmark crypto-random-bytes-32 ()
    (crypto:crypto-random-bytes 32))
  
  (benchmark:defbenchmark crypto-random-bytes-256 ()
    (crypto:crypto-random-bytes 256))
  
  (benchmark:defbenchmark crypto-random-bytes-4096 ()
    (crypto:crypto-random-bytes 4096))
  
  (benchmark:defbenchmark crypto-random-integer ()
    (crypto:crypto-random-integer most-positive-fixnum)))

;;; Certificate Operations Benchmarks

(defun register-certificate-benchmarks ()
  "Register certificate operation benchmarks"
  
  (let ((test-key nil))
    
    ;; Initialize key for certificate benchmarks
    (handler-case
        (progn
          (setf test-key (crypto:generate-rsa-key :bits 2048))
          
          ;; Certificate creation
          (benchmark:defbenchmark crypto-certificate-create ()
            (crypto:create-certificate
             :key test-key
             :subject-cn "benchmark.example.com"
             :serial 1
             :not-before (get-universal-time)
             :not-after (+ (get-universal-time) (* 365 24 60 60))))
          
          ;; CSR creation
          (benchmark:defbenchmark crypto-csr-create ()
            (crypto:create-csr test-key
                              :subject-cn "benchmark.example.com"
                              :subject-c "US"
                              :subject-o "Benchmark Corp")))
      (error (e) 
        (format t "Warning: Could not initialize certificate benchmarks: ~A~%" e)))))

;;; Main registration function

(defun register-crypto-benchmarks ()
  "Register all cryptography benchmarks"
  (register-hash-benchmarks)
  (register-kdf-benchmarks)
  (register-symmetric-benchmarks)
  (register-asymmetric-benchmarks)
  (register-random-benchmarks)
  (register-certificate-benchmarks)
  
  ;; Register the crypto suite
  (suites:register-suite 'crypto-operations
                        :description "Cryptographic operations benchmarks"
                        :benchmarks '(;; Hash functions
                                     crypto-sha256-1kb
                                     crypto-sha256-64kb
                                     crypto-sha512-1kb
                                     crypto-blake2b-1kb
                                     crypto-blake2s-1kb
                                     ;; KDF
                                     crypto-pbkdf2-1k-iterations
                                     crypto-pbkdf2-10k-iterations
                                     ;; Symmetric
                                     crypto-aes256-gcm-encrypt-1kb
                                     crypto-chacha20-poly1305-encrypt-1kb
                                     ;; Asymmetric
                                     crypto-rsa-2048-sign
                                     crypto-ec-p256-sign
                                     crypto-ed25519-sign
                                     ;; Random
                                     crypto-random-bytes-32
                                     ;; Certificates
                                     crypto-certificate-create
                                     crypto-csr-create)))