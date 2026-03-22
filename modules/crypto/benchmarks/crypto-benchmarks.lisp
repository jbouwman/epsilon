;;;; Cryptography Benchmarks
;;;;
;;;; This module provides benchmarks for cryptographic operations.
;;;; Uses throughput metrics (MB/s) for data processing operations.

(defpackage epsilon.tool.benchmark.crypto
  (:use cl)
  (:local-nicknames
   (bench epsilon.tool.benchmark)
   (crypto epsilon.crypto)
   (blake2 epsilon.crypto)
   (aead epsilon.crypto))
  (:export
   register-crypto-benchmarks
   run-hash-throughput-benchmarks
   run-encryption-throughput-benchmarks)
  (:enter t))

;;; Hash Function Benchmarks

(defun register-hash-benchmarks ()
  "Register hash function benchmarks with throughput metrics"

  ;; SHA-256 benchmarks
  (bench:defbenchmark crypto/hash/sha256-1kb ()
    (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (crypto:digest data crypto:+digest-sha256+))))

  (bench:defbenchmark crypto/hash/sha256-64kb ()
    (let ((data (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (crypto:digest data crypto:+digest-sha256+))))

  ;; SHA-512 benchmark
  (bench:defbenchmark crypto/hash/sha512-1kb ()
    (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (crypto:digest data crypto:+digest-sha512+))))

  ;; SHA3-256 benchmark
  (when (boundp 'crypto:+digest-sha3-256+)
    (bench:defbenchmark crypto/hash/sha3-256-1kb ()
      (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
        (bench:consume (crypto:digest data crypto:+digest-sha3-256+)))))

  ;; BLAKE2b benchmarks
  (when (find-package "EPSILON.CRYPTO.HASH.BLAKE2")
    (benchmark:defbenchmark crypto-blake2b-1kb ()
      (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
        (bench:consume (blake2:blake2b data :output-length 32))))

    (bench:defbenchmark crypto/hash/blake2b-64kb ()
      (let ((data (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
        (bench:consume (blake2:blake2b data :output-length 32))))

    (bench:defbenchmark crypto/hash/blake2s-1kb ()
      (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
        (bench:consume (blake2:blake2s data))))))

;;; Parameterized Hash Throughput Benchmarks

(defun run-hash-throughput-benchmarks ()
  "Run hash benchmarks with various sizes to measure throughput"
  (bench:with-benchmark-group "crypto/hash-throughput"
    (dolist (size '(1024 4096 16384 65536 262144 1048576))
      (let ((data (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
        (bench:benchmark-with-input (format nil "sha256-~A" (format-size size))
          :input data
          :throughput (bench:throughput-bytes size)
          (bench:consume (crypto:digest input crypto:+digest-sha256+)))))))

(defun format-size (bytes)
  "Format byte size for display"
  (cond
    ((>= bytes 1048576) (format nil "~DMB" (/ bytes 1048576)))
    ((>= bytes 1024) (format nil "~DKB" (/ bytes 1024)))
    (t (format nil "~DB" bytes))))

;;; KDF Benchmarks

(defun register-kdf-benchmarks ()
  "Register Key Derivation Function benchmarks"

  ;; PBKDF2 with various iteration counts
  (bench:defbenchmark crypto/kdf/pbkdf2-1k ()
    (let ((password "benchmark-password")
          (salt (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42)))
      (bench:consume
       (crypto:pbkdf2 password salt
                      :iterations 1000
                      :key-length 32
                      :digest crypto:+digest-sha256+))))

  (bench:defbenchmark crypto/kdf/pbkdf2-10k ()
    (let ((password "benchmark-password")
          (salt (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42)))
      (bench:consume
       (crypto:pbkdf2 password salt
                      :iterations 10000
                      :key-length 32
                      :digest crypto:+digest-sha256+))))

  ;; HKDF
  (when (fboundp 'crypto:hkdf)
    (bench:defbenchmark crypto/kdf/hkdf ()
      (let ((key-material (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
            (info (make-array 16 :element-type '(unsigned-byte 8) :initial-element 2)))
        (bench:consume
         (crypto:hkdf key-material
                      :info info
                      :length 64
                      :digest crypto:+digest-sha256+)))))

  ;; Scrypt (if available)
  (when (fboundp 'crypto:scrypt)
    (bench:defbenchmark crypto/kdf/scrypt ()
      (let ((password "benchmark-password")
            (salt (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42)))
        (bench:consume
         (crypto:scrypt password salt
                        :n 16384 :r 8 :p 1
                        :key-length 32))))))

;;; Symmetric Encryption Benchmarks

(defun register-symmetric-benchmarks ()
  "Register symmetric encryption benchmarks with throughput"

  ;; AES-256-GCM
  (bench:defbenchmark crypto/aead/aes256-gcm-1kb ()
    (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
          (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0))
          (data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (crypto:aes-gcm-encrypt data key nonce))))

  (bench:defbenchmark crypto/aead/aes256-gcm-64kb ()
    (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
          (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0))
          (data (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (crypto:aes-gcm-encrypt data key nonce))))

  ;; ChaCha20-Poly1305
  (bench:defbenchmark crypto/aead/chacha20-poly1305-1kb ()
    (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
          (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0))
          (data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (crypto:chacha20-poly1305-encrypt data key nonce))))

  (bench:defbenchmark crypto/aead/chacha20-poly1305-64kb ()
    (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
          (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0))
          (data (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (crypto:chacha20-poly1305-encrypt data key nonce)))))

;;; Parameterized Encryption Throughput Benchmarks

(defun run-encryption-throughput-benchmarks ()
  "Run encryption benchmarks with various sizes to measure throughput"
  (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
        (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
    (bench:with-benchmark-group "crypto/aead-throughput"
      (dolist (size '(1024 4096 16384 65536 262144))
        (let ((data (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
          (bench:benchmark-with-input (format nil "aes256-gcm-~A" (format-size size))
            :input data
            :throughput (bench:throughput-bytes size)
            (bench:consume (crypto:aes-gcm-encrypt input key nonce))))))))

;;; Asymmetric Cryptography Benchmarks

(defun register-asymmetric-benchmarks ()
  "Register asymmetric cryptography benchmarks"

  ;; RSA operations
  (let ((rsa-2048-key nil)
        (rsa-4096-key nil))

    ;; Key generation benchmarks
    (bench:defbenchmark crypto/rsa/keygen-2048 ()
      (bench:consume (crypto:generate-rsa-key :bits 2048)))

    (bench:defbenchmark crypto/rsa/keygen-4096 ()
      (bench:consume (crypto:generate-rsa-key :bits 4096)))

    ;; Initialize keys for other benchmarks
    (handler-case
        (progn
          (setf rsa-2048-key (crypto:generate-rsa-key :bits 2048))
          (setf rsa-4096-key (crypto:generate-rsa-key :bits 4096))

          ;; RSA signing benchmarks
          (bench:defbenchmark crypto/rsa/sign-2048 ()
            (bench:consume
             (crypto:sign-message rsa-2048-key "Benchmark message for signing")))

          (bench:defbenchmark crypto/rsa/sign-4096 ()
            (bench:consume
             (crypto:sign-message rsa-4096-key "Benchmark message for signing")))

          ;; RSA encryption benchmarks
          (bench:defbenchmark crypto/rsa/encrypt-2048 ()
            (bench:consume (crypto:encrypt rsa-2048-key "Short message")))

          (bench:defbenchmark crypto/rsa/encrypt-4096 ()
            (bench:consume (crypto:encrypt rsa-4096-key "Short message"))))
      (error (e)
        (format t "Warning: Could not initialize RSA benchmarks: ~A~%" e))))

  ;; ECC operations
  (let ((ec-p256-key nil)
        (ec-p384-key nil))

    ;; Key generation
    (bench:defbenchmark crypto/ec/keygen-p256 ()
      (bench:consume (crypto:generate-ec-key :curve :p256)))

    (bench:defbenchmark crypto/ec/keygen-p384 ()
      (bench:consume (crypto:generate-ec-key :curve :p384)))

    ;; Initialize keys for other benchmarks
    (handler-case
        (progn
          (setf ec-p256-key (crypto:generate-ec-key :curve :p256))
          (setf ec-p384-key (crypto:generate-ec-key :curve :p384))

          ;; ECC signing benchmarks
          (bench:defbenchmark crypto/ec/sign-p256 ()
            (bench:consume
             (crypto:sign-message ec-p256-key "Benchmark message for signing")))

          (bench:defbenchmark crypto/ec/sign-p384 ()
            (bench:consume
             (crypto:sign-message ec-p384-key "Benchmark message for signing"))))
      (error (e)
        (format t "Warning: Could not initialize ECC benchmarks: ~A~%" e))))

  ;; Ed25519 operations
  (let ((ed25519-key nil))

    (bench:defbenchmark crypto/ed25519/keygen ()
      (bench:consume (crypto:generate-ed25519-key)))

    ;; Initialize key for other benchmarks
    (handler-case
        (progn
          (setf ed25519-key (crypto:generate-ed25519-key))

          (bench:defbenchmark crypto/ed25519/sign ()
            (bench:consume
             (crypto:sign-message ed25519-key "Benchmark message for signing"))))
      (error (e)
        (format t "Warning: Could not initialize Ed25519 benchmarks: ~A~%" e)))))

;;; Random Number Generation Benchmarks

(defun register-random-benchmarks ()
  "Register random number generation benchmarks with throughput"

  (bench:defbenchmark crypto/random/bytes-32 ()
    (bench:consume (crypto:crypto-random-bytes 32)))

  (bench:defbenchmark crypto/random/bytes-256 ()
    (bench:consume (crypto:crypto-random-bytes 256)))

  (bench:defbenchmark crypto/random/bytes-4096 ()
    (bench:consume (crypto:crypto-random-bytes 4096)))

  (bench:defbenchmark crypto/random/integer ()
    (bench:consume (crypto:crypto-random-integer most-positive-fixnum))))

;;; Certificate Operations Benchmarks

(defun register-certificate-benchmarks ()
  "Register certificate operation benchmarks"

  (let ((test-key nil))

    ;; Initialize key for certificate benchmarks
    (handler-case
        (progn
          (setf test-key (crypto:generate-rsa-key :bits 2048))

          ;; Certificate creation
          (bench:defbenchmark crypto/cert/create ()
            (bench:consume
             (crypto:create-certificate
              :key test-key
              :subject-cn "benchmark.example.com"
              :serial 1
              :not-before (get-universal-time)
              :not-after (+ (get-universal-time) (* 365 24 60 60)))))

          ;; CSR creation
          (bench:defbenchmark crypto/cert/csr ()
            (bench:consume
             (crypto:create-csr test-key
                                :subject-cn "benchmark.example.com"
                                :subject-c "US"
                                :subject-o "Benchmark Corp"))))
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
  (register-certificate-benchmarks))
