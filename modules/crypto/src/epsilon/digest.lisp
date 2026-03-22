;;;; epsilon.digest - High-Performance Hashing Module
;;;;
;;;; Provides state-of-the-art hash algorithms with native SIMD acceleration
;;;; and seamless epsilon.io integration.
;;;;
;;;; Algorithms:
;;;; - BLAKE3: Cryptographic, fastest crypto hash, XOF/KDF/MAC support
;;;; - xxHash3: Non-cryptographic, ultra-fast, 64/128-bit variants
;;;; - SHA-2/SHA-3: Traditional cryptographic hashes via epsilon.ssl
;;;; - MD5/SHA-1: Legacy hashes (for compatibility only)
;;;; - CRC-32: Non-cryptographic checksum
;;;;
;;;; Usage:
;;;;
;;;; One-shot hashing:
;;;;   (blake3 data)                    ; => 32-byte hash
;;;;   (xxhash64 data)                  ; => u64 integer
;;;;   (sha256 data)                    ; => 32-byte hash
;;;;
;;;; Streaming:
;;;;   (let ((h (make-blake3-hasher)))
;;;;     (hasher-update h chunk1)
;;;;     (hasher-update h chunk2)
;;;;     (hasher-finalize h))
;;;;
;;;; IO Integration:
;;;;   (hash-reader file-reader :blake3)

(defpackage epsilon.digest
  (:use :cl)
  (:require (epsilon.symbol sym)
            (epsilon.digest.protocol proto)
            (epsilon.digest.blake3 blake3)
            (epsilon.digest.xxhash3 xxh)
            (epsilon.digest.crc32 crc)
            (epsilon.digest.io dio)
            (epsilon.digest.ssl-hashers ssl-hashers)
            (epsilon.ssl ssl))
  (:enter t))

;;; Re-export protocol
(sym:reexport :epsilon.digest.protocol
              '(hasher-update hasher-finalize hasher-reset hasher-copy
                hasher-algorithm hasher-output-length hasher-block-length
                hasher-p cryptographic-hasher-p keyed-hasher-p
                hash-bytes hash-reader make-hashing-reader make-hashing-writer
                +blake3-output-length+ +blake3-key-length+ +blake3-block-length+
                +xxhash64-output-length+ +xxhash128-output-length+
                +md5-output-length+ +sha1-output-length+
                +sha256-output-length+ +sha384-output-length+ +sha512-output-length+
                +sha3-256-output-length+ +sha3-384-output-length+ +sha3-512-output-length+
                +crc32-output-length+))

;;; Re-export BLAKE3
(sym:reexport :epsilon.digest.blake3
              '(blake3-hasher make-blake3-hasher
                blake3 blake3-hex blake3-keyed blake3-keyed-hex
                blake3-derive-key blake3-xof blake3-xof-seek
                blake3-state make-blake3-state make-blake3-keyed-state
                make-blake3-derive-key-state
                blake3-update blake3-finalize blake3-copy))

;;; Re-export xxHash3
(sym:reexport :epsilon.digest.xxhash3
              '(xxhash64-hasher xxhash128-hasher
                make-xxhash64-hasher make-xxhash128-hasher
                xxhash64 xxhash128 xxhash64-bytes xxhash128-bytes xxh64
                xxhash-available-p xxhash64-to-hex xxhash128-to-hex))

;;; Re-export pure-Lisp hash functions from epsilon.ssl (always available)
(sym:reexport :epsilon.ssl
              '(md5 sha1 sha256 sha384 sha512 sha3-256 sha3-384 sha3-512))

;;; Re-export CRC-32
(sym:reexport :epsilon.digest.crc32
              '(crc32-hasher make-crc32-hasher crc32 crc32-int))

;;; Re-export IO integration
(sym:reexport :epsilon.digest.io
              '(hashing-reader hashing-writer
                make-hashing-reader make-hashing-writer
                multi-hashing-reader make-multi-hashing-reader
                hash-reader hash-reader-blake3 hash-reader-xxhash64 hash-reader-xxhash128
                hash-reader-md5 hash-reader-sha256 hash-reader-sha512
                hash-buffer hash-buffer-blake3 hash-buffer-xxhash64
                hash-buffer-md5 hash-buffer-sha256
                with-hashing-reader with-hashing-writer))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun available-algorithms ()
  "Return a list of available hash algorithms."
  '(;; Modern algorithms
    :blake3
    :blake3-keyed
    :blake3-derive
    :xxhash64
    :xxhash128
    ;; Traditional cryptographic algorithms via epsilon.ssl
    :md5
    :sha1
    :sha256
    :sha384
    :sha512
    :sha3-256
    :sha3-384
    :sha3-512
    ;; Non-cryptographic checksums
    :crc32))

(defun make-hasher (algorithm &rest args)
  "Create a hasher for ALGORITHM.

   ALGORITHM can be:
   - :blake3, :blake3-keyed, :blake3-derive (modern cryptographic)
   - :xxhash64, :xxhash128 (non-cryptographic)
   - :md5, :sha1, :sha256, :sha384, :sha512 (traditional via epsilon.ssl)
   - :sha3-256, :sha3-384, :sha3-512 (SHA-3 via epsilon.ssl)
   - :crc32 (checksum)

   ARGS are algorithm-specific keyword arguments."
  (case algorithm
    ;; Modern algorithms
    (:blake3 (apply #'blake3:make-blake3-hasher args))
    (:blake3-keyed (apply #'blake3:make-blake3-hasher :mode :keyed args))
    (:blake3-derive (apply #'blake3:make-blake3-hasher :mode :derive args))
    (:xxhash64 (apply #'xxh:make-xxhash64-hasher args))
    (:xxhash128 (apply #'xxh:make-xxhash128-hasher args))
    ;; Checksum
    (:crc32 (crc:make-crc32-hasher))
    ;; Traditional via epsilon.ssl (wrapped for hasher protocol)
    ((:md5 :sha1 :sha256 :sha384 :sha512 :sha3-256 :sha3-384 :sha3-512)
     (ssl-hashers:make-ssl-hasher algorithm))
    (t (error "Unknown hash algorithm: ~A" algorithm))))

(defun bytes-to-hex (bytes)
  "Convert a byte array to a lowercase hexadecimal string."
  (string-downcase
   (with-output-to-string (s)
     (loop for byte across bytes
           do (format s "~2,'0x" byte)))))

(defun hex-to-bytes (hex-string)
  "Convert a hexadecimal string to a byte array."
  (let* ((len (length hex-string))
         (bytes (make-array (/ len 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (/ len 2)
          do (setf (aref bytes i)
                   (parse-integer hex-string :start (* i 2) :end (+ (* i 2) 2) :radix 16)))
    bytes))
