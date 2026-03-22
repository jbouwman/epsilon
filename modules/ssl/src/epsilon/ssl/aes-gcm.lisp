;;;; AES-GCM (Galois/Counter Mode, NIST SP 800-38D)
;;;;
;;;; Authenticated encryption with associated data (AEAD) combining
;;;; AES-CTR encryption with GHASH authentication.

(defpackage epsilon.ssl.aes-gcm
  (:use :cl)
  (:local-nicknames
   (#:aes #:epsilon.ssl.aes)
   (#:ghash #:epsilon.ssl.ghash))
  (:import-from #:epsilon.ssl.primitives #:u64-to-bytes-be)
  (:export
   #:aes-gcm-encrypt
   #:aes-gcm-decrypt))

(in-package :epsilon.ssl.aes-gcm)

(defun pad-to-16 (data)
  "Pad DATA to a multiple of 16 bytes with zeros. Returns new array."
  (let* ((len (length data))
         (padded-len (* 16 (ceiling len 16))))
    (if (= len padded-len)
        data
        (let ((padded (make-array padded-len :element-type '(unsigned-byte 8) :initial-element 0)))
          (replace padded data)
          padded))))

(defun gcm-inc32 (counter)
  "Increment the rightmost 32 bits of a 16-byte counter block (big-endian)."
  (declare (type (simple-array (unsigned-byte 8) (*)) counter))
  (loop for i from 15 downto 12
        do (let ((new (1+ (aref counter i))))
             (if (> new #xFF)
                 (setf (aref counter i) 0)
                 (progn
                   (setf (aref counter i) (logand #xFF new))
                   (return))))))

(defun gcm-encrypt-ctr (plaintext round-keys j0)
  "Encrypt plaintext using AES-CTR with initial counter J0+1.
   Returns ciphertext."
  (let* ((len (length plaintext))
         (output (make-array len :element-type '(unsigned-byte 8)))
         (counter (copy-seq j0)))
    (gcm-inc32 counter) ; Start from J0+1
    (loop for offset from 0 below len by 16
          for block-len = (min 16 (- len offset))
          for keystream = (aes:aes-encrypt-block counter round-keys)
          do (loop for i from 0 below block-len
                   do (setf (aref output (+ offset i))
                            (logxor (aref plaintext (+ offset i))
                                    (aref keystream i))))
             (gcm-inc32 counter))
    output))

(defun compute-ghash-tag (hash-key aad ciphertext j0 round-keys)
  "Compute the GCM authentication tag.
   GHASH(H, A, C) where A=AAD, C=ciphertext, then XOR with E(K, J0)."
  ;; Build GHASH input: pad(A) || pad(C) || len(A)*8 || len(C)*8
  (let* ((a-padded (pad-to-16 aad))
         (c-padded (pad-to-16 ciphertext))
         (total-len (+ (length a-padded) (length c-padded) 16))
         (ghash-input (make-array total-len :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Copy padded AAD
    (replace ghash-input a-padded)
    ;; Copy padded ciphertext
    (replace ghash-input c-padded :start1 (length a-padded))
    ;; Append bit lengths (64-bit big-endian)
    (let ((len-offset (+ (length a-padded) (length c-padded))))
      (u64-to-bytes-be (* (length aad) 8) ghash-input len-offset)
      (u64-to-bytes-be (* (length ciphertext) 8) ghash-input (+ len-offset 8)))
    ;; Compute GHASH
    (let ((s (ghash:ghash hash-key ghash-input))
          ;; Encrypt J0 to get the tag mask
          (tag-mask (aes:aes-encrypt-block j0 round-keys)))
      ;; Tag = GHASH(H, A, C) XOR E(K, J0)
      (let ((tag (make-array 16 :element-type '(unsigned-byte 8))))
        (loop for i from 0 below 16
              do (setf (aref tag i) (logxor (aref s i) (aref tag-mask i))))
        tag))))

(defun aes-gcm-encrypt (plaintext key nonce &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "Encrypt plaintext using AES-GCM.
   KEY: 16 or 32 bytes. NONCE: 12 bytes. AAD: optional associated data.
   Returns (values ciphertext tag) where tag is 16 bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext key nonce aad))
  (let* ((round-keys (aes:make-aes-round-keys key))
         ;; H = AES_K(0^128)
         (zero-block (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
         (h (aes:aes-encrypt-block zero-block round-keys))
         (hash-key (ghash:make-ghash-key h))
         ;; J0 = nonce || 0x00000001 (for 96-bit nonce)
         (j0 (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace j0 nonce :end1 12)
    (setf (aref j0 15) 1)
    ;; Encrypt
    (let* ((ciphertext (gcm-encrypt-ctr plaintext round-keys j0))
           ;; Compute tag
           (tag (compute-ghash-tag hash-key aad ciphertext j0 round-keys)))
      (values ciphertext tag))))

(defun aes-gcm-decrypt (ciphertext key nonce tag &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "Decrypt ciphertext using AES-GCM.
   KEY: 16 or 32 bytes. NONCE: 12 bytes. TAG: 16 bytes. AAD: optional.
   Returns plaintext if tag is valid, or signals an error."
  (declare (type (simple-array (unsigned-byte 8) (*)) ciphertext key nonce tag aad))
  (let* ((round-keys (aes:make-aes-round-keys key))
         (zero-block (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
         (h (aes:aes-encrypt-block zero-block round-keys))
         (hash-key (ghash:make-ghash-key h))
         (j0 (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace j0 nonce :end1 12)
    (setf (aref j0 15) 1)
    ;; Verify tag first
    (let ((computed-tag (compute-ghash-tag hash-key aad ciphertext j0 round-keys)))
      ;; Constant-time comparison
      (let ((diff 0))
        (loop for i from 0 below 16
              do (setf diff (logior diff (logxor (aref tag i) (aref computed-tag i)))))
        (unless (zerop diff)
          (error "AES-GCM: authentication tag mismatch"))))
    ;; Decrypt (CTR mode is symmetric)
    (gcm-encrypt-ctr ciphertext round-keys j0)))
