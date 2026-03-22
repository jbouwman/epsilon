;;;; ChaCha20-Poly1305 AEAD (RFC 8439)
;;;;
;;;; Authenticated encryption with associated data combining
;;;; ChaCha20 for encryption and Poly1305 for authentication.

(defpackage epsilon.ssl.chacha20-poly1305
  (:use :cl)
  (:local-nicknames
   (#:chacha #:epsilon.ssl.chacha20)
   (#:poly #:epsilon.ssl.poly1305))
  (:export
   #:chacha20-poly1305-encrypt
   #:chacha20-poly1305-decrypt))

(in-package :epsilon.ssl.chacha20-poly1305)

(defun pad16 (len)
  "Number of zero padding bytes needed to reach a multiple of 16."
  (let ((r (mod len 16)))
    (if (zerop r) 0 (- 16 r))))

(defun u64-to-le-bytes (val)
  "Convert a 64-bit integer to 8 little-endian bytes."
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 8
          do (setf (aref bytes i) (logand #xFF (ash val (* i -8)))))
    bytes))

(defun construct-mac-data (aad ciphertext)
  "Construct the Poly1305 MAC input per RFC 8439 Section 2.8:
   AAD || pad(AAD) || ciphertext || pad(ciphertext) || len(AAD) as u64le || len(CT) as u64le"
  (let* ((aad-len (length aad))
         (ct-len (length ciphertext))
         (aad-pad (pad16 aad-len))
         (ct-pad (pad16 ct-len))
         (total (+ aad-len aad-pad ct-len ct-pad 8 8))
         (data (make-array total :element-type '(unsigned-byte 8) :initial-element 0))
         (offset 0))
    ;; AAD
    (replace data aad :start1 0)
    (incf offset (+ aad-len aad-pad))
    ;; Ciphertext
    (replace data ciphertext :start1 offset)
    (incf offset (+ ct-len ct-pad))
    ;; AAD length (64-bit LE)
    (let ((aad-len-bytes (u64-to-le-bytes aad-len)))
      (replace data aad-len-bytes :start1 offset))
    (incf offset 8)
    ;; Ciphertext length (64-bit LE)
    (let ((ct-len-bytes (u64-to-le-bytes ct-len)))
      (replace data ct-len-bytes :start1 offset))
    data))

(defun chacha20-poly1305-encrypt (plaintext key nonce &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "Encrypt plaintext using ChaCha20-Poly1305 AEAD (RFC 8439).
   KEY: 32 bytes. NONCE: 12 bytes. AAD: optional associated data.
   Returns (values ciphertext tag) where tag is 16 bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext key nonce aad))
  ;; Step 1: Generate Poly1305 one-time key from first ChaCha20 block (counter=0)
  (let* ((poly-key-material (chacha:chacha20-block key 0 nonce))
         (poly-key (subseq poly-key-material 0 32))
         ;; Step 2: Encrypt plaintext using ChaCha20 starting at counter=1
         (ciphertext (chacha:chacha20-encrypt plaintext key nonce :initial-counter 1))
         ;; Step 3: Compute Poly1305 tag over constructed MAC data
         (mac-data (construct-mac-data aad ciphertext))
         (tag (poly:poly1305 poly-key mac-data)))
    (values ciphertext tag)))

(defun chacha20-poly1305-decrypt (ciphertext key nonce tag &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "Decrypt ciphertext using ChaCha20-Poly1305 AEAD (RFC 8439).
   KEY: 32 bytes. NONCE: 12 bytes. TAG: 16 bytes. AAD: optional.
   Returns plaintext if tag is valid, or signals an error."
  (declare (type (simple-array (unsigned-byte 8) (*)) ciphertext key nonce tag aad))
  ;; Step 1: Generate Poly1305 one-time key
  (let* ((poly-key-material (chacha:chacha20-block key 0 nonce))
         (poly-key (subseq poly-key-material 0 32))
         ;; Step 2: Verify tag
         (mac-data (construct-mac-data aad ciphertext))
         (computed-tag (poly:poly1305 poly-key mac-data)))
    ;; Constant-time comparison
    (let ((diff 0))
      (loop for i from 0 below 16
            do (setf diff (logior diff (logxor (aref tag i) (aref computed-tag i)))))
      (unless (zerop diff)
        (error "ChaCha20-Poly1305: authentication tag mismatch"))))
  ;; Step 3: Decrypt (ChaCha20 is symmetric)
  (chacha:chacha20-encrypt ciphertext key nonce :initial-counter 1))
