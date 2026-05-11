;;;; AES-CCM AEAD (NIST SP 800-38C / RFC 3610)
;;;;
;;;; Counter with CBC-MAC. CCM combines CBC-MAC for authentication and
;;;; CTR mode for encryption, both keyed by the same AES key. Used in
;;;; ZigBee, Bluetooth LE, IEEE 802.11 (WPA2 CCMP), and MQTT-over-DTLS.
;;;; Less common than GCM in TLS but still listed in RFC 6655.
;;;;
;;;; Parameters (RFC 3610 §2.2):
;;;;   - K  : AES key (16 or 32 bytes; AES-192 also valid but rarely used)
;;;;   - N  : nonce, 7 to 13 bytes (typically 12)
;;;;   - L  : 15 - len(N), the encoded length-of-plaintext field width
;;;;   - M  : tag length in bytes; one of 4, 6, 8, 10, 12, 14, 16
;;;;
;;;; The authenticated input layout (B0 || encode_AAD(A) || pad || P || pad)
;;;; is from SP 800-38C §6.2; the CTR encryption layout (A0, A1, ...) is
;;;; from §6.3.

(defpackage epsilon.crypto.aes-ccm
  (:use :cl)
  (:import (epsilon.crypto.aes aes))
  (:export
   #:aes-ccm-encrypt
   #:aes-ccm-decrypt))

(in-package :epsilon.crypto.aes-ccm)

(defconstant +block-size+ 16)

(defun %valid-tag-length-p (m)
  (and (integerp m) (>= m 4) (<= m 16) (evenp m)))

(defun %valid-nonce-length-p (n-len)
  (and (integerp n-len) (>= n-len 7) (<= n-len 13)))

(defun %encode-length-be (value width)
  "Encode VALUE as a WIDTH-byte big-endian integer."
  (let ((b (make-array width :element-type '(unsigned-byte 8)
                       :initial-element 0)))
    (loop for i from (1- width) downto 0
          for shift from 0 by 8
          do (setf (aref b i) (logand #xFF (ash value (- shift)))))
    b))

(defun %make-b0 (n m plaintext-len has-aad)
  "Build the first authenticated block B0 per SP 800-38C §6.2.
   Flags byte: bit 6 = Adata flag, bits 5..3 = (M-2)/2, bits 2..0 = L-1."
  (let* ((n-len (length n))
         (l (- 15 n-len))
         (flags (logior (if has-aad #x40 0)
                        (ash (/ (- m 2) 2) 3)
                        (logand #x07 (1- l))))
         (q (%encode-length-be plaintext-len l))
         (b0 (make-array +block-size+ :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (setf (aref b0 0) flags)
    (replace b0 n :start1 1 :end1 (1+ n-len))
    (replace b0 q :start1 (- +block-size+ l))
    b0))

(defun %encode-aad-prefix (aad-len)
  "Encode the AAD length prefix (RFC 3610 §2.2 / SP 800-38C §A.2.2).
   Returns the prefix bytes (which are concatenated with AAD itself
   before zero-padding to a block boundary)."
  (cond
    ((zerop aad-len) (make-array 0 :element-type '(unsigned-byte 8)))
    ((< aad-len 65280)             ; 0xFF00
     (let ((b (make-array 2 :element-type '(unsigned-byte 8))))
       (setf (aref b 0) (logand #xFF (ash aad-len -8)))
       (setf (aref b 1) (logand #xFF aad-len))
       b))
    ((< aad-len (ash 1 32))
     (let ((b (make-array 6 :element-type '(unsigned-byte 8))))
       (setf (aref b 0) #xFF)
       (setf (aref b 1) #xFE)
       (loop for i from 5 downto 2
             for shift from 0 by 8
             do (setf (aref b i) (logand #xFF (ash aad-len (- shift)))))
       b))
    (t
     (let ((b (make-array 10 :element-type '(unsigned-byte 8))))
       (setf (aref b 0) #xFF)
       (setf (aref b 1) #xFF)
       (loop for i from 9 downto 2
             for shift from 0 by 8
             do (setf (aref b i) (logand #xFF (ash aad-len (- shift)))))
       b))))

(defun %xor-block-into! (dest src start)
  "DEST[0..15] ^= SRC[start..start+16]."
  (loop for i from 0 below +block-size+
        do (setf (aref dest i) (logxor (aref dest i) (aref src (+ start i))))))

(defun %xor-partial-into! (dest src src-start src-len)
  "DEST[0..src-len-1] ^= SRC[src-start..src-start+src-len]."
  (loop for i from 0 below src-len
        do (setf (aref dest i) (logxor (aref dest i)
                                       (aref src (+ src-start i))))))

(defun %cbc-mac (round-keys data)
  "Compute CBC-MAC over DATA (whose length must be a multiple of 16)
   using the supplied AES round keys. Returns the 16-byte final block."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-array +block-size+ :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (loop for offset from 0 below (length data) by +block-size+
          do (%xor-block-into! state data offset)
             (let ((enc (aes:aes-encrypt-block state round-keys)))
               (replace state enc)))
    state))

(defun %build-mac-input (b0 aad plaintext)
  "Concatenate B0 || encode_aad_prefix(len(AAD)) || AAD || zero-pad ||
   plaintext || zero-pad with each section padded to a multiple of 16."
  (declare (type (simple-array (unsigned-byte 8) (*)) b0 aad plaintext))
  (let* ((aad-len (length aad))
         (aad-prefix (%encode-aad-prefix aad-len))
         (aad-section-len
           (let ((raw (+ (length aad-prefix) aad-len)))
             (if (zerop raw) 0 (* +block-size+ (ceiling raw +block-size+)))))
         (pt-section-len
           (if (zerop (length plaintext))
               0
               (* +block-size+ (ceiling (length plaintext) +block-size+))))
         (total (+ +block-size+ aad-section-len pt-section-len))
         (out (make-array total :element-type '(unsigned-byte 8)
                          :initial-element 0))
         (off 0))
    (replace out b0 :start1 0)
    (incf off +block-size+)
    (when (plusp aad-section-len)
      (replace out aad-prefix :start1 off)
      (replace out aad :start1 (+ off (length aad-prefix)))
      (incf off aad-section-len))
    (when (plusp pt-section-len)
      (replace out plaintext :start1 off
               :end2 (length plaintext))
      (incf off pt-section-len))
    out))

(defun %make-ctr-block (n counter)
  "Build a CTR mode formatting block A_i for SP 800-38C §6.3.
   Flags byte: bits 7..6 = 0, bits 5..3 = 0 (reserved), bits 2..0 = L-1."
  (let* ((n-len (length n))
         (l (- 15 n-len))
         (flags (logand #x07 (1- l)))
         (block (make-array +block-size+ :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (setf (aref block 0) flags)
    (replace block n :start1 1 :end1 (1+ n-len))
    (replace block (%encode-length-be counter l)
             :start1 (- +block-size+ l))
    block))

(defun %ctr-encrypt (round-keys n plaintext s0)
  "CTR encryption per SP 800-38C §6.3.
   S0 receives AES_K(A_0); subsequent counters 1, 2, ... encrypt the
   plaintext blocks. Returns the ciphertext."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext))
  (let* ((len (length plaintext))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (let ((a0 (%make-ctr-block n 0)))
      (replace s0 (aes:aes-encrypt-block a0 round-keys)))
    (loop with offset = 0
          for ctr from 1
          while (< offset len)
          for block-len = (min +block-size+ (- len offset))
          for ctr-block = (%make-ctr-block n ctr)
          for ks = (aes:aes-encrypt-block ctr-block round-keys)
          do (loop for i from 0 below block-len
                   do (setf (aref out (+ offset i))
                            (logxor (aref plaintext (+ offset i))
                                    (aref ks i))))
             (incf offset block-len))
    out))

(defun aes-ccm-encrypt (plaintext key nonce
                        &key (aad (make-array 0 :element-type '(unsigned-byte 8)))
                             (tag-length 16))
  "Encrypt PLAINTEXT under AES-CCM (NIST SP 800-38C / RFC 3610).
   KEY:        16 or 32 bytes (AES-128 or AES-256).
   NONCE:      7..13 bytes (typically 12).
   AAD:        optional associated data.
   TAG-LENGTH: one of 4, 6, 8, 10, 12, 14, 16; defaults to 16.
   Returns (values ciphertext tag) where TAG is TAG-LENGTH bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext key nonce aad))
  (unless (%valid-nonce-length-p (length nonce))
    (error "AES-CCM: nonce length ~D is not in [7, 13]" (length nonce)))
  (unless (%valid-tag-length-p tag-length)
    (error "AES-CCM: tag length ~D is not one of {4,6,8,10,12,14,16}"
           tag-length))
  (let* ((round-keys (aes:make-aes-round-keys key))
         (b0 (%make-b0 nonce tag-length (length plaintext) (plusp (length aad))))
         (mac-input (%build-mac-input b0 aad plaintext))
         (t-block (%cbc-mac round-keys mac-input))
         (s0 (make-array +block-size+ :element-type '(unsigned-byte 8)
                         :initial-element 0))
         (ciphertext (%ctr-encrypt round-keys nonce plaintext s0))
         (tag (make-array tag-length :element-type '(unsigned-byte 8))))
    (loop for i from 0 below tag-length
          do (setf (aref tag i) (logxor (aref t-block i) (aref s0 i))))
    (values ciphertext tag)))

(defun aes-ccm-decrypt (ciphertext key nonce tag
                        &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "Decrypt CIPHERTEXT produced by AES-CCM and verify TAG.
   Returns the plaintext on success; signals an error if the tag does
   not match. TAG-LENGTH is inferred from (length tag)."
  (declare (type (simple-array (unsigned-byte 8) (*)) ciphertext key nonce tag aad))
  (unless (%valid-nonce-length-p (length nonce))
    (error "AES-CCM: nonce length ~D is not in [7, 13]" (length nonce)))
  (unless (%valid-tag-length-p (length tag))
    (error "AES-CCM: tag length ~D is not one of {4,6,8,10,12,14,16}"
           (length tag)))
  (let* ((round-keys (aes:make-aes-round-keys key))
         (s0 (make-array +block-size+ :element-type '(unsigned-byte 8)
                         :initial-element 0))
         (plaintext (%ctr-encrypt round-keys nonce ciphertext s0))
         (b0 (%make-b0 nonce (length tag) (length ciphertext)
                       (plusp (length aad))))
         (mac-input (%build-mac-input b0 aad plaintext))
         (t-block (%cbc-mac round-keys mac-input))
         (computed (make-array (length tag) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length tag)
          do (setf (aref computed i) (logxor (aref t-block i) (aref s0 i))))
    ;; Constant-time tag comparison
    (let ((diff 0))
      (loop for i from 0 below (length tag)
            do (setf diff (logior diff (logxor (aref tag i)
                                               (aref computed i)))))
      (unless (zerop diff)
        (error "AES-CCM: authentication tag mismatch")))
    plaintext))
