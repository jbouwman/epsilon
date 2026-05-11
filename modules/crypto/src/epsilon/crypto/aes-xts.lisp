;;;; AES-XTS (NIST SP 800-38E / IEEE Std 1619-2007)
;;;;
;;;; Tweakable block-cipher mode for storage encryption (FileVault,
;;;; BitLocker, LUKS, ZFS encryption). Uses two independent AES keys:
;;;; K1 encrypts the XEX-style data path, K2 encrypts the data unit
;;;; number to derive the per-data-unit tweak.
;;;;
;;;; Construction (one data unit):
;;;;   T_0 = AES_K2(DUN_le128)
;;;;   For j = 0..m-1:    C_j = AES_K1(P_j XOR T_j) XOR T_j
;;;;   where T_{j+1} = T_j * alpha in GF(2^128)
;;;;   reducing polynomial x^128 + x^7 + x^2 + x + 1 (i.e. byte 0 ^= 0x87
;;;;   when the high bit of byte 15 wraps).
;;;;
;;;; When the data unit length isn't a multiple of 16 bytes, ciphertext
;;;; stealing (CTS) is used so the output is the same length as the
;;;; input. This implementation requires len(plaintext) >= 16 (one full
;;;; block) per IEEE 1619, and accepts any byte length from 16 up.

(defpackage epsilon.crypto.aes-xts
  (:use :cl)
  (:import (epsilon.crypto.aes aes))
  (:export
   #:aes-xts-encrypt
   #:aes-xts-decrypt))

(in-package :epsilon.crypto.aes-xts)

(defconstant +block-size+ 16)

(defun %xor-block (a b)
  "Return a fresh 16-byte block A XOR B."
  (let ((out (make-array +block-size+ :element-type '(unsigned-byte 8))))
    (loop for i from 0 below +block-size+
          do (setf (aref out i) (logxor (aref a i) (aref b i))))
    out))

(defun %dun-to-tweak-input (dun)
  "Encode DUN (a non-negative integer < 2^128) as a 16-byte little-endian
   block, the input to AES_K2."
  (declare (type integer dun))
  (let ((b (make-array +block-size+ :element-type '(unsigned-byte 8)
                       :initial-element 0)))
    (loop for i from 0 below +block-size+
          for shift from 0 by 8
          do (setf (aref b i) (logand #xFF (ash dun (- shift)))))
    b))

(defun %multiply-tweak-by-alpha! (t-block)
  "T <- T * alpha in GF(2^128). The 128-bit polynomial is stored in
   T-BLOCK with byte 0 holding the low-order coefficients (bit 0 of
   byte 0 = constant term, bit 7 of byte 15 = x^127)."
  (declare (type (simple-array (unsigned-byte 8) (*)) t-block))
  (let ((carry 0))
    (loop for i from 0 below +block-size+
          for byte = (aref t-block i)
          for new-carry = (logand 1 (ash byte -7))
          do (setf (aref t-block i)
                   (logand #xFF (logior (ash byte 1) carry)))
             (setf carry new-carry))
    (when (= carry 1)
      ;; x^128 = x^7 + x^2 + x + 1 = 0x87 in the low byte.
      (setf (aref t-block 0) (logxor (aref t-block 0) #x87)))))

(defun %xex (round-keys block tweak)
  "One XEX step: AES_K(block XOR tweak) XOR tweak. Returns a fresh
   16-byte ciphertext block."
  (let ((tmp (%xor-block block tweak)))
    (%xor-block (aes:aes-encrypt-block tmp round-keys) tweak)))

(defun %xex-inv (decrypt-round-keys block tweak)
  "Inverse XEX step using the AES decrypt round-keys."
  (let ((tmp (%xor-block block tweak)))
    (%xor-block (aes:aes-decrypt-block tmp decrypt-round-keys) tweak)))

(defun %check-keys (key)
  "Validate the combined XTS key length and split into (K1, K2)."
  (let ((klen (length key)))
    (unless (or (= klen 32) (= klen 64))
      (error "AES-XTS: combined key length must be 32 (AES-128) or 64 ~
              (AES-256) bytes; got ~D" klen))
    (values (subseq key 0 (/ klen 2))
            (subseq key (/ klen 2) klen))))

(defun aes-xts-encrypt (plaintext key dun)
  "Encrypt PLAINTEXT under AES-XTS.

KEY is the concatenated XTS key (32 bytes for AES-128-XTS, 64 bytes
for AES-256-XTS). DUN is the data unit number (a non-negative integer
< 2^128). PLAINTEXT must be at least 16 bytes; lengths that are not
multiples of 16 trigger ciphertext stealing on the last two blocks.

Returns the ciphertext as a fresh byte vector of the same length as
PLAINTEXT."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext key)
           (type integer dun))
  (let ((len (length plaintext)))
    (when (< len +block-size+)
      (error "AES-XTS: plaintext must be >= 16 bytes (got ~D)" len))
    (multiple-value-bind (k1 k2) (%check-keys key)
      (let* ((rk1 (aes:make-aes-round-keys k1))
             (rk2 (aes:make-aes-round-keys k2))
             (tweak (aes:aes-encrypt-block (%dun-to-tweak-input dun) rk2))
             (full-blocks (floor len +block-size+))
             (rem (mod len +block-size+))
             (out (make-array len :element-type '(unsigned-byte 8))))
        (cond
          ;; No partial trailing block -- straight XEX over each block.
          ((zerop rem)
           (loop for j from 0 below full-blocks
                 for offset = (* j +block-size+)
                 for pj = (subseq plaintext offset (+ offset +block-size+))
                 for cj = (%xex rk1 pj tweak)
                 do (replace out cj :start1 offset)
                    (%multiply-tweak-by-alpha! tweak)))
          ;; Partial trailing block (size 1..15) -- use ciphertext
          ;; stealing on blocks (full-blocks-1) and the partial tail.
          (t
           (loop for j from 0 below (1- full-blocks)
                 for offset = (* j +block-size+)
                 for pj = (subseq plaintext offset (+ offset +block-size+))
                 for cj = (%xex rk1 pj tweak)
                 do (replace out cj :start1 offset)
                    (%multiply-tweak-by-alpha! tweak))
           ;; Now `tweak' corresponds to T_{m-1}.
           (let* ((m-1-offset (* (1- full-blocks) +block-size+))
                  (last-full (subseq plaintext m-1-offset
                                     (+ m-1-offset +block-size+)))
                  (cc (%xex rk1 last-full tweak))      ; tentative C_{m-1}
                  (partial-offset (* full-blocks +block-size+))
                  ;; Build the merged final block: partial plaintext
                  ;; concatenated with the tail of CC.
                  (cp (make-array +block-size+ :element-type '(unsigned-byte 8))))
             ;; First REM bytes of cp = partial plaintext.
             (replace cp plaintext
                      :start1 0 :end1 rem
                      :start2 partial-offset :end2 (+ partial-offset rem))
             ;; Remaining bytes of cp = tail of CC starting at byte REM.
             (replace cp cc
                      :start1 rem :end1 +block-size+
                      :start2 rem :end2 +block-size+)
             ;; Advance the tweak to T_m and process the merged block as
             ;; the new C_{m-1} of the output.
             (%multiply-tweak-by-alpha! tweak)
             (let ((new-cm-1 (%xex rk1 cp tweak)))
               (replace out new-cm-1 :start1 m-1-offset))
             ;; The trailing partial ciphertext is the first REM bytes of CC.
             (replace out cc :start1 partial-offset
                      :end1 (+ partial-offset rem)
                      :start2 0 :end2 rem))))
        out))))

(defun aes-xts-decrypt (ciphertext key dun)
  "Inverse of `aes-xts-encrypt'. Same key/DUN/length rules."
  (declare (type (simple-array (unsigned-byte 8) (*)) ciphertext key)
           (type integer dun))
  (let ((len (length ciphertext)))
    (when (< len +block-size+)
      (error "AES-XTS: ciphertext must be >= 16 bytes (got ~D)" len))
    (multiple-value-bind (k1 k2) (%check-keys key)
      (let* ((rk1 (aes:make-aes-round-keys k1))
             (drk1 (aes:make-aes-decrypt-round-keys k1))
             (rk2 (aes:make-aes-round-keys k2))
             (tweak (aes:aes-encrypt-block (%dun-to-tweak-input dun) rk2))
             (full-blocks (floor len +block-size+))
             (rem (mod len +block-size+))
             (out (make-array len :element-type '(unsigned-byte 8))))
        (declare (ignore rk1))
        (cond
          ((zerop rem)
           (loop for j from 0 below full-blocks
                 for offset = (* j +block-size+)
                 for cj = (subseq ciphertext offset (+ offset +block-size+))
                 for pj = (%xex-inv drk1 cj tweak)
                 do (replace out pj :start1 offset)
                    (%multiply-tweak-by-alpha! tweak)))
          (t
           (loop for j from 0 below (1- full-blocks)
                 for offset = (* j +block-size+)
                 for cj = (subseq ciphertext offset (+ offset +block-size+))
                 for pj = (%xex-inv drk1 cj tweak)
                 do (replace out pj :start1 offset)
                    (%multiply-tweak-by-alpha! tweak))
           ;; Stash T_{m-1}, advance to T_m for the merged block decrypt.
           (let* ((tweak-m-1 (copy-seq tweak)))
             (%multiply-tweak-by-alpha! tweak)
             (let* ((m-1-offset (* (1- full-blocks) +block-size+))
                    (cm-1 (subseq ciphertext m-1-offset
                                  (+ m-1-offset +block-size+)))
                    (pp (%xex-inv drk1 cm-1 tweak)) ; tentative full P_{m-1}
                    (partial-offset (* full-blocks +block-size+))
                    (merged (make-array +block-size+
                                        :element-type '(unsigned-byte 8))))
               ;; merged = partial-ciphertext || pp[rem..]
               (replace merged ciphertext
                        :start1 0 :end1 rem
                        :start2 partial-offset :end2 (+ partial-offset rem))
               (replace merged pp
                        :start1 rem :end1 +block-size+
                        :start2 rem :end2 +block-size+)
               ;; Decrypt merged with T_{m-1} to recover the actual P_{m-1}.
               (let ((real-pm-1 (%xex-inv drk1 merged tweak-m-1)))
                 (replace out real-pm-1 :start1 m-1-offset))
               ;; The partial plaintext tail = first REM bytes of pp.
               (replace out pp :start1 partial-offset
                        :end1 (+ partial-offset rem)
                        :start2 0 :end2 rem)))))
        out))))
