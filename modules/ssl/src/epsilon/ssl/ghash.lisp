;;;; GHASH (GF(2^128) multiplication for AES-GCM)
;;;;
;;;; Implements the GHASH universal hash function used in AES-GCM (NIST SP 800-38D).
;;;; Uses schoolbook multiplication in GF(2^128) with the irreducible polynomial
;;;; x^128 + x^7 + x^2 + x + 1.

(defpackage epsilon.ssl.ghash
  (:use :cl)
  (:import-from #:epsilon.ssl.primitives #:u64-to-bytes-be #:+u64-mask+)
  (:export
   #:ghash
   #:make-ghash-key))

(in-package :epsilon.ssl.ghash)

;;; ---------------------------------------------------------------------------
;;; GF(2^128) multiplication
;;; ---------------------------------------------------------------------------
;;; We represent 128-bit values as two 64-bit integers (hi, lo) where
;;; hi contains bits 127..64 and lo contains bits 63..0.
;;; The GCM polynomial is R = x^128 + x^7 + x^2 + x + 1, which means
;;; the reduction constant for the high bit is 0xe1000000_00000000 (big-endian).

(defconstant +ghash-r+ #xe100000000000000)

(defun bytes-to-u64-be (bytes offset)
  "Read 8 bytes from BYTES at OFFSET as a big-endian 64-bit integer."
  (logior (ash (aref bytes offset) 56)
          (ash (aref bytes (+ offset 1)) 48)
          (ash (aref bytes (+ offset 2)) 40)
          (ash (aref bytes (+ offset 3)) 32)
          (ash (aref bytes (+ offset 4)) 24)
          (ash (aref bytes (+ offset 5)) 16)
          (ash (aref bytes (+ offset 6)) 8)
          (aref bytes (+ offset 7))))

(defun gf128-mul (a-hi a-lo b-hi b-lo)
  "Multiply two GF(2^128) elements. Returns (values result-hi result-lo).
   Implements NIST SP 800-38D Algorithm 1.
   Bit 0 is MSB of first byte, bit 127 is LSB of last byte.
   R = 11100001 || 0^120 = 0xe100000000000000 in the high 64-bit word."
  (let ((z-hi 0) (z-lo 0)
        (v-hi a-hi) (v-lo a-lo))
    (flet ((shift-and-reduce ()
             ;; Right-shift V by 1. Bit 127 (LSB of v-lo) falls off.
             ;; If it was 1, XOR R into V after shifting.
             (let ((carry (logand v-lo 1)))
               ;; Shift right: MSB of v-lo gets LSB of v-hi
               (setf v-lo (logand +u64-mask+
                                  (logior (ash v-lo -1)
                                          (ash (logand v-hi 1) 63))))
               (setf v-hi (logand +u64-mask+ (ash v-hi -1)))
               (when (= carry 1)
                 (setf v-hi (logxor v-hi +ghash-r+))))))
      ;; Process b-hi (GCM bits 0..63 = bit 63 downto 0 of b-hi)
      (loop for i from 63 downto 0
            do (when (logbitp i b-hi)
                 (setf z-hi (logxor z-hi v-hi))
                 (setf z-lo (logxor z-lo v-lo)))
               (shift-and-reduce))
      ;; Process b-lo (GCM bits 64..127 = bit 63 downto 0 of b-lo)
      (loop for i from 63 downto 0
            do (when (logbitp i b-lo)
                 (setf z-hi (logxor z-hi v-hi))
                 (setf z-lo (logxor z-lo v-lo)))
               (shift-and-reduce)))
    (values z-hi z-lo)))

;;; ---------------------------------------------------------------------------
;;; GHASH function
;;; ---------------------------------------------------------------------------

(defstruct (ghash-key (:constructor %make-ghash-key))
  "Precomputed GHASH key (the hash key H)."
  (h-hi 0 :type integer)
  (h-lo 0 :type integer))

(defun make-ghash-key (h-bytes)
  "Create a GHASH key from a 16-byte hash subkey H (= AES_K(0^128))."
  (declare (type (simple-array (unsigned-byte 8) (*)) h-bytes))
  (%make-ghash-key :h-hi (bytes-to-u64-be h-bytes 0)
                   :h-lo (bytes-to-u64-be h-bytes 8)))

(defun ghash (key data)
  "Compute GHASH_H(data) where H is the hash key.
   DATA must be a multiple of 16 bytes.
   Returns a 16-byte result."
  (let ((h-hi (ghash-key-h-hi key))
        (h-lo (ghash-key-h-lo key))
        (x-hi 0) (x-lo 0)
        (len (length data)))
    ;; Process each 16-byte block
    (loop for offset from 0 below len by 16
          do (let ((d-hi (bytes-to-u64-be data offset))
                   (d-lo (bytes-to-u64-be data (+ offset 8))))
               ;; X = (X xor D) * H
               (setf x-hi (logxor x-hi d-hi))
               (setf x-lo (logxor x-lo d-lo))
               (multiple-value-setq (x-hi x-lo) (gf128-mul x-hi x-lo h-hi h-lo))))
    ;; Convert result to bytes
    (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
      (u64-to-bytes-be x-hi result 0)
      (u64-to-bytes-be x-lo result 8)
      result)))
