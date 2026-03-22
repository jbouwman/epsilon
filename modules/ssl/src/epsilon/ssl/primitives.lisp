;;;; Shared bit-manipulation primitives for crypto implementations
;;;;
;;;; Consolidates duplicated 32-bit and 64-bit arithmetic helpers used
;;;; across SHA-256, MD5, BLAKE3, BLAKE2, ChaCha20, and xxHash (IMPL-244).
;;;; All functions are declared inline for hot-path performance.

(defpackage epsilon.ssl.primitives
  (:use :cl)
  (:export
   ;; Constants
   #:+u32-mask+
   #:+u64-mask+
   ;; 32-bit arithmetic
   #:u32+
   #:u32*
   #:u32-rotl
   #:u32-rotr
   #:u32-shr
   #:u32-xor
   #:u32-and
   #:u32-not
   #:read-le32
   ;; 64-bit arithmetic
   #:u64+
   #:u64*
   #:u64-rotl
   #:u64-rotr
   #:u64-shr
   #:u64-to-bytes-be
   #:read-le64))

(in-package :epsilon.ssl.primitives)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconstant +u32-mask+ #xFFFFFFFF)
(defconstant +u64-mask+ #xFFFFFFFFFFFFFFFF)

;;; ---------------------------------------------------------------------------
;;; 32-bit arithmetic
;;; ---------------------------------------------------------------------------

(declaim (inline u32+ u32* u32-rotl u32-rotr u32-shr u32-xor u32-and u32-not read-le32))

(defun u32+ (&rest args)
  "Add multiple values modulo 2^32."
  (logand +u32-mask+ (apply #'+ args)))

(defun u32* (a b)
  "Multiply two values modulo 2^32."
  (logand +u32-mask+ (* a b)))

(defun u32-rotl (x n)
  "Left-rotate a 32-bit value X by N bits."
  (declare (type (unsigned-byte 32) x)
           (type (integer 0 31) n))
  (logand +u32-mask+
          (logior (ash x n)
                  (ash x (- n 32)))))

(defun u32-rotr (x n)
  "Right-rotate a 32-bit value X by N bits."
  (declare (type (unsigned-byte 32) x)
           (type (integer 0 31) n))
  (logand +u32-mask+
          (logior (ash x (- n))
                  (ash x (- 32 n)))))

(defun u32-shr (x n)
  "Right-shift a 32-bit value X by N bits."
  (declare (type (unsigned-byte 32) x)
           (type (integer 0 31) n))
  (ash x (- n)))

(defun u32-xor (&rest args)
  "XOR multiple 32-bit values."
  (logand +u32-mask+ (apply #'logxor args)))

(defun u32-and (a b)
  "AND two 32-bit values."
  (logand a b))

(defun u32-not (x)
  "Bitwise NOT of a 32-bit value."
  (logand +u32-mask+ (lognot x)))

(defun read-le32 (buf offset)
  "Read a little-endian 32-bit value from BUF at OFFSET."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum offset)
           (optimize (speed 3) (safety 1)))
  (logior (aref buf offset)
          (ash (aref buf (+ offset 1)) 8)
          (ash (aref buf (+ offset 2)) 16)
          (ash (aref buf (+ offset 3)) 24)))

;;; ---------------------------------------------------------------------------
;;; 64-bit arithmetic
;;; ---------------------------------------------------------------------------

(declaim (inline u64+ u64* u64-rotl u64-rotr u64-shr u64-to-bytes-be read-le64))

(defun u64+ (&rest args)
  "Add multiple values modulo 2^64."
  (logand +u64-mask+ (apply #'+ args)))

(defun u64* (a b)
  "Multiply two values modulo 2^64."
  (logand +u64-mask+ (* a b)))

(defun u64-rotl (x n)
  "Left-rotate a 64-bit value X by N bits."
  (declare (type (unsigned-byte 64) x)
           (type (integer 0 63) n))
  (logand +u64-mask+
          (logior (ash x n)
                  (ash x (- n 64)))))

(defun u64-rotr (x n)
  "Right-rotate a 64-bit value X by N bits."
  (declare (type (unsigned-byte 64) x)
           (type (integer 0 63) n))
  (logand +u64-mask+
          (logior (ash x (- n))
                  (ash x (- 64 n)))))

(defun u64-shr (x n)
  "Right-shift a 64-bit value X by N bits."
  (declare (type (unsigned-byte 64) x)
           (type (integer 0 63) n))
  (ash x (- n)))

(defun u64-to-bytes-be (val bytes offset)
  "Write a 64-bit integer VAL as 8 big-endian bytes into BYTES at OFFSET."
  (setf (aref bytes offset)       (logand #xFF (ash val -56)))
  (setf (aref bytes (+ offset 1)) (logand #xFF (ash val -48)))
  (setf (aref bytes (+ offset 2)) (logand #xFF (ash val -40)))
  (setf (aref bytes (+ offset 3)) (logand #xFF (ash val -32)))
  (setf (aref bytes (+ offset 4)) (logand #xFF (ash val -24)))
  (setf (aref bytes (+ offset 5)) (logand #xFF (ash val -16)))
  (setf (aref bytes (+ offset 6)) (logand #xFF (ash val -8)))
  (setf (aref bytes (+ offset 7)) (logand #xFF val)))

(defun read-le64 (buf offset)
  "Read a little-endian 64-bit value from BUF at OFFSET."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum offset)
           (optimize (speed 3) (safety 1)))
  (logior (aref buf offset)
          (ash (aref buf (+ offset 1)) 8)
          (ash (aref buf (+ offset 2)) 16)
          (ash (aref buf (+ offset 3)) 24)
          (ash (aref buf (+ offset 4)) 32)
          (ash (aref buf (+ offset 5)) 40)
          (ash (aref buf (+ offset 6)) 48)
          (ash (aref buf (+ offset 7)) 56)))
