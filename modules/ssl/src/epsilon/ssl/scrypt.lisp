;;;; scrypt (RFC 7914)
;;;;
;;;; Memory-hard password-based key derivation function.
;;;; Uses Salsa20/8 core, BlockMix, and ROMix.

(defpackage epsilon.ssl.scrypt
  (:use :cl)
  (:local-nicknames
   (#:pbkdf2 #:epsilon.ssl.pbkdf2)
   (#:prim #:epsilon.ssl.primitives))
  (:import-from #:epsilon.ssl.primitives #:u32+ #:u32-rotl)
  (:export
   #:scrypt))

(in-package :epsilon.ssl.scrypt)

(defun salsa20-8 (b)
  "Apply the Salsa20/8 core to a 64-byte block B (in-place).
   B is a (simple-array (unsigned-byte 8) (*)) of at least 64 bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) b))
  ;; Read 16 little-endian 32-bit words
  (let ((x (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
    (loop for i from 0 below 16
          for base = (* i 4)
          do (setf (aref x i)
                   (logior (aref b base)
                           (ash (aref b (+ base 1)) 8)
                           (ash (aref b (+ base 2)) 16)
                           (ash (aref b (+ base 3)) 24))))
    (let ((w (copy-seq x)))
      ;; 8 rounds (4 double-rounds)
      (loop repeat 4
            do ;; Column round
               (setf (aref w  4) (logxor (aref w  4) (u32-rotl (u32+ (aref w  0) (aref w 12))  7)))
               (setf (aref w  8) (logxor (aref w  8) (u32-rotl (u32+ (aref w  4) (aref w  0))  9)))
               (setf (aref w 12) (logxor (aref w 12) (u32-rotl (u32+ (aref w  8) (aref w  4)) 13)))
               (setf (aref w  0) (logxor (aref w  0) (u32-rotl (u32+ (aref w 12) (aref w  8)) 18)))

               (setf (aref w  9) (logxor (aref w  9) (u32-rotl (u32+ (aref w  5) (aref w  1))  7)))
               (setf (aref w 13) (logxor (aref w 13) (u32-rotl (u32+ (aref w  9) (aref w  5))  9)))
               (setf (aref w  1) (logxor (aref w  1) (u32-rotl (u32+ (aref w 13) (aref w  9)) 13)))
               (setf (aref w  5) (logxor (aref w  5) (u32-rotl (u32+ (aref w  1) (aref w 13)) 18)))

               (setf (aref w 14) (logxor (aref w 14) (u32-rotl (u32+ (aref w 10) (aref w  6))  7)))
               (setf (aref w  2) (logxor (aref w  2) (u32-rotl (u32+ (aref w 14) (aref w 10))  9)))
               (setf (aref w  6) (logxor (aref w  6) (u32-rotl (u32+ (aref w  2) (aref w 14)) 13)))
               (setf (aref w 10) (logxor (aref w 10) (u32-rotl (u32+ (aref w  6) (aref w  2)) 18)))

               (setf (aref w  3) (logxor (aref w  3) (u32-rotl (u32+ (aref w 15) (aref w 11))  7)))
               (setf (aref w  7) (logxor (aref w  7) (u32-rotl (u32+ (aref w  3) (aref w 15))  9)))
               (setf (aref w 11) (logxor (aref w 11) (u32-rotl (u32+ (aref w  7) (aref w  3)) 13)))
               (setf (aref w 15) (logxor (aref w 15) (u32-rotl (u32+ (aref w 11) (aref w  7)) 18)))

               ;; Row round
               (setf (aref w  1) (logxor (aref w  1) (u32-rotl (u32+ (aref w  0) (aref w  3))  7)))
               (setf (aref w  2) (logxor (aref w  2) (u32-rotl (u32+ (aref w  1) (aref w  0))  9)))
               (setf (aref w  3) (logxor (aref w  3) (u32-rotl (u32+ (aref w  2) (aref w  1)) 13)))
               (setf (aref w  0) (logxor (aref w  0) (u32-rotl (u32+ (aref w  3) (aref w  2)) 18)))

               (setf (aref w  6) (logxor (aref w  6) (u32-rotl (u32+ (aref w  5) (aref w  4))  7)))
               (setf (aref w  7) (logxor (aref w  7) (u32-rotl (u32+ (aref w  6) (aref w  5))  9)))
               (setf (aref w  4) (logxor (aref w  4) (u32-rotl (u32+ (aref w  7) (aref w  6)) 13)))
               (setf (aref w  5) (logxor (aref w  5) (u32-rotl (u32+ (aref w  4) (aref w  7)) 18)))

               (setf (aref w 11) (logxor (aref w 11) (u32-rotl (u32+ (aref w 10) (aref w  9))  7)))
               (setf (aref w  8) (logxor (aref w  8) (u32-rotl (u32+ (aref w 11) (aref w 10))  9)))
               (setf (aref w  9) (logxor (aref w  9) (u32-rotl (u32+ (aref w  8) (aref w 11)) 13)))
               (setf (aref w 10) (logxor (aref w 10) (u32-rotl (u32+ (aref w  9) (aref w  8)) 18)))

               (setf (aref w 12) (logxor (aref w 12) (u32-rotl (u32+ (aref w 15) (aref w 14))  7)))
               (setf (aref w 13) (logxor (aref w 13) (u32-rotl (u32+ (aref w 12) (aref w 15))  9)))
               (setf (aref w 14) (logxor (aref w 14) (u32-rotl (u32+ (aref w 13) (aref w 12)) 13)))
               (setf (aref w 15) (logxor (aref w 15) (u32-rotl (u32+ (aref w 14) (aref w 13)) 18))))
      ;; Add input to output
      (loop for i from 0 below 16
            do (setf (aref x i) (u32+ (aref x i) (aref w i))))
      ;; Write back as little-endian bytes
      (loop for i from 0 below 16
            for base = (* i 4)
            for word = (aref x i)
            do (setf (aref b base) (logand #xFF word))
               (setf (aref b (+ base 1)) (logand #xFF (ash word -8)))
               (setf (aref b (+ base 2)) (logand #xFF (ash word -16)))
               (setf (aref b (+ base 3)) (logand #xFF (ash word -24)))))))

;;; ---------------------------------------------------------------------------
;;; BlockMix (RFC 7914 Section 4)
;;; ---------------------------------------------------------------------------

(defun xor-block-64 (dst src dst-offset src-offset)
  "XOR 64 bytes from SRC at SRC-OFFSET into DST at DST-OFFSET."
  (loop for i from 0 below 64
        do (setf (aref dst (+ dst-offset i))
                 (logxor (aref dst (+ dst-offset i))
                         (aref src (+ src-offset i))))))

(defun scrypt-block-mix (b r)
  "Apply scryptBlockMix to B (a byte array of 128*r bytes).
   Returns a new byte array of the same length."
  (let* ((block-size (* 128 r))
         (num-blocks (* 2 r))
         ;; X = B[2r-1] (last 64-byte block)
         (x (make-array 64 :element-type '(unsigned-byte 8)))
         (y (make-array block-size :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Initialize X from last block
    (replace x b :start2 (* (1- num-blocks) 64) :end2 (* num-blocks 64))
    ;; Process each block
    (loop for i from 0 below num-blocks
          for block-offset = (* i 64)
          do ;; X = X XOR B[i]
             (xor-block-64 x b 0 block-offset)
             ;; X = Salsa20/8(X)
             (salsa20-8 x)
             ;; Y[i] = X
             ;; But output order is: even blocks first, then odd blocks
             (let ((out-offset (if (evenp i)
                                   (* (/ i 2) 64)
                                   (* (+ (/ (1- i) 2) r) 64))))
               (replace y x :start1 out-offset :end1 (+ out-offset 64))))
    y))

;;; ---------------------------------------------------------------------------
;;; ROMix (RFC 7914 Section 5)
;;; ---------------------------------------------------------------------------

(defun integerify (b r)
  "Extract the last 64 bytes of B as a little-endian integer (mod N).
   Per RFC 7914 Section 5, we take the first 8 bytes of the last 64-byte block."
  ;; The last 64-byte block starts at offset (2r-1)*64
  (let* ((offset (* (1- (* 2 r)) 64)))
    ;; Read first 8 bytes (64-bit LE integer)
    (logior (aref b offset)
            (ash (aref b (+ offset 1)) 8)
            (ash (aref b (+ offset 2)) 16)
            (ash (aref b (+ offset 3)) 24)
            (ash (aref b (+ offset 4)) 32)
            (ash (aref b (+ offset 5)) 40)
            (ash (aref b (+ offset 6)) 48)
            (ash (aref b (+ offset 7)) 56))))

(defun xor-arrays (a b len)
  "XOR first LEN bytes of A and B, returning a new array."
  (let ((result (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          do (setf (aref result i) (logxor (aref a i) (aref b i))))
    result))

(defun scrypt-romix (b r n)
  "Apply scryptROMix to B (a byte array of 128*r bytes).
   N must be a power of 2."
  (let* ((block-size (* 128 r))
         (x (copy-seq b))
         ;; V is a vector of N blocks, each 128*r bytes
         (v (make-array n)))
    ;; Step 1: Build the ROM
    (loop for i from 0 below n
          do (setf (aref v i) (copy-seq x))
             (setf x (scrypt-block-mix x r)))
    ;; Step 2: Mix using ROM
    (loop for i from 0 below n
          do (let ((j (mod (integerify x r) n)))
               (setf x (scrypt-block-mix (xor-arrays x (aref v j) block-size) r))))
    x))

;;; ---------------------------------------------------------------------------
;;; scrypt (RFC 7914 Section 6)
;;; ---------------------------------------------------------------------------

(defun scrypt (password salt n r p dk-len)
  "Derive DK-LEN bytes using scrypt.
   PASSWORD and SALT are byte arrays.
   N is the CPU/memory cost parameter (power of 2).
   R is the block size parameter.
   P is the parallelization parameter.
   DK-LEN is the desired key length."
  (declare (type (simple-array (unsigned-byte 8) (*)) password salt)
           (type (integer 1) n r p dk-len))
  ;; Step 1: Generate initial blocks using PBKDF2-SHA256
  (let* ((block-size (* 128 r p))
         (b (pbkdf2:pbkdf2 :sha256 password salt 1 block-size)))
    ;; Step 2: Apply ROMix to each p-sized block
    (loop for i from 0 below p
          for offset = (* i 128 r)
          do (let* ((block-i (subseq b offset (+ offset (* 128 r))))
                    (mixed (scrypt-romix block-i r n)))
               (replace b mixed :start1 offset)))
    ;; Step 3: Derive output using PBKDF2-SHA256
    (pbkdf2:pbkdf2 :sha256 password b 1 dk-len)))
