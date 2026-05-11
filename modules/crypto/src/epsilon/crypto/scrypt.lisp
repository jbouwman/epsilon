;;;; scrypt (RFC 7914)
;;;;
;;;; Memory-hard password-based key derivation function.
;;;; Uses Salsa20/8 core, BlockMix, and ROMix.

(defpackage epsilon.crypto.scrypt
  (:use :cl)
  (:import
   (epsilon.crypto.pbkdf2 pbkdf2)
   (epsilon.crypto.primitives prim))
  (:import-from #:epsilon.crypto.primitives #:u32+ #:u32-rotl)
  (:export
   #:scrypt))

(in-package :epsilon.crypto.scrypt)

;;; ---------------------------------------------------------------------------
;;; Salsa20/8 core — operates on a u32 scratch array to avoid byte conversion
;;; ---------------------------------------------------------------------------

(declaim (inline %le32-ref %le32-set))

(defun %le32-ref (b i)
  "Read little-endian u32 from byte array B at byte offset I."
  (declare (type (simple-array (unsigned-byte 8) (*)) b)
           (type fixnum i)
           (optimize (speed 3) (safety 0)))
  (logior (aref b i)
          (ash (aref b (+ i 1)) 8)
          (ash (aref b (+ i 2)) 16)
          (ash (aref b (+ i 3)) 24)))

(defun %le32-set (b i val)
  "Write little-endian u32 to byte array B at byte offset I."
  (declare (type (simple-array (unsigned-byte 8) (*)) b)
           (type fixnum i)
           (type (unsigned-byte 32) val)
           (optimize (speed 3) (safety 0)))
  (setf (aref b i) (logand val #xFF)
        (aref b (+ i 1)) (logand (ash val -8) #xFF)
        (aref b (+ i 2)) (logand (ash val -16) #xFF)
        (aref b (+ i 3)) (logand (ash val -24) #xFF)))

(defun salsa20-8 (b offset)
  "Apply the Salsa20/8 core to 64 bytes at B+OFFSET (in-place)."
  (declare (type (simple-array (unsigned-byte 8) (*)) b)
           (type fixnum offset)
           (optimize (speed 3) (safety 0)))
  (let ((x0  (%le32-ref b (+ offset  0))) (x1  (%le32-ref b (+ offset  4)))
        (x2  (%le32-ref b (+ offset  8))) (x3  (%le32-ref b (+ offset 12)))
        (x4  (%le32-ref b (+ offset 16))) (x5  (%le32-ref b (+ offset 20)))
        (x6  (%le32-ref b (+ offset 24))) (x7  (%le32-ref b (+ offset 28)))
        (x8  (%le32-ref b (+ offset 32))) (x9  (%le32-ref b (+ offset 36)))
        (x10 (%le32-ref b (+ offset 40))) (x11 (%le32-ref b (+ offset 44)))
        (x12 (%le32-ref b (+ offset 48))) (x13 (%le32-ref b (+ offset 52)))
        (x14 (%le32-ref b (+ offset 56))) (x15 (%le32-ref b (+ offset 60))))
    (declare (type (unsigned-byte 32) x0 x1 x2 x3 x4 x5 x6 x7
                                      x8 x9 x10 x11 x12 x13 x14 x15))
    (let ((w0 x0) (w1 x1) (w2 x2) (w3 x3) (w4 x4) (w5 x5) (w6 x6) (w7 x7)
          (w8 x8) (w9 x9) (w10 x10) (w11 x11) (w12 x12) (w13 x13) (w14 x14) (w15 x15))
      (declare (type (unsigned-byte 32) w0 w1 w2 w3 w4 w5 w6 w7
                                        w8 w9 w10 w11 w12 w13 w14 w15))
      (loop repeat 4 do
        ;; Column round
        (setf w4  (logxor w4  (u32-rotl (u32+ w0  w12)  7)))
        (setf w8  (logxor w8  (u32-rotl (u32+ w4  w0)   9)))
        (setf w12 (logxor w12 (u32-rotl (u32+ w8  w4)  13)))
        (setf w0  (logxor w0  (u32-rotl (u32+ w12 w8)  18)))
        (setf w9  (logxor w9  (u32-rotl (u32+ w5  w1)   7)))
        (setf w13 (logxor w13 (u32-rotl (u32+ w9  w5)   9)))
        (setf w1  (logxor w1  (u32-rotl (u32+ w13 w9)  13)))
        (setf w5  (logxor w5  (u32-rotl (u32+ w1  w13) 18)))
        (setf w14 (logxor w14 (u32-rotl (u32+ w10 w6)   7)))
        (setf w2  (logxor w2  (u32-rotl (u32+ w14 w10)  9)))
        (setf w6  (logxor w6  (u32-rotl (u32+ w2  w14) 13)))
        (setf w10 (logxor w10 (u32-rotl (u32+ w6  w2)  18)))
        (setf w3  (logxor w3  (u32-rotl (u32+ w15 w11)  7)))
        (setf w7  (logxor w7  (u32-rotl (u32+ w3  w15)  9)))
        (setf w11 (logxor w11 (u32-rotl (u32+ w7  w3)  13)))
        (setf w15 (logxor w15 (u32-rotl (u32+ w11 w7)  18)))
        ;; Row round
        (setf w1  (logxor w1  (u32-rotl (u32+ w0  w3)   7)))
        (setf w2  (logxor w2  (u32-rotl (u32+ w1  w0)   9)))
        (setf w3  (logxor w3  (u32-rotl (u32+ w2  w1)  13)))
        (setf w0  (logxor w0  (u32-rotl (u32+ w3  w2)  18)))
        (setf w6  (logxor w6  (u32-rotl (u32+ w5  w4)   7)))
        (setf w7  (logxor w7  (u32-rotl (u32+ w6  w5)   9)))
        (setf w4  (logxor w4  (u32-rotl (u32+ w7  w6)  13)))
        (setf w5  (logxor w5  (u32-rotl (u32+ w4  w7)  18)))
        (setf w11 (logxor w11 (u32-rotl (u32+ w10 w9)   7)))
        (setf w8  (logxor w8  (u32-rotl (u32+ w11 w10)  9)))
        (setf w9  (logxor w9  (u32-rotl (u32+ w8  w11) 13)))
        (setf w10 (logxor w10 (u32-rotl (u32+ w9  w8)  18)))
        (setf w12 (logxor w12 (u32-rotl (u32+ w15 w14)  7)))
        (setf w13 (logxor w13 (u32-rotl (u32+ w12 w15)  9)))
        (setf w14 (logxor w14 (u32-rotl (u32+ w13 w12) 13)))
        (setf w15 (logxor w15 (u32-rotl (u32+ w14 w13) 18))))
      ;; Add input and write back
      (%le32-set b (+ offset  0) (u32+ x0  w0))  (%le32-set b (+ offset  4) (u32+ x1  w1))
      (%le32-set b (+ offset  8) (u32+ x2  w2))  (%le32-set b (+ offset 12) (u32+ x3  w3))
      (%le32-set b (+ offset 16) (u32+ x4  w4))  (%le32-set b (+ offset 20) (u32+ x5  w5))
      (%le32-set b (+ offset 24) (u32+ x6  w6))  (%le32-set b (+ offset 28) (u32+ x7  w7))
      (%le32-set b (+ offset 32) (u32+ x8  w8))  (%le32-set b (+ offset 36) (u32+ x9  w9))
      (%le32-set b (+ offset 40) (u32+ x10 w10)) (%le32-set b (+ offset 44) (u32+ x11 w11))
      (%le32-set b (+ offset 48) (u32+ x12 w12)) (%le32-set b (+ offset 52) (u32+ x13 w13))
      (%le32-set b (+ offset 56) (u32+ x14 w14)) (%le32-set b (+ offset 60) (u32+ x15 w15)))))

;;; ---------------------------------------------------------------------------
;;; BlockMix — operates in-place using pre-allocated scratch buffers
;;; ---------------------------------------------------------------------------

(defun %xor-64 (dst dst-off src src-off)
  "XOR 64 bytes: dst[dst-off..] ^= src[src-off..]"
  (declare (type (simple-array (unsigned-byte 8) (*)) dst src)
           (type fixnum dst-off src-off)
           (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below 64
        do (setf (aref dst (+ dst-off i))
                 (logxor (aref dst (+ dst-off i))
                         (aref src (+ src-off i))))))

(defun %block-mix (b r x y)
  "BlockMix in-place: reads from B, writes result to Y, uses X as 64-byte scratch.
   B and Y are both 128*r bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) b x y)
           (type fixnum r)
           (optimize (speed 3) (safety 0)))
  (let ((num-blocks (* 2 r)))
    ;; X = B[2r-1]
    (replace x b :start2 (* (1- num-blocks) 64) :end2 (* num-blocks 64))
    ;; Process each block
    (loop for i fixnum from 0 below num-blocks
          for block-off fixnum = (* i 64)
          do ;; X ^= B[i]
             (%xor-64 x 0 b block-off)
             ;; Salsa20/8(X) in-place
             (salsa20-8 x 0)
             ;; Write to Y with interleaved output: evens first, then odds
             (let ((out-off (if (evenp i)
                                (* (ash i -1) 64)
                                (* (+ (ash (1- i) -1) r) 64))))
               (replace y x :start1 out-off :end1 (+ out-off 64))))))

;;; ---------------------------------------------------------------------------
;;; ROMix — pre-allocates all buffers
;;; ---------------------------------------------------------------------------

(defun %integerify (b r)
  "Extract low 32 bits of last 64-byte block of B (LE). Callers mask with (1- N),
   so only the low log2(N) bits matter; 32 is plenty for any practical N and
   stays within fixnum range on 64-bit SBCL (no bignum consing)."
  (declare (type (simple-array (unsigned-byte 8) (*)) b)
           (type fixnum r)
           (optimize (speed 3) (safety 0)))
  (let ((off (* (1- (* 2 r)) 64)))
    (declare (type fixnum off))
    (logior (aref b off)
            (ash (aref b (+ off 1)) 8)
            (ash (aref b (+ off 2)) 16)
            (ash (aref b (+ off 3)) 24))))

(defun %xor-range (dst src src-off len)
  "dst[0..len] ^= src[src-off..src-off+len]"
  (declare (type (simple-array (unsigned-byte 8) (*)) dst src)
           (type fixnum src-off len)
           (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below len
        do (setf (aref dst i) (logxor (aref dst i) (aref src (+ src-off i))))))

(defun scrypt-romix (b r n)
  "ROMix.  B is 128*r bytes.  N must be a power of 2.  Returns new array.
   V is allocated as one contiguous byte buffer (N * 128 * r bytes) rather than
   N separate vectors, to keep GC pressure manageable at large N."
  (declare (type fixnum r n)
           (type (simple-array (unsigned-byte 8) (*)) b)
           (optimize (speed 3) (safety 1)))
  (let* ((block-size (the fixnum (* 128 r)))
         (x (copy-seq b))
         (y (make-array block-size :element-type '(unsigned-byte 8)))
         (scratch (make-array 64 :element-type '(unsigned-byte 8)))
         (v (make-array (the fixnum (* n block-size))
                        :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) x y scratch v))
    ;; Phase 1: build ROM
    (loop for i fixnum from 0 below n
          for off fixnum = 0 then (+ off block-size)
          do (replace v x :start1 off :end1 (+ off block-size))
             (%block-mix x r scratch y)
             (rotatef x y))
    ;; Phase 2: mix from ROM
    (loop for i fixnum from 0 below n
          do (let* ((j (logand (%integerify x r) (1- n)))
                    (v-off (the fixnum (* j block-size))))
               (declare (type fixnum j v-off))
               (%xor-range x v v-off block-size)
               (%block-mix x r scratch y)
               (rotatef x y)))
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
  (let* ((block-size (* 128 r p))
         (b (pbkdf2:pbkdf2 :sha256 password salt 1 block-size)))
    (loop for i from 0 below p
          for offset = (* i 128 r)
          do (let* ((block-i (subseq b offset (+ offset (* 128 r))))
                    (mixed (scrypt-romix block-i r n)))
               (replace b mixed :start1 offset)))
    (pbkdf2:pbkdf2 :sha256 password b 1 dk-len)))
