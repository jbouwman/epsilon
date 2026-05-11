;;;; Argon2id password hashing -- RFC 9106
;;;;
;;;; Pure-Lisp implementation of the Argon2id variant only. Argon2d
;;;; (data-dependent) and Argon2i (data-independent) are not exposed at
;;;; the top level; the indexing logic supports both because Argon2id
;;;; is defined as Argon2i for the first half of the first pass and
;;;; Argon2d everywhere else.
;;;;
;;;; Reference: RFC 9106, "Argon2 Memory-Hard Function for Password
;;;; Hashing and Proof-of-Work Applications" (September 2021).

(defpackage epsilon.crypto.argon2
  (:use :cl)
  (:import (epsilon.crypto.blake2 b2))
  (:export
   #:argon2id
   #:+argon2-version+
   ;; Exported for testing only.
   #:%h-prime
   #:%compute-h0
   #:%h0-input-blob
   #:%compress-g
   #:%bytes-to-block
   #:%block-to-bytes
   #:%zero-block))

(in-package :epsilon.crypto.argon2)

(defconstant +argon2-version+ #x13
  "Argon2 version number 1.3 (the only version RFC 9106 standardises).")

(defconstant +argon2id-type+ 2
  "Argon2 type identifier for Argon2id (RFC 9106 §3.1).")

(defconstant +block-size+ 1024
  "Argon2 memory block size in bytes.")

(defconstant +block-words+ 128
  "Argon2 memory block size in 64-bit words (1024 / 8).")

(defconstant +sync-points+ 4
  "Number of vertical slices each lane is divided into per pass (RFC 9106 §3.4).")

(deftype u64 () '(unsigned-byte 64))
(deftype u64-vector () '(simple-array (unsigned-byte 64) (*)))

;;; ---------------------------------------------------------------------------
;;; Little-endian byte / word helpers
;;; ---------------------------------------------------------------------------

(declaim (inline u32-to-le u64-to-le))

(defun u32-to-le (n)
  "Encode a 32-bit unsigned integer as 4 little-endian bytes."
  (let ((out (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref out 0) (logand n #xff))
    (setf (aref out 1) (logand (ash n -8) #xff))
    (setf (aref out 2) (logand (ash n -16) #xff))
    (setf (aref out 3) (logand (ash n -24) #xff))
    out))

(defun bytes-to-block (bytes)
  "Decode a 1024-byte BYTES vector into a fresh u64-vector of 128 words
   in little-endian order. (RFC 9106 §3.6 treats blocks as 128-word
   little-endian arrays.)"
  (let ((block (make-array +block-words+ :element-type '(unsigned-byte 64)
                                         :initial-element 0)))
    (loop for i from 0 below +block-words+
          for off = (* i 8)
          do (setf (aref block i)
                   (logior (aref bytes off)
                           (ash (aref bytes (+ off 1)) 8)
                           (ash (aref bytes (+ off 2)) 16)
                           (ash (aref bytes (+ off 3)) 24)
                           (ash (aref bytes (+ off 4)) 32)
                           (ash (aref bytes (+ off 5)) 40)
                           (ash (aref bytes (+ off 6)) 48)
                           (ash (aref bytes (+ off 7)) 56))))
    block))

(defun block-to-bytes (block)
  "Encode a 128-word u64-vector as 1024 little-endian bytes."
  (let ((out (make-array +block-size+ :element-type '(unsigned-byte 8))))
    (loop for i from 0 below +block-words+
          for off = (* i 8)
          for w = (aref block i)
          do (setf (aref out off)        (logand w #xff))
             (setf (aref out (+ off 1)) (logand (ash w -8) #xff))
             (setf (aref out (+ off 2)) (logand (ash w -16) #xff))
             (setf (aref out (+ off 3)) (logand (ash w -24) #xff))
             (setf (aref out (+ off 4)) (logand (ash w -32) #xff))
             (setf (aref out (+ off 5)) (logand (ash w -40) #xff))
             (setf (aref out (+ off 6)) (logand (ash w -48) #xff))
             (setf (aref out (+ off 7)) (logand (ash w -56) #xff)))
    out))

(defun zero-block ()
  (make-array +block-words+ :element-type '(unsigned-byte 64) :initial-element 0))

;; Test-visible aliases for the internal helpers.
(declaim (inline %zero-block %bytes-to-block %block-to-bytes))
(defun %zero-block () (zero-block))
(defun %bytes-to-block (b) (bytes-to-block b))
(defun %block-to-bytes (b) (block-to-bytes b))

(defun copy-block (b)
  (let ((out (make-array +block-words+ :element-type '(unsigned-byte 64))))
    (replace out b)
    out))

(defun xor-block-into (dest src)
  "DEST <- DEST XOR SRC, both 128-word u64-vectors."
  (declare (type u64-vector dest src))
  (loop for i from 0 below +block-words+
        do (setf (aref dest i) (logxor (aref dest i) (aref src i))))
  dest)

;;; ---------------------------------------------------------------------------
;;; Variable-length hash H' (RFC 9106 §3.2)
;;; ---------------------------------------------------------------------------

(defun %h-prime (input tag-length) (h-prime input tag-length))
(defun %compute-h0 (password salt secret ad parallelism tag-length memory-kb iterations)
  (compute-h0 password salt secret ad parallelism tag-length memory-kb iterations
              +argon2id-type+))
(defun %h0-input-blob (password salt secret ad parallelism tag-length memory-kb iterations)
  "Return the byte vector that would be hashed by Blake2b to produce H0.
   Exposed for tests so the H0 input encoding can be pinned independently
   of the Blake2b call."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (u32-to-le parallelism)
               (u32-to-le tag-length)
               (u32-to-le memory-kb)
               (u32-to-le iterations)
               (u32-to-le +argon2-version+)
               (u32-to-le +argon2id-type+)
               (length-prefixed password)
               (length-prefixed salt)
               (length-prefixed secret)
               (length-prefixed ad)))
(defun %compress-g (x y) (compress-g x y))

(defun h-prime (input tag-length)
  "Argon2 variable-length hash H'.

   For TAG-LENGTH <= 64, returns Blake2b-T(LE32(T) || INPUT).

   For longer outputs, runs the iterated construction in §3.2: produces
   r = ceil(T/32) - 2 intermediate Blake2b-64 outputs whose first 32
   bytes form the leading prefix, then a final Blake2b-(T - 32r) ouput
   for the trailing partial block."
  (let ((t-le (u32-to-le tag-length)))
    (cond
      ((<= tag-length 64)
       (let ((buf (concatenate '(simple-array (unsigned-byte 8) (*))
                               t-le input)))
         (b2:blake2b buf :digest-length tag-length)))
      (t
       ;; Mirror the C reference implementation (blake2b_long) rather
       ;; than the sometimes-ambiguous RFC prose. We write successive
       ;; 32-byte halves of Blake2b-64 outputs while more than 64 bytes
       ;; remain, then finish with a single Blake2b of exactly the
       ;; remaining length.
       (let* ((out (make-array tag-length :element-type '(unsigned-byte 8)))
              (buf (concatenate '(simple-array (unsigned-byte 8) (*))
                                t-le input))
              (vi (b2:blake2b buf :digest-length 64))
              (offset 0)
              (remaining tag-length))
         (replace out vi :end1 32)
         (incf offset 32)
         (decf remaining 32)
         (loop while (> remaining 64) do
           (setf vi (b2:blake2b vi :digest-length 64))
           (replace out vi :start1 offset :end1 (+ offset 32))
           (incf offset 32)
           (decf remaining 32))
         (let ((final (b2:blake2b vi :digest-length remaining)))
           (replace out final :start1 offset :end1 (+ offset remaining)))
         out)))))

;;; ---------------------------------------------------------------------------
;;; The compression function G (RFC 9106 §3.6)
;;; ---------------------------------------------------------------------------
;;;
;;; G takes two 1024-byte blocks X and Y and returns a 1024-byte block.
;;; Internally:
;;;   R = X XOR Y
;;;   Apply Blake2-style permutation P to each row of R (8 rounds), then
;;;     to each column (8 more rounds), giving Z (in place on a copy).
;;;   Output = R XOR Z.
;;;
;;; The mixing function GB used by P is NOT vanilla Blake2b: it adds an
;;; extra multiplication step (RFC 9106 §3.6) so the data dependency
;;; can't be elided by short multipliers.

(declaim (inline mask64 rotr64))

(defun mask64 (x) (logand x #xffffffffffffffff))

(defun rotr64 (x n)
  (declare (type u64 x) (type (integer 0 63) n))
  (logior (ash x (- n)) (mask64 (ash x (- 64 n)))))

(declaim (inline gb-step))

(defun gb-step (block ia ib ic id)
  "Argon2 GB mixing function operating on four 64-bit words at indices
   IA, IB, IC, ID within BLOCK.

   The GB step (RFC 9106 §3.6) differs from Blake2b's G by adding a
   2*trunc(a)*trunc(b) term (and similarly for c,d) where trunc(x) is
   the low 32 bits of x."
  (declare (type u64-vector block)
           (type fixnum ia ib ic id))
  (let ((a (aref block ia))
        (b (aref block ib))
        (c (aref block ic))
        (d (aref block id)))
    (declare (type u64 a b c d))
    ;; a = a + b + 2 * lo32(a) * lo32(b)
    (setf a (mask64 (+ a b
                       (mask64 (* 2 (logand a #xffffffff)
                                  (logand b #xffffffff))))))
    (setf d (rotr64 (logxor d a) 32))
    (setf c (mask64 (+ c d
                       (mask64 (* 2 (logand c #xffffffff)
                                  (logand d #xffffffff))))))
    (setf b (rotr64 (logxor b c) 24))
    (setf a (mask64 (+ a b
                       (mask64 (* 2 (logand a #xffffffff)
                                  (logand b #xffffffff))))))
    (setf d (rotr64 (logxor d a) 16))
    (setf c (mask64 (+ c d
                       (mask64 (* 2 (logand c #xffffffff)
                                  (logand d #xffffffff))))))
    (setf b (rotr64 (logxor b c) 63))
    (setf (aref block ia) a)
    (setf (aref block ib) b)
    (setf (aref block ic) c)
    (setf (aref block id) d)))

(defun permutation-p (block base)
  "Apply the Argon2 permutation P (8 GB calls) to 16 consecutive 64-bit
   words starting at BASE within BLOCK. The 16 words are interpreted
   as a 4x4 matrix of u64s in column-major order; P performs four
   column-mix GB calls followed by four diagonal-mix GB calls."
  (declare (type u64-vector block) (type fixnum base))
  ;; Columns
  (gb-step block (+ base  0) (+ base  4) (+ base  8) (+ base 12))
  (gb-step block (+ base  1) (+ base  5) (+ base  9) (+ base 13))
  (gb-step block (+ base  2) (+ base  6) (+ base 10) (+ base 14))
  (gb-step block (+ base  3) (+ base  7) (+ base 11) (+ base 15))
  ;; Diagonals
  (gb-step block (+ base  0) (+ base  5) (+ base 10) (+ base 15))
  (gb-step block (+ base  1) (+ base  6) (+ base 11) (+ base 12))
  (gb-step block (+ base  2) (+ base  7) (+ base  8) (+ base 13))
  (gb-step block (+ base  3) (+ base  4) (+ base  9) (+ base 14)))

(defun compress-g (x y)
  "Argon2 compression function G(X, Y) -> 1024-byte block.

   X and Y are 128-word u64-vectors. Returns a fresh 128-word vector."
  (declare (type u64-vector x y))
  (let ((r (copy-block x))
        (z nil))
    (xor-block-into r y)
    (setf z (copy-block r))
    ;; The 1024-byte block is viewed as an 8x8 matrix of 16-byte
    ;; registers, which means 8 rows of 16 u64-words each.
    ;; First pass: P applied to each row of Z.
    (loop for row from 0 below 8
          do (permutation-p z (* row 16)))
    ;; Second pass: P applied to each column. Column i contains the
    ;; words (i*2, i*2+1, i*2+16, i*2+17, i*2+32, i*2+33, ..., i*2+112,
    ;; i*2+113), i.e. two consecutive words from each row at the same
    ;; column index. We have 8 columns; each call to P consumes 16
    ;; consecutive words, so we gather them into a temp buffer, run P,
    ;; and scatter back.
    (let ((tmp (make-array 16 :element-type '(unsigned-byte 64))))
      (loop for col from 0 below 8 do
        (let ((base (* col 2)))
          (loop for k from 0 below 8 do
            (setf (aref tmp (* k 2))       (aref z (+ base (* k 16))))
            (setf (aref tmp (1+ (* k 2)))  (aref z (+ base 1 (* k 16)))))
          (permutation-p tmp 0)
          (loop for k from 0 below 8 do
            (setf (aref z (+ base (* k 16)))     (aref tmp (* k 2)))
            (setf (aref z (+ base 1 (* k 16))) (aref tmp (1+ (* k 2))))))))
    ;; Output = R XOR Z.
    (xor-block-into z r)
    z))

;;; ---------------------------------------------------------------------------
;;; Pre-hash H0 (RFC 9106 §3.2)
;;; ---------------------------------------------------------------------------

(defun length-prefixed (bytes)
  "Encode BYTES as LE32(length) || BYTES (the format used in the H0 input)."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (u32-to-le (length bytes))
               bytes))

(defun compute-h0 (password salt secret ad parallelism tag-length memory-kb
                   iterations type)
  "Compute the 64-byte pre-hash H0 from the Argon2 parameters and inputs."
  (let ((blob (concatenate '(simple-array (unsigned-byte 8) (*))
                           (u32-to-le parallelism)
                           (u32-to-le tag-length)
                           (u32-to-le memory-kb)
                           (u32-to-le iterations)
                           (u32-to-le +argon2-version+)
                           (u32-to-le type)
                           (length-prefixed password)
                           (length-prefixed salt)
                           (length-prefixed secret)
                           (length-prefixed ad))))
    (b2:blake2b blob :digest-length 64)))

;;; ---------------------------------------------------------------------------
;;; Initial blocks (RFC 9106 §3.4)
;;; ---------------------------------------------------------------------------

(defun initial-block (h0 lane-bytes block-index lane-index)
  "Compute B[lane][block-index] for block-index in {0, 1} via H'."
  (declare (ignore lane-bytes))
  (let ((input (concatenate '(simple-array (unsigned-byte 8) (*))
                            h0
                            (u32-to-le block-index)
                            (u32-to-le lane-index))))
    (bytes-to-block (h-prime input +block-size+))))

;;; ---------------------------------------------------------------------------
;;; Reference index computation (RFC 9106 §3.4)
;;; ---------------------------------------------------------------------------

(defun map-relative-position (j1 ref-area-size)
  "Map J1 to a relative position within a window of size REF-AREA-SIZE
   using Argon2's non-uniform distribution (RFC 9106 §3.4):
     x = J1^2 / 2^32
     y = (W * x) / 2^32
     z = W - 1 - y."
  (let* ((x (ash (mask64 (* j1 j1)) -32))
         (y (ash (mask64 (* ref-area-size x)) -32))
         (z (- ref-area-size 1 y)))
    z))

;;; ---------------------------------------------------------------------------
;;; Argon2i pseudo-random index generation (RFC 9106 §3.4.1.1)
;;; ---------------------------------------------------------------------------
;;;
;;; For Argon2i (and the first half of the first pass of Argon2id), the
;;; (J1, J2) values used to pick reference blocks are themselves derived
;;; pseudo-randomly from the parameters by feeding a synthetic block
;;; through G twice. We generate them in batches of 128 (one block's
;;; worth of 64-bit halves) and consume them as the segment is filled.

(defun make-argon2i-input-block (pass lane slice memory-blocks total-passes
                                 argon2-type counter)
  (let ((b (zero-block)))
    (setf (aref b 0) pass)
    (setf (aref b 1) lane)
    (setf (aref b 2) slice)
    (setf (aref b 3) memory-blocks)
    (setf (aref b 4) total-passes)
    (setf (aref b 5) argon2-type)
    (setf (aref b 6) counter)
    b))

(defun next-address-block (input-block zero-blk)
  "Generate the next pseudo-random address block for Argon2i indexing
   by computing G(zero, G(zero, input-block))."
  (let ((tmp (compress-g zero-blk input-block)))
    (compress-g zero-blk tmp)))

;;; ---------------------------------------------------------------------------
;;; Memory filling (RFC 9106 §3.4)
;;; ---------------------------------------------------------------------------

(defun segment-length (memory-blocks lane-count)
  "Number of blocks per (lane, slice) segment, after rounding the total
   memory down so each segment is the same size."
  (floor memory-blocks (* lane-count +sync-points+)))

(defstruct fill-context
  pass lane slice
  segment-length lane-length
  lane-count memory-blocks
  total-passes
  argon2-type
  use-argon2i-addressing-p)

(defun fill-segment (memory ctx)
  "Fill one (lane, slice) segment of MEMORY according to CTX. MEMORY is
   a flat vector of LANE-COUNT * LANE-LENGTH blocks; lane i, position j
   lives at index (i * LANE-LENGTH + j)."
  (let* ((seg-len (fill-context-segment-length ctx))
         (lane (fill-context-lane ctx))
         (slice (fill-context-slice ctx))
         (pass (fill-context-pass ctx))
         (lane-len (fill-context-lane-length ctx))
         (lane-count (fill-context-lane-count ctx))
         (use-i (fill-context-use-argon2i-addressing-p ctx))
         ;; Argon2i pseudo-random address machinery (only used when USE-I).
         ;; Mirroring the C reference: input-blk starts with counter=0,
         ;; and `next-address-block` increments the counter BEFORE
         ;; generating the address block, so the first batch uses
         ;; counter=1, second uses 2, etc.
         (zero-blk (zero-block))
         (input-blk (when use-i
                      (make-argon2i-input-block
                       pass lane slice
                       (fill-context-memory-blocks ctx)
                       (fill-context-total-passes ctx)
                       (fill-context-argon2-type ctx)
                       0)))
         (address-blk nil)
         ;; Starting position within the segment. Pass 0 / slice 0 starts
         ;; at index 2 because positions 0 and 1 were initialised
         ;; directly from H'.
         (start-pos (if (and (zerop pass) (zerop slice)) 2 0)))
    ;; Mirror the C reference: when we are skipping i=0,1 (pass 0 slice
    ;; 0) but using Argon2i addressing, the address block must be
    ;; generated up front so that index i=2 has a valid pseudo-random
    ;; pool to read from.
    (when (and use-i (= start-pos 2))
      (incf (aref input-blk 6))
      (setf address-blk (next-address-block input-blk zero-blk)))
    (loop for i from start-pos below seg-len
          for j = (+ (* slice seg-len) i)
          for cur-index = (+ (* lane lane-len) j)
          for prev-index = (if (zerop j)
                               (+ (* lane lane-len) (1- lane-len))
                               (1- cur-index))
          do (let* ((prev-block (aref memory prev-index))
                    ;; Compute (J1, J2). For Argon2i / id-first-half:
                    ;; pull from the pseudo-random address block,
                    ;; refreshing it every 128 positions. For Argon2d:
                    ;; (J1, J2) is the first 64 bits of the previous
                    ;; block.
                    (j1j2-pair
                      (cond
                        (use-i
                         (when (zerop (mod i 128))
                           ;; Increment the counter and refresh the
                           ;; address block. The C reference does this
                           ;; UNCONDITIONALLY at i=0, 128, 256, ... so
                           ;; the first batch uses counter=1.
                           (incf (aref input-blk 6))
                           (setf address-blk
                                 (next-address-block input-blk zero-blk)))
                         (aref address-blk (mod i 128)))
                        (t
                         (aref prev-block 0))))
                    (j1 (logand j1j2-pair #xffffffff))
                    (j2 (logand (ash j1j2-pair -32) #xffffffff))
                    ;; Choose ref-lane FIRST: pass 0 / slice 0 forces
                    ;; same-lane; otherwise lane = J2 mod lane_count.
                    (ref-lane (if (and (zerop pass) (zerop slice))
                                  lane
                                  (mod j2 lane-count)))
                    (same-lane-p (= ref-lane lane))
                    ;; Now compute the reference window size, which
                    ;; depends on (pass, slice, same-lane-p, i).
                    (ref-area-size
                      (cond
                        ((zerop pass)
                         (cond
                           ((zerop slice)
                            ;; Forced same lane: positions 0..i-2.
                            (- i 1))
                           (same-lane-p
                            (+ (* slice seg-len) i -1))
                           (t
                            (+ (* slice seg-len)
                               (if (zerop i) -1 0)))))
                        (t
                         (cond
                           (same-lane-p
                            (+ (- lane-len seg-len) i -1))
                           (t
                            (+ (- lane-len seg-len)
                               (if (zerop i) -1 0)))))))
                    (rel-pos (map-relative-position j1 ref-area-size))
                    ;; The reference window starts at position 0 of the
                    ;; ref lane on pass 0; on later passes it begins at
                    ;; the segment immediately after the current one.
                    (start-position (if (zerop pass)
                                        0
                                        (mod (* (1+ slice) seg-len) lane-len)))
                    (abs-pos (mod (+ start-position rel-pos) lane-len))
                    (ref-index (+ (* ref-lane lane-len) abs-pos))
                    (ref-block (aref memory ref-index))
                    (new-block (compress-g prev-block ref-block)))
               (cond
                 ((zerop pass)
                  (setf (aref memory cur-index) new-block))
                 (t
                  (xor-block-into (aref memory cur-index) new-block)))))))

;;; ---------------------------------------------------------------------------
;;; Top-level driver
;;; ---------------------------------------------------------------------------

(defun argon2id (password salt &key (iterations 3) (memory-kb 65536)
                                    (parallelism 4) (tag-length 32)
                                    secret associated-data)
  "Argon2id (RFC 9106). Derive a tag from PASSWORD and SALT.

   PASSWORD and SALT must be byte vectors. The optional SECRET (`K`) and
   ASSOCIATED-DATA (`X`) parameters default to empty.

   Parameters:
     :ITERATIONS    -- t, number of passes over memory. Min 1.
     :MEMORY-KB     -- m, memory cost in kibibytes. Min 8 * PARALLELISM.
     :PARALLELISM   -- p, number of lanes. Min 1, max 2^24 - 1.
     :TAG-LENGTH    -- T, output length in bytes. Min 4.

   Returns a byte vector of length TAG-LENGTH.

   This implementation runs all lanes sequentially within each segment;
   it is correct but does not exploit the inter-lane parallelism the
   spec permits."
  (declare (type (simple-array (unsigned-byte 8) (*)) password salt))
  (let* ((password (coerce password '(simple-array (unsigned-byte 8) (*))))
         (salt     (coerce salt '(simple-array (unsigned-byte 8) (*))))
         (secret   (coerce (or secret #()) '(simple-array (unsigned-byte 8) (*))))
         (ad       (coerce (or associated-data #())
                           '(simple-array (unsigned-byte 8) (*)))))
    (assert (>= iterations 1) () "argon2id: iterations must be >= 1")
    (assert (>= parallelism 1) () "argon2id: parallelism must be >= 1")
    (assert (>= tag-length 4) () "argon2id: tag-length must be >= 4")
    (assert (>= memory-kb (* 8 parallelism)) ()
            "argon2id: memory-kb must be >= 8*parallelism")
    (let* ((h0-base (compute-h0 password salt secret ad
                                parallelism tag-length memory-kb iterations
                                +argon2id-type+))
           ;; Round memory down so each segment is equal-size.
           (seg-len (segment-length memory-kb parallelism))
           (lane-len (* seg-len +sync-points+))
           (memory-blocks (* lane-len parallelism))
           (memory (make-array memory-blocks)))
      ;; Pre-fill with zero blocks so any reference-index bug surfaces as
      ;; a wrong-block test failure rather than a TYPE-ERROR deep inside
      ;; the compression function.
      (loop for i from 0 below memory-blocks do
        (setf (aref memory i) (zero-block)))
      ;; Initial two blocks per lane.
      (loop for lane from 0 below parallelism do
        (setf (aref memory (* lane lane-len))
              (initial-block h0-base nil 0 lane))
        (setf (aref memory (1+ (* lane lane-len)))
              (initial-block h0-base nil 1 lane)))
      ;; Fill the rest of memory.
      (loop for pass from 0 below iterations do
        (loop for slice from 0 below +sync-points+ do
          (loop for lane from 0 below parallelism do
            (let ((ctx (make-fill-context
                        :pass pass :lane lane :slice slice
                        :segment-length seg-len :lane-length lane-len
                        :lane-count parallelism
                        :memory-blocks memory-blocks
                        :total-passes iterations
                        :argon2-type +argon2id-type+
                        :use-argon2i-addressing-p
                        ;; Argon2id: data-independent for the first half
                        ;; (slices 0 and 1) of the first pass, otherwise
                        ;; data-dependent.
                        (and (zerop pass) (< slice 2)))))
              (fill-segment memory ctx)))))
      ;; Combine the last block of every lane.
      (let ((c (copy-block (aref memory (1- lane-len)))))
        (loop for lane from 1 below parallelism do
          (xor-block-into c (aref memory (1- (* (1+ lane) lane-len)))))
        (h-prime (block-to-bytes c) tag-length)))))
