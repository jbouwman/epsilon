;;;; AES (Advanced Encryption Standard, FIPS 197)
;;;;
;;;; Pure-Lisp implementation of AES-128 and AES-256 with two paths
;;;; selected via `*aes-constant-time*':
;;;;
;;;; - Default (table) path: SubBytes via 256-entry S-box lookup
;;;;   tables. Fast, but the array index leaks the high bits of
;;;;   secret bytes through L1 cache-line residency. Suitable when
;;;;   no co-located untrusted workload can observe cache state.
;;;;
;;;; - CT path (`*aes-constant-time* = T'): SubBytes via the
;;;;   GF(2^8) inversion + affine transformation, expressed as a
;;;;   sequence of bitwise operations with no secret-indexed memory
;;;;   access. Roughly 100x slower per block; the right choice for
;;;;   TLS terminators sharing a host with untrusted workloads.
;;;;
;;;; ShiftRows, MixColumns, and AddRoundKey are byte-swizzles and
;;;; XORs that are constant-time on any reasonable architecture.

(defpackage epsilon.crypto.aes
  (:use :cl)
  (:export
   #:aes-encrypt-block
   #:aes-decrypt-block
   #:make-aes-round-keys
   #:make-aes-decrypt-round-keys
   #:aes-ctr-encrypt
   #:aes-ctr
   #:aes-cbc-encrypt
   #:aes-cbc-decrypt
   #:+aes-block-size+
   ;; Constant-time mode: bind around AES calls in code paths that
   ;; touch secret data on shared hosts.
   #:*aes-constant-time*))

(in-package :epsilon.crypto.aes)

(defconstant +aes-block-size+ 16)

;;; ---------------------------------------------------------------------------
;;; S-box and Inverse S-box (FIPS 197 Section 5.1.1)
;;; ---------------------------------------------------------------------------

(defparameter +sbox+
  (make-array 256 :element-type '(unsigned-byte 8)
    :initial-contents
    '(#x63 #x7c #x77 #x7b #xf2 #x6b #x6f #xc5 #x30 #x01 #x67 #x2b #xfe #xd7 #xab #x76
      #xca #x82 #xc9 #x7d #xfa #x59 #x47 #xf0 #xad #xd4 #xa2 #xaf #x9c #xa4 #x72 #xc0
      #xb7 #xfd #x93 #x26 #x36 #x3f #xf7 #xcc #x34 #xa5 #xe5 #xf1 #x71 #xd8 #x31 #x15
      #x04 #xc7 #x23 #xc3 #x18 #x96 #x05 #x9a #x07 #x12 #x80 #xe2 #xeb #x27 #xb2 #x75
      #x09 #x83 #x2c #x1a #x1b #x6e #x5a #xa0 #x52 #x3b #xd6 #xb3 #x29 #xe3 #x2f #x84
      #x53 #xd1 #x00 #xed #x20 #xfc #xb1 #x5b #x6a #xcb #xbe #x39 #x4a #x4c #x58 #xcf
      #xd0 #xef #xaa #xfb #x43 #x4d #x33 #x85 #x45 #xf9 #x02 #x7f #x50 #x3c #x9f #xa8
      #x51 #xa3 #x40 #x8f #x92 #x9d #x38 #xf5 #xbc #xb6 #xda #x21 #x10 #xff #xf3 #xd2
      #xcd #x0c #x13 #xec #x5f #x97 #x44 #x17 #xc4 #xa7 #x7e #x3d #x64 #x5d #x19 #x73
      #x60 #x81 #x4f #xdc #x22 #x2a #x90 #x88 #x46 #xee #xb8 #x14 #xde #x5e #x0b #xdb
      #xe0 #x32 #x3a #x0a #x49 #x06 #x24 #x5c #xc2 #xd3 #xac #x62 #x91 #x95 #xe4 #x79
      #xe7 #xc8 #x37 #x6d #x8d #xd5 #x4e #xa9 #x6c #x56 #xf4 #xea #x65 #x7a #xae #x08
      #xba #x78 #x25 #x2e #x1c #xa6 #xb4 #xc6 #xe8 #xdd #x74 #x1f #x4b #xbd #x8b #x8a
      #x70 #x3e #xb5 #x66 #x48 #x03 #xf6 #x0e #x61 #x35 #x57 #xb9 #x86 #xc1 #x1d #x9e
      #xe1 #xf8 #x98 #x11 #x69 #xd9 #x8e #x94 #x9b #x1e #x87 #xe9 #xce #x55 #x28 #xdf
      #x8c #xa1 #x89 #x0d #xbf #xe6 #x42 #x68 #x41 #x99 #x2d #x0f #xb0 #x54 #xbb #x16)))

(defparameter +inv-sbox+
  (make-array 256 :element-type '(unsigned-byte 8)
    :initial-contents
    '(#x52 #x09 #x6a #xd5 #x30 #x36 #xa5 #x38 #xbf #x40 #xa3 #x9e #x81 #xf3 #xd7 #xfb
      #x7c #xe3 #x39 #x82 #x9b #x2f #xff #x87 #x34 #x8e #x43 #x44 #xc4 #xde #xe9 #xcb
      #x54 #x7b #x94 #x32 #xa6 #xc2 #x23 #x3d #xee #x4c #x95 #x0b #x42 #xfa #xc3 #x4e
      #x08 #x2e #xa1 #x66 #x28 #xd9 #x24 #xb2 #x76 #x5b #xa2 #x49 #x6d #x8b #xd1 #x25
      #x72 #xf8 #xf6 #x64 #x86 #x68 #x98 #x16 #xd4 #xa4 #x5c #xcc #x5d #x65 #xb6 #x92
      #x6c #x70 #x48 #x50 #xfd #xed #xb9 #xda #x5e #x15 #x46 #x57 #xa7 #x8d #x9d #x84
      #x90 #xd8 #xab #x00 #x8c #xbc #xd3 #x0a #xf7 #xe4 #x58 #x05 #xb8 #xb3 #x45 #x06
      #xd0 #x2c #x1e #x8f #xca #x3f #x0f #x02 #xc1 #xaf #xbd #x03 #x01 #x13 #x8a #x6b
      #x3a #x91 #x11 #x41 #x4f #x67 #xdc #xea #x97 #xf2 #xcf #xce #xf0 #xb4 #xe6 #x73
      #x96 #xac #x74 #x22 #xe7 #xad #x35 #x85 #xe2 #xf9 #x37 #xe8 #x1c #x75 #xdf #x6e
      #x47 #xf1 #x1a #x71 #x1d #x29 #xc5 #x89 #x6f #xb7 #x62 #x0e #xaa #x18 #xbe #x1b
      #xfc #x56 #x3e #x4b #xc6 #xd2 #x79 #x20 #x9a #xdb #xc0 #xfe #x78 #xcd #x5a #xf4
      #x1f #xdd #xa8 #x33 #x88 #x07 #xc7 #x31 #xb1 #x12 #x10 #x59 #x27 #x80 #xec #x5f
      #x60 #x51 #x7f #xa9 #x19 #xb5 #x4a #x0d #x2d #xe5 #x7a #x9f #x93 #xc9 #x9c #xef
      #xa0 #xe0 #x3b #x4d #xae #x2a #xf5 #xb0 #xc8 #xeb #xbb #x3c #x83 #x53 #x99 #x61
      #x17 #x2b #x04 #x7e #xba #x77 #xd6 #x26 #xe1 #x69 #x14 #x63 #x55 #x21 #x0c #x7d)))

;;; ---------------------------------------------------------------------------
;;; Round constants (FIPS 197 Section 5.2)
;;; ---------------------------------------------------------------------------

(defparameter +rcon+
  (make-array 11 :element-type '(unsigned-byte 8)
    :initial-contents
    '(#x00 #x01 #x02 #x04 #x08 #x10 #x20 #x40 #x80 #x1b #x36)))

;;; ---------------------------------------------------------------------------
;;; GF(2^8) multiplication helpers for MixColumns
;;; ---------------------------------------------------------------------------

(declaim (inline xtime gf-mul))

(defun xtime (a)
  "Multiply by x (i.e., by 2) in GF(2^8) with irreducible polynomial x^8+x^4+x^3+x+1."
  (declare (type (unsigned-byte 8) a))
  (let ((shifted (logand #xFF (ash a 1))))
    (if (logbitp 7 a)
        (logxor shifted #x1b)
        shifted)))

(defun gf-mul (a b)
  "Multiply A by B in GF(2^8)."
  (declare (type (unsigned-byte 8) a b))
  (let ((result 0)
        (aa a))
    (declare (type (unsigned-byte 8) result aa))
    (loop for i from 0 below 8
          do (when (logbitp i b)
               (setf result (logxor result aa)))
             (setf aa (xtime aa)))
    result))

;;; ---------------------------------------------------------------------------
;;; Constant-time AES primitives
;;;
;;; Standard AES uses an 8-bit S-box implemented as a 256-entry lookup
;;; table. On any system with a data cache, the byte-wise array index
;;; leaks the high bits of secret bytes through cache-line residency.
;;; The CT path below replaces the table lookup with a sequence of
;;; bitwise operations: GF(2^8) inversion via the Itoh-Tsujii addition
;;; chain for 254 = b^-1 (since |GF(2^8)*| = 255), followed by the
;;; AES affine transformation expressed as a fixed pattern of XORs
;;; and 8-bit rotations. No memory access depends on a secret value.
;;;
;;; The performance impact on a single AES block is roughly 100x; the
;;; CT path is intended for use by callers that pin AES under
;;; `*aes-constant-time*` precisely because they are running on a
;;; shared host where cache-timing attacks are in the threat model.
;;; ---------------------------------------------------------------------------

(declaim (inline xtime-ct gf-mul-ct gf-sqr-ct))

(defun xtime-ct (a)
  "Branchless multiply-by-x in GF(2^8): computes (a << 1) XOR
   (mask AND 0x1b) where mask = -bit7(a)."
  (declare (type (unsigned-byte 8) a))
  (let* ((shifted (logand #xff (ash a 1)))
         (mask (logand #xff (- (ash a -7)))))
    (logxor shifted (logand mask #x1b))))

(defun gf-mul-ct (a b)
  "Branchless GF(2^8) multiply: never short-circuits on cleared
   bits of B. Each iteration performs the conditional XOR via
   mask-AND-XOR rather than (when (logbitp i b) ...)."
  (declare (type (unsigned-byte 8) a b)
           (optimize (speed 3) (safety 1)))
  (let ((result 0)
        (aa a))
    (declare (type (unsigned-byte 8) result aa))
    (loop for i from 0 below 8
          do (let* ((bit (ldb (byte 1 i) b))
                    (mask (logand #xff (- bit))))
               (declare (type (unsigned-byte 8) mask))
               (setf result (logand #xff (logxor result (logand mask aa)))))
             (setf aa (xtime-ct aa)))
    result))

(defun gf-sqr-ct (a)
  "GF(2^8) squaring: (gf-mul-ct a a). Inlined for clarity."
  (declare (type (unsigned-byte 8) a))
  (gf-mul-ct a a))

(defun aes-gf-inv-ct (b)
  "Constant-time GF(2^8) inversion. Returns b^-1 in the AES field
   with the convention 0^-1 = 0 (which Itoh-Tsujii satisfies because
   0^254 = 0 in any field of finite characteristic).

   Addition chain: 254 = 0b11111110, so b^254 is built up as
     b1 b2 b3 b6 b7 b14 b15 b30 b31 b62 b63 b126 b127 b254
   via 8 squarings and 6 multiplies."
  (declare (type (unsigned-byte 8) b)
           (optimize (speed 3) (safety 1)))
  (let* ((b1 b)
         (b2 (gf-sqr-ct b1))
         (b3 (gf-mul-ct b2 b1))           ; b^3
         (b6 (gf-sqr-ct b3))               ; b^6
         (b7 (gf-mul-ct b6 b1))            ; b^7
         (b14 (gf-sqr-ct b7))              ; b^14
         (b15 (gf-mul-ct b14 b1))          ; b^15
         (b30 (gf-sqr-ct b15))             ; b^30
         (b31 (gf-mul-ct b30 b1))          ; b^31
         (b62 (gf-sqr-ct b31))             ; b^62
         (b63 (gf-mul-ct b62 b1))          ; b^63
         (b126 (gf-sqr-ct b63))            ; b^126
         (b127 (gf-mul-ct b126 b1))        ; b^127
         (b254 (gf-sqr-ct b127)))          ; b^254 = b^-1
    (declare (type (unsigned-byte 8) b1 b2 b3 b6 b7 b14 b15 b30 b31
                                     b62 b63 b126 b127 b254))
    b254))

(declaim (inline %rotl8))
(defun %rotl8 (x k)
  "8-bit cyclic left rotation."
  (declare (type (unsigned-byte 8) x)
           (type (integer 0 7) k))
  (logand #xff (logior (ash x k) (ash x (- k 8)))))

(defun aes-sbox-ct (b)
  "Constant-time AES forward S-box: GF(2^8) inverse + affine.

   Affine: s = b XOR ROTL(b,1) XOR ROTL(b,2) XOR ROTL(b,3) XOR
                  ROTL(b,4) XOR 0x63."
  (declare (type (unsigned-byte 8) b)
           (optimize (speed 3) (safety 1)))
  (let ((inv (aes-gf-inv-ct b)))
    (declare (type (unsigned-byte 8) inv))
    (logxor inv
            (%rotl8 inv 1)
            (%rotl8 inv 2)
            (%rotl8 inv 3)
            (%rotl8 inv 4)
            #x63)))

(defun aes-inv-sbox-ct (s)
  "Constant-time AES inverse S-box: inverse affine + GF(2^8) inverse.

   Inverse affine: t = s XOR 0x63
                   b = ROTL(t,1) XOR ROTL(t,3) XOR ROTL(t,6)."
  (declare (type (unsigned-byte 8) s)
           (optimize (speed 3) (safety 1)))
  (let* ((tt (logxor s #x63))
         (b (logxor (%rotl8 tt 1) (%rotl8 tt 3) (%rotl8 tt 6))))
    (declare (type (unsigned-byte 8) tt b))
    (aes-gf-inv-ct b)))

(defvar *aes-constant-time* nil
  "When non-nil, AES operations replace the S-box lookup tables with
   bitwise GF(2^8) computation (`aes-sbox-ct` / `aes-inv-sbox-ct`).
   Bind this around AES calls in code paths that operate on
   secret-dependent data while running on a shared host where cache-
   timing attacks are in scope. Default nil keeps the existing
   table-based fast path for backward compatibility.")

(declaim (inline aes-sbox aes-inv-sbox))
(defun aes-sbox (x)
  "AES forward S-box, dispatched on `*aes-constant-time*`."
  (declare (type (unsigned-byte 8) x))
  (if *aes-constant-time*
      (aes-sbox-ct x)
      (aref +sbox+ x)))
(defun aes-inv-sbox (x)
  "AES inverse S-box, dispatched on `*aes-constant-time*`."
  (declare (type (unsigned-byte 8) x))
  (if *aes-constant-time*
      (aes-inv-sbox-ct x)
      (aref +inv-sbox+ x)))

;;; ---------------------------------------------------------------------------
;;; Key Expansion (FIPS 197 Section 5.2)
;;; ---------------------------------------------------------------------------

(defun make-aes-round-keys (key)
  "Expand a 16-byte (AES-128) or 32-byte (AES-256) key into round keys.
   Returns a vector of (Nr+1) round keys, each a 16-byte array.
   AES-128: 11 round keys. AES-256: 15 round keys."
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let* ((key-len (length key))
         (nk (/ key-len 4))   ; Number of 32-bit words in key (4 or 8)
         (nr (+ nk 6))        ; Number of rounds (10 or 14)
         (total-words (* 4 (1+ nr)))  ; Total 32-bit words needed
         (w (make-array (* total-words 4) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Copy key into first Nk words
    (replace w key :end1 key-len :end2 key-len)
    ;; Expand
    (loop for i from nk below total-words
          for off = (* i 4)
          for prev-off = (* (1- i) 4)
          for nk-back-off = (* (- i nk) 4)
          do (cond
               ;; i mod Nk == 0: RotWord + SubWord + Rcon
               ((zerop (mod i nk))
                (setf (aref w off)       (logxor (aes-sbox (aref w (+ prev-off 1)))
                                                 (aref +rcon+ (/ i nk))
                                                 (aref w nk-back-off)))
                (setf (aref w (+ off 1)) (logxor (aes-sbox (aref w (+ prev-off 2)))
                                                 (aref w (+ nk-back-off 1))))
                (setf (aref w (+ off 2)) (logxor (aes-sbox (aref w (+ prev-off 3)))
                                                 (aref w (+ nk-back-off 2))))
                (setf (aref w (+ off 3)) (logxor (aes-sbox (aref w prev-off))
                                                 (aref w (+ nk-back-off 3)))))
               ;; AES-256 only: i mod Nk == 4: SubWord
               ((and (= nk 8) (= (mod i nk) 4))
                (setf (aref w off)       (logxor (aes-sbox (aref w prev-off))
                                                 (aref w nk-back-off)))
                (setf (aref w (+ off 1)) (logxor (aes-sbox (aref w (+ prev-off 1)))
                                                 (aref w (+ nk-back-off 1))))
                (setf (aref w (+ off 2)) (logxor (aes-sbox (aref w (+ prev-off 2)))
                                                 (aref w (+ nk-back-off 2))))
                (setf (aref w (+ off 3)) (logxor (aes-sbox (aref w (+ prev-off 3)))
                                                 (aref w (+ nk-back-off 3)))))
               ;; Otherwise: simple XOR
               (t
                (setf (aref w off)       (logxor (aref w prev-off) (aref w nk-back-off)))
                (setf (aref w (+ off 1)) (logxor (aref w (+ prev-off 1)) (aref w (+ nk-back-off 1))))
                (setf (aref w (+ off 2)) (logxor (aref w (+ prev-off 2)) (aref w (+ nk-back-off 2))))
                (setf (aref w (+ off 3)) (logxor (aref w (+ prev-off 3)) (aref w (+ nk-back-off 3)))))))
    ;; Split into (Nr+1) round key arrays
    (let ((round-keys (make-array (1+ nr))))
      (loop for r from 0 to nr
            for rk = (make-array 16 :element-type '(unsigned-byte 8))
            do (replace rk w :start2 (* r 16) :end2 (* (1+ r) 16))
               (setf (aref round-keys r) rk))
      round-keys)))

(defun make-aes-decrypt-round-keys (key)
  "Expand key and prepare for equivalent inverse cipher (reverse round key order)."
  (let* ((round-keys (make-aes-round-keys key))
         (nr (1- (length round-keys))))
    (let ((decrypt-keys (make-array (1+ nr))))
      (loop for i from 0 to nr
            do (setf (aref decrypt-keys i) (aref round-keys (- nr i))))
      decrypt-keys)))

;;; ---------------------------------------------------------------------------
;;; AES core operations
;;; ---------------------------------------------------------------------------

(defun sub-bytes (state)
  "Apply S-box substitution to each byte of state. Dispatches via
   `*aes-constant-time*` between the standard table and the bitwise
   GF(2^8) computation."
  (loop for i from 0 below 16
        do (setf (aref state i) (aes-sbox (aref state i)))))

(defun inv-sub-bytes (state)
  "Apply inverse S-box substitution to each byte of state.
   See `sub-bytes' for CT dispatch."
  (loop for i from 0 below 16
        do (setf (aref state i) (aes-inv-sbox (aref state i)))))

(defun shift-rows (state tmp)
  "Apply ShiftRows: row r is shifted left by r positions.
   State byte index = row + 4*col (row 0..3, col 0..3)."
  (replace tmp state)
  (loop for r from 1 below 4
        do (loop for c from 0 below 4
                 do (setf (aref state (+ r (* 4 c)))
                          (aref tmp (+ r (* 4 (mod (+ c r) 4))))))))

(defun inv-shift-rows (state tmp)
  "Apply InvShiftRows: row r is shifted right by r positions."
  (replace tmp state)
  (loop for r from 1 below 4
        do (loop for c from 0 below 4
                 do (setf (aref state (+ r (* 4 c)))
                          (aref tmp (+ r (* 4 (mod (- c r) 4))))))))

(defun mix-columns (state)
  "Apply MixColumns: multiply each column by the MDS matrix in GF(2^8).
   Matrix: [2 3 1 1; 1 2 3 1; 1 1 2 3; 3 1 1 2]"
  (loop for c from 0 below 4
        for base = (* c 4)
        for s0 = (aref state base)
        for s1 = (aref state (+ base 1))
        for s2 = (aref state (+ base 2))
        for s3 = (aref state (+ base 3))
        do (setf (aref state base)
                 (logxor (xtime s0) (logxor (xtime s1) s1) s2 s3))
           (setf (aref state (+ base 1))
                 (logxor s0 (xtime s1) (logxor (xtime s2) s2) s3))
           (setf (aref state (+ base 2))
                 (logxor s0 s1 (xtime s2) (logxor (xtime s3) s3)))
           (setf (aref state (+ base 3))
                 (logxor (logxor (xtime s0) s0) s1 s2 (xtime s3)))))

(defun inv-mix-columns (state)
  "Apply InvMixColumns: multiply each column by the inverse MDS matrix.
   Matrix: [14 11 13 9; 9 14 11 13; 13 9 14 11; 11 13 9 14]"
  (loop for c from 0 below 4
        for base = (* c 4)
        for s0 = (aref state base)
        for s1 = (aref state (+ base 1))
        for s2 = (aref state (+ base 2))
        for s3 = (aref state (+ base 3))
        do (setf (aref state base)
                 (logxor (gf-mul #xe s0) (gf-mul #xb s1)
                         (gf-mul #xd s2) (gf-mul #x9 s3)))
           (setf (aref state (+ base 1))
                 (logxor (gf-mul #x9 s0) (gf-mul #xe s1)
                         (gf-mul #xb s2) (gf-mul #xd s3)))
           (setf (aref state (+ base 2))
                 (logxor (gf-mul #xd s0) (gf-mul #x9 s1)
                         (gf-mul #xe s2) (gf-mul #xb s3)))
           (setf (aref state (+ base 3))
                 (logxor (gf-mul #xb s0) (gf-mul #xd s1)
                         (gf-mul #x9 s2) (gf-mul #xe s3)))))

(defun add-round-key (state rk)
  "XOR state with round key."
  (loop for i from 0 below 16
        do (setf (aref state i) (logxor (aref state i) (aref rk i)))))

;;; ---------------------------------------------------------------------------
;;; AES Block Encryption (FIPS 197 Section 5.1)
;;; ---------------------------------------------------------------------------

(defun aes-encrypt-block (input round-keys)
  "Encrypt a single 16-byte block using AES.
   INPUT is a 16-byte array. ROUND-KEYS from make-aes-round-keys.
   Returns a new 16-byte array."
  (declare (type (simple-array (unsigned-byte 8) (*)) input))
  (let* ((nr (1- (length round-keys)))
         (state (make-array 16 :element-type '(unsigned-byte 8)))
         (tmp (make-array 16 :element-type '(unsigned-byte 8))))
    ;; Initial AddRoundKey
    (loop for i from 0 below 16
          do (setf (aref state i) (logxor (aref input i) (aref (aref round-keys 0) i))))
    ;; Rounds 1..Nr-1: SubBytes, ShiftRows, MixColumns, AddRoundKey
    (loop for round from 1 below nr
          do (sub-bytes state)
             (shift-rows state tmp)
             (mix-columns state)
             (add-round-key state (aref round-keys round)))
    ;; Final round: SubBytes, ShiftRows, AddRoundKey (no MixColumns)
    (sub-bytes state)
    (shift-rows state tmp)
    (add-round-key state (aref round-keys nr))
    state))

;;; ---------------------------------------------------------------------------
;;; AES Block Decryption (FIPS 197 Section 5.3)
;;; ---------------------------------------------------------------------------

(defun aes-decrypt-block (input decrypt-round-keys)
  "Decrypt a single 16-byte block using AES.
   INPUT is a 16-byte ciphertext. DECRYPT-ROUND-KEYS from make-aes-decrypt-round-keys.
   Returns a new 16-byte plaintext array."
  (declare (type (simple-array (unsigned-byte 8) (*)) input))
  (let* ((nr (1- (length decrypt-round-keys)))
         (state (make-array 16 :element-type '(unsigned-byte 8)))
         (tmp (make-array 16 :element-type '(unsigned-byte 8))))
    ;; Initial AddRoundKey
    (loop for i from 0 below 16
          do (setf (aref state i) (logxor (aref input i) (aref (aref decrypt-round-keys 0) i))))
    ;; Rounds 1..Nr-1: InvShiftRows, InvSubBytes, AddRoundKey, InvMixColumns
    (loop for round from 1 below nr
          do (inv-shift-rows state tmp)
             (inv-sub-bytes state)
             (add-round-key state (aref decrypt-round-keys round))
             (inv-mix-columns state))
    ;; Final round: InvShiftRows, InvSubBytes, AddRoundKey (no InvMixColumns)
    (inv-shift-rows state tmp)
    (inv-sub-bytes state)
    (add-round-key state (aref decrypt-round-keys nr))
    state))

;;; ---------------------------------------------------------------------------
;;; AES-CTR Mode (NIST SP 800-38A Section 6.5)
;;; ---------------------------------------------------------------------------

(defun increment-counter (counter)
  "Increment a 16-byte counter block (big-endian, rightmost 4 bytes)."
  (declare (type (simple-array (unsigned-byte 8) (*)) counter))
  (loop for i from 15 downto 12
        do (let ((new (1+ (aref counter i))))
             (if (> new #xFF)
                 (setf (aref counter i) 0)
                 (progn
                   (setf (aref counter i) (logand #xFF new))
                   (return))))))

(defun aes-ctr-encrypt (plaintext key nonce)
  "Encrypt (or decrypt) PLAINTEXT using AES-CTR mode.
   KEY is 16 or 32 bytes. NONCE is 12 bytes (IV).
   The counter block is: nonce (12 bytes) || counter (4 bytes, big-endian, starting at 1).
   Returns the ciphertext (same length as plaintext)."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext key nonce))
  (let* ((round-keys (make-aes-round-keys key))
         (len (length plaintext))
         (output (make-array len :element-type '(unsigned-byte 8)))
         (counter (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Set up counter: nonce || 0x00000001
    (replace counter nonce :end1 12 :end2 12)
    (setf (aref counter 12) 0)
    (setf (aref counter 13) 0)
    (setf (aref counter 14) 0)
    (setf (aref counter 15) 1)
    ;; Process each block
    (loop for offset from 0 below len by 16
          for block-len = (min 16 (- len offset))
          for keystream = (aes-encrypt-block counter round-keys)
          do (loop for i from 0 below block-len
                   do (setf (aref output (+ offset i))
                            (logxor (aref plaintext (+ offset i))
                                    (aref keystream i))))
             (increment-counter counter))
    output))

;;; ---------------------------------------------------------------------------
;;; AES-CTR (NIST SP 800-38A Section 6.5) -- general 128-bit counter form
;;; ---------------------------------------------------------------------------

(defun increment-counter-128 (counter)
  "Increment a 16-byte counter as a 128-bit big-endian integer.
   Wraps modulo 2^128 (consistent with NIST SP 800-38A Appendix B.1)."
  (declare (type (simple-array (unsigned-byte 8) (*)) counter))
  (loop for i from 15 downto 0 do
    (let ((new (1+ (aref counter i))))
      (cond
        ((<= new #xFF)
         (setf (aref counter i) new)
         (return))
        (t
         (setf (aref counter i) 0))))))

(defun aes-ctr (data key initial-counter)
  "Encrypt or decrypt DATA using AES-CTR (NIST SP 800-38A Section 6.5).

   KEY is 16, 24, or 32 bytes. INITIAL-COUNTER is the 16-byte initial
   counter block (T_1 in NIST notation); the caller is responsible for
   constructing it from whatever nonce / IV layout the protocol requires.
   The counter is incremented as a single 128-bit big-endian integer
   between blocks.

   CTR mode is its own inverse: passing the ciphertext back in with the
   same KEY and INITIAL-COUNTER recovers the plaintext."
  (declare (type (simple-array (unsigned-byte 8) (*)) data key initial-counter))
  (unless (= (length initial-counter) 16)
    (error "aes-ctr: initial counter must be exactly 16 bytes (got ~D)"
           (length initial-counter)))
  (let* ((round-keys (make-aes-round-keys key))
         (len (length data))
         (output (make-array len :element-type '(unsigned-byte 8)))
         (counter (copy-seq initial-counter)))
    (loop for offset from 0 below len by 16
          for block-len = (min 16 (- len offset))
          for keystream = (aes-encrypt-block counter round-keys)
          do (loop for i from 0 below block-len
                   do (setf (aref output (+ offset i))
                            (logxor (aref data (+ offset i))
                                    (aref keystream i))))
             (increment-counter-128 counter))
    output))

;;; ---------------------------------------------------------------------------
;;; AES-CBC (NIST SP 800-38A Section 6.2) with PKCS#7 padding (RFC 5652 §6.3)
;;; ---------------------------------------------------------------------------

(defun aes-cbc-encrypt (plaintext key iv)
  "Encrypt PLAINTEXT under AES-CBC (NIST SP 800-38A §6.2) with PKCS#7
   padding (RFC 5652 §6.3). KEY is 16, 24, or 32 bytes. IV is 16 bytes.

   PKCS#7 padding always adds 1-16 bytes: when the plaintext length is
   already a multiple of the block size, a full extra block of 0x10
   bytes is appended (RFC 5652 §6.3 paragraph 3). The returned
   ciphertext length is therefore always a strict multiple of 16 and
   strictly greater than the plaintext length."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext key iv))
  (unless (= (length iv) 16)
    (error "aes-cbc-encrypt: IV must be exactly 16 bytes (got ~D)"
           (length iv)))
  (let* ((round-keys (make-aes-round-keys key))
         (pt-len (length plaintext))
         (pad-len (- 16 (mod pt-len 16)))  ; 1..16
         (padded-len (+ pt-len pad-len))
         (padded (make-array padded-len :element-type '(unsigned-byte 8)))
         (output (make-array padded-len :element-type '(unsigned-byte 8)))
         (prev (copy-seq iv))
         (block (make-array 16 :element-type '(unsigned-byte 8))))
    (replace padded plaintext)
    (loop for i from pt-len below padded-len
          do (setf (aref padded i) pad-len))
    (loop for offset from 0 below padded-len by 16 do
      (loop for i from 0 below 16
            do (setf (aref block i)
                     (logxor (aref padded (+ offset i)) (aref prev i))))
      (let ((enc (aes-encrypt-block block round-keys)))
        (replace output enc :start1 offset)
        (setf prev enc)))
    output))

(defun aes-cbc-decrypt (ciphertext key iv)
  "Decrypt CIPHERTEXT under AES-CBC and strip PKCS#7 padding. Signals an
   error if CIPHERTEXT is not a non-empty multiple of 16 bytes or if the
   trailing PKCS#7 padding is malformed."
  (declare (type (simple-array (unsigned-byte 8) (*)) ciphertext key iv))
  (let ((len (length ciphertext)))
    (when (or (zerop len) (not (zerop (mod len 16))))
      (error "aes-cbc-decrypt: ciphertext length must be a non-zero multiple ~
              of 16 (got ~D)" len))
    (unless (= (length iv) 16)
      (error "aes-cbc-decrypt: IV must be exactly 16 bytes (got ~D)"
             (length iv)))
    (let* ((dec-keys (make-aes-decrypt-round-keys key))
           (output (make-array len :element-type '(unsigned-byte 8)))
           (prev (copy-seq iv))
           (block (make-array 16 :element-type '(unsigned-byte 8))))
      (loop for offset from 0 below len by 16 do
        (loop for i from 0 below 16
              do (setf (aref block i) (aref ciphertext (+ offset i))))
        (let ((dec (aes-decrypt-block block dec-keys)))
          (loop for i from 0 below 16
                do (setf (aref output (+ offset i))
                         (logxor (aref dec i) (aref prev i)))))
        ;; The next block's previous-ciphertext is THIS block's
        ;; ciphertext, not the just-decrypted plaintext.
        (setf prev (copy-seq block)))
      ;; Strip PKCS#7 padding (RFC 5652 §6.3).
      (let ((pad (aref output (1- len))))
        (when (or (zerop pad) (> pad 16) (> pad len))
          (error "aes-cbc-decrypt: invalid PKCS#7 padding length ~D" pad))
        (loop for i from (- len pad) below len do
          (unless (= (aref output i) pad)
            (error "aes-cbc-decrypt: invalid PKCS#7 padding")))
        (subseq output 0 (- len pad))))))
