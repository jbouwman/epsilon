;;;; SHA-256 (FIPS 180-4)
;;;;
;;;; Pure-Lisp implementation of the SHA-256 hash function.
;;;; This is the workhorse hash used throughout the crypto library,
;;;; including HMAC-DRBG, HKDF, and TLS 1.3 key schedule.

(defpackage epsilon.ssl.sha256
  (:use :cl :epsilon.ssl.primitives)
  (:export
   ;; One-shot hashing
   #:sha256
   #:sha256-hex
   ;; Incremental hashing
   #:make-sha256-state
   #:sha256-update
   #:sha256-finalize
   #:sha256-copy
   ;; SHA-224
   #:sha224
   #:sha224-hex
   #:make-sha224-state
   #:sha224-update
   #:sha224-finalize
   #:sha224-copy
   ;; Constants
   #:+sha256-block-size+
   #:+sha256-digest-size+
   #:+sha224-block-size+
   #:+sha224-digest-size+))

(in-package :epsilon.ssl.sha256)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconstant +sha256-block-size+ 64
  "SHA-256 processes data in 64-byte (512-bit) blocks.")

(defconstant +sha256-digest-size+ 32
  "SHA-256 produces a 32-byte (256-bit) digest.")

;; First 32 bits of the fractional parts of the cube roots of the first 64 primes
(defparameter +k+
  (make-array 64
    :element-type '(unsigned-byte 32)
    :initial-contents
    '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
      #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
      #xd807aa98 #x12835b01 #x243185be #x550c7dc3
      #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
      #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
      #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
      #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
      #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
      #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
      #x650a7354 #x766a0abb #x81c2c92e #x92722c85
      #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
      #xd192e819 #xd6990624 #xf40e3585 #x106aa070
      #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
      #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
      #x748f82ee #x78a5636f #x84c87814 #x8cc70208
      #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))

;; Initial hash values: first 32 bits of the fractional parts
;; of the square roots of the first 8 primes
(defparameter +h0-init+
  (make-array 8
    :element-type '(unsigned-byte 32)
    :initial-contents
    '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
      #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))

;; SHA-224 initial hash values (FIPS 180-4 Section 5.3.2)
(defparameter +h0-sha224+
  (make-array 8
    :element-type '(unsigned-byte 32)
    :initial-contents
    '(#xc1059ed8 #x367cd507 #x3070dd17 #xf70e5939
      #xffc00b31 #x68581511 #x64f98fa7 #xbefa4fa4)))

(defconstant +sha224-block-size+ 64)
(defconstant +sha224-digest-size+ 28)

;;; ---------------------------------------------------------------------------
;;; SHA-256 functions
;;; ---------------------------------------------------------------------------

(declaim (inline ch maj big-sigma0 big-sigma1 small-sigma0 small-sigma1))

(defun ch (x y z)
  "Choice function: Ch(x,y,z) = (x AND y) XOR (NOT x AND z)"
  (u32-xor (u32-and x y) (u32-and (u32-not x) z)))

(defun maj (x y z)
  "Majority function: Maj(x,y,z) = (x AND y) XOR (x AND z) XOR (y AND z)"
  (u32-xor (u32-and x y) (u32-and x z) (u32-and y z)))

(defun big-sigma0 (x)
  "SIGMA_0(x) = ROTR^2(x) XOR ROTR^13(x) XOR ROTR^22(x)"
  (u32-xor (u32-rotr x 2) (u32-rotr x 13) (u32-rotr x 22)))

(defun big-sigma1 (x)
  "SIGMA_1(x) = ROTR^6(x) XOR ROTR^11(x) XOR ROTR^25(x)"
  (u32-xor (u32-rotr x 6) (u32-rotr x 11) (u32-rotr x 25)))

(defun small-sigma0 (x)
  "sigma_0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x)"
  (u32-xor (u32-rotr x 7) (u32-rotr x 18) (u32-shr x 3)))

(defun small-sigma1 (x)
  "sigma_1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x)"
  (u32-xor (u32-rotr x 17) (u32-rotr x 19) (u32-shr x 10)))

;;; ---------------------------------------------------------------------------
;;; SHA-256 state
;;; ---------------------------------------------------------------------------

(defstruct (sha256-state (:constructor %make-sha256-state))
  "Internal state for incremental SHA-256 hashing."
  ;; Hash values (8 x 32-bit words)
  (h (let ((arr (make-array 8 :element-type '(unsigned-byte 32))))
       (replace arr +h0-init+)
       arr)
   :type (simple-array (unsigned-byte 32) (8)))
  ;; Buffer for incomplete block
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (64)))
  ;; Number of bytes currently in the buffer (0..63)
  (buffer-count 0 :type (integer 0 64))
  ;; Total number of bytes processed (for length padding)
  (total-length 0 :type (unsigned-byte 64)))

(defun make-sha256-state ()
  "Create a fresh SHA-256 hasher state."
  (%make-sha256-state))

(defun sha256-copy (state)
  "Return an independent copy of a SHA-256 state."
  (let ((new (%make-sha256-state)))
    (replace (sha256-state-h new) (sha256-state-h state))
    (replace (sha256-state-buffer new) (sha256-state-buffer state))
    (setf (sha256-state-buffer-count new) (sha256-state-buffer-count state))
    (setf (sha256-state-total-length new) (sha256-state-total-length state))
    new))

;;; ---------------------------------------------------------------------------
;;; Block processing
;;; ---------------------------------------------------------------------------

(defun process-block (state block offset)
  "Process a single 64-byte block from BLOCK starting at OFFSET."
  (declare (type sha256-state state)
           (type (simple-array (unsigned-byte 8) (*)) block)
           (type fixnum offset)
           (optimize (speed 3) (safety 1)))
  (let ((w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0))
        (hv (sha256-state-h state)))
    (declare (type (simple-array (unsigned-byte 32) (64)) w)
             (type (simple-array (unsigned-byte 32) (8)) hv))
    ;; Step 1: Prepare the message schedule (W[0..63])
    ;; W[0..15] = big-endian 32-bit words from the block
    (loop for t-idx fixnum from 0 below 16
          for byte-idx fixnum = (+ offset (* t-idx 4))
          do (setf (aref w t-idx)
                   (logior (ash (aref block byte-idx) 24)
                           (ash (aref block (+ byte-idx 1)) 16)
                           (ash (aref block (+ byte-idx 2)) 8)
                           (aref block (+ byte-idx 3)))))
    ;; W[16..63] = sigma1(W[t-2]) + W[t-7] + sigma0(W[t-15]) + W[t-16]
    (loop for t-idx fixnum from 16 below 64
          do (setf (aref w t-idx)
                   (u32+ (small-sigma1 (aref w (- t-idx 2)))
                         (aref w (- t-idx 7))
                         (small-sigma0 (aref w (- t-idx 15)))
                         (aref w (- t-idx 16)))))
    ;; Step 2: Initialize working variables
    (let ((a (aref hv 0)) (b (aref hv 1))
          (c (aref hv 2)) (d (aref hv 3))
          (e (aref hv 4)) (f (aref hv 5))
          (g (aref hv 6)) (hh (aref hv 7)))
      (declare (type (unsigned-byte 32) a b c d e f g hh))
      ;; Step 3: 64 rounds of compression
      (loop for t-idx fixnum from 0 below 64
            do (let* ((t1 (u32+ hh (big-sigma1 e) (ch e f g)
                                (aref +k+ t-idx) (aref w t-idx)))
                      (t2 (u32+ (big-sigma0 a) (maj a b c))))
                 (declare (type (unsigned-byte 32) t1 t2))
                 (setf hh g  g f  f e  e (u32+ d t1)
                       d c  c b  b a  a (u32+ t1 t2))))
      ;; Step 4: Update hash values
      (setf (aref hv 0) (u32+ (aref hv 0) a)
            (aref hv 1) (u32+ (aref hv 1) b)
            (aref hv 2) (u32+ (aref hv 2) c)
            (aref hv 3) (u32+ (aref hv 3) d)
            (aref hv 4) (u32+ (aref hv 4) e)
            (aref hv 5) (u32+ (aref hv 5) f)
            (aref hv 6) (u32+ (aref hv 6) g)
            (aref hv 7) (u32+ (aref hv 7) hh)))))

;;; ---------------------------------------------------------------------------
;;; Incremental API
;;; ---------------------------------------------------------------------------

(defun sha256-update (state data &key (start 0) (end nil))
  "Feed DATA bytes into the SHA-256 state."
  (declare (type sha256-state state)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start))
  (let* ((end (or end (length data)))
         (data-len (- end start))
         (buf (sha256-state-buffer state))
         (buf-count (sha256-state-buffer-count state)))
    (declare (type fixnum end data-len buf-count))
    (incf (sha256-state-total-length state) data-len)
    (let ((pos start))
      (declare (type fixnum pos))
      ;; If there's data in the buffer, try to fill it to a complete block
      (when (> buf-count 0)
        (let ((space (- 64 buf-count))
              (copy-len (min (- 64 buf-count) data-len)))
          (declare (type fixnum space copy-len)
                   (ignore space))
          (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                            :start2 pos :end2 (+ pos copy-len))
          (incf pos copy-len)
          (incf buf-count copy-len)
          (when (= buf-count 64)
            (process-block state buf 0)
            (setf buf-count 0))))
      ;; Process complete blocks directly from input
      (loop while (<= (+ pos 64) end)
            do (process-block state data pos)
               (incf pos 64))
      ;; Buffer remaining bytes
      (when (< pos end)
        (let ((remaining (- end pos)))
          (replace buf data :start1 0 :end1 remaining
                            :start2 pos :end2 end)
          (setf buf-count remaining)))
      (setf (sha256-state-buffer-count state) buf-count)))
  state)

(defun sha256-finalize (state)
  "Finalize the SHA-256 computation and return the 32-byte digest.
   The state should not be used after this call."
  (declare (type sha256-state state))
  (let* ((buf (sha256-state-buffer state))
         (buf-count (sha256-state-buffer-count state))
         (total-bits (* (sha256-state-total-length state) 8)))
    ;; Append the 1 bit (0x80 byte)
    (setf (aref buf buf-count) #x80)
    (incf buf-count)
    ;; Zero the rest of the buffer
    (loop for i from buf-count below 64
          do (setf (aref buf i) 0))
    ;; If not enough room for the 8-byte length, process this block
    ;; and start a new one
    (when (> buf-count 56)
      (process-block state buf 0)
      (loop for i from 0 below 64
            do (setf (aref buf i) 0)))
    ;; Append total length in bits as big-endian 64-bit integer
    (setf (aref buf 56) (logand #xFF (ash total-bits -56)))
    (setf (aref buf 57) (logand #xFF (ash total-bits -48)))
    (setf (aref buf 58) (logand #xFF (ash total-bits -40)))
    (setf (aref buf 59) (logand #xFF (ash total-bits -32)))
    (setf (aref buf 60) (logand #xFF (ash total-bits -24)))
    (setf (aref buf 61) (logand #xFF (ash total-bits -16)))
    (setf (aref buf 62) (logand #xFF (ash total-bits -8)))
    (setf (aref buf 63) (logand #xFF total-bits))
    ;; Process the final block
    (process-block state buf 0)
    ;; Extract the digest as big-endian bytes
    (let ((digest (make-array 32 :element-type '(unsigned-byte 8)))
          (hv (sha256-state-h state)))
      (loop for i from 0 below 8
            for word = (aref hv i)
            for base = (* i 4)
            do (setf (aref digest base)       (logand #xFF (ash word -24)))
               (setf (aref digest (+ base 1)) (logand #xFF (ash word -16)))
               (setf (aref digest (+ base 2)) (logand #xFF (ash word -8)))
               (setf (aref digest (+ base 3)) (logand #xFF word)))
      digest)))

;;; ---------------------------------------------------------------------------
;;; One-shot API
;;; ---------------------------------------------------------------------------

(defun sha256 (data &key (start 0) (end nil))
  "Compute the SHA-256 hash of DATA (a byte array).
   Returns a 32-byte digest."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-sha256-state)))
    (sha256-update state data :start start :end end)
    (sha256-finalize state)))

(defun sha256-hex (data &key (start 0) (end nil))
  "Compute the SHA-256 hash of DATA and return it as a lowercase hex string."
  (let ((digest (sha256 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest
            do (format s "~(~2,'0x~)" byte)))))

;;; ---------------------------------------------------------------------------
;;; SHA-224 (FIPS 180-4 Section 6.3)
;;; SHA-224 uses the same compression function as SHA-256, with different
;;; initial hash values and output truncated to 28 bytes.
;;; ---------------------------------------------------------------------------

(defun make-sha224-state ()
  "Create a fresh SHA-224 hasher state."
  (let ((state (%make-sha256-state)))
    (replace (sha256-state-h state) +h0-sha224+)
    state))

(defun sha224-copy (state)
  "Return an independent copy of a SHA-224 state."
  (sha256-copy state))

(defun sha224-update (state data &key (start 0) (end nil))
  "Feed DATA bytes into the SHA-224 state."
  (sha256-update state data :start start :end end))

(defun sha224-finalize (state)
  "Finalize SHA-224 and return 28-byte digest."
  (subseq (sha256-finalize state) 0 28))

(defun sha224 (data &key (start 0) (end nil))
  "Compute SHA-224 hash of DATA. Returns 28-byte digest."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-sha224-state)))
    (sha224-update state data :start start :end end)
    (sha224-finalize state)))

(defun sha224-hex (data &key (start 0) (end nil))
  "Compute SHA-224 hash and return as lowercase hex string."
  (let ((digest (sha224 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest
            do (format s "~(~2,'0x~)" byte)))))
