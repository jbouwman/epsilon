;;;; SHA-512 and SHA-384 (FIPS 180-4)
;;;;
;;;; Pure-Lisp implementation of SHA-512 and SHA-384.
;;;; SHA-512 uses 64-bit words, 80 rounds, and 128-byte blocks.
;;;; SHA-384 is SHA-512 with different initial values and truncated output.

(defpackage epsilon.ssl.sha512
  (:use :cl)
  (:import-from #:epsilon.ssl.primitives #:u64+ #:u64-rotr #:u64-shr #:+u64-mask+)
  (:export
   ;; SHA-512
   #:sha512
   #:sha512-hex
   #:make-sha512-state
   #:sha512-update
   #:sha512-finalize
   #:sha512-copy
   ;; SHA-384
   #:sha384
   #:sha384-hex
   #:make-sha384-state
   #:sha384-update
   #:sha384-finalize
   #:sha384-copy
   ;; Constants
   #:+sha512-block-size+
   #:+sha512-digest-size+
   #:+sha384-block-size+
   #:+sha384-digest-size+))

(in-package :epsilon.ssl.sha512)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconstant +sha512-block-size+ 128)
(defconstant +sha512-digest-size+ 64)
(defconstant +sha384-block-size+ 128)
(defconstant +sha384-digest-size+ 48)

;; First 64 bits of fractional parts of cube roots of first 80 primes
(defparameter +k+
  (coerce
   '(#x428a2f98d728ae22 #x7137449123ef65cd #xb5c0fbcfec4d3b2f #xe9b5dba58189dbbc
     #x3956c25bf348b538 #x59f111f1b605d019 #x923f82a4af194f9b #xab1c5ed5da6d8118
     #xd807aa98a3030242 #x12835b0145706fbe #x243185be4ee4b28c #x550c7dc3d5ffb4e2
     #x72be5d74f27b896f #x80deb1fe3b1696b1 #x9bdc06a725c71235 #xc19bf174cf692694
     #xe49b69c19ef14ad2 #xefbe4786384f25e3 #x0fc19dc68b8cd5b5 #x240ca1cc77ac9c65
     #x2de92c6f592b0275 #x4a7484aa6ea6e483 #x5cb0a9dcbd41fbd4 #x76f988da831153b5
     #x983e5152ee66dfab #xa831c66d2db43210 #xb00327c898fb213f #xbf597fc7beef0ee4
     #xc6e00bf33da88fc2 #xd5a79147930aa725 #x06ca6351e003826f #x142929670a0e6e70
     #x27b70a8546d22ffc #x2e1b21385c26c926 #x4d2c6dfc5ac42aed #x53380d139d95b3df
     #x650a73548baf63de #x766a0abb3c77b2a8 #x81c2c92e47edaee6 #x92722c851482353b
     #xa2bfe8a14cf10364 #xa81a664bbc423001 #xc24b8b70d0f89791 #xc76c51a30654be30
     #xd192e819d6ef5218 #xd69906245565a910 #xf40e35855771202a #x106aa07032bbd1b8
     #x19a4c116b8d2d0c8 #x1e376c085141ab53 #x2748774cdf8eeb99 #x34b0bcb5e19b48a8
     #x391c0cb3c5c95a63 #x4ed8aa4ae3418acb #x5b9cca4f7763e373 #x682e6ff3d6b2b8a3
     #x748f82ee5defb2fc #x78a5636f43172f60 #x84c87814a1f0ab72 #x8cc702081a6439ec
     #x90befffa23631e28 #xa4506cebde82bde9 #xbef9a3f7b2c67915 #xc67178f2e372532b
     #xca273eceea26619c #xd186b8c721c0c207 #xeada7dd6cde0eb1e #xf57d4f7fee6ed178
     #x06f067aa72176fba #x0a637dc5a2c898a6 #x113f9804bef90dae #x1b710b35131c471b
     #x28db77f523047d84 #x32caab7b40c72493 #x3c9ebe0a15c9bebc #x431d67c49c100d4c
     #x4cc5d4becb3e42b6 #x597f299cfc657e2a #x5fcb6fab3ad6faec #x6c44198c4a475817)
   '(simple-array (unsigned-byte 64) (80))))

;; SHA-512 initial hash values
(defparameter +h0-sha512+
  (coerce
   '(#x6a09e667f3bcc908 #xbb67ae8584caa73b
     #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
     #x510e527fade682d1 #x9b05688c2b3e6c1f
     #x1f83d9abfb41bd6b #x5be0cd19137e2179)
   '(simple-array (unsigned-byte 64) (8))))

;; SHA-384 initial hash values
(defparameter +h0-sha384+
  (coerce
   '(#xcbbb9d5dc1059ed8 #x629a292a367cd507
     #x9159015a3070dd17 #x152fecd8f70e5939
     #x67332667ffc00b31 #x8eb44a8768581511
     #xdb0c2e0d64f98fa7 #x47b5481dbefa4fa4)
   '(simple-array (unsigned-byte 64) (8))))

;;; ---------------------------------------------------------------------------
;;; SHA-512 functions
;;; ---------------------------------------------------------------------------

(declaim (inline ch-64 maj-64 big-sigma0-64 big-sigma1-64
                 small-sigma0-64 small-sigma1-64))

(defun ch-64 (x y z)
  (logxor (logand x y) (logand (logand +u64-mask+ (lognot x)) z)))

(defun maj-64 (x y z)
  (logxor (logand x y) (logand x z) (logand y z)))

(defun big-sigma0-64 (x)
  (logxor (u64-rotr x 28) (u64-rotr x 34) (u64-rotr x 39)))

(defun big-sigma1-64 (x)
  (logxor (u64-rotr x 14) (u64-rotr x 18) (u64-rotr x 41)))

(defun small-sigma0-64 (x)
  (logxor (u64-rotr x 1) (u64-rotr x 8) (u64-shr x 7)))

(defun small-sigma1-64 (x)
  (logxor (u64-rotr x 19) (u64-rotr x 61) (u64-shr x 6)))

;;; ---------------------------------------------------------------------------
;;; SHA-512 state
;;; ---------------------------------------------------------------------------

(defstruct (sha512-state (:constructor %make-sha512-state))
  (h (let ((arr (make-array 8 :element-type '(unsigned-byte 64))))
       (replace arr +h0-sha512+)
       arr)
   :type (simple-array (unsigned-byte 64) (8)))
  (buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (128)))
  (buffer-count 0 :type (integer 0 128))
  (total-length 0 :type unsigned-byte))

(defun make-sha512-state ()
  "Create a fresh SHA-512 hasher state."
  (%make-sha512-state))

(defun sha512-copy (state)
  "Return an independent copy of a SHA-512 state."
  (let ((new (%make-sha512-state)))
    (replace (sha512-state-h new) (sha512-state-h state))
    (replace (sha512-state-buffer new) (sha512-state-buffer state))
    (setf (sha512-state-buffer-count new) (sha512-state-buffer-count state))
    (setf (sha512-state-total-length new) (sha512-state-total-length state))
    new))

;;; SHA-384 state (same structure, different initial values)

(defstruct (sha384-state (:constructor %make-sha384-state))
  (h (let ((arr (make-array 8 :element-type '(unsigned-byte 64))))
       (replace arr +h0-sha384+)
       arr)
   :type (simple-array (unsigned-byte 64) (8)))
  (buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (128)))
  (buffer-count 0 :type (integer 0 128))
  (total-length 0 :type unsigned-byte))

(defun make-sha384-state ()
  "Create a fresh SHA-384 hasher state."
  (%make-sha384-state))

(defun sha384-copy (state)
  "Return an independent copy of a SHA-384 state."
  (let ((new (%make-sha384-state)))
    (replace (sha384-state-h new) (sha384-state-h state))
    (replace (sha384-state-buffer new) (sha384-state-buffer state))
    (setf (sha384-state-buffer-count new) (sha384-state-buffer-count state))
    (setf (sha384-state-total-length new) (sha384-state-total-length state))
    new))

;;; ---------------------------------------------------------------------------
;;; Block processing (shared between SHA-512 and SHA-384)
;;; ---------------------------------------------------------------------------

(defun process-block-512 (h block offset)
  "Process a single 128-byte block."
  (declare (type (simple-array (unsigned-byte 64) (8)) h)
           (type (simple-array (unsigned-byte 8) (*)) block)
           (type fixnum offset)
           (optimize (speed 3) (safety 1)))
  (let ((w (make-array 80 :element-type '(unsigned-byte 64) :initial-element 0)))
    ;; Prepare message schedule W[0..79]
    ;; W[0..15] = big-endian 64-bit words from the block
    (loop for t-idx from 0 below 16
          for byte-idx = (+ offset (* t-idx 8))
          do (setf (aref w t-idx)
                   (logior (ash (aref block byte-idx) 56)
                           (ash (aref block (+ byte-idx 1)) 48)
                           (ash (aref block (+ byte-idx 2)) 40)
                           (ash (aref block (+ byte-idx 3)) 32)
                           (ash (aref block (+ byte-idx 4)) 24)
                           (ash (aref block (+ byte-idx 5)) 16)
                           (ash (aref block (+ byte-idx 6)) 8)
                           (aref block (+ byte-idx 7)))))
    ;; W[16..79]
    (loop for t-idx from 16 below 80
          do (setf (aref w t-idx)
                   (u64+ (small-sigma1-64 (aref w (- t-idx 2)))
                         (aref w (- t-idx 7))
                         (small-sigma0-64 (aref w (- t-idx 15)))
                         (aref w (- t-idx 16)))))
    ;; Working variables
    (let ((a (aref h 0)) (b (aref h 1))
          (c (aref h 2)) (d (aref h 3))
          (e (aref h 4)) (f (aref h 5))
          (g (aref h 6)) (hh (aref h 7)))
      ;; 80 rounds
      (loop for t-idx from 0 below 80
            do (let* ((t1 (u64+ hh (big-sigma1-64 e) (ch-64 e f g)
                                (aref +k+ t-idx) (aref w t-idx)))
                      (t2 (u64+ (big-sigma0-64 a) (maj-64 a b c))))
                 (setf hh g  g f  f e  e (u64+ d t1)
                       d c  c b  b a  a (u64+ t1 t2))))
      ;; Update hash values
      (setf (aref h 0) (u64+ (aref h 0) a)
            (aref h 1) (u64+ (aref h 1) b)
            (aref h 2) (u64+ (aref h 2) c)
            (aref h 3) (u64+ (aref h 3) d)
            (aref h 4) (u64+ (aref h 4) e)
            (aref h 5) (u64+ (aref h 5) f)
            (aref h 6) (u64+ (aref h 6) g)
            (aref h 7) (u64+ (aref h 7) hh)))))

;;; ---------------------------------------------------------------------------
;;; Generic update/finalize for SHA-512 family
;;; ---------------------------------------------------------------------------

(defun update-512 (h buffer buffer-count-accessor total-length-accessor
                   data start end)
  "Generic update for SHA-512/384."
  (let* ((data-len (- end start))
         (buf buffer)
         (buf-count (funcall buffer-count-accessor)))
    (funcall total-length-accessor data-len)
    (let ((pos start))
      (when (> buf-count 0)
        (let ((copy-len (min (- 128 buf-count) data-len)))
          (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                            :start2 pos :end2 (+ pos copy-len))
          (incf pos copy-len)
          (incf buf-count copy-len)
          (when (= buf-count 128)
            (process-block-512 h buf 0)
            (setf buf-count 0))))
      (loop while (<= (+ pos 128) end)
            do (process-block-512 h data pos)
               (incf pos 128))
      (when (< pos end)
        (let ((remaining (- end pos)))
          (replace buf data :start1 0 :end1 remaining
                            :start2 pos :end2 end)
          (setf buf-count remaining)))
      buf-count)))

(defun finalize-512 (h buffer buf-count total-length digest-size)
  "Generic finalize for SHA-512/384."
  (let ((total-bits (* total-length 8)))
    (setf (aref buffer buf-count) #x80)
    (incf buf-count)
    (loop for i from buf-count below 128
          do (setf (aref buffer i) 0))
    (when (> buf-count 112)
      (process-block-512 h buffer 0)
      (loop for i from 0 below 128
            do (setf (aref buffer i) 0)))
    ;; Append 128-bit length (we only use the low 64 bits)
    ;; High 64 bits = 0 for messages < 2^64 bytes
    (loop for i from 0 below 8
          do (setf (aref buffer (+ 120 i))
                   (logand #xFF (ash total-bits (- (* (- 7 i) 8))))))
    (process-block-512 h buffer 0)
    ;; Extract digest
    (let ((digest (make-array digest-size :element-type '(unsigned-byte 8))))
      (loop for i from 0 below (/ digest-size 8)
            for word = (aref h i)
            for base = (* i 8)
            do (loop for j from 0 below 8
                     when (< (+ base j) digest-size)
                     do (setf (aref digest (+ base j))
                              (logand #xFF (ash word (- (* (- 7 j) 8)))))))
      digest)))

;;; ---------------------------------------------------------------------------
;;; SHA-512 API
;;; ---------------------------------------------------------------------------

(defun sha512-update (state data &key (start 0) (end nil))
  "Feed DATA into SHA-512 state."
  (declare (type sha512-state state)
           (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((end (or end (length data))))
    (setf (sha512-state-buffer-count state)
          (update-512 (sha512-state-h state)
                      (sha512-state-buffer state)
                      (lambda () (sha512-state-buffer-count state))
                      (lambda (n) (incf (sha512-state-total-length state) n))
                      data start end)))
  state)

(defun sha512-finalize (state)
  "Finalize SHA-512 and return 64-byte digest."
  (declare (type sha512-state state))
  (finalize-512 (sha512-state-h state)
                (sha512-state-buffer state)
                (sha512-state-buffer-count state)
                (sha512-state-total-length state)
                64))

(defun sha512 (data &key (start 0) (end nil))
  "Compute SHA-512 hash of DATA. Returns 64-byte digest."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-sha512-state)))
    (sha512-update state data :start start :end end)
    (sha512-finalize state)))

(defun sha512-hex (data &key (start 0) (end nil))
  "Compute SHA-512 hash and return as lowercase hex string."
  (let ((digest (sha512 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest
            do (format s "~(~2,'0x~)" byte)))))

;;; ---------------------------------------------------------------------------
;;; SHA-384 API
;;; ---------------------------------------------------------------------------

(defun sha384-update (state data &key (start 0) (end nil))
  "Feed DATA into SHA-384 state."
  (declare (type sha384-state state)
           (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((end (or end (length data))))
    (setf (sha384-state-buffer-count state)
          (update-512 (sha384-state-h state)
                      (sha384-state-buffer state)
                      (lambda () (sha384-state-buffer-count state))
                      (lambda (n) (incf (sha384-state-total-length state) n))
                      data start end)))
  state)

(defun sha384-finalize (state)
  "Finalize SHA-384 and return 48-byte digest."
  (declare (type sha384-state state))
  (finalize-512 (sha384-state-h state)
                (sha384-state-buffer state)
                (sha384-state-buffer-count state)
                (sha384-state-total-length state)
                48))

(defun sha384 (data &key (start 0) (end nil))
  "Compute SHA-384 hash of DATA. Returns 48-byte digest."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-sha384-state)))
    (sha384-update state data :start start :end end)
    (sha384-finalize state)))

(defun sha384-hex (data &key (start 0) (end nil))
  "Compute SHA-384 hash and return as lowercase hex string."
  (let ((digest (sha384 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest
            do (format s "~(~2,'0x~)" byte)))))
