;;;; SHA-3 / Keccak (FIPS 202)
;;;;
;;;; Pure-Lisp implementation of the Keccak-p[1600,24] permutation
;;;; and the SHA-3 / SHAKE family built on top of it.

(defpackage epsilon.ssl.sha3
  (:use :cl)
  (:export
   ;; SHA3 one-shot
   #:sha3-256 #:sha3-256-hex
   #:sha3-384 #:sha3-384-hex
   #:sha3-512 #:sha3-512-hex
   ;; SHA3 incremental
   #:make-sha3-256-state #:sha3-256-update #:sha3-256-finalize #:sha3-256-copy
   #:make-sha3-384-state #:sha3-384-update #:sha3-384-finalize #:sha3-384-copy
   #:make-sha3-512-state #:sha3-512-update #:sha3-512-finalize #:sha3-512-copy
   ;; SHAKE XOF
   #:shake128 #:shake256
   #:make-shake128-state #:shake128-update #:shake128-squeeze
   #:make-shake256-state #:shake256-update #:shake256-squeeze))

(in-package :epsilon.ssl.sha3)

;;; ---------------------------------------------------------------------------
;;; Keccak constants
;;; ---------------------------------------------------------------------------

(defparameter +round-constants+
  (coerce
   '(#x0000000000000001 #x0000000000008082 #x800000000000808A
     #x8000000080008000 #x000000000000808B #x0000000080000001
     #x8000000080008081 #x8000000000008009 #x000000000000008A
     #x0000000000000088 #x0000000080008009 #x000000008000000A
     #x000000008000808B #x800000000000008B #x8000000000008089
     #x8000000000008003 #x8000000000008002 #x8000000000000080
     #x000000000000800A #x800000008000000A #x8000000080008081
     #x8000000000008080 #x0000000080000001 #x8000000080008008)
   '(simple-array (unsigned-byte 64) (24))))

;; Rotation offsets for rho step, indexed as [x][y]
;; r[x,y] from FIPS 202 Table 2
(defparameter +rotations+
  #2A(( 0 36  3 41 18)
      ( 1 44 10 45  2)
      (62  6 43 15 61)
      (28 55 25 21 56)
      (27 20 39  8 14)))

(defconstant +u64-mask+ #xFFFFFFFFFFFFFFFF)

(declaim (inline u64-rot64))

(defun u64-rot64 (x n)
  "Rotate 64-bit value X left by N bits."
  (declare (type (unsigned-byte 64) x)
           (type (integer 0 63) n))
  (logand +u64-mask+
          (logior (ash x n)
                  (ash x (- n 64)))))

;;; ---------------------------------------------------------------------------
;;; Keccak-p[1600,24] permutation
;;; ---------------------------------------------------------------------------

(defun keccak-f (state)
  "Apply the Keccak-f[1600] permutation to a 5x5 array of 64-bit lanes.
   STATE is a (simple-array (unsigned-byte 64) (25)), laid out as state[x + 5*y]."
  (declare (type (simple-array (unsigned-byte 64) (25)) state)
           (optimize (speed 3) (safety 1)))
  (let ((c (make-array 5 :element-type '(unsigned-byte 64) :initial-element 0))
        (d (make-array 5 :element-type '(unsigned-byte 64) :initial-element 0))
        (b (make-array 25 :element-type '(unsigned-byte 64) :initial-element 0)))
    (loop for round-idx from 0 below 24
          do
       ;; theta
       (loop for x from 0 below 5
             do (setf (aref c x)
                      (logxor (aref state x)
                              (aref state (+ x 5))
                              (aref state (+ x 10))
                              (aref state (+ x 15))
                              (aref state (+ x 20)))))
       (loop for x from 0 below 5
             do (setf (aref d x)
                      (logand +u64-mask+
                              (logxor (aref c (mod (+ x 4) 5))
                                      (u64-rot64 (aref c (mod (+ x 1) 5)) 1)))))
       (loop for i from 0 below 25
             do (setf (aref state i)
                      (logand +u64-mask+
                              (logxor (aref state i)
                                      (aref d (mod i 5))))))
       ;; rho and pi
       (loop for x from 0 below 5
             do (loop for y from 0 below 5
                      for src-idx = (+ x (* 5 y))
                      for dst-x = y
                      for dst-y = (mod (+ (* 2 x) (* 3 y)) 5)
                      for dst-idx = (+ dst-x (* 5 dst-y))
                      do (setf (aref b dst-idx)
                               (u64-rot64 (aref state src-idx)
                                          (aref +rotations+ x y)))))
       ;; chi
       (loop for y from 0 below 5
             for y5 = (* y 5)
             do (loop for x from 0 below 5
                      do (setf (aref state (+ x y5))
                               (logand +u64-mask+
                                       (logxor (aref b (+ x y5))
                                               (logand (logand +u64-mask+
                                                                (lognot (aref b (+ (mod (+ x 1) 5) y5))))
                                                       (aref b (+ (mod (+ x 2) 5) y5))))))))
       ;; iota
       (setf (aref state 0)
             (logand +u64-mask+
                     (logxor (aref state 0)
                             (aref +round-constants+ round-idx))))))
  state)

;;; ---------------------------------------------------------------------------
;;; Sponge construction
;;; ---------------------------------------------------------------------------

(defstruct (keccak-state (:constructor %make-keccak-state))
  "Keccak sponge state."
  (lanes (make-array 25 :element-type '(unsigned-byte 64) :initial-element 0)
   :type (simple-array (unsigned-byte 64) (25)))
  ;; Rate in bytes (r/8)
  (rate-bytes 0 :type fixnum)
  ;; Position in the current block (bytes absorbed so far mod rate)
  (absorbed 0 :type fixnum)
  ;; Domain separation byte: #x06 for SHA-3, #x1F for SHAKE
  (domain-sep #x06 :type (unsigned-byte 8))
  ;; Has squeeze phase started?
  (squeezed nil :type boolean)
  ;; Squeeze position (for XOF)
  (squeeze-pos 0 :type fixnum))

(defun make-keccak-state (rate-bytes domain-sep)
  "Create a Keccak sponge with given rate (in bytes) and domain separation byte."
  (let ((state (%make-keccak-state)))
    (setf (keccak-state-rate-bytes state) rate-bytes)
    (setf (keccak-state-domain-sep state) domain-sep)
    state))

(defun keccak-copy (state)
  "Copy a keccak state."
  (let ((new (%make-keccak-state)))
    (replace (keccak-state-lanes new) (keccak-state-lanes state))
    (setf (keccak-state-rate-bytes new) (keccak-state-rate-bytes state))
    (setf (keccak-state-absorbed new) (keccak-state-absorbed state))
    (setf (keccak-state-domain-sep new) (keccak-state-domain-sep state))
    (setf (keccak-state-squeezed new) (keccak-state-squeezed state))
    (setf (keccak-state-squeeze-pos new) (keccak-state-squeeze-pos state))
    new))

(defun xor-byte-into-lane (lanes byte-offset byte-val)
  "XOR a single byte into the lanes array at the given byte offset (little-endian)."
  (declare (type (simple-array (unsigned-byte 64) (25)) lanes)
           (type fixnum byte-offset)
           (type (unsigned-byte 8) byte-val))
  (let* ((lane-idx (floor byte-offset 8))
         (bit-offset (* (mod byte-offset 8) 8)))
    (setf (aref lanes lane-idx)
          (logand +u64-mask+
                  (logxor (aref lanes lane-idx)
                          (ash byte-val bit-offset))))))

(defun keccak-absorb (state data start end)
  "Absorb bytes into the sponge."
  (declare (type keccak-state state)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end))
  (let ((lanes (keccak-state-lanes state))
        (rate (keccak-state-rate-bytes state))
        (pos (keccak-state-absorbed state)))
    (loop for i from start below end
          do (xor-byte-into-lane lanes pos (aref data i))
             (incf pos)
             (when (= pos rate)
               (keccak-f lanes)
               (setf pos 0)))
    (setf (keccak-state-absorbed state) pos))
  state)

(defun keccak-pad-and-permute (state)
  "Apply padding and final permutation."
  (let ((lanes (keccak-state-lanes state))
        (rate (keccak-state-rate-bytes state))
        (pos (keccak-state-absorbed state))
        (dsep (keccak-state-domain-sep state)))
    ;; Padding: domain-sep byte, then zeros, then 0x80 at end of rate block
    (xor-byte-into-lane lanes pos dsep)
    (xor-byte-into-lane lanes (1- rate) #x80)
    (keccak-f lanes)
    (setf (keccak-state-squeezed state) t)
    (setf (keccak-state-squeeze-pos state) 0)))

(defun extract-byte-from-lane (lanes byte-offset)
  "Extract a single byte from lanes at the given byte offset (little-endian)."
  (declare (type (simple-array (unsigned-byte 64) (25)) lanes)
           (type fixnum byte-offset))
  (let* ((lane-idx (floor byte-offset 8))
         (bit-offset (* (mod byte-offset 8) 8)))
    (logand #xFF (ash (aref lanes lane-idx) (- bit-offset)))))

(defun keccak-squeeze (state output-len)
  "Squeeze output-len bytes from the sponge."
  (unless (keccak-state-squeezed state)
    (keccak-pad-and-permute state))
  (let ((lanes (keccak-state-lanes state))
        (rate (keccak-state-rate-bytes state))
        (pos (keccak-state-squeeze-pos state))
        (output (make-array output-len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below output-len
          do (setf (aref output i) (extract-byte-from-lane lanes pos))
             (incf pos)
             (when (= pos rate)
               (keccak-f lanes)
               (setf pos 0)))
    (setf (keccak-state-squeeze-pos state) pos)
    output))

;;; ---------------------------------------------------------------------------
;;; SHA3-256
;;; ---------------------------------------------------------------------------

(defun make-sha3-256-state ()
  "Create a SHA3-256 state (rate=1088 bits = 136 bytes)."
  (make-keccak-state 136 #x06))

(defun sha3-256-update (state data &key (start 0) (end nil))
  (let ((end (or end (length data))))
    (keccak-absorb state data start end))
  state)

(defun sha3-256-finalize (state)
  "Finalize and return 32-byte digest."
  (keccak-squeeze state 32))

(defun sha3-256-copy (state) (keccak-copy state))

(defun sha3-256 (data &key (start 0) (end nil))
  "Compute SHA3-256 of DATA. Returns 32-byte digest."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-sha3-256-state)))
    (sha3-256-update state data :start start :end end)
    (sha3-256-finalize state)))

(defun sha3-256-hex (data &key (start 0) (end nil))
  (let ((digest (sha3-256 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))

;;; ---------------------------------------------------------------------------
;;; SHA3-384
;;; ---------------------------------------------------------------------------

(defun make-sha3-384-state ()
  "Create a SHA3-384 state (rate=832 bits = 104 bytes)."
  (make-keccak-state 104 #x06))

(defun sha3-384-update (state data &key (start 0) (end nil))
  (let ((end (or end (length data))))
    (keccak-absorb state data start end))
  state)

(defun sha3-384-finalize (state)
  (keccak-squeeze state 48))

(defun sha3-384-copy (state) (keccak-copy state))

(defun sha3-384 (data &key (start 0) (end nil))
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-sha3-384-state)))
    (sha3-384-update state data :start start :end end)
    (sha3-384-finalize state)))

(defun sha3-384-hex (data &key (start 0) (end nil))
  (let ((digest (sha3-384 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))

;;; ---------------------------------------------------------------------------
;;; SHA3-512
;;; ---------------------------------------------------------------------------

(defun make-sha3-512-state ()
  "Create a SHA3-512 state (rate=576 bits = 72 bytes)."
  (make-keccak-state 72 #x06))

(defun sha3-512-update (state data &key (start 0) (end nil))
  (let ((end (or end (length data))))
    (keccak-absorb state data start end))
  state)

(defun sha3-512-finalize (state)
  (keccak-squeeze state 64))

(defun sha3-512-copy (state) (keccak-copy state))

(defun sha3-512 (data &key (start 0) (end nil))
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-sha3-512-state)))
    (sha3-512-update state data :start start :end end)
    (sha3-512-finalize state)))

(defun sha3-512-hex (data &key (start 0) (end nil))
  (let ((digest (sha3-512 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))

;;; ---------------------------------------------------------------------------
;;; SHAKE128 (XOF)
;;; ---------------------------------------------------------------------------

(defun make-shake128-state ()
  "Create a SHAKE128 state (rate=1344 bits = 168 bytes)."
  (make-keccak-state 168 #x1F))

(defun shake128-update (state data &key (start 0) (end nil))
  (let ((end (or end (length data))))
    (keccak-absorb state data start end))
  state)

(defun shake128-squeeze (state output-len)
  "Squeeze OUTPUT-LEN bytes from SHAKE128."
  (keccak-squeeze state output-len))

(defun shake128 (data output-len &key (start 0) (end nil))
  "Compute SHAKE128 of DATA with given output length."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-shake128-state)))
    (shake128-update state data :start start :end end)
    (shake128-squeeze state output-len)))

;;; ---------------------------------------------------------------------------
;;; SHAKE256 (XOF)
;;; ---------------------------------------------------------------------------

(defun make-shake256-state ()
  "Create a SHAKE256 state (rate=1088 bits = 136 bytes)."
  (make-keccak-state 136 #x1F))

(defun shake256-update (state data &key (start 0) (end nil))
  (let ((end (or end (length data))))
    (keccak-absorb state data start end))
  state)

(defun shake256-squeeze (state output-len)
  "Squeeze OUTPUT-LEN bytes from SHAKE256."
  (keccak-squeeze state output-len))

(defun shake256 (data output-len &key (start 0) (end nil))
  "Compute SHAKE256 of DATA with given output length."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-shake256-state)))
    (shake256-update state data :start start :end end)
    (shake256-squeeze state output-len)))
