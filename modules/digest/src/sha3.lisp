(defpackage :epsilon.digest.sha3
  (:use
   :cl
   :epsilon.syntax
   :epsilon.type
   :epsilon.symbol
   :epsilon.digest.common
   :epsilon.digest.generic
   :epsilon.digest.reader)
  (:export
   :make-sha3-256-digest))

;;; Implementation of SHA3-256 (Keccak)
;;; Based on FIPS 202 specification

(in-package :epsilon.digest.sha3)

(in-ironclad-readtable)

;;; Constants and parameters

(defconstant +keccak-rounds+ 24)
(defconstant +state-lanes+ 25)  ; 5x5 array
(defconstant +lane-bits+ 64)
(defconstant +state-bits+ 1600)  ; 25 * 64

;; SHA3-256 specific parameters
(defconstant +sha3-256-rate-bits+ 1088)  ; r = 1088 bits
(defconstant +sha3-256-rate-bytes+ 136)  ; r/8 = 136 bytes
(defconstant +sha3-256-capacity+ 512)    ; c = 512 bits
(defconstant +sha3-256-output-bytes+ 32) ; 256 bits / 8

;; Round constants for iota step
(define-constant +round-constants+
  (make-array 24 :element-type '(unsigned-byte 64)
                 :initial-contents
                 '(#x0000000000000001 #x0000000000008082
                   #x800000000000808a #x8000000080008000
                   #x000000000000808b #x0000000080000001
                   #x8000000080008081 #x8000000000008009
                   #x000000000000008a #x0000000000000088
                   #x0000000080008009 #x000000008000000a
                   #x000000008000808b #x800000000000008b
                   #x8000000000008089 #x8000000000008003
                   #x8000000000008002 #x8000000000000080
                   #x000000000000800a #x800000008000000a
                   #x8000000080008081 #x8000000000008080
                   #x0000000080000001 #x8000000080008008)))

;; Rotation offsets for rho step (r[x,y] values)
(define-constant +rotation-offsets+
  (make-array '(5 5) :element-type '(unsigned-byte 8)
                     :initial-contents
                     '(( 0 36  3 41 18)
                       ( 1 44 10 45  2)
                       (62  6 43 15 61)
                       (28 55 25 21 56)
                       (27 20 39  8 14))))

;;; State type and accessors

(deftype keccak-lane () '(unsigned-byte 64))
(deftype keccak-state () '(simple-array (unsigned-byte 64) (25)))

(declaim (inline make-keccak-state))
(defun make-keccak-state ()
  "Create a new Keccak state initialized to zeros"
  (make-array +state-lanes+ :element-type 'keccak-lane :initial-element 0))

(declaim (inline state-ref))
(defun state-ref (state x y)
  "Access state[x,y] where state is viewed as 5x5 array"
  (declare (type keccak-state state)
           (type (integer 0 4) x y))
  (aref state (+ x (* 5 y))))

(declaim (inline (setf state-ref)))
(defun (setf state-ref) (value state x y)
  "Set state[x,y] where state is viewed as 5x5 array"
  (declare (type keccak-state state)
           (type (integer 0 4) x y)
           (type keccak-lane value))
  (setf (aref state (+ x (* 5 y))) value))

;;; Keccak-f[1600] permutation steps

(defun theta (state)
  "Theta step: Column parity"
  (declare (type keccak-state state))
  (let ((c (make-array 5 :element-type 'keccak-lane))
        (d (make-array 5 :element-type 'keccak-lane)))
    ;; Compute column parities
    (dotimes (x 5)
      (setf (aref c x)
            (logxor (state-ref state x 0)
                    (state-ref state x 1)
                    (state-ref state x 2)
                    (state-ref state x 3)
                    (state-ref state x 4))))
    ;; Compute D values
    (dotimes (x 5)
      (setf (aref d x)
            (logxor (aref c (mod (1- x) 5))
                    (rol64 (aref c (mod (1+ x) 5)) 1))))
    ;; Apply to state
    (dotimes (x 5)
      (dotimes (y 5)
        (setf (state-ref state x y)
              (logxor (state-ref state x y) (aref d x)))))))

(defun rho (state)
  "Rho step: Rotations"
  (declare (type keccak-state state))
  (dotimes (x 5)
    (dotimes (y 5)
      (let ((offset (aref +rotation-offsets+ x y)))
        (unless (and (= x 0) (= y 0))  ; Don't rotate [0,0]
          (setf (state-ref state x y)
                (rol64 (state-ref state x y) offset)))))))

(defun keccak-pi (state)
  "Pi step: Permutation"
  (declare (type keccak-state state))
  (let ((temp (make-keccak-state)))
    ;; Copy current state
    (dotimes (i 25)
      (setf (aref temp i) (aref state i)))
    ;; Apply permutation: (x,y) -> (y, 2x+3y mod 5)
    (dotimes (x 5)
      (dotimes (y 5)
        (setf (state-ref state x y)
              (state-ref temp (mod (+ x (* 3 y)) 5) x))))))

(defun chi (state)
  "Chi step: Non-linear transformation"
  (declare (type keccak-state state))
  (dotimes (y 5)
    (let ((temp (make-array 5 :element-type 'keccak-lane)))
      ;; Save row
      (dotimes (x 5)
        (setf (aref temp x) (state-ref state x y)))
      ;; Apply chi: a[x] = a[x] XOR ((NOT a[x+1]) AND a[x+2])
      (dotimes (x 5)
        (setf (state-ref state x y)
              (logxor (aref temp x)
                      (logand (lognot (aref temp (mod (1+ x) 5)))
                              (aref temp (mod (+ x 2) 5)))))))))

(defun iota (state round)
  "Iota step: Add round constant"
  (declare (type keccak-state state)
           (type (integer 0 23) round))
  (setf (state-ref state 0 0)
        (logxor (state-ref state 0 0)
                (aref +round-constants+ round))))

(defun keccak-f (state)
  "Complete Keccak-f[1600] permutation (24 rounds)"
  (declare (type keccak-state state))
  (dotimes (round +keccak-rounds+)
    (theta state)
    (rho state)
    (keccak-pi state)
    (chi state)
    (iota state round)))

;;; Sponge construction helpers

(defun absorb-block (state block rate-bytes)
  "XOR a block into the state (absorb)"
  (declare (type keccak-state state)
           (type ->u8 block)
           (type (integer 0 200) rate-bytes))
  ;; XOR block into state, lane by lane
  (dotimes (lane-idx (floor rate-bytes 8))
    (let ((lane-value 0))
      (declare (type (unsigned-byte 64) lane-value))
      ;; Read 8 bytes as little-endian 64-bit word
      (dotimes (byte-idx 8)
        (let ((block-idx (+ (* lane-idx 8) byte-idx)))
          (when (< block-idx rate-bytes)
            (setf lane-value 
                  (logior lane-value 
                          (ash (aref block block-idx) (* byte-idx 8)))))))
      ;; XOR into state
      (setf (aref state lane-idx)
            (logxor (aref state lane-idx) lane-value)))))

(defun squeeze-output (state output-bytes)
  "Extract output from state (squeeze)"
  (declare (type keccak-state state)
           (type (integer 0 200) output-bytes))
  (let ((output (make-array output-bytes :element-type 'u8)))
    ;; Extract bytes from state (little-endian)
    (dotimes (byte-idx output-bytes)
      (let* ((lane-idx (floor byte-idx 8))
             (byte-in-lane (mod byte-idx 8))
             (lane-value (aref state lane-idx)))
        (setf (aref output byte-idx)
              (ldb (byte 8 (* byte-in-lane 8)) lane-value))))
    output))

;;; SHA3-256 digest structure

(defstruct (sha3-256
             (:constructor %make-sha3-256-digest nil)
             (:constructor %make-sha3-256-state 
                           (state buffer buffer-index total-bytes))
             (:copier nil)
             (:include mdx))
  (state (make-keccak-state) :type keccak-state :read-only t)
  (total-bytes 0 :type integer))

(defmethod reinitialize-instance ((digest sha3-256) &rest initargs)
  (declare (ignore initargs))
  ;; Reset state to zeros
  (dotimes (i 25)
    (setf (aref (sha3-256-state digest) i) 0))
  (setf (sha3-256-amount digest) 0
        (sha3-256-buffer-index digest) 0
        (sha3-256-total-bytes digest) 0)
  digest)

(defmethod copy-digest ((digest sha3-256) &optional copy)
  (check-type copy (or null sha3-256))
  (cond
    (copy
     (replace (sha3-256-state copy) (sha3-256-state digest))
     (replace (sha3-256-buffer copy) (sha3-256-buffer digest))
     (setf (sha3-256-amount copy) (sha3-256-amount digest)
           (sha3-256-buffer-index copy) (sha3-256-buffer-index digest)
           (sha3-256-total-bytes copy) (sha3-256-total-bytes digest))
     copy)
    (t
     (%make-sha3-256-state (copy-seq (sha3-256-state digest))
                           (copy-seq (sha3-256-buffer digest))
                           (sha3-256-buffer-index digest)
                           (sha3-256-total-bytes digest)))))

(define-digest-updater sha3-256
  "Update SHA3-256 state with input data"
  (flet ((compress (digest-state sequence offset)
           (declare (type sha3-256 digest-state)
                    (type ->u8 sequence)
                    (type array-index offset))
           ;; Process one block
           (let ((block (make-array +sha3-256-rate-bytes+ :element-type 'u8)))
             ;; Copy data to block
             (dotimes (i +sha3-256-rate-bytes+)
               (setf (aref block i) (aref sequence (+ offset i))))
             ;; Absorb block
             (absorb-block (sha3-256-state digest-state) block +sha3-256-rate-bytes+)
             ;; Apply permutation
             (keccak-f (sha3-256-state digest-state)))))
    (declare (dynamic-extent #'compress))
    ;; Use the standard mdx-updater with our rate as block size
    (let ((saved-buffer (sha3-256-buffer state)))
      ;; Temporarily adjust buffer size for our rate
      (setf (slot-value state 'epsilon.digest.generic::buffer)
            (if (= (length saved-buffer) +sha3-256-rate-bytes+)
                saved-buffer
                (make-array +sha3-256-rate-bytes+ :element-type 'u8)))
      ;; Ensure buffer content is preserved if switching
      (when (/= (length saved-buffer) +sha3-256-rate-bytes+)
        (replace (sha3-256-buffer state) saved-buffer 
                 :end1 (min (length saved-buffer) +sha3-256-rate-bytes+)))
      (prog1
          (mdx-updater state #'compress sequence start end)
        ;; Update total bytes
        (incf (sha3-256-total-bytes state) (- end start))))))

(defmethod produce-digest ((state sha3-256) &key digest (digest-start 0))
  "Finalize SHA3-256 and return 32-byte digest"
  (let ((state-copy (copy-digest state)))
    (let ((state-array (sha3-256-state state-copy))
          (buffer (sha3-256-buffer state-copy))
          (buffer-index (sha3-256-buffer-index state-copy)))
      (declare (type keccak-state state-array)
               (type ->u8 buffer)
               (type (integer 0 136) buffer-index))
      
      ;; Create final block with padding
      (let ((final-block (make-array +sha3-256-rate-bytes+ :element-type 'u8
                                                            :initial-element 0)))
        ;; Copy remaining buffer content
        (dotimes (i buffer-index)
          (setf (aref final-block i) (aref buffer i)))
        
        ;; SHA-3 padding: append 0x06, then pad with zeros, then 0x80
        ;; The 0x06 distinguishes SHA-3 from Keccak (which uses 0x01)
        (setf (aref final-block buffer-index) #x06)
        (setf (aref final-block (1- +sha3-256-rate-bytes+))
              (logior (aref final-block (1- +sha3-256-rate-bytes+)) #x80))
        
        ;; Absorb final padded block
        (absorb-block state-array final-block +sha3-256-rate-bytes+)
        (keccak-f state-array))
      
      ;; Squeeze output
      (let ((output (squeeze-output state-array +sha3-256-output-bytes+)))
        ;; If digest buffer provided, copy output there; otherwise return new array
        (if digest
            (progn
              (replace digest output :start1 digest-start)
              digest)
            output)))))

(defdigest sha3-256 :digest-length 32 :block-length 136)

(defun make-sha3-256-digest ()
  "Create a new SHA3-256 digest state"
  (%make-sha3-256-digest))