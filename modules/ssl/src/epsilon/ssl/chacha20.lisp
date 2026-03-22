;;;; ChaCha20 (RFC 8439)
;;;;
;;;; Pure-Lisp implementation of the ChaCha20 stream cipher.
;;;; Uses 256-bit key, 96-bit nonce, 32-bit counter.

(defpackage epsilon.ssl.chacha20
  (:use :cl :epsilon.ssl.primitives)
  (:export
   #:chacha20-encrypt
   #:chacha20-block))

(in-package :epsilon.ssl.chacha20)

;;; ---------------------------------------------------------------------------
;;; ChaCha20 Quarter Round (RFC 8439 Section 2.1)
;;; ---------------------------------------------------------------------------

(defun quarter-round (state a b c d)
  "Apply the ChaCha20 quarter round to state indices A, B, C, D."
  (declare (type (simple-array (unsigned-byte 32) (*)) state))
  ;; a += b; d ^= a; d <<<= 16;
  (setf (aref state a) (u32+ (aref state a) (aref state b)))
  (setf (aref state d) (u32-rotl (logxor (aref state d) (aref state a)) 16))
  ;; c += d; b ^= c; b <<<= 12;
  (setf (aref state c) (u32+ (aref state c) (aref state d)))
  (setf (aref state b) (u32-rotl (logxor (aref state b) (aref state c)) 12))
  ;; a += b; d ^= a; d <<<= 8;
  (setf (aref state a) (u32+ (aref state a) (aref state b)))
  (setf (aref state d) (u32-rotl (logxor (aref state d) (aref state a)) 8))
  ;; c += d; b ^= c; b <<<= 7;
  (setf (aref state c) (u32+ (aref state c) (aref state d)))
  (setf (aref state b) (u32-rotl (logxor (aref state b) (aref state c)) 7)))

;;; ---------------------------------------------------------------------------
;;; ChaCha20 Block Function (RFC 8439 Section 2.3)
;;; ---------------------------------------------------------------------------

(defun chacha20-block (key counter nonce)
  "Generate a 64-byte keystream block from KEY (32 bytes), COUNTER (u32), NONCE (12 bytes).
   Returns a 64-byte array."
  (declare (type (simple-array (unsigned-byte 8) (*)) key nonce)
           (type (unsigned-byte 32) counter))
  (let ((state (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; Constants "expand 32-byte k"
    (setf (aref state 0) #x61707865)  ; "expa"
    (setf (aref state 1) #x3320646e)  ; "nd 3"
    (setf (aref state 2) #x79622d32)  ; "2-by"
    (setf (aref state 3) #x6b206574)  ; "te k"
    ;; Key (8 words, little-endian)
    (loop for i from 0 below 8
          for off = (* i 4)
          do (setf (aref state (+ 4 i))
                   (logior (aref key off)
                           (ash (aref key (+ off 1)) 8)
                           (ash (aref key (+ off 2)) 16)
                           (ash (aref key (+ off 3)) 24))))
    ;; Counter
    (setf (aref state 12) counter)
    ;; Nonce (3 words, little-endian)
    (loop for i from 0 below 3
          for off = (* i 4)
          do (setf (aref state (+ 13 i))
                   (logior (aref nonce off)
                           (ash (aref nonce (+ off 1)) 8)
                           (ash (aref nonce (+ off 2)) 16)
                           (ash (aref nonce (+ off 3)) 24))))
    ;; Save initial state
    (let ((initial (copy-seq state)))
      ;; 20 rounds = 10 double-rounds
      (loop repeat 10
            do ;; Column rounds
               (quarter-round state 0 4  8 12)
               (quarter-round state 1 5  9 13)
               (quarter-round state 2 6 10 14)
               (quarter-round state 3 7 11 15)
               ;; Diagonal rounds
               (quarter-round state 0 5 10 15)
               (quarter-round state 1 6 11 12)
               (quarter-round state 2 7  8 13)
               (quarter-round state 3 4  9 14))
      ;; Add initial state
      (loop for i from 0 below 16
            do (setf (aref state i) (u32+ (aref state i) (aref initial i))))
      ;; Serialize as little-endian bytes
      (let ((output (make-array 64 :element-type '(unsigned-byte 8))))
        (loop for i from 0 below 16
              for off = (* i 4)
              for w = (aref state i)
              do (setf (aref output off) (logand #xFF w))
                 (setf (aref output (+ off 1)) (logand #xFF (ash w -8)))
                 (setf (aref output (+ off 2)) (logand #xFF (ash w -16)))
                 (setf (aref output (+ off 3)) (logand #xFF (ash w -24))))
        output))))

;;; ---------------------------------------------------------------------------
;;; ChaCha20 Encryption (RFC 8439 Section 2.4)
;;; ---------------------------------------------------------------------------

(defun chacha20-encrypt (plaintext key nonce &key (initial-counter 0))
  "Encrypt (or decrypt) PLAINTEXT using ChaCha20.
   KEY: 32 bytes. NONCE: 12 bytes. INITIAL-COUNTER defaults to 0.
   Returns ciphertext of same length."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext key nonce)
           (type (unsigned-byte 32) initial-counter))
  (let* ((len (length plaintext))
         (output (make-array len :element-type '(unsigned-byte 8))))
    (loop for offset from 0 below len by 64
          for counter from initial-counter
          for block-len = (min 64 (- len offset))
          for keystream = (chacha20-block key counter nonce)
          do (loop for i from 0 below block-len
                   do (setf (aref output (+ offset i))
                            (logxor (aref plaintext (+ offset i))
                                    (aref keystream i)))))
    output))
