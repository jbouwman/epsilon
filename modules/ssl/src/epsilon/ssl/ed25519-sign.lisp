;;;; Ed25519 Signature Scheme (RFC 8032 Section 5.1)
;;;;
;;;; Implements EdDSA sign/verify over the Ed25519 curve.
;;;; Uses SHA-512 for hashing per the RFC specification.

(defpackage epsilon.ssl.ed25519-sign
  (:use :cl)
  (:local-nicknames
   (#:ed #:epsilon.ssl.ed25519)
   (#:sha512 #:epsilon.ssl.sha512))
  (:export
   #:ed25519-sign
   #:ed25519-verify
   #:ed25519-public-key-from-private))

(in-package :epsilon.ssl.ed25519-sign)

;;; The group order L
(defconstant +l+ ed:+l+)

(defun clamp-private-key (h)
  "Clamp the first 32 bytes of SHA-512(private-key) per RFC 8032 Section 5.1.5."
  (let ((a (subseq h 0 32)))
    ;; Clear lowest 3 bits
    (setf (aref a 0) (logand (aref a 0) 248))
    ;; Clear highest bit, set second-highest
    (setf (aref a 31) (logior (logand (aref a 31) 127) 64))
    a))

(defun bytes-to-scalar (bytes)
  "Decode little-endian bytes to an integer."
  (let ((n 0))
    (loop for i from (1- (length bytes)) downto 0
          do (setf n (logior (ash n 8) (aref bytes i))))
    n))

(defun scalar-to-bytes (n len)
  "Encode integer N as LEN little-endian bytes."
  (let ((bytes (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from 0 below len
          do (setf (aref bytes i) (logand n #xFF))
             (setf n (ash n -8)))
    bytes))

(defun sha512-modl (data)
  "Compute SHA-512(data) and reduce mod L."
  (mod (bytes-to-scalar (sha512:sha512 data)) +l+))

(defun concat-bytes (&rest arrays)
  "Concatenate byte arrays."
  (let* ((total (reduce #'+ arrays :key #'length))
         (result (make-array total :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (arr arrays result)
      (replace result arr :start1 pos)
      (incf pos (length arr)))))

(defun ed25519-public-key-from-private (private-key)
  "Derive Ed25519 public key from 32-byte private key.
   Returns a 32-byte encoded public key."
  (declare (type (simple-array (unsigned-byte 8) (32)) private-key))
  (let* ((h (sha512:sha512 private-key))
         (a-bytes (clamp-private-key h))
         (a (bytes-to-scalar a-bytes))
         ;; A = a * B
         (big-a (ed:ed-scalar-mul a (ed:ed-base-point))))
    (ed:ed-point-encode big-a)))

(defun ed25519-sign (private-key message)
  "Sign MESSAGE with 32-byte PRIVATE-KEY per RFC 8032 Section 5.1.6.
   Returns a 64-byte signature."
  (declare (type (simple-array (unsigned-byte 8) (32)) private-key))
  (let* ((h (sha512:sha512 private-key))
         (a-bytes (clamp-private-key h))
         (a (bytes-to-scalar a-bytes))
         ;; prefix = second 32 bytes of h
         (prefix (subseq h 32 64))
         ;; A = a * B (public key point)
         (big-a (ed:ed-scalar-mul a (ed:ed-base-point)))
         (pub-bytes (ed:ed-point-encode big-a))
         ;; r = SHA-512(prefix || message) mod L
         (r (sha512-modl (concat-bytes prefix message)))
         ;; R = r * B
         (big-r (ed:ed-scalar-mul r (ed:ed-base-point)))
         (r-bytes (ed:ed-point-encode big-r))
         ;; S = (r + SHA-512(R || A || message) * a) mod L
         (k (sha512-modl (concat-bytes r-bytes pub-bytes message)))
         (s (mod (+ r (* k a)) +l+))
         (s-bytes (scalar-to-bytes s 32)))
    ;; Signature = R || S (64 bytes)
    (concat-bytes r-bytes s-bytes)))

(defun ed25519-verify (public-key message signature)
  "Verify a 64-byte SIGNATURE on MESSAGE with 32-byte PUBLIC-KEY.
   Returns T if valid, NIL otherwise."
  (declare (type (simple-array (unsigned-byte 8) (32)) public-key)
           (type (simple-array (unsigned-byte 8) (64)) signature))
  ;; Decode public key
  (let ((big-a (ed:ed-point-decode public-key)))
    (unless big-a
      (return-from ed25519-verify nil))
    ;; Extract R and S from signature
    (let* ((r-bytes (subseq signature 0 32))
           (s-bytes (subseq signature 32 64))
           (big-r (ed:ed-point-decode r-bytes))
           (s (bytes-to-scalar s-bytes)))
      (unless big-r
        (return-from ed25519-verify nil))
      ;; Check S < L
      (when (>= s +l+)
        (return-from ed25519-verify nil))
      ;; k = SHA-512(R || A || message) mod L
      (let* ((k (sha512-modl (concat-bytes r-bytes public-key message)))
             ;; Check: S*B = R + k*A
             (lhs (ed:ed-scalar-mul s (ed:ed-base-point)))
             (rhs (ed:ed-point-add big-r (ed:ed-scalar-mul k big-a))))
        (ed:ed-point-equal lhs rhs)))))
