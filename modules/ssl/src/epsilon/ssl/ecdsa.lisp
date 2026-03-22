;;;; ECDSA Digital Signature Algorithm
;;;;
;;;; Implements ECDSA sign/verify over P-256 with deterministic
;;;; nonce generation per RFC 6979.

(defpackage epsilon.ssl.ecdsa
  (:use :cl)
  (:local-nicknames
   (#:ec #:epsilon.ssl.ec-p256)
   (#:field #:epsilon.ssl.field-p256)
   (#:mod-arith #:epsilon.ssl.modular)
   (#:hmac #:epsilon.ssl.hmac)
   (#:sha256 #:epsilon.ssl.sha256))
  (:export
   #:ecdsa-sign
   #:ecdsa-verify
   #:ecdsa-public-key-from-private))

(in-package :epsilon.ssl.ecdsa)

(defconstant +n+ ec:+n+)
(defconstant +p+ ec:+p+)

(defun int-to-bytes (n len)
  "Encode integer N as LEN big-endian bytes."
  (let ((bytes (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from (1- len) downto 0
          do (setf (aref bytes i) (logand n #xFF))
             (setf n (ash n -8)))
    bytes))

(defun bytes-to-int (bytes)
  "Decode big-endian bytes to integer."
  (let ((n 0))
    (loop for b across bytes
          do (setf n (logior (ash n 8) b)))
    n))

;;; ---------------------------------------------------------------------------
;;; RFC 6979 Deterministic Nonce Generation
;;; ---------------------------------------------------------------------------

(defun bits2int (bytes qlen)
  "Convert byte sequence to integer, right-shifting if bit length > qlen."
  (let ((blen (* 8 (length bytes)))
        (x (bytes-to-int bytes)))
    (if (> blen qlen)
        (ash x (- qlen blen))
        x)))

(defun int2octets (x qlen)
  "Encode integer X as ceil(qlen/8) big-endian bytes."
  (int-to-bytes (mod x +n+) (ceiling qlen 8)))

(defun bits2octets (bytes qlen)
  "Convert bit string to octet string reduced mod n."
  (let ((z1 (bits2int bytes qlen)))
    (int2octets (mod z1 +n+) qlen)))

(defun rfc6979-generate-k (private-key hash-bytes)
  "Generate deterministic nonce k per RFC 6979 Section 3.2.
   Uses HMAC-SHA256."
  (let* ((qlen (integer-length +n+))
         (hlen 32) ; SHA-256
         (x-bytes (int2octets private-key qlen))
         (h1-bytes (bits2octets hash-bytes qlen))
         (rlen (length x-bytes))
         ;; Step a: h1 = hash(message) -- already provided
         ;; Step b: V = 0x01 0x01 ... (hlen bytes)
         (v (make-array hlen :element-type '(unsigned-byte 8) :initial-element #x01))
         ;; Step c: K = 0x00 0x00 ... (hlen bytes)
         (k-hmac (make-array hlen :element-type '(unsigned-byte 8) :initial-element #x00)))
    ;; Step d: K = HMAC_K(V || 0x00 || int2octets(x) || bits2octets(h1))
    (let ((data (make-array (+ hlen 1 rlen rlen) :element-type '(unsigned-byte 8))))
      (replace data v)
      (setf (aref data hlen) #x00)
      (replace data x-bytes :start1 (+ hlen 1))
      (replace data h1-bytes :start1 (+ hlen 1 rlen))
      (setf k-hmac (hmac:hmac-sha256 k-hmac data)))
    ;; Step e: V = HMAC_K(V)
    (setf v (hmac:hmac-sha256 k-hmac v))
    ;; Step f: K = HMAC_K(V || 0x01 || int2octets(x) || bits2octets(h1))
    (let ((data (make-array (+ hlen 1 rlen rlen) :element-type '(unsigned-byte 8))))
      (replace data v)
      (setf (aref data hlen) #x01)
      (replace data x-bytes :start1 (+ hlen 1))
      (replace data h1-bytes :start1 (+ hlen 1 rlen))
      (setf k-hmac (hmac:hmac-sha256 k-hmac data)))
    ;; Step g: V = HMAC_K(V)
    (setf v (hmac:hmac-sha256 k-hmac v))
    ;; Step h: Generate k
    (loop
      ;; Step h.1: T = empty
      ;; Step h.2: While tlen < qlen, T = T || HMAC_K(V)
      (setf v (hmac:hmac-sha256 k-hmac v))
      (let ((k-candidate (bits2int v qlen)))
        ;; Step h.3: If 1 <= k < q, return k
        (when (and (plusp k-candidate) (< k-candidate +n+))
          (return k-candidate))
        ;; Otherwise: K = HMAC_K(V || 0x00), V = HMAC_K(V)
        (let ((data (make-array (+ hlen 1) :element-type '(unsigned-byte 8))))
          (replace data v)
          (setf (aref data hlen) #x00)
          (setf k-hmac (hmac:hmac-sha256 k-hmac data)))
        (setf v (hmac:hmac-sha256 k-hmac v))))))

;;; ---------------------------------------------------------------------------
;;; ECDSA operations
;;; ---------------------------------------------------------------------------

(defun ecdsa-public-key-from-private (private-key)
  "Compute the P-256 public key from an integer private key.
   Returns an ec-p256 point."
  (ec:p256-scalar-mul private-key (ec:p256-base-point)))

(defun ecdsa-sign (private-key message &key (hash :sha256))
  "Sign MESSAGE with private key (integer).
   Returns (values r s) as integers."
  (declare (ignore hash)) ; Currently only SHA-256
  (let* ((z-bytes (sha256:sha256 message))
         (z (mod (bytes-to-int z-bytes) +n+))
         (k (rfc6979-generate-k private-key z-bytes))
         ;; R = k * G
         (big-r (ec:p256-scalar-mul k (ec:p256-base-point))))
    ;; r = R.x mod n
    (multiple-value-bind (rx ry) (ec::p256-point-to-affine big-r)
      (declare (ignore ry))
      (let* ((r (mod rx +n+))
             ;; s = k^(-1) * (z + r*d) mod n
             (k-inv (mod-arith:mod-inv k +n+))
             (s (mod (* k-inv (+ z (* r private-key))) +n+)))
        ;; If r or s is 0, we should retry (extremely unlikely with deterministic k)
        (when (or (zerop r) (zerop s))
          (error "ECDSA signature generation failed: r or s is zero"))
        ;; Normalize s to low-S form (BIP-62 / RFC 6979 convention)
        (when (> s (ash +n+ -1))
          (setf s (- +n+ s)))
        (values r s)))))

(defun ecdsa-verify (public-key message r s)
  "Verify an ECDSA signature (r, s) on MESSAGE with PUBLIC-KEY (ec-p256 point).
   Returns T if valid."
  ;; Check r and s are in [1, n-1]
  (unless (and (plusp r) (< r +n+) (plusp s) (< s +n+))
    (return-from ecdsa-verify nil))
  (let* ((z-bytes (sha256:sha256 message))
         (z (mod (bytes-to-int z-bytes) +n+))
         ;; w = s^(-1) mod n
         (w (mod-arith:mod-inv s +n+))
         ;; u1 = z*w mod n, u2 = r*w mod n
         (u1 (mod (* z w) +n+))
         (u2 (mod (* r w) +n+))
         ;; R' = u1*G + u2*Q
         (point1 (ec:p256-scalar-mul u1 (ec:p256-base-point)))
         (point2 (ec:p256-scalar-mul u2 public-key))
         (big-r (ec:p256-point-add point1 point2)))
    ;; Check R' is not the point at infinity
    (when (ec::p256-point-is-neutral big-r)
      (return-from ecdsa-verify nil))
    ;; v = R'.x mod n
    (multiple-value-bind (rx ry) (ec::p256-point-to-affine big-r)
      (declare (ignore ry))
      (= (mod rx +n+) r))))
