;;;; ECDSA over P-384 with SHA-384 (FIPS 186-4)
;;;;
;;;; The structure mirrors `epsilon.crypto.ecdsa` (P-256 + SHA-256). RFC
;;;; 6979 deterministic nonce derivation is parameterised by the curve
;;;; order and the HMAC function.

(defpackage epsilon.crypto.ecdsa-p384
  (:use :cl)
  (:import
   (epsilon.crypto.ec-p384 ec)
   (epsilon.crypto.modular mod-arith)
   (epsilon.crypto.hmac hmac)
   (epsilon.crypto.sha512 sha))
  (:import-from :epsilon.crypto.primitives
                #:int-to-be-bytes #:be-bytes-to-int #:bits2int)
  (:export
   #:ecdsa-p384-sign
   #:ecdsa-p384-verify
   #:ecdsa-p384-public-key-from-private))

(in-package :epsilon.crypto.ecdsa-p384)

(defconstant +n+ ec:+n+)
(defconstant +hash-bytes+ 48)              ; SHA-384 output size
(defconstant +qlen+ 384)                   ; bit length of N

(defun int2octets (x qlen)
  (int-to-be-bytes (mod x +n+) (ceiling qlen 8)))

(defun bits2octets (bytes qlen)
  (int2octets (mod (bits2int bytes qlen) +n+) qlen))

(defun rfc6979-generate-k (private-key hash-bytes)
  "Deterministic nonce per RFC 6979 §3.2 using HMAC-SHA-384."
  (let* ((qlen +qlen+)
         (hlen +hash-bytes+)
         (x-bytes (int2octets private-key qlen))
         (h1-bytes (bits2octets hash-bytes qlen))
         (rlen (length x-bytes))
         (v (make-array hlen :element-type '(unsigned-byte 8)
                        :initial-element #x01))
         (k-hmac (make-array hlen :element-type '(unsigned-byte 8)
                             :initial-element #x00)))
    (let ((data (make-array (+ hlen 1 rlen rlen)
                            :element-type '(unsigned-byte 8))))
      (replace data v)
      (setf (aref data hlen) #x00)
      (replace data x-bytes :start1 (+ hlen 1))
      (replace data h1-bytes :start1 (+ hlen 1 rlen))
      (setf k-hmac (hmac:hmac-sha384 k-hmac data)))
    (setf v (hmac:hmac-sha384 k-hmac v))
    (let ((data (make-array (+ hlen 1 rlen rlen)
                            :element-type '(unsigned-byte 8))))
      (replace data v)
      (setf (aref data hlen) #x01)
      (replace data x-bytes :start1 (+ hlen 1))
      (replace data h1-bytes :start1 (+ hlen 1 rlen))
      (setf k-hmac (hmac:hmac-sha384 k-hmac data)))
    (setf v (hmac:hmac-sha384 k-hmac v))
    (loop
      (setf v (hmac:hmac-sha384 k-hmac v))
      (let ((k-candidate (bits2int v qlen)))
        (when (and (plusp k-candidate) (< k-candidate +n+))
          (return k-candidate))
        (let ((data (make-array (+ hlen 1)
                                :element-type '(unsigned-byte 8))))
          (replace data v)
          (setf (aref data hlen) #x00)
          (setf k-hmac (hmac:hmac-sha384 k-hmac data)))
        (setf v (hmac:hmac-sha384 k-hmac v))))))

(defun ecdsa-p384-public-key-from-private (private-key)
  "Compute the P-384 public key (point) from an integer private scalar.
   Uses constant-time scalar multiplication."
  (ec:p384-scalar-mul-ct private-key (ec:p384-base-point)))

(defun ecdsa-p384-sign (private-key message)
  "Sign MESSAGE under PRIVATE-KEY (integer in [1, n-1]) with ECDSA-P384
   over SHA-384. Returns (values r s) as integers.

   The k * G scalar multiplication uses the constant-time path
   (`p384-scalar-mul-ct`); verification continues to use the faster
   non-CT `p384-scalar-mul' because its scalars are public."
  (let* ((z-bytes (sha:sha384 message))
         (z (mod (bits2int z-bytes +qlen+) +n+))
         (k (rfc6979-generate-k private-key z-bytes))
         (big-r (ec:p384-scalar-mul-ct k (ec:p384-base-point))))
    (multiple-value-bind (rx ry) (ec:p384-point-to-affine big-r)
      (declare (ignore ry))
      (let* ((r (mod rx +n+))
             (k-inv (mod-arith:mod-inv k +n+))
             (s (mod (* k-inv (+ z (* r private-key))) +n+)))
        (when (or (zerop r) (zerop s))
          (error "ECDSA-P384: signature generation produced r=0 or s=0"))
        ;; No low-S normalisation: low-S is a BIP-62 / Bitcoin convention,
        ;; not required by RFC 6979 / FIPS 186-4. Producing the natural S
        ;; lets us match the RFC 6979 test vectors verbatim and stays
        ;; interoperable with FIPS-compliant TLS verifiers.
        (values r s)))))

(defun ecdsa-p384-verify (public-key message r s)
  "Verify (R, S) against MESSAGE and PUBLIC-KEY (P-384 point)."
  (unless (and (plusp r) (< r +n+) (plusp s) (< s +n+))
    (return-from ecdsa-p384-verify nil))
  (let* ((z-bytes (sha:sha384 message))
         (z (mod (bits2int z-bytes +qlen+) +n+))
         (w (mod-arith:mod-inv s +n+))
         (u1 (mod (* z w) +n+))
         (u2 (mod (* r w) +n+))
         (point1 (ec:p384-scalar-mul u1 (ec:p384-base-point)))
         (point2 (ec:p384-scalar-mul u2 public-key))
         (big-r (ec:p384-point-add point1 point2)))
    (when (ec:p384-point-is-neutral big-r)
      (return-from ecdsa-p384-verify nil))
    (multiple-value-bind (rx ry) (ec:p384-point-to-affine big-r)
      (declare (ignore ry))
      (= (mod rx +n+) r))))
