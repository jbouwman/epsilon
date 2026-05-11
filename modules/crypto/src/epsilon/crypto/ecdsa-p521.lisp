;;;; ECDSA over P-521 with SHA-512 (FIPS 186-4)
;;;;
;;;; Mirror of `epsilon.crypto.ecdsa-p384' for the 521-bit curve. RFC 6979
;;;; §3.2 with HMAC-SHA-512. Note QLEN=521 means the leftmost-bit
;;;; truncation in `bits2int' actually shifts by a non-byte amount.

(defpackage epsilon.crypto.ecdsa-p521
  (:use :cl)
  (:import
   (epsilon.crypto.ec-p521 ec)
   (epsilon.crypto.modular mod-arith)
   (epsilon.crypto.hmac hmac)
   (epsilon.crypto.sha512 sha))
  (:import-from :epsilon.crypto.primitives
                #:int-to-be-bytes #:be-bytes-to-int #:bits2int)
  (:export
   #:ecdsa-p521-sign
   #:ecdsa-p521-verify
   #:ecdsa-p521-public-key-from-private))

(in-package :epsilon.crypto.ecdsa-p521)

(defconstant +n+ ec:+n+)
(defconstant +hash-bytes+ 64)              ; SHA-512 output size
(defconstant +qlen+ 521)                   ; bit length of N

(defun int2octets (x qlen)
  (int-to-be-bytes (mod x +n+) (ceiling qlen 8)))

(defun bits2octets (bytes qlen)
  (int2octets (mod (bits2int bytes qlen) +n+) qlen))

(defun rfc6979-generate-k (private-key hash-bytes)
  "Deterministic nonce per RFC 6979 §3.2 using HMAC-SHA-512."
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
      (setf k-hmac (hmac:hmac-sha512 k-hmac data)))
    (setf v (hmac:hmac-sha512 k-hmac v))
    (let ((data (make-array (+ hlen 1 rlen rlen)
                            :element-type '(unsigned-byte 8))))
      (replace data v)
      (setf (aref data hlen) #x01)
      (replace data x-bytes :start1 (+ hlen 1))
      (replace data h1-bytes :start1 (+ hlen 1 rlen))
      (setf k-hmac (hmac:hmac-sha512 k-hmac data)))
    (setf v (hmac:hmac-sha512 k-hmac v))
    ;; Step h. RFC 6979 §3.2: T accumulates HMAC outputs until
    ;; tlen >= qlen. For P-521 / SHA-512 (qlen=521, hlen=512) a single
    ;; HMAC produces only 512 bits, so we must concatenate two outputs
    ;; before reading a 521-bit candidate.
    (loop
      (let* ((t-buf (make-array 0 :element-type '(unsigned-byte 8))))
        (loop while (< (* 8 (length t-buf)) qlen)
              do (setf v (hmac:hmac-sha512 k-hmac v))
                 (let ((nt (make-array (+ (length t-buf) hlen)
                                       :element-type '(unsigned-byte 8))))
                   (replace nt t-buf)
                   (replace nt v :start1 (length t-buf))
                   (setf t-buf nt)))
        (let ((k-candidate (bits2int t-buf qlen)))
          (when (and (plusp k-candidate) (< k-candidate +n+))
            (return k-candidate))))
      (let ((data (make-array (+ hlen 1)
                              :element-type '(unsigned-byte 8))))
        (replace data v)
        (setf (aref data hlen) #x00)
        (setf k-hmac (hmac:hmac-sha512 k-hmac data)))
      (setf v (hmac:hmac-sha512 k-hmac v)))))

(defun ecdsa-p521-public-key-from-private (private-key)
  "Compute the P-521 public key (point) from an integer private scalar.
   Uses constant-time scalar multiplication."
  (ec:p521-scalar-mul-ct private-key (ec:p521-base-point)))

(defun ecdsa-p521-sign (private-key message)
  "ECDSA over P-521 with SHA-512. Returns (values r s).
   The k * G scalar mul uses `p521-scalar-mul-ct'."
  (let* ((z-bytes (sha:sha512 message))
         (z (mod (bits2int z-bytes +qlen+) +n+))
         (k (rfc6979-generate-k private-key z-bytes))
         (big-r (ec:p521-scalar-mul-ct k (ec:p521-base-point))))
    (multiple-value-bind (rx ry) (ec:p521-point-to-affine big-r)
      (declare (ignore ry))
      (let* ((r (mod rx +n+))
             (k-inv (mod-arith:mod-inv k +n+))
             (s (mod (* k-inv (+ z (* r private-key))) +n+)))
        (when (or (zerop r) (zerop s))
          (error "ECDSA-P521: signature generation produced r=0 or s=0"))
        ;; No low-S normalisation -- see the same note in ecdsa-p384.lisp.
        (values r s)))))

(defun ecdsa-p521-verify (public-key message r s)
  (unless (and (plusp r) (< r +n+) (plusp s) (< s +n+))
    (return-from ecdsa-p521-verify nil))
  (let* ((z-bytes (sha:sha512 message))
         (z (mod (bits2int z-bytes +qlen+) +n+))
         (w (mod-arith:mod-inv s +n+))
         (u1 (mod (* z w) +n+))
         (u2 (mod (* r w) +n+))
         (point1 (ec:p521-scalar-mul u1 (ec:p521-base-point)))
         (point2 (ec:p521-scalar-mul u2 public-key))
         (big-r (ec:p521-point-add point1 point2)))
    (when (ec:p521-point-is-neutral big-r)
      (return-from ecdsa-p521-verify nil))
    (multiple-value-bind (rx ry) (ec:p521-point-to-affine big-r)
      (declare (ignore ry))
      (= (mod rx +n+) r))))
