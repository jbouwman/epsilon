;;;; PKCS#12 proprietary key derivation function (RFC 7292 Appendix B.2)
;;;;
;;;; This KDF is used by PKCS#12 (`.p12` / `.pfx`) files to derive
;;;; encryption keys, IVs, and MAC keys from a password. Modern PKCS#12
;;;; files can alternatively use PBES2/PBKDF2 for encryption and PBMAC1
;;;; for integrity, but the classic "PKCS#12 KDF" with SHA-1 or SHA-256
;;;; remains the common on-the-wire format produced by e.g. OpenSSL.
;;;;
;;;; Reference: RFC 7292 Appendix B.2.

(defpackage epsilon.crypto.pkcs12-kdf
  (:use :cl)
  (:import (epsilon.crypto.sha1 sha1)
            (epsilon.crypto.sha256 sha256))
  (:export
   #:pkcs12-kdf
   #:password-to-bmp-string
   #:+pkcs12-id-key+
   #:+pkcs12-id-iv+
   #:+pkcs12-id-mac+))

(in-package :epsilon.crypto.pkcs12-kdf)

(defconstant +pkcs12-id-key+ 1
  "PKCS#12 KDF ID byte for cipher key material (RFC 7292 Appendix B.2).")

(defconstant +pkcs12-id-iv+ 2
  "PKCS#12 KDF ID byte for cipher initialisation vector.")

(defconstant +pkcs12-id-mac+ 3
  "PKCS#12 KDF ID byte for MAC key material.")

;;; ---------------------------------------------------------------------------
;;; Password encoding
;;; ---------------------------------------------------------------------------

(defun password-to-bmp-string (password)
  "Encode a Lisp string as a PKCS#12 BMPString: UTF-16BE followed by a
   two-byte null terminator. PKCS#12 passwords are always encoded this
   way before being passed to the KDF (RFC 7292 Appendix B.1).

   Non-BMP (supplementary-plane) code points are not currently handled;
   signal an error rather than produce a silently-wrong encoding."
  (let* ((chars (map 'vector #'char-code password))
         (out (make-array (+ (* 2 (length chars)) 2)
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (loop for i from 0 below (length chars)
          for code = (aref chars i)
          do (when (> code #xFFFF)
               (error "password-to-bmp-string: code point U+~X is outside ~
                       the BMP and cannot be encoded as a PKCS#12 BMPString"
                      code))
             (setf (aref out (* 2 i)) (ldb (byte 8 8) code))
             (setf (aref out (1+ (* 2 i))) (ldb (byte 8 0) code)))
    ;; Trailing U+0000 pair is already present (initial-element 0).
    out))

;;; ---------------------------------------------------------------------------
;;; Hash abstraction
;;; ---------------------------------------------------------------------------

(defstruct hash-info
  (name :sha1 :type keyword)
  (v 64 :type fixnum)     ; block size in bytes
  (u 20 :type fixnum)     ; output size in bytes
  (fn nil :type function)) ; one-shot hash function (bytes -> bytes)

(defun resolve-hash (kind)
  "Look up a hash descriptor by keyword (:sha1 or :sha256)."
  (ecase kind
    (:sha1
     (make-hash-info :name :sha1 :v 64 :u 20 :fn #'sha1:sha1))
    (:sha256
     (make-hash-info :name :sha256 :v 64 :u 32 :fn #'sha256:sha256))))

;;; ---------------------------------------------------------------------------
;;; Expansion helpers
;;; ---------------------------------------------------------------------------

(defun expand-to-multiple (bytes v)
  "Expand BYTES by repetition into a byte vector whose length is the
   smallest multiple of V that is at least as long as BYTES. If BYTES
   is empty, returns an empty vector."
  (let ((len (length bytes)))
    (cond
      ((zerop len)
       (make-array 0 :element-type '(unsigned-byte 8)))
      (t
       (let* ((padded-len (* v (ceiling len v)))
              (out (make-array padded-len :element-type '(unsigned-byte 8))))
         (loop for i from 0 below padded-len
               do (setf (aref out i) (aref bytes (mod i len))))
         out)))))

(defun hash-iterated (hash-fn input iterations)
  "Apply HASH-FN to INPUT once, then repeatedly to each output, for a
   total of ITERATIONS hash calls. Returns the final digest."
  (let ((out (funcall hash-fn input)))
    (loop repeat (1- iterations)
          do (setf out (funcall hash-fn out)))
    out))

(defun add-mod-2^v (block b)
  "BLOCK and B are both byte vectors of length V treated as big-endian
   unsigned integers. Compute (BLOCK + B + 1) mod 2^(8V) and write the
   result back into BLOCK in place. Returns BLOCK for chaining."
  (let* ((v (length block))
         (carry 1))
    (declare (type fixnum carry))
    (loop for i from (1- v) downto 0
          for sum = (+ (aref block i) (aref b i) carry)
          do (setf (aref block i) (logand sum #xff))
             (setf carry (ash sum -8)))
    block))

;;; ---------------------------------------------------------------------------
;;; The KDF itself
;;; ---------------------------------------------------------------------------

(defun pkcs12-kdf (password salt id n iterations &key (hash :sha1))
  "PKCS#12 key derivation function (RFC 7292 Appendix B.2).

   PASSWORD is a byte vector. For a human string, pass it through
   `password-to-bmp-string` first.
   SALT is the salt byte vector.
   ID is the purpose byte: 1 for cipher key, 2 for IV, 3 for MAC key.
   N is the number of output bytes desired.
   ITERATIONS is the number of hash iterations per block (c in the RFC).
   HASH is :sha1 (default) or :sha256.

   Returns an N-byte vector."
  (let* ((info (resolve-hash hash))
         (v (hash-info-v info))
         (u (hash-info-u info))
         (hash-fn (hash-info-fn info))
         (d (make-array v :element-type '(unsigned-byte 8) :initial-element id))
         ;; I = S-expanded || P-expanded.
         (s-expanded (expand-to-multiple salt v))
         (p-expanded (expand-to-multiple password v))
         (i-block (concatenate '(simple-array (unsigned-byte 8) (*))
                               s-expanded p-expanded))
         (out (make-array n :element-type '(unsigned-byte 8)))
         (produced 0))
    (loop while (< produced n) do
      (let* ((a (hash-iterated hash-fn
                               (concatenate '(simple-array (unsigned-byte 8) (*))
                                            d i-block)
                               iterations))
             (take (min u (- n produced))))
        ;; Copy this block's contribution into OUT.
        (replace out a :start1 produced :end1 (+ produced take)
                       :end2 take)
        (incf produced take)
        ;; If more output is needed, update I by adding (A-tiled + 1)
        ;; to each v-byte block of I as a big-endian unsigned integer.
        (when (< produced n)
          (let ((b (make-array v :element-type '(unsigned-byte 8))))
            ;; B = v bytes of A tiled.
            (loop for k from 0 below v
                  do (setf (aref b k) (aref a (mod k u))))
            ;; Update each v-byte block of I in place.
            (loop for off from 0 below (length i-block) by v do
              (let ((slice (make-array v :element-type '(unsigned-byte 8))))
                (replace slice i-block :start2 off :end2 (+ off v))
                (add-mod-2^v slice b)
                (replace i-block slice :start1 off :end1 (+ off v))))))))
    out))
