;;;; Legacy OpenSSL PEM encryption (RFC 1421 header style)
;;;;
;;;; Decrypts PEM blocks that carry `Proc-Type: 4,ENCRYPTED` and
;;;; `DEK-Info: <cipher>,<hex IV>` headers -- the classic OpenSSL `-pass`
;;;; format emitted by `openssl ec -aes-256-cbc`, used on disk for the
;;;; internal CA's encrypted root/intermediate private keys. The key schedule is
;;;; EVP_BytesToKey (RFC 1423-flavoured MD5-based KDF) seeded with the
;;;; password and the first 8 bytes of the IV as salt.
;;;;
;;;; This is a migration aid, not a general-purpose key store. Modern
;;;; code should use `epsilon.crypto.pbes2` (PKCS#8 v2) for encrypted keys.

(defpackage epsilon.crypto.pem-enc
  (:use :cl)
  (:import (epsilon.crypto.aes aes)
            (epsilon.crypto.md5 md5)
            (epsilon.crypto.pem pem))
  (:export
   #:decrypt-legacy-pem
   #:legacy-encrypted-pem-p))

(in-package :epsilon.crypto.pem-enc)

(defun legacy-encrypted-pem-p (block)
  "True if BLOCK carries an RFC 1421 `Proc-Type: 4,ENCRYPTED` header."
  (let ((proc-type (cdr (assoc "Proc-Type" (pem:pem-block-headers block)
                               :test #'string=))))
    (and proc-type (search "ENCRYPTED" proc-type))))

(defun %hex-char-value (c)
  (cond
    ((<= (char-code #\0) (char-code c) (char-code #\9))
     (- (char-code c) (char-code #\0)))
    ((<= (char-code #\a) (char-code c) (char-code #\f))
     (+ 10 (- (char-code c) (char-code #\a))))
    ((<= (char-code #\A) (char-code c) (char-code #\F))
     (+ 10 (- (char-code c) (char-code #\A))))
    (t (error "pem-enc: invalid hex character ~S" c))))

(defun %hex->bytes (string)
  (let ((len (length string)))
    (unless (zerop (mod len 2))
      (error "pem-enc: hex string must have even length (got ~D)" len))
    (let ((bytes (make-array (/ len 2) :element-type '(unsigned-byte 8))))
      (loop for i from 0 below (/ len 2)
            for hi = (%hex-char-value (char string (* 2 i)))
            for lo = (%hex-char-value (char string (1+ (* 2 i))))
            do (setf (aref bytes i) (logior (ash hi 4) lo)))
      bytes)))

(defun %string->utf8 (string)
  (map '(simple-array (unsigned-byte 8) (*))
       #'char-code string))

(defun %concat-bytes (&rest arrays)
  (let* ((total (reduce #'+ arrays :key #'length))
         (out (make-array total :element-type '(unsigned-byte 8)))
         (offset 0))
    (dolist (a arrays out)
      (replace out a :start1 offset)
      (incf offset (length a)))))

(defun %evp-bytes-to-key (password salt key-length)
  "OpenSSL's EVP_BytesToKey with MD5, iterations=1. PASSWORD is a byte
vector, SALT is an 8-byte vector, KEY-LENGTH is the number of key bytes
to derive. Returns (values key iv-tail) where IV-TAIL is any extra
material past KEY-LENGTH -- unused here because the IV already comes
from the DEK-Info header.

  D_i = MD5(D_{i-1} || password || salt)   with D_0 empty.
  key = D_1 || D_2 || ... truncated to KEY-LENGTH."
  (let ((accumulated (make-array 0 :element-type '(unsigned-byte 8)))
        (prev (make-array 0 :element-type '(unsigned-byte 8))))
    (loop while (< (length accumulated) key-length)
          do (let ((digest (md5:md5 (%concat-bytes prev password salt))))
               (setf prev digest
                     accumulated (%concat-bytes accumulated digest))))
    (subseq accumulated 0 key-length)))

(defun %parse-dek-info (header-value)
  "Split `AES-256-CBC,<hex IV>` into (values cipher-name iv-bytes)."
  (let ((comma (position #\, header-value)))
    (unless comma
      (error "pem-enc: malformed DEK-Info header: ~S" header-value))
    (values (string-upcase (subseq header-value 0 comma))
            (%hex->bytes (subseq header-value (1+ comma))))))

(defun %cipher-key-length (cipher)
  (cond
    ((string= cipher "AES-256-CBC") 32)
    ((string= cipher "AES-192-CBC") 24)
    ((string= cipher "AES-128-CBC") 16)
    (t (error "pem-enc: unsupported cipher ~S (only AES-*-CBC is ~
              implemented for legacy PEM)" cipher))))

(defun decrypt-legacy-pem (pem-string password)
  "Decrypt a PEM block that uses the OpenSSL legacy header-style
encryption (Proc-Type: 4,ENCRYPTED). Returns (values der-bytes label)
where DER-BYTES is the plaintext inner structure and LABEL is the PEM
block label, e.g. \"EC PRIVATE KEY\".

PASSWORD may be a string or a byte vector. The caller is responsible
for stripping any trailing newline before passing it in.

Signals an error if the block carries no Proc-Type header, if the
DEK-Info cipher is not a supported AES-CBC variant, or if the padding
fails to validate (usually indicating a wrong password)."
  (let* ((block (pem:pem-decode pem-string)))
    (unless block
      (error "pem-enc: no PEM block in input"))
    (unless (legacy-encrypted-pem-p block)
      (error "pem-enc: PEM block ~S is not legacy-encrypted"
             (pem:pem-block-label block)))
    (let* ((headers (pem:pem-block-headers block))
           (dek-info (cdr (assoc "DEK-Info" headers :test #'string=))))
      (unless dek-info
        (error "pem-enc: Proc-Type: 4,ENCRYPTED without DEK-Info header"))
      (multiple-value-bind (cipher iv) (%parse-dek-info dek-info)
        (let* ((pw-bytes (etypecase password
                           (string (%string->utf8 password))
                           ((vector (unsigned-byte 8)) password)))
               (salt (subseq iv 0 8))
               (key (%evp-bytes-to-key pw-bytes salt
                                       (%cipher-key-length cipher)))
               (ct (pem:pem-block-data block)))
          (values (aes:aes-cbc-decrypt ct key iv)
                  (pem:pem-block-label block)))))))
