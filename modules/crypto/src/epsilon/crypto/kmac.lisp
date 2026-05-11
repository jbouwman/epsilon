;;;; KMAC and cSHAKE (NIST SP 800-185)
;;;;
;;;; Built on the Keccak[1600,24] permutation already implemented in
;;;; `epsilon.crypto.sha3'. Customizable SHAKE (cSHAKE) is the underlying
;;;; primitive; KMAC is cSHAKE with the function name "KMAC" and a
;;;; specific input encoding that turns it into a keyed MAC.
;;;;
;;;; cSHAKE differs from SHAKE only in two ways:
;;;;   - domain separation byte is 0x04 (instead of 0x1F),
;;;;   - the absorbed input is prefixed with `bytepad(encode_string(N) ||
;;;;     encode_string(S), rate)' where N is the function name and S the
;;;;     user customization string.
;;;; If N and S are both empty, cSHAKE is defined to fall back to SHAKE
;;;; (with the 0x1F byte).
;;;;
;;;; Encoding helpers (left_encode, right_encode, encode_string,
;;;; bytepad) come from NIST SP 800-185 Section 2.3.

(defpackage epsilon.crypto.kmac
  (:use :cl)
  (:import (epsilon.crypto.sha3 sha3))
  (:export
   #:cshake128
   #:cshake256
   #:kmac128
   #:kmac256))

(in-package :epsilon.crypto.kmac)

;;; ---------------------------------------------------------------------------
;;; SP 800-185 string encodings
;;; ---------------------------------------------------------------------------

(defun %byte-length-of (n)
  "Number of bytes required to represent the non-negative integer N
   in unsigned big-endian form. Returns 1 for zero (per left/right_encode)."
  (declare (type (integer 0) n))
  (if (zerop n)
      1
      (loop with bytes = 0
            with x = n
            while (> x 0)
            do (setf x (ash x -8))
               (incf bytes)
            finally (return bytes))))

(defun %integer-to-be-bytes (x num-bytes)
  "Encode integer X as NUM-BYTES big-endian bytes."
  (let ((out (make-array num-bytes :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (loop for i from (1- num-bytes) downto 0
          for shift from 0 by 8
          do (setf (aref out i) (logand #xFF (ash x (- shift)))))
    out))

(defun left-encode (x)
  "left_encode(x) per SP 800-185 §2.3.1.
   Output: <n><x as n bytes BE>, where n is itself a single byte."
  (declare (type (integer 0) x))
  (let* ((n (%byte-length-of x))
         (out (make-array (1+ n) :element-type '(unsigned-byte 8))))
    (setf (aref out 0) n)
    (replace out (%integer-to-be-bytes x n) :start1 1)
    out))

(defun right-encode (x)
  "right_encode(x) per SP 800-185 §2.3.1. Same body as left_encode but
   with the length byte appended at the tail rather than prepended."
  (declare (type (integer 0) x))
  (let* ((n (%byte-length-of x))
         (out (make-array (1+ n) :element-type '(unsigned-byte 8))))
    (replace out (%integer-to-be-bytes x n) :start1 0)
    (setf (aref out n) n)
    out))

(defun encode-string (bytes)
  "encode_string(S) = left_encode(len(S) in bits) || S."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let* ((bit-len (* 8 (length bytes)))
         (prefix (left-encode bit-len))
         (out (make-array (+ (length prefix) (length bytes))
                          :element-type '(unsigned-byte 8))))
    (replace out prefix :start1 0)
    (replace out bytes :start1 (length prefix))
    out))

(defun bytepad (x w)
  "bytepad(X, w) per SP 800-185 §2.3.3. Prepends left_encode(w) and
   right-pads with zero bytes so the total length is a multiple of W bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) x)
           (type (integer 1) w))
  (let* ((prefix (left-encode w))
         (len (+ (length prefix) (length x)))
         (padded-len (* w (ceiling len w)))
         (out (make-array padded-len :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (replace out prefix :start1 0)
    (replace out x :start1 (length prefix))
    out))

;;; ---------------------------------------------------------------------------
;;; cSHAKE
;;; ---------------------------------------------------------------------------

(defun %ensure-bytes (x)
  "Coerce strings to UTF-8-ish byte vectors (ASCII only -- KMAC and
   cSHAKE callers in practice supply ASCII function names and
   customization strings; reject anything outside the BMP)."
  (etypecase x
    (string
     (let ((bytes (make-array (length x) :element-type '(unsigned-byte 8))))
       (loop for i from 0 below (length x)
             for c = (char-code (aref x i))
             do (when (> c 127)
                  (error "non-ASCII in cSHAKE/KMAC name or customization: ~S" x))
                (setf (aref bytes i) c))
       bytes))
    ((simple-array (unsigned-byte 8) (*)) x)
    ((vector (unsigned-byte 8))
     (coerce x '(simple-array (unsigned-byte 8) (*))))))

(defun %cshake (rate-bytes data output-len function-name customization)
  "Underlying cSHAKE driver. RATE-BYTES is 168 (cSHAKE128) or 136
   (cSHAKE256). FUNCTION-NAME (`N') and CUSTOMIZATION (`S') are strings
   or byte vectors. When both are empty cSHAKE collapses to SHAKE."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type (integer 0) output-len))
  (let* ((n-bytes (%ensure-bytes function-name))
         (s-bytes (%ensure-bytes customization)))
    (if (and (zerop (length n-bytes)) (zerop (length s-bytes)))
        ;; cSHAKE(X, L, "", "") = SHAKE(X, L); domain byte is 0x1F.
        (let ((state (sha3:make-keccak-state rate-bytes #x1F)))
          (sha3:keccak-absorb state data 0 (length data))
          (sha3:keccak-squeeze state output-len))
        ;; Otherwise prepend bytepad(encode_string(N) || encode_string(S))
        ;; and use cSHAKE domain byte 0x04.
        (let* ((n-enc (encode-string n-bytes))
               (s-enc (encode-string s-bytes))
               (concat (make-array (+ (length n-enc) (length s-enc))
                                   :element-type '(unsigned-byte 8))))
          (replace concat n-enc :start1 0)
          (replace concat s-enc :start1 (length n-enc))
          (let* ((prefix (bytepad concat rate-bytes))
                 (state (sha3:make-keccak-state rate-bytes #x04)))
            (sha3:keccak-absorb state prefix 0 (length prefix))
            (sha3:keccak-absorb state data 0 (length data))
            (sha3:keccak-squeeze state output-len))))))

(defun cshake128 (data output-len &key (function-name "") (customization ""))
  "cSHAKE128(X, L, N, S) per NIST SP 800-185 §3.3.
   Returns OUTPUT-LEN bytes. With both N and S empty, falls back to SHAKE128."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (%cshake 168 data output-len function-name customization))

(defun cshake256 (data output-len &key (function-name "") (customization ""))
  "cSHAKE256(X, L, N, S) per NIST SP 800-185 §3.3.
   Returns OUTPUT-LEN bytes. With both N and S empty, falls back to SHAKE256."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (%cshake 136 data output-len function-name customization))

;;; ---------------------------------------------------------------------------
;;; KMAC
;;; ---------------------------------------------------------------------------

(defun %kmac (rate-bytes key data output-len customization)
  "KMAC driver per SP 800-185 §4.3.
   Input absorbed by cSHAKE(N=\"KMAC\") is:
     bytepad(encode_string(K), rate) || X || right_encode(L_in_bits)."
  (declare (type (simple-array (unsigned-byte 8) (*)) key data)
           (type (integer 0) output-len))
  (let* ((bit-len (* 8 output-len))
         (key-block (bytepad (encode-string key) rate-bytes))
         (suffix (right-encode bit-len))
         (newx (make-array (+ (length key-block) (length data) (length suffix))
                           :element-type '(unsigned-byte 8))))
    (replace newx key-block :start1 0)
    (replace newx data :start1 (length key-block))
    (replace newx suffix :start1 (+ (length key-block) (length data)))
    (%cshake rate-bytes newx output-len "KMAC" customization)))

(defun kmac128 (key data output-len &key (customization ""))
  "KMAC128(K, X, L, S) per SP 800-185 §4.3.
   Returns an OUTPUT-LEN byte authentication tag over DATA under KEY."
  (declare (type (simple-array (unsigned-byte 8) (*)) key data))
  (%kmac 168 key data output-len customization))

(defun kmac256 (key data output-len &key (customization ""))
  "KMAC256(K, X, L, S) per SP 800-185 §4.3.
   Returns an OUTPUT-LEN byte authentication tag over DATA under KEY."
  (declare (type (simple-array (unsigned-byte 8) (*)) key data))
  (%kmac 136 key data output-len customization))
