;;;; PBKDF2 (RFC 2898 / RFC 8018)
;;;;
;;;; Password-Based Key Derivation Function 2.

(defpackage epsilon.ssl.pbkdf2
  (:use :cl)
  (:local-nicknames
   (#:hmac #:epsilon.ssl.hmac))
  (:export
   #:pbkdf2))

(in-package :epsilon.ssl.pbkdf2)

;;; ---------------------------------------------------------------------------
;;; Hash algorithm properties
;;; ---------------------------------------------------------------------------

(defun hash-output-length (hash-algorithm)
  (ecase hash-algorithm
    (:sha256 32)
    (:sha384 48)
    (:sha512 64)))

;;; ---------------------------------------------------------------------------
;;; PBKDF2 (RFC 2898 Section 5.2)
;;; ---------------------------------------------------------------------------

(defun pbkdf2 (hash-algorithm password salt iterations dk-len)
  "Derive DK-LEN bytes of key material from PASSWORD using SALT and ITERATIONS.
   HASH-ALGORITHM is :sha256, :sha384, or :sha512.
   PASSWORD and SALT are byte arrays.
   Returns a byte array of DK-LEN bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) password salt)
           (type (integer 1) iterations dk-len))
  (let* ((h-len (hash-output-length hash-algorithm))
         (num-blocks (ceiling dk-len h-len))
         (dk (make-array dk-len :element-type '(unsigned-byte 8) :initial-element 0))
         (dk-offset 0))
    (loop for block-idx from 1 to num-blocks
          do (let* (;; U_1 = HMAC(password, salt || INT_32_BE(block-idx))
                    (salt-block (make-array (+ (length salt) 4)
                                            :element-type '(unsigned-byte 8)
                                            :initial-element 0))
                    (u nil)
                    (t-block nil))
               ;; Construct salt || INT_32_BE(i)
               (replace salt-block salt)
               (let ((offset (length salt)))
                 (setf (aref salt-block offset)       (logand #xFF (ash block-idx -24)))
                 (setf (aref salt-block (+ offset 1)) (logand #xFF (ash block-idx -16)))
                 (setf (aref salt-block (+ offset 2)) (logand #xFF (ash block-idx -8)))
                 (setf (aref salt-block (+ offset 3)) (logand #xFF block-idx)))
               ;; U_1
               (setf u (hmac:hmac hash-algorithm password salt-block))
               (setf t-block (copy-seq u))
               ;; U_2 .. U_c
               (loop for iter from 2 to iterations
                     do (setf u (hmac:hmac hash-algorithm password u))
                        (loop for j from 0 below h-len
                              do (setf (aref t-block j)
                                       (logxor (aref t-block j) (aref u j)))))
               ;; Copy T_i to DK
               (let ((copy-len (min h-len (- dk-len dk-offset))))
                 (replace dk t-block :start1 dk-offset :end1 (+ dk-offset copy-len))
                 (incf dk-offset copy-len))))
    dk))
