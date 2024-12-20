(in-package :epsilon.net.tls)

(defun random-bytes (count)
  "Generates COUNT cryptographically strong pseudo-random bytes. Returns
the bytes as a SIMPLE-ARRAY with ELEMENT-TYPE u8. Signals
an ERROR in case of problems; for example, when the OpenSSL random number
generator has not been seeded with enough randomness to ensure an
unpredictable byte sequence."
  (let* ((result (->u8 count))
         (buf (make-buffer count))
         (ret (with-pointer-to-vector-data (ptr buf)
                (rand-bytes ptr count))))
    (when (/= 1 ret)
      (error "RANDOM-BYTES failed: error reported by the OpenSSL RAND_bytes function. ~A."
             (format-ssl-error-queue nil (read-ssl-error-queue))))
    (s/b-replace result buf)))

;; TODO: Should we define random-specific constants and condition classes for
;; RAND_F_RAND_GET_RAND_METHOD, RAND_F_SSLEAY_RAND_BYTES, RAND_R_PRNG_NOT_SEEDED
;; (defined in the rand.h file of the OpenSSl sources)?
;; Where to place these constants/condtitions, here or in the conditions.lisp?
;; On the other hand, those constants are just numbers defined for C,
;; for now we jsut report human readable strings, without possibility
;; to distinguish these error causes programmatically.
