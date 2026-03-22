;;;; RSA Cryptographic Operations
;;;;
;;;; Implements RSA key operations, PKCS#1 v1.5 signature verification,
;;;; and RSA-PSS sign/verify per RFC 8017 (PKCS#1 v2.2).
;;;;
;;;; Uses CL native bignums for modular exponentiation.
;;;; CRT (Chinese Remainder Theorem) acceleration for private key operations.

(defpackage epsilon.ssl.rsa
  (:use :cl)
  (:local-nicknames
   (#:mod-arith #:epsilon.ssl.modular)
   (#:sha256 #:epsilon.ssl.sha256)
   (#:sha512 #:epsilon.ssl.sha512)
   (#:sha1 #:epsilon.ssl.sha1))
  (:export
   ;; Key types
   #:rsa-public-key
   #:rsa-private-key
   #:make-rsa-public-key
   #:make-rsa-private-key
   #:rsa-public-key-n
   #:rsa-public-key-e
   #:rsa-private-key-n
   #:rsa-private-key-e
   #:rsa-private-key-d
   #:rsa-private-key-p
   #:rsa-private-key-q
   #:rsa-private-key-dp
   #:rsa-private-key-dq
   #:rsa-private-key-qinv
   #:rsa-key-bits
   ;; Core operations
   #:rsa-encrypt
   #:rsa-decrypt
   ;; PKCS#1 v1.5
   #:pkcs1-v15-verify
   ;; RSA-PSS
   #:rsa-pss-sign
   #:rsa-pss-verify
   ;; Key generation
   #:rsa-generate-key
   #:miller-rabin-prime-p))

(in-package :epsilon.ssl.rsa)

;;; ---------------------------------------------------------------------------
;;; Key structures
;;; ---------------------------------------------------------------------------

(defstruct (rsa-public-key (:constructor %make-rsa-public-key (n e)))
  "RSA public key (n, e)."
  (n 0 :type integer)
  (e 0 :type integer))

(defun make-rsa-public-key (n e)
  (%make-rsa-public-key n e))

(defstruct (rsa-private-key (:constructor %make-rsa-private-key (n e d p q dp dq qinv))
                            (:predicate nil))
  "RSA private key with CRT components."
  (n 0 :type integer)
  (e 0 :type integer)
  (d 0 :type integer)
  ;; CRT components (optional, for faster decryption)
  (p 0 :type integer)
  (q 0 :type integer)
  (dp 0 :type integer)   ; d mod (p-1)
  (dq 0 :type integer)   ; d mod (q-1)
  (qinv 0 :type integer)) ; q^(-1) mod p

(defun make-rsa-private-key (n e d &key (p 0) (q 0) (dp 0) (dq 0) (qinv 0))
  (%make-rsa-private-key n e d p q dp dq qinv))

(defun rsa-key-bits (key)
  "Return the bit length of the RSA modulus."
  (integer-length (if (rsa-public-key-p key)
                      (rsa-public-key-n key)
                      (rsa-private-key-n key))))

;;; ---------------------------------------------------------------------------
;;; Core RSA operations
;;; ---------------------------------------------------------------------------

(defun rsa-encrypt (m pub-key)
  "RSA encryption: c = m^e mod n."
  (mod-arith:mod-expt m (rsa-public-key-e pub-key) (rsa-public-key-n pub-key)))

(defun rsa-decrypt (c priv-key)
  "RSA decryption with CRT acceleration when available."
  (let ((n (rsa-private-key-n priv-key))
        (d (rsa-private-key-d priv-key))
        (p (rsa-private-key-p priv-key))
        (q (rsa-private-key-q priv-key)))
    (if (and (plusp p) (plusp q))
        ;; CRT: m1 = c^dp mod p, m2 = c^dq mod q, h = qinv*(m1-m2) mod p, m = m2 + h*q
        (let* ((dp (rsa-private-key-dp priv-key))
               (dq (rsa-private-key-dq priv-key))
               (qinv (rsa-private-key-qinv priv-key))
               (m1 (mod-arith:mod-expt c dp p))
               (m2 (mod-arith:mod-expt c dq q))
               (h (mod (* qinv (- m1 m2)) p)))
          (mod (+ m2 (* h q)) n))
        ;; Fallback: m = c^d mod n
        (mod-arith:mod-expt c d n))))

;;; ---------------------------------------------------------------------------
;;; Utility: integer <-> octet string conversions (RFC 8017 Section 4)
;;; ---------------------------------------------------------------------------

(defun i2osp (x len)
  "Integer to Octet String Primitive (RFC 8017 Section 4.1).
   Encodes integer X as big-endian byte array of length LEN."
  (let ((bytes (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from (1- len) downto 0
          do (setf (aref bytes i) (logand x #xFF))
             (setf x (ash x -8)))
    bytes))

(defun os2ip (bytes)
  "Octet String to Integer Primitive (RFC 8017 Section 4.2).
   Decodes big-endian byte array to an integer."
  (let ((x 0))
    (loop for b across bytes
          do (setf x (logior (ash x 8) b)))
    x))

;;; ---------------------------------------------------------------------------
;;; Hash algorithm OIDs for PKCS#1 v1.5 (DER-encoded DigestInfo prefixes)
;;; ---------------------------------------------------------------------------

(defun pkcs1-digest-info-prefix (hash-algorithm)
  "Return the DER-encoded DigestInfo prefix for a hash algorithm.
   This is the ASN.1 structure: DigestInfo ::= SEQUENCE { digestAlgorithm, digest }"
  (ecase hash-algorithm
    (:sha1
     ;; 30 21 30 09 06 05 2b 0e 03 02 1a 05 00 04 14
     (coerce '(#x30 #x21 #x30 #x09 #x06 #x05 #x2b #x0e #x03 #x02 #x1a #x05 #x00 #x04 #x14)
             '(vector (unsigned-byte 8))))
    (:sha256
     ;; 30 31 30 0d 06 09 60 86 48 01 65 03 04 02 01 05 00 04 20
     (coerce '(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x01 #x05 #x00 #x04 #x20)
             '(vector (unsigned-byte 8))))
    (:sha384
     ;; 30 41 30 0d 06 09 60 86 48 01 65 03 04 02 02 05 00 04 30
     (coerce '(#x30 #x41 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x02 #x05 #x00 #x04 #x30)
             '(vector (unsigned-byte 8))))
    (:sha512
     ;; 30 51 30 0d 06 09 60 86 48 01 65 03 04 02 03 05 00 04 40
     (coerce '(#x30 #x51 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x03 #x05 #x00 #x04 #x40)
             '(vector (unsigned-byte 8))))))

;;; ---------------------------------------------------------------------------
;;; PKCS#1 v1.5 Signature Verification (RFC 8017 Section 8.2.2)
;;; ---------------------------------------------------------------------------

(defun pkcs1-v15-verify (pub-key message signature &key (hash :sha256))
  "Verify a PKCS#1 v1.5 signature.
   Returns T if the signature is valid, NIL otherwise."
  (let* ((n (rsa-public-key-n pub-key))
         (k (ceiling (integer-length n) 8))
         (s (os2ip signature)))
    ;; Step 1: Check signature length
    (unless (= (length signature) k)
      (return-from pkcs1-v15-verify nil))
    ;; Step 2: RSA verification: m = s^e mod n
    (when (>= s n)
      (return-from pkcs1-v15-verify nil))
    (let* ((m (rsa-encrypt s pub-key))
           (em (i2osp m k)))
      ;; Step 3: EMSA-PKCS1-v1_5 verification
      ;; em should be: 0x00 0x01 [0xFF padding] 0x00 [DigestInfo]
      (unless (and (= (aref em 0) #x00) (= (aref em 1) #x01))
        (return-from pkcs1-v15-verify nil))
      ;; Find the 0x00 separator
      (let ((sep-pos nil))
        (loop for i from 2 below (length em)
              do (cond
                   ((= (aref em i) #xFF) nil) ; padding byte
                   ((= (aref em i) #x00)
                    (setf sep-pos i)
                    (return))
                   (t (return-from pkcs1-v15-verify nil)))) ; invalid padding
        (unless sep-pos
          (return-from pkcs1-v15-verify nil))
        ;; Must have at least 8 bytes of 0xFF padding
        (unless (>= (- sep-pos 2) 8)
          (return-from pkcs1-v15-verify nil))
        ;; Extract DigestInfo
        (let* ((digest-info (subseq em (1+ sep-pos)))
               (prefix (pkcs1-digest-info-prefix hash))
               (hash-len (ecase hash (:sha1 20) (:sha256 32) (:sha384 48) (:sha512 64)))
               (expected-len (+ (length prefix) hash-len)))
          ;; Check DigestInfo length
          (unless (= (length digest-info) expected-len)
            (return-from pkcs1-v15-verify nil))
          ;; Check prefix
          (unless (equalp (subseq digest-info 0 (length prefix)) prefix)
            (return-from pkcs1-v15-verify nil))
          ;; Compare hash
          (let ((sig-hash (subseq digest-info (length prefix)))
                (msg-hash (compute-hash hash message)))
            (equalp sig-hash msg-hash)))))))

;;; ---------------------------------------------------------------------------
;;; RSA-PSS (RFC 8017 Section 8.1)
;;; ---------------------------------------------------------------------------

(defun compute-hash (algorithm message)
  "Compute hash of MESSAGE using ALGORITHM."
  (ecase algorithm
    (:sha1 (sha1:sha1 message))
    (:sha256 (sha256:sha256 message))
    (:sha384 (sha512:sha384 message))
    (:sha512 (sha512:sha512 message))))

(defun hash-length (algorithm)
  "Return hash output length in bytes."
  (ecase algorithm (:sha1 20) (:sha256 32) (:sha384 48) (:sha512 64)))

(defun mgf1 (seed mask-len hash)
  "MGF1 mask generation function (RFC 8017 Appendix B.2.1)."
  (let ((result (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for counter from 0
          while (< (length result) mask-len)
          for c-bytes = (i2osp counter 4)
          for input = (let ((buf (make-array (+ (length seed) 4) :element-type '(unsigned-byte 8))))
                        (replace buf seed)
                        (replace buf c-bytes :start1 (length seed))
                        buf)
          for h = (compute-hash hash input)
          do (loop for b across h
                   while (< (length result) mask-len)
                   do (vector-push-extend b result)))
    (coerce result '(simple-array (unsigned-byte 8) (*)))))

(defun emsa-pss-encode (m em-bits &key (hash :sha256) (salt-length :auto))
  "EMSA-PSS encoding (RFC 8017 Section 9.1.1).
   Returns the encoded message EM."
  (let* ((h-len (hash-length hash))
         (s-len (if (eq salt-length :auto) h-len salt-length))
         (em-len (ceiling em-bits 8))
         (m-hash (compute-hash hash m)))
    ;; Step 3: Check em-len >= h-len + s-len + 2
    (when (< em-len (+ h-len s-len 2))
      (error "Encoding error: em-len too small"))
    ;; Step 4: Generate random salt
    (let* ((salt (make-array s-len :element-type '(unsigned-byte 8) :initial-element 0))
           ;; Use DRBG if available, otherwise zero salt for deterministic testing
           (_ (when (plusp s-len)
                (let ((drbg-pkg (find-package :epsilon.ssl.drbg)))
                  (when drbg-pkg
                    (let ((random-bytes-fn (find-symbol "RANDOM-BYTES" drbg-pkg)))
                      (when (and random-bytes-fn (fboundp random-bytes-fn))
                        (setf salt (funcall random-bytes-fn s-len))))))))
           ;; Step 5: M' = 0x00...00 (8 bytes) || mHash || salt
           (m-prime (make-array (+ 8 h-len s-len) :element-type '(unsigned-byte 8) :initial-element 0)))
      (declare (ignore _))
      (replace m-prime m-hash :start1 8)
      (replace m-prime salt :start1 (+ 8 h-len))
      ;; Step 6: H = Hash(M')
      (let* ((h (compute-hash hash m-prime))
             ;; Step 7-8: DB = PS || 0x01 || salt
             (ps-len (- em-len s-len h-len 2))
             (db (make-array (- em-len h-len 1) :element-type '(unsigned-byte 8) :initial-element 0)))
        (setf (aref db ps-len) #x01)
        (replace db salt :start1 (1+ ps-len))
        ;; Step 9: dbMask = MGF(H, emLen - hLen - 1)
        (let ((db-mask (mgf1 h (- em-len h-len 1) hash)))
          ;; Step 10: maskedDB = DB xor dbMask
          (loop for i from 0 below (length db)
                do (setf (aref db i) (logxor (aref db i) (aref db-mask i))))
          ;; Step 11: Set leftmost (8*emLen - emBits) bits to zero
          (let ((top-bits (- (* 8 em-len) em-bits)))
            (when (plusp top-bits)
              (setf (aref db 0) (logand (aref db 0) (ash #xFF (- top-bits))))))
          ;; Step 12: EM = maskedDB || H || 0xbc
          (let ((em (make-array em-len :element-type '(unsigned-byte 8))))
            (replace em db)
            (replace em h :start1 (- em-len h-len 1))
            (setf (aref em (1- em-len)) #xBC)
            em))))))

(defun emsa-pss-verify (m em em-bits &key (hash :sha256) (salt-length :auto))
  "EMSA-PSS verification (RFC 8017 Section 9.1.2).
   Returns T if the signature is consistent."
  (let* ((h-len (hash-length hash))
         (s-len (if (eq salt-length :auto) h-len salt-length))
         (em-len (ceiling em-bits 8))
         (m-hash (compute-hash hash m)))
    ;; Step 3: Check em-len >= h-len + s-len + 2
    (when (< em-len (+ h-len s-len 2))
      (return-from emsa-pss-verify nil))
    ;; Step 4: Check rightmost byte is 0xBC
    (unless (= (aref em (1- em-len)) #xBC)
      (return-from emsa-pss-verify nil))
    ;; Step 5: Extract maskedDB and H
    (let* ((db-len (- em-len h-len 1))
           (masked-db (subseq em 0 db-len))
           (h (subseq em db-len (+ db-len h-len))))
      ;; Step 6: Check leftmost bits are zero
      (let ((top-bits (- (* 8 em-len) em-bits)))
        (when (plusp top-bits)
          (let ((mask (logxor #xFF (ash #xFF (- top-bits)))))
            (unless (zerop (logand (aref masked-db 0) mask))
              (return-from emsa-pss-verify nil)))))
      ;; Step 7: dbMask = MGF(H, emLen - hLen - 1)
      (let ((db-mask (mgf1 h db-len hash)))
        ;; Step 8: DB = maskedDB xor dbMask
        (let ((db (make-array db-len :element-type '(unsigned-byte 8))))
          (loop for i from 0 below db-len
                do (setf (aref db i) (logxor (aref masked-db i) (aref db-mask i))))
          ;; Step 9: Set leftmost bits to zero
          (let ((top-bits (- (* 8 em-len) em-bits)))
            (when (plusp top-bits)
              (setf (aref db 0) (logand (aref db 0) (ash #xFF (- top-bits))))))
          ;; Step 10: Check PS is all zero and has 0x01 separator
          (let ((ps-len (- db-len s-len 1)))
            (loop for i from 0 below ps-len
                  unless (zerop (aref db i))
                    do (return-from emsa-pss-verify nil))
            (unless (= (aref db ps-len) #x01)
              (return-from emsa-pss-verify nil))
            ;; Step 11: Extract salt
            (let ((salt (subseq db (1+ ps-len))))
              ;; Step 12-13: M' = 0x00...00 || mHash || salt, H' = Hash(M')
              (let ((m-prime (make-array (+ 8 h-len s-len) :element-type '(unsigned-byte 8) :initial-element 0)))
                (replace m-prime m-hash :start1 8)
                (replace m-prime salt :start1 (+ 8 h-len))
                (let ((h-prime (compute-hash hash m-prime)))
                  ;; Step 14: Check H = H'
                  (equalp h h-prime))))))))))

(defun rsa-pss-sign (priv-key message &key (hash :sha256) (salt-length :auto))
  "Sign a message with RSA-PSS (RFC 8017 Section 8.1.1)."
  (let* ((n (rsa-private-key-n priv-key))
         (mod-bits (integer-length n))
         (em (emsa-pss-encode message (1- mod-bits) :hash hash :salt-length salt-length))
         (m (os2ip em))
         (s (rsa-decrypt m priv-key))
         (k (ceiling mod-bits 8)))
    (i2osp s k)))

(defun rsa-pss-verify (pub-key message signature &key (hash :sha256) (salt-length :auto))
  "Verify an RSA-PSS signature (RFC 8017 Section 8.1.2).
   Returns T if the signature is valid."
  (let* ((n (rsa-public-key-n pub-key))
         (mod-bits (integer-length n))
         (k (ceiling mod-bits 8))
         (s (os2ip signature)))
    ;; Check signature length
    (unless (= (length signature) k)
      (return-from rsa-pss-verify nil))
    ;; Check s < n
    (when (>= s n)
      (return-from rsa-pss-verify nil))
    ;; RSAVP1: m = s^e mod n
    (let* ((m (rsa-encrypt s pub-key))
           (em-len (ceiling (1- mod-bits) 8))
           (em (i2osp m em-len)))
      (emsa-pss-verify message em (1- mod-bits) :hash hash :salt-length salt-length))))

;;; ---------------------------------------------------------------------------
;;; Miller-Rabin primality test
;;; ---------------------------------------------------------------------------

(defun miller-rabin-prime-p (n &optional (k 20))
  "Miller-Rabin primality test with K rounds.
   Returns T if N is probably prime, NIL if composite."
  (cond
    ((< n 2) nil)
    ((= n 2) t)
    ((= n 3) t)
    ((evenp n) nil)
    (t
     ;; Write n-1 = 2^r * d where d is odd
     (let ((d (1- n)) (r 0))
       (loop while (evenp d)
             do (setf d (ash d -1))
                (incf r))
       ;; Witnesses: for small n use deterministic witnesses
       (let ((witnesses (if (< n (expt 2 64))
                            ;; Deterministic witnesses for n < 2^64
                            '(2 3 5 7 11 13 17 19 23 29 31 37)
                            ;; Random witnesses for large n
                            (loop repeat k collect (+ 2 (random (- n 4)))))))
         (dolist (a witnesses t)
           (when (>= a n) (return t))
           ;; Compute x = a^d mod n
           (let ((x (mod-arith:mod-expt a d n)))
             (unless (or (= x 1) (= x (1- n)))
               (block witness-loop
                 (loop repeat (1- r)
                       do (setf x (mod (* x x) n))
                          (when (= x (1- n))
                            (return-from witness-loop)))
                 ;; If we get here, n is composite
                 (return nil))))))))))

;;; ---------------------------------------------------------------------------
;;; RSA key generation
;;; ---------------------------------------------------------------------------

(defun random-odd-integer (bits)
  "Generate a random odd integer of exactly BITS bits."
  (let ((n 0))
    ;; Generate random bytes
    (let ((byte-count (ceiling bits 8)))
      (let ((drbg-pkg (find-package :epsilon.ssl.drbg)))
        (if (and drbg-pkg (fboundp (find-symbol "RANDOM-BYTES" drbg-pkg)))
            (let ((bytes (funcall (find-symbol "RANDOM-BYTES" drbg-pkg) byte-count)))
              (loop for b across bytes
                    do (setf n (logior (ash n 8) b))))
            ;; Fallback to CL random
            (loop repeat byte-count
                  do (setf n (logior (ash n 8) (random 256)))))))
    ;; Set the top bit to ensure exact bit length
    (setf n (logior n (ash 1 (1- bits))))
    ;; Set the bottom bit to ensure odd
    (setf n (logior n 1))
    ;; Mask to exact bit length
    (logand n (1- (ash 1 bits)))))

(defun generate-prime (bits &optional (max-attempts 10000))
  "Generate a random prime of exactly BITS bits."
  (loop repeat max-attempts
        for candidate = (random-odd-integer bits)
        when (miller-rabin-prime-p candidate)
          return candidate
        finally (error "Failed to generate prime after ~A attempts" max-attempts)))

(defun rsa-generate-key (bits &key (e 65537))
  "Generate an RSA key pair of BITS bits.
   Returns (values public-key private-key)."
  (let ((half-bits (/ bits 2)))
    (loop
      (let* ((p (generate-prime half-bits))
             (q (generate-prime half-bits))
             (n (* p q)))
        ;; Ensure n has the right bit length and p != q
        (when (and (= (integer-length n) bits)
                   (/= p q))
          ;; Compute lambda(n) = lcm(p-1, q-1)
          (let* ((p1 (1- p))
                 (q1 (1- q))
                 (lambda-n (/ (* p1 q1) (gcd p1 q1))))
            ;; Ensure gcd(e, lambda) = 1
            (when (= (gcd e lambda-n) 1)
              ;; Compute d = e^(-1) mod lambda(n)
              (let ((d (mod-arith:mod-inv e lambda-n)))
                (return
                  (values
                   (make-rsa-public-key n e)
                   (make-rsa-private-key n e d
                                         :p p :q q
                                         :dp (mod d p1)
                                         :dq (mod d q1)
                                         :qinv (mod-arith:mod-inv q p))))))))))))
