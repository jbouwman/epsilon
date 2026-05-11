;;;; RSA key operations, PKCS#1 v1.5 signature verification, and
;;;; RSA-PSS sign/verify per RFC 8017 (PKCS#1 v2.2).
;;;;
;;;; Uses CL native bignums for modular exponentiation.
;;;; CRT (Chinese Remainder Theorem) acceleration for private key operations.
(defpackage epsilon.crypto.rsa
  (:use :cl)
  (:import (epsilon.crypto.modular mod-arith)
           (epsilon.crypto.sha256 sha256)
           (epsilon.crypto.sha512 sha512)
           (epsilon.crypto.sha1 sha1)
           (epsilon.crypto.ct ct)
           (epsilon.crypto.drbg drbg))
  (:export
    ;; Key types
    rsa-public-key
    rsa-private-key
    make-rsa-public-key
    make-rsa-private-key
    rsa-public-key-n
    rsa-public-key-e
    rsa-private-key-n
    rsa-private-key-e
    rsa-private-key-d
    rsa-private-key-p
    rsa-private-key-q
    rsa-private-key-dp
    rsa-private-key-dq
    rsa-private-key-qinv
    rsa-key-bits
    ;; Core operations
    rsa-encrypt
    rsa-decrypt
    ;; PKCS#1 v1.5
    pkcs1-v15-sign
    pkcs1-v15-verify
    ;; RSA-PSS
    rsa-pss-sign
    rsa-pss-verify
    ;; RSAES-OAEP (PKCS#1 v2.2)
    rsa-oaep-encrypt
    rsa-oaep-decrypt
    ;; Key generation
    rsa-generate-key
    miller-rabin-prime-p))

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
  (dp 0 :type integer) ; d mod (p-1)
  (dq 0 :type integer) ; d mod (q-1)
  (qinv 0 :type integer))

; q^(-1) mod p
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

(defun %rsa-random-blinding-factor (n)
  "Return a random integer in [2, n-1] that is coprime to N. Uses
   epsilon.crypto.drbg's random-integer; signals an error if DRBG is not
   loaded.

   Coprimality is required because the caller will compute r^-1 mod n.
   For real RSA moduli (n = pq with p, q ~thousand-bit primes) the
   probability of drawing a non-coprime r is ~1/p + 1/q, i.e.
   negligible -- a single draw effectively always succeeds. The retry
   loop exists so small test moduli (e.g. n = 53*61 = 3233, where
   ~3.4% of candidates share a factor with n) do not produce flaky
   failures. Iterating on a public modulus with public random material
   is not a side-channel concern: any r that triggers a retry would
   itself reveal a factor of n via gcd, which is a far larger problem."
  (loop for r = (+ 2 (drbg:random-integer (- n 3)))
        when (= 1 (gcd r n)) return r))

(defun rsa-decrypt (c priv-key &key (blind t))
  "RSA decryption with CRT acceleration when available.

   The secret-exponent multiplications go through `mod-expt-ct`,
   which performs a fixed iteration count and uses branchless
   selection between the square-only and square-and-multiply
   results. This eliminates the algorithmic timing leak in the
   prior square-and-multiply implementation.

   When :BLIND is non-nil (default), the input is multiplicatively
   blinded with a fresh random factor r per call: c' = c * r^e mod n,
   m' = c'^d, m = m' * r^-1 mod n. This randomises each modexp
   invocation so that any residual timing leak in the underlying
   bignum arithmetic cannot be correlated across calls -- a remote
   attacker observing thousands of decryptions sees uncorrelated
   noise rather than a learnable signal. See
   `manual/architecture/crypto-side-channels.md` for the threat
   model.

   :BLIND should only be disabled in tests that need deterministic
   intermediate values; production callers must leave it on."
  (if blind
      (let* ((n (rsa-private-key-n priv-key))
             (e (rsa-private-key-e priv-key))
             ;; Blinded path: c' = c * r^e mod n, m' = c'^d, m = m' * r^-1.
             (r (%rsa-random-blinding-factor n))
             (r-inv (mod-arith:mod-inv r n))
             (r-e (mod-arith:mod-expt r e n)) ; e is public, fast non-CT OK
             (c-blinded (mod (* c r-e) n))
             (m-blinded (%rsa-decrypt-raw c-blinded priv-key)))
        (mod (* m-blinded r-inv) n))
      (%rsa-decrypt-raw c priv-key)))

(defun %rsa-decrypt-raw (c priv-key)
  "Inner CT decryption without blinding. Used directly by tests that
   pin intermediate values; production callers should use rsa-decrypt
   with the default blinding."
  (let ((n (rsa-private-key-n priv-key))
        (d (rsa-private-key-d priv-key))
        (p (rsa-private-key-p priv-key))
        (q (rsa-private-key-q priv-key)))
    (if (and (plusp p) (plusp q))
      ;; CRT: m1 = c^dp mod p, m2 = c^dq mod q, h = qinv*(m1-m2) mod p, m = m2 + h*q.
      ;; The CT iteration count is fixed at the bit-length of p (resp. q),
      ;; which bounds dp (resp. dq) without leaking either secret value.
      (let* ((dp (rsa-private-key-dp priv-key))
             (dq (rsa-private-key-dq priv-key))
             (qinv (rsa-private-key-qinv priv-key))
             (m1 (mod-arith:mod-expt-ct c dp p))
             (m2 (mod-arith:mod-expt-ct c dq q))
             (h (mod (* qinv (- m1 m2)) p)))
        (mod (+ m2 (* h q)) n))
      ;; Fallback: m = c^d mod n
      (mod-arith:mod-expt-ct c d n))))

;;; ---------------------------------------------------------------------------
;;; Utility: integer <-> octet string conversions (RFC 8017 Section 4)
;;; ---------------------------------------------------------------------------
(defun i2osp (x len)
  "Integer to Octet String Primitive (RFC 8017 Section 4.1).
   Encodes integer X as big-endian byte array of length LEN."
  (let ((bytes (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from (1- len) downto 0
          do (setf (aref bytes i) (logand x #xFF)) (setf x (ash x -8)))
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
     (coerce '(#x30 #x31
                    #x30
                    #x0d
                    #x06
                    #x09
                    #x60
                    #x86
                    #x48
                    #x01
                    #x65
                    #x03
                    #x04
                    #x02
                    #x01
                    #x05
                    #x00
                    #x04
                    #x20)
             '(vector (unsigned-byte 8))))
    (:sha384
     ;; 30 41 30 0d 06 09 60 86 48 01 65 03 04 02 02 05 00 04 30
     (coerce '(#x30 #x41
                    #x30
                    #x0d
                    #x06
                    #x09
                    #x60
                    #x86
                    #x48
                    #x01
                    #x65
                    #x03
                    #x04
                    #x02
                    #x02
                    #x05
                    #x00
                    #x04
                    #x30)
             '(vector (unsigned-byte 8))))
    (:sha512
     ;; 30 51 30 0d 06 09 60 86 48 01 65 03 04 02 03 05 00 04 40
     (coerce '(#x30 #x51
                    #x30
                    #x0d
                    #x06
                    #x09
                    #x60
                    #x86
                    #x48
                    #x01
                    #x65
                    #x03
                    #x04
                    #x02
                    #x03
                    #x05
                    #x00
                    #x04
                    #x40)
             '(vector (unsigned-byte 8))))))

;;; ---------------------------------------------------------------------------
;;; PKCS#1 v1.5 Signature Generation (RFC 8017 Section 8.2.1)
;;; ---------------------------------------------------------------------------
(defun pkcs1-v15-sign (priv-key message &key (hash :sha256))
  "Sign MESSAGE with PRIV-KEY using RSASSA-PKCS1-v1_5.
Returns the signature as a byte vector of length k = ceiling(modBits/8)."
  (let* ((n (rsa-private-key-n priv-key))
         (k (ceiling (integer-length n) 8))
         (msg-hash (ecase hash
                     (:sha1 (sha1:sha1 message))
                     (:sha256 (sha256:sha256 message))
                     (:sha384 (sha512:sha384 message))
                     (:sha512 (sha512:sha512 message))))
         (digest-info-prefix (pkcs1-digest-info-prefix hash))
         (t-bytes (concatenate '(vector (unsigned-byte 8))
                               digest-info-prefix msg-hash))
         (t-len (length t-bytes)))
    ;; EMSA-PKCS1-v1_5 encoding: 0x00 || 0x01 || PS || 0x00 || T
    ;; where PS is at least 8 bytes of 0xFF
    (when (< k (+ t-len 11))
      (error "RSA key too small for PKCS#1 v1.5 signature"))
    (let* ((ps-len (- k t-len 3))
           (em (make-array k :element-type '(unsigned-byte 8) :initial-element #xFF)))
      (setf (aref em 0) #x00)
      (setf (aref em 1) #x01)
      ;; PS is already filled with 0xFF
      (setf (aref em (+ 2 ps-len)) #x00)
      (replace em t-bytes :start1 (+ 3 ps-len))
      ;; m = OS2IP(EM); s = m^d mod n; signature = I2OSP(s, k)
      (let* ((m (os2ip em))
             (s (rsa-decrypt m priv-key)))
        (i2osp s k)))))

;;; ---------------------------------------------------------------------------
;;; PKCS#1 v1.5 Signature Verification (RFC 8017 Section 8.2.2)
;;; ---------------------------------------------------------------------------
(defun pkcs1-v15-verify (pub-key message signature &key (hash :sha256))
  "Verify a PKCS#1 v1.5 signature using constant-time comparison.
   Constructs the expected EM and compares the full block rather
   than parsing the received EM (Valsorda construction)."
  (let* ((n (rsa-public-key-n pub-key))
         (k (ceiling (integer-length n) 8))
         (s (os2ip signature)))
    (unless (= (length signature) k)
      (return-from pkcs1-v15-verify nil))
    (when (>= s n)
      (return-from pkcs1-v15-verify nil))
    (let* ((m (rsa-encrypt s pub-key))
           (em (i2osp m k))
           ;; Construct expected EM
           (msg-hash (compute-hash hash message))
           (prefix (pkcs1-digest-info-prefix hash))
           (t-block (concatenate '(simple-array (unsigned-byte 8) (*))
                                 prefix msg-hash))
           (t-len (length t-block))
           (ps-len (- k t-len 3)))
      (when (< ps-len 8)
        (return-from pkcs1-v15-verify nil))
      ;; Expected: 0x00 0x01 [0xFF * ps-len] 0x00 T
      (let ((expected (make-array k :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
        (setf (aref expected 0) #x00)
        (setf (aref expected 1) #x01)
        (fill expected #xFF :start 2 :end (+ 2 ps-len))
        (setf (aref expected (+ 2 ps-len)) #x00)
        (replace expected t-block :start1 (+ 3 ps-len))
        (ct:ct-equal em expected)))))

;;; ---------------------------------------------------------------------------
;;; RSA-PSS (RFC 8017 Section 8.1)
;;; ---------------------------------------------------------------------------
(defun compute-hash (algorithm message)
  "Compute hash of MESSAGE using ALGORITHM."
  (ecase algorithm
    (:sha1
     (sha1:sha1 message))
    (:sha256
     (sha256:sha256 message))
    (:sha384
     (sha512:sha384 message))
    (:sha512
     (sha512:sha512 message))))

(defun hash-length (algorithm)
  "Return hash output length in bytes."
  (ecase algorithm
    (:sha1
     20)
    (:sha256
     32)
    (:sha384
     48)
    (:sha512
     64)))

(defun mgf1 (seed mask-len hash)
  "MGF1 mask generation function (RFC 8017 Appendix B.2.1).
   Pre-sizes the output buffer to avoid per-iteration growth."
  (let* ((h-len (hash-length hash))
         (iterations (ceiling mask-len h-len))
         (result (make-array (* iterations h-len)
                             :element-type '(unsigned-byte 8)))
         (input (make-array (+ (length seed) 4)
                            :element-type '(unsigned-byte 8))))
    (replace input seed)
    (loop for counter from 0 below iterations
          for offset = (* counter h-len)
          do (let ((c-bytes (i2osp counter 4)))
               (replace input c-bytes :start1 (length seed))
               (let ((h (compute-hash hash input)))
                 (replace result h :start1 offset))))
    (if (= (length result) mask-len)
        result
        (subseq result 0 mask-len))))

(defun emsa-pss-encode (m em-bits &key (hash :sha256) (salt-length :auto))
  "EMSA-PSS encoding (RFC 8017 Section 9.1.1).
   Returns the encoded message EM."
  (let* ((h-len (hash-length hash))
         (s-len (if (eq salt-length :auto)
                  h-len
                  salt-length))
         (em-len (ceiling em-bits 8))
         (m-hash (compute-hash hash m)))
    ;; Step 3: Check em-len >= h-len + s-len + 2
    (when (< em-len (+ h-len s-len 2))
      (error "Encoding error: em-len too small"))
    ;; Step 4: Generate random salt
    (let* ((salt (if (plusp s-len)
                     (drbg:random-bytes s-len)
                     (make-array 0 :element-type '(unsigned-byte 8))))
           ;; Step 5: M' = 0x00...00 (8 bytes) || mHash || salt
           (m-prime (make-array (+ 8 h-len s-len)
                                :element-type
                                '(unsigned-byte 8)
                                :initial-element
                                0)))
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
         (s-len (if (eq salt-length :auto)
                  h-len
                  salt-length))
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
              (let ((m-prime (make-array (+ 8 h-len s-len)
                                         :element-type
                                         '(unsigned-byte 8)
                                         :initial-element
                                         0)))
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
;;; RSAES-OAEP (RFC 8017 Section 7.1)
;;; ---------------------------------------------------------------------------
;;;
;;; Padding scheme used by JWE `alg=RSA-OAEP` (SHA-1) and `alg=RSA-OAEP-256`
;;; (SHA-256), and by most modern RSA-encryption consumers. The mask
;;; generation function is MGF1 using the same hash.

(defun %xor-bytes! (dst src)
  "In-place DST = DST XOR SRC. DST and SRC must be the same length."
  (declare (type (simple-array (unsigned-byte 8) (*)) dst src))
  (dotimes (i (length dst) dst)
    (setf (aref dst i) (logxor (aref dst i) (aref src i)))))

(defun %random-bytes-oaep (n)
  (drbg:random-bytes n))

(defun rsa-oaep-encrypt (pub-key message &key (hash :sha1)
                                              (label (make-array 0 :element-type '(unsigned-byte 8)))
                                              seed)
  "RSAES-OAEP-ENCRYPT (RFC 8017 §7.1.1). PUB-KEY is an `rsa-public-key`
   struct; MESSAGE is a byte vector whose length must satisfy
   `|M| <= k - 2*hLen - 2` where `k` is the modulus size in bytes.
   HASH is one of :sha1/:sha256/:sha384/:sha512 and is used for both the
   label hash and MGF1. LABEL is the optional OAEP label (an empty
   byte vector by default, matching JWE and most other consumers).
   SEED lets tests pin the randomness; production callers leave it nil."
  (let* ((k (ceiling (rsa-key-bits pub-key) 8))
         (h-len (hash-length hash))
         (m-len (length message)))
    (when (> m-len (- k (* 2 h-len) 2))
      (error "rsa-oaep-encrypt: message too long (~D bytes) for modulus ~
              ~D bytes and hash ~A" m-len k hash))
    (let* ((l-hash (compute-hash hash label))
           (ps-len (- k m-len (* 2 h-len) 2))
           (ps (make-array ps-len :element-type '(unsigned-byte 8)
                                  :initial-element 0))
           ;; DB = lHash || PS || 0x01 || M  (length k - hLen - 1)
           (db-len (- k h-len 1))
           (db (make-array db-len :element-type '(unsigned-byte 8))))
      (replace db l-hash)
      (replace db ps :start1 h-len)
      (setf (aref db (+ h-len ps-len)) 1)
      (replace db message :start1 (+ h-len ps-len 1))
      (let* ((seed (or (and seed (coerce seed '(simple-array (unsigned-byte 8) (*))))
                       (%random-bytes-oaep h-len)))
             (db-mask (coerce (mgf1 seed db-len hash)
                              '(simple-array (unsigned-byte 8) (*))))
             (masked-db (copy-seq db)))
        (%xor-bytes! masked-db db-mask)
        (let* ((seed-mask (coerce (mgf1 masked-db h-len hash)
                                  '(simple-array (unsigned-byte 8) (*))))
               (masked-seed (copy-seq seed)))
          (%xor-bytes! masked-seed seed-mask)
          ;; EM = 0x00 || maskedSeed || maskedDB
          (let ((em (make-array k :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
            (replace em masked-seed :start1 1)
            (replace em masked-db :start1 (1+ h-len))
            ;; RSAEP
            (let ((c (rsa-encrypt (os2ip em) pub-key)))
              (i2osp c k))))))))

(defun rsa-oaep-decrypt (priv-key ciphertext &key (hash :sha1)
                                                  (label (make-array 0 :element-type '(unsigned-byte 8))))
  "RSAES-OAEP-DECRYPT (RFC 8017 §7.1.2). Returns the recovered message
   on success; signals an error on any integrity-check failure. The
   error message does NOT distinguish among the four possible failure
   modes (§7.1.2 note 1) to avoid Manger-style oracle attacks."
  (let* ((k (ceiling (rsa-key-bits priv-key) 8))
         (h-len (hash-length hash)))
    (unless (and (= (length ciphertext) k) (>= k (+ (* 2 h-len) 2)))
      (error "rsa-oaep-decrypt: decryption error"))
    (let* ((m (rsa-decrypt (os2ip ciphertext) priv-key))
           (em (i2osp m k))
           (l-hash (compute-hash hash label))
           (y (aref em 0))
           (masked-seed (subseq em 1 (1+ h-len)))
           (masked-db (subseq em (1+ h-len) k))
           (seed-mask (coerce (mgf1 masked-db h-len hash)
                              '(simple-array (unsigned-byte 8) (*))))
           (seed (copy-seq masked-seed)))
      (%xor-bytes! seed seed-mask)
      (let* ((db-mask (coerce (mgf1 seed (- k h-len 1) hash)
                              '(simple-array (unsigned-byte 8) (*))))
             (db (copy-seq masked-db)))
        (%xor-bytes! db db-mask)
        (unless (zerop y)
          (error "rsa-oaep-decrypt: decryption error"))
        (let ((stored-hash (subseq db 0 h-len)))
          (unless (equalp stored-hash l-hash)
            (error "rsa-oaep-decrypt: decryption error")))
        ;; Walk past the PS zero bytes to the 0x01 separator.
        (let ((sep (position 1 db :start h-len)))
          (unless sep
            (error "rsa-oaep-decrypt: decryption error"))
          ;; All bytes between h-len and sep must be zero.
          (loop for i from h-len below sep
                unless (zerop (aref db i))
                do (error "rsa-oaep-decrypt: decryption error"))
          (subseq db (1+ sep)))))))

;;; ---------------------------------------------------------------------------
;;; Miller-Rabin primality test
;;; ---------------------------------------------------------------------------
(defun miller-rabin-prime-p (n &optional (k 20))
  "Miller-Rabin primality test with K rounds.
   Returns T if N is probably prime, NIL if composite."
  (cond
    ((< n 2)
     nil)
    ((= n 2)
     t)
    ((= n 3)
     t)
    ((evenp n)
     nil)
    (t
     ;; Write n-1 = 2^r * d where d is odd
     (let ((d (1- n))
           (r 0))
       (loop while (evenp d)
             do (setf d (ash d -1)) (incf r))
       ;; Witnesses: for small n use deterministic witnesses
       (let ((witnesses (if (< n (expt 2 64))
                          ;; Deterministic witnesses for n < 2^64
                          '(2 3 5 7 11 13 17 19 23 29 31 37)
                          ;; Random witnesses via DRBG
                          (loop repeat k
                                collect (+ 2 (drbg:random-integer (- n 4)))))))
         (dolist (a witnesses t)
           (when (>= a n)
             (return t))
           ;; Compute x = a^d mod n
           (let ((x (mod-arith:mod-expt a d n)))
             (unless (or (= x 1) (= x (1- n)))
               (block witness-loop
                 (loop repeat (1- r)
                       do (setf x (mod (* x x) n)) (when (= x (1- n))
                         (return-from witness-loop)))
                 ;; If we get here, n is composite
                 (return nil))))))))))

;;; ---------------------------------------------------------------------------
;;; RSA key generation
;;; ---------------------------------------------------------------------------
(defun random-odd-integer (bits)
  "Generate a random odd integer of exactly BITS bits."
  (let ((n 0)
        (byte-count (ceiling bits 8)))
    (loop for b across (drbg:random-bytes byte-count)
          do (setf n (logior (ash n 8) b)))
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
    (loop (let* ((p (generate-prime half-bits))
                 (q (generate-prime half-bits))
                 (n (* p q)))
            ;; Ensure n has the right bit length and p != q
            (when (and (= (integer-length n) bits) (/= p q))
              ;; Compute lambda(n) = lcm(p-1, q-1)
              (let* ((p1 (1- p))
                     (q1 (1- q))
                     (lambda-n (/ (* p1 q1) (gcd p1 q1))))
                ;; Ensure gcd(e, lambda) = 1
                (when (= (gcd e lambda-n) 1)
                  ;; Compute d = e^(-1) mod lambda(n)
                  (let ((d (mod-arith:mod-inv e lambda-n)))
                    (return (values (make-rsa-public-key n e)
                                    (make-rsa-private-key n
                                                          e
                                                          d
                                                          :p
                                                          p
                                                          :q
                                                          q
                                                          :dp
                                                          (mod d p1)
                                                          :dq
                                                          (mod d q1)
                                                          :qinv
                                                          (mod-arith:mod-inv q p))))))))))))
