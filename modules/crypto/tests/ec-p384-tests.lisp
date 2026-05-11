;;;; Tests for the P-384 curve, ECDSA-P384, and ECDH-P384.
;;;;
;;;; ECDSA known-answer vectors come from RFC 6979 Appendix A.2.5
;;;; (P-384 + SHA-384, message "sample" / "test").

(defpackage epsilon.crypto.ec-p384-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.ec-p384 ec)
           (epsilon.crypto.ecdsa-p384 ecdsa)
           (epsilon.crypto.ecdh ecdh)))

(in-package :epsilon.crypto.ec-p384-tests)

(defun %hex-int (s) (parse-integer s :radix 16))

;;; ---------------------------------------------------------------------------
;;; Curve sanity
;;; ---------------------------------------------------------------------------

(deftest test-p384-base-point-on-curve
  "The P-384 generator G satisfies the curve equation"
  (assert-true (ec:p384-on-curve-p (ec:p384-base-point))))

(deftest test-p384-base-point-order
  "n * G is the point at infinity"
  (assert-true (ec:p384-point-is-neutral
                (ec:p384-scalar-mul ec:+n+ (ec:p384-base-point)))))

(deftest test-p384-double-equals-add-self
  "2G computed via doubling matches G + G via addition"
  (let* ((g (ec:p384-base-point))
         (g+g (ec:p384-point-add g g))
         (2g (ec:p384-point-double g)))
    (assert-true (ec:p384-point-equal g+g 2g))))

(deftest test-p384-scalar-mul-distributes-over-add
  "(a + b) * G = a*G + b*G"
  (let* ((a 12345678901234567890)
         (b 98765432109876543210)
         (g (ec:p384-base-point))
         (lhs (ec:p384-scalar-mul (+ a b) g))
         (rhs (ec:p384-point-add (ec:p384-scalar-mul a g)
                                 (ec:p384-scalar-mul b g))))
    (assert-true (ec:p384-point-equal lhs rhs))))

(deftest test-p384-encode-decode-roundtrip
  "Uncompressed SEC1 encode/decode round-trips for several scalar multiples"
  (loop for k in '(1 2 3 100 12345 1234567890)
        for point = (ec:p384-scalar-mul k (ec:p384-base-point))
        for bytes = (ec:p384-point-encode-uncompressed point)
        for decoded = (ec:p384-point-decode bytes)
        do (assert-= 97 (length bytes))
           (assert-true decoded)
           (assert-true (ec:p384-point-equal point decoded))))

(deftest test-p384-decode-rejects-off-curve
  "p384-point-decode rejects points that don't satisfy the curve equation"
  (let ((bad (make-array 97 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref bad 0) #x04)
    ;; x = 1, y = 1 is not on the curve.
    (setf (aref bad 96) 1)
    (setf (aref bad 48) 1)
    (assert-true (null (ec:p384-point-decode bad)))))

;;; ---------------------------------------------------------------------------
;;; ECDSA-P384 RFC 6979 known-answer
;;;
;;; A.2.5 secret key:
;;;   x = 6B9D3DAD2E1B8C1C05B19875B6659F4DE23C3B667BF297BA9AA47740787137D8
;;;       96D5724E4C70A825F872C9EA60D2EDF5
;;;   Q.x = EC3A4E415B4E19A4568618029F427FA5DA9A8BC4AE92E02E06AAE5286B300C64
;;;         DEF8F0EA9055866064A254515480BC13
;;;   Q.y = 8015D9B72D7D57244EA8EF9AC0C621896708A59367F9DFB9F54CA84B3F1C9DB1
;;;         288B231C3AE0D4FE7344FD2533264720
;;;
;;; "sample" / SHA-384:
;;;   k = r = 94EDBB92A5ECB8AAD4736E56C691916B3F88140666CE9FA73D64C4EA95AD133C
;;;           81A648152E44ACF96E36DD1E80FABE46
;;;   s     = 99EF4AEB15F178CEA1FE40DB2603138F130E740A19624526203B6351D0A3A94F
;;;           A329C145786E679E7B82C71A38628AC8
;;;
;;; "test" / SHA-384:
;;;   k = 015EE46A5BF88773ED9123A5AB0807962D193719503C527B031B4C2D225092ADA71F4A459BC0DA98ADB95837DB8312EA
;;;   r = 6D6DEFAC9AB64DABAFE36C6BF510352A4CC27001263638E5B16D9BB51D451559F918EEDAF2293BE5B475CC8F0188636B
;;;   s = 2D46F3BECBCC523D5F1A1256BF0C9B024D879BA9E838144C8BA6BAEB4B53B47D51AB373F9845C0514EEFB14024787265
;;; ---------------------------------------------------------------------------

(defparameter +rfc6979-p384-d+
  (%hex-int (concatenate 'string
                         "6B9D3DAD2E1B8C1C05B19875B6659F4DE23C3B667BF297BA"
                         "9AA47740787137D896D5724E4C70A825F872C9EA60D2EDF5")))

(defparameter +rfc6979-p384-qx+
  (%hex-int (concatenate 'string
                         "EC3A4E415B4E19A4568618029F427FA5DA9A8BC4AE92E02E"
                         "06AAE5286B300C64DEF8F0EA9055866064A254515480BC13")))

(defparameter +rfc6979-p384-qy+
  (%hex-int (concatenate 'string
                         "8015D9B72D7D57244EA8EF9AC0C621896708A59367F9DFB9"
                         "F54CA84B3F1C9DB1288B231C3AE0D4FE7344FD2533264720")))

(deftest test-ecdsa-p384-public-key-from-private
  "Public key derivation matches RFC 6979 §A.2.5"
  (let ((q (ecdsa:ecdsa-p384-public-key-from-private +rfc6979-p384-d+)))
    (multiple-value-bind (qx qy) (ec:p384-point-to-affine q)
      (assert-= +rfc6979-p384-qx+ qx)
      (assert-= +rfc6979-p384-qy+ qy))))

(deftest test-ecdsa-p384-rfc6979-sample
  "RFC 6979 §A.2.5 SHA-384 / message=\"sample\""
  (let* ((message (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                       "sample"))
         (expected-r
           (%hex-int (concatenate 'string
                                  "94EDBB92A5ECB8AAD4736E56C691916B3F881406"
                                  "66CE9FA73D64C4EA95AD133C81A648152E44ACF9"
                                  "6E36DD1E80FABE46")))
         (expected-s
           (%hex-int (concatenate 'string
                                  "99EF4AEB15F178CEA1FE40DB2603138F130E740A"
                                  "19624526203B6351D0A3A94FA329C145786E679E"
                                  "7B82C71A38628AC8"))))
    (multiple-value-bind (r s)
        (ecdsa:ecdsa-p384-sign +rfc6979-p384-d+ message)
      (assert-= expected-r r)
      (assert-= expected-s s)
      ;; And the signature verifies under the matching public key.
      (let ((q (ecdsa:ecdsa-p384-public-key-from-private +rfc6979-p384-d+)))
        (assert-true (ecdsa:ecdsa-p384-verify q message r s))))))

(deftest test-ecdsa-p384-rfc6979-test-message
  "RFC 6979 §A.2.5 SHA-384 / message=\"test\""
  (let* ((message (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                       "test"))
         (expected-r
           (%hex-int (concatenate 'string
                                  "8203B63D3C853E8D77227FB377BCF7B7B772E978"
                                  "92A80F36AB775D509D7A5FEB0542A7F0812998DA"
                                  "8F1DD3CA3CF023DB")))
         (expected-s
           (%hex-int (concatenate 'string
                                  "DDD0760448D42D8A43AF45AF836FCE4DE8BE06B4"
                                  "85E9B61B827C2F13173923E06A739F040649A667"
                                  "BF3B828246BAA5A5"))))
    (multiple-value-bind (r s)
        (ecdsa:ecdsa-p384-sign +rfc6979-p384-d+ message)
      (assert-= expected-r r)
      (assert-= expected-s s))))

(deftest test-ecdsa-p384-tampered-message-rejected
  "A signature on one message must not verify against another"
  (let* ((d +rfc6979-p384-d+)
         (m1 (map '(simple-array (unsigned-byte 8) (*)) #'char-code "sample"))
         (m2 (map '(simple-array (unsigned-byte 8) (*)) #'char-code "Sample"))
         (q (ecdsa:ecdsa-p384-public-key-from-private d)))
    (multiple-value-bind (r s) (ecdsa:ecdsa-p384-sign d m1)
      (assert-true (ecdsa:ecdsa-p384-verify q m1 r s))
      (assert-not (ecdsa:ecdsa-p384-verify q m2 r s)))))

(deftest test-ecdsa-p384-bad-signature-rejected
  "r or s outside [1, n-1] is rejected"
  (let ((q (ecdsa:ecdsa-p384-public-key-from-private +rfc6979-p384-d+))
        (m (map '(simple-array (unsigned-byte 8) (*)) #'char-code "msg")))
    (assert-not (ecdsa:ecdsa-p384-verify q m 0 1))
    (assert-not (ecdsa:ecdsa-p384-verify q m 1 0))
    (assert-not (ecdsa:ecdsa-p384-verify q m ec:+n+ 1))))

;;; ---------------------------------------------------------------------------
;;; ECDH-P384
;;; ---------------------------------------------------------------------------

(deftest test-ecdh-p384-shared-secret-symmetric
  "Alice and Bob agree on the same 48-byte X coordinate"
  (multiple-value-bind (a-priv a-pub) (ecdh:ecdh-p384-generate-keypair)
    (multiple-value-bind (b-priv b-pub) (ecdh:ecdh-p384-generate-keypair)
      (let ((s-a (ecdh:ecdh-p384-shared-secret a-priv b-pub))
            (s-b (ecdh:ecdh-p384-shared-secret b-priv a-pub)))
        (assert-= 48 (length s-a))
        (assert-true (equalp s-a s-b))))))

(deftest test-ecdh-p384-shared-secret-stable
  "Identical input keys give identical secret bytes"
  (let* ((d-a 12345)
         (d-b 67890)
         (q-a (ecdh:ecdh-p384-public-key-from-private d-a))
         (q-b (ecdh:ecdh-p384-public-key-from-private d-b))
         (s1 (ecdh:ecdh-p384-shared-secret d-a q-b))
         (s2 (ecdh:ecdh-p384-shared-secret d-b q-a))
         (s3 (ecdh:ecdh-p384-shared-secret d-a q-b)))
    (assert-true (equalp s1 s2))
    (assert-true (equalp s1 s3))))

;;; ---------------------------------------------------------------------------
;;; Constant-time scalar multiplication (RCB unified addition)
;;; ---------------------------------------------------------------------------

(deftest test-p384-scalar-mul-ct-trivial
  "p384-scalar-mul-ct on small scalars matches p384-scalar-mul."
  (let ((g (ec:p384-base-point)))
    (loop for k from 0 to 12
          do (assert-true
              (ec:p384-point-equal (ec:p384-scalar-mul k g)
                                   (ec:p384-scalar-mul-ct k g))))))

(deftest test-p384-scalar-mul-ct-medium
  "Round-trip equivalence on medium-magnitude scalars."
  (let ((g (ec:p384-base-point)))
    (dolist (k '(13 17 65537 #xdeadbeefdeadbeef #xcafebabef00d))
      (assert-true
       (ec:p384-point-equal (ec:p384-scalar-mul k g)
                            (ec:p384-scalar-mul-ct k g))))))

(deftest test-p384-scalar-mul-ct-near-order
  "CT scalar mul handles scalars approaching the group order n."
  (let ((g (ec:p384-base-point)))
    (dolist (k (list (1- ec:+n+) (- ec:+n+ 7)))
      (assert-true
       (ec:p384-point-equal (ec:p384-scalar-mul k g)
                            (ec:p384-scalar-mul-ct k g))))))

(deftest test-p384-scalar-mul-ct-non-base-point
  "CT scalar mul works when the input point is not the base point."
  (let* ((g (ec:p384-base-point))
         (h (ec:p384-scalar-mul 7 g)))
    (dolist (k '(3 11 65537))
      (assert-true
       (ec:p384-point-equal (ec:p384-scalar-mul k h)
                            (ec:p384-scalar-mul-ct k h))))))
