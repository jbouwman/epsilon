;;;; TOTP Tests

(defpackage epsilon.crypto.tests.totp
  (:use :cl :epsilon.test)
  (:require (epsilon.crypto.totp totp)
            (epsilon.base-encode enc))
  (:enter t))

;;; ============================================================================
;;; Secret Generation
;;; ============================================================================

(deftest test-generate-secret-default-length
  "Default secret is 20 bytes"
  (let ((secret (totp:generate-totp-secret)))
    (assert-= 20 (length secret))
    (assert-true (typep secret '(simple-array (unsigned-byte 8) (*))))))

(deftest test-generate-secret-custom-length
  "Custom secret length is respected"
  (let ((secret (totp:generate-totp-secret :bytes 32)))
    (assert-= 32 (length secret))))

(deftest test-generate-secret-is-random
  "Two generated secrets are different"
  (let ((s1 (totp:generate-totp-secret))
        (s2 (totp:generate-totp-secret)))
    (assert-not (equalp s1 s2))))

;;; ============================================================================
;;; TOTP Computation
;;; ============================================================================

(deftest test-compute-totp-returns-6-digits
  "Default TOTP is a 6-digit string"
  (let* ((secret (totp:generate-totp-secret))
         (code (totp:compute-totp secret :time 1000000)))
    (assert-= 6 (length code))
    (assert-true (every #'digit-char-p code))))

(deftest test-compute-totp-returns-8-digits
  "8-digit TOTP works correctly"
  (let* ((secret (totp:generate-totp-secret))
         (code (totp:compute-totp secret :time 1000000 :digits 8)))
    (assert-= 8 (length code))
    (assert-true (every #'digit-char-p code))))

(deftest test-compute-totp-deterministic
  "Same secret and time produce same code"
  (let ((secret (totp:generate-totp-secret)))
    (assert-equal (totp:compute-totp secret :time 1000000)
                  (totp:compute-totp secret :time 1000000))))

(deftest test-compute-totp-different-times
  "Different time steps produce different codes (usually)"
  (let* ((secret (totp:generate-totp-secret))
         (code1 (totp:compute-totp secret :time 1000000))
         (code2 (totp:compute-totp secret :time 1000030)))
    ;; These could theoretically collide, but extremely unlikely
    (assert-not (string= code1 code2))))

(deftest test-compute-totp-rfc-4226-test-vector
  "Verify HMAC-SHA1 based HOTP against RFC 4226 test vector.
   Secret = '12345678901234567890' (ASCII), counter = 0 => expected '755224'"
  (let ((secret (sb-ext:string-to-octets "12345678901234567890" :external-format :ascii)))
    ;; counter=0 maps to time=0 with time-step=1 (HOTP mode)
    (assert-equal "755224" (totp:compute-totp secret :time 0 :time-step 1))))

;;; ============================================================================
;;; TOTP Verification
;;; ============================================================================

(deftest test-verify-totp-exact-match
  "Exact time match verifies"
  (let* ((secret (totp:generate-totp-secret))
         (time 1000000)
         (code (totp:compute-totp secret :time time)))
    (assert-true (totp:verify-totp secret code :time time))))

(deftest test-verify-totp-within-window
  "Code within window verifies"
  (let* ((secret (totp:generate-totp-secret))
         (time 1000000)
         (code (totp:compute-totp secret :time time)))
    ;; Verify with time offset by one step
    (assert-true (totp:verify-totp secret code :time (+ time 30) :window 1))))

(deftest test-verify-totp-outside-window
  "Code outside window fails"
  (let* ((secret (totp:generate-totp-secret))
         (time 1000000)
         (code (totp:compute-totp secret :time time)))
    ;; Verify with time offset by 3 steps, window 1
    (assert-not (totp:verify-totp secret code :time (+ time 90) :window 1))))

(deftest test-verify-totp-wrong-code
  "Wrong code fails verification"
  (let ((secret (totp:generate-totp-secret)))
    (assert-not (totp:verify-totp secret "000000" :time 1000000))))

;;; ============================================================================
;;; Provisioning URI
;;; ============================================================================

(deftest test-provisioning-uri-format
  "Provisioning URI has correct otpauth format"
  (let* ((secret (totp:generate-totp-secret))
         (uri (totp:totp-provisioning-uri secret
                :issuer "TestApp"
                :account-name "alice@example.com")))
    (assert-true (search "otpauth://totp/" uri))
    (assert-true (search "TestApp" uri))
    (assert-true (search "alice@example.com" uri))
    (assert-true (search "secret=" uri))
    (assert-true (search "algorithm=SHA1" uri))
    (assert-true (search "digits=6" uri))
    (assert-true (search "period=30" uri))))

(deftest test-provisioning-uri-no-padding
  "Provisioning URI secret has no base32 padding"
  (let* ((secret (totp:generate-totp-secret))
         (uri (totp:totp-provisioning-uri secret :issuer "Test"))
         ;; Extract the secret value between "secret=" and "&"
         (start (+ (search "secret=" uri) 7))
         (end (position #\& uri :start start))
         (secret-value (subseq uri start end)))
    (assert-not (search "=" secret-value))))
