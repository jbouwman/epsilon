;;;; TOTP (Time-Based One-Time Password) - RFC 6238
;;;;
;;;; Implements TOTP for multi-factor authentication. Uses HMAC-SHA1 as
;;;; the default algorithm per the standard.
;;;;
;;;; Example:
;;;;   (let ((secret (generate-totp-secret)))
;;;;     (compute-totp secret)          ;; => "482901"
;;;;     (verify-totp secret "482901")) ;; => T

(defpackage epsilon.crypto.totp
  (:use :cl)
  (:require (epsilon.ssl ssl)
            (epsilon.base-encode enc))
  (:export generate-totp-secret
           compute-totp
           verify-totp
           totp-provisioning-uri)
  (:enter t))

;;; ---------------------------------------------------------------------------
;;; HMAC-SHA1 (not in epsilon.ssl.hmac which only covers SHA-256/384/512)
;;; ---------------------------------------------------------------------------

(defconstant +sha1-block-size+ 64)
(defconstant +sha1-digest-size+ 20)

(defun %find-fn (pkg-name fn-name)
  "Find a function by package and symbol name strings."
  (let ((pkg (find-package pkg-name)))
    (when pkg
      (let ((sym (find-symbol fn-name pkg)))
        (when (and sym (fboundp sym)) (fdefinition sym))))))

(defun hmac-sha1 (key message)
  "Compute HMAC-SHA1(key, message) per RFC 2104.
   KEY and MESSAGE are byte vectors. Returns a 20-byte digest."
  (let ((sha1-fn (%find-fn :epsilon.ssl.sha1 "SHA1")))
    (unless sha1-fn
      (error "HMAC-SHA1 requires epsilon.ssl.sha1"))
    (let ((key-block (make-array +sha1-block-size+ :element-type '(unsigned-byte 8)
                                                    :initial-element 0)))
      ;; If key > block size, hash it first
      (if (> (length key) +sha1-block-size+)
          (let ((hashed (funcall sha1-fn key)))
            (replace key-block hashed))
          (replace key-block key))
      ;; Build ipad and opad XOR'd key blocks
      (let ((ipad-key (make-array +sha1-block-size+ :element-type '(unsigned-byte 8)))
            (opad-key (make-array +sha1-block-size+ :element-type '(unsigned-byte 8))))
        (loop for i below +sha1-block-size+
              do (setf (aref ipad-key i) (logxor (aref key-block i) #x36))
                 (setf (aref opad-key i) (logxor (aref key-block i) #x5c)))
        ;; HMAC = H(opad-key || H(ipad-key || message))
        (let* ((inner-data (concatenate '(vector (unsigned-byte 8)) ipad-key message))
               (inner-hash (funcall sha1-fn inner-data))
               (outer-data (concatenate '(vector (unsigned-byte 8)) opad-key inner-hash)))
          (funcall sha1-fn outer-data))))))

;;; ---------------------------------------------------------------------------
;;; TOTP core
;;; ---------------------------------------------------------------------------

(defconstant +default-time-step+ 30)
(defconstant +default-digits+ 6)
(defconstant +default-secret-bytes+ 20)

(defun generate-totp-secret (&key (bytes +default-secret-bytes+))
  "Generate a random TOTP secret. Returns the raw byte vector.
   Use (enc:base32-encode secret) to get the user-visible base32 string."
  (ssl:random-bytes bytes))

(defun %unix-time ()
  "Return current Unix epoch time in seconds."
  (- (get-universal-time) 2208988800))

(defun %integer-to-8-bytes (n)
  "Encode integer N as an 8-byte big-endian byte vector."
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from 7 downto 0
          for shift from 0 by 8
          do (setf (aref bytes i) (logand (ash n (- shift)) #xff)))
    bytes))

(defun %dynamic-truncate (hmac-result digits)
  "Extract a DIGITS-length code from HMAC result via dynamic truncation (RFC 4226 Section 5.4)."
  (let* ((offset (logand (aref hmac-result (1- (length hmac-result))) #x0f))
         (bin-code (logior (ash (logand (aref hmac-result offset) #x7f) 24)
                           (ash (aref hmac-result (+ offset 1)) 16)
                           (ash (aref hmac-result (+ offset 2)) 8)
                           (aref hmac-result (+ offset 3))))
         (modulus (expt 10 digits)))
    (format nil "~V,'0D" digits (mod bin-code modulus))))

(defun compute-totp (secret &key (time nil) (time-step +default-time-step+)
                                  (digits +default-digits+))
  "Compute a TOTP code from SECRET at the given TIME.
   SECRET is a byte vector (the raw secret, not base32-encoded).
   TIME is Unix epoch seconds (defaults to now).
   Returns a string of DIGITS digits."
  (let* ((unix-time (or time (%unix-time)))
         (counter (floor unix-time time-step))
         (counter-bytes (%integer-to-8-bytes counter))
         (hmac-result (hmac-sha1 secret counter-bytes)))
    (%dynamic-truncate hmac-result digits)))

(defun verify-totp (secret code &key (time nil) (time-step +default-time-step+)
                                       (digits +default-digits+) (window 1))
  "Verify a TOTP code against SECRET.
   Checks the current time step and +/- WINDOW adjacent steps.
   Returns T if valid, NIL otherwise."
  (let ((unix-time (or time (%unix-time))))
    (loop for offset from (- window) to window
          for check-time = (+ unix-time (* offset time-step))
          when (string= code (compute-totp secret :time check-time
                                                   :time-step time-step
                                                   :digits digits))
          return t)))

(defun totp-provisioning-uri (secret &key issuer account-name
                                          (algorithm "SHA1") (digits 6) (period 30))
  "Generate an otpauth:// URI for QR code provisioning.
   SECRET is a byte vector (will be base32-encoded).
   ISSUER is the service name, ACCOUNT-NAME is the user identifier."
  (let ((b32-secret (enc:base32-encode secret)))
    ;; Remove padding for compatibility
    (let ((clean-secret (string-right-trim "=" b32-secret)))
      (format nil "otpauth://totp/~A:~A?secret=~A&issuer=~A&algorithm=~A&digits=~D&period=~D"
              (or issuer "Kreisler")
              (or account-name "user")
              clean-secret
              (or issuer "Kreisler")
              algorithm
              digits
              period))))
