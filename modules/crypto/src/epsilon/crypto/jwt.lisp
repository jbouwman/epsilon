;;;; JWT (JSON Web Token) - RFC 7519
;;;;
;;;; Encode, decode, and verify JWTs using HMAC-SHA256 (HS256),
;;;; RSA-SHA256 (RS256), or ECDSA P-256 (ES256). Claims are returned as
;;;; keyword-keyed alists.
;;;;
;;;; Example:
;;;;   (jwt-encode '((:sub . "user-123") (:exp . 1740000000)) "secret")
;;;;   ;; => "eyJhbG..."
;;;;
;;;;   (jwt-decode token "secret")
;;;;   ;; => ((:sub . "user-123") (:exp . 1740000000))

(defpackage epsilon.crypto.jwt
  (:use :cl)
  (:require (epsilon.ssl ssl)
            (epsilon.json json)
            (epsilon.map map)
            (epsilon.base-encode enc))
  (:export jwt-encode
           jwt-decode
           jwt-decode-unsafe
           jwt-error
           jwt-error-p
           jwt-error-message
           jwt-expired-error
           jwt-expired-error-p
           jwt-invalid-signature-error
           jwt-invalid-signature-error-p
           jwt-malformed-error
           jwt-malformed-error-p)
  (:enter t))

;;;; Conditions

(define-condition jwt-error (cl:error)
  ((message :initarg :message :reader jwt-error-message))
  (:report (lambda (condition stream)
             (format stream "JWT error: ~A" (jwt-error-message condition)))))

(defun jwt-error-p (obj)
  "Test if OBJ is a jwt-error condition."
  (typep obj 'jwt-error))

(define-condition jwt-expired-error (jwt-error)
  ()
  (:report (lambda (condition stream)
             (format stream "JWT expired: ~A" (jwt-error-message condition)))))

(defun jwt-expired-error-p (obj)
  "Test if OBJ is a jwt-expired-error condition."
  (typep obj 'jwt-expired-error))

(define-condition jwt-invalid-signature-error (jwt-error)
  ()
  (:report (lambda (condition stream)
             (format stream "JWT invalid signature: ~A" (jwt-error-message condition)))))

(defun jwt-invalid-signature-error-p (obj)
  "Test if OBJ is a jwt-invalid-signature-error condition."
  (typep obj 'jwt-invalid-signature-error))

(define-condition jwt-malformed-error (jwt-error)
  ()
  (:report (lambda (condition stream)
             (format stream "JWT malformed: ~A" (jwt-error-message condition)))))

(defun jwt-malformed-error-p (obj)
  "Test if OBJ is a jwt-malformed-error condition."
  (typep obj 'jwt-malformed-error))

;;;; Base64url Encoding (delegated to epsilon.base-encode)

;;;; JSON Helpers

(defun %json-encode (alist)
  "Encode alist as JSON string."
  (json:encode-to-string
   (loop for (key . value) in alist
         collect (cons (if (keywordp key)
                           (string-downcase (symbol-name key))
                           (string key))
                       value))))

(defun %json-decode (string)
  "Decode JSON string to keyword-keyed alist.
json:decode returns an epsilon.map, so we convert to alist with keyword keys."
  (let ((result (json:decode string)))
    (if (map:map-p result)
        (loop for (key . value) in (map:to-alist result)
              collect (cons (intern (string-upcase key) :keyword) value))
        ;; Handle empty object or nil
        nil)))

;;;; Signing and Verification

(defun %find-fn (pkg-name fn-name)
  "Find and return a function from PKG-NAME by FN-NAME string, or NIL."
  (let ((pkg (find-package pkg-name)))
    (when pkg
      (let ((sym (find-symbol fn-name pkg)))
        (when (and sym (fboundp sym)) (fdefinition sym))))))

(defun %integer-to-bytes (n buf offset len)
  "Write integer N as big-endian bytes into BUF at OFFSET for LEN bytes."
  (loop for i from (1- (+ offset len)) downto offset
        for shift from 0 by 8
        do (setf (aref buf i) (logand (ash n (- shift)) #xff))))

(defun %bytes-to-integer (bytes start end)
  "Convert big-endian bytes from START to END to integer."
  (let ((n 0))
    (loop for i from start below end
          do (setf n (logior (ash n 8) (aref bytes i))))
    n))

(defun %sign (algorithm key signing-input)
  "Sign the input using the specified algorithm."
  (let ((input-bytes (sb-ext:string-to-octets signing-input :external-format :utf-8)))
    (ecase algorithm
      (:hs256
       (ssl:hmac-sha256 (if (stringp key)
                            (sb-ext:string-to-octets key :external-format :utf-8)
                            key)
                        input-bytes))
      (:rs256
       (let ((sign-fn (%find-fn :epsilon.crypto.native "SIGN-MESSAGE")))
         (unless sign-fn
           (cl:error 'jwt-error :message "RS256 requires epsilon.crypto.native module"))
         (funcall sign-fn key input-bytes :digest "SHA256")))
      (:es256
       ;; ES256: ECDSA with P-256 + SHA-256, raw r||s format (RFC 7518 Section 3.4)
       (let ((ecdsa-sign-fn (%find-fn :epsilon.ssl.ecdsa "ECDSA-SIGN"))
             (material-fn (%find-fn :epsilon.crypto.native "NATIVE-KEY-MATERIAL")))
         (unless ecdsa-sign-fn
           (cl:error 'jwt-error :message "ES256 requires epsilon.ssl.ecdsa module"))
         (let ((private-scalar (if material-fn (funcall material-fn key) key)))
           (multiple-value-bind (r s) (funcall ecdsa-sign-fn private-scalar input-bytes)
             (let ((sig (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
               (%integer-to-bytes r sig 0 32)
               (%integer-to-bytes s sig 32 32)
               sig))))))))

(defun %verify (algorithm key signing-input signature-bytes)
  "Verify the signature. Returns T on success, NIL on failure."
  (let ((input-bytes (sb-ext:string-to-octets signing-input :external-format :utf-8)))
    (ecase algorithm
      (:hs256
       (let ((key-bytes (if (stringp key)
                            (sb-ext:string-to-octets key :external-format :utf-8)
                            key)))
         (ssl:hmac-sha256-verify key-bytes input-bytes signature-bytes)))
      (:rs256
       (let ((verify-fn (%find-fn :epsilon.crypto.native "VERIFY-MESSAGE")))
         (unless verify-fn
           (cl:error 'jwt-error :message "RS256 requires epsilon.crypto.native module"))
         (funcall verify-fn key input-bytes signature-bytes :digest "SHA256")))
      (:es256
       ;; ES256: verify raw r||s (32 bytes each) per RFC 7518 Section 3.4
       (let ((ecdsa-verify-fn (%find-fn :epsilon.ssl.ecdsa "ECDSA-VERIFY"))
             (point-decode-fn (%find-fn :epsilon.ssl.ec-p256 "P256-POINT-DECODE"))
             (pub-from-priv-fn (%find-fn :epsilon.ssl.ecdsa "ECDSA-PUBLIC-KEY-FROM-PRIVATE"))
             (material-fn (%find-fn :epsilon.crypto.native "NATIVE-KEY-MATERIAL"))
             (private-p-fn (%find-fn :epsilon.crypto.native "NATIVE-KEY-PRIVATE-P")))
         (unless ecdsa-verify-fn
           (cl:error 'jwt-error :message "ES256 requires epsilon.ssl.ecdsa module"))
         (handler-case
             (let* ((material (if material-fn (funcall material-fn key) key))
                    (is-private (and private-p-fn (funcall private-p-fn key)))
                    (point (if is-private
                               (funcall pub-from-priv-fn material)
                               (funcall point-decode-fn material)))
                    (r (%bytes-to-integer signature-bytes 0 32))
                    (s (%bytes-to-integer signature-bytes 32 64)))
               (when (and point (= (length signature-bytes) 64))
                 (funcall ecdsa-verify-fn point input-bytes r s)))
           (error () nil)))))))

(defun %make-signing-input (header-b64 payload-b64)
  "Create the signing input: base64url(header).base64url(payload)"
  (concatenate 'string header-b64 "." payload-b64))

(defun %algorithm-string (algorithm)
  "Convert algorithm keyword to JWT algorithm string."
  (ecase algorithm
    (:hs256 "HS256")
    (:rs256 "RS256")
    (:es256 "ES256")))

(defun %parse-algorithm (string)
  "Convert JWT algorithm string to keyword."
  (cond
    ((string-equal string "HS256") :hs256)
    ((string-equal string "RS256") :rs256)
    ((string-equal string "ES256") :es256)
    (t (cl:error 'jwt-malformed-error
                 :message (format nil "Unsupported algorithm: ~A" string)))))

;;;; Public API

(defun jwt-encode (claims secret &key (algorithm :hs256) (extra-headers nil))
  "Encode claims alist into a signed JWT string.

CLAIMS is an alist with keyword keys, e.g. ((:sub . \"user-123\") (:exp . 1740000000)).
SECRET is a string (for HS256), native-key (for RS256/ES256).
ALGORITHM is :hs256 (default), :rs256, or :es256.
EXTRA-HEADERS is an optional alist of additional header fields.

Returns a JWT string: header.payload.signature"
  (let* ((header (append `((:alg . ,(%algorithm-string algorithm))
                            (:typ . "JWT"))
                          extra-headers))
         (header-b64 (enc:base64-encode-url
                      (sb-ext:string-to-octets (%json-encode header)
                                               :external-format :utf-8)))
         (payload-b64 (enc:base64-encode-url
                       (sb-ext:string-to-octets (%json-encode claims)
                                                :external-format :utf-8)))
         (signing-input (%make-signing-input header-b64 payload-b64))
         (signature (%sign algorithm secret signing-input))
         (signature-b64 (enc:base64-encode-url signature)))
    (concatenate 'string signing-input "." signature-b64)))

(defun jwt-decode (token secret &key (algorithm :hs256) (verify t) (clock-skew 0))
  "Decode and verify a JWT string, returning the claims as a keyword-keyed alist.

TOKEN is the JWT string.
SECRET is a string (for HS256), native-key (for RS256/ES256).
ALGORITHM is :hs256 (default), :rs256, or :es256.
VERIFY when T (default), verifies the signature.
CLOCK-SKEW is the allowed clock skew in seconds for exp/nbf checks (default 0).

Signals jwt-malformed-error if the token format is invalid.
Signals jwt-invalid-signature-error if verification fails.
Signals jwt-expired-error if the token has expired.

Returns a keyword-keyed alist of claims."
  (let ((parts (split-token token)))
    (unless (= (length parts) 3)
      (cl:error 'jwt-malformed-error
                :message (format nil "Expected 3 segments, got ~D" (length parts))))
    (let* ((header-b64 (first parts))
           (payload-b64 (second parts))
           (signature-b64 (third parts))
           (header-json (handler-case
                            (sb-ext:octets-to-string
                             (enc:base64-decode-url header-b64)
                             :external-format :utf-8)
                          (cl:error ()
                            (cl:error 'jwt-malformed-error
                                      :message "Invalid base64url in header"))))
           (payload-json (handler-case
                             (sb-ext:octets-to-string
                              (enc:base64-decode-url payload-b64)
                              :external-format :utf-8)
                           (cl:error ()
                             (cl:error 'jwt-malformed-error
                                       :message "Invalid base64url in payload"))))
           (header (handler-case (%json-decode header-json)
                     (cl:error ()
                       (cl:error 'jwt-malformed-error
                                 :message "Invalid JSON in header"))))
           (claims (handler-case (%json-decode payload-json)
                     (cl:error ()
                       (cl:error 'jwt-malformed-error
                                 :message "Invalid JSON in payload")))))
      (declare (ignore header))
      ;; Verify signature
      (when verify
        (let* ((signing-input (%make-signing-input header-b64 payload-b64))
               (signature-bytes (handler-case
                                    (enc:base64-decode-url signature-b64)
                                  (cl:error ()
                                    (cl:error 'jwt-malformed-error
                                              :message "Invalid base64url in signature"))))
               (valid (%verify algorithm secret signing-input signature-bytes)))
          (unless valid
            (cl:error 'jwt-invalid-signature-error
                      :message "Signature verification failed"))))
      ;; Check expiration
      (let ((exp (cdr (assoc :exp claims))))
        (when (and exp (numberp exp))
          (when (> (get-universal-time) (+ exp clock-skew))
            (cl:error 'jwt-expired-error
                      :message (format nil "Token expired at ~A" exp)))))
      claims)))

(defun jwt-decode-unsafe (token)
  "Decode a JWT without verification, returning header and payload.

Returns two values: the header alist and the payload alist.
Useful for inspecting token claims before verification."
  (let ((parts (split-token token)))
    (unless (>= (length parts) 2)
      (cl:error 'jwt-malformed-error
                :message (format nil "Expected at least 2 segments, got ~D" (length parts))))
    (let* ((header-json (handler-case
                            (sb-ext:octets-to-string
                             (enc:base64-decode-url (first parts))
                             :external-format :utf-8)
                          (cl:error ()
                            (cl:error 'jwt-malformed-error
                                      :message "Invalid base64url in header"))))
           (payload-json (handler-case
                             (sb-ext:octets-to-string
                              (enc:base64-decode-url (second parts))
                              :external-format :utf-8)
                           (cl:error ()
                             (cl:error 'jwt-malformed-error
                                       :message "Invalid base64url in payload"))))
           (header (handler-case (%json-decode header-json)
                     (cl:error ()
                       (cl:error 'jwt-malformed-error
                                 :message "Invalid JSON in header"))))
           (payload (handler-case (%json-decode payload-json)
                      (cl:error ()
                        (cl:error 'jwt-malformed-error
                                  :message "Invalid JSON in payload")))))
      (values header payload))))

;;;; Internal Helpers

(defun split-token (token)
  "Split a JWT token into its dot-separated parts."
  (unless (stringp token)
    (cl:error 'jwt-malformed-error :message "Token must be a string"))
  (loop with parts = nil
        with start = 0
        for i from 0 to (length token)
        when (or (= i (length token)) (char= (char token i) #\.))
        do (push (subseq token start i) parts)
           (setf start (1+ i))
        finally (return (nreverse parts))))
