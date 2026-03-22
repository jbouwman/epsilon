;;;; JWK (JSON Web Key) - RFC 7517
;;;;
;;;; Serialize and deserialize cryptographic keys to/from JWK and JWKS
;;;; (JSON Web Key Set) format. Supports RSA and EC P-256 key types.
;;;;
;;;; Example:
;;;;   (key-to-jwk ec-key :kid "key-1" :use "sig" :alg "ES256")
;;;;   ;; => ((:kty . "EC") (:crv . "P-256") (:x . "...") (:y . "...") (:kid . "key-1") ...)
;;;;
;;;;   (keys-to-jwks (list jwk1 jwk2))
;;;;   ;; => "{\"keys\":[...]}"

(defpackage epsilon.crypto.jwk
  (:use :cl)
  (:require (epsilon.json json)
            (epsilon.map map)
            (epsilon.base-encode enc))
  (:export key-to-jwk
           key-from-jwk
           keys-to-jwks
           keys-from-jwks
           jwk-to-json
           jwks-to-json)
  (:enter t))

;;; ---------------------------------------------------------------------------
;;; Internal helpers
;;; ---------------------------------------------------------------------------

(defun %find-fn (pkg-name fn-name)
  "Find a function by package and symbol name strings."
  (let ((pkg (find-package pkg-name)))
    (when pkg
      (let ((sym (find-symbol fn-name pkg)))
        (when (and sym (fboundp sym)) (fdefinition sym))))))

(defun %integer-to-bytes (n len)
  "Encode integer N as a big-endian byte vector of exactly LEN bytes."
  (let ((bytes (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from (1- len) downto 0
          for shift from 0 by 8
          do (setf (aref bytes i) (logand (ash n (- shift)) #xff)))
    bytes))

(defun %bytes-to-integer (bytes)
  "Convert big-endian byte vector to integer."
  (let ((n 0))
    (loop for b across bytes
          do (setf n (logior (ash n 8) b)))
    n))

(defun %integer-byte-length (n)
  "Minimum number of bytes to represent unsigned integer N."
  (if (zerop n) 1
      (ceiling (integer-length n) 8)))

(defun %base64url-encode-integer (n &optional fixed-len)
  "Encode integer N as base64url with no padding.
   If FIXED-LEN is given, encode as exactly that many bytes (zero-padded)."
  (let ((len (or fixed-len (%integer-byte-length n))))
    (enc:base64-encode-url (%integer-to-bytes n len))))

(defun %base64url-decode-integer (str)
  "Decode base64url string to integer."
  (%bytes-to-integer (enc:base64-decode-url str)))

;;; ---------------------------------------------------------------------------
;;; Key to JWK
;;; ---------------------------------------------------------------------------

(defun key-to-jwk (key &key kid use alg key-ops)
  "Convert a native-key to a JWK alist (RFC 7517).

   KEY is an epsilon.crypto.native:native-key struct.
   Returns an alist with standard JWK fields.
   Only public key components are included (private keys are never serialized
   to JWK by default -- use key-to-jwk-private for that)."
  (let* ((type-fn (%find-fn :epsilon.crypto.native "NATIVE-KEY-TYPE"))
         (material-fn (%find-fn :epsilon.crypto.native "NATIVE-KEY-MATERIAL"))
         (private-p-fn (%find-fn :epsilon.crypto.native "NATIVE-KEY-PRIVATE-P"))
         (key-type (funcall type-fn key))
         (material (funcall material-fn key))
         (is-private (funcall private-p-fn key))
         (base (ecase key-type
                 (:ec-p256 (%ec-p256-to-jwk material is-private))
                 (:rsa (%rsa-to-jwk material is-private)))))
    ;; Append optional metadata
    (when kid (push (cons :kid kid) base))
    (when use (push (cons :use use) base))
    (when alg (push (cons :alg alg) base))
    (when key-ops (push (cons :key_ops key-ops) base))
    ;; Ensure kty is first for convention
    (let ((kty-pair (assoc :kty base)))
      (cons kty-pair (remove kty-pair base)))))

(defun %ec-p256-to-jwk (material is-private)
  "Convert EC P-256 key material to JWK fields.
   Private key material is an integer (scalar d).
   Public key material is SEC1 uncompressed bytes (65 bytes: 04 || x || y)."
  (let (x-bytes y-bytes)
    (if is-private
        ;; Derive public point from private scalar
        (let ((pub-from-priv (%find-fn :epsilon.ssl.ecdsa "ECDSA-PUBLIC-KEY-FROM-PRIVATE"))
              (point-encode (%find-fn :epsilon.ssl.ec-p256 "P256-POINT-ENCODE-UNCOMPRESSED")))
          (let* ((point (funcall pub-from-priv material))
                 (sec1 (funcall point-encode point)))
            (setf x-bytes (subseq sec1 1 33))
            (setf y-bytes (subseq sec1 33 65))))
        ;; Public key: SEC1 uncompressed format
        (progn
          (setf x-bytes (subseq material 1 33))
          (setf y-bytes (subseq material 33 65))))
    (list (cons :kty "EC")
          (cons :crv "P-256")
          (cons :x (enc:base64-encode-url x-bytes))
          (cons :y (enc:base64-encode-url y-bytes)))))

(defun %rsa-to-jwk (material is-private)
  "Convert RSA key material to JWK fields.
   Extracts modulus (n) and exponent (e) for the public key."
  (let ((n-fn (if is-private
                  (%find-fn :epsilon.ssl.rsa "RSA-PRIVATE-KEY-N")
                  (%find-fn :epsilon.ssl.rsa "RSA-PUBLIC-KEY-N")))
        (e-fn (if is-private
                  (%find-fn :epsilon.ssl.rsa "RSA-PRIVATE-KEY-E")
                  (%find-fn :epsilon.ssl.rsa "RSA-PUBLIC-KEY-E"))))
    (let ((n (funcall n-fn material))
          (e (funcall e-fn material)))
      (list (cons :kty "RSA")
            (cons :n (%base64url-encode-integer n))
            (cons :e (%base64url-encode-integer e))))))

;;; ---------------------------------------------------------------------------
;;; JWK to Key
;;; ---------------------------------------------------------------------------

(defun key-from-jwk (jwk)
  "Convert a JWK alist to a native-key struct.
   Supports RSA and EC P-256 public keys."
  (let ((kty (cdr (assoc :kty jwk))))
    (cond
      ((string-equal kty "EC") (%jwk-to-ec-p256 jwk))
      ((string-equal kty "RSA") (%jwk-to-rsa jwk))
      (t (error "Unsupported JWK key type: ~A" kty)))))

(defun %jwk-to-ec-p256 (jwk)
  "Convert an EC JWK to a native-key."
  (let ((crv (cdr (assoc :crv jwk)))
        (x-b64 (cdr (assoc :x jwk)))
        (y-b64 (cdr (assoc :y jwk))))
    (unless (string-equal crv "P-256")
      (error "Unsupported EC curve: ~A (only P-256 supported)" crv))
    (let* ((x-bytes (enc:base64-decode-url x-b64))
           (y-bytes (enc:base64-decode-url y-b64))
           ;; Construct SEC1 uncompressed point: 04 || x || y
           (sec1 (make-array 65 :element-type '(unsigned-byte 8))))
      (setf (aref sec1 0) #x04)
      (replace sec1 x-bytes :start1 1)
      (replace sec1 y-bytes :start1 33)
      (let ((make-key (%find-fn :epsilon.crypto.native "MAKE-NATIVE-KEY")))
        (funcall make-key :type :ec-p256 :private-p nil :material sec1)))))

(defun %jwk-to-rsa (jwk)
  "Convert an RSA JWK to a native-key."
  (let ((n (%base64url-decode-integer (cdr (assoc :n jwk))))
        (e (%base64url-decode-integer (cdr (assoc :e jwk))))
        (make-key (%find-fn :epsilon.crypto.native "MAKE-NATIVE-KEY"))
        (make-rsa-pub (%find-fn :epsilon.ssl.rsa "MAKE-RSA-PUBLIC-KEY")))
    (funcall make-key :type :rsa :private-p nil
             :material (funcall make-rsa-pub n e))))

;;; ---------------------------------------------------------------------------
;;; JWKS (JSON Web Key Set)
;;; ---------------------------------------------------------------------------

(defun keys-to-jwks (jwk-list)
  "Convert a list of JWK alists to a JWKS alist.
   Returns an alist with a single :keys entry."
  (list (cons :keys jwk-list)))

(defun keys-from-jwks (jwks)
  "Extract the list of JWK alists from a JWKS alist or parsed JSON."
  (let ((keys-entry (cdr (assoc :keys jwks))))
    (if (listp keys-entry) keys-entry nil)))

;;; ---------------------------------------------------------------------------
;;; JSON serialization
;;; ---------------------------------------------------------------------------

(defun jwk-to-json (jwk)
  "Serialize a JWK alist to a JSON string."
  (%alist-to-json jwk))

(defun jwks-to-json (jwk-list)
  "Serialize a list of JWK alists to a JWKS JSON string."
  (%alist-to-json (keys-to-jwks jwk-list)))

(defun %alist-to-json (alist)
  "Encode a keyword-keyed alist to JSON."
  (json:encode-to-string
   (loop for (key . value) in alist
         collect (cons (if (keywordp key)
                           (string-downcase (symbol-name key))
                           (string key))
                       (if (and (listp value) (consp (car value)))
                           ;; Nested alist (like keys array of objects)
                           (mapcar #'%alist-to-json-obj value)
                           value)))))

(defun %alist-to-json-obj (alist)
  "Convert a nested alist to a map for JSON encoding."
  (let ((m (map:make-map)))
    (loop for (key . value) in alist
          do (map:put m (if (keywordp key)
                            (string-downcase (symbol-name key))
                            (string key))
                      value))
    m))
