;;;; Native Age Decryption
;;;;
;;;; Implements the age-encryption.org/v1 file format for X25519 recipients.
;;;; Decrypts age-encrypted data using epsilon.crypto primitives:
;;;;   - X25519 ECDH for key agreement
;;;;   - HKDF-SHA256 for key derivation
;;;;   - ChaCha20-Poly1305 for file key unwrapping
;;;;   - ChaCha20-Poly1305 STREAM for payload decryption
;;;;
;;;; The age file key is 16 bytes. Payload uses the STREAM construction
;;;; with 64KB chunks. For SOPS, payloads are small (32-byte data keys),
;;;; so there is typically a single STREAM chunk.
;;;;
;;;; Reference: https://age-encryption.org/v1

(defpackage :epsilon.crypto.age
  (:use :cl)
  (:local-nicknames
   (:ssl :epsilon.ssl)
   (:b64 :epsilon.base-encode)
   (:log :epsilon.log))
  (:export
   ;; Bech32 decoding (for age identity keys)
   #:bech32-decode

   ;; Age identity parsing
   #:decode-age-identity

   ;; Age file format
   #:parse-age-file
   #:age-header
   #:age-header-p
   #:age-header-recipients
   #:age-header-mac
   #:age-header-payload
   #:age-recipient
   #:age-recipient-type
   #:age-recipient-args
   #:age-recipient-body

   ;; Age decryption
   #:decrypt-age-file
   #:decrypt-age-x25519

   ;; Conditions
   #:age-error
   #:age-error-message)
  (:enter t))

;;; ============================================================================
;;; Conditions
;;; ============================================================================

(define-condition age-error (error)
  ((message :initarg :message :reader age-error-message))
  (:report (lambda (c s)
             (format s "Age error: ~A" (age-error-message c)))))

;;; ============================================================================
;;; Bech32 Decoder
;;; ============================================================================

;; Bech32 character set (RFC 3548 Section 5, BIP-173)
(defparameter *bech32-charset* "qpzry9x8gf2tvdw0s3jn54khce6mua7l")

(defparameter *bech32-reverse*
  (let ((table (make-array 128 :initial-element -1)))
    (loop for i from 0 below (length *bech32-charset*)
          do (setf (aref table (char-code (char *bech32-charset* i))) i))
    table)
  "Reverse lookup table: ASCII code -> 5-bit value, -1 for invalid.")

(defun bech32-polymod (values)
  "Compute the Bech32 BCH checksum polymod."
  (let ((chk 1))
    (dolist (v values chk)
      (let ((b (ash chk -25)))
        (setf chk (logxor (ash (logand chk #x1ffffff) 5) v))
        (when (logbitp 0 b) (setf chk (logxor chk #x3b6a57b2)))
        (when (logbitp 1 b) (setf chk (logxor chk #x26508e6d)))
        (when (logbitp 2 b) (setf chk (logxor chk #x1ea119fa)))
        (when (logbitp 3 b) (setf chk (logxor chk #x3d4233dd)))
        (when (logbitp 4 b) (setf chk (logxor chk #x2a1462b3)))))))

(defun bech32-hrp-expand (hrp)
  "Expand the human-readable part for checksum computation."
  (let ((result nil))
    (loop for ch across hrp
          do (push (ash (char-code ch) -5) result))
    (push 0 result)
    (loop for ch across hrp
          do (push (logand (char-code ch) 31) result))
    (nreverse result)))

(defun bech32-verify-checksum (hrp data-values)
  "Verify a Bech32 checksum. Returns T if valid."
  (= 1 (bech32-polymod (append (bech32-hrp-expand hrp) data-values))))

(defun bech32-decode (bech32-string)
  "Decode a Bech32-encoded string.
   Returns (values hrp data-bytes) where data-bytes is the decoded payload.
   Signals age-error on invalid input."
  (let* ((str (string-upcase bech32-string))
         (last-1 (position #\1 str :from-end t)))
    (unless last-1
      (error 'age-error :message "Invalid Bech32: no separator '1' found"))
    (when (< last-1 1)
      (error 'age-error :message "Invalid Bech32: empty HRP"))
    (when (< (- (length str) last-1 1) 6)
      (error 'age-error :message "Invalid Bech32: data too short for checksum"))

    (let* ((hrp (string-downcase (subseq str 0 last-1)))
           (data-part (subseq str (1+ last-1)))
           (data-values nil))
      ;; Decode the data characters to 5-bit values
      (loop for ch across data-part
            for code = (char-code (char-downcase ch))
            do (when (or (>= code 128) (= -1 (aref *bech32-reverse* code)))
                 (error 'age-error
                        :message (format nil "Invalid Bech32 character: ~C" ch)))
               (push (aref *bech32-reverse* code) data-values))
      (setf data-values (nreverse data-values))

      ;; Verify checksum
      (unless (bech32-verify-checksum hrp data-values)
        (error 'age-error :message "Invalid Bech32 checksum"))

      ;; Remove the 6-character checksum
      (let ((payload-5bit (butlast data-values 6)))
        ;; Convert from 5-bit groups to 8-bit bytes
        (values hrp (convert-bits payload-5bit 5 8 nil))))))

(defun convert-bits (data from-bits to-bits &optional (pad t))
  "Convert a list of values from FROM-BITS width to TO-BITS width.
   Returns a byte vector."
  (let ((acc 0)
        (bits 0)
        (result nil)
        (maxv (1- (ash 1 to-bits))))
    (dolist (value data)
      (setf acc (logior (ash acc from-bits) value))
      (incf bits from-bits)
      (loop while (>= bits to-bits)
            do (decf bits to-bits)
               (push (logand (ash acc (- bits)) maxv) result)))
    (when pad
      (when (plusp bits)
        (push (logand (ash acc (- to-bits bits)) maxv) result)))
    (let ((bytes (nreverse result)))
      (make-array (length bytes)
                  :element-type '(unsigned-byte 8)
                  :initial-contents bytes))))

;;; ============================================================================
;;; Age Identity Decoding
;;; ============================================================================

(defun decode-age-identity (identity-string)
  "Decode an AGE-SECRET-KEY-1... Bech32 string to a 32-byte X25519 private key.
   Returns a byte vector of 32 bytes."
  (multiple-value-bind (hrp data) (bech32-decode identity-string)
    (unless (string= hrp "age-secret-key-")
      (error 'age-error
             :message (format nil "Invalid age identity HRP: expected 'age-secret-key-', got '~A'" hrp)))
    (unless (= (length data) 32)
      (error 'age-error
             :message (format nil "Invalid age identity: expected 32 bytes, got ~D" (length data))))
    data))

;;; ============================================================================
;;; Base64 Utilities (age uses standard base64, sometimes without padding)
;;; ============================================================================

(defun base64-decode-nopad (string)
  "Decode a base64 string, adding padding if necessary.
   Age uses standard base64 without padding."
  (let* ((rem (mod (length string) 4))
         (padded (if (zerop rem)
                     string
                     (concatenate 'string string
                                  (make-string (- 4 rem) :initial-element #\=)))))
    (b64:base64-decode padded)))

;;; ============================================================================
;;; Age File Format Parser
;;; ============================================================================

(defstruct (age-header (:constructor %make-age-header))
  "Parsed age file header."
  (recipients nil :type list)     ; list of recipient stanzas
  (mac nil :type (or null (vector (unsigned-byte 8))))
  (payload nil :type (or null (vector (unsigned-byte 8)))))

(defstruct (age-recipient (:constructor %make-age-recipient))
  "A single age recipient stanza."
  (type nil :type (or string null))
  (args nil :type list)           ; list of string arguments
  (body nil :type (or null (vector (unsigned-byte 8)))))

(defun parse-age-file (data)
  "Parse an age-encrypted file from raw bytes or a string.
   Returns an age-header struct.
   Uses latin-1 encoding for byte conversion to preserve binary payload."
  (let ((text (etypecase data
                (string data)
                ((vector (unsigned-byte 8))
                 (sb-ext:octets-to-string data :external-format :latin-1)))))
    (parse-age-text text)))

(defun parse-age-text (text)
  "Parse the textual age file format."
  (let ((lines (split-lines text))
        (pos 0)
        (recipients nil)
        (mac-bytes nil)
        (payload-start nil))

    ;; First line must be the version header
    (unless (and (> (length lines) 0)
                 (string= "age-encryption.org/v1" (aref lines 0)))
      (error 'age-error :message "Invalid age file: missing version header"))
    (incf pos)

    ;; Parse recipient stanzas
    (loop while (< pos (length lines))
          for line = (aref lines pos)
          do (cond
               ;; Recipient stanza header: -> TYPE ARG1 ARG2 ...
               ((and (>= (length line) 3)
                     (string= "-> " (subseq line 0 3)))
                (let* ((parts (split-spaces (subseq line 3)))
                       (type (first parts))
                       (args (rest parts))
                       (body-lines nil))
                  (incf pos)
                  ;; Read body lines (base64 encoded, no -> or --- prefix)
                  (loop while (and (< pos (length lines))
                                   (not (starts-with-p "-> " (aref lines pos)))
                                   (not (starts-with-p "---" (aref lines pos))))
                        do (push (aref lines pos) body-lines)
                           (incf pos))
                  (let* ((body-text (format nil "~{~A~}" (nreverse body-lines)))
                         (body-bytes (when (plusp (length body-text))
                                       (base64-decode-nopad body-text))))
                    (push (%make-age-recipient :type type
                                               :args args
                                               :body body-bytes)
                          recipients))))
               ;; Header MAC line: --- <mac-base64>
               ((starts-with-p "---" line)
                (let ((mac-text (string-trim '(#\Space) (subseq line 3))))
                  (when (plusp (length mac-text))
                    (setf mac-bytes (base64-decode-nopad mac-text))))
                (incf pos)
                ;; Everything after this is the binary payload
                (setf payload-start pos)
                (return))
               (t
                (error 'age-error
                       :message (format nil "Unexpected line in age header: ~A" line)))))

    ;; Extract binary payload
    ;; The payload is the raw bytes after the header MAC line
    ;; We need to find the byte offset of the payload in the original data
    (let ((payload (extract-payload text payload-start lines)))
      (%make-age-header
       :recipients (nreverse recipients)
       :mac mac-bytes
       :payload payload))))

(defun extract-payload (text payload-line-start lines)
  "Extract the binary payload bytes after the header section.
   The payload starts immediately after the --- line's newline."
  (when (and payload-line-start (< payload-line-start (length lines)))
    ;; Find the byte position after the --- line
    (let ((byte-pos 0)
          (line-count 0))
      (loop for i from 0 below (length text)
            do (when (char= (char text i) #\Newline)
                 (incf line-count)
                 (when (= line-count payload-line-start)
                   (setf byte-pos (1+ i))
                   (return))))
      ;; Convert the remaining text to bytes
      (when (< byte-pos (length text))
        (let ((payload-text (subseq text byte-pos)))
          (sb-ext:string-to-octets payload-text :external-format :latin-1))))))

(defun split-lines (text)
  "Split text into lines, returning a vector."
  (let ((lines nil)
        (start 0))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (push (string-right-trim '(#\Return) (subseq text start i)) lines)
               (setf start (1+ i)))
    (when (< start (length text))
      (push (string-right-trim '(#\Return) (subseq text start)) lines))
    (coerce (nreverse lines) 'vector)))

(defun split-spaces (text)
  "Split text on spaces."
  (let ((parts nil)
        (start 0)
        (len (length text)))
    (loop for i from 0 to len
          do (when (or (= i len) (char= (char text i) #\Space))
               (when (> i start)
                 (push (subseq text start i) parts))
               (setf start (1+ i))))
    (nreverse parts)))

(defun starts-with-p (prefix string)
  "Check if STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

;;; ============================================================================
;;; Age X25519 Recipient Decryption
;;; ============================================================================

(defun decrypt-age-x25519 (age-data identity-private-bytes)
  "Decrypt an age-encrypted file using an X25519 identity.

   AGE-DATA is the raw age file content (string or bytes).
   IDENTITY-PRIVATE-BYTES is the 32-byte X25519 private key.

   Returns the decrypted plaintext as a byte vector."
  (let ((header (parse-age-file age-data)))
    ;; Find X25519 recipient stanza
    (let ((x25519-recipient (find-if (lambda (r)
                                       (string= "X25519" (age-recipient-type r)))
                                     (age-header-recipients header))))
      (unless x25519-recipient
        (error 'age-error :message "No X25519 recipient stanza found"))

      ;; Extract ephemeral public key from args
      (unless (= 1 (length (age-recipient-args x25519-recipient)))
        (error 'age-error :message "X25519 stanza requires exactly one argument"))

      (let* ((ephemeral-share-b64 (first (age-recipient-args x25519-recipient)))
             (ephemeral-pub (base64-decode-nopad ephemeral-share-b64))
             (wrapped-key (age-recipient-body x25519-recipient)))

        (unless (= 32 (length ephemeral-pub))
          (error 'age-error
                 :message (format nil "Invalid ephemeral key size: ~D" (length ephemeral-pub))))

        ;; Derive identity public key and compute ECDH shared secret
        ;; using raw X25519 scalar multiplication (epsilon.ssl API)
        (let* ((identity-pub (ssl:x25519-base identity-private-bytes))

               ;; ECDH: shared_secret = X25519(identity_private, ephemeral_public)
               (shared-secret (ssl:x25519 identity-private-bytes ephemeral-pub)))

          ;; Derive wrap key via HKDF-SHA256
          ;; salt = ephemeral_pub || identity_pub (64 bytes)
          ;; info = "age-encryption.org/v1/X25519"
          (let* ((salt (concatenate '(vector (unsigned-byte 8)) ephemeral-pub identity-pub))
                 (info-bytes (sb-ext:string-to-octets "age-encryption.org/v1/X25519"
                                                       :external-format :utf-8))
                 (wrap-key (ssl:hkdf :sha256 salt shared-secret info-bytes 32)))

            ;; Unwrap the file key: ChaCha20-Poly1305 with nonce=zeros(12)
            ;; wrapped-key = ciphertext(16) || tag(16) = 32 bytes total
            (unless (= 32 (length wrapped-key))
              (error 'age-error
                     :message (format nil "Invalid wrapped key size: ~D (expected 32)"
                                      (length wrapped-key))))

            (let* ((ct (subseq wrapped-key 0 16))
                   (tag (subseq wrapped-key 16 32))
                   (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0))
                   (file-key (ssl:chacha20-poly1305-decrypt ct wrap-key nonce tag)))

              ;; Decrypt the STREAM payload
              (stream-decrypt file-key (age-header-payload header)))))))))

;;; ============================================================================
;;; STREAM Decryption (age payload encryption)
;;; ============================================================================

(defconstant +stream-chunk-size+ 65536
  "STREAM chunk size: 64KB of plaintext per chunk.")

(defun stream-decrypt (file-key payload)
  "Decrypt an age STREAM-encrypted payload.

   The payload format is:
     - 16-byte nonce
     - One or more chunks, each being ChaCha20-Poly1305 ciphertext + 16-byte tag
     - The last chunk has a flag byte of 0x01 in its nonce

   FILE-KEY is the 16-byte age file key.
   PAYLOAD is the raw encrypted payload bytes."
  (unless (>= (length payload) 16)
    (error 'age-error :message "STREAM payload too short: missing nonce"))

  ;; Extract STREAM nonce (first 16 bytes)
  (let* ((stream-nonce (subseq payload 0 16))
         ;; Derive payload key: HKDF-SHA256(file-key, salt=stream-nonce, info="payload", length=32)
         (payload-key (ssl:hkdf :sha256 stream-nonce file-key
                                (sb-ext:string-to-octets "payload" :external-format :utf-8)
                                32))
         (ciphertext (subseq payload 16))
         (plaintext-parts nil)
         (offset 0)
         (counter 0))

    ;; Process STREAM chunks
    ;; Each chunk: up to 64KB + 16 bytes tag of ciphertext
    ;; Nonce for each chunk: 11 bytes big-endian counter + 1 byte flag
    ;; Flag: 0x00 for non-final, 0x01 for final
    (loop
      (when (>= offset (length ciphertext))
        (error 'age-error :message "STREAM: unexpected end of ciphertext"))

      (let* ((remaining (- (length ciphertext) offset))
             (is-final (< remaining (+ +stream-chunk-size+ 16 16)))
             ;; Chunk size: min(remaining, 64KB + 16 tag)
             (chunk-size (min remaining (+ +stream-chunk-size+ 16)))
             (chunk-ct-len (- chunk-size 16))
             (chunk-ct (subseq ciphertext offset (+ offset chunk-ct-len)))
             (chunk-tag (subseq ciphertext (+ offset chunk-ct-len) (+ offset chunk-size)))
             ;; Build STREAM nonce: 11-byte big-endian counter + 1-byte flag
             (chunk-nonce (make-stream-nonce counter (if is-final 1 0))))

        ;; Decrypt this chunk
        (let ((chunk-plaintext (ssl:chacha20-poly1305-decrypt
                                chunk-ct payload-key chunk-nonce chunk-tag)))
          (push chunk-plaintext plaintext-parts))

        (incf offset chunk-size)
        (incf counter)

        (when is-final
          (return))))

    ;; Concatenate all plaintext chunks
    (let ((total-len (reduce #'+ plaintext-parts :key #'length))
          (pos 0))
      (let ((result (make-array total-len :element-type '(unsigned-byte 8))))
        (dolist (part (nreverse plaintext-parts))
          (replace result part :start1 pos)
          (incf pos (length part)))
        result))))

(defun make-stream-nonce (counter flag)
  "Build a 12-byte STREAM nonce: 11-byte big-endian counter + 1-byte flag."
  (let ((nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Write counter as 11-byte big-endian
    (let ((c counter))
      (loop for i from 10 downto 0
            do (setf (aref nonce i) (logand c #xff))
               (setf c (ash c -8))))
    ;; Last byte is the flag
    (setf (aref nonce 11) flag)
    nonce))

;;; ============================================================================
;;; High-Level Decryption
;;; ============================================================================

(defun decrypt-age-file (age-data identity-string)
  "Decrypt an age-encrypted file using an age identity string.

   AGE-DATA is the raw age file content (string or bytes).
   IDENTITY-STRING is the AGE-SECRET-KEY-1... Bech32 identity.

   Returns the decrypted plaintext as a byte vector."
  (let ((private-bytes (decode-age-identity identity-string)))
    (decrypt-age-x25519 age-data private-bytes)))
