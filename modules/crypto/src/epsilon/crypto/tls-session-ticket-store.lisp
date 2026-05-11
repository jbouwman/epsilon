;;;; TLS Session Ticket Encryption Key (STEK) Store
;;;;
;;;; A STEK is the symmetric key the server uses to seal opaque session
;;;; ticket payloads (RFC 8446 Section 4.6.1). Tickets are encrypted with
;;;; AES-256-GCM. The store holds at most two keys: the current key
;;;; (used for new seals) and the previous key (still accepted for opens
;;;; until the next rotation).
;;;;
;;;; Sealed ticket layout (all fields big-endian):
;;;;
;;;;     +-------+-------+-------+----------+-----+
;;;;     |  ver  | key-id| nonce |  cipher  | tag |
;;;;     | 1 B   | 4 B   | 12 B  |  N B     |16 B |
;;;;     +-------+-------+-------+----------+-----+
;;;;
;;;; The version byte gates future format changes. The key-id selects
;;;; which STEK to use. The nonce is freshly random per seal. The
;;;; ciphertext+tag is AES-256-GCM(plaintext) with the version+key-id+nonce
;;;; as AAD so a tampered header fails authentication.
;;;;
;;;; Open returns NIL on any failure (unknown key-id, bad tag, truncation,
;;;; bad version). The caller treats NIL as "fall back to a full handshake."

(defpackage epsilon.crypto.tls-session-ticket-store
  (:use :cl)
  (:import
   (epsilon.crypto.aes-gcm aes-gcm)
   (epsilon.crypto.drbg drbg))
  (:export
   #:stek-store
   #:make-stek-store
   #:stek-store-current
   #:stek-store-previous
   #:stek-rotate
   #:stek-seal
   #:stek-open))

(in-package :epsilon.crypto.tls-session-ticket-store)

(defconstant +ticket-format-version+ 1)
(defconstant +stek-key-bytes+ 32)        ; AES-256
(defconstant +stek-nonce-bytes+ 12)
(defconstant +stek-tag-bytes+ 16)
(defconstant +stek-key-id-bytes+ 4)
(defconstant +stek-header-bytes+ (+ 1 +stek-key-id-bytes+ +stek-nonce-bytes+))

(defstruct stek
  "A single session ticket encryption key."
  (id  nil :type (simple-array (unsigned-byte 8) (*)))
  (key nil :type (simple-array (unsigned-byte 8) (*))))

(defstruct (stek-store (:constructor %make-stek-store-raw))
  "A two-key STEK ring: a current key for sealing, and an optional
   previous key still accepted for opens until the next rotation."
  (current  nil :type (or null stek))
  (previous nil :type (or null stek)))

(defun %generate-stek ()
  (make-stek :id  (drbg:random-bytes +stek-key-id-bytes+)
             :key (drbg:random-bytes +stek-key-bytes+)))

(defun make-stek-store ()
  "Create a STEK store pre-populated with a freshly generated current key."
  (%make-stek-store-raw :current (%generate-stek) :previous nil))

(defun stek-rotate (store)
  "Rotate keys: the old current becomes previous; a fresh key becomes current.
   Any prior previous key is discarded."
  (setf (stek-store-previous store) (stek-store-current store))
  (setf (stek-store-current store) (%generate-stek))
  store)

(defun %lookup-key (store id)
  "Return the STEK with matching ID, or NIL."
  (let ((cur (stek-store-current store))
        (prev (stek-store-previous store)))
    (cond
      ((and cur (equalp (stek-id cur) id)) cur)
      ((and prev (equalp (stek-id prev) id)) prev)
      (t nil))))

(defun stek-seal (store plaintext)
  "Encrypt PLAINTEXT under the store's current key. Returns a sealed
   byte vector suitable for use as the opaque ticket field of a
   NewSessionTicket message."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext))
  (let* ((stek (or (stek-store-current store)
                   (error "STEK store has no current key")))
         (nonce (drbg:random-bytes +stek-nonce-bytes+))
         (header (make-array +stek-header-bytes+ :element-type '(unsigned-byte 8))))
    (setf (aref header 0) +ticket-format-version+)
    (replace header (stek-id stek) :start1 1)
    (replace header nonce :start1 (+ 1 +stek-key-id-bytes+))
    (multiple-value-bind (ct tag)
        (aes-gcm:aes-gcm-encrypt plaintext (stek-key stek) nonce :aad header)
      (let* ((out-len (+ +stek-header-bytes+ (length ct) +stek-tag-bytes+))
             (out (make-array out-len :element-type '(unsigned-byte 8))))
        (replace out header)
        (replace out ct :start1 +stek-header-bytes+)
        (replace out tag :start1 (+ +stek-header-bytes+ (length ct)))
        out))))

(defun stek-open (store sealed)
  "Decrypt and authenticate a sealed ticket. Returns the original
   plaintext, or NIL on any failure (unknown key-id, bad tag,
   truncation, version mismatch)."
  (declare (type (simple-array (unsigned-byte 8) (*)) sealed))
  (when (< (length sealed) (+ +stek-header-bytes+ +stek-tag-bytes+))
    (return-from stek-open nil))
  (unless (= (aref sealed 0) +ticket-format-version+)
    (return-from stek-open nil))
  (let* ((id (subseq sealed 1 (+ 1 +stek-key-id-bytes+)))
         (nonce (subseq sealed (+ 1 +stek-key-id-bytes+) +stek-header-bytes+))
         (header (subseq sealed 0 +stek-header-bytes+))
         (stek (%lookup-key store id)))
    (unless stek
      (return-from stek-open nil))
    (let* ((ct-end (- (length sealed) +stek-tag-bytes+))
           (ct (subseq sealed +stek-header-bytes+ ct-end))
           (tag (subseq sealed ct-end)))
      (handler-case
          (aes-gcm:aes-gcm-decrypt ct (stek-key stek) nonce tag :aad header)
        (error () nil)))))
