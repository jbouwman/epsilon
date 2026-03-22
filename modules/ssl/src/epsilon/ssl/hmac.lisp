;;;; HMAC (RFC 2104)
;;;;
;;;; Hash-based Message Authentication Code.
;;;; Supports SHA-256, SHA-384, and SHA-512.

(defpackage epsilon.ssl.hmac
  (:use :cl)
  (:local-nicknames
   (#:sha256 #:epsilon.ssl.sha256)
   (#:sha512 #:epsilon.ssl.sha512)
   (#:ct #:epsilon.ssl.ct))
  (:export
   #:hmac-sha256
   #:hmac-sha256-verify
   #:hmac-sha384
   #:hmac-sha512
   #:hmac
   #:+hmac-sha256-output-size+
   #:+hmac-sha384-output-size+
   #:+hmac-sha512-output-size+))

(in-package :epsilon.ssl.hmac)

(defconstant +hmac-sha256-output-size+ 32)

(defun hmac-sha256 (key message &key (key-start 0) (key-end nil)
                                     (msg-start 0) (msg-end nil))
  "Compute HMAC-SHA256(key, message) per RFC 2104.
   Returns a 32-byte authentication tag."
  (declare (type (simple-array (unsigned-byte 8) (*)) key message))
  (let* ((key-end (or key-end (length key)))
         (msg-end (or msg-end (length message)))
         (block-size sha256:+sha256-block-size+)
         ;; Step 1: If key is longer than block size, hash it
         (actual-key (if (> (- key-end key-start) block-size)
                         (sha256:sha256 key :start key-start :end key-end)
                         (subseq key key-start key-end))))
    (ct:with-secure-buffers ((ipad block-size)
                             (opad block-size))
      ;; Step 2: Create inner and outer padded keys
      ;; Zero-pad the key to block size (buffers start at 0)
      (replace ipad actual-key)
      (replace opad actual-key)
      ;; XOR with ipad (0x36) and opad (0x5c)
      (loop for i from 0 below block-size
            do (setf (aref ipad i) (logxor (aref ipad i) #x36))
               (setf (aref opad i) (logxor (aref opad i) #x5c)))
      ;; Step 3: inner hash = SHA256(ipad || message)
      (let ((inner-state (sha256:make-sha256-state)))
        (sha256:sha256-update inner-state ipad)
        (sha256:sha256-update inner-state message :start msg-start :end msg-end)
        (let ((inner-hash (sha256:sha256-finalize inner-state)))
          ;; Step 4: outer hash = SHA256(opad || inner_hash)
          (let ((outer-state (sha256:make-sha256-state)))
            (sha256:sha256-update outer-state opad)
            (sha256:sha256-update outer-state inner-hash)
            (sha256:sha256-finalize outer-state)))))))

(defun hmac-sha256-verify (key message expected-tag
                           &key (key-start 0) (key-end nil)
                                (msg-start 0) (msg-end nil))
  "Verify HMAC-SHA256 in constant time. Returns T if valid."
  (declare (type (simple-array (unsigned-byte 8) (*)) key message expected-tag))
  (let ((computed (hmac-sha256 key message
                               :key-start key-start :key-end key-end
                               :msg-start msg-start :msg-end msg-end)))
    (ct:ct-equal computed expected-tag)))

;;; ---------------------------------------------------------------------------
;;; HMAC-SHA384
;;; ---------------------------------------------------------------------------

(defconstant +hmac-sha384-output-size+ 48)

(defun hmac-sha384 (key message &key (key-start 0) (key-end nil)
                                     (msg-start 0) (msg-end nil))
  "Compute HMAC-SHA384(key, message) per RFC 2104. Returns 48-byte tag."
  (declare (type (simple-array (unsigned-byte 8) (*)) key message))
  (let* ((key-end (or key-end (length key)))
         (msg-end (or msg-end (length message)))
         (block-size sha512:+sha384-block-size+)
         (actual-key (if (> (- key-end key-start) block-size)
                         (sha512:sha384 key :start key-start :end key-end)
                         (subseq key key-start key-end))))
    (ct:with-secure-buffers ((ipad block-size)
                             (opad block-size))
      (replace ipad actual-key)
      (replace opad actual-key)
      (loop for i from 0 below block-size
            do (setf (aref ipad i) (logxor (aref ipad i) #x36))
               (setf (aref opad i) (logxor (aref opad i) #x5c)))
      (let ((inner-state (sha512:make-sha384-state)))
        (sha512:sha384-update inner-state ipad)
        (sha512:sha384-update inner-state message :start msg-start :end msg-end)
        (let ((inner-hash (sha512:sha384-finalize inner-state)))
          (let ((outer-state (sha512:make-sha384-state)))
            (sha512:sha384-update outer-state opad)
            (sha512:sha384-update outer-state inner-hash)
            (sha512:sha384-finalize outer-state)))))))

;;; ---------------------------------------------------------------------------
;;; HMAC-SHA512
;;; ---------------------------------------------------------------------------

(defconstant +hmac-sha512-output-size+ 64)

(defun hmac-sha512 (key message &key (key-start 0) (key-end nil)
                                     (msg-start 0) (msg-end nil))
  "Compute HMAC-SHA512(key, message) per RFC 2104. Returns 64-byte tag."
  (declare (type (simple-array (unsigned-byte 8) (*)) key message))
  (let* ((key-end (or key-end (length key)))
         (msg-end (or msg-end (length message)))
         (block-size sha512:+sha512-block-size+)
         (actual-key (if (> (- key-end key-start) block-size)
                         (sha512:sha512 key :start key-start :end key-end)
                         (subseq key key-start key-end))))
    (ct:with-secure-buffers ((ipad block-size)
                             (opad block-size))
      (replace ipad actual-key)
      (replace opad actual-key)
      (loop for i from 0 below block-size
            do (setf (aref ipad i) (logxor (aref ipad i) #x36))
               (setf (aref opad i) (logxor (aref opad i) #x5c)))
      (let ((inner-state (sha512:make-sha512-state)))
        (sha512:sha512-update inner-state ipad)
        (sha512:sha512-update inner-state message :start msg-start :end msg-end)
        (let ((inner-hash (sha512:sha512-finalize inner-state)))
          (let ((outer-state (sha512:make-sha512-state)))
            (sha512:sha512-update outer-state opad)
            (sha512:sha512-update outer-state inner-hash)
            (sha512:sha512-finalize outer-state)))))))

;;; ---------------------------------------------------------------------------
;;; Generic HMAC dispatch
;;; ---------------------------------------------------------------------------

(defun hmac (hash-algorithm key message &key (key-start 0) (key-end nil)
                                             (msg-start 0) (msg-end nil))
  "Compute HMAC with the specified hash algorithm.
   HASH-ALGORITHM is one of :sha256, :sha384, :sha512."
  (ecase hash-algorithm
    (:sha256 (hmac-sha256 key message :key-start key-start :key-end key-end
                                      :msg-start msg-start :msg-end msg-end))
    (:sha384 (hmac-sha384 key message :key-start key-start :key-end key-end
                                      :msg-start msg-start :msg-end msg-end))
    (:sha512 (hmac-sha512 key message :key-start key-start :key-end key-end
                                      :msg-start msg-start :msg-end msg-end))))
