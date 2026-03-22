;;;; Poly1305 (RFC 8439)
;;;;
;;;; Pure-Lisp implementation of the Poly1305 message authentication code.
;;;; Uses 130-bit arithmetic via Common Lisp bignums.

(defpackage epsilon.ssl.poly1305
  (:use :cl)
  (:export
   #:poly1305
   #:poly1305-verify
   #:make-poly1305-state
   #:poly1305-update
   #:poly1305-finalize))

(in-package :epsilon.ssl.poly1305)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

;; The prime 2^130 - 5
(defconstant +p+ (- (ash 1 130) 5))

;;; ---------------------------------------------------------------------------
;;; Key clamping (RFC 8439 Section 2.5)
;;; ---------------------------------------------------------------------------

(defun clamp-r (r-bytes)
  "Clamp the 'r' portion of the key per RFC 8439.
   R-BYTES is a 16-byte array. Returns a clamped integer."
  (declare (type (simple-array (unsigned-byte 8) (*)) r-bytes))
  ;; Read 16 bytes as little-endian 128-bit integer
  (let ((r 0))
    (loop for i from 0 below 16
          do (setf r (logior r (ash (aref r-bytes i) (* i 8)))))
    ;; Clamp: clear bits 4,5,6,7 of bytes 3,7,11,15 and top bit of bytes 4,8,12
    (logand r #x0ffffffc0ffffffc0ffffffc0fffffff)))

(defun le-bytes-to-int (bytes start count)
  "Read COUNT little-endian bytes from BYTES at START as an integer."
  (let ((val 0))
    (loop for i from 0 below count
          do (setf val (logior val (ash (aref bytes (+ start i)) (* i 8)))))
    val))

(defun int-to-le-bytes (n count)
  "Convert integer N to COUNT little-endian bytes."
  (let ((result (make-array count :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from 0 below count
          do (setf (aref result i) (logand #xFF (ash n (- (* i 8))))))
    result))

;;; ---------------------------------------------------------------------------
;;; One-shot API
;;; ---------------------------------------------------------------------------

(defun poly1305 (key message &key (msg-start 0) (msg-end nil))
  "Compute Poly1305 MAC.
   KEY is a 32-byte array (first 16 bytes = r, last 16 bytes = s).
   MESSAGE is a byte array.
   Returns a 16-byte tag."
  (declare (type (simple-array (unsigned-byte 8) (*)) key message))
  (let* ((msg-end (or msg-end (length message)))
         ;; Split key into r (first 16 bytes) and s (last 16 bytes)
         (r (clamp-r key))
         (s (le-bytes-to-int key 16 16))
         ;; Accumulator
         (acc 0)
         ;; Process message in 16-byte blocks
         (pos msg-start))
    (loop while (< pos msg-end)
          do (let* ((block-size (min 16 (- msg-end pos)))
                    ;; Read block as little-endian integer
                    (n (le-bytes-to-int message pos block-size)))
               ;; Add the high bit (2^(8*block-size)) -- the "one" padding
               (setf n (logior n (ash 1 (* 8 block-size))))
               ;; acc = ((acc + n) * r) mod p
               (setf acc (mod (* (+ acc n) r) +p+))
               (incf pos block-size)))
    ;; Final: tag = (acc + s) mod 2^128
    (let ((tag (logand (1- (ash 1 128)) (+ acc s))))
      (int-to-le-bytes tag 16))))

;;; ---------------------------------------------------------------------------
;;; Incremental API
;;; ---------------------------------------------------------------------------

(defstruct (poly1305-state (:constructor %make-poly1305-state))
  (r 0 :type integer)
  (s 0 :type integer)
  (acc 0 :type integer)
  (buffer (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (16)))
  (buffer-count 0 :type fixnum))

(defun make-poly1305-state (key)
  "Create a Poly1305 state from a 32-byte key."
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let ((state (%make-poly1305-state)))
    (setf (poly1305-state-r state) (clamp-r key))
    (setf (poly1305-state-s state) (le-bytes-to-int key 16 16))
    state))

(defun poly1305-process-block (state block-bytes is-full)
  "Process a single block."
  (let* ((r (poly1305-state-r state))
         (n (le-bytes-to-int (poly1305-state-buffer state) 0 block-bytes)))
    (when is-full
      (setf n (logior n (ash 1 128))))
    (when (not is-full)
      (setf n (logior n (ash 1 (* 8 block-bytes)))))
    (setf (poly1305-state-acc state)
          (mod (* (+ (poly1305-state-acc state) n) r) +p+))))

(defun poly1305-update (state data &key (start 0) (end nil))
  "Feed data into Poly1305."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let* ((end (or end (length data)))
         (buf (poly1305-state-buffer state))
         (buf-count (poly1305-state-buffer-count state))
         (pos start))
    ;; Fill buffer if partially full
    (when (> buf-count 0)
      (let ((copy-len (min (- 16 buf-count) (- end pos))))
        (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                          :start2 pos :end2 (+ pos copy-len))
        (incf pos copy-len)
        (incf buf-count copy-len)
        (when (= buf-count 16)
          (poly1305-process-block state 16 t)
          (setf buf-count 0))))
    ;; Process full blocks
    (loop while (<= (+ pos 16) end)
          do (replace buf data :start1 0 :end1 16
                               :start2 pos :end2 (+ pos 16))
             (poly1305-process-block state 16 t)
             (incf pos 16))
    ;; Buffer remaining
    (when (< pos end)
      (let ((remaining (- end pos)))
        (replace buf data :start1 0 :end1 remaining
                          :start2 pos :end2 end)
        (setf buf-count remaining)))
    (setf (poly1305-state-buffer-count state) buf-count))
  state)

(defun poly1305-finalize (state)
  "Finalize and return 16-byte tag."
  (let ((buf-count (poly1305-state-buffer-count state)))
    ;; Process any remaining partial block
    (when (> buf-count 0)
      ;; Zero the rest of the buffer
      (loop for i from buf-count below 16
            do (setf (aref (poly1305-state-buffer state) i) 0))
      (poly1305-process-block state buf-count nil)))
  ;; Final: tag = (acc + s) mod 2^128
  (let ((tag (logand (1- (ash 1 128))
                     (+ (poly1305-state-acc state)
                        (poly1305-state-s state)))))
    (int-to-le-bytes tag 16)))

(defun poly1305-verify (key message expected-tag
                        &key (msg-start 0) (msg-end nil))
  "Verify a Poly1305 tag in constant time. Returns T if valid."
  (declare (type (simple-array (unsigned-byte 8) (*)) key message expected-tag))
  (let ((computed (poly1305 key message :msg-start msg-start :msg-end msg-end))
        (match t))
    ;; Constant-time comparison
    (let ((diff 0))
      (loop for i from 0 below 16
            do (setf diff (logior diff (logxor (aref computed i) (aref expected-tag i)))))
      (setf match (= diff 0)))
    match))
