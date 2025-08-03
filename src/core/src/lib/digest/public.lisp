(defpackage #:epsilon.digest
  (:use
   #:cl)
  (:local-nicknames
   (:generic :epsilon.digest.generic)
   (:sha-2 :epsilon.digest.sha-2))
  (:export
   #:make-digest
   #:digest-stream
   #:digest-vector
   #:get-digest
   #:sha1-digest
   #:sha256))

(in-package #:epsilon.digest)

(defun make-digest (name)
  (ecase name
    (:sha-256 (sha-2:make-sha256-digest))))

(defun digest-stream (digest stream)
  (generic:update-digest-from-stream digest stream))

(defun digest-vector (digest vector)
  (generic:update-digest-from-vector digest vector 0 (length vector)))

(defun get-digest (digest)
  (generic:produce-digest digest))

(defun sha1-digest (octets)
  "Compute SHA-1 digest of octets. Stub implementation for WebSocket support."
  ;; SHA-1 produces a 160-bit (20 byte) hash
  ;; This is a stub that returns a fixed 20-byte array for now
  ;; In production, this should use a proper SHA-1 implementation
  (let ((result (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Simple stub: just XOR the input bytes and repeat the pattern
    (when (> (length octets) 0)
      (loop for i from 0 below 20
            do (setf (aref result i) 
                     (mod (loop for j from i below (length octets) by 20
                                sum (aref octets j))
                          256))))
    result))

(defun sha256 (octets)
  "Compute SHA-256 digest of octets"
  (let ((digest (make-digest :sha-256)))
    (digest-vector digest octets)
    (get-digest digest)))
