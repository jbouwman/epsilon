(defpackage #:epsilon.lib.digest
  (:use
   #:cl)
  (:export
   #:make-digest
   #:digest-stream
   #:digest-vector
   #:get-digest))

(in-package #:epsilon.lib.digest)

(defun make-digest (name)
  (ecase name
    (:sha-256 (epsilon.lib.digest.sha-2::%make-sha256-digest))))

(defun digest-stream (digest stream)
  (epsilon.lib.digest.generic:update-digest-from-stream digest stream))

(defun digest-vector (digest vector)
  (epsilon.lib.digest.generic:update-digest-from-vector digest vector 0 (length vector)))

(defun get-digest (digest)
  (epsilon.lib.digest.generic:produce-digest digest))
