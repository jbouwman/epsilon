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
   #:get-digest))

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
