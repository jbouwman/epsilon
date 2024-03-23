(defpackage #:lib.digest
  (:use
   #:cl)
  (:export
   #:make-digest
   #:digest-stream
   #:get-digest))

(in-package #:lib.digest)

(defun make-digest (name)
  (ecase name
    (:sha-256 (lib.digest.sha-2::%make-sha256-digest))))

(defun digest-stream (digest stream)
  (lib.digest.generic:update-digest-from-stream digest stream))

(defun get-digest (digest)
  (lib.digest.generic:produce-digest digest))
