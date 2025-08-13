;;;; Main epsilon.cryptography Module
;;;;
;;;; This file serves as the main entry point, loading all sub-packages
;;;; in dependency order

(in-package :epsilon.cryptography)

;; The package definitions and exports are already in package.lisp
;; The type definitions are in types.lisp
;; The FFI bindings are in ffi.lisp
;; The TLS implementation is in tls.lisp
;; The crypto operations are in crypto.lisp
;; The certificate operations are in certificate.lisp

;; Re-export key functions from sub-packages if needed
;; Most exports are already defined in package.lisp

;; Module initialization
(defun initialize-cryptography ()
  "Initialize the cryptography module (OpenSSL libraries)"
  ;; OpenSSL is typically auto-initialized in modern versions
  ;; Add any necessary initialization here
  t)

;; Module cleanup
(defun cleanup-cryptography ()
  "Cleanup cryptography module resources"
  ;; Add any necessary cleanup here
  t)

;; Initialize on load
(initialize-cryptography)