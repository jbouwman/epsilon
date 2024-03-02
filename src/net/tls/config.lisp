(defpackage #:net.tls/config
  (:documentation "By default net.tls searches for OpenSSL shared libraries
in platform-dependent default locations.

To explicitly specify what to load, use the net.tls/config
module before loading net.tls:

    (ql:quickload \"net.tls/config\")
    (net.tls/config:define-libssl-path \"/opt/local/lib/libssl.dylib\")
    (net.tls/config:define-libcrypto-path \"/opt/local/lib/libcrypto.dylib\")
    (ql:quickload \"net.tls\")

The PATH parameter of those two macros is not evaluated.
This is dictated by CFFI. So either use a literal
or compute it at the macro-expansion time.
")
  (:export #:define-libssl-path
           #:define-libcrypto-path)
  (:use
   #:cl)
  (:local-nicknames
   (#:ffi #:sys.ffi)))

(in-package #:net.tls/config)

(defvar *libssl-override* nil)

(defvar *libcrypto-override* nil)

(defmacro define-libssl-path (path)
  "Define the path where libssl resides to be PATH (not evaluated). This
macro should be used before loading net.tls.
"
  `(progn
     (ffi:define-foreign-library libssl (t ,path))
     (setq *libssl-override* t)))

(defmacro define-libcrypto-path (path)
  "Define the path where libcrypto resides to be PATH (not evaluated). This
macro should be used before loading net.tls.
"
  `(progn
     (ffi:define-foreign-library libcrypto (t ,path))
     (setq *libcrypto-override* t)))
