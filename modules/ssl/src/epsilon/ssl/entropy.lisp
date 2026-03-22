;;;; Platform entropy sources
;;;;
;;;; Provides access to OS-level cryptographic random number generators.
;;;; This is the ONLY FFI used in the entire native crypto library.
;;;;
;;;; Linux:   getrandom(2) - reads from /dev/urandom kernel pool
;;;; macOS:   getentropy(2) - reads from kernel CSPRNG
;;;; Windows: BCryptGenRandom - reads from CNG provider

(defpackage epsilon.ssl.entropy
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.foreign))
  (:export
   #:fill-random-bytes
   #:random-bytes
   #:entropy-available-p))

(in-package :epsilon.ssl.entropy)

;;; ---------------------------------------------------------------------------
;;; Platform detection
;;; ---------------------------------------------------------------------------

(defun os-type ()
  "Return the current operating system as a keyword."
  (cond
    ((member :linux *features*) :linux)
    ((member :darwin *features*) :darwin)
    ((member :win32 *features*) :windows)
    ((member :windows *features*) :windows)
    (t :unknown)))

;;; ---------------------------------------------------------------------------
;;; FFI bindings (the only FFI in the native crypto library)
;;; ---------------------------------------------------------------------------

;; Linux: ssize_t getrandom(void *buf, size_t buflen, unsigned int flags)
(ffi:defshared %getrandom "getrandom" "libc" :long
  (buf :pointer) (buflen :unsigned-long) (flags :unsigned-int))

;; macOS: int getentropy(void *buf, size_t length)
;; Note: getentropy is in libc on macOS (actually libsystem)
(ffi:defshared %getentropy "getentropy" "libc" :int
  (buf :pointer) (length :unsigned-long))

;;; ---------------------------------------------------------------------------
;;; Platform-specific implementations
;;; ---------------------------------------------------------------------------

(defun fill-random-bytes-linux (buf)
  "Fill BUF with random bytes using Linux getrandom(2).
   BUF must be a (simple-array (unsigned-byte 8) (*))."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf))
  (let ((len (length buf)))
    ;; getrandom with flags=0 blocks until entropy is available,
    ;; reads from /dev/urandom pool (safe for crypto after boot)
    (sb-sys:with-pinned-objects (buf)
      (let ((ret (%getrandom (sb-sys:vector-sap buf) len 0)))
        (unless (= ret len)
          (error "getrandom failed: requested ~D bytes, got ~D" len ret)))))
  buf)

(defun fill-random-bytes-darwin (buf)
  "Fill BUF with random bytes using macOS getentropy(2).
   BUF must be a (simple-array (unsigned-byte 8) (*)).
   getentropy is limited to 256 bytes per call."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf))
  (let ((len (length buf)))
    (sb-sys:with-pinned-objects (buf)
      (let ((sap (sb-sys:vector-sap buf))
            (offset 0))
        (loop while (< offset len)
              do (let* ((chunk (min 256 (- len offset)))
                        (ptr (sb-sys:sap+ sap offset))
                        (ret (%getentropy ptr chunk)))
                   (unless (zerop ret)
                     (error "getentropy failed with return code ~D" ret))
                   (incf offset chunk))))))
  buf)

;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(defun entropy-available-p ()
  "Return T if a platform entropy source is available."
  (member (os-type) '(:linux :darwin)))

(defun fill-random-bytes (buf)
  "Fill byte array BUF with cryptographically secure random bytes.
   Returns BUF."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf))
  (when (zerop (length buf))
    (return-from fill-random-bytes buf))
  (ecase (os-type)
    (:linux (fill-random-bytes-linux buf))
    (:darwin (fill-random-bytes-darwin buf)))
  buf)

(defun random-bytes (n)
  "Return a fresh byte array of N cryptographically secure random bytes."
  (declare (type (integer 0) n))
  (let ((buf (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
    (fill-random-bytes buf)
    buf))
