;;;; epsilon-module
;;;; :requires (epsilon.foreign epsilon.library epsilon.foreign.binding-ir)
;;;; :provides (epsilon.compression.zstd.ffi)
;;;;
;;;; FFI Bindings for Zstandard (zstd) compression library
;;;;
;;;; Zstd provides fast compression with excellent compression ratios.
;;;; It supports both one-shot and streaming APIs.

(defpackage :epsilon.compression.zstd.ffi
  (:use :cl)
  (:import
   (epsilon.foreign ffi)
   (epsilon.library lib)
   (epsilon.foreign.binding-ir bir))
  (:export
   ;; Library definition
   #:ensure-zstd-loaded
   #:zstd-available-p

   ;; Constants
   #:+zstd-clevel-default+
   #:+zstd-clevel-min+
   #:+zstd-clevel-max+

   ;; Error checking
   #:%is-error
   #:%get-error-name
   #:%get-error-code

   ;; One-shot functions
   #:%compress
   #:%decompress
   #:%compress-bound
   #:%get-frame-content-size
   #:%find-decompressed-size

   ;; Streaming compression
   #:%cstream-create
   #:%cstream-free
   #:%cstream-init
   #:%cstream-compress
   #:%cstream-end
   #:%cstream-flush
   #:%cstream-reset

   ;; Streaming decompression
   #:%dstream-create
   #:%dstream-free
   #:%dstream-init
   #:%dstream-decompress
   #:%dstream-reset

   ;; Buffer size recommendations
   #:%cstream-in-size
   #:%cstream-out-size
   #:%dstream-in-size
   #:%dstream-out-size

   ;; Version
   #:%version-number
   #:%version-string

   ;; Buffer structure management
   #:make-zstd-buffer
   #:free-zstd-buffer
   #:with-zstd-buffer
   #:zstd-buffer-ptr
   #:zstd-buffer-size
   #:zstd-buffer-pos
   #:set-zstd-buffer-ptr
   #:set-zstd-buffer-size
   #:set-zstd-buffer-pos))

;;;; Library Definition

(lib:define-library :zstd
  :base-name "zstd"
  :version "1"
  :bundled-p nil
  :critical-p nil
  :search-names (list #+darwin "/opt/homebrew/lib/libzstd.dylib"
                      #+darwin "/opt/homebrew/lib/libzstd.1.dylib"
                      #+darwin "/usr/local/lib/libzstd.dylib"
                      #+darwin "/usr/local/lib/libzstd.1.dylib"
                      #+darwin "libzstd.dylib"
                      #+linux "/usr/lib/x86_64-linux-gnu/libzstd.so.1"
                      #+linux "/usr/lib/x86_64-linux-gnu/libzstd.so"
                      #+linux "/lib/x86_64-linux-gnu/libzstd.so.1"
                      #+linux "/lib/x86_64-linux-gnu/libzstd.so"
                      #+linux "libzstd.so.1" #+linux "libzstd.so"
                      #+windows "zstd.dll" #+windows "libzstd.dll")
  :description "Zstandard compression library")

(defvar *zstd-loaded* nil
  "Flag indicating whether zstd has been loaded")

(defun ensure-zstd-loaded ()
  "Ensure zstd library is loaded"
  (unless *zstd-loaded*
    (lib:lib-open :zstd)
    (setf *zstd-loaded* t)))

(defun zstd-available-p ()
  "Check if zstd library is available"
  (handler-case
      (progn (ensure-zstd-loaded) t)
    (error () nil)))

;;;; Constants
;;;;
;;;; Some constants loaded from BIR, others computed manually.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *zstd-bir-path*
    (epsilon.loader:module-resource "epsilon.compression" "bindings/zstd.bir.lisp")))

;; Compression level default from BIR
(bir:define-constants-from-bir *zstd-bir-path*
  :names ("ZSTD_CLEVEL_DEFAULT"))

;; Min/max levels are runtime functions in zstd, use typical values
(defconstant +zstd-clevel-min+ 1 "Minimum compression level")
(defconstant +zstd-clevel-max+ 22 "Maximum compression level")

;; Content size constants - need unsigned 64-bit representation
;; BIR has -1/-2 but we need the unsigned equivalents for comparison
(defconstant +zstd-contentsize-unknown+ (1- (ash 1 64))
  "Content size unknown (ZSTD_CONTENTSIZE_UNKNOWN)")
(defconstant +zstd-contentsize-error+ (- (ash 1 64) 2)
  "Error determining content size (ZSTD_CONTENTSIZE_ERROR)")

;;;; ZSTD_inBuffer and ZSTD_outBuffer structures
;;;;
;;;; typedef struct {
;;;;     const void* src;    // offset 0, 8 bytes (pointer)
;;;;     size_t      size;   // offset 8, 8 bytes
;;;;     size_t      pos;    // offset 16, 8 bytes
;;;; } ZSTD_inBuffer;       // total: 24 bytes
;;;;
;;;; typedef struct {
;;;;     void*  dst;         // offset 0, 8 bytes (pointer)
;;;;     size_t size;        // offset 8, 8 bytes
;;;;     size_t pos;         // offset 16, 8 bytes
;;;; } ZSTD_outBuffer;      // total: 24 bytes

(defconstant +zstd-buffer-size+ 24 "Size of ZSTD buffer structures")
(defconstant +zstd-buffer-ptr-offset+ 0)
(defconstant +zstd-buffer-size-offset+ 8)
(defconstant +zstd-buffer-pos-offset+ 16)

(defun make-zstd-buffer ()
  "Allocate a ZSTD_inBuffer or ZSTD_outBuffer structure"
  (ffi:foreign-alloc +zstd-buffer-size+ :initial-element 0))

(defun free-zstd-buffer (buffer)
  "Free a ZSTD buffer structure"
  (ffi:foreign-free buffer))

(defmacro with-zstd-buffer ((var) &body body)
  "Execute body with a fresh ZSTD buffer structure"
  `(let ((,var (make-zstd-buffer)))
     (unwind-protect
          (progn ,@body)
       (free-zstd-buffer ,var))))

;; Buffer accessors
(defun zstd-buffer-ptr (buffer)
  "Get pointer from ZSTD buffer"
  (sb-sys:sap-ref-sap buffer +zstd-buffer-ptr-offset+))

(defun zstd-buffer-size (buffer)
  "Get size from ZSTD buffer"
  (sb-sys:sap-ref-64 buffer +zstd-buffer-size-offset+))

(defun zstd-buffer-pos (buffer)
  "Get position from ZSTD buffer"
  (sb-sys:sap-ref-64 buffer +zstd-buffer-pos-offset+))

;; Buffer setters
(defun set-zstd-buffer-ptr (buffer ptr)
  "Set pointer in ZSTD buffer"
  (setf (sb-sys:sap-ref-sap buffer +zstd-buffer-ptr-offset+) ptr))

(defun set-zstd-buffer-size (buffer size)
  "Set size in ZSTD buffer"
  (setf (sb-sys:sap-ref-64 buffer +zstd-buffer-size-offset+) size))

(defun set-zstd-buffer-pos (buffer pos)
  "Set position in ZSTD buffer"
  (setf (sb-sys:sap-ref-64 buffer +zstd-buffer-pos-offset+) pos))

;;;; Error Checking Functions

(ffi:defshared %is-error "ZSTD_isError" :zstd :unsigned-int
  (code :unsigned-long)
  :documentation "Check if return code is an error")

(ffi:defshared %get-error-name "ZSTD_getErrorName" :zstd :string
  (code :unsigned-long)
  :documentation "Get error name string")

(ffi:defshared %get-error-code-internal "ZSTD_getErrorCode" :zstd :int
  (code :unsigned-long)
  :documentation "Get error code from result")

(defun %get-error-code (result)
  "Get error code from zstd result"
  (%get-error-code-internal result))

;;;; One-shot Compression Functions

(ffi:defshared %compress "ZSTD_compress" :zstd :unsigned-long
  (dst :pointer) (dst-capacity :unsigned-long)
  (src :pointer) (src-size :unsigned-long)
  (compression-level :int)
  :documentation "Compress src into dst with given compression level")

(ffi:defshared %decompress "ZSTD_decompress" :zstd :unsigned-long
  (dst :pointer) (dst-capacity :unsigned-long)
  (src :pointer) (src-size :unsigned-long)
  :documentation "Decompress src into dst")

(ffi:defshared %compress-bound "ZSTD_compressBound" :zstd :unsigned-long
  (src-size :unsigned-long)
  :documentation "Get maximum compressed size for source size")

(ffi:defshared %get-frame-content-size "ZSTD_getFrameContentSize" :zstd :unsigned-long
  (src :pointer) (src-size :unsigned-long)
  :documentation "Get decompressed size from frame header (if known)")

(ffi:defshared %find-decompressed-size "ZSTD_findDecompressedSize" :zstd :unsigned-long
  (src :pointer) (src-size :unsigned-long)
  :documentation "Find decompressed size by reading all frame headers")

;;;; Streaming Compression Functions

(ffi:defshared %cstream-create "ZSTD_createCStream" :zstd :pointer ()
  :documentation "Create compression stream context")

(ffi:defshared %cstream-free "ZSTD_freeCStream" :zstd :unsigned-long
  (cstream :pointer)
  :documentation "Free compression stream context")

(ffi:defshared %cstream-init "ZSTD_initCStream" :zstd :unsigned-long
  (cstream :pointer) (compression-level :int)
  :documentation "Initialize compression stream with compression level")

(ffi:defshared %cstream-compress "ZSTD_compressStream" :zstd :unsigned-long
  (cstream :pointer) (output :pointer) (input :pointer)
  :documentation "Compress streaming data")

(ffi:defshared %cstream-compress2 "ZSTD_compressStream2" :zstd :unsigned-long
  (cstream :pointer) (output :pointer) (input :pointer) (end-op :int)
  :documentation "Compress streaming data with end directive")

(ffi:defshared %cstream-end "ZSTD_endStream" :zstd :unsigned-long
  (cstream :pointer) (output :pointer)
  :documentation "End compression stream")

(ffi:defshared %cstream-flush "ZSTD_flushStream" :zstd :unsigned-long
  (cstream :pointer) (output :pointer)
  :documentation "Flush compression stream")

(ffi:defshared %cstream-reset "ZSTD_CCtx_reset" :zstd :unsigned-long
  (cctx :pointer) (reset :int)
  :documentation "Reset compression context")

;;;; Streaming Decompression Functions

(ffi:defshared %dstream-create "ZSTD_createDStream" :zstd :pointer ()
  :documentation "Create decompression stream context")

(ffi:defshared %dstream-free "ZSTD_freeDStream" :zstd :unsigned-long
  (dstream :pointer)
  :documentation "Free decompression stream context")

(ffi:defshared %dstream-init "ZSTD_initDStream" :zstd :unsigned-long
  (dstream :pointer)
  :documentation "Initialize decompression stream")

(ffi:defshared %dstream-decompress "ZSTD_decompressStream" :zstd :unsigned-long
  (dstream :pointer) (output :pointer) (input :pointer)
  :documentation "Decompress streaming data")

(ffi:defshared %dstream-reset "ZSTD_DCtx_reset" :zstd :unsigned-long
  (dctx :pointer) (reset :int)
  :documentation "Reset decompression context")

;;;; Buffer Size Recommendations

(ffi:defshared %cstream-in-size "ZSTD_CStreamInSize" :zstd :unsigned-long ()
  :documentation "Recommended input buffer size for compression")

(ffi:defshared %cstream-out-size "ZSTD_CStreamOutSize" :zstd :unsigned-long ()
  :documentation "Recommended output buffer size for compression")

(ffi:defshared %dstream-in-size "ZSTD_DStreamInSize" :zstd :unsigned-long ()
  :documentation "Recommended input buffer size for decompression")

(ffi:defshared %dstream-out-size "ZSTD_DStreamOutSize" :zstd :unsigned-long ()
  :documentation "Recommended output buffer size for decompression")

;;;; Version Functions

(ffi:defshared %version-number "ZSTD_versionNumber" :zstd :unsigned-int ()
  :documentation "Get zstd version as number (major*100*100 + minor*100 + patch)")

(ffi:defshared %version-string "ZSTD_versionString" :zstd :string ()
  :documentation "Get zstd version string")

;;;; Initialize on load (but don't fail if unavailable)
(eval-when (:load-toplevel :execute)
  (ignore-errors (ensure-zstd-loaded)))
