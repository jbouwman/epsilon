;;;; epsilon-module
;;;; :requires (epsilon.foreign epsilon.library epsilon.foreign.binding-ir)
;;;; :provides (epsilon.compression.ffi)
;;;;
;;;; FFI Bindings for zlib compression library
;;;;
;;;; This file contains all Foreign Function Interface bindings for zlib
;;;; compression operations including one-shot and streaming APIs.

(defpackage :epsilon.compression.ffi
  (:use :cl)
  (:import
   (epsilon.foreign ffi)
   (epsilon.library lib)
   (epsilon.foreign.binding-ir bir))
  (:export
   ;; Library definition
   #:ensure-zlib-loaded

   ;; Constants
   #:+z-ok+
   #:+z-stream-end+
   #:+z-need-dict+
   #:+z-errno+
   #:+z-stream-error+
   #:+z-data-error+
   #:+z-mem-error+
   #:+z-buf-error+
   #:+z-version-error+

   ;; Flush values
   #:+z-no-flush+
   #:+z-partial-flush+
   #:+z-sync-flush+
   #:+z-full-flush+
   #:+z-finish+
   #:+z-block+
   #:+z-trees+

   ;; Compression levels
   #:+z-no-compression+
   #:+z-best-speed+
   #:+z-best-compression+
   #:+z-default-compression+

   ;; Strategy values
   #:+z-filtered+
   #:+z-huffman-only+
   #:+z-rle+
   #:+z-fixed+
   #:+z-default-strategy+

   ;; Data types
   #:+z-binary+
   #:+z-text+
   #:+z-ascii+
   #:+z-unknown+

   ;; Window bits
   #:+max-wbits+
   #:+gzip-wbits+
   #:+raw-wbits+
   #:+auto-wbits+

   ;; One-shot functions
   #:%compress
   #:%compress2
   #:%compress-bound
   #:%uncompress
   #:%uncompress2

   ;; Streaming functions
   #:%deflate-init
   #:%deflate-init2
   #:%deflate
   #:%deflate-end
   #:%deflate-reset
   #:%deflate-set-dictionary

   #:%inflate-init
   #:%inflate-init2
   #:%inflate
   #:%inflate-end
   #:%inflate-reset
   #:%inflate-set-dictionary

   ;; Checksum functions
   #:%crc32
   #:%crc32-combine
   #:%adler32
   #:%adler32-combine

   ;; Utility functions
   #:%zlib-version
   #:%z-error

   ;; z_stream structure management
   #:make-z-stream
   #:free-z-stream
   #:with-z-stream
   #:z-stream-next-in
   #:z-stream-avail-in
   #:z-stream-total-in
   #:z-stream-next-out
   #:z-stream-avail-out
   #:z-stream-total-out
   #:z-stream-msg
   #:set-z-stream-next-in
   #:set-z-stream-avail-in
   #:set-z-stream-next-out
   #:set-z-stream-avail-out))

;;;; Library Definition

;; Define zlib library local to this module
(lib:define-library :zlib
  :base-name "z"
  :version "1"
  :bundled-p nil
  :critical-p nil
  :search-names (list #+darwin "/opt/homebrew/lib/libz.dylib"
                      #+darwin "/opt/homebrew/lib/libz.1.dylib"
                      #+darwin "/usr/lib/libz.1.dylib"
                      #+darwin "/usr/lib/libz.dylib"
                      #+darwin "libz.dylib"
                      #+linux "/usr/lib/x86_64-linux-gnu/libz.so.1"
                      #+linux "/usr/lib/x86_64-linux-gnu/libz.so"
                      #+linux "/lib/x86_64-linux-gnu/libz.so.1"
                      #+linux "/lib/x86_64-linux-gnu/libz.so"
                      #+linux "libz.so.1" #+linux "libz.so"
                      #+windows "zlib1.dll" #+windows "zlib.dll")
  :description "zlib compression library")

(defvar *zlib-loaded* nil
  "Flag indicating whether zlib has been loaded")

(defun ensure-zlib-loaded ()
  "Ensure zlib library is loaded"
  (unless *zlib-loaded*
    (lib:lib-open :zlib)
    (setf *zlib-loaded* t)))

;;;; Constants
;;;;
;;;; Z_* constants are loaded from the grovelled BIR file.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *zlib-bir-path*
    (epsilon.loader:module-resource "epsilon.compression" "bindings/zlib.bir.lisp")))

;; Return codes and flush values from zlib.h (generated from BIR)
(bir:define-constants-from-bir *zlib-bir-path*
  :names ("Z_OK" "Z_STREAM_END" "Z_NEED_DICT" "Z_ERRNO"
          "Z_STREAM_ERROR" "Z_DATA_ERROR" "Z_MEM_ERROR"
          "Z_BUF_ERROR" "Z_VERSION_ERROR"
          "Z_NO_FLUSH" "Z_PARTIAL_FLUSH" "Z_SYNC_FLUSH"
          "Z_FULL_FLUSH" "Z_FINISH" "Z_BLOCK" "Z_TREES"
          "Z_NO_COMPRESSION" "Z_BEST_SPEED" "Z_BEST_COMPRESSION"
          "Z_DEFAULT_COMPRESSION"
          "Z_FILTERED" "Z_HUFFMAN_ONLY" "Z_RLE" "Z_FIXED"
          "Z_DEFAULT_STRATEGY"
          "Z_BINARY" "Z_TEXT" "Z_ASCII" "Z_UNKNOWN"))

;; Window bits for different formats (derived constants, not in zlib.h as macros)
(defconstant +max-wbits+ 15 "Maximum window bits")
(defconstant +gzip-wbits+ (+ 15 16) "Window bits for gzip format (15 + 16)")
(defconstant +raw-wbits+ -15 "Window bits for raw deflate (negative)")
(defconstant +auto-wbits+ (+ 15 32) "Auto-detect format (15 + 32)")

;;;; z_stream Structure
;;;;
;;;; Field offsets are grovelled from zlib.h via BIR.
;;;; The struct name in zlib is "z_stream_s" (the typedef "z_stream" aliases it).

;; Size and field offsets grovelled from zlib.h
(bir:define-struct-offsets-from-bir
    *zlib-bir-path*
  "z_stream_s"
  :prefix "+z-stream-"
  :size-constant +z-stream-size+)

(defun make-z-stream ()
  "Allocate and zero-initialize a z_stream structure"
  (let ((stream (ffi:foreign-alloc +z-stream-size+ :initial-element 0)))
    stream))

(defun free-z-stream (stream)
  "Free a z_stream structure"
  (ffi:foreign-free stream))

(defmacro with-z-stream ((var) &body body)
  "Execute body with a fresh z_stream structure, cleaning up on exit"
  `(let ((,var (make-z-stream)))
     (unwind-protect
          (progn ,@body)
       (free-z-stream ,var))))

;; z_stream accessors
(defun z-stream-next-in (stream)
  "Get next_in pointer from z_stream"
  (sb-sys:sap-ref-sap stream +z-stream-next-in-offset+))

(defun z-stream-avail-in (stream)
  "Get avail_in from z_stream"
  (sb-sys:sap-ref-32 stream +z-stream-avail-in-offset+))

(defun z-stream-total-in (stream)
  "Get total_in from z_stream"
  (sb-sys:sap-ref-64 stream +z-stream-total-in-offset+))

(defun z-stream-next-out (stream)
  "Get next_out pointer from z_stream"
  (sb-sys:sap-ref-sap stream +z-stream-next-out-offset+))

(defun z-stream-avail-out (stream)
  "Get avail_out from z_stream"
  (sb-sys:sap-ref-32 stream +z-stream-avail-out-offset+))

(defun z-stream-total-out (stream)
  "Get total_out from z_stream"
  (sb-sys:sap-ref-64 stream +z-stream-total-out-offset+))

(defun z-stream-msg (stream)
  "Get msg pointer from z_stream"
  (sb-sys:sap-ref-sap stream +z-stream-msg-offset+))

;; z_stream setters
(defun set-z-stream-next-in (stream ptr)
  "Set next_in pointer in z_stream"
  (setf (sb-sys:sap-ref-sap stream +z-stream-next-in-offset+) ptr))

(defun set-z-stream-avail-in (stream value)
  "Set avail_in in z_stream"
  (setf (sb-sys:sap-ref-32 stream +z-stream-avail-in-offset+) value))

(defun set-z-stream-next-out (stream ptr)
  "Set next_out pointer in z_stream"
  (setf (sb-sys:sap-ref-sap stream +z-stream-next-out-offset+) ptr))

(defun set-z-stream-avail-out (stream value)
  "Set avail_out in z_stream"
  (setf (sb-sys:sap-ref-32 stream +z-stream-avail-out-offset+) value))

;;;; One-shot Compression FFI Functions

(ffi:defshared %compress "compress" :zlib :int
  (dest :pointer) (dest-len :pointer) (source :pointer) (source-len :unsigned-long)
  :documentation "Compress source to dest. dest-len is in/out parameter.")

(ffi:defshared %compress2 "compress2" :zlib :int
  (dest :pointer) (dest-len :pointer) (source :pointer) (source-len :unsigned-long)
  (level :int)
  :documentation "Compress with specified level (0-9)")

(ffi:defshared %compress-bound "compressBound" :zlib :unsigned-long
  (source-len :unsigned-long)
  :documentation "Get upper bound on compressed size")

(ffi:defshared %uncompress "uncompress" :zlib :int
  (dest :pointer) (dest-len :pointer) (source :pointer) (source-len :unsigned-long)
  :documentation "Decompress source to dest. dest-len is in/out parameter.")

(ffi:defshared %uncompress2 "uncompress2" :zlib :int
  (dest :pointer) (dest-len :pointer) (source :pointer) (source-len :pointer)
  :documentation "Decompress with source-len as in/out parameter")

;;;; Streaming Compression FFI Functions

;; deflateInit is actually a macro that calls deflateInit_
;; deflateInit_(stream, level, ZLIB_VERSION, sizeof(z_stream))
(ffi:defshared %deflate-init-internal "deflateInit_" :zlib :int
  (stream :pointer) (level :int) (version :string) (stream-size :int)
  :documentation "Initialize deflate stream (internal)")

(defun %deflate-init (stream level)
  "Initialize deflate stream with default parameters"
  (%deflate-init-internal stream level (%zlib-version) +z-stream-size+))

;; deflateInit2 is also a macro
(ffi:defshared %deflate-init2-internal "deflateInit2_" :zlib :int
  (stream :pointer) (level :int) (method :int) (window-bits :int)
  (mem-level :int) (strategy :int) (version :string) (stream-size :int)
  :documentation "Initialize deflate with full parameters (internal)")

(defun %deflate-init2 (stream level method window-bits mem-level strategy)
  "Initialize deflate with full parameters"
  (%deflate-init2-internal stream level method window-bits mem-level strategy
                           (%zlib-version) +z-stream-size+))

(ffi:defshared %deflate "deflate" :zlib :int
  (stream :pointer) (flush :int)
  :documentation "Compress data in stream")

(ffi:defshared %deflate-end "deflateEnd" :zlib :int
  (stream :pointer)
  :documentation "Free deflate state")

(ffi:defshared %deflate-reset "deflateReset" :zlib :int
  (stream :pointer)
  :documentation "Reset deflate state for reuse")

(ffi:defshared %deflate-set-dictionary "deflateSetDictionary" :zlib :int
  (stream :pointer) (dictionary :pointer) (dict-length :unsigned-int)
  :documentation "Set compression dictionary")

;;;; Streaming Decompression FFI Functions

;; inflateInit is a macro that calls inflateInit_
(ffi:defshared %inflate-init-internal "inflateInit_" :zlib :int
  (stream :pointer) (version :string) (stream-size :int)
  :documentation "Initialize inflate stream (internal)")

(defun %inflate-init (stream)
  "Initialize inflate stream"
  (%inflate-init-internal stream (%zlib-version) +z-stream-size+))

;; inflateInit2 is also a macro
(ffi:defshared %inflate-init2-internal "inflateInit2_" :zlib :int
  (stream :pointer) (window-bits :int) (version :string) (stream-size :int)
  :documentation "Initialize inflate with window bits (internal)")

(defun %inflate-init2 (stream window-bits)
  "Initialize inflate with specific window bits"
  (%inflate-init2-internal stream window-bits (%zlib-version) +z-stream-size+))

(ffi:defshared %inflate "inflate" :zlib :int
  (stream :pointer) (flush :int)
  :documentation "Decompress data in stream")

(ffi:defshared %inflate-end "inflateEnd" :zlib :int
  (stream :pointer)
  :documentation "Free inflate state")

(ffi:defshared %inflate-reset "inflateReset" :zlib :int
  (stream :pointer)
  :documentation "Reset inflate state for reuse")

(ffi:defshared %inflate-set-dictionary "inflateSetDictionary" :zlib :int
  (stream :pointer) (dictionary :pointer) (dict-length :unsigned-int)
  :documentation "Set decompression dictionary")

;;;; Checksum Functions

(ffi:defshared %crc32 "crc32" :zlib :unsigned-long
  (crc :unsigned-long) (buf :pointer) (len :unsigned-int)
  :documentation "Calculate CRC-32 checksum")

(ffi:defshared %crc32-combine "crc32_combine" :zlib :unsigned-long
  (crc1 :unsigned-long) (crc2 :unsigned-long) (len2 :long)
  :documentation "Combine two CRC-32 checksums")

(ffi:defshared %adler32 "adler32" :zlib :unsigned-long
  (adler :unsigned-long) (buf :pointer) (len :unsigned-int)
  :documentation "Calculate Adler-32 checksum")

(ffi:defshared %adler32-combine "adler32_combine" :zlib :unsigned-long
  (adler1 :unsigned-long) (adler2 :unsigned-long) (len2 :long)
  :documentation "Combine two Adler-32 checksums")

;;;; Utility Functions

(ffi:defshared %zlib-version "zlibVersion" :zlib :string ()
  :documentation "Get zlib version string")

(ffi:defshared %z-error "zError" :zlib :string
  (err :int)
  :documentation "Get error message for error code")

;;;; Initialize on load
(eval-when (:load-toplevel :execute)
  (ensure-zlib-loaded))
