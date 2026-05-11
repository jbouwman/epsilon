;;;; Epsilon Compress Module - Main Entry Point
;;;;
;;;; This package re-exports the public API from all sub-packages, providing
;;;; a single import point for users. All commonly-used compression
;;;; operations are available through this package.
;;;;
;;;; Example:
;;;;   (import (epsilon.compression compress))
;;;;   (compress:compress data)
;;;;   (compress:decompress compressed-data)
;;;;   (let ((ctx (compress:make-deflater :level 6)))
;;;;     (compress:deflate-update ctx chunk)
;;;;     (compress:deflate-finish ctx))

(defpackage epsilon.compression
  (:use :cl)
  (:import (epsilon.symbol sym)
            (epsilon.compression.errors err)
            (epsilon.compression.zlib.api zlib)
            (epsilon.compression.zstd.api zstd)
            (epsilon.compression.brotli.api brotli)
            (epsilon.compression.stream stream))
  (:export compress-error
           compress-error-p
           compress-error-operation
           compress-error-code
           compress-error-message
           zlib-error
           zlib-error-p
           invalid-data-error
           invalid-data-error-p
           buffer-overflow-error
           buffer-overflow-error-p
           decompression-bomb-error
           decompression-bomb-error-p
           signal-compress-error
           *max-compression-ratio*
           *max-uncompressed-size*
           compress
           decompress
           compress-bound
           deflate-context
           deflate-context-p
           make-deflater
           deflate-update
           deflate-finish
           deflate-close
           with-deflater
           inflate-context
           inflate-context-p
           make-inflater
           inflate-update
           inflate-finished-p
           inflate-close
           with-inflater
           crc32
           adler32
           zlib-version
           +no-compression+
           +best-speed+
           +best-compression+
           +default-compression+
           +max-wbits+
           +gzip-wbits+
           +raw-wbits+
           +auto-wbits+
           +no-flush+
           +sync-flush+
           +full-flush+
           +finish+
           deflate-reader
           deflate-reader-p
           make-deflate-reader
           with-deflate-reader
           deflate-writer
           deflate-writer-p
           make-deflate-writer
           with-deflate-writer
           make-inflate-reader
           with-inflate-reader
           compress-stream
           decompress-stream
           *stream-buffer-size*
           zstd-available-p
           zstd-compress
           zstd-decompress
           zstd-compress-bound
           zstd-compress-context
           zstd-compress-context-p
           make-zstd-compressor
           zstd-compress-update
           zstd-compress-finish
           zstd-compress-close
           with-zstd-compressor
           zstd-decompress-context
           zstd-decompress-context-p
           make-zstd-decompressor
           zstd-decompress-update
           zstd-decompress-finished-p
           zstd-decompress-close
           with-zstd-decompressor
           zstd-version
           +zstd-default-level+
           +zstd-min-level+
           +zstd-max-level+
           brotli-available-p
           brotli-compress
           brotli-decompress
           brotli-compress-bound
           brotli-compress-context
           brotli-compress-context-p
           make-brotli-compressor
           brotli-compress-update
           brotli-compress-finish
           brotli-compress-close
           with-brotli-compressor
           brotli-decompress-context
           brotli-decompress-context-p
           make-brotli-decompressor
           brotli-decompress-update
           brotli-decompress-finished-p
           brotli-decompress-close
           with-brotli-decompressor
           brotli-version
           +brotli-quality-min+
           +brotli-quality-default+
           +brotli-quality-max+
           +brotli-mode-generic+
           +brotli-mode-text+
           +brotli-mode-font+))

;;; ---------------------------------------------------------------------------
;;; Error Conditions (re-exported types)
;;; ---------------------------------------------------------------------------

(sym:reexport :epsilon.compression.errors
  '(compress-error-p compress-error-operation compress-error-code
    compress-error-message zlib-error-p invalid-data-error-p
    buffer-overflow-error-p decompression-bomb-error-p signal-compress-error))

;; Define type names as aliases
(deftype compress-error () 'err:compress-error)
(deftype zlib-error () 'err:zlib-error)
(deftype invalid-data-error () 'err:invalid-data-error)
(deftype buffer-overflow-error () 'err:buffer-overflow-error)
(deftype decompression-bomb-error () 'err:decompression-bomb-error)

;;; ---------------------------------------------------------------------------
;;; Safety Limits
;;; ---------------------------------------------------------------------------

(define-symbol-macro *max-compression-ratio* zlib:*max-compression-ratio*)
(define-symbol-macro *max-uncompressed-size* zlib:*max-uncompressed-size*)

;;; ---------------------------------------------------------------------------
;;; Zlib (one-shot, streaming, checksums, utilities)
;;; ---------------------------------------------------------------------------

(sym:reexport :epsilon.compression.zlib.api
  '(compress decompress compress-bound
    deflate-context-p make-deflater deflate-update deflate-finish deflate-close
    inflate-context-p make-inflater inflate-update inflate-finished-p inflate-close
    crc32 adler32 zlib-version))

(deftype deflate-context () 'zlib:deflate-context)

(defmacro with-deflater ((var &rest args) &body body)
  "Execute BODY with VAR bound to a deflate context. Context is cleaned up after."
  `(zlib:with-deflater (,var ,@args) ,@body))

(deftype inflate-context () 'zlib:inflate-context)

(defmacro with-inflater ((var &rest args) &body body)
  "Execute BODY with VAR bound to an inflate context. Context is cleaned up after."
  `(zlib:with-inflater (,var ,@args) ,@body))

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

;; Import constants from FFI package
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +no-compression+ 0)
  (defconstant +best-speed+ 1)
  (defconstant +best-compression+ 9)
  (defconstant +default-compression+ -1)

  (defconstant +max-wbits+ 15)
  (defconstant +gzip-wbits+ 31)      ; 15 + 16
  (defconstant +raw-wbits+ -15)
  (defconstant +auto-wbits+ 47)      ; 15 + 32

  (defconstant +no-flush+ 0)
  (defconstant +sync-flush+ 2)
  (defconstant +full-flush+ 3)
  (defconstant +finish+ 4))

;;; ---------------------------------------------------------------------------
;;; Stream Wrappers
;;; ---------------------------------------------------------------------------

(sym:reexport :epsilon.compression.stream
  '(deflate-reader-p make-deflate-reader deflate-writer-p make-deflate-writer
    make-inflate-reader compress-stream decompress-stream))

(deftype deflate-reader () 'stream:deflate-reader)

(defmacro with-deflate-reader ((var source &rest args) &body body)
  "Execute BODY with a decompressing reader. Reader is cleaned up after."
  `(stream:with-deflate-reader (,var ,source ,@args) ,@body))

(deftype deflate-writer () 'stream:deflate-writer)

(defmacro with-deflate-writer ((var sink &rest args) &body body)
  "Execute BODY with a compressing writer. Writer is cleaned up after."
  `(stream:with-deflate-writer (,var ,sink ,@args) ,@body))

(defmacro with-inflate-reader ((var source &rest args) &body body)
  "Alias for with-deflate-reader."
  `(stream:with-inflate-reader (,var ,source ,@args) ,@body))

(define-symbol-macro *stream-buffer-size* stream:*stream-buffer-size*)

;;; ---------------------------------------------------------------------------
;;; Zstd Compression
;;; ---------------------------------------------------------------------------

(sym:reexport :epsilon.compression.zstd.api
  '(zstd-available-p zstd-version))

;; Renamed re-exports (zstd-* prefix added)
(setf (symbol-function 'zstd-compress) #'zstd:compress)
(setf (symbol-function 'zstd-decompress) #'zstd:decompress)
(setf (symbol-function 'zstd-compress-bound) #'zstd:compress-bound)

;; Streaming compression
(setf (symbol-function 'zstd-compress-context-p) #'zstd:compress-context-p)
(setf (symbol-function 'make-zstd-compressor) #'zstd:make-compressor)
(setf (symbol-function 'zstd-compress-update) #'zstd:compress-update)
(setf (symbol-function 'zstd-compress-finish) #'zstd:compress-finish)
(setf (symbol-function 'zstd-compress-close) #'zstd:compress-close)

(deftype zstd-compress-context () 'zstd:compress-context)

(defmacro with-zstd-compressor ((var &rest args) &body body)
  "Execute body with zstd compressor context, ensure cleanup."
  `(zstd:with-compressor (,var ,@args) ,@body))

;; Streaming decompression
(setf (symbol-function 'zstd-decompress-context-p) #'zstd:decompress-context-p)
(setf (symbol-function 'make-zstd-decompressor) #'zstd:make-decompressor)
(setf (symbol-function 'zstd-decompress-update) #'zstd:decompress-update)
(setf (symbol-function 'zstd-decompress-finished-p) #'zstd:decompress-finished-p)
(setf (symbol-function 'zstd-decompress-close) #'zstd:decompress-close)

(deftype zstd-decompress-context () 'zstd:decompress-context)

(defmacro with-zstd-decompressor ((var) &body body)
  "Execute body with zstd decompressor context, ensure cleanup."
  `(zstd:with-decompressor (,var) ,@body))

;; Constants
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +zstd-default-level+ 3)
  (defconstant +zstd-min-level+ 1)
  (defconstant +zstd-max-level+ 22))

;;; ---------------------------------------------------------------------------
;;; Brotli Compression
;;; ---------------------------------------------------------------------------

(sym:reexport :epsilon.compression.brotli.api
  '(brotli-available-p brotli-version))

;; Renamed re-exports (brotli-* prefix added)
(setf (symbol-function 'brotli-compress) #'brotli:compress)
(setf (symbol-function 'brotli-decompress) #'brotli:decompress)
(setf (symbol-function 'brotli-compress-bound) #'brotli:compress-bound)

;; Streaming compression
(setf (symbol-function 'brotli-compress-context-p) #'brotli:compress-context-p)
(setf (symbol-function 'make-brotli-compressor) #'brotli:make-compressor)
(setf (symbol-function 'brotli-compress-update) #'brotli:compress-update)
(setf (symbol-function 'brotli-compress-finish) #'brotli:compress-finish)
(setf (symbol-function 'brotli-compress-close) #'brotli:compress-close)

(deftype brotli-compress-context () 'brotli:compress-context)

(defmacro with-brotli-compressor ((var &rest args) &body body)
  "Execute body with brotli compressor context, ensure cleanup."
  `(brotli:with-compressor (,var ,@args) ,@body))

;; Streaming decompression
(setf (symbol-function 'brotli-decompress-context-p) #'brotli:decompress-context-p)
(setf (symbol-function 'make-brotli-decompressor) #'brotli:make-decompressor)
(setf (symbol-function 'brotli-decompress-update) #'brotli:decompress-update)
(setf (symbol-function 'brotli-decompress-finished-p) #'brotli:decompress-finished-p)
(setf (symbol-function 'brotli-decompress-close) #'brotli:decompress-close)

(deftype brotli-decompress-context () 'brotli:decompress-context)

(defmacro with-brotli-decompressor ((var) &body body)
  "Execute body with brotli decompressor context, ensure cleanup."
  `(brotli:with-decompressor (,var) ,@body))

;; Constants
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +brotli-quality-min+ 0)
  (defconstant +brotli-quality-default+ 11)
  (defconstant +brotli-quality-max+ 11)
  (defconstant +brotli-mode-generic+ 0)
  (defconstant +brotli-mode-text+ 1)
  (defconstant +brotli-mode-font+ 2))
