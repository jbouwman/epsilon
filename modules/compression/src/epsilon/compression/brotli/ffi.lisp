;;;; epsilon-module
;;;; :requires (epsilon.foreign epsilon.library epsilon.foreign.binding-ir)
;;;; :provides (epsilon.compression.brotli.ffi)
;;;;
;;;; FFI Bindings for Brotli compression library
;;;;
;;;; Brotli is a general-purpose lossless compression algorithm that
;;;; achieves excellent compression ratios, especially for text content.
;;;; It's the standard compression for HTTP/2 and WOFF2 fonts.

(defpackage :epsilon.compression.brotli.ffi
  (:use :cl)
  (:import
   (epsilon.foreign ffi)
   (epsilon.library lib)
   (epsilon.foreign.binding-ir bir))
  (:export
   ;; Library definition
   #:ensure-brotli-loaded
   #:brotli-available-p

   ;; Constants - Quality levels
   #:+quality-min+
   #:+quality-default+
   #:+quality-max+

   ;; Constants - Modes
   #:+mode-generic+
   #:+mode-text+
   #:+mode-font+

   ;; Constants - Window sizes
   #:+lgwin-min+
   #:+lgwin-default+
   #:+lgwin-max+

   ;; Constants - Operations
   #:+operation-process+
   #:+operation-flush+
   #:+operation-finish+
   #:+operation-emit-metadata+

   ;; Constants - Decoder results
   #:+decoder-result-error+
   #:+decoder-result-success+
   #:+decoder-result-needs-more-input+
   #:+decoder-result-needs-more-output+

   ;; One-shot functions
   #:%encoder-max-compressed-size
   #:%encoder-compress
   #:%decoder-decompress

   ;; Streaming encoder
   #:%encoder-create-instance
   #:%encoder-destroy-instance
   #:%encoder-set-parameter
   #:%encoder-compress-stream
   #:%encoder-has-more-output
   #:%encoder-take-output
   #:%encoder-is-finished

   ;; Streaming decoder
   #:%decoder-create-instance
   #:%decoder-destroy-instance
   #:%decoder-decompress-stream
   #:%decoder-has-more-output
   #:%decoder-take-output
   #:%decoder-is-finished
   #:%decoder-is-used
   #:%decoder-get-error-code

   ;; Version
   #:%encoder-version
   #:%decoder-version

   ;; Encoder parameters
   #:+param-mode+
   #:+param-quality+
   #:+param-lgwin+
   #:+param-lgblock+
   #:+param-size-hint+
   #:+param-large-window+

   ;; Window sizes
   #:+lgwin-min+
   #:+lgwin-default+
   #:+lgwin-max+))

;;;; Library Definition

(lib:define-library :brotli-enc
  :base-name "brotlienc"
  :version "1"
  :bundled-p nil
  :critical-p nil
  :search-names (list #+darwin "/opt/homebrew/lib/libbrotlienc.dylib"
                      #+darwin "/opt/homebrew/lib/libbrotlienc.1.dylib"
                      #+darwin "/usr/local/lib/libbrotlienc.dylib"
                      #+darwin "/usr/local/lib/libbrotlienc.1.dylib"
                      #+darwin "libbrotlienc.dylib"
                      #+linux "/usr/lib/x86_64-linux-gnu/libbrotlienc.so.1"
                      #+linux "/usr/lib/x86_64-linux-gnu/libbrotlienc.so"
                      #+linux "libbrotlienc.so.1" #+linux "libbrotlienc.so"
                      #+windows "brotlienc.dll")
  :description "Brotli encoder library")

(lib:define-library :brotli-dec
  :base-name "brotlidec"
  :version "1"
  :bundled-p nil
  :critical-p nil
  :search-names (list #+darwin "/opt/homebrew/lib/libbrotlidec.dylib"
                      #+darwin "/opt/homebrew/lib/libbrotlidec.1.dylib"
                      #+darwin "/usr/local/lib/libbrotlidec.dylib"
                      #+darwin "/usr/local/lib/libbrotlidec.1.dylib"
                      #+darwin "libbrotlidec.dylib"
                      #+linux "/usr/lib/x86_64-linux-gnu/libbrotlidec.so.1"
                      #+linux "/usr/lib/x86_64-linux-gnu/libbrotlidec.so"
                      #+linux "libbrotlidec.so.1" #+linux "libbrotlidec.so"
                      #+windows "brotlidec.dll")
  :description "Brotli decoder library")

(defvar *brotli-loaded* nil
  "Flag indicating whether brotli libraries have been loaded")

(defun ensure-brotli-loaded ()
  "Ensure brotli libraries are loaded"
  (unless *brotli-loaded*
    (lib:lib-open :brotli-enc)
    (lib:lib-open :brotli-dec)
    (setf *brotli-loaded* t)))

(defun brotli-available-p ()
  "Check if brotli libraries are available"
  (handler-case
      (progn (ensure-brotli-loaded) t)
    (error () nil)))

;;;; Constants
;;;;
;;;; Most constants are loaded from BIR, with custom transforms to match exported names.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *brotli-bir-path*
    (epsilon.loader:module-resource "epsilon.compression" "bindings/brotli.bir.lisp")))

;; Quality levels (from macros)
(bir:define-constants-from-bir *brotli-bir-path*
  :names ("BROTLI_MIN_QUALITY" "BROTLI_MAX_QUALITY" "BROTLI_DEFAULT_QUALITY")
  :transform (lambda (name)
               (cond ((string= name "BROTLI_MIN_QUALITY") "+QUALITY-MIN+")
                     ((string= name "BROTLI_MAX_QUALITY") "+QUALITY-MAX+")
                     ((string= name "BROTLI_DEFAULT_QUALITY") "+QUALITY-DEFAULT+"))))

;; Window sizes (from macros - using WINDOW_BITS naming in C)
(bir:define-constants-from-bir
    *brotli-bir-path*
  :names ("BROTLI_MIN_WINDOW_BITS" "BROTLI_MAX_WINDOW_BITS" "BROTLI_DEFAULT_WINDOW")
  :transform (lambda (name)
               (cond ((string= name "BROTLI_MIN_WINDOW_BITS") "+LGWIN-MIN+")
                     ((string= name "BROTLI_MAX_WINDOW_BITS") "+LGWIN-MAX+")
                     ((string= name "BROTLI_DEFAULT_WINDOW") "+LGWIN-DEFAULT+"))))

;; Encoder modes (from enum BrotliEncoderMode)
(bir:define-enum-constants-from-bir
    *brotli-bir-path*
  "BrotliEncoderMode"
  :prefix "BROTLI_MODE_"
  :transform (lambda (name)
               (format nil "+MODE-~A+" (substitute #\- #\_ (string-upcase name)))))

;; Encoder parameters (from enum BrotliEncoderParameter)
(bir:define-enum-constants-from-bir
    *brotli-bir-path*
  "BrotliEncoderParameter"
  :prefix "BROTLI_PARAM_"
  :transform (lambda (name)
               (format nil "+PARAM-~A+" (substitute #\- #\_ (string-upcase name)))))

;; Encoder operations (from enum BrotliEncoderOperation)
(bir:define-enum-constants-from-bir
    *brotli-bir-path*
  "BrotliEncoderOperation"
  :prefix "BROTLI_OPERATION_"
  :transform (lambda (name)
               (format nil "+OPERATION-~A+" (substitute #\- #\_ (string-upcase name)))))

;; Decoder results (from enum BrotliDecoderResult)
(bir:define-enum-constants-from-bir
    *brotli-bir-path*
  "BrotliDecoderResult"
  :prefix "BROTLI_DECODER_RESULT_"
  :transform (lambda (name)
               (format nil "+DECODER-RESULT-~A+" (substitute #\- #\_ (string-upcase name)))))

;; Decoder error codes (selected values from enum BrotliDecoderErrorCode)
;; Note: The enum has many specific error codes; we define the commonly used ones
(bir:define-enum-constants-from-bir
    *brotli-bir-path*
  "BrotliDecoderErrorCode"
  :names ("BROTLI_DECODER_NO_ERROR" "BROTLI_DECODER_ERROR_INVALID_ARGUMENTS"
          "BROTLI_DECODER_ERROR_UNREACHABLE")
  :transform (lambda (name)
               (cond ((string= name "BROTLI_DECODER_NO_ERROR") "+DECODER-ERROR-NO-ERROR+")
                     ((string= name "BROTLI_DECODER_ERROR_INVALID_ARGUMENTS") "+DECODER-ERROR-INVALID-ARGUMENT+")
                     ((string= name "BROTLI_DECODER_ERROR_UNREACHABLE") "+DECODER-ERROR-UNREACHABLE+"))))

;; Additional decoder error codes not in enum with matching values
;; (the enum has FORMAT errors with different numbering)
(defconstant +decoder-error-format+ 1 "Invalid format (general)")
(defconstant +decoder-error-dictionary-not-set+ 2 "Dictionary not set")
(defconstant +decoder-error-alloc-failed+ 4 "Allocation failed")

;;;; One-shot Compression Functions

(ffi:defshared %encoder-max-compressed-size "BrotliEncoderMaxCompressedSize" :brotli-enc
  :unsigned-long
  (input-size :unsigned-long)
  :documentation "Get maximum compressed size for input size")

(ffi:defshared %encoder-compress "BrotliEncoderCompress" :brotli-enc :int
  (quality :int) (lgwin :int) (mode :int)
  (input-size :unsigned-long) (input-buffer :pointer)
  (encoded-size :pointer) (encoded-buffer :pointer)
  :documentation "One-shot compression")

(ffi:defshared %decoder-decompress "BrotliDecoderDecompress" :brotli-dec :int
  (encoded-size :unsigned-long) (encoded-buffer :pointer)
  (decoded-size :pointer) (decoded-buffer :pointer)
  :documentation "One-shot decompression")

;;;; Streaming Encoder Functions

(ffi:defshared %encoder-create-instance "BrotliEncoderCreateInstance" :brotli-enc :pointer
  (alloc-func :pointer) (free-func :pointer) (opaque :pointer)
  :documentation "Create encoder instance")

(ffi:defshared %encoder-destroy-instance "BrotliEncoderDestroyInstance" :brotli-enc :void
  (state :pointer)
  :documentation "Destroy encoder instance")

(ffi:defshared %encoder-set-parameter "BrotliEncoderSetParameter" :brotli-enc :int
  (state :pointer) (param :int) (value :unsigned-int)
  :documentation "Set encoder parameter")

(ffi:defshared %encoder-compress-stream "BrotliEncoderCompressStream" :brotli-enc :int
  (state :pointer) (op :int)
  (available-in :pointer) (next-in :pointer)
  (available-out :pointer) (next-out :pointer)
  (total-out :pointer)
  :documentation "Streaming compression")

(ffi:defshared %encoder-has-more-output "BrotliEncoderHasMoreOutput" :brotli-enc :int
  (state :pointer)
  :documentation "Check if encoder has pending output")

(ffi:defshared %encoder-take-output "BrotliEncoderTakeOutput" :brotli-enc :pointer
  (state :pointer) (size :pointer)
  :documentation "Take output from encoder")

(ffi:defshared %encoder-is-finished "BrotliEncoderIsFinished" :brotli-enc :int
  (state :pointer)
  :documentation "Check if encoder is finished")

;;;; Streaming Decoder Functions

(ffi:defshared %decoder-create-instance "BrotliDecoderCreateInstance" :brotli-dec :pointer
  (alloc-func :pointer) (free-func :pointer) (opaque :pointer)
  :documentation "Create decoder instance")

(ffi:defshared %decoder-destroy-instance "BrotliDecoderDestroyInstance" :brotli-dec :void
  (state :pointer)
  :documentation "Destroy decoder instance")

(ffi:defshared %decoder-decompress-stream "BrotliDecoderDecompressStream" :brotli-dec :int
  (state :pointer)
  (available-in :pointer) (next-in :pointer)
  (available-out :pointer) (next-out :pointer)
  (total-out :pointer)
  :documentation "Streaming decompression")

(ffi:defshared %decoder-has-more-output "BrotliDecoderHasMoreOutput" :brotli-dec :int
  (state :pointer)
  :documentation "Check if decoder has pending output")

(ffi:defshared %decoder-take-output "BrotliDecoderTakeOutput" :brotli-dec :pointer
  (state :pointer) (size :pointer)
  :documentation "Take output from decoder")

(ffi:defshared %decoder-is-finished "BrotliDecoderIsFinished" :brotli-dec :int
  (state :pointer)
  :documentation "Check if decoder is finished")

(ffi:defshared %decoder-is-used "BrotliDecoderIsUsed" :brotli-dec :int
  (state :pointer)
  :documentation "Check if decoder has been used")

(ffi:defshared %decoder-get-error-code "BrotliDecoderGetErrorCode" :brotli-dec :int
  (state :pointer)
  :documentation "Get decoder error code")

;;;; Version Functions

(ffi:defshared %encoder-version "BrotliEncoderVersion" :brotli-enc :unsigned-int ()
  :documentation "Get encoder version (major*0x1000000 + minor*0x1000 + patch)")

(ffi:defshared %decoder-version "BrotliDecoderVersion" :brotli-dec :unsigned-int ()
  :documentation "Get decoder version")

;;;; Initialize on load (but don't fail if unavailable)
(eval-when (:load-toplevel :execute)
  (ignore-errors (ensure-brotli-loaded)))
