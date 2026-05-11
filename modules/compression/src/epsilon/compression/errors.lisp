;;;; Compression Error Conditions
;;;;
;;;; Defines the error hierarchy for compression operations.

(defpackage epsilon.compression.errors
  (:use :cl :epsilon.syntax)
  (:import-from :epsilon.symbol #:define-condition-predicates))

;;; Base compression error

(define-condition compress-error (error)
  ((operation :initarg :operation :reader compress-error-operation
              :type keyword
              :documentation "The compression operation that failed")
   (code :initarg :code :reader compress-error-code
         :type (or integer null)
         :initform nil
         :documentation "Underlying library error code if available")
   (message :initarg :message :reader compress-error-message
            :type (or string null)
            :initform nil
            :documentation "Error message from underlying library"))
  (:documentation "Base condition for compression errors")
  (:report (lambda (condition stream)
             (format stream "Compression error during ~A"
                     (compress-error-operation condition))
             (when (compress-error-code condition)
               (format stream " (code ~D)" (compress-error-code condition)))
             (when (compress-error-message condition)
               (format stream ": ~A" (compress-error-message condition))))))

;;; Specific error types

(define-condition zlib-error (compress-error)
  ()
  (:documentation "Error from zlib library"))

(define-condition zstd-error (compress-error)
  ()
  (:documentation "Error from zstd library"))

(define-condition brotli-error (compress-error)
  ()
  (:documentation "Error from brotli library"))

(define-condition invalid-data-error (compress-error)
  ()
  (:documentation "Input data is not valid compressed data")
  (:report (lambda (condition stream)
             (format stream "Invalid compressed data during ~A"
                     (compress-error-operation condition))
             (when (compress-error-message condition)
               (format stream ": ~A" (compress-error-message condition))))))

(define-condition buffer-overflow-error (compress-error)
  ((required :initarg :required :reader buffer-overflow-required
             :type integer
             :documentation "Required buffer size")
   (available :initarg :available :reader buffer-overflow-available
              :type integer
              :documentation "Available buffer size"))
  (:documentation "Output buffer too small for operation")
  (:report (lambda (condition stream)
             (format stream "Buffer overflow during ~A: required ~D bytes, have ~D"
                     (compress-error-operation condition)
                     (buffer-overflow-required condition)
                     (buffer-overflow-available condition)))))

(define-condition decompression-bomb-error (compress-error)
  ((compressed-size :initarg :compressed-size :reader decompression-bomb-compressed-size
                    :type integer)
   (uncompressed-size :initarg :uncompressed-size :reader decompression-bomb-uncompressed-size
                      :type integer)
   (ratio :initarg :ratio :reader decompression-bomb-ratio
          :type number))
  (:documentation "Detected potential decompression bomb")
  (:report (lambda (condition stream)
             (format stream "Decompression bomb detected: ~D -> ~D bytes (ratio ~,1F:1)"
                     (decompression-bomb-compressed-size condition)
                     (decompression-bomb-uncompressed-size condition)
                     (decompression-bomb-ratio condition)))))

(define-condition stream-state-error (compress-error)
  ((expected-state :initarg :expected-state :reader stream-state-expected)
   (actual-state :initarg :actual-state :reader stream-state-actual))
  (:documentation "Compression stream in unexpected state"))

;;; Error signaling helpers

(defun signal-compress-error (operation code &optional message)
  "Signal a compression error"
  (error 'compress-error
         :operation operation
         :code code
         :message message))

(defun signal-zlib-error (operation code &optional message)
  "Signal a zlib-specific error"
  (error 'zlib-error
         :operation operation
         :code code
         :message (or message (zlib-error-string code))))

(defun signal-invalid-data (operation &optional message)
  "Signal invalid compressed data"
  (error 'invalid-data-error
         :operation operation
         :message message))

(defun signal-buffer-overflow (operation required available)
  "Signal buffer overflow"
  (error 'buffer-overflow-error
         :operation operation
         :required required
         :available available))

(defun signal-decompression-bomb (compressed-size uncompressed-size)
  "Signal potential decompression bomb"
  (error 'decompression-bomb-error
         :operation :decompress
         :compressed-size compressed-size
         :uncompressed-size uncompressed-size
         :ratio (/ uncompressed-size compressed-size)))

;;; Predicates (macro lives in epsilon.symbol)

(define-condition-predicates
  compress-error
  zlib-error
  zstd-error
  brotli-error
  invalid-data-error
  buffer-overflow-error
  decompression-bomb-error)

;;; Zlib error code translation

(defun zlib-error-string (code)
  "Convert zlib error code to descriptive string"
  (case code
    (0 "Z_OK")
    (1 "Z_STREAM_END")
    (2 "Z_NEED_DICT")
    (-1 "Z_ERRNO: file operation error")
    (-2 "Z_STREAM_ERROR: invalid stream state")
    (-3 "Z_DATA_ERROR: invalid or incomplete data")
    (-4 "Z_MEM_ERROR: out of memory")
    (-5 "Z_BUF_ERROR: buffer too small")
    (-6 "Z_VERSION_ERROR: zlib version mismatch")
    (otherwise (format nil "Unknown zlib error: ~D" code))))

(defun check-zlib-result (result operation)
  "Check zlib result code and signal error if needed"
  (unless (or (= result 0)   ; Z_OK
              (= result 1))  ; Z_STREAM_END (sometimes expected)
    (signal-zlib-error operation result))
  result)
