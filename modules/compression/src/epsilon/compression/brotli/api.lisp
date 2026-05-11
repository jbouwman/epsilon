;;;; High-Level Brotli API
;;;;
;;;; Provides a clean Lisp interface over the Brotli FFI bindings.
;;;; Includes one-shot compression/decompression and streaming APIs.
;;;;
;;;; Brotli achieves excellent compression ratios, especially for text.
;;;; It's the standard compression for HTTP/2 and WOFF2 fonts.

(defpackage epsilon.compression.brotli.api
  (:use :cl :epsilon.syntax)
  (:import (epsilon.compression.brotli.ffi ffi)
            (epsilon.compression.errors err)
            (epsilon.foreign foreign))
  (:export brotli-available-p
           +quality-min+
           +quality-default+
           +quality-max+
           +mode-generic+
           +mode-text+
           +mode-font+
           *max-compression-ratio*
           *max-uncompressed-size*
           compress
           decompress
           compress-bound
           compress-context
           compress-context-p
           make-compressor
           compress-update
           compress-finish
           compress-close
           with-compressor
           decompress-context
           decompress-context-p
           make-decompressor
           decompress-update
           decompress-finished-p
           decompress-close
           with-decompressor
           brotli-version))

;;; Quality level constants

(defconstant +quality-min+ ffi:+quality-min+ "Minimum quality (fastest)")
(defconstant +quality-default+ ffi:+quality-default+ "Default quality (11)")
(defconstant +quality-max+ ffi:+quality-max+ "Maximum quality (best compression)")

;;; Mode constants

(defconstant +mode-generic+ ffi:+mode-generic+ "Generic mode (default)")
(defconstant +mode-text+ ffi:+mode-text+ "Text mode (UTF-8 optimized)")
(defconstant +mode-font+ ffi:+mode-font+ "Font mode (WOFF2 optimized)")

;;; Safety limits for decompression

(defparameter *max-compression-ratio* 1000
  "Maximum allowed compression ratio to prevent decompression bombs")

(defparameter *max-uncompressed-size* (* 1024 1024 1024)  ; 1GB
  "Maximum allowed uncompressed size in bytes")

(defparameter *default-buffer-size* 65536
  "Default buffer size for streaming operations (64KB)")

;;; Availability check

(defun brotli-available-p ()
  "Check if brotli library is available"
  (ffi:brotli-available-p))

;;; One-shot Compression

(defun compress (data &key (quality +quality-default+)
                         (lgwin ffi:+lgwin-default+)
                         (mode +mode-generic+))
  "Compress DATA (byte vector) and return compressed bytes.
   QUALITY: compression quality 0-11 (higher = better ratio, slower)
   LGWIN: log2 of window size (10-24, default 22 = 4MB window)
   MODE: compression mode (:generic, :text, or :font)"
  (ffi:ensure-brotli-loaded)
  (check-type data (vector (unsigned-byte 8)))
  (check-type quality (integer 0 11))
  (check-type lgwin (integer 10 24))
  (let* ((source-len (length data))
         (dest-len (ffi:%encoder-max-compressed-size source-len))
         (dest (make-array dest-len :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (data dest)
      (let ((source-ptr (sb-sys:vector-sap data))
            (dest-ptr (sb-sys:vector-sap dest)))
        (foreign:with-foreign-memory ((dest-len-ptr :unsigned-long))
          (setf (sb-sys:sap-ref-64 dest-len-ptr 0) dest-len)
          (let ((result (ffi:%encoder-compress quality lgwin mode
                                               source-len source-ptr
                                               dest-len-ptr dest-ptr)))
            (unless (= result 1)  ; BROTLI_TRUE
              (err:signal-compress-error :compress nil "Brotli compression failed"))
            ;; Return only the actual compressed data
            (let ((actual-len (sb-sys:sap-ref-64 dest-len-ptr 0)))
              (subseq dest 0 actual-len))))))))

(defun decompress (data &key max-output-size)
  "Decompress DATA (byte vector) and return decompressed bytes.
   MAX-OUTPUT-SIZE: if specified, limits output size"
  (ffi:ensure-brotli-loaded)
  (check-type data (vector (unsigned-byte 8)))
  (let* ((source-len (length data))
         (max-size (or max-output-size *max-uncompressed-size*))
         ;; Brotli doesn't have a way to get decompressed size, start with 4x
         ;; but ensure we have a reasonable minimum
         (initial-size (min (max (* source-len 4) 65536) max-size)))
    ;; Try decompression with increasing buffer sizes
    ;; Note: BrotliDecoderDecompress returns ERROR (0) for both actual errors
    ;; AND when buffer is too small, so we retry with larger buffers
    (loop with dest-size = initial-size
          with attempts = 0
          with max-attempts = 20  ; Allow up to 2^20 growth
          do (let ((dest (make-array dest-size :element-type '(unsigned-byte 8))))
               (sb-sys:with-pinned-objects (data dest)
                 (let ((source-ptr (sb-sys:vector-sap data))
                       (dest-ptr (sb-sys:vector-sap dest)))
                   (foreign:with-foreign-memory ((dest-len-ptr :unsigned-long))
                     (setf (sb-sys:sap-ref-64 dest-len-ptr 0) dest-size)
                     (let ((result (ffi:%decoder-decompress source-len source-ptr
                                                            dest-len-ptr dest-ptr)))
                       (cond
                         ;; Success
                         ((= result ffi:+decoder-result-success+)
                          (let ((actual-len (sb-sys:sap-ref-64 dest-len-ptr 0)))
                            ;; Check for decompression bomb
                            (when (and (> source-len 0)
                                       (> actual-len 0)
                                       (> (/ actual-len source-len) *max-compression-ratio*))
                              (err:signal-decompression-bomb source-len actual-len))
                            (return (subseq dest 0 actual-len))))
                         ;; Error - might be buffer too small, try larger buffer
                         ;; BrotliDecoderDecompress returns 0 for both real errors
                         ;; and "buffer too small" cases
                         ((and (= result ffi:+decoder-result-error+)
                               (< dest-size max-size)
                               (< attempts max-attempts))
                          (setf dest-size (min (* dest-size 2) max-size))
                          (incf attempts))
                         ;; Actual error or exceeded limits
                         (t
                          (err:signal-compress-error :decompress result
                                                     "Brotli decompression failed")))))))))))

(defun compress-bound (source-length)
  "Return upper bound on compressed size for SOURCE-LENGTH bytes"
  (ffi:ensure-brotli-loaded)
  (ffi:%encoder-max-compressed-size source-length))

;;; Streaming Compression

(defstruct compress-context
  "Context for streaming brotli compression"
  (state nil :type (or null sb-sys:system-area-pointer))
  (quality +quality-default+ :type integer)
  (mode +mode-generic+ :type integer)
  (finalized-p nil :type boolean)
  (total-in 0 :type integer)
  (total-out 0 :type integer))

(defun make-compressor (&key (quality +quality-default+)
                           (lgwin ffi:+lgwin-default+)
                           (mode +mode-generic+))
  "Create a streaming brotli compressor.
   QUALITY: compression quality 0-11
   LGWIN: log2 of window size (10-24)
   MODE: compression mode"
  (ffi:ensure-brotli-loaded)
  (check-type quality (integer 0 11))
  (check-type lgwin (integer 10 24))
  (let ((state (ffi:%encoder-create-instance
                (sb-sys:int-sap 0) (sb-sys:int-sap 0) (sb-sys:int-sap 0))))
    (when (sb-sys:sap= state (sb-sys:int-sap 0))
      (error 'err:compress-error :operation :create-compressor
                                 :message "Failed to create brotli encoder"))
    ;; Set parameters
    (ffi:%encoder-set-parameter state ffi:+param-quality+ quality)
    (ffi:%encoder-set-parameter state ffi:+param-lgwin+ lgwin)
    (ffi:%encoder-set-parameter state ffi:+param-mode+ mode)
    (make-compress-context :state state :quality quality :mode mode)))

(defun compress-update (ctx input)
  "Feed INPUT bytes to compressor, return compressed output."
  (check-type ctx compress-context)
  (check-type input (vector (unsigned-byte 8)))
  (when (compress-context-finalized-p ctx)
    (error 'err:stream-state-error
           :operation :compress-update
           :expected-state :active
           :actual-state :finalized))
  (compress-stream-impl ctx input ffi:+operation-process+))

(defun compress-finish (ctx)
  "Finish compression and return final output."
  (check-type ctx compress-context)
  (when (compress-context-finalized-p ctx)
    (return-from compress-finish
      (make-array 0 :element-type '(unsigned-byte 8))))
  (prog1
      (compress-stream-impl ctx (make-array 0 :element-type '(unsigned-byte 8))
                            ffi:+operation-finish+)
    (setf (compress-context-finalized-p ctx) t)))

(defun compress-stream-impl (ctx input operation)
  "Implementation of streaming compression."
  (let* ((state (compress-context-state ctx))
         (input-len (length input))
         (output-size *default-buffer-size*)
         (output (make-array output-size :element-type '(unsigned-byte 8)))
         (output-chunks nil))
    (sb-sys:with-pinned-objects (input output)
      (foreign:with-foreign-memory ((avail-in :unsigned-long)
                                    (next-in :pointer)
                                    (avail-out :unsigned-long)
                                    (next-out :pointer)
                                    (total-out :unsigned-long))
        (setf (sb-sys:sap-ref-64 avail-in 0) input-len)
        (setf (sb-sys:sap-ref-sap next-in 0) (sb-sys:vector-sap input))
        (setf (sb-sys:sap-ref-64 total-out 0) 0)
        ;; Process until done
        (loop do (setf (sb-sys:sap-ref-64 avail-out 0) output-size)
                 (setf (sb-sys:sap-ref-sap next-out 0) (sb-sys:vector-sap output))
                 (let ((result (ffi:%encoder-compress-stream
                                state operation
                                avail-in next-in
                                avail-out next-out
                                total-out)))
                   (unless (= result 1)
                     (err:signal-compress-error :compress-stream nil
                                                "Brotli streaming compression failed"))
                   ;; Collect output
                   (let ((produced (- output-size (sb-sys:sap-ref-64 avail-out 0))))
                     (when (> produced 0)
                       (push (subseq output 0 produced) output-chunks))))
              ;; Continue while there's more output or input to process
              while (or (plusp (ffi:%encoder-has-more-output state))
                        (and (= operation ffi:+operation-finish+)
                             (zerop (ffi:%encoder-is-finished state)))))
        ;; Update totals
        (incf (compress-context-total-in ctx) input-len)
        (incf (compress-context-total-out ctx) (sb-sys:sap-ref-64 total-out 0))))
    ;; Concatenate output chunks
    (if (null output-chunks)
        (make-array 0 :element-type '(unsigned-byte 8))
        (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse output-chunks)))))

(defun compress-close (ctx)
  "Release compressor resources."
  (when (and ctx (compress-context-state ctx))
    (ffi:%encoder-destroy-instance (compress-context-state ctx))
    (setf (compress-context-state ctx) nil)))

(defmacro with-compressor ((var &rest args) &body body)
  "Execute body with compressor context, ensure cleanup."
  `(let ((,var (make-compressor ,@args)))
     (unwind-protect
          (progn ,@body)
       (compress-close ,var))))

;;; Streaming Decompression

(defstruct decompress-context
  "Context for streaming brotli decompression"
  (state nil :type (or null sb-sys:system-area-pointer))
  (finished-p nil :type boolean)
  (total-in 0 :type integer)
  (total-out 0 :type integer))

(defun make-decompressor ()
  "Create a streaming brotli decompressor."
  (ffi:ensure-brotli-loaded)
  (let ((state (ffi:%decoder-create-instance
                (sb-sys:int-sap 0) (sb-sys:int-sap 0) (sb-sys:int-sap 0))))
    (when (sb-sys:sap= state (sb-sys:int-sap 0))
      (error 'err:compress-error :operation :create-decompressor
                                 :message "Failed to create brotli decoder"))
    (make-decompress-context :state state)))

(defun decompress-update (ctx input)
  "Feed INPUT bytes to decompressor, return decompressed output."
  (check-type ctx decompress-context)
  (check-type input (vector (unsigned-byte 8)))
  (when (decompress-context-finished-p ctx)
    (error 'err:stream-state-error
           :operation :decompress-update
           :expected-state :active
           :actual-state :finished))
  (let* ((state (decompress-context-state ctx))
         (input-len (length input))
         (output-size *default-buffer-size*)
         (output (make-array output-size :element-type '(unsigned-byte 8)))
         (output-chunks nil)
         (total-produced 0))
    (sb-sys:with-pinned-objects (input output)
      (foreign:with-foreign-memory ((avail-in :unsigned-long)
                                    (next-in :pointer)
                                    (avail-out :unsigned-long)
                                    (next-out :pointer)
                                    (total-out :unsigned-long))
        (setf (sb-sys:sap-ref-64 avail-in 0) input-len)
        (setf (sb-sys:sap-ref-sap next-in 0) (sb-sys:vector-sap input))
        (setf (sb-sys:sap-ref-64 total-out 0) 0)
        ;; Process until done
        (loop do (setf (sb-sys:sap-ref-64 avail-out 0) output-size)
                 (setf (sb-sys:sap-ref-sap next-out 0) (sb-sys:vector-sap output))
                 (let ((result (ffi:%decoder-decompress-stream
                                state
                                avail-in next-in
                                avail-out next-out
                                total-out)))
                   (cond
                     ;; Success or need more input/output
                     ((or (= result ffi:+decoder-result-success+)
                          (= result ffi:+decoder-result-needs-more-input+)
                          (= result ffi:+decoder-result-needs-more-output+))
                      ;; Collect output
                      (let ((produced (- output-size (sb-sys:sap-ref-64 avail-out 0))))
                        (incf total-produced produced)
                        ;; Check for decompression bomb
                        (when (and (> input-len 0)
                                   (> total-produced 0)
                                   (> (/ total-produced input-len) *max-compression-ratio*))
                          (err:signal-decompression-bomb input-len total-produced))
                        (when (> produced 0)
                          (push (subseq output 0 produced) output-chunks)))
                      ;; Check if finished
                      (when (= result ffi:+decoder-result-success+)
                        (setf (decompress-context-finished-p ctx) t)
                        (return)))
                     ;; Error
                     (t
                      (let ((error-code (ffi:%decoder-get-error-code state)))
                        (err:signal-compress-error :decompress-stream error-code
                                                   "Brotli streaming decompression failed")))))
              ;; Continue while there's more to process
              while (and (plusp (sb-sys:sap-ref-64 avail-in 0))
                         (not (decompress-context-finished-p ctx))))
        ;; Update totals
        (incf (decompress-context-total-in ctx)
              (- input-len (sb-sys:sap-ref-64 avail-in 0)))
        (incf (decompress-context-total-out ctx) total-produced)))
    ;; Concatenate output chunks
    (if (null output-chunks)
        (make-array 0 :element-type '(unsigned-byte 8))
        (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse output-chunks)))))

(defun decompress-finished-p (ctx)
  "Check if decompression is complete."
  (decompress-context-finished-p ctx))

(defun decompress-close (ctx)
  "Release decompressor resources."
  (when (and ctx (decompress-context-state ctx))
    (ffi:%decoder-destroy-instance (decompress-context-state ctx))
    (setf (decompress-context-state ctx) nil)))

(defmacro with-decompressor ((var) &body body)
  "Execute body with decompressor context, ensure cleanup."
  `(let ((,var (make-decompressor)))
     (unwind-protect
          (progn ,@body)
       (decompress-close ,var))))

;;; Utility Functions

(defun brotli-version ()
  "Return brotli version string."
  (ffi:ensure-brotli-loaded)
  (let ((version (ffi:%encoder-version)))
    (format nil "~D.~D.~D"
            (ash version -24)
            (logand (ash version -12) #xFFF)
            (logand version #xFFF))))
