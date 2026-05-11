;;;; High-Level Zstd API
;;;;
;;;; Provides a clean Lisp interface over the zstd FFI bindings.
;;;; Includes one-shot compression/decompression and streaming APIs.
;;;;
;;;; Zstd (Zstandard) provides very fast compression with excellent ratios.
;;;; It's particularly well-suited for real-time compression scenarios.

(defpackage epsilon.compression.zstd.api
  (:use :cl :epsilon.syntax)
  (:import (epsilon.compression.zstd.ffi ffi)
            (epsilon.compression.errors err)
            (epsilon.foreign foreign))
  (:export zstd-available-p
           +default-level+
           +min-level+
           +max-level+
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
           zstd-version))

;;; Compression level constants

(defconstant +default-level+ ffi:+zstd-clevel-default+
  "Default compression level (3)")
(defconstant +min-level+ ffi:+zstd-clevel-min+
  "Minimum compression level (1)")
(defconstant +max-level+ ffi:+zstd-clevel-max+
  "Maximum compression level (22)")

;;; Safety limits for decompression

(defparameter *max-compression-ratio* 1000
  "Maximum allowed compression ratio to prevent decompression bombs")

(defparameter *max-uncompressed-size* (* 1024 1024 1024)  ; 1GB
  "Maximum allowed uncompressed size in bytes")

(defparameter *default-buffer-size* 131072
  "Default buffer size for streaming operations (128KB)")

;;; Availability check

(defun zstd-available-p ()
  "Check if zstd library is available"
  (ffi:zstd-available-p))

;;; Error checking helper

(defun check-zstd-result (result operation)
  "Check zstd result and signal error if needed"
  (when (plusp (ffi:%is-error result))
    (err:signal-compress-error operation
                               (ffi:%get-error-code result)
                               (ffi:%get-error-name result)))
  result)

;;; One-shot Compression

(defun compress (data &key (level +default-level+))
  "Compress DATA (byte vector) and return compressed bytes.
   LEVEL: compression level 1-22
     1 = fastest compression
     22 = best compression ratio
     3 = default (good balance)"
  (ffi:ensure-zstd-loaded)
  (check-type data (vector (unsigned-byte 8)))
  (check-type level (integer 1 22))
  (let* ((source-len (length data))
         (dest-len (ffi:%compress-bound source-len))
         (dest (make-array dest-len :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (data dest)
      (let ((source-ptr (sb-sys:vector-sap data))
            (dest-ptr (sb-sys:vector-sap dest)))
        (let ((result (ffi:%compress dest-ptr dest-len source-ptr source-len level)))
          (check-zstd-result result :compress)
          ;; Result is the actual compressed size
          (subseq dest 0 result))))))

(defun decompress (data &key max-output-size)
  "Decompress DATA (byte vector) and return decompressed bytes.
   MAX-OUTPUT-SIZE: if specified, limits output size"
  (ffi:ensure-zstd-loaded)
  (check-type data (vector (unsigned-byte 8)))
  (let* ((source-len (length data))
         (max-size (or max-output-size *max-uncompressed-size*)))
    (sb-sys:with-pinned-objects (data)
      (let* ((source-ptr (sb-sys:vector-sap data))
             ;; Try to get frame content size first
             (frame-size (ffi:%get-frame-content-size source-ptr source-len))
             ;; Determine initial buffer size
             (initial-size (cond
                             ;; Known size from frame
                             ((and (< frame-size (1- (ash 1 63)))
                                   (> frame-size 0))
                              (min frame-size max-size))
                             ;; Unknown - start with 4x input
                             (t (min (* source-len 4) max-size)))))
        ;; Try decompression with increasing buffer sizes
        (loop with dest-size = (max initial-size 1024)
              do (let ((dest (make-array dest-size :element-type '(unsigned-byte 8))))
                   (sb-sys:with-pinned-objects (dest)
                     (let* ((dest-ptr (sb-sys:vector-sap dest))
                            (result (ffi:%decompress dest-ptr dest-size source-ptr source-len)))
                       (cond
                         ;; Success - result is decompressed size
                         ((zerop (ffi:%is-error result))
                          ;; Check for decompression bomb
                          (when (and (> source-len 0)
                                     (> result 0)
                                     (> (/ result source-len) *max-compression-ratio*))
                            (err:signal-decompression-bomb source-len result))
                          (return (subseq dest 0 result)))
                         ;; Buffer too small - try larger
                         ((and (string= "Destination buffer is too small"
                                        (ffi:%get-error-name result))
                               (< dest-size max-size))
                          (setf dest-size (min (* dest-size 2) max-size)))
                         ;; Other error
                         (t
                          (check-zstd-result result :decompress)))))))))))

(defun compress-bound (source-length)
  "Return upper bound on compressed size for SOURCE-LENGTH bytes"
  (ffi:ensure-zstd-loaded)
  (ffi:%compress-bound source-length))

;;; Streaming Compression

(defstruct compress-context
  "Context for streaming zstd compression"
  (stream nil :type (or null sb-sys:system-area-pointer))
  (level +default-level+ :type integer)
  (finalized-p nil :type boolean)
  (total-in 0 :type integer)
  (total-out 0 :type integer))

(defun make-compressor (&key (level +default-level+))
  "Create a streaming zstd compressor.
   LEVEL: compression level 1-22 (default 3)"
  (ffi:ensure-zstd-loaded)
  (check-type level (integer 1 22))
  (let ((cstream (ffi:%cstream-create)))
    (when (sb-sys:sap= cstream (sb-sys:int-sap 0))
      (error 'err:compress-error :operation :create-compressor
                                 :message "Failed to create compression stream"))
    (let ((result (ffi:%cstream-init cstream level)))
      (when (plusp (ffi:%is-error result))
        (ffi:%cstream-free cstream)
        (err:signal-compress-error :init-compressor
                                   (ffi:%get-error-code result)
                                   (ffi:%get-error-name result))))
    (make-compress-context :stream cstream :level level)))

(defun compress-update (ctx input)
  "Feed INPUT bytes to compressor, return compressed output."
  (check-type ctx compress-context)
  (check-type input (vector (unsigned-byte 8)))
  (when (compress-context-finalized-p ctx)
    (error 'err:stream-state-error
           :operation :compress-update
           :expected-state :active
           :actual-state :finalized))
  (let* ((cstream (compress-context-stream ctx))
         (input-len (length input))
         (output-size (max (compress-bound input-len) *default-buffer-size*))
         (output (make-array output-size :element-type '(unsigned-byte 8)))
         (output-chunks nil))
    (sb-sys:with-pinned-objects (input output)
      (ffi:with-zstd-buffer (in-buf)
        (ffi:with-zstd-buffer (out-buf)
          (ffi:set-zstd-buffer-ptr in-buf (sb-sys:vector-sap input))
          (ffi:set-zstd-buffer-size in-buf input-len)
          (ffi:set-zstd-buffer-pos in-buf 0)
          ;; Process all input
          (loop while (< (ffi:zstd-buffer-pos in-buf) input-len)
                do (ffi:set-zstd-buffer-ptr out-buf (sb-sys:vector-sap output))
                   (ffi:set-zstd-buffer-size out-buf output-size)
                   (ffi:set-zstd-buffer-pos out-buf 0)
                   (let ((result (ffi:%cstream-compress cstream out-buf in-buf)))
                     (check-zstd-result result :compress-stream)
                     ;; Collect output
                     (let ((produced (ffi:zstd-buffer-pos out-buf)))
                       (when (> produced 0)
                         (push (subseq output 0 produced) output-chunks)))))
          ;; Update totals
          (incf (compress-context-total-in ctx) input-len)
          (incf (compress-context-total-out ctx)
                (reduce #'+ output-chunks :key #'length :initial-value 0)))))
    ;; Concatenate output chunks
    (if (null output-chunks)
        (make-array 0 :element-type '(unsigned-byte 8))
        (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse output-chunks)))))

(defun compress-finish (ctx)
  "Finish compression and return final output."
  (check-type ctx compress-context)
  (when (compress-context-finalized-p ctx)
    (return-from compress-finish
      (make-array 0 :element-type '(unsigned-byte 8))))
  (let* ((cstream (compress-context-stream ctx))
         (output-size *default-buffer-size*)
         (output (make-array output-size :element-type '(unsigned-byte 8)))
         (output-chunks nil))
    (sb-sys:with-pinned-objects (output)
      (ffi:with-zstd-buffer (out-buf)
        ;; Flush remaining data
        (loop do (ffi:set-zstd-buffer-ptr out-buf (sb-sys:vector-sap output))
                 (ffi:set-zstd-buffer-size out-buf output-size)
                 (ffi:set-zstd-buffer-pos out-buf 0)
                 (let ((remaining (ffi:%cstream-end cstream out-buf)))
                   (check-zstd-result remaining :compress-end)
                   ;; Collect output
                   (let ((produced (ffi:zstd-buffer-pos out-buf)))
                     (when (> produced 0)
                       (push (subseq output 0 produced) output-chunks)))
                   ;; remaining=0 means done
                   (when (zerop remaining)
                     (return))))))
    (setf (compress-context-finalized-p ctx) t)
    ;; Concatenate output chunks
    (if (null output-chunks)
        (make-array 0 :element-type '(unsigned-byte 8))
        (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse output-chunks)))))

(defun compress-close (ctx)
  "Release compressor resources."
  (when (and ctx (compress-context-stream ctx))
    (ffi:%cstream-free (compress-context-stream ctx))
    (setf (compress-context-stream ctx) nil)))

(defmacro with-compressor ((var &rest args) &body body)
  "Execute body with compressor context, ensure cleanup."
  `(let ((,var (make-compressor ,@args)))
     (unwind-protect
          (progn ,@body)
       (compress-close ,var))))

;;; Streaming Decompression

(defstruct decompress-context
  "Context for streaming zstd decompression"
  (stream nil :type (or null sb-sys:system-area-pointer))
  (finished-p nil :type boolean)
  (total-in 0 :type integer)
  (total-out 0 :type integer))

(defun make-decompressor ()
  "Create a streaming zstd decompressor."
  (ffi:ensure-zstd-loaded)
  (let ((dstream (ffi:%dstream-create)))
    (when (sb-sys:sap= dstream (sb-sys:int-sap 0))
      (error 'err:compress-error :operation :create-decompressor
                                 :message "Failed to create decompression stream"))
    (let ((result (ffi:%dstream-init dstream)))
      (when (plusp (ffi:%is-error result))
        (ffi:%dstream-free dstream)
        (err:signal-compress-error :init-decompressor
                                   (ffi:%get-error-code result)
                                   (ffi:%get-error-name result))))
    (make-decompress-context :stream dstream)))

(defun decompress-update (ctx input)
  "Feed INPUT bytes to decompressor, return decompressed output."
  (check-type ctx decompress-context)
  (check-type input (vector (unsigned-byte 8)))
  (when (decompress-context-finished-p ctx)
    (error 'err:stream-state-error
           :operation :decompress-update
           :expected-state :active
           :actual-state :finished))
  (let* ((dstream (decompress-context-stream ctx))
         (input-len (length input))
         (output-size *default-buffer-size*)
         (output (make-array output-size :element-type '(unsigned-byte 8)))
         (output-chunks nil)
         (total-produced 0))
    (sb-sys:with-pinned-objects (input output)
      (ffi:with-zstd-buffer (in-buf)
        (ffi:with-zstd-buffer (out-buf)
          (ffi:set-zstd-buffer-ptr in-buf (sb-sys:vector-sap input))
          (ffi:set-zstd-buffer-size in-buf input-len)
          (ffi:set-zstd-buffer-pos in-buf 0)
          ;; Process all input
          (loop while (< (ffi:zstd-buffer-pos in-buf) input-len)
                do (ffi:set-zstd-buffer-ptr out-buf (sb-sys:vector-sap output))
                   (ffi:set-zstd-buffer-size out-buf output-size)
                   (ffi:set-zstd-buffer-pos out-buf 0)
                   (let ((result (ffi:%dstream-decompress dstream out-buf in-buf)))
                     (check-zstd-result result :decompress-stream)
                     ;; Collect output
                     (let ((produced (ffi:zstd-buffer-pos out-buf)))
                       (incf total-produced produced)
                       ;; Check for decompression bomb
                       (when (and (> input-len 0)
                                  (> total-produced 0)
                                  (> (/ total-produced input-len) *max-compression-ratio*))
                         (err:signal-decompression-bomb input-len total-produced))
                       (when (> produced 0)
                         (push (subseq output 0 produced) output-chunks)))
                     ;; result=0 means frame complete
                     (when (zerop result)
                       (setf (decompress-context-finished-p ctx) t)
                       (return))))
          ;; Update totals
          (incf (decompress-context-total-in ctx) (ffi:zstd-buffer-pos in-buf))
          (incf (decompress-context-total-out ctx) total-produced))))
    ;; Concatenate output chunks
    (if (null output-chunks)
        (make-array 0 :element-type '(unsigned-byte 8))
        (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse output-chunks)))))

(defun decompress-finished-p (ctx)
  "Check if decompression is complete."
  (decompress-context-finished-p ctx))

(defun decompress-close (ctx)
  "Release decompressor resources."
  (when (and ctx (decompress-context-stream ctx))
    (ffi:%dstream-free (decompress-context-stream ctx))
    (setf (decompress-context-stream ctx) nil)))

(defmacro with-decompressor ((var) &body body)
  "Execute body with decompressor context, ensure cleanup."
  `(let ((,var (make-decompressor)))
     (unwind-protect
          (progn ,@body)
       (decompress-close ,var))))

;;; Utility Functions

(defun zstd-version ()
  "Return zstd version string."
  (ffi:ensure-zstd-loaded)
  (ffi:%version-string))
