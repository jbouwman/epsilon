;;;; High-Level Zlib API
;;;;
;;;; Provides a clean Lisp interface over the zlib FFI bindings.
;;;; Includes one-shot compression/decompression and streaming APIs.

(defpackage epsilon.compression.zlib.api
  (:use :cl :epsilon.syntax)
  (:import (epsilon.compression.ffi ffi)
            (epsilon.compression.errors err)
            (epsilon.foreign foreign))
  (:export +max-wbits+
           +gzip-wbits+
           +raw-wbits+
           +auto-wbits+
           +z-default-compression+
           +z-no-compression+
           +z-best-speed+
           +z-best-compression+
           +z-no-flush+
           +z-sync-flush+
           +z-full-flush+
           +z-finish+
           +z-default-strategy+
           +z-filtered+
           +z-huffman-only+
           +z-rle+
           +z-fixed+
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
           zlib-version))

;;; Safety limits for decompression

(defparameter *max-compression-ratio* 1000
  "Maximum allowed compression ratio (uncompressed/compressed) to prevent bombs")

(defparameter *max-uncompressed-size* (* 1024 1024 1024)  ; 1GB
  "Maximum allowed uncompressed size in bytes")

(defparameter *default-buffer-size* 65536
  "Default buffer size for streaming operations (64KB)")

;;; One-shot Compression

(defun compress (data &key (level ffi:+z-default-compression+))
  "Compress DATA (byte vector) and return compressed bytes.
   LEVEL: compression level 0-9, or -1 for default
     0 = no compression
     1 = best speed
     9 = best compression
     -1 = default (typically 6)"
  (check-type data (vector (unsigned-byte 8)))
  (check-type level (integer -1 9))
  (let* ((source-len (length data))
         (dest-len (ffi:%compress-bound source-len))
         (dest (make-array dest-len :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (data dest)
      (let ((source-ptr (sb-sys:vector-sap data))
            (dest-ptr (sb-sys:vector-sap dest)))
        (foreign:with-foreign-memory ((dest-len-ptr :unsigned-long))
          (setf (sb-sys:sap-ref-64 dest-len-ptr 0) dest-len)
          (let ((result (ffi:%compress2 dest-ptr dest-len-ptr
                                        source-ptr source-len level)))
            (unless (= result ffi:+z-ok+)
              (err:signal-zlib-error :compress result))
            ;; Return only the actual compressed data
            (let ((actual-len (sb-sys:sap-ref-64 dest-len-ptr 0)))
              (subseq dest 0 actual-len))))))))

(defun decompress (data &key max-output-size)
  "Decompress DATA (byte vector) and return decompressed bytes.
   MAX-OUTPUT-SIZE: if specified, limits output size (prevents decompression bombs)"
  (check-type data (vector (unsigned-byte 8)))
  (let* ((source-len (length data))
         ;; Start with 4x the input size, will grow if needed
         (initial-size (min (max (* source-len 4) 1024)
                            (or max-output-size *max-uncompressed-size*)))
         (max-size (or max-output-size *max-uncompressed-size*)))
    ;; Try decompression with increasing buffer sizes
    (loop with dest-size = initial-size
          do (let ((dest (make-array dest-size :element-type '(unsigned-byte 8))))
               (sb-sys:with-pinned-objects (data dest)
                 (let ((source-ptr (sb-sys:vector-sap data))
                       (dest-ptr (sb-sys:vector-sap dest)))
                   (foreign:with-foreign-memory ((dest-len-ptr :unsigned-long))
                     (setf (sb-sys:sap-ref-64 dest-len-ptr 0) dest-size)
                     (let ((result (ffi:%uncompress dest-ptr dest-len-ptr
                                                    source-ptr source-len)))
                       (cond
                         ;; Success
                         ((= result ffi:+z-ok+)
                          (let ((actual-len (sb-sys:sap-ref-64 dest-len-ptr 0)))
                            ;; Check for decompression bomb
                            (when (and (> source-len 0)
                                       (> (/ actual-len source-len)
                                          *max-compression-ratio*))
                              (err:signal-decompression-bomb source-len actual-len))
                            (return (subseq dest 0 actual-len))))
                         ;; Buffer too small, try larger
                         ((= result ffi:+z-buf-error+)
                          (setf dest-size (* dest-size 2))
                          (when (> dest-size max-size)
                            (err:signal-buffer-overflow :decompress dest-size max-size)))
                         ;; Data error
                         ((= result ffi:+z-data-error+)
                          (err:signal-invalid-data :decompress))
                         ;; Other error
                         (t
                          (err:signal-zlib-error :decompress result)))))))))))

(defun compress-bound (source-length)
  "Return upper bound on compressed size for SOURCE-LENGTH bytes"
  (ffi:%compress-bound source-length))

;;; Streaming Compression

(defstruct deflate-context
  "Context for streaming deflate compression"
  (stream nil :type (or null sb-sys:system-area-pointer))
  (level ffi:+z-default-compression+ :type integer)
  (window-bits ffi:+max-wbits+ :type integer)
  (finalized-p nil :type boolean)
  (total-in 0 :type integer)
  (total-out 0 :type integer))

(defun make-deflater (&key (level ffi:+z-default-compression+)
                        (window-bits ffi:+max-wbits+)
                        (mem-level 8)
                        (strategy ffi:+z-default-strategy+))
  "Create a streaming deflate compressor.
   WINDOW-BITS:
     8-15: raw deflate
     24-31 (add 16): gzip format
     -8 to -15: raw deflate without header
   MEM-LEVEL: 1-9, memory usage vs compression ratio
   STRATEGY: compression strategy"
  (let* ((z-stream (ffi:make-z-stream))
         (result (ffi:%deflate-init2 z-stream level 8 ; method is always 8 (deflate)
                                     window-bits mem-level strategy)))
    (unless (= result ffi:+z-ok+)
      (ffi:free-z-stream z-stream)
      (err:signal-zlib-error :deflate-init result))
    (make-deflate-context :stream z-stream
                          :level level
                          :window-bits window-bits)))

(defun deflate-update (ctx input &key (flush ffi:+z-no-flush+))
  "Feed INPUT bytes to deflater, return compressed output.
   FLUSH: Z_NO_FLUSH, Z_SYNC_FLUSH, Z_FULL_FLUSH, or Z_FINISH"
  (check-type ctx deflate-context)
  (check-type input (vector (unsigned-byte 8)))
  (when (deflate-context-finalized-p ctx)
    (error 'err:stream-state-error
           :operation :deflate-update
           :expected-state :active
           :actual-state :finalized))
  (let* ((z-stream (deflate-context-stream ctx))
         (input-len (length input))
         (output-size (max (compress-bound input-len) *default-buffer-size*))
         (output (make-array output-size :element-type '(unsigned-byte 8)))
         (output-chunks nil))
    (sb-sys:with-pinned-objects (input output)
      (ffi:set-z-stream-next-in z-stream (sb-sys:vector-sap input))
      (ffi:set-z-stream-avail-in z-stream input-len)
      ;; Process all input
      (loop while (or (> (ffi:z-stream-avail-in z-stream) 0)
                      (= flush ffi:+z-finish+))
            do (ffi:set-z-stream-next-out z-stream (sb-sys:vector-sap output))
            (ffi:set-z-stream-avail-out z-stream output-size)
            (let ((result (ffi:%deflate z-stream flush)))
              (cond
                ((or (= result ffi:+z-ok+)
                     (= result ffi:+z-stream-end+))
                 ;; Collect output
                 (let ((produced (- output-size (ffi:z-stream-avail-out z-stream))))
                   (when (> produced 0)
                     (push (subseq output 0 produced) output-chunks)))
                 ;; If stream ended, mark as finalized
                 (when (= result ffi:+z-stream-end+)
                   (setf (deflate-context-finalized-p ctx) t)
                   (return)))
                (t
                 (err:signal-zlib-error :deflate result))))
            ;; If no more input and not finishing, exit
            when (and (zerop (ffi:z-stream-avail-in z-stream))
                      (/= flush ffi:+z-finish+))
            do (return))
      ;; Update totals
      (setf (deflate-context-total-in ctx) (ffi:z-stream-total-in z-stream))
      (setf (deflate-context-total-out ctx) (ffi:z-stream-total-out z-stream)))
    ;; Concatenate output chunks
    (if (null output-chunks)
        (make-array 0 :element-type '(unsigned-byte 8))
        (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse output-chunks)))))

(defun deflate-finish (ctx)
  "Finish compression and return final output"
  (deflate-update ctx (make-array 0 :element-type '(unsigned-byte 8))
    :flush ffi:+z-finish+))

(defun deflate-close (ctx)
  "Release deflate context resources"
  (when (and ctx (deflate-context-stream ctx))
    (ffi:%deflate-end (deflate-context-stream ctx))
    (ffi:free-z-stream (deflate-context-stream ctx))
    (setf (deflate-context-stream ctx) nil)))

(defmacro with-deflater ((var &rest args) &body body)
  "Execute body with deflater context, ensure cleanup"
  `(let ((,var (make-deflater ,@args)))
     (unwind-protect
          (progn ,@body)
       (deflate-close ,var))))

;;; Streaming Decompression

(defstruct inflate-context
  "Context for streaming inflate decompression"
  (stream nil :type (or null sb-sys:system-area-pointer))
  (window-bits ffi:+max-wbits+ :type integer)
  (finished-p nil :type boolean)
  (total-in 0 :type integer)
  (total-out 0 :type integer))

(defun make-inflater (&key (window-bits ffi:+max-wbits+))
  "Create a streaming inflate decompressor.
   WINDOW-BITS:
     8-15: raw inflate
     24-31 (add 16): gzip format
     40-47 (add 32): auto-detect format
     -8 to -15: raw inflate without header"
  (let* ((z-stream (ffi:make-z-stream))
         (result (ffi:%inflate-init2 z-stream window-bits)))
    (unless (= result ffi:+z-ok+)
      (ffi:free-z-stream z-stream)
      (err:signal-zlib-error :inflate-init result))
    (make-inflate-context :stream z-stream
                          :window-bits window-bits)))

(defun inflate-update (ctx input)
  "Feed INPUT bytes to inflater, return decompressed output"
  (check-type ctx inflate-context)
  (check-type input (vector (unsigned-byte 8)))
  (when (inflate-context-finished-p ctx)
    (error 'err:stream-state-error
           :operation :inflate-update
           :expected-state :active
           :actual-state :finished))
  (let* ((z-stream (inflate-context-stream ctx))
         (input-len (length input))
         (output-size *default-buffer-size*)
         (output (make-array output-size :element-type '(unsigned-byte 8)))
         (output-chunks nil)
         (total-produced 0))
    (sb-sys:with-pinned-objects (input output)
      (ffi:set-z-stream-next-in z-stream (sb-sys:vector-sap input))
      (ffi:set-z-stream-avail-in z-stream input-len)
      ;; Process all input
      (loop while (> (ffi:z-stream-avail-in z-stream) 0)
            do (ffi:set-z-stream-next-out z-stream (sb-sys:vector-sap output))
            (ffi:set-z-stream-avail-out z-stream output-size)
            (let ((result (ffi:%inflate z-stream ffi:+z-no-flush+)))
              (cond
                ((or (= result ffi:+z-ok+)
                     (= result ffi:+z-stream-end+))
                 ;; Collect output
                 (let ((produced (- output-size (ffi:z-stream-avail-out z-stream))))
                   (incf total-produced produced)
                   ;; Safety check for decompression bomb
                   (when (and (> input-len 0)
                              (> total-produced 0)
                              (> (/ total-produced input-len) *max-compression-ratio*))
                     (err:signal-decompression-bomb input-len total-produced))
                   (when (> produced 0)
                     (push (subseq output 0 produced) output-chunks)))
                 ;; If stream ended, mark as finished
                 (when (= result ffi:+z-stream-end+)
                   (setf (inflate-context-finished-p ctx) t)
                   (return)))
                ((= result ffi:+z-data-error+)
                 (err:signal-invalid-data :inflate))
                (t
                 (err:signal-zlib-error :inflate result)))))
      ;; Update totals
      (setf (inflate-context-total-in ctx) (ffi:z-stream-total-in z-stream))
      (setf (inflate-context-total-out ctx) (ffi:z-stream-total-out z-stream)))
    ;; Concatenate output chunks
    (if (null output-chunks)
        (make-array 0 :element-type '(unsigned-byte 8))
        (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse output-chunks)))))

(defun inflate-finished-p (ctx)
  "Check if decompression is complete"
  (inflate-context-finished-p ctx))

(defun inflate-close (ctx)
  "Release inflate context resources"
  (when (and ctx (inflate-context-stream ctx))
    (ffi:%inflate-end (inflate-context-stream ctx))
    (ffi:free-z-stream (inflate-context-stream ctx))
    (setf (inflate-context-stream ctx) nil)))

(defmacro with-inflater ((var &rest args) &body body)
  "Execute body with inflater context, ensure cleanup"
  `(let ((,var (make-inflater ,@args)))
     (unwind-protect
          (progn ,@body)
       (inflate-close ,var))))

;;; Checksum Functions

(defun crc32 (data &optional (initial 0))
  "Calculate CRC-32 checksum of DATA, optionally continuing from INITIAL"
  (check-type data (vector (unsigned-byte 8)))
  (sb-sys:with-pinned-objects (data)
    (ffi:%crc32 initial (sb-sys:vector-sap data) (length data))))

(defun adler32 (data &optional (initial 1))
  "Calculate Adler-32 checksum of DATA, optionally continuing from INITIAL"
  (check-type data (vector (unsigned-byte 8)))
  (sb-sys:with-pinned-objects (data)
    (ffi:%adler32 initial (sb-sys:vector-sap data) (length data))))

;;; Utility Functions

(defun zlib-version ()
  "Return zlib version string"
  (ffi:%zlib-version))
