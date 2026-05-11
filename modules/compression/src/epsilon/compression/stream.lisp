;;;; Compression Stream Wrappers
;;;;
;;;; Provides Reader/Writer wrappers for streaming compression that
;;;; integrate with the epsilon.io protocol system.
;;;;
;;;; These streams allow transparent compression/decompression when
;;;; reading from or writing to other streams.

(defpackage epsilon.compression.stream
  (:use :cl :epsilon.syntax)
  (:import (epsilon.compression.zlib.api zlib)
            (epsilon.compression.ffi ffi)
            (epsilon.compression.errors err)
            (epsilon.io io)))

;;; Default buffer size for stream operations
(defparameter *stream-buffer-size* 65536
  "Default buffer size for compression stream operations (64KB)")

;;; ---------------------------------------------------------------------------
;;; Deflate Reader (Decompressing Reader)
;;;
;;; Wraps a reader of compressed data and provides decompressed output.
;;; Useful for reading compressed files or network streams.
;;; ---------------------------------------------------------------------------

(defstruct (deflate-reader (:constructor %make-deflate-reader))
  "A reader that decompresses data from an underlying source.
   Implements the Reader protocol for transparent decompression."
  (source nil)
  (context nil :type (or null zlib:inflate-context))
  (input-buffer nil :type (or null (vector (unsigned-byte 8))))
  (input-pos 0 :type fixnum)
  (input-end 0 :type fixnum)
  (output-buffer nil :type (or null (vector (unsigned-byte 8))))
  (output-pos 0 :type fixnum)
  (output-end 0 :type fixnum)
  (eof-p nil :type boolean)
  (window-bits ffi:+auto-wbits+ :type integer))

(defun make-deflate-reader (source &key (window-bits ffi:+auto-wbits+)
                                        (buffer-size *stream-buffer-size*))
  "Create a decompressing reader wrapping SOURCE.
   SOURCE: underlying reader providing compressed data
   WINDOW-BITS: format detection mode
     +auto-wbits+ (47): auto-detect gzip or zlib
     +gzip-wbits+ (31): expect gzip format
     +max-wbits+ (15): expect zlib format
     +raw-wbits+ (-15): expect raw deflate"
  (%make-deflate-reader
   :source source
   :context (zlib:make-inflater :window-bits window-bits)
   :input-buffer (make-array buffer-size :element-type '(unsigned-byte 8))
   :output-buffer (make-array buffer-size :element-type '(unsigned-byte 8))
   :window-bits window-bits))

(defun deflate-reader-fill-input (reader)
  "Fill the input buffer from the source"
  (let* ((buf (deflate-reader-input-buffer reader))
         (source (deflate-reader-source reader))
         (bytes-read (io:read-into source buf)))
    (setf (deflate-reader-input-pos reader) 0)
    (setf (deflate-reader-input-end reader) bytes-read)
    bytes-read))

(defun deflate-reader-decompress-chunk (reader)
  "Decompress a chunk from input buffer to output buffer"
  (let* ((ctx (deflate-reader-context reader))
         (input-buf (deflate-reader-input-buffer reader))
         (input-pos (deflate-reader-input-pos reader))
         (input-end (deflate-reader-input-end reader))
         (input-len (- input-end input-pos)))
    (when (zerop input-len)
      ;; Need more input
      (let ((bytes (deflate-reader-fill-input reader)))
        (when (zerop bytes)
          ;; No more input, check if we're done
          (setf (deflate-reader-eof-p reader) t)
          (return-from deflate-reader-decompress-chunk 0))
        (setf input-pos 0)
        (setf input-len bytes)))
    ;; Decompress available input
    (let* ((input-chunk (subseq input-buf input-pos input-end))
           (output (zlib:inflate-update ctx input-chunk)))
      ;; Update input position (all consumed by inflate)
      (setf (deflate-reader-input-pos reader) input-end)
      ;; Store output
      (when (> (length output) 0)
        (let ((out-buf (deflate-reader-output-buffer reader)))
          ;; Copy to output buffer
          (replace out-buf output)
          (setf (deflate-reader-output-pos reader) 0)
          (setf (deflate-reader-output-end reader) (length output))))
      ;; Check if stream ended
      (when (zlib:inflate-finished-p ctx)
        (setf (deflate-reader-eof-p reader) t))
      (length output))))

(defmethod io:read-into ((reader deflate-reader) buffer &key (start 0) end)
  "Read decompressed bytes into BUFFER"
  (let* ((end (or end (length buffer)))
         (total-read 0)
         (dest-pos start))
    ;; First, use any buffered output
    (let ((out-pos (deflate-reader-output-pos reader))
          (out-end (deflate-reader-output-end reader)))
      (when (< out-pos out-end)
        (let* ((available (- out-end out-pos))
               (needed (- end dest-pos))
               (to-copy (min available needed))
               (out-buf (deflate-reader-output-buffer reader)))
          (replace buffer out-buf
                   :start1 dest-pos
                   :end1 (+ dest-pos to-copy)
                   :start2 out-pos
                   :end2 (+ out-pos to-copy))
          (incf (deflate-reader-output-pos reader) to-copy)
          (incf dest-pos to-copy)
          (incf total-read to-copy))))
    ;; Decompress more if needed
    (loop while (and (< dest-pos end)
                     (not (deflate-reader-eof-p reader)))
          do (let ((decompressed (deflate-reader-decompress-chunk reader)))
               (when (zerop decompressed)
                 (return))
               ;; Copy new output to user buffer
               (let* ((out-pos (deflate-reader-output-pos reader))
                      (out-end (deflate-reader-output-end reader))
                      (available (- out-end out-pos))
                      (needed (- end dest-pos))
                      (to-copy (min available needed))
                      (out-buf (deflate-reader-output-buffer reader)))
                 (replace buffer out-buf
                          :start1 dest-pos
                          :end1 (+ dest-pos to-copy)
                          :start2 out-pos
                          :end2 (+ out-pos to-copy))
                 (incf (deflate-reader-output-pos reader) to-copy)
                 (incf dest-pos to-copy)
                 (incf total-read to-copy))))
    total-read))

(defmethod io:close* ((reader deflate-reader))
  "Close the deflate reader and release resources"
  (when (deflate-reader-context reader)
    (zlib:inflate-close (deflate-reader-context reader))
    (setf (deflate-reader-context reader) nil))
  ;; Optionally close the source
  (when (deflate-reader-source reader)
    (io:close* (deflate-reader-source reader))))

(defmacro with-deflate-reader ((var source &rest args) &body body)
  "Execute BODY with a deflate reader, ensuring cleanup"
  `(let ((,var (make-deflate-reader ,source ,@args)))
     (unwind-protect
          (progn ,@body)
       (io:close* ,var))))

;;; ---------------------------------------------------------------------------
;;; Deflate Writer (Compressing Writer)
;;;
;;; Wraps a writer and compresses data before writing.
;;; Useful for writing compressed files or network streams.
;;; ---------------------------------------------------------------------------

(defstruct (deflate-writer (:constructor %make-deflate-writer))
  "A writer that compresses data before writing to an underlying sink.
   Implements the Writer protocol for transparent compression."
  (sink nil)
  (context nil :type (or null zlib:deflate-context))
  (input-buffer nil :type (or null (vector (unsigned-byte 8))))
  (input-pos 0 :type fixnum)
  (level ffi:+z-default-compression+ :type integer)
  (window-bits ffi:+max-wbits+ :type integer)
  (closed-p nil :type boolean))

(defun make-deflate-writer (sink &key (level ffi:+z-default-compression+)
                                      (window-bits ffi:+max-wbits+)
                                      (buffer-size *stream-buffer-size*))
  "Create a compressing writer wrapping SINK.
   SINK: underlying writer for compressed output
   LEVEL: compression level (0-9, or -1 for default)
   WINDOW-BITS: format mode
     +max-wbits+ (15): zlib format
     +gzip-wbits+ (31): gzip format
     +raw-wbits+ (-15): raw deflate"
  (%make-deflate-writer
   :sink sink
   :context (zlib:make-deflater :level level :window-bits window-bits)
   :input-buffer (make-array buffer-size :element-type '(unsigned-byte 8))
   :level level
   :window-bits window-bits))

(defun deflate-writer-flush-input (writer flush-mode)
  "Compress and write accumulated input"
  (let* ((ctx (deflate-writer-context writer))
         (buf (deflate-writer-input-buffer writer))
         (pos (deflate-writer-input-pos writer))
         (sink (deflate-writer-sink writer)))
    (when (or (> pos 0) (= flush-mode ffi:+z-finish+))
      (let* ((input (subseq buf 0 pos))
             (output (zlib:deflate-update ctx input :flush flush-mode)))
        (setf (deflate-writer-input-pos writer) 0)
        (when (> (length output) 0)
          (io:write-from sink output))))))

(defmethod io:write-from ((writer deflate-writer) buffer &key (start 0) end)
  "Write bytes to be compressed"
  (when (deflate-writer-closed-p writer)
    (error "Cannot write to closed deflate-writer"))
  (let* ((end (or end (length buffer)))
         (total-written 0)
         (src-pos start)
         (input-buf (deflate-writer-input-buffer writer))
         (buf-size (length input-buf)))
    (loop while (< src-pos end)
          do (let* ((space (- buf-size (deflate-writer-input-pos writer)))
                    (to-copy (min space (- end src-pos))))
               ;; Copy to input buffer
               (replace input-buf buffer
                        :start1 (deflate-writer-input-pos writer)
                        :start2 src-pos
                        :end2 (+ src-pos to-copy))
               (incf (deflate-writer-input-pos writer) to-copy)
               (incf src-pos to-copy)
               (incf total-written to-copy)
               ;; Flush if buffer is full
               (when (= (deflate-writer-input-pos writer) buf-size)
                 (deflate-writer-flush-input writer ffi:+z-no-flush+))))
    total-written))

(defmethod io:flush ((writer deflate-writer))
  "Flush compression state and underlying writer"
  (unless (deflate-writer-closed-p writer)
    ;; Flush with sync to ensure all data is written
    (deflate-writer-flush-input writer ffi:+z-sync-flush+)
    ;; Flush underlying sink
    (io:flush (deflate-writer-sink writer))))

(defmethod io:close* ((writer deflate-writer))
  "Finalize compression and close"
  (unless (deflate-writer-closed-p writer)
    ;; Finish compression
    (deflate-writer-flush-input writer ffi:+z-finish+)
    (let ((final (zlib:deflate-finish (deflate-writer-context writer))))
      (when (> (length final) 0)
        (io:write-from (deflate-writer-sink writer) final)))
    ;; Cleanup context
    (zlib:deflate-close (deflate-writer-context writer))
    (setf (deflate-writer-context writer) nil)
    ;; Close sink
    (io:close* (deflate-writer-sink writer))
    (setf (deflate-writer-closed-p writer) t)))

(defmacro with-deflate-writer ((var sink &rest args) &body body)
  "Execute BODY with a deflate writer, ensuring cleanup"
  `(let ((,var (make-deflate-writer ,sink ,@args)))
     (unwind-protect
          (progn ,@body)
       (io:close* ,var))))

;;; ---------------------------------------------------------------------------
;;; Inflate Reader/Writer Aliases
;;;
;;; For consistency, provide inflate-* as aliases since inflate is the
;;; inverse of deflate (decompression).
;;; ---------------------------------------------------------------------------

(defun make-inflate-reader (source &rest args)
  "Alias for make-deflate-reader (decompressing reader)"
  (apply #'make-deflate-reader source args))

(defmacro with-inflate-reader ((var source &rest args) &body body)
  "Alias for with-deflate-reader"
  `(with-deflate-reader (,var ,source ,@args) ,@body))

;;; ---------------------------------------------------------------------------
;;; Convenience Functions
;;; ---------------------------------------------------------------------------

(defun compress-stream (input output &key (level ffi:+z-default-compression+)
                                          (window-bits ffi:+max-wbits+)
                                          (buffer-size *stream-buffer-size*))
  "Compress from INPUT reader to OUTPUT writer"
  (with-deflate-writer (w output :level level :window-bits window-bits)
    (let ((buf (make-array buffer-size :element-type '(unsigned-byte 8))))
      (loop for bytes-read = (io:read-into input buf)
            while (> bytes-read 0)
            do (io:write-from w buf :end bytes-read)))))

(defun decompress-stream (input output &key (window-bits ffi:+auto-wbits+)
                                            (buffer-size *stream-buffer-size*))
  "Decompress from INPUT reader to OUTPUT writer"
  (with-deflate-reader (r input :window-bits window-bits)
    (let ((buf (make-array buffer-size :element-type '(unsigned-byte 8))))
      (loop for bytes-read = (io:read-into r buf)
            while (> bytes-read 0)
            do (io:write-from output buf :end bytes-read)))))
