;;;; epsilon.digest.io - epsilon.io stream integration
;;;;
;;;; Provides seamless integration with epsilon.io streams:
;;;; - Hash readers/writers (tee-style)
;;;; - Stream hashing utilities
;;;; - Buffer hashing

(defpackage epsilon.digest.io
  (:use :cl)
  (:require (epsilon.digest.protocol proto)
            (epsilon.io.protocol io-proto)
            (epsilon.io.buffer buf)
            (epsilon.ssl ssl))
  (:enter t))

;;; ============================================================================
;;; Hashing Reader - Hash data as it is read
;;; ============================================================================

(defstruct (hashing-reader (:constructor %make-hashing-reader))
  "A reader that hashes all data as it passes through.

   Data read from SOURCE is automatically fed to HASHER.
   After reading is complete, call (hasher-finalize hasher) to get the hash."
  (source nil)
  (hasher nil))

(defun make-hashing-reader (source hasher)
  "Create a reader that hashes all data as it is read.

   SOURCE: the underlying reader
   HASHER: a hasher object (e.g., from make-blake3-hasher)

   Example:
     (let* ((h (make-blake3-hasher))
            (hr (make-hashing-reader file-reader h)))
       (read-all hr)
       (hasher-finalize h))"
  (%make-hashing-reader :source source :hasher hasher))

(defmethod io-proto:read-into ((reader hashing-reader) buffer &key (start 0) (end (length buffer)))
  "Read from source into buffer, updating hasher with the data."
  (let ((n (io-proto:read-into (hashing-reader-source reader) buffer
                               :start start :end end)))
    (when (plusp n)
      (proto:hasher-update (hashing-reader-hasher reader) buffer
                           :start start :end (+ start n)))
    n))

(defmethod io-proto:close* ((reader hashing-reader))
  "Close the underlying source reader."
  (io-proto:close* (hashing-reader-source reader)))

(defmethod io-proto:open-p ((reader hashing-reader))
  (io-proto:open-p (hashing-reader-source reader)))

;;; ============================================================================
;;; Hashing Writer - Hash data as it is written
;;; ============================================================================

(defstruct (hashing-writer (:constructor %make-hashing-writer))
  "A writer that hashes all data as it passes through.

   Data written to SINK is automatically fed to HASHER.
   After writing is complete, call (hasher-finalize hasher) to get the hash."
  (sink nil)
  (hasher nil))

(defun make-hashing-writer (sink hasher)
  "Create a writer that hashes all data as it is written.

   SINK: the underlying writer
   HASHER: a hasher object

   Example:
     (let* ((h (make-xxhash64-hasher))
            (hw (make-hashing-writer file-writer h)))
       (write-all hw data)
       (flush hw)
       (hasher-finalize h))"
  (%make-hashing-writer :sink sink :hasher hasher))

(defmethod io-proto:write-from ((writer hashing-writer) buffer &key (start 0) (end (length buffer)))
  "Write buffer to sink, updating hasher with the data."
  (let ((n (io-proto:write-from (hashing-writer-sink writer) buffer
                                :start start :end end)))
    (when (plusp n)
      (proto:hasher-update (hashing-writer-hasher writer) buffer
                           :start start :end (+ start n)))
    n))

(defmethod io-proto:flush ((writer hashing-writer))
  "Flush the underlying sink."
  (io-proto:flush (hashing-writer-sink writer)))

(defmethod io-proto:close* ((writer hashing-writer))
  "Close the underlying sink writer."
  (io-proto:close* (hashing-writer-sink writer)))

(defmethod io-proto:open-p ((writer hashing-writer))
  (io-proto:open-p (hashing-writer-sink writer)))

;;; ============================================================================
;;; Stream Hashing Utilities
;;; ============================================================================

(defconstant +default-hash-buffer-size+ 8192
  "Default buffer size for stream hashing.")

(defun hash-reader (reader hasher &key (buffer-size +default-hash-buffer-size+))
  "Hash all data from READER using HASHER.
   Returns the hasher (call finalize to get the digest)."
  (let ((buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop
      (let ((n (io-proto:read-into reader buffer)))
        (when (zerop n)
          (return hasher))
        (proto:hasher-update hasher buffer :start 0 :end n)))))

(defun hash-reader-blake3 (reader &key (output-length 32) (buffer-size +default-hash-buffer-size+))
  "Hash all data from READER using BLAKE3.
   Returns the hash digest."
  ;; Import at runtime to avoid circular dependency
  (let* ((blake3-pkg (find-package "EPSILON.DIGEST.BLAKE3"))
         (make-hasher (symbol-function (intern "MAKE-BLAKE3-HASHER" blake3-pkg)))
         (hasher (funcall make-hasher)))
    (hash-reader reader hasher :buffer-size buffer-size)
    (proto:hasher-finalize hasher :output-length output-length)))

(defun hash-reader-xxhash64 (reader &key (seed 0) (buffer-size +default-hash-buffer-size+))
  "Hash all data from READER using xxHash3 64-bit.
   Returns the hash as an unsigned 64-bit integer."
  (let* ((xxhash-pkg (find-package "EPSILON.DIGEST.XXHASH3"))
         (make-hasher (symbol-function (intern "MAKE-XXHASH64-HASHER" xxhash-pkg)))
         (hasher (funcall make-hasher :seed seed)))
    (hash-reader reader hasher :buffer-size buffer-size)
    (proto:hasher-finalize hasher)))

(defun hash-reader-xxhash128 (reader &key (seed 0) (buffer-size +default-hash-buffer-size+))
  "Hash all data from READER using xxHash3 128-bit.
   Returns (values low64 high64)."
  (let* ((xxhash-pkg (find-package "EPSILON.DIGEST.XXHASH3"))
         (make-hasher (symbol-function (intern "MAKE-XXHASH128-HASHER" xxhash-pkg)))
         (hasher (funcall make-hasher :seed seed)))
    (hash-reader reader hasher :buffer-size buffer-size)
    (proto:hasher-finalize hasher)))

(defun hash-reader-md5 (reader &key (buffer-size +default-hash-buffer-size+))
  "Hash all data from READER using MD5.
   Returns 16-byte hash digest.
   WARNING: MD5 is cryptographically broken."
  (let ((state (ssl:make-md5-state))
        (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop (let ((n (io-proto:read-into reader buffer)))
            (when (zerop n)
              (return (ssl:md5-finalize state)))
            (ssl:md5-update state buffer :start 0 :end n)))))

(defun hash-reader-sha256 (reader &key (buffer-size +default-hash-buffer-size+))
  "Hash all data from READER using SHA-256.
   Returns 32-byte hash digest."
  (let ((state (ssl:make-sha256-state))
        (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop (let ((n (io-proto:read-into reader buffer)))
            (when (zerop n)
              (return (ssl:sha256-finalize state)))
            (ssl:sha256-update state buffer :start 0 :end n)))))

(defun hash-reader-sha512 (reader &key (buffer-size +default-hash-buffer-size+))
  "Hash all data from READER using SHA-512.
   Returns 64-byte hash digest."
  (let ((state (ssl:make-sha512-state))
        (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop (let ((n (io-proto:read-into reader buffer)))
            (when (zerop n)
              (return (ssl:sha512-finalize state)))
            (ssl:sha512-update state buffer :start 0 :end n)))))

;;; ============================================================================
;;; Buffer Hashing Utilities
;;; ============================================================================

(defun hash-buffer (buffer hasher)
  "Hash the readable content of BUFFER using HASHER.
   Returns the hasher (call finalize to get the digest)."
  (let* ((data (buf:buf-data buffer))
         (start (buf:buf-position buffer))
         (end (buf:buf-limit buffer)))
    (proto:hasher-update hasher data :start start :end end)
    hasher))

(defun hash-buffer-blake3 (buffer &key (output-length 32))
  "Hash the readable content of BUFFER using BLAKE3.
   Returns the hash digest."
  (let* ((blake3-pkg (find-package "EPSILON.DIGEST.BLAKE3"))
         (blake3-fn (symbol-function (intern "BLAKE3" blake3-pkg)))
         (data (buf:buf-data buffer))
         (start (buf:buf-position buffer))
         (end (buf:buf-limit buffer)))
    (funcall blake3-fn data :start start :end end :output-length output-length)))

(defun hash-buffer-xxhash64 (buffer &key (seed 0))
  "Hash the readable content of BUFFER using xxHash3 64-bit.
   Returns the hash as an unsigned 64-bit integer."
  (let* ((xxhash-pkg (find-package "EPSILON.DIGEST.XXHASH3"))
         (xxhash-fn (symbol-function (intern "XXHASH64" xxhash-pkg)))
         (data (buf:buf-data buffer))
         (start (buf:buf-position buffer))
         (end (buf:buf-limit buffer)))
    (funcall xxhash-fn data :start start :end end :seed seed)))

(defun hash-buffer-md5 (buffer)
  "Hash the readable content of BUFFER using MD5.
   Returns 16-byte hash digest.
   WARNING: MD5 is cryptographically broken."
  (let ((data (buf:buf-data buffer))
        (start (buf:buf-position buffer))
        (end (buf:buf-limit buffer)))
    (ssl:md5 data :start start :end end)))

(defun hash-buffer-sha256 (buffer)
  "Hash the readable content of BUFFER using SHA-256.
   Returns 32-byte hash digest."
  (let ((data (buf:buf-data buffer))
        (start (buf:buf-position buffer))
        (end (buf:buf-limit buffer)))
    (ssl:sha256 data :start start :end end)))

;;; ============================================================================
;;; Protocol Methods for hash-reader
;;; ============================================================================

(defmethod proto:hash-reader (reader (algorithm (eql :blake3)) &key key (output-length 32) (buffer-size +default-hash-buffer-size+))
  "Hash a reader using BLAKE3."
  (let* ((blake3-pkg (find-package "EPSILON.DIGEST.BLAKE3"))
         (make-hasher (symbol-function (intern "MAKE-BLAKE3-HASHER" blake3-pkg)))
         (hasher (if key
                     (funcall make-hasher :key key)
                     (funcall make-hasher))))
    (hash-reader reader hasher :buffer-size buffer-size)
    (proto:hasher-finalize hasher :output-length output-length)))

(defmethod proto:hash-reader (reader (algorithm (eql :xxhash64)) &key key output-length (buffer-size +default-hash-buffer-size+))
  "Hash a reader using xxHash3 64-bit."
  (declare (ignore output-length))
  (hash-reader-xxhash64 reader :seed (or key 0) :buffer-size buffer-size))

(defmethod proto:hash-reader (reader (algorithm (eql :xxhash128)) &key key output-length (buffer-size +default-hash-buffer-size+))
  "Hash a reader using xxHash3 128-bit."
  (declare (ignore output-length))
  (hash-reader-xxhash128 reader :seed (or key 0) :buffer-size buffer-size))

(defmethod proto:hash-reader (reader (algorithm (eql :md5)) &key key output-length (buffer-size +default-hash-buffer-size+))
  "Hash a reader using MD5."
  (declare (ignore key output-length))
  (hash-reader-md5 reader :buffer-size buffer-size))

(defmethod proto:hash-reader (reader (algorithm (eql :sha256)) &key key output-length (buffer-size +default-hash-buffer-size+))
  "Hash a reader using SHA-256."
  (declare (ignore key output-length))
  (hash-reader-sha256 reader :buffer-size buffer-size))

(defmethod proto:hash-reader (reader (algorithm (eql :sha512)) &key key output-length (buffer-size +default-hash-buffer-size+))
  "Hash a reader using SHA-512."
  (declare (ignore key output-length))
  (hash-reader-sha512 reader :buffer-size buffer-size))

;;; ============================================================================
;;; Protocol Methods for make-hashing-reader/writer
;;; ============================================================================

(defmethod proto:make-hashing-reader (source hasher)
  "Create a hashing reader wrapper."
  (make-hashing-reader source hasher))

(defmethod proto:make-hashing-writer (sink hasher)
  "Create a hashing writer wrapper."
  (make-hashing-writer sink hasher))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-hashing-reader ((var source hasher) &body body)
  "Execute BODY with VAR bound to a hashing reader.
   The reader is closed when BODY exits."
  `(let ((,var (make-hashing-reader ,source ,hasher)))
     (unwind-protect
          (progn ,@body)
       (io-proto:close* ,var))))

(defmacro with-hashing-writer ((var sink hasher) &body body)
  "Execute BODY with VAR bound to a hashing writer.
   The writer is flushed and closed when BODY exits."
  `(let ((,var (make-hashing-writer ,sink ,hasher)))
     (unwind-protect
          (progn ,@body)
       (io-proto:flush ,var)
       (io-proto:close* ,var))))

;;; ============================================================================
;;; Dual Hashing - Hash with multiple algorithms simultaneously
;;; ============================================================================

(defstruct (multi-hashing-reader (:constructor %make-multi-hashing-reader))
  "A reader that updates multiple hashers simultaneously."
  (source nil)
  (hashers nil :type list))

(defun make-multi-hashing-reader (source &rest hashers)
  "Create a reader that updates multiple hashers as data is read."
  (%make-multi-hashing-reader :source source :hashers hashers))

(defmethod io-proto:read-into ((reader multi-hashing-reader) buffer &key (start 0) (end (length buffer)))
  "Read from source, updating all hashers."
  (let ((n (io-proto:read-into (multi-hashing-reader-source reader) buffer
                               :start start :end end)))
    (when (plusp n)
      (dolist (hasher (multi-hashing-reader-hashers reader))
        (proto:hasher-update hasher buffer :start start :end (+ start n))))
    n))

(defmethod io-proto:close* ((reader multi-hashing-reader))
  (io-proto:close* (multi-hashing-reader-source reader)))

(defmethod io-proto:open-p ((reader multi-hashing-reader))
  (io-proto:open-p (multi-hashing-reader-source reader)))
