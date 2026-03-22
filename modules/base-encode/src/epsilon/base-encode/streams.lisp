;;;; epsilon.base-encode.streams - epsilon.io Reader/Writer integration
;;;;
;;;; Provides streaming encode/decode wrappers that implement the
;;;; epsilon.io Reader and Writer protocols. An encoding-writer accepts
;;;; raw bytes and writes encoded ASCII to an underlying writer; a
;;;; decoding-reader reads encoded ASCII from an underlying reader and
;;;; yields decoded bytes.

(defpackage epsilon.base-encode.streams
  (:use :cl)
  (:require (epsilon.io.protocol proto)
            (epsilon.io.conditions cond)
            (epsilon.typeclass tc)
            (epsilon.base-encode.tables tbl)
            (epsilon.base-encode.base16 b16)
            (epsilon.base-encode.base32 b32)
            (epsilon.base-encode.base64 b64))
  (:export
   ;; Types
   #:encoding-writer
   #:decoding-reader

   ;; Base64 constructors
   #:make-base64-encoding-writer
   #:make-base64-decoding-reader

   ;; Base32 constructors
   #:make-base32-encoding-writer
   #:make-base32-decoding-reader

   ;; Base16 constructors
   #:make-base16-encoding-writer
   #:make-base16-decoding-reader)
  (:enter t))

(defun check-open (stream operation)
  "Signal closed-error if STREAM is not open."
  (unless (proto:open-p stream)
    (error 'cond:closed-error :operation operation :stream stream)))

;;; ============================================================================
;;; Encoding Writer
;;; ============================================================================

(defstruct (encoding-writer (:constructor %make-encoding-writer))
  "Writer that encodes bytes and writes encoded ASCII to an underlying sink.
   Slots: sink (proto:writer), encode-fn, flush-fn, buffer, buf-pos, group-size.

   Implements: Writer, Closer protocols."
  (sink nil)
  (encode-fn nil :type function)
  (flush-fn nil :type (or function null))
  (buffer nil :type (simple-array (unsigned-byte 8) (*)))
  (buf-pos 0 :type fixnum)
  (group-size 3 :type fixnum)
  (closed-p nil :type boolean))

(tc:definstance proto:closer encoding-writer
  (proto:close* (writer)
    (if (encoding-writer-closed-p writer)
        nil
        (progn
          ;; Flush any remaining partial group
          (when (plusp (encoding-writer-buf-pos writer))
            (let* ((flush-fn (encoding-writer-flush-fn writer))
                   (buf (encoding-writer-buffer writer))
                   (count (encoding-writer-buf-pos writer))
                   (partial (subseq buf 0 count))
                   (encoded (if flush-fn
                                (funcall flush-fn partial count)
                                (funcall (encoding-writer-encode-fn writer) partial))))
              (let ((out-bytes (sb-ext:string-to-octets encoded :external-format :ascii)))
                (proto:write-all (encoding-writer-sink writer) out-bytes))))
          (setf (encoding-writer-closed-p writer) t)
          (proto:close* (encoding-writer-sink writer))
          t)))
  (proto:open-p (writer)
    (not (encoding-writer-closed-p writer))))

(tc:definstance proto:writer encoding-writer
  (proto:write-from (writer buffer &key (start 0) (end (length buffer)))
    (check-open writer :write)
    (let ((total (- end start))
          (pos start)
          (group-size (encoding-writer-group-size writer))
          (buf (encoding-writer-buffer writer))
          (buf-pos (encoding-writer-buf-pos writer))
          (encode-fn (encoding-writer-encode-fn writer))
          (sink (encoding-writer-sink writer)))
      (declare (type fixnum total pos group-size buf-pos))
      ;; Fill accumulation buffer first
      (loop while (and (< pos end) (< buf-pos group-size))
            do (setf (aref buf buf-pos) (aref buffer pos))
               (incf buf-pos)
               (incf pos))
      ;; If buffer is full, encode and emit
      (when (= buf-pos group-size)
        (let* ((encoded (funcall encode-fn buf))
               (out-bytes (sb-ext:string-to-octets encoded :external-format :ascii)))
          (proto:write-all sink out-bytes))
        (setf buf-pos 0))
      ;; Process remaining complete groups directly from input
      (loop while (>= (- end pos) group-size)
            do (let* ((group-bytes (subseq buffer pos (+ pos group-size)))
                      (encoded (funcall encode-fn group-bytes))
                      (out-bytes (sb-ext:string-to-octets encoded :external-format :ascii)))
                 (proto:write-all sink out-bytes)
                 (incf pos group-size)))
      ;; Store any leftover in buffer
      (loop while (< pos end)
            do (setf (aref buf buf-pos) (aref buffer pos))
               (incf buf-pos)
               (incf pos))
      (setf (encoding-writer-buf-pos writer) buf-pos)
      total))
  (proto:flush (writer)
    (proto:flush (encoding-writer-sink writer))
    writer))

;;; ============================================================================
;;; Decoding Reader
;;; ============================================================================

(defstruct (decoding-reader (:constructor %make-decoding-reader))
  "Reader that reads encoded ASCII from an underlying source and yields decoded bytes.
   Slots: source (proto:reader), decode-fn, output-buf, out-pos, out-end, group-size.

   Implements: Reader, Closer protocols."
  (source nil)
  (decode-fn nil :type function)
  (output-buf nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (out-pos 0 :type fixnum)
  (out-end 0 :type fixnum)
  (group-size 4 :type fixnum)
  (eof-p nil :type boolean)
  (closed-p nil :type boolean))

(tc:definstance proto:closer decoding-reader
  (proto:close* (reader)
    (if (decoding-reader-closed-p reader)
        nil
        (progn
          (setf (decoding-reader-closed-p reader) t)
          (proto:close* (decoding-reader-source reader))
          t)))
  (proto:open-p (reader)
    (not (decoding-reader-closed-p reader))))

(defun %refill-decoding-reader (reader)
  "Read and decode the next chunk from the source.
   Returns T if data was produced, NIL on EOF."
  (when (decoding-reader-eof-p reader)
    (return-from %refill-decoding-reader nil))
  (let* ((group-size (decoding-reader-group-size reader))
         ;; Read enough encoded chars for several groups
         (read-size (* group-size 256))
         (in-buf (make-array read-size :element-type '(unsigned-byte 8)))
         (n (proto:read-into (decoding-reader-source reader) in-buf)))
    (declare (type fixnum group-size read-size n))
    (when (zerop n)
      (setf (decoding-reader-eof-p reader) t)
      (return-from %refill-decoding-reader nil))
    ;; Convert ASCII bytes to string for the decode function
    (let* ((encoded-str (sb-ext:octets-to-string (subseq in-buf 0 n) :external-format :ascii))
           (decoded (funcall (decoding-reader-decode-fn reader) encoded-str)))
      (when (plusp (length decoded))
        (setf (decoding-reader-output-buf reader)
              (if (typep decoded '(simple-array (unsigned-byte 8) (*)))
                  decoded
                  (coerce decoded '(simple-array (unsigned-byte 8) (*)))))
        (setf (decoding-reader-out-pos reader) 0)
        (setf (decoding-reader-out-end reader) (length decoded))
        t))))

(tc:definstance proto:reader decoding-reader
  (proto:read-into (reader buffer &key (start 0) (end (length buffer)))
    (check-open reader :read)
    (let ((total 0)
          (pos start))
      (declare (type fixnum total pos))
      (loop
        ;; Try to satisfy from current output buffer
        (when (and (decoding-reader-output-buf reader)
                   (< (decoding-reader-out-pos reader) (decoding-reader-out-end reader)))
          (let* ((available (- (decoding-reader-out-end reader)
                               (decoding-reader-out-pos reader)))
                 (requested (- end pos))
                 (count (min available requested)))
            (declare (type fixnum available requested count))
            (replace buffer (decoding-reader-output-buf reader)
                     :start1 pos
                     :end1 (+ pos count)
                     :start2 (decoding-reader-out-pos reader))
            (incf (decoding-reader-out-pos reader) count)
            (incf pos count)
            (incf total count)))
        ;; If request satisfied, return
        (when (>= pos end)
          (return total))
        ;; Try to refill
        (unless (%refill-decoding-reader reader)
          (return total))))))

;;; ============================================================================
;;; Base64 Stream Constructors
;;; ============================================================================

(defun make-base64-encoding-writer (sink &key (url nil) (pad t))
  "Create a writer that base64-encodes bytes written to it.
   URL selects base64url alphabet. PAD controls padding on close."
  (let ((table (if url tbl:+base64url-encode+ tbl:+base64-encode+))
        (pad-char (if (and pad (not url)) #\= (if pad #\= nil))))
    (%make-encoding-writer
     :sink sink
     :encode-fn (lambda (bytes)
                  (b64:encode bytes :table table :pad-char pad-char :columns 0))
     :flush-fn (lambda (bytes count)
                 (declare (ignore count))
                 (b64:encode bytes :table table :pad-char pad-char :columns 0))
     :buffer (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)
     :buf-pos 0
     :group-size 3)))

(defun make-base64-decoding-reader (source &key (url nil))
  "Create a reader that base64-decodes data read from SOURCE.
   URL selects base64url alphabet."
  (let ((table (if url tbl:+base64url-decode+ tbl:+base64-decode+)))
    (%make-decoding-reader
     :source source
     :decode-fn (lambda (string)
                  (b64:decode string :table table))
     :group-size 4)))

;;; ============================================================================
;;; Base32 Stream Constructors
;;; ============================================================================

(defun make-base32-encoding-writer (sink &key (table tbl:+base32-encode+) (pad t))
  "Create a writer that base32-encodes bytes written to it."
  (%make-encoding-writer
   :sink sink
   :encode-fn (lambda (bytes) (b32:encode bytes :table table :pad pad))
   :flush-fn (lambda (bytes count)
               (declare (ignore count))
               (b32:encode bytes :table table :pad pad))
   :buffer (make-array 5 :element-type '(unsigned-byte 8) :initial-element 0)
   :buf-pos 0
   :group-size 5))

(defun make-base32-decoding-reader (source &key (table tbl:+base32-decode+))
  "Create a reader that base32-decodes data read from SOURCE."
  (%make-decoding-reader
   :source source
   :decode-fn (lambda (string) (b32:decode string :table table))
   :group-size 8))

;;; ============================================================================
;;; Base16 Stream Constructors
;;; ============================================================================

(defun make-base16-encoding-writer (sink &key (uppercase nil))
  "Create a writer that hex-encodes bytes written to it."
  (%make-encoding-writer
   :sink sink
   :encode-fn (lambda (bytes) (b16:encode bytes :uppercase uppercase))
   :flush-fn (lambda (bytes count)
               (declare (ignore count))
               (b16:encode bytes :uppercase uppercase))
   :buffer (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
   :buf-pos 0
   :group-size 1))

(defun make-base16-decoding-reader (source)
  "Create a reader that hex-decodes data read from SOURCE."
  (%make-decoding-reader
   :source source
   :decode-fn (lambda (string) (b16:decode string))
   :group-size 2))
