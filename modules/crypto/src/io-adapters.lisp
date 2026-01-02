;;;; TLS IO Adapters - epsilon.io protocol implementations for TLS connections
;;;;
;;;; Provides Reader/Writer protocol implementations that wrap TLS connections.
;;;; These adapters enable using epsilon.io abstractions (buffered readers, etc.)
;;;; with TLS connections.

(in-package :epsilon.crypto)

;;; ============================================================================
;;; TLS Reader - wraps TLS connection for reading
;;; ============================================================================

(defstruct (tls-reader (:constructor %make-tls-reader))
  "Reader that reads from a TLS connection.

   Implements epsilon.io Reader, Closer protocols.

   Note: The TLS connection is shared; closing the reader closes the
   underlying connection."
  (connection nil)  ; TLS connection (openssl-connection or tls-connection)
  (closed-p nil :type boolean))

(defun make-tls-reader (tls-connection)
  "Create a tls-reader wrapping TLS-CONNECTION.

   TLS-CONNECTION must be an epsilon.crypto TLS connection object
   (either openssl-connection or tls-connection).
   The reader will delegate read operations to tls-read."
  (%make-tls-reader :connection tls-connection))

(defmethod epsilon.io:read-into ((reader tls-reader) buffer &key (start 0) (end (length buffer)))
  "Read bytes from the TLS connection into BUFFER."
  (when (tls-reader-closed-p reader)
    (error "TLS reader is closed"))
  (when (null (tls-reader-connection reader))
    (return-from epsilon.io:read-into 0))
  (let ((n (tls-read (tls-reader-connection reader) buffer :start start :end end)))
    (when (zerop n)
      ;; EOF - connection closed by peer
      (setf (tls-reader-closed-p reader) t))
    n))

(defmethod epsilon.io:close* ((reader tls-reader))
  "Close the TLS reader (and underlying connection)."
  (if (tls-reader-closed-p reader)
      nil
      (progn
        (setf (tls-reader-closed-p reader) t)
        (when (tls-reader-connection reader)
          (tls-close (tls-reader-connection reader)))
        t)))

(defmethod epsilon.io:open-p ((reader tls-reader))
  (not (tls-reader-closed-p reader)))

;;; ============================================================================
;;; TLS Writer - wraps TLS connection for writing
;;; ============================================================================

(defstruct (tls-writer (:constructor %make-tls-writer))
  "Writer that writes to a TLS connection.

   Implements epsilon.io Writer, Closer protocols.

   Note: The TLS connection is shared; closing the writer closes the
   underlying connection."
  (connection nil)  ; TLS connection (openssl-connection or tls-connection)
  (closed-p nil :type boolean))

(defun make-tls-writer (tls-connection)
  "Create a tls-writer wrapping TLS-CONNECTION.

   TLS-CONNECTION must be an epsilon.crypto TLS connection object.
   The writer will delegate write operations to tls-write."
  (%make-tls-writer :connection tls-connection))

(defmethod epsilon.io:write-from ((writer tls-writer) buffer &key (start 0) (end (length buffer)))
  "Write bytes from BUFFER to the TLS connection."
  (when (tls-writer-closed-p writer)
    (error "TLS writer is closed"))
  (when (null (tls-writer-connection writer))
    (return-from epsilon.io:write-from 0))
  ;; tls-write expects just the data, so we need to create a slice
  (let* ((count (- end start))
         (data (if (and (= start 0) (= end (length buffer)))
                   buffer
                   (subseq buffer start end))))
    (tls-write (tls-writer-connection writer) data)
    count))

(defmethod epsilon.io:flush ((writer tls-writer))
  "Flush is a no-op for TLS - data is written immediately."
  writer)

(defmethod epsilon.io:close* ((writer tls-writer))
  "Close the TLS writer (and underlying connection)."
  (if (tls-writer-closed-p writer)
      nil
      (progn
        (setf (tls-writer-closed-p writer) t)
        (when (tls-writer-connection writer)
          (tls-close (tls-writer-connection writer)))
        t)))

(defmethod epsilon.io:open-p ((writer tls-writer))
  (not (tls-writer-closed-p writer)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun tls-connection-to-reader (tls-connection)
  "Create a Reader from a TLS connection.
   Shorthand for make-tls-reader."
  (make-tls-reader tls-connection))

(defun tls-connection-to-writer (tls-connection)
  "Create a Writer from a TLS connection.
   Shorthand for make-tls-writer."
  (make-tls-writer tls-connection))
