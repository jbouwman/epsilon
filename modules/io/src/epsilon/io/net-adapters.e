;;;; epsilon.io.net-adapters - TCP stream adapters for epsilon.io protocols
;;;;
;;;; Provides Reader/Writer protocol implementations for TCP streams.
;;;; These adapters wrap epsilon.net tcp-stream objects to enable
;;;; composition with epsilon.io abstractions (buffered readers, etc.)

(package epsilon.io.net-adapters
  (import (epsilon.io.protocol proto)
          (epsilon.net net)))

;;; ============================================================================
;;; TCP Reader - wraps tcp-stream for reading
;;; ============================================================================

(defstruct (tcp-reader (:constructor %make-tcp-reader))
  "Reader that reads from a TCP stream.

   Implements: Reader, Closer protocols.

   Note: The tcp-stream is shared; closing the reader closes the
   underlying stream."
  (stream nil)  ; tcp-stream object
  (closed-p nil :type boolean))

(defun make-tcp-reader (tcp-stream)
  "Create a tcp-reader wrapping TCP-STREAM.

   TCP-STREAM must be an epsilon.net tcp-stream object.
   The reader will delegate read operations to net:tcp-read."
  (%make-tcp-reader :stream tcp-stream))

(defmethod proto:read-into ((reader tcp-reader) buffer &key (start 0) (end (length buffer)))
  "Read bytes from the TCP stream into BUFFER."
  (when (tcp-reader-closed-p reader)
    (error "TCP reader is closed"))
  (when (null (tcp-reader-stream reader))
    (return-from proto:read-into 0))
  (let ((n (net:tcp-read (tcp-reader-stream reader) buffer :start start :end end)))
    (when (zerop n)
      ;; EOF - connection closed by peer
      (setf (tcp-reader-closed-p reader) t))
    n))

(defmethod proto:close* ((reader tcp-reader))
  "Close the TCP reader (and underlying stream)."
  (if (tcp-reader-closed-p reader)
      nil
      (progn
        (setf (tcp-reader-closed-p reader) t)
        (when (tcp-reader-stream reader)
          (net:tcp-close (tcp-reader-stream reader)))
        t)))

(defmethod proto:open-p ((reader tcp-reader))
  (not (tcp-reader-closed-p reader)))

;;; ============================================================================
;;; TCP Writer - wraps tcp-stream for writing
;;; ============================================================================

(defstruct (tcp-writer (:constructor %make-tcp-writer))
  "Writer that writes to a TCP stream.

   Implements: Writer, Closer protocols.

   Note: The tcp-stream is shared; closing the writer closes the
   underlying stream."
  (stream nil)  ; tcp-stream object
  (closed-p nil :type boolean))

(defun make-tcp-writer (tcp-stream)
  "Create a tcp-writer wrapping TCP-STREAM.

   TCP-STREAM must be an epsilon.net tcp-stream object.
   The writer will delegate write operations to net:tcp-write."
  (%make-tcp-writer :stream tcp-stream))

(defmethod proto:write-from ((writer tcp-writer) buffer &key (start 0) (end (length buffer)))
  "Write bytes from BUFFER to the TCP stream."
  (when (tcp-writer-closed-p writer)
    (error "TCP writer is closed"))
  (when (null (tcp-writer-stream writer))
    (return-from proto:write-from 0))
  (net:tcp-write (tcp-writer-stream writer) buffer :start start :end end))

(defmethod proto:flush ((writer tcp-writer))
  "Flush any buffered data to the network."
  (when (tcp-writer-stream writer)
    (net:tcp-flush (tcp-writer-stream writer)))
  writer)

(defmethod proto:close* ((writer tcp-writer))
  "Close the TCP writer (and underlying stream)."
  (if (tcp-writer-closed-p writer)
      nil
      (progn
        (setf (tcp-writer-closed-p writer) t)
        (when (tcp-writer-stream writer)
          (net:tcp-close (tcp-writer-stream writer)))
        t)))

(defmethod proto:open-p ((writer tcp-writer))
  (not (tcp-writer-closed-p writer)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun tcp-stream-to-reader (tcp-stream)
  "Create a Reader from a TCP stream.
   Shorthand for make-tcp-reader."
  (make-tcp-reader tcp-stream))

(defun tcp-stream-to-writer (tcp-stream)
  "Create a Writer from a TCP stream.
   Shorthand for make-tcp-writer."
  (make-tcp-writer tcp-stream))
