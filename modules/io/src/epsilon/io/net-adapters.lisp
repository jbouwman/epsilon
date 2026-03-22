;;;; epsilon.io.net-adapters - TCP stream adapters for epsilon.io protocols
;;;;
;;;; Provides Reader/Writer protocol implementations for TCP streams.
;;;; These adapters wrap epsilon.net tcp-stream objects to enable
;;;; composition with epsilon.io abstractions (buffered readers, etc.)

(defpackage epsilon.io.net-adapters
  (:use :cl)
  (:require (epsilon.io.protocol proto)
            (epsilon.net net)
            (epsilon.typeclass tc))
  (:enter t))

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

(tc:definstance proto:closer tcp-reader
  (proto:close* (reader)
    "Close the TCP reader (and underlying stream)."
    (if (tcp-reader-closed-p reader)
        nil
        (progn
          (setf (tcp-reader-closed-p reader) t)
          (when (tcp-reader-stream reader)
            (net:tcp-close (tcp-reader-stream reader)))
          t)))
  (proto:open-p (reader)
    (not (tcp-reader-closed-p reader))))

(tc:definstance proto:reader tcp-reader
  (proto:read-into (reader buffer &key (start 0) (end (length buffer)))
    "Read bytes from the TCP stream into BUFFER.
     Blocks until data is available or connection is closed."
    (when (tcp-reader-closed-p reader)
      (error "TCP reader is closed"))
    (when (null (tcp-reader-stream reader))
      (return-from proto:read-into 0))
    (let ((stream (tcp-reader-stream reader)))
      ;; Loop until we get data or detect EOF
      (loop
        (let* ((sem (sb-thread:make-semaphore :name "poll-read" :count 0))
               (result (net:tcp-poll-read stream
                                          (lambda ()
                                            (sb-thread:signal-semaphore sem)))))
          (ecase result
            (:ready
             ;; Data is available, read it
             (let ((n (net:tcp-read stream buffer :start start :end end)))
               (when (zerop n)
                 (setf (tcp-reader-closed-p reader) t))
               (return n)))
            (:pending
             ;; Wait for waker notification (timeout 5 seconds, will retry)
             (sb-thread:wait-on-semaphore sem :timeout 5)
             ;; Check if connection is still alive
             (unless (net:tcp-connected-p stream)
               (setf (tcp-reader-closed-p reader) t)
               (return 0)))))))))

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

(tc:definstance proto:closer tcp-writer
  (proto:close* (writer)
    "Close the TCP writer (and underlying stream)."
    (if (tcp-writer-closed-p writer)
        nil
        (progn
          (setf (tcp-writer-closed-p writer) t)
          (when (tcp-writer-stream writer)
            (net:tcp-close (tcp-writer-stream writer)))
          t)))
  (proto:open-p (writer)
    (not (tcp-writer-closed-p writer))))

(tc:definstance proto:writer tcp-writer
  (proto:write-from (writer buffer &key (start 0) (end (length buffer)))
    "Write bytes from BUFFER to the TCP stream."
    (when (tcp-writer-closed-p writer)
      (error "TCP writer is closed"))
    (when (null (tcp-writer-stream writer))
      (return-from proto:write-from 0))
    (net:tcp-write (tcp-writer-stream writer) buffer :start start :end end))
  (proto:flush (writer)
    "Flush any buffered data to the network."
    (when (tcp-writer-stream writer)
      (net:tcp-flush (tcp-writer-stream writer)))
    writer))

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
