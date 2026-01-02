;;;; CL Stream Adapters - epsilon.io protocol implementations for SBCL streams
;;;;
;;;; Provides Reader/Writer protocol implementations that wrap standard
;;;; Common Lisp binary streams. This enables using epsilon.io abstractions
;;;; with SBCL's two-way-stream, file-stream, etc.

(package epsilon.io.cl-stream-adapters
  (import (epsilon.io.protocol proto)
          (epsilon.io.conditions cond)))

;;; ============================================================================
;;; CL Stream Reader - wraps CL binary input stream for reading
;;; ============================================================================

(defstruct (cl-stream-reader (:constructor %make-cl-stream-reader))
  "Reader that reads from a Common Lisp binary input stream.

   Implements epsilon.io Reader, Closer protocols.

   The wrapped stream should be an input stream with element-type
   (unsigned-byte 8) or a two-way-stream."
  (stream nil)
  (closed-p nil :type boolean))

(defun make-cl-stream-reader (stream)
  "Create a cl-stream-reader wrapping STREAM.

   STREAM must be a CL input stream capable of binary I/O.
   For two-way-streams, reads will come from the input side."
  (%make-cl-stream-reader :stream stream))

(defmethod proto:read-into ((reader cl-stream-reader) buffer &key (start 0) (end (length buffer)))
  "Read bytes from the CL stream into BUFFER."
  (when (cl-stream-reader-closed-p reader)
    (error 'cond:closed-error :stream reader :operation :read-into))
  (let ((stream (cl-stream-reader-stream reader)))
    (when (null stream)
      (return-from proto:read-into 0))
    ;; Read bytes one at a time (simple but correct)
    (let ((count 0))
      (loop for i from start below end
            do (let ((byte (read-byte stream nil nil)))
                 (if byte
                     (progn
                       (setf (aref buffer i) byte)
                       (incf count))
                     (return))))
      (when (zerop count)
        ;; EOF - mark as closed
        (setf (cl-stream-reader-closed-p reader) t))
      count)))

(defmethod proto:close* ((reader cl-stream-reader))
  "Close the CL stream reader (and underlying stream)."
  (if (cl-stream-reader-closed-p reader)
      nil
      (progn
        (setf (cl-stream-reader-closed-p reader) t)
        (when (cl-stream-reader-stream reader)
          (ignore-errors (close (cl-stream-reader-stream reader))))
        t)))

(defmethod proto:open-p ((reader cl-stream-reader))
  (not (cl-stream-reader-closed-p reader)))

;;; ============================================================================
;;; CL Stream Writer - wraps CL binary output stream for writing
;;; ============================================================================

(defstruct (cl-stream-writer (:constructor %make-cl-stream-writer))
  "Writer that writes to a Common Lisp binary output stream.

   Implements epsilon.io Writer, Closer protocols.

   The wrapped stream should be an output stream with element-type
   (unsigned-byte 8) or a two-way-stream."
  (stream nil)
  (closed-p nil :type boolean))

(defun make-cl-stream-writer (stream)
  "Create a cl-stream-writer wrapping STREAM.

   STREAM must be a CL output stream capable of binary I/O.
   For two-way-streams, writes will go to the output side."
  (%make-cl-stream-writer :stream stream))

(defmethod proto:write-from ((writer cl-stream-writer) buffer &key (start 0) (end (length buffer)))
  "Write bytes from BUFFER to the CL stream."
  (when (cl-stream-writer-closed-p writer)
    (error 'cond:closed-error :stream writer :operation :write-from))
  (let ((stream (cl-stream-writer-stream writer)))
    (when (null stream)
      (return-from proto:write-from 0))
    (let ((count (- end start)))
      (loop for i from start below end
            do (write-byte (aref buffer i) stream))
      count)))

(defmethod proto:flush ((writer cl-stream-writer))
  "Flush the CL stream writer."
  (when (and (not (cl-stream-writer-closed-p writer))
             (cl-stream-writer-stream writer))
    (force-output (cl-stream-writer-stream writer)))
  writer)

(defmethod proto:close* ((writer cl-stream-writer))
  "Close the CL stream writer (and underlying stream)."
  (if (cl-stream-writer-closed-p writer)
      nil
      (progn
        (setf (cl-stream-writer-closed-p writer) t)
        (when (cl-stream-writer-stream writer)
          (ignore-errors
            (force-output (cl-stream-writer-stream writer))
            (close (cl-stream-writer-stream writer))))
        t)))

(defmethod proto:open-p ((writer cl-stream-writer))
  (not (cl-stream-writer-closed-p writer)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun cl-stream-to-reader (stream)
  "Create a Reader from a CL input stream.
   Shorthand for make-cl-stream-reader."
  (make-cl-stream-reader stream))

(defun cl-stream-to-writer (stream)
  "Create a Writer from a CL output stream.
   Shorthand for make-cl-stream-writer."
  (make-cl-stream-writer stream))
