;;;; epsilon.io.protocol - stream protocols
;;;;
;;;; Defines the fundamental abstractions for IO operations:
;;;; - Reader: byte source
;;;; - Writer: byte sink
;;;; - Closer: resource cleanup
;;;; - Seeker: random access positioning
;;;;
;;;; Each protocol defines one method that must be implemented; all
;;;; others have default implementations built on top.

(defpackage epsilon.io.protocol
  (:use :cl)
  (:require (epsilon.io.conditions cond)
            (epsilon.typeclass tc))
  (:enter t))

;;; ============================================================================
;;; Reader Protocol
;;; ============================================================================

(tc:deftypeclass reader (closer)
  "Byte source protocol. Only read-into must be implemented."
  (read-into (reader buffer &key start end)
    "Read bytes from READER into BUFFER between START and END.
     Returns positive integer (bytes read) or 0 (EOF).")
  (read-byte* (reader)
    "Read a single byte. Returns 0-255 or NIL (EOF)."
    :default (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
               (declare (dynamic-extent buf))
               (let ((n (read-into reader buf)))
                 (if (plusp n)
                     (aref buf 0)
                     nil))))
  (read-exact (reader buffer &key start end)
    "Read exactly (- END START) bytes into BUFFER."
    :default (let ((start (or start 0))
                   (end (or end (length buffer))))
               (let ((total 0)
                     (remaining (- end start)))
                 (loop while (plusp remaining) do
                   (let ((n (read-into reader buffer :start (+ start total) :end end)))
                     (when (zerop n)
                       (error 'cond:short-read-error
                              :operation :read-exact
                              :expected remaining
                              :actual total))
                     (incf total n)
                     (decf remaining n)))
                 total)))
  (read-all (reader &key max-size initial-size)
    "Read all bytes from READER until EOF."
    :default (let* ((initial-size (or initial-size 4096))
                    (chunks nil)
                    (total 0)
                    (chunk-size initial-size))
               (loop
                 (when (and max-size (>= total max-size))
                   (return))
                 (let* ((size (if max-size
                                  (min chunk-size (- max-size total))
                                  chunk-size))
                        (buf (make-array size :element-type '(unsigned-byte 8)))
                        (n (read-into reader buf)))
                   (when (zerop n)
                     (return))
                   (push (if (= n size) buf (subseq buf 0 n)) chunks)
                   (incf total n)
                   (setf chunk-size (min (* chunk-size 2) 65536))))
               (if (null chunks)
                   (make-array 0 :element-type '(unsigned-byte 8))
                   (let ((result (make-array total :element-type '(unsigned-byte 8)))
                         (pos 0))
                     (dolist (chunk (nreverse chunks))
                       (replace result chunk :start1 pos)
                       (incf pos (length chunk)))
                     result)))))

;;; ============================================================================
;;; Writer Protocol
;;; ============================================================================

(tc:deftypeclass writer (closer)
  "Byte sink protocol. Only write-from must be implemented."
  (write-from (writer buffer &key start end)
    "Write bytes from BUFFER[START:END] to WRITER.
     Returns positive integer (bytes written).")
  (write-byte* (writer byte)
    "Write a single BYTE to WRITER."
    :default (let ((buf (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element byte)))
               (declare (dynamic-extent buf))
               (write-from writer buf)))
  (write-all (writer buffer &key start end)
    "Write all bytes from BUFFER[START:END] to WRITER."
    :default (let ((start (or start 0))
                   (end (or end (length buffer))))
               (let ((total 0)
                     (remaining (- end start)))
                 (loop while (plusp remaining) do
                   (let ((n (write-from writer buffer :start (+ start total) :end end)))
                     (when (zerop n)
                       (error 'cond:short-write-error
                              :operation :write-all
                              :expected remaining
                              :actual total))
                     (incf total n)
                     (decf remaining n)))
                 total)))
  (flush (writer)
    "Ensure all buffered data is written to the underlying sink."
    :default writer))

;;; ============================================================================
;;; Closer Protocol
;;; ============================================================================

(tc:deftypeclass closer ()
  "Resource cleanup protocol."
  (close* (closable)
    "Release resources associated with CLOSABLE.
     Idempotent: calling multiple times has no additional effect.
     Returns T if resources were released, NIL if already closed."
    :default nil)
  (open-p (closable)
    "Return T if CLOSABLE is open, NIL if closed."
    :default t))

;;; ============================================================================
;;; Seeker Protocol
;;; ============================================================================

(tc:deftypeclass seeker ()
  "Random access positioning protocol."
  (seek* (seeker offset whence)
    "Set the position within SEEKER.
     WHENCE is :start, :current, or :end.
     Returns the new absolute position.")
  (position* (positionable)
    "Return current byte position, or NIL if unavailable."
    :default nil)
  (size* (sizable)
    "Return total size in bytes, or NIL if unknown."
    :default nil))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-open ((var opener &rest args) &body body)
  "Execute BODY with VAR bound to result of (OPENER ARGS...).
   Ensures CLOSE* is called on VAR when BODY exits.

   Example:
     (with-open (f #'open-file \"data.bin\")
       (read-all f))"
  `(let ((,var (funcall ,opener ,@args)))
     (unwind-protect
          (progn ,@body)
       (close* ,var))))

(defmacro with-reader ((var reader) &body body)
  "Execute BODY with VAR bound to READER, closing on exit."
  `(with-open (,var (lambda () ,reader))
     ,@body))

(defmacro with-writer ((var writer) &body body)
  "Execute BODY with VAR bound to WRITER, flushing and closing on exit."
  `(let ((,var ,writer))
     (unwind-protect
          (progn ,@body)
       (flush ,var)
       (close* ,var))))

;;; ============================================================================
;;; Protocol Predicates
;;; ============================================================================

(defun reader-p (obj)
  "Return T if OBJ implements the reader protocol."
  (and (compute-applicable-methods #'read-into (list obj #() :start 0 :end 0))
       t))

(defun writer-p (obj)
  "Return T if OBJ implements the writer protocol."
  (and (compute-applicable-methods #'write-from (list obj #() :start 0 :end 0))
       t))

(defun closer-p (obj)
  "Return T if OBJ implements the closer protocol."
  (and (compute-applicable-methods #'close* (list obj))
       t))

(defun seeker-p (obj)
  "Return T if OBJ implements the seeker protocol."
  (and (compute-applicable-methods #'seek* (list obj 0 :start))
       t))
