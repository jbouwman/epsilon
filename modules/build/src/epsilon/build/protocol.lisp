;;;; epsilon.build.protocol - Length-prefixed s-expression framing
;;;;
;;;; Used between the parent scheduler and SBCL build workers. Each frame
;;;; is encoded as a decimal character count, a newline, then exactly that
;;;; many characters of a printed s-expression. The reader is sandboxed:
;;;; *read-eval* is bound to NIL and *package* to the keyword package.

(defpackage epsilon.build.protocol
  (:use :cl)
  (:export #:write-frame
           #:read-frame
           #:protocol-error
           #:+max-frame-bytes+))

(in-package :epsilon.build.protocol)

(defparameter +max-frame-bytes+ (* 16 1024 1024)
  "Hard ceiling on a single frame to bound memory in the face of a
   misbehaving peer. 16 MiB is enough for any compile reply we expect.")

(define-condition protocol-error (error)
  ((message :initarg :message :reader protocol-error-message))
  (:report (lambda (c s)
             (format s "Parallel-build protocol error: ~A"
                     (protocol-error-message c)))))

(defun write-frame (stream sexp)
  "Serialise SEXP to STREAM as a length-prefixed frame. Flushes the
   stream so the peer is never left waiting for buffered output."
  (let ((s (let ((*print-readably* t)
                 (*print-pretty* nil)
                 (*print-circle* nil)
                 (*print-length* nil)
                 (*print-level* nil)
                 (*print-escape* t)
                 (*package* (find-package :keyword)))
             (prin1-to-string sexp))))
    (format stream "~D~%~A" (length s) s)
    (finish-output stream)))

(defun read-frame (stream)
  "Read one frame from STREAM. Returns the s-expression, or NIL at EOF.
   Signals PROTOCOL-ERROR on malformed input or unexpected EOF mid-frame."
  (let ((len-line (read-line stream nil nil)))
    (unless len-line
      (return-from read-frame nil))
    (let ((trimmed (string-trim '(#\Space #\Tab #\Return) len-line)))
      (when (zerop (length trimmed))
        (return-from read-frame (read-frame stream)))
      (let ((len (handler-case (parse-integer trimmed)
                   (error ()
                     (error 'protocol-error
                            :message (format nil "bad length line: ~S" len-line))))))
        (when (or (minusp len) (> len +max-frame-bytes+))
          (error 'protocol-error
                 :message (format nil "frame length out of range: ~D" len)))
        (let ((buf (make-string len))
              (pos 0))
          (loop while (< pos len)
                for got = (read-sequence buf stream :start pos)
                do (when (= got pos)
                     (error 'protocol-error
                            :message (format nil "EOF after ~D of ~D bytes" pos len)))
                   (setf pos got))
          (let ((*read-eval* nil)
                (*package* (find-package :keyword)))
            (read-from-string buf)))))))
