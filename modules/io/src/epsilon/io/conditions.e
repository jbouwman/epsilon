;;;; epsilon.io.conditions - IO error hierarchy
;;;;
;;;; A structured error hierarchy for IO operations.
;;;;
;;;; All IO errors derive from io-error and carry contextual information.

(package epsilon.io.conditions)

;;; ============================================================================
;;; Base Condition
;;; ============================================================================

(define-condition io-error (error)
  ((operation :initarg :operation
              :initform nil
              :reader io-error-operation
              :documentation "The operation that failed (:read, :write, etc.)")
   (stream :initarg :stream
           :initform nil
           :reader io-error-stream
           :documentation "The stream on which the error occurred")
   (position :initarg :position
             :initform nil
             :reader io-error-position
             :documentation "Stream position when error occurred, if known")
   (cause :initarg :cause
          :initform nil
          :reader io-error-cause
          :documentation "Underlying cause, if any"))
  (:report (lambda (c s)
             (format s "IO error during ~A~@[ on ~A~]~@[ at position ~D~]~@[: ~A~]"
                     (io-error-operation c)
                     (io-error-stream c)
                     (io-error-position c)
                     (io-error-cause c))))
  (:documentation "Base condition for all IO-related errors."))

;;; ============================================================================
;;; EOF and Short Transfer Errors
;;; ============================================================================

(define-condition eof-error (io-error)
  ()
  (:default-initargs :operation :read)
  (:report (lambda (c s)
             (format s "Unexpected end of file~@[ on ~A~]~@[ at position ~D~]"
                     (io-error-stream c)
                     (io-error-position c))))
  (:documentation "Signaled when EOF is encountered unexpectedly."))

(define-condition short-read-error (io-error)
  ((expected :initarg :expected
             :reader short-read-error-expected
             :documentation "Number of bytes expected")
   (actual :initarg :actual
           :reader short-read-error-actual
           :documentation "Number of bytes actually read"))
  (:default-initargs :operation :read-exact)
  (:report (lambda (c s)
             (format s "Short read: expected ~D bytes, got ~D~@[ on ~A~]"
                     (short-read-error-expected c)
                     (short-read-error-actual c)
                     (io-error-stream c))))
  (:documentation "Signaled when read-exact cannot read all requested bytes."))

(define-condition short-write-error (io-error)
  ((expected :initarg :expected
             :reader short-write-error-expected
             :documentation "Number of bytes to write")
   (actual :initarg :actual
           :reader short-write-error-actual
           :documentation "Number of bytes actually written"))
  (:default-initargs :operation :write-all)
  (:report (lambda (c s)
             (format s "Short write: expected ~D bytes, wrote ~D~@[ on ~A~]"
                     (short-write-error-expected c)
                     (short-write-error-actual c)
                     (io-error-stream c))))
  (:documentation "Signaled when write-all cannot write all bytes."))

;;; ============================================================================
;;; Resource State Errors
;;; ============================================================================

(define-condition closed-error (io-error)
  ()
  (:report (lambda (c s)
             (format s "Operation ~A on closed stream~@[ ~A~]"
                     (io-error-operation c)
                     (io-error-stream c))))
  (:documentation "Signaled when operating on a closed stream."))

(define-condition would-block-error (io-error)
  ()
  (:report (lambda (c s)
             (format s "Operation ~A would block~@[ on ~A~]"
                     (io-error-operation c)
                     (io-error-stream c))))
  (:documentation "Signaled when non-blocking IO would need to wait."))

;;; ============================================================================
;;; Async-Specific Errors
;;; ============================================================================

(define-condition timeout-error (io-error)
  ((duration :initarg :duration
             :reader timeout-error-duration
             :documentation "The timeout duration that elapsed"))
  (:report (lambda (c s)
             (format s "Operation ~A timed out after ~A ms~@[ on ~A~]"
                     (io-error-operation c)
                     (timeout-error-duration c)
                     (io-error-stream c))))
  (:documentation "Signaled when an async operation exceeds its timeout."))

(define-condition cancelled-error (io-error)
  ()
  (:report (lambda (c s)
             (format s "Operation ~A was cancelled~@[ on ~A~]"
                     (io-error-operation c)
                     (io-error-stream c))))
  (:documentation "Signaled when an async operation is cancelled."))

;;; ============================================================================
;;; Seeking Errors
;;; ============================================================================

(define-condition seek-error (io-error)
  ((offset :initarg :offset
           :reader seek-error-offset)
   (whence :initarg :whence
           :reader seek-error-whence))
  (:default-initargs :operation :seek)
  (:report (lambda (c s)
             (format s "Cannot seek to ~A ~D~@[ on ~A~]"
                     (seek-error-whence c)
                     (seek-error-offset c)
                     (io-error-stream c))))
  (:documentation "Signaled when a seek operation fails."))

(define-condition not-seekable-error (seek-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Stream does not support seeking")))
  (:documentation "Signaled when seeking on a non-seekable stream."))

;;; ============================================================================
;;; Limit Errors
;;; ============================================================================

(define-condition limit-exceeded-error (io-error)
  ((limit :initarg :limit
          :reader limit-exceeded-error-limit)
   (attempted :initarg :attempted
              :reader limit-exceeded-error-attempted))
  (:report (lambda (c s)
             (format s "Limit of ~D bytes exceeded (attempted ~D)~@[ on ~A~]"
                     (limit-exceeded-error-limit c)
                     (limit-exceeded-error-attempted c)
                     (io-error-stream c))))
  (:documentation "Signaled when a limit-reader or limit-writer exceeds its bound."))

;;; ============================================================================
;;; Buffer Errors
;;; ============================================================================

(define-condition buffer-error (io-error)
  ()
  (:documentation "Base for buffer-related errors."))

(define-condition buffer-overflow-error (buffer-error)
  ((capacity :initarg :capacity
             :reader buffer-overflow-error-capacity)
   (required :initarg :required
             :reader buffer-overflow-error-required))
  (:report (lambda (c s)
             (format s "Buffer overflow: capacity ~D, required ~D"
                     (buffer-overflow-error-capacity c)
                     (buffer-overflow-error-required c))))
  (:documentation "Signaled when buffer cannot accommodate data."))

(define-condition buffer-underflow-error (buffer-error)
  ((available :initarg :available
              :reader buffer-underflow-error-available)
   (requested :initarg :requested
              :reader buffer-underflow-error-requested))
  (:report (lambda (c s)
             (format s "Buffer underflow: available ~D, requested ~D"
                     (buffer-underflow-error-available c)
                     (buffer-underflow-error-requested c))))
  (:documentation "Signaled when buffer has insufficient data."))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun signal-would-block (stream operation)
  "Signal would-block-error for non-blocking operations."
  (error 'would-block-error :operation operation :stream stream))
