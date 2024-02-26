(in-package #:encode)

(define-condition stream-closed (stream-error)
  ()
  (:documentation
   "Signaled when a closed stream is written.")
  (:report
   (lambda (condition stream)
     (format stream "Stream ~S is closed"
             (stream-error-stream condition)))))

(define-condition stream-exhausted (stream-error)
  ()
  (:documentation
   "Signaled when a stream is exhausted.")
  (:report
   (lambda (condition stream)
     (format stream "Stream ~S exhausted."
             (stream-error-stream condition)))))

;; jx

(define-condition invalid-format-error (simple-error)
  ((format :initarg :format :reader invalid-format))
  (:report (lambda (condition stream)
             (format stream "Invalid format ~S"
                     (invalid-format condition))))
  (:documentation "Signaled when an invalid format name is passed to
MAKE-DSTATE, MAKE-INFLATE-STATE, or DECOMPRESS."))

(define-condition invalid-checksum-error (simple-error)
  ((expected-checksum :initarg :stored :reader expected-checksum)
   (actual-checksum :initarg :computed :reader actual-checksum)
   (kind :initarg :kind :reader checksum-kind))
  (:report (lambda (condition stream)
             (format stream "Invalid ~A checksum, expected ~X, got ~X"
                     (checksum-kind condition)
                     (expected-checksum condition)
                     (actual-checksum condition))))
  (:documentation "Signaled when the checksum of decompressed data does
not match the expected value."))

(define-condition invalid-zlib-header-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid zlib header")))
  (:documentation "Signaled when a zlib header does not pass the
consistency check."))

(define-condition invalid-gzip-header-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid gzip header")))
  (:documentation "Signaled when a gzip header does not have the proper ID."))

(define-condition reserved-block-type-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid deflate block")))
  (:documentation "Signaled when an invalid deflate block is found."))

(define-condition invalid-stored-block-length-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid stored block length")))
  (:documentation "Signaled when a stored block's length does not pass
the consistency check."))

(define-condition code-lengths-bounds-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Code lengths expand out of bounds")))
  (:documentation "Signaled when the code length section of a dynamic block would produce more
code lengths than declared."))

(define-condition code-lengths-start-with-repetition-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Code lengths start with a repetition")))
  (:documentation "Signaled when the code length section of a dynamic block begins with \"repeat
last code\"."))

(define-condition unassigned-huffman-code-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Unassigned Huffman code")))
  (:documentation "Signaled when an unassigned Huffman code is referenced."))

(define-condition illegal-length-code-error (simple-error)
  ((code :initarg :code :reader illegal-code))
  (:report (lambda (condition stream)
             (format stream "Illegal length code: ~d" (illegal-code condition))))
  (:documentation "Signaled when the illegal length codes 286 or 287 are used."))

(define-condition illegal-distance-code-error (simple-error)
  ((code :initarg :code :reader illegal-code))
  (:report (lambda (condition stream)
             (format stream "Illegal distance code: ~d" (illegal-code condition))))
  (:documentation "Signaled when the illegal distance codes 30 or 31 are used."))

(define-condition invalid-bzip2-data (simple-error)
  ()
  (:documentation "Signaled when invalid bzip2 data is found."))
