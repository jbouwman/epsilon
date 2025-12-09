;;;; epsilon.registry.integrity - Checksum verification
;;;;
;;;; Provides integrity verification for packages using SHA-256 checksums.

(in-package epsilon.registry)

;;; Checksum operations

(defun compute-checksum (content)
  "Compute SHA-256 checksum of CONTENT.
   Returns a string in the format 'sha256:hexdigest'."
  (let ((hash (digest:sha256 content)))
    (format nil "sha256:~A" (string-downcase (digest:bytes-to-hex hash)))))

(defun verify-checksum (content expected-checksum)
  "Verify that CONTENT matches EXPECTED-CHECKSUM.
   EXPECTED-CHECKSUM should be in format 'sha256:hexdigest'.
   Returns T if checksums match, signals an error otherwise."
  (let ((computed (compute-checksum content)))
    (unless (string= computed expected-checksum)
      (error 'checksum-mismatch
             :expected expected-checksum
             :actual computed))
    t))

(define-condition checksum-mismatch (error)
  ((expected :initarg :expected :reader expected-checksum)
   (actual :initarg :actual :reader actual-checksum))
  (:report (lambda (condition stream)
             (format stream "Checksum mismatch: expected ~A but got ~A"
                     (expected-checksum condition)
                     (actual-checksum condition))))
  (:documentation "Signaled when package checksum verification fails."))

(defun parse-checksum (checksum-string)
  "Parse a checksum string into (algorithm . digest).
   Returns (values algorithm digest)."
  (let ((pos (position #\: checksum-string)))
    (if pos
        (values (subseq checksum-string 0 pos)
                (subseq checksum-string (1+ pos)))
        (values "sha256" checksum-string))))

(defun checksum-algorithm (checksum-string)
  "Extract the algorithm from a checksum string."
  (multiple-value-bind (algo digest)
      (parse-checksum checksum-string)
    (declare (ignore digest))
    algo))

(defun checksum-digest (checksum-string)
  "Extract the digest from a checksum string."
  (multiple-value-bind (algo digest)
      (parse-checksum checksum-string)
    (declare (ignore algo))
    digest))
