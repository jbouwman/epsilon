;;;; Tests for epsilon.build.protocol -- length-prefixed s-expression
;;;; framing used between the parent build scheduler and its SBCL
;;;; worker subprocesses.

(cl:defpackage epsilon.build.protocol-tests
  (:use :cl :epsilon.test)
  (:local-nicknames (proto :epsilon.build.protocol)))

(cl:in-package :epsilon.build.protocol-tests)

(defun roundtrip (sexp)
  "Encode SEXP via write-frame, then decode it via read-frame.
   Returns the decoded value."
  (let ((s (with-output-to-string (out)
             (proto:write-frame out sexp))))
    (with-input-from-string (in s)
      (proto:read-frame in))))

;;; --------------------------------------------------------------------------
;;; Round-trip
;;; --------------------------------------------------------------------------

(deftest test-frame-roundtrip-keyword
  (assert-equal :ping (roundtrip :ping)))

(deftest test-frame-roundtrip-list
  (let ((result (roundtrip '(:hello "world" 42))))
    (assert-equal :hello (first result))
    (assert-equal "world" (second result))
    (assert-equal 42 (third result))))

(deftest test-frame-roundtrip-nested
  "Nested list structure preserved positionally"
  (let ((result (roundtrip '(:request 7 (:eval "(+ 1 2)")))))
    (assert-equal :request (first result))
    (assert-equal 7 (second result))
    (assert-equal :eval (first (third result)))
    (assert-equal "(+ 1 2)" (second (third result)))))

(deftest test-frame-roundtrip-empty-list
  (assert-true (null (roundtrip nil))))

(deftest test-frame-roundtrip-string-with-quotes
  (assert-equal "say \"hi\"" (roundtrip "say \"hi\"")))

;;; --------------------------------------------------------------------------
;;; EOF
;;; --------------------------------------------------------------------------

(deftest test-read-frame-empty-stream-returns-nil
  "EOF before any frame is a clean termination signal, not an error"
  (with-input-from-string (in "")
    (assert-true (null (proto:read-frame in)))))

(deftest test-read-frame-skips-blank-length-lines
  "A spurious blank line between frames is tolerated"
  (let* ((payload (with-output-to-string (out)
                    (proto:write-frame out :hello)))
         (with-blank (concatenate 'string (string #\Newline) payload)))
    (with-input-from-string (in with-blank)
      (assert-equal :hello (proto:read-frame in)))))

;;; --------------------------------------------------------------------------
;;; Error paths
;;; --------------------------------------------------------------------------

(deftest test-read-frame-rejects-non-numeric-length
  (assert-condition (proto:protocol-error)
    (with-input-from-string (in "garbage
hello")
      (proto:read-frame in))))

(deftest test-read-frame-rejects-negative-length
  (assert-condition (proto:protocol-error)
    (with-input-from-string (in "-1
")
      (proto:read-frame in))))

(deftest test-read-frame-rejects-oversized-length
  (assert-condition (proto:protocol-error)
    (with-input-from-string (in (format nil "~D~%" (1+ proto:+max-frame-bytes+)))
      (proto:read-frame in))))

(deftest test-read-frame-rejects-truncated-payload
  "Length declares 100 bytes but only 5 are present -- mid-frame EOF"
  (assert-condition (proto:protocol-error)
    (with-input-from-string (in (format nil "100~%hello"))
      (proto:read-frame in))))

;;; --------------------------------------------------------------------------
;;; *read-eval* sandbox
;;; --------------------------------------------------------------------------

(deftest test-read-frame-rejects-read-eval-injection
  "#. would let a malicious peer evaluate arbitrary code at the
   parent.  The reader must run with *read-eval* nil."
  (assert-condition (error)
    (with-input-from-string (in (format nil "11~%#.(+ 1 2 3)"))
      (proto:read-frame in))))
