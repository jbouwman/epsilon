;;;; Tests for Poly1305 (RFC 8439)

(defpackage epsilon.ssl.poly1305-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:poly #:epsilon.ssl.poly1305))
  (:enter t))

(in-package :epsilon.ssl.poly1305-tests)

;;; RFC 8439 Section 2.5.2 - Poly1305 Test Vector
(deftest test-poly1305-rfc8439
  "RFC 8439 Section 2.5.2 test vector"
  (let* ((key (hex-to-bytes "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"))
         (msg (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                   "Cryptographic Forum Research Group"))
         (expected (hex-to-bytes "a8061dc1305136c6c22b8baf0c0127a9"))
         (tag (poly:poly1305 key msg)))
    (assert-true (equalp tag expected))))

;;; RFC 8439 Section A.3 - Test Vector #1
(deftest test-poly1305-rfc8439-a3-1
  "RFC 8439 Appendix A.3 Test Vector #1 (all zeros)"
  (let* ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (msg (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
         (expected (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
         (tag (poly:poly1305 key msg)))
    (assert-true (equalp tag expected))))

;;; Verify function
(deftest test-poly1305-verify
  "Poly1305 verify returns T for correct tag"
  (let* ((key (hex-to-bytes "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"))
         (msg (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                   "Cryptographic Forum Research Group"))
         (tag (hex-to-bytes "a8061dc1305136c6c22b8baf0c0127a9")))
    (assert-true (poly:poly1305-verify key msg tag))))

(deftest test-poly1305-verify-bad-tag
  "Poly1305 verify returns NIL for incorrect tag"
  (let* ((key (hex-to-bytes "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"))
         (msg (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                   "Cryptographic Forum Research Group"))
         (bad-tag (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-not (poly:poly1305-verify key msg bad-tag))))

;;; Incremental API
(deftest test-poly1305-incremental
  "Poly1305 incremental matches one-shot"
  (let* ((key (hex-to-bytes "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"))
         (msg (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                   "Cryptographic Forum Research Group"))
         (one-shot (poly:poly1305 key msg))
         (state (poly:make-poly1305-state key)))
    ;; Feed in chunks
    (poly:poly1305-update state (subseq msg 0 10))
    (poly:poly1305-update state (subseq msg 10))
    (let ((incremental (poly:poly1305-finalize state)))
      (assert-true (equalp one-shot incremental)))))

;;; Empty message
(deftest test-poly1305-empty
  "Poly1305 of empty message"
  (let* ((key (hex-to-bytes "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"))
         (msg (make-array 0 :element-type '(unsigned-byte 8)))
         (tag (poly:poly1305 key msg)))
    ;; For empty message, result = s (the second half of the key)
    (assert-= (length tag) 16)))
