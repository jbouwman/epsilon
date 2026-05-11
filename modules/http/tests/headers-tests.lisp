;;;; HTTP Headers Tests
;;;;
;;;; Tests for the case-folding header map wrapper.
;;;;
;;;; The header map wraps an immutable epsilon.map with case-insensitive
;;;; lookup on string keys per RFC 7230 (HTTP header field names are
;;;; case-insensitive). Iteration preserves the originally-supplied casing
;;;; so that wire output can present headers in their canonical form.

(defpackage :epsilon.http.headers.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.headers headers)
   (epsilon.map map)))

(in-package :epsilon.http.headers.tests)

;;; Construction

(deftest test-empty-headers ()
  "Empty headers should have count 0 and return nil for lookups"
  (let ((h (headers:make-headers)))
    (assert-true (headers:headers-p h))
    (assert-equal 0 (headers:headers-count h))
    (assert-nil (headers:headers-get h "Content-Type"))))

(deftest test-empty-singleton ()
  "headers:+empty+ should always be empty"
  (assert-true (headers:headers-p headers:+empty+))
  (assert-equal 0 (headers:headers-count headers:+empty+))
  (assert-nil (headers:headers-get headers:+empty+ "Anything")))

(deftest test-make-headers-with-pairs ()
  "make-headers should accept alternating key/value pairs"
  (let ((h (headers:make-headers "Content-Type" "text/plain"
                                 "Content-Length" "42")))
    (assert-equal 2 (headers:headers-count h))
    (assert-equal "text/plain" (headers:headers-get h "Content-Type"))
    (assert-equal "42" (headers:headers-get h "Content-Length"))))

;;; Case-insensitive lookup

(deftest test-case-insensitive-lookup ()
  "Lookups must be case-insensitive regardless of how the key was stored"
  (let ((h (headers:headers-assoc headers:+empty+ "Content-Type" "text/plain")))
    (assert-equal "text/plain" (headers:headers-get h "Content-Type"))
    (assert-equal "text/plain" (headers:headers-get h "content-type"))
    (assert-equal "text/plain" (headers:headers-get h "CONTENT-TYPE"))
    (assert-equal "text/plain" (headers:headers-get h "CoNtEnT-tYpE"))))

(deftest test-case-insensitive-store ()
  "Storing then looking up with different case should still find the entry"
  (let ((h (headers:headers-assoc headers:+empty+ "content-type" "application/json")))
    (assert-equal "application/json" (headers:headers-get h "Content-Type"))
    (assert-equal "application/json" (headers:headers-get h "CONTENT-TYPE"))))

(deftest test-default-value ()
  "headers-get should return the supplied default when key is missing"
  (let ((h (headers:make-headers "X-Foo" "bar")))
    (assert-equal "fallback" (headers:headers-get h "X-Bar" "fallback"))
    (assert-nil (headers:headers-get h "X-Bar"))))

;;; Update behavior

(deftest test-assoc-overwrites-case-insensitively ()
  "assoc with a different-case version of an existing key should replace, not duplicate"
  (let* ((h1 (headers:headers-assoc headers:+empty+ "Content-Type" "text/plain"))
         (h2 (headers:headers-assoc h1 "content-type" "application/json")))
    (assert-equal 1 (headers:headers-count h2))
    (assert-equal "application/json" (headers:headers-get h2 "Content-Type"))))

(deftest test-assoc-immutable ()
  "Each assoc must return a new headers value without mutating the original"
  (let* ((h1 (headers:make-headers "X-One" "1"))
         (h2 (headers:headers-assoc h1 "X-Two" "2")))
    (assert-equal 1 (headers:headers-count h1))
    (assert-equal 2 (headers:headers-count h2))
    (assert-nil (headers:headers-get h1 "X-Two"))
    (assert-equal "2" (headers:headers-get h2 "X-Two"))))

(deftest test-dissoc-case-insensitive ()
  "dissoc with a different-case version of an existing key should still remove it"
  (let* ((h1 (headers:make-headers "Content-Type" "text/plain" "X-Foo" "bar"))
         (h2 (headers:headers-dissoc h1 "content-type")))
    (assert-equal 1 (headers:headers-count h2))
    (assert-nil (headers:headers-get h2 "Content-Type"))
    (assert-equal "bar" (headers:headers-get h2 "X-Foo"))))

(deftest test-contains-p-case-insensitive ()
  "contains-p should be case-insensitive"
  (let ((h (headers:make-headers "Authorization" "Bearer x")))
    (assert-true (headers:headers-contains-p h "Authorization"))
    (assert-true (headers:headers-contains-p h "authorization"))
    (assert-true (headers:headers-contains-p h "AUTHORIZATION"))
    (assert-false (headers:headers-contains-p h "x-other"))))

;;; Iteration preserves original casing

(deftest test-each-preserves-original-case ()
  "Iteration should yield keys in the casing they were supplied with."
  (let ((h (headers:make-headers "Content-Type" "text/plain"
                                 "X-Custom-Header" "value")))
    (let ((seen nil))
      (headers:headers-each
       (lambda (k v)
         (declare (ignore v))
         (push k seen))
       h)
      (assert-true (member "Content-Type" seen :test #'string=))
      (assert-true (member "X-Custom-Header" seen :test #'string=)))))

(deftest test-each-uses-most-recent-case ()
  "If a header is re-assoced with new casing, iteration should use the new casing."
  (let* ((h1 (headers:headers-assoc headers:+empty+ "Content-Type" "text/plain"))
         (h2 (headers:headers-assoc h1 "CONTENT-TYPE" "application/json")))
    (let ((seen nil))
      (headers:headers-each
       (lambda (k v)
         (declare (ignore v))
         (push k seen))
       h2)
      (assert-equal 1 (length seen))
      (assert-equal "CONTENT-TYPE" (first seen)))))

(deftest test-headers-keys-and-values ()
  "headers-keys returns canonical names, headers-values returns values."
  (let ((h (headers:make-headers "X-A" "1" "X-B" "2")))
    (let ((keys (headers:headers-keys h))
          (vals (headers:headers-values h)))
      (assert-equal 2 (length keys))
      (assert-equal 2 (length vals))
      (assert-true (member "X-A" keys :test #'string=))
      (assert-true (member "X-B" keys :test #'string=))
      (assert-true (member "1" vals :test #'string=))
      (assert-true (member "2" vals :test #'string=)))))

;;; Merging

(deftest test-merge-prefers-second ()
  "headers-merge should let the second map's values override the first."
  (let* ((h1 (headers:make-headers "Host" "server1" "User-Agent" "test/1"))
         (h2 (headers:make-headers "host" "server2"))
         (m  (headers:headers-merge h1 h2)))
    (assert-equal 2 (headers:headers-count m))
    (assert-equal "server2" (headers:headers-get m "Host"))
    (assert-equal "test/1" (headers:headers-get m "User-Agent"))))

(deftest test-merge-with-empty ()
  "Merging with empty headers should be a no-op (modulo identity)."
  (let ((h (headers:make-headers "X-Foo" "bar")))
    (let ((m1 (headers:headers-merge h headers:+empty+))
          (m2 (headers:headers-merge headers:+empty+ h)))
      (assert-equal 1 (headers:headers-count m1))
      (assert-equal 1 (headers:headers-count m2))
      (assert-equal "bar" (headers:headers-get m1 "X-Foo"))
      (assert-equal "bar" (headers:headers-get m2 "X-Foo")))))

;;; Integration with raw maps

(deftest test-headers-from-map ()
  "headers-from-map should accept any epsilon.map and produce a headers value."
  (let* ((m (map:make-map "Content-Type" "text/plain" "X-Test" "yes"))
         (h (headers:headers-from-map m)))
    (assert-equal 2 (headers:headers-count h))
    (assert-equal "text/plain" (headers:headers-get h "content-type"))
    (assert-equal "yes" (headers:headers-get h "x-test"))))

(deftest test-headers-from-pairs ()
  "headers-from-pairs accepts a list of cons pairs (alist)."
  (let ((h (headers:headers-from-pairs '(("Accept" . "*/*")
                                         ("X-Trace" . "abc")))))
    (assert-equal 2 (headers:headers-count h))
    (assert-equal "*/*" (headers:headers-get h "accept"))
    (assert-equal "abc" (headers:headers-get h "X-Trace"))))

(deftest test-headers-to-alist ()
  "headers-to-alist returns ((canonical-key . value)...) pairs."
  (let* ((h (headers:make-headers "X-A" "1" "X-B" "2"))
         (alist (headers:headers-to-alist h)))
    (assert-equal 2 (length alist))
    (assert-equal "1" (cdr (assoc "X-A" alist :test #'string=)))
    (assert-equal "2" (cdr (assoc "X-B" alist :test #'string=)))))

;;; Numeric values are tolerated (HTTP often uses these for Content-Length)

(deftest test-numeric-values-tolerated ()
  "Storing a number as a value should round-trip without coercion."
  (let ((h (headers:headers-assoc headers:+empty+ "Content-Length" 42)))
    (assert-equal 42 (headers:headers-get h "content-length"))))

;;; Coalescing parsed headers

(deftest test-headers-from-parsed-lines ()
  "headers-parse-line + assoc loop accepts wire-format header lines."
  (let* ((h (headers:headers-assoc headers:+empty+ "Content-Type" "text/plain"))
         (h (headers:headers-assoc h "X-Lower" "v1"))
         (h (headers:headers-assoc h "X-UPPER" "v2")))
    (assert-equal "text/plain" (headers:headers-get h "Content-Type"))
    (assert-equal "v1" (headers:headers-get h "x-lower"))
    (assert-equal "v2" (headers:headers-get h "x-upper"))))

;;; Equality / structural

(deftest test-headers-equal ()
  "Two header maps with the same entries (case-insensitive) compare equal."
  (let ((a (headers:make-headers "Content-Type" "text/plain" "X-Foo" "bar"))
        (b (headers:make-headers "content-type" "text/plain" "x-foo" "bar")))
    (assert-true (headers:headers-equal a b))))

(deftest test-headers-not-equal-on-value ()
  "Different values for the same key should not compare equal."
  (let ((a (headers:make-headers "X" "1"))
        (b (headers:make-headers "X" "2")))
    (assert-false (headers:headers-equal a b))))

;;; Integration with epsilon.http.request and epsilon.http.response
;;;
;;; The request/response slots remain plain epsilon.map for backwards
;;; compatibility with downstream code. Case-insensitive lookup is
;;; available via the request-header / response-header convenience
;;; accessors, and the parse paths uniformly store names in lowercase
;;; canonical form so plain (map:get headers "lowercase-key") works.

(deftest test-request-header-case-insensitive ()
  "request-header looks up case-insensitively, trying the supplied casing
   first then the lowercase canonical form."
  (let ((req (epsilon.http.request:make-request "POST" "/foo")))
    (epsilon.http.request:add-header req "Content-Type" "text/plain")
    (epsilon.http.request:add-header req "x-trace-id" "abc123")
    ;; Same-case lookups always work
    (assert-equal "text/plain"
                  (epsilon.http.request:request-header req "Content-Type"))
    ;; Lowercased lookup against a header stored with lowercase key works
    (assert-equal "abc123"
                  (epsilon.http.request:request-header req "x-trace-id"))
    ;; Different case lookups: server-parsed headers (lowercase) are
    ;; reachable via the lowercase fallback
    (assert-equal "abc123"
                  (epsilon.http.request:request-header req "X-Trace-Id"))))

(deftest test-response-header-case-insensitive ()
  "response-header looks up case-insensitively. set-header preserves the
   case the caller supplied (so the wire output uses canonical casing),
   but response-header still finds the entry."
  (let ((resp (epsilon.http.response:make-response :status 200)))
    (epsilon.http.response:set-header resp "Content-Type" "application/json")
    (epsilon.http.response:set-header resp "X-Server" "epsilon")
    (assert-equal "application/json"
                  (epsilon.http.response:response-header resp "content-type"))
    (assert-equal "application/json"
                  (epsilon.http.response:response-header resp "Content-Type"))
    (assert-equal "epsilon"
                  (epsilon.http.response:response-header resp "x-server"))))

(deftest test-make-request-from-header-map ()
  "make-request should accept a header-map and project it back to a plain
   epsilon.map keyed by lowercase canonical names."
  (let* ((h (headers:make-headers "Content-Type" "text/html"
                                  "X-Custom" "value"))
         (req (epsilon.http.request:make-request "GET" "/" :headers h)))
    (assert-equal "text/html"
                  (epsilon.http.request:request-header req "content-type"))
    (assert-equal "value"
                  (epsilon.http.request:request-header req "x-custom"))))

(deftest test-make-response-from-header-map ()
  "make-response should accept a header-map and project it back to a plain
   epsilon.map keyed by lowercase canonical names."
  (let* ((h (headers:make-headers "X-Powered-By" "epsilon"))
         (resp (epsilon.http.response:make-response :headers h)))
    (assert-equal "epsilon"
                  (epsilon.http.response:response-header resp "x-powered-by"))))

;;; Wire canonicalization

(deftest test-canonicalize-name-lowercase ()
  "All-lowercase names should be Title-Cased on each '-'-delimited segment."
  (assert-equal "X-Request-Id" (headers:canonicalize-name "x-request-id"))
  (assert-equal "Content-Type" (headers:canonicalize-name "content-type"))
  (assert-equal "Strict-Transport-Security"
                (headers:canonicalize-name "strict-transport-security")))

(deftest test-canonicalize-name-uppercase ()
  "All-uppercase names should be normalized to Title-Case."
  (assert-equal "X-Frame-Options" (headers:canonicalize-name "X-FRAME-OPTIONS"))
  (assert-equal "Content-Type" (headers:canonicalize-name "CONTENT-TYPE")))

(deftest test-canonicalize-name-mixed ()
  "Mixed casing should be normalized — first letter of each segment up,
   rest lower (Go-style textproto.CanonicalMIMEHeaderKey)."
  (assert-equal "X-Custom-Header" (headers:canonicalize-name "x-Custom-HEADER"))
  (assert-equal "Server" (headers:canonicalize-name "SERVER")))

(deftest test-canonicalize-name-already-canonical ()
  "Already-canonical names should be idempotent."
  (assert-equal "Content-Type" (headers:canonicalize-name "Content-Type"))
  (assert-equal "X-Request-Id" (headers:canonicalize-name "X-Request-Id")))

(deftest test-canonicalize-name-non-string-passthrough ()
  "Non-string keys are returned unchanged."
  (assert-equal :foo (headers:canonicalize-name :foo))
  (assert-equal 'bar (headers:canonicalize-name 'bar)))

(deftest test-add-header-preserves-supplied-case ()
  "add-header preserves the case the caller supplied so wire output uses
   canonical casing. request-header still finds the entry case-insensitively."
  (let ((req (epsilon.http.request:make-request "GET" "/")))
    (epsilon.http.request:add-header req "Content-Type" "text/plain")
    (assert-equal "text/plain"
                  (map:get (epsilon.http.request:request-headers req)
                           "Content-Type"))
    ;; Case-insensitive lookup also works
    (assert-equal "text/plain"
                  (epsilon.http.request:request-header req "content-type"))))
