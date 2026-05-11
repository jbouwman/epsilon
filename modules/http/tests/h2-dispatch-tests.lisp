;;;; H2 Dispatch Tests
;;;;
;;;; Unit tests for the HTTP/2 client integration in epsilon.http.client.
;;;; These tests cover the helper functions used by the dispatch path
;;;; (header conversion, response wrapping) without requiring a real H2
;;;; server. End-to-end H2 over TLS is exercised by tls-alpn-tests.

(defpackage :epsilon.http.h2-dispatch.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client client)
   (epsilon.http.headers headers)
   (epsilon.http.response response)
   (epsilon.map map)))

(in-package :epsilon.http.h2-dispatch.tests)

(deftest test-headers-to-h2-alist-from-header-map ()
  "Header-map should convert to a lowercase-named alist for H2."
  (let* ((h (headers:make-headers "Content-Type" "application/json"
                                  "X-Trace" "abc"))
         (alist (client::headers-to-h2-alist h)))
    (assert-equal "application/json"
                  (cdr (assoc "content-type" alist :test #'string=)))
    (assert-equal "abc"
                  (cdr (assoc "x-trace" alist :test #'string=)))))

(deftest test-headers-to-h2-alist-from-plain-map ()
  "Plain epsilon.map should also convert correctly."
  (let* ((m (map:make-map "Authorization" "Bearer xyz"))
         (alist (client::headers-to-h2-alist m)))
    (assert-equal "Bearer xyz"
                  (cdr (assoc "authorization" alist :test #'string=)))))

(deftest test-headers-to-h2-alist-from-nil ()
  "Nil headers should produce an empty alist, not error."
  (assert-nil (client::headers-to-h2-alist nil)))

(deftest test-headers-to-h2-alist-coerces-numeric-values ()
  "Numeric header values should be stringified for the wire."
  (let* ((h (headers:make-headers "Content-Length" 42))
         (alist (client::headers-to-h2-alist h)))
    (assert-equal "42"
                  (cdr (assoc "content-length" alist :test #'string=)))))

(deftest test-h2-response-to-http-response-basic ()
  "Wrapping an H2 plist should produce a valid http-response."
  (let* ((h2-resp (list :headers '((":status" . "200")
                                   ("content-type" . "text/plain"))
                        :body "Hello"))
         (resp (client::h2-response-to-http-response h2-resp)))
    (assert-equal 200 (response:response-status resp))
    (assert-equal "text/plain"
                  (map:get (response:response-headers resp) "content-type"))
    (assert-equal "Hello" (response:response-body resp))))

(deftest test-h2-response-to-http-response-strips-pseudo-headers ()
  "Pseudo-headers (`:status`, `:method`) should not appear in the
   response's HTTP headers."
  (let* ((h2-resp (list :headers '((":status" . "201")
                                   (":method" . "POST")
                                   ("location" . "/new"))
                        :body nil))
         (resp (client::h2-response-to-http-response h2-resp)))
    (assert-equal 201 (response:response-status resp))
    (assert-nil (map:get (response:response-headers resp) ":status"))
    (assert-nil (map:get (response:response-headers resp) ":method"))
    (assert-equal "/new"
                  (map:get (response:response-headers resp) "location"))))

(deftest test-h2-response-without-status ()
  "Defaults to 200 when no :status pseudo-header is present."
  (let* ((h2-resp (list :headers '(("server" . "test"))
                        :body "ok"))
         (resp (client::h2-response-to-http-response h2-resp)))
    (assert-equal 200 (response:response-status resp))))

(deftest test-h2-response-empty-body ()
  "An empty H2 body should be preserved as nil on the wrapper."
  (let* ((h2-resp (list :headers '((":status" . "204")) :body nil))
         (resp (client::h2-response-to-http-response h2-resp)))
    (assert-equal 204 (response:response-status resp))
    (assert-nil (response:response-body resp))))

(deftest test-h2-symbol-resolves ()
  "h2-symbol should successfully resolve known H2 entry points so the
   integration code can call them via fdefinition at runtime."
  (assert-not-null (client::h2-symbol "MAKE-HTTP2-CONNECTION"))
  (assert-not-null (client::h2-symbol "CREATE-STREAM"))
  (assert-not-null (client::h2-symbol "STREAM-SEND-HEADERS"))
  (assert-not-null (client::h2-symbol "READ-HTTP2-RESPONSE")))

(deftest test-h2-symbol-errors-on-missing ()
  "An unknown H2 symbol should signal an error rather than returning nil."
  (assert-condition (error)
    (client::h2-symbol "DEFINITELY-NOT-A-REAL-SYMBOL")))
