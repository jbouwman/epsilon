;;;; HTTP Cookie Store Tests
;;;;
;;;; Tests for RFC 6265 cookie storage, domain/path matching, and expiration.

(defpackage :epsilon.http.cookie.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client-builder cb)))

(deftest test-cookie-store-set-get ()
  "Test basic cookie storage and retrieval"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/" "session" "abc123")
    (assert-equal "abc123" (cb:cookie-store-get store "example.com" "/" "session"))))

(deftest test-cookie-store-set-with-flags ()
  "Test storing cookies with http-only and secure flags"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/" "token" "xyz"
                         :http-only t :secure t
                         :expires (+ (get-universal-time) 3600))
    (assert-equal "xyz" (cb:cookie-store-get store "example.com" "/" "token"))))

(deftest test-cookie-store-overwrite ()
  "Test that setting a cookie with the same key overwrites it"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/" "key" "value1")
    (cb:cookie-store-set store "example.com" "/" "key" "value2")
    (assert-equal "value2" (cb:cookie-store-get store "example.com" "/" "key"))))

(deftest test-cookie-store-clear ()
  "Test clearing all cookies"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/" "a" "1")
    (cb:cookie-store-set store "example.com" "/" "b" "2")
    (cb:cookie-store-clear store)
    (assert-true (null (cb:cookie-store-get store "example.com" "/" "a")))
    (assert-true (null (cb:cookie-store-get store "example.com" "/" "b")))))

(deftest test-cookie-store-remove-expired ()
  "Test removing expired cookies"
  (let ((store (cb:make-cookie-store))
        (past (- (get-universal-time) 1))
        (future (+ (get-universal-time) 3600)))
    (cb:cookie-store-set store "example.com" "/" "expired" "old" :expires past)
    (cb:cookie-store-set store "example.com" "/" "valid" "new" :expires future)
    (let ((removed (cb:cookie-store-remove-expired store)))
      (assert-equal 1 removed)
      (assert-true (null (cb:cookie-store-get store "example.com" "/" "expired")))
      (assert-equal "new" (cb:cookie-store-get store "example.com" "/" "valid")))))

(deftest test-domain-matching-exact ()
  "Test exact domain matching"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/" "sid" "123")
    (let ((cookies (cb:cookie-store-for-request store "https://example.com/page")))
      (assert-equal 1 (length cookies))
      (assert-equal "sid" (car (first cookies)))
      (assert-equal "123" (cdr (first cookies))))))

(deftest test-domain-matching-subdomain ()
  "Test domain matching with subdomains"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store ".example.com" "/" "sid" "123")
    ;; Should match subdomain
    (let ((cookies (cb:cookie-store-for-request store "https://www.example.com/page")))
      (assert-equal 1 (length cookies)))
    ;; Should match deep subdomain
    (let ((cookies (cb:cookie-store-for-request store "https://sub.www.example.com/")))
      (assert-equal 1 (length cookies)))
    ;; Should not match different domain
    (let ((cookies (cb:cookie-store-for-request store "https://notexample.com/")))
      (assert-equal 0 (length cookies)))))

(deftest test-path-matching ()
  "Test path matching"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/api" "token" "abc")
    ;; Exact path match
    (let ((cookies (cb:cookie-store-for-request store "https://example.com/api")))
      (assert-equal 1 (length cookies)))
    ;; Sub-path match
    (let ((cookies (cb:cookie-store-for-request store "https://example.com/api/users")))
      (assert-equal 1 (length cookies)))
    ;; Root path should not match /api cookie
    (let ((cookies (cb:cookie-store-for-request store "https://example.com/")))
      (assert-equal 0 (length cookies)))))

(deftest test-secure-cookie-on-http ()
  "Test that secure cookies are not sent over HTTP"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/" "secure-token" "secret" :secure t)
    ;; HTTPS should include cookie
    (let ((cookies (cb:cookie-store-for-request store "https://example.com/")))
      (assert-equal 1 (length cookies)))
    ;; HTTP should exclude secure cookie
    (let ((cookies (cb:cookie-store-for-request store "http://example.com/")))
      (assert-equal 0 (length cookies)))))

(deftest test-expired-cookies-not-returned ()
  "Test that expired cookies are not returned for requests"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/" "old" "stale"
                         :expires (- (get-universal-time) 1))
    (let ((cookies (cb:cookie-store-for-request store "https://example.com/")))
      (assert-equal 0 (length cookies)))))

(deftest test-multiple-cookies-for-request ()
  "Test returning multiple matching cookies"
  (let ((store (cb:make-cookie-store)))
    (cb:cookie-store-set store "example.com" "/" "a" "1")
    (cb:cookie-store-set store "example.com" "/" "b" "2")
    (cb:cookie-store-set store "example.com" "/api" "c" "3")
    (let ((cookies (cb:cookie-store-for-request store "https://example.com/api/data")))
      (assert-equal 3 (length cookies)))))

(deftest test-parse-set-cookie ()
  "Test Set-Cookie header parsing"
  (multiple-value-bind (name value domain path _expires http-only secure)
      (cb:parse-set-cookie "session=abc123; Domain=example.com; Path=/; HttpOnly; Secure")
    (declare (ignore _expires))
    (assert-equal "session" name)
    (assert-equal "abc123" value)
    (assert-equal "example.com" domain)
    (assert-equal "/" path)
    (assert-true http-only)
    (assert-true secure)))

(deftest test-parse-set-cookie-max-age ()
  "Test Set-Cookie parsing with Max-Age"
  (let ((before (get-universal-time)))
    (multiple-value-bind (name value _domain path expires _http-only _secure)
        (cb:parse-set-cookie "token=xyz; Max-Age=3600; Path=/api")
      (declare (ignore _domain _http-only _secure))
      (assert-equal "token" name)
      (assert-equal "xyz" value)
      (assert-equal "/api" path)
      ;; expires should be approximately now + 3600
      (assert-true (and expires
               (>= expires (+ before 3599))
               (<= expires (+ before 3601)))))))
