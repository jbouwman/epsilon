(defpackage epsilon.url-test
  (:use :cl :epsilon.test :epsilon.syntax :epsilon.url)
  (:import (epsilon.map map)))

;;;; ==========================================================================
;;;; URL Encoding/Decoding Tests
;;;; ==========================================================================

(deftest test-url-encode-component
  "Test URL component encoding"
  (assert-equal "hello%20world" (url-encode-component "hello world"))
  (assert-equal "foo%2Bbar" (url-encode-component "foo+bar"))
  (assert-equal "test%40example.com" (url-encode-component "test@example.com"))
  (assert-equal "simple" (url-encode-component "simple"))
  (assert-equal "" (url-encode-component ""))
  (assert-equal nil (url-encode-component nil)))

(deftest test-url-decode-component
  "Test URL component decoding"
  (assert-equal "hello world" (url-decode-component "hello%20world"))
  (assert-equal "foo+bar" (url-decode-component "foo%2Bbar"))
  (assert-equal "test@example.com" (url-decode-component "test%40example.com"))
  (assert-equal "hello world" (url-decode-component "hello+world"))
  (assert-equal "simple" (url-decode-component "simple"))
  (assert-equal "" (url-decode-component ""))
  (assert-equal nil (url-decode-component nil)))

(deftest test-url-encode-decode-roundtrip
  "Test URL encoding/decoding roundtrip"
  (let ((test-strings '("hello world" "foo+bar" "test@example.com" "path/to/file")))
    (dolist (str test-strings)
      (assert-equal str (url-decode-component (url-encode-component str))))))

;;;; ==========================================================================
;;;; URL Construction Tests
;;;; ==========================================================================

(deftest test-make-url-basic
  "Test basic URL construction"
  (let ((url (make-url :scheme "http" :host "example.com" :path "/test")))
    (assert-equal "http" (url-scheme url))
    (assert-equal "example.com" (url-host url))
    (assert-equal "/test" (url-path url))
    (assert-equal nil (url-port url))
    (assert-equal nil (url-query url))
    (assert-equal nil (url-fragment url))))

(deftest test-make-url-complete
  "Test complete URL construction"
  (let ((url (make-url :scheme "https"
                       :userinfo "user:pass"
                       :host "example.com"
                       :port 8080
                       :path "/api/v1/test"
                       :query "param=value"
                       :fragment "section")))
    (assert-equal "https" (url-scheme url))
    (assert-equal "user:pass" (url-userinfo url))
    (assert-equal "example.com" (url-host url))
    (assert-equal 8080 (url-port url))
    (assert-equal "/api/v1/test" (url-path url))
    (assert-equal "param=value" (url-query url))
    (assert-equal "section" (url-fragment url))))

(deftest test-url-string-simple
  "Test simple URL string generation"
  (let ((url (make-url :scheme "http" :host "example.com" :path "/test")))
    (assert-equal "http://example.com/test" (url-string url))))

(deftest test-url-string-complete
  "Test complete URL string generation"
  (let ((url (make-url :scheme "https"
                       :userinfo "user:pass"
                       :host "example.com"
                       :port 8080
                       :path "/api/v1/test"
                       :query "param=value&other=test"
                       :fragment "section")))
    (assert-equal "https://user:pass@example.com:8080/api/v1/test?param=value&other=test#section"
              (url-string url))))

(deftest test-url-string-default-ports
  "Test URL string generation with default ports"
  (let ((url-http (make-url :scheme "http" :host "example.com" :port 80 :path "/test"))
        (url-https (make-url :scheme "https" :host "example.com" :port 443 :path "/test")))
    (assert-equal "http://example.com/test" (url-string url-http))
    (assert-equal "https://example.com/test" (url-string url-https))))

;;;; ==========================================================================
;;;; URL Parsing Tests
;;;; ==========================================================================

(deftest test-parse-url-simple
  "Test simple URL parsing"
  (let ((url (parse-url "http://example.com/test")))
    (assert-equal "http" (url-scheme url))
    (assert-equal "example.com" (url-host url))
    (assert-equal "/test" (url-path url))
    (assert-equal nil (url-port url))
    (assert-equal nil (url-query url))
    (assert-equal nil (url-fragment url))))

(deftest test-parse-url-complete
  "Test complete URL parsing"
  (let ((url (parse-url "https://user:pass@example.com:8080/api/v1/test?param=value&other=test#section")))
    (assert-equal "https" (url-scheme url))
    (assert-equal "user:pass" (url-userinfo url))
    (assert-equal "example.com" (url-host url))
    (assert-equal 8080 (url-port url))
    (assert-equal "/api/v1/test" (url-path url))
    (assert-equal "param=value&other=test" (url-query url))
    (assert-equal "section" (url-fragment url))))

(deftest test-parse-url-no-scheme
  "Test parsing URL without scheme"
  (let ((url (parse-url "/relative/path")))
    (assert-equal nil (url-scheme url))
    (assert-equal nil (url-host url))
    (assert-equal "/relative/path" (url-path url))))

(deftest test-parse-url-query-only
  "Test parsing query-only URL"
  (let ((url (parse-url "?param=value")))
    (assert-equal nil (url-scheme url))
    (assert-equal nil (url-host url))
    (assert-equal "" (url-path url))
    (assert-equal "param=value" (url-query url))))

(deftest test-parse-url-fragment-only
  "Test parsing fragment-only URL"
  (let ((url (parse-url "#section")))
    (assert-equal nil (url-scheme url))
    (assert-equal nil (url-host url))
    (assert-equal "" (url-path url))
    (assert-equal nil (url-query url))
    (assert-equal "section" (url-fragment url))))

(deftest test-parse-url-edge-cases
  "Test URL parsing edge cases"
  (assert-equal nil (parse-url ""))
  (assert-equal nil (parse-url nil))
  (let ((url (parse-url "http://example.com")))
    (assert-equal "/" (url-path url))))

;;;; ==========================================================================
;;;; Query Parameter Tests
;;;; ==========================================================================

(deftest test-parse-query-string
  "Test query string parsing"
  (let ((params (parse-query-string "param1=value1&param2=value2&param3")))
    (assert-equal 3 (length params))
    (assert-equal "value1" (cdr (assoc "param1" params :test 'string=)))
    (assert-equal "value2" (cdr (assoc "param2" params :test 'string=)))
    (assert-equal nil (cdr (assoc "param3" params :test 'string=)))))

(deftest test-build-query-string
  "Test query string building"
  (let* ((params '(("param1" . "value1") ("param2" . "value2") ("param3" . nil)))
         (query (build-query-string params)))
    (assert-true (or (string= "param1=value1&param2=value2&param3" query)
            (string= "param2=value2&param1=value1&param3" query)
            (string= "param3&param1=value1&param2=value2" query)))))

(deftest test-query-params
  "Test query parameter extraction"
  (let* ((url (parse-url "http://example.com/test?param1=value1&param2=value2"))
         (params (query-params url)))
    (assert-equal 2 (length params))
    (assert-equal "value1" (cdr (assoc "param1" params :test 'string=)))
    (assert-equal "value2" (cdr (assoc "param2" params :test 'string=)))))

(deftest test-get-query-param
  "Test getting query parameters"
  (let ((url (parse-url "http://example.com/test?param1=value1&param2=value2")))
    (assert-equal "value1" (get-query-param url "param1"))
    (assert-equal "value2" (get-query-param url "param2"))
    (assert-equal nil (get-query-param url "nonexistent"))))

(deftest test-set-query-param
  "Test setting query parameters"
  (let* ((url (parse-url "http://example.com/test?param1=value1"))
         (new-url (set-query-param url "param2" "value2")))
    (assert-equal "value1" (get-query-param new-url "param1"))
    (assert-equal "value2" (get-query-param new-url "param2"))))

(deftest test-remove-query-param
  "Test removing query parameters"
  (let* ((url (parse-url "http://example.com/test?param1=value1&param2=value2"))
         (new-url (remove-query-param url "param1")))
    (assert-equal nil (get-query-param new-url "param1"))
    (assert-equal "value2" (get-query-param new-url "param2"))))

;;;; ==========================================================================
;;;; URL Manipulation Tests
;;;; ==========================================================================

(deftest test-url-absolute-p
  "Test URL absolute predicate"
  (assert-true (url-absolute-p (parse-url "http://example.com/test")))
  (assert-not (url-absolute-p (parse-url "/relative/path")))
  (assert-not (url-absolute-p (parse-url "relative/path"))))

(deftest test-url-relative-p
  "Test URL relative predicate"
  (assert-not (url-relative-p (parse-url "http://example.com/test")))
  (assert-true (url-relative-p (parse-url "/relative/path")))
  (assert-true (url-relative-p (parse-url "relative/path"))))

(deftest test-url-equal
  "Test URL equality"
  (assert-true (url-equal "http://example.com/test" "http://example.com/test"))
  (assert-not (url-equal "http://example.com/test" "https://example.com/test"))
  (assert-not (url-equal "http://example.com/test" "http://example.com/other"))
  (assert-true (url-equal (parse-url "http://example.com/test")
                 (parse-url "http://example.com/test"))))

(deftest test-url-normalize
  "Test URL normalization"
  (let* ((url (parse-url "http://example.com/path/to/../from/./file"))
         (normalized (url-normalize url)))
    (assert-equal "/path/from/file" (url-path normalized))))

(deftest test-url-join
  "Test URL joining"
  (let* ((base (parse-url "http://example.com/api/v1"))
         (joined (url-join base "users" "123" "profile")))
    (assert-equal "http://example.com/api/v1/users/123/profile" (url-string joined))))

;;;; ==========================================================================
;;;; Path <-> URL Conversion Tests
;;;; ==========================================================================

(deftest test-file-url-p
  "Test file URL predicate"
  (assert-true (file-url-p (parse-url "file:///path/to/file")))
  (assert-not (file-url-p (parse-url "http://example.com/file"))))

(deftest test-path-to-url
  "Test path to URL conversion"
  (let ((url (path-to-url "/usr/local/bin/test")))
    (assert-equal "file" (url-scheme url))
    (assert-equal "/usr/local/bin/test" (url-path url))))

(deftest test-url-to-path
  "Test URL to path conversion"
  (let ((path (url-to-path "file:///usr/local/bin/test")))
    (assert-true (typep path 'epsilon.fs:path))
    (assert-equal "/usr/local/bin/test" (epsilon.fs:path-string path))))

(deftest test-path-url-roundtrip
  "Test path-URL roundtrip conversion"
  (let* ((original-path "/usr/local/bin/test")
         (path (url-to-path (path-to-url original-path))))
    (assert-equal original-path (epsilon.fs:path-string path))))

;;;; ==========================================================================
;;;; Protocol Handler Tests
;;;; ==========================================================================

(deftest test-default-ports
  "Test default port lookup"
  (assert-equal 80 (default-port "http"))
  (assert-equal 443 (default-port "https"))
  (assert-equal 21 (default-port "ftp"))
  (assert-equal nil (default-port "unknown")))

(deftest test-protocol-handler-registration
  "Test protocol handler registration"
  (let ((handler (lambda (url) (format nil "Handled: ~A" url))))
    (register-protocol-handler "test" handler)
    (assert-equal handler (get-protocol-handler "test"))
    (assert-true (member "test" (supported-schemes) :test 'string=))
    (unregister-protocol-handler "test")
    (assert-equal nil (get-protocol-handler "test"))))

;;;; ==========================================================================
;;;; Validation Tests
;;;; ==========================================================================

(deftest test-valid-scheme-p
  "Test scheme validation"
  (assert-true (valid-scheme-p "http"))
  (assert-true (valid-scheme-p "https"))
  (assert-true (valid-scheme-p "ftp"))
  (assert-true (valid-scheme-p "custom-scheme"))
  (assert-not (valid-scheme-p "123invalid"))
  (assert-not (valid-scheme-p ""))
  (assert-not (valid-scheme-p nil)))

(deftest test-valid-host-p
  "Test host validation"
  (assert-true (valid-host-p "example.com"))
  (assert-true (valid-host-p "localhost"))
  (assert-true (valid-host-p "192.168.1.1"))
  (assert-not (valid-host-p ""))
  (assert-not (valid-host-p nil))
  (assert-not (valid-host-p "invalid host")))

(deftest test-valid-url-p
  "Test URL validation"
  (assert-true (valid-url-p "http://example.com/test"))
  (assert-true (valid-url-p "https://example.com:8080/test"))
  (assert-true (valid-url-p "/relative/path"))
  (assert-not (valid-url-p "123invalid://example.com"))
  (assert-not (valid-url-p "http://example.com:99999")))

;;;; ==========================================================================
;;;; Common Scheme Predicate Tests
;;;; ==========================================================================

(deftest test-scheme-predicates
  "Test common scheme predicates"
  (let ((http-url (parse-url "http://example.com/test"))
        (https-url (parse-url "https://example.com/test"))
        (ftp-url (parse-url "ftp://example.com/test"))
        (mailto-url (parse-url "mailto:test@example.com")))
    (assert-true (http-url-p http-url))
    (assert-not (http-url-p https-url))
    (assert-true (https-url-p https-url))
    (assert-not (https-url-p http-url))
    (assert-true (ftp-url-p ftp-url))
    (assert-not (ftp-url-p http-url))
    (assert-true (mailto-url-p mailto-url))
    (assert-not (mailto-url-p http-url))))

;;;; ==========================================================================
;;;; Map-Based Query Parameter Tests
;;;; ==========================================================================

(deftest test-build-query-string-from-map-basic
  "Test basic query string building from map"
  (let ((result (build-query-string-from-map (map:make-map "q" "test"))))
    (assert-equal "q=test" result))
  (let ((result (build-query-string-from-map (map:make-map "a" "1" "b" "2"))))
    (assert-true (or (string= "a=1&b=2" result)
                     (string= "b=2&a=1" result)))))

(deftest test-build-query-string-from-map-encoding
  "Test that special characters are percent-encoded"
  (let ((result (build-query-string-from-map (map:make-map "q" "hello world"))))
    (assert-equal "q=hello%20world" result))
  (let ((result (build-query-string-from-map (map:make-map "q" "a&b=c"))))
    (assert-equal "q=a%26b%3Dc" result)))

(deftest test-build-query-string-from-map-number-coercion
  "Test that numeric values are coerced to strings"
  (let ((result (build-query-string-from-map (map:make-map "limit" 20))))
    (assert-equal "limit=20" result)))

(deftest test-build-query-string-from-map-nil-skip
  "Test that nil values are omitted"
  (let ((result (build-query-string-from-map (map:make-map "a" "1" "b" nil))))
    (assert-equal "a=1" result)))

(deftest test-build-query-string-from-map-empty
  "Test that empty map returns nil"
  (assert-equal nil (build-query-string-from-map map:+empty+)))

(deftest test-url-with-params-basic
  "Test basic url-with-params"
  (let ((result (url-with-params "https://example.com/search"
                                 (map:make-map "q" "test"))))
    (assert-equal "https://example.com/search?q=test" result)))

(deftest test-url-with-params-existing-query
  "Test appending params when base URL already has query string"
  (let ((result (url-with-params "/items?sort=date"
                                 (map:make-map "page" 2))))
    (assert-equal "/items?sort=date&page=2" result)))

(deftest test-url-with-params-empty-map
  "Test that empty map returns base URL unchanged"
  (assert-equal "https://example.com" (url-with-params "https://example.com" map:+empty+)))

(deftest test-url-with-params-encoding
  "Test that url-with-params encodes special characters"
  (let ((result (url-with-params "https://example.com/search"
                                 (map:make-map "q" "hello world"))))
    (assert-equal "https://example.com/search?q=hello%20world" result)))

(deftest test-url-with-params-relative-path
  "Test url-with-params with relative paths"
  (let ((result (url-with-params "/search"
                                 (map:make-map "q" "test" "limit" 10))))
    (assert-true (search "q=test" result))
    (assert-true (search "limit=10" result))
    (assert-true (char= #\? (char result 7)))))

(deftest test-parse-query-string-to-map-basic
  "Test parsing query string to map and roundtrip"
  (let* ((original (map:make-map "a" "1" "b" "2"))
         (query (build-query-string-from-map original))
         (parsed (parse-query-string-to-map query)))
    (assert-equal "1" (map:get parsed "a"))
    (assert-equal "2" (map:get parsed "b"))))

(deftest test-parse-query-string-to-map-empty
  "Test that nil/empty query string returns empty map"
  (assert-equal map:+empty+ (parse-query-string-to-map nil))
  (assert-equal map:+empty+ (parse-query-string-to-map "")))
