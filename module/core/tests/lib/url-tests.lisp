(defpackage :epsilon.lib.url.tests
  (:use :cl :epsilon.test :epsilon.lib.url))

(in-package :epsilon.lib.url.tests)

;;;; ==========================================================================
;;;; URL Encoding/Decoding Tests
;;;; ==========================================================================

(deftest test-url-encode-component
  "Test URL component encoding"
  (is-equal "hello%20world" (url-encode-component "hello world"))
  (is-equal "foo%2Bbar" (url-encode-component "foo+bar"))
  (is-equal "test%40example.com" (url-encode-component "test@example.com"))
  (is-equal "simple" (url-encode-component "simple"))
  (is-equal "" (url-encode-component ""))
  (is-equal nil (url-encode-component nil)))

(deftest test-url-decode-component
  "Test URL component decoding"
  (is-equal "hello world" (url-decode-component "hello%20world"))
  (is-equal "foo+bar" (url-decode-component "foo%2Bbar"))
  (is-equal "test@example.com" (url-decode-component "test%40example.com"))
  (is-equal "hello world" (url-decode-component "hello+world"))
  (is-equal "simple" (url-decode-component "simple"))
  (is-equal "" (url-decode-component ""))
  (is-equal nil (url-decode-component nil)))

(deftest test-url-encode-decode-roundtrip
  "Test URL encoding/decoding roundtrip"
  (let ((test-strings '("hello world" "foo+bar" "test@example.com" "path/to/file")))
    (dolist (str test-strings)
      (is-equal str (url-decode-component (url-encode-component str))))))

;;;; ==========================================================================
;;;; URL Construction Tests
;;;; ==========================================================================

(deftest test-make-url-basic
  "Test basic URL construction"
  (let ((url (make-url :scheme "http" :host "example.com" :path "/test")))
    (is-equal "http" (url-scheme url))
    (is-equal "example.com" (url-host url))
    (is-equal "/test" (url-path url))
    (is-equal nil (url-port url))
    (is-equal nil (url-query url))
    (is-equal nil (url-fragment url))))

(deftest test-make-url-complete
  "Test complete URL construction"
  (let ((url (make-url :scheme "https" 
                       :userinfo "user:pass"
                       :host "example.com" 
                       :port 8080
                       :path "/api/v1/test"
                       :query "param=value"
                       :fragment "section")))
    (is-equal "https" (url-scheme url))
    (is-equal "user:pass" (url-userinfo url))
    (is-equal "example.com" (url-host url))
    (is-equal 8080 (url-port url))
    (is-equal "/api/v1/test" (url-path url))
    (is-equal "param=value" (url-query url))
    (is-equal "section" (url-fragment url))))

(deftest test-url-string-simple
  "Test simple URL string generation"
  (let ((url (make-url :scheme "http" :host "example.com" :path "/test")))
    (is-equal "http://example.com/test" (url-string url))))

(deftest test-url-string-complete
  "Test complete URL string generation"
  (let ((url (make-url :scheme "https" 
                       :userinfo "user:pass"
                       :host "example.com" 
                       :port 8080
                       :path "/api/v1/test"
                       :query "param=value&other=test"
                       :fragment "section")))
    (is-equal "https://user:pass@example.com:8080/api/v1/test?param=value&other=test#section" 
              (url-string url))))

(deftest test-url-string-default-ports
  "Test URL string generation with default ports"
  (let ((url-http (make-url :scheme "http" :host "example.com" :port 80 :path "/test"))
        (url-https (make-url :scheme "https" :host "example.com" :port 443 :path "/test")))
    (is-equal "http://example.com/test" (url-string url-http))
    (is-equal "https://example.com/test" (url-string url-https))))

;;;; ==========================================================================
;;;; URL Parsing Tests
;;;; ==========================================================================

(deftest test-parse-url-simple
  "Test simple URL parsing"
  (let ((url (parse-url "http://example.com/test")))
    (is-equal "http" (url-scheme url))
    (is-equal "example.com" (url-host url))
    (is-equal "/test" (url-path url))
    (is-equal nil (url-port url))
    (is-equal nil (url-query url))
    (is-equal nil (url-fragment url))))

(deftest test-parse-url-complete
  "Test complete URL parsing"
  (let ((url (parse-url "https://user:pass@example.com:8080/api/v1/test?param=value&other=test#section")))
    (is-equal "https" (url-scheme url))
    (is-equal "user:pass" (url-userinfo url))
    (is-equal "example.com" (url-host url))
    (is-equal 8080 (url-port url))
    (is-equal "/api/v1/test" (url-path url))
    (is-equal "param=value&other=test" (url-query url))
    (is-equal "section" (url-fragment url))))

(deftest test-parse-url-no-scheme
  "Test parsing URL without scheme"
  (let ((url (parse-url "/relative/path")))
    (is-equal nil (url-scheme url))
    (is-equal nil (url-host url))
    (is-equal "/relative/path" (url-path url))))

(deftest test-parse-url-query-only
  "Test parsing query-only URL"
  (let ((url (parse-url "?param=value")))
    (is-equal nil (url-scheme url))
    (is-equal nil (url-host url))
    (is-equal "" (url-path url))
    (is-equal "param=value" (url-query url))))

(deftest test-parse-url-fragment-only
  "Test parsing fragment-only URL"
  (let ((url (parse-url "#section")))
    (is-equal nil (url-scheme url))
    (is-equal nil (url-host url))
    (is-equal "" (url-path url))
    (is-equal nil (url-query url))
    (is-equal "section" (url-fragment url))))

(deftest test-parse-url-edge-cases
  "Test URL parsing edge cases"
  (is-equal nil (parse-url ""))
  (is-equal nil (parse-url nil))
  (let ((url (parse-url "http://example.com")))
    (is-equal "/" (url-path url))))

;;;; ==========================================================================
;;;; Query Parameter Tests
;;;; ==========================================================================

(deftest test-parse-query-string
  "Test query string parsing"
  (let ((params (parse-query-string "param1=value1&param2=value2&param3")))
    (is-equal 3 (length params))
    (is-equal "value1" (cdr (assoc "param1" params :test 'string=)))
    (is-equal "value2" (cdr (assoc "param2" params :test 'string=)))
    (is-equal nil (cdr (assoc "param3" params :test 'string=)))))

(deftest test-build-query-string
  "Test query string building"
  (let* ((params '(("param1" . "value1") ("param2" . "value2") ("param3" . nil)))
         (query (build-query-string params)))
    (is (or (string= "param1=value1&param2=value2&param3" query)
            (string= "param2=value2&param1=value1&param3" query)
            (string= "param3&param1=value1&param2=value2" query)))))

(deftest test-query-params
  "Test query parameter extraction"
  (let* ((url (parse-url "http://example.com/test?param1=value1&param2=value2"))
         (params (query-params url)))
    (is-equal 2 (length params))
    (is-equal "value1" (cdr (assoc "param1" params :test 'string=)))
    (is-equal "value2" (cdr (assoc "param2" params :test 'string=)))))

(deftest test-get-query-param
  "Test getting query parameters"
  (let ((url (parse-url "http://example.com/test?param1=value1&param2=value2")))
    (is-equal "value1" (get-query-param url "param1"))
    (is-equal "value2" (get-query-param url "param2"))
    (is-equal nil (get-query-param url "nonexistent"))))

(deftest test-set-query-param
  "Test setting query parameters"
  (let* ((url (parse-url "http://example.com/test?param1=value1"))
         (new-url (set-query-param url "param2" "value2")))
    (is-equal "value1" (get-query-param new-url "param1"))
    (is-equal "value2" (get-query-param new-url "param2"))))

(deftest test-remove-query-param
  "Test removing query parameters"
  (let* ((url (parse-url "http://example.com/test?param1=value1&param2=value2"))
         (new-url (remove-query-param url "param1")))
    (is-equal nil (get-query-param new-url "param1"))
    (is-equal "value2" (get-query-param new-url "param2"))))

;;;; ==========================================================================
;;;; URL Manipulation Tests
;;;; ==========================================================================

(deftest test-url-absolute-p
  "Test URL absolute predicate"
  (is (url-absolute-p (parse-url "http://example.com/test")))
  (is-not (url-absolute-p (parse-url "/relative/path")))
  (is-not (url-absolute-p (parse-url "relative/path"))))

(deftest test-url-relative-p
  "Test URL relative predicate"
  (is-not (url-relative-p (parse-url "http://example.com/test")))
  (is (url-relative-p (parse-url "/relative/path")))
  (is (url-relative-p (parse-url "relative/path"))))

(deftest test-url-equal
  "Test URL equality"
  (is (url-equal "http://example.com/test" "http://example.com/test"))
  (is-not (url-equal "http://example.com/test" "https://example.com/test"))
  (is-not (url-equal "http://example.com/test" "http://example.com/other"))
  (is (url-equal (parse-url "http://example.com/test") 
                 (parse-url "http://example.com/test"))))

(deftest test-url-normalize
  "Test URL normalization"
  (let* ((url (parse-url "http://example.com/path/to/../from/./file"))
         (normalized (url-normalize url)))
    (is-equal "/path/from/file" (url-path normalized))))

(deftest test-url-join
  "Test URL joining"
  (let* ((base (parse-url "http://example.com/api/v1"))
         (joined (url-join base "users" "123" "profile")))
    (is-equal "http://example.com/api/v1/users/123/profile" (url-string joined))))

;;;; ==========================================================================
;;;; Path <-> URL Conversion Tests
;;;; ==========================================================================

(deftest test-file-url-p
  "Test file URL predicate"
  (is (file-url-p (parse-url "file:///path/to/file")))
  (is-not (file-url-p (parse-url "http://example.com/file"))))

(deftest test-path-to-url
  "Test path to URL conversion"
  (let ((url (path-to-url "/usr/local/bin/test")))
    (is-equal "file" (url-scheme url))
    (is-equal "/usr/local/bin/test" (url-path url))))

(deftest test-url-to-path
  "Test URL to path conversion"
  (let ((path (url-to-path "file:///usr/local/bin/test")))
    (is (typep path 'epsilon.lib.path:path))
    (is-equal "/usr/local/bin/test" (epsilon.lib.path:path-string path))))

(deftest test-path-url-roundtrip
  "Test path-URL roundtrip conversion"
  (let* ((original-path "/usr/local/bin/test")
         (path (url-to-path (path-to-url original-path))))
    (is-equal original-path (epsilon.lib.path:path-string path))))

;;;; ==========================================================================
;;;; Protocol Handler Tests
;;;; ==========================================================================

(deftest test-default-ports
  "Test default port lookup"
  (is-equal 80 (default-port "http"))
  (is-equal 443 (default-port "https"))
  (is-equal 21 (default-port "ftp"))
  (is-equal nil (default-port "unknown")))

(deftest test-protocol-handler-registration
  "Test protocol handler registration"
  (let ((handler (lambda (url) (format nil "Handled: ~A" url))))
    (register-protocol-handler "test" handler)
    (is-equal handler (get-protocol-handler "test"))
    (is (member "test" (supported-schemes) :test 'string=))
    (unregister-protocol-handler "test")
    (is-equal nil (get-protocol-handler "test"))))

;;;; ==========================================================================
;;;; Validation Tests
;;;; ==========================================================================

(deftest test-valid-scheme-p
  "Test scheme validation"
  (is (valid-scheme-p "http"))
  (is (valid-scheme-p "https"))
  (is (valid-scheme-p "ftp"))
  (is (valid-scheme-p "custom-scheme"))
  (is-not (valid-scheme-p "123invalid"))
  (is-not (valid-scheme-p ""))
  (is-not (valid-scheme-p nil)))

(deftest test-valid-host-p
  "Test host validation"
  (is (valid-host-p "example.com"))
  (is (valid-host-p "localhost"))
  (is (valid-host-p "192.168.1.1"))
  (is-not (valid-host-p ""))
  (is-not (valid-host-p nil))
  (is-not (valid-host-p "invalid host")))

(deftest test-valid-url-p
  "Test URL validation"
  (is (valid-url-p "http://example.com/test"))
  (is (valid-url-p "https://example.com:8080/test"))
  (is (valid-url-p "/relative/path"))
  (is-not (valid-url-p "123invalid://example.com"))
  (is-not (valid-url-p "http://example.com:99999")))

;;;; ==========================================================================
;;;; Common Scheme Predicate Tests
;;;; ==========================================================================

(deftest test-scheme-predicates
  "Test common scheme predicates"
  (let ((http-url (parse-url "http://example.com/test"))
        (https-url (parse-url "https://example.com/test"))
        (ftp-url (parse-url "ftp://example.com/test"))
        (mailto-url (parse-url "mailto:test@example.com")))
    (is (http-url-p http-url))
    (is-not (http-url-p https-url))
    (is (https-url-p https-url))
    (is-not (https-url-p http-url))
    (is (ftp-url-p ftp-url))
    (is-not (ftp-url-p http-url))
    (is (mailto-url-p mailto-url))
    (is-not (mailto-url-p http-url))))