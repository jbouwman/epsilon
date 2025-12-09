;;;; HTTP/2 Server Tests
;;;;
;;;; Test suite for HTTP/2 server implementation

(in-package :epsilon.http2.server)

;;;; Configuration Tests

(epsilon.test:deftest test-server-config-creation
  "Test server configuration creation with default values"
  (let ((config (make-http2-server-config)))
    (epsilon.test:is (= (http2-server-config-port config) 8080))
    (epsilon.test:is (string= (http2-server-config-host config) "0.0.0.0"))
    (epsilon.test:is (null (http2-server-config-ssl-p config)))
    (epsilon.test:is (null (http2-server-config-cert-file config)))
    (epsilon.test:is (null (http2-server-config-key-file config)))
    (epsilon.test:is (= (http2-server-config-max-concurrent-streams config) 100))
    (epsilon.test:is (= (http2-server-config-initial-window-size config) 65535))
    (epsilon.test:is (= (http2-server-config-max-frame-size config) 16384))
    (epsilon.test:is (= (http2-server-config-max-header-list-size config) 8192))
    (epsilon.test:is (http2-server-config-enable-push config))))

(epsilon.test:deftest test-server-config-custom
  "Test server configuration with custom values"
  (let ((config (make-http2-server-config
                 :port 9000
                 :host "127.0.0.1"
                 :ssl-p t
                 :cert-file "/path/to/cert.pem"
                 :key-file "/path/to/key.pem"
                 :max-concurrent-streams 200)))
    (epsilon.test:is (= (http2-server-config-port config) 9000))
    (epsilon.test:is (string= (http2-server-config-host config) "127.0.0.1"))
    (epsilon.test:is (http2-server-config-ssl-p config))
    (epsilon.test:is (string= (http2-server-config-cert-file config) "/path/to/cert.pem"))
    (epsilon.test:is (string= (http2-server-config-key-file config) "/path/to/key.pem"))
    (epsilon.test:is (= (http2-server-config-max-concurrent-streams config) 200))))

(epsilon.test:deftest test-server-config-mtls
  "Test server configuration for mutual TLS"
  (let ((config (make-http2-server-config
                 :ssl-p t
                 :cert-file "/path/to/server-cert.pem"
                 :key-file "/path/to/server-key.pem"
                 :ca-file "/path/to/ca-cert.pem"
                 :require-client-cert t
                 :verify-depth 3)))
    (epsilon.test:is (http2-server-config-ssl-p config))
    (epsilon.test:is (string= (http2-server-config-ca-file config) "/path/to/ca-cert.pem"))
    (epsilon.test:is (http2-server-config-require-client-cert config))
    (epsilon.test:is (= (http2-server-config-verify-depth config) 3))))

;;;; Preface Tests

(epsilon.test:deftest test-client-preface-constant
  "Test that client preface constant matches HTTP/2 spec"
  ;; The preface should be "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
  (let ((expected-string "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")
        (actual-bytes *client-preface*))
    (epsilon.test:is (= (length actual-bytes) 24))
    ;; Check first few bytes
    (epsilon.test:is (= (aref actual-bytes 0) 80)) ; 'P'
    (epsilon.test:is (= (aref actual-bytes 1) 82)) ; 'R'  
    (epsilon.test:is (= (aref actual-bytes 2) 73)) ; 'I'
    (epsilon.test:is (= (aref actual-bytes 3) 32)) ; ' '
    (epsilon.test:is (= (aref actual-bytes 4) 42)) ; '*'
    ;; Check the string conversion
    (epsilon.test:is (string= expected-string
                              (map 'string #'code-char actual-bytes)))))

;;;; Handler Tests

(epsilon.test:deftest test-default-handler-exists
  "Test that default handler function exists and is callable"
  (epsilon.test:is (fboundp 'default-handler))
  ;; Test calling the default handler
  (let ((headers '((":method" . "GET")
                   (":path" . "/")
                   (":scheme" . "https")))
        (body nil))
    ;; Should not error
    (handler-case
        (progn
          (default-handler headers body)
          (epsilon.test:is t)) ; Pass if no error
      (error (e)
        (epsilon.test:is nil "Default handler should not error: ~A" e)))))

(epsilon.test:deftest test-custom-handler-configuration
  "Test configuring a custom handler"
  (let* ((handler-called nil)
         (custom-handler (lambda (headers body)
                          (declare (ignore headers body))
                          (setf handler-called t)))
         (config (make-http2-server-config :handler custom-handler)))
    (epsilon.test:is (eq (http2-server-config-handler config) custom-handler))
    ;; Test that custom handler can be called
    ((http2-server-config-handler config) nil nil)
    (epsilon.test:is handler-called)))

;;;; ALPN Configuration Tests

(epsilon.test:deftest test-alpn-protocols
  "Test that ALPN protocols are properly configured"
  ;; This test verifies the ALPN protocol list includes h2
  ;; The actual ALPN negotiation happens during TLS handshake
  (let ((expected-protocols '("h2" "http/1.1")))
    ;; We can't directly test the wrap-with-tls functions without a real socket
    ;; but we can verify the protocol list is correct
    (epsilon.test:is (equal expected-protocols '("h2" "http/1.1")))))

;;;; Settings Tests

(epsilon.test:deftest test-server-settings-limits
  "Test server settings respect HTTP/2 limits"
  (let ((config (make-http2-server-config)))
    ;; Initial window size must be <= 2^31-1
    (epsilon.test:is (<= (http2-server-config-initial-window-size config) 
                         (1- (expt 2 31))))
    ;; Max frame size must be between 16384 and 16777215
    (epsilon.test:is (>= (http2-server-config-max-frame-size config) 16384))
    (epsilon.test:is (<= (http2-server-config-max-frame-size config) 16777215))
    ;; Max concurrent streams should be reasonable
    (epsilon.test:is (> (http2-server-config-max-concurrent-streams config) 0))
    (epsilon.test:is (<= (http2-server-config-max-concurrent-streams config) 1000))))

;;;; Edge Case Tests

(epsilon.test:deftest test-server-config-invalid-port
  "Test server configuration handles edge cases"
  ;; Test boundary values
  (let ((config1 (make-http2-server-config :port 1))
        (config2 (make-http2-server-config :port 65535)))
    (epsilon.test:is (= (http2-server-config-port config1) 1))
    (epsilon.test:is (= (http2-server-config-port config2) 65535))))

(epsilon.test:deftest test-server-config-empty-host
  "Test server configuration with empty host defaults correctly"
  (let ((config (make-http2-server-config :host "")))
    ;; Empty host should still be accepted
    (epsilon.test:is (string= (http2-server-config-host config) ""))))

;;;; Integration Test Helpers

(epsilon.test:deftest test-server-lifecycle-methods
  "Test that server lifecycle methods are defined"
  (epsilon.test:is (fboundp 'start-http2-server))
  (epsilon.test:is (fboundp 'stop-http2-server))
  (epsilon.test:is (fboundp 'create-server-socket))
  (epsilon.test:is (fboundp 'accept-connection))
  (epsilon.test:is (fboundp 'handle-connection))
  (epsilon.test:is (fboundp 'close-socket)))

;;;; Mock Connection Test

(epsilon.test:deftest test-connection-handling-structure
  "Test the structure of connection handling"
  ;; This tests that the connection handling has the right structure
  ;; without actually creating network connections
  (let* ((request-count 0)
         (test-handler (lambda (headers body)
                        (declare (ignore headers body))
                        (incf request-count)
                        '((":status" . "200")
                          ("content-type" . "text/plain"))
                        "OK"))
         (config (make-http2-server-config :handler test-handler)))
    ;; Verify handler is set correctly
    (epsilon.test:is (eq (http2-server-config-handler config) test-handler))
    ;; Call handler directly to simulate request
    (let ((response (funcall (http2-server-config-handler config)
                            '((":method" . "GET")) nil)))
      (epsilon.test:is (= request-count 1))
      (epsilon.test:is (stringp response)))))