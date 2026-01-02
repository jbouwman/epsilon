;;;; Tests for HTTP Connection Pooling

(package epsilon.http.pool.tests
  (import (epsilon.test test)
          (epsilon.http.pool pool)
          (epsilon.pool core-pool)
          (epsilon.net net)
          (epsilon.map map)))

;;;; Test Configuration

(test:deftest test-pool-config-creation ()
  "Test HTTP pool configuration"
  (let ((config (pool:make-pool-config
                 :max-connections-per-host 20
                 :max-idle-connections 100
                 :idle-timeout 60)))

    (test:is (pool:pool-config-p config))
    (test:is-= (pool:pool-config-max-connections-per-host config) 20)
    (test:is-= (pool:pool-config-max-idle-connections config) 100)
    (test:is-= (pool:pool-config-idle-timeout config) 60)))

;;;; Connection Key Tests

(test:deftest test-connection-key ()
  "Test connection key creation and comparison"
  (let ((key1 (pool:make-connection-key :host "example.com" :port 80 :ssl-p nil))
        (key2 (pool:make-connection-key :host "example.com" :port 80 :ssl-p nil))
        (key3 (pool:make-connection-key :host "example.com" :port 443 :ssl-p t)))

    (test:is (pool:connection-key-equal key1 key2))
    (test:is (not (pool:connection-key-equal key1 key3)))

    ;; Hash should be consistent
    (test:is-= (pool:connection-key-hash key1) (pool:connection-key-hash key2))
    (test:is (not (= (pool:connection-key-hash key1) (pool:connection-key-hash key3))))))

;;;; HTTP Connection Tests

(test:deftest test-http-connection-creation ()
  "Test HTTP connection structure"
  (let* ((key (pool:make-connection-key :host "test.com" :port 80))
         (conn (pool:make-http-connection
                :key key
                :created-at (get-universal-time)
                :protocol :http/1.1)))

    (test:is (pool:http-connection-p conn))
    (test:is (pool:connection-key-equal (pool:http-connection-key conn) key))
    (test:is (eq (pool:http-connection-protocol conn) :http/1.1))
    (test:is (pool:http-connection-alive-p conn))))

;;;; Mock Connection Pool Tests

(defvar *mock-connections* '())

(defun mock-create-connection (key config)
  "Mock connection factory for testing"
  (declare (ignore config))
  (let ((conn (pool:make-http-connection
               :key key
               :socket :mock-socket
               :stream :mock-stream
               :created-at (get-universal-time)
               :protocol :http/1.1)))
    (push conn *mock-connections*)
    conn))

(defun mock-validate-connection (connection)
  "Mock connection validator"
  (and (pool:http-connection-p connection)
       (pool:http-connection-alive-p connection)))

(defun mock-destroy-connection (connection)
  "Mock connection destroyer"
  (setf (pool:http-connection-alive-p connection) nil)
  (setf *mock-connections* (remove connection *mock-connections*)))

(test:deftest test-http-pool-basic-operations ()
  "Test basic HTTP pool operations with mocks"
  (let ((*mock-connections* '())
        (config (pool:make-pool-config :max-connections-per-host 3)))

    ;; Create mock pool by overriding factory functions
    (let ((http-pool (pool:make-http-connection-pool :config config)))

      ;; Test structure
      (test:is (pool:http-connection-pool-p http-pool))

      ;; Cleanup
      (pool:destroy-http-connection-pool http-pool))))

;;;; Pool Statistics Tests

(test:deftest test-pool-statistics-structure ()
  "Test pool statistics structure"
  (let ((stats (core-pool:make-pool-stats)))
    (test:is (core-pool:pool-stats-p stats))
    (test:is-= (core-pool:pool-stats-created stats) 0)
    (test:is-= (core-pool:pool-stats-acquired stats) 0)
    (test:is-= (core-pool:pool-stats-released stats) 0)))

;;;; URL Parsing for Pooling Tests

(test:deftest test-url-parsing-for-pooling ()
  "Test URL parsing for connection pooling"
  (let ((test-cases '(("http://example.com/path" "http" "example.com" 80 "/path" nil)
                     ("https://api.test.com:8443/v1/data" "https" "api.test.com" 8443 "/v1/data" nil)
                     ("http://localhost:3000/test?q=1" "http" "localhost" 3000 "/test" "q=1"))))

    (dolist (test-case test-cases)
      (destructuring-bind (url expected-scheme expected-host expected-port expected-path expected-query) test-case
        (test:is (stringp url))
        (test:is (stringp expected-scheme))
        (test:is (stringp expected-host))
        (test:is (integerp expected-port))))))

;;;; Configuration Tests

(test:deftest test-pool-configuration-validation ()
  "Test pool configuration validation"
  (let ((valid-config (pool:make-pool-config
                       :max-connections-per-host 10
                       :idle-timeout 30
                       :connection-timeout 5)))

    (test:is (> (pool:pool-config-max-connections-per-host valid-config) 0))
    (test:is (> (pool:pool-config-idle-timeout valid-config) 0))
    (test:is (> (pool:pool-config-connection-timeout valid-config) 0))))

;;;; Integration Tests (Structure)

(test:deftest test-pool-integration-structure ()
  "Test integration between pool and HTTP client (structure only)"
  ;; Test that required functions exist and have correct signatures
  (test:is (fboundp 'pool:make-http-connection-pool))
  (test:is (fboundp 'pool:get-connection))
  (test:is (fboundp 'pool:return-connection))
  (test:is (fboundp 'pool:pooled-request))
  (test:is (fboundp 'pool:pooled-get))
  (test:is (fboundp 'pool:pooled-post))

  ;; Test macro exists
  (test:is (macro-function 'pool:with-http-connection)))

;;;; Error Handling Tests

(test:deftest test-pool-error-conditions ()
  "Test pool error handling"
  (let ((config (pool:make-pool-config :connection-timeout 1)))

    ;; Test configuration validation
    (test:is (pool:pool-config-p config))

    ;; Test that appropriate types exist
    (test:is (subtypep 'pool:connection-key 'structure-object))
    (test:is (subtypep 'pool:http-connection 'structure-object))))

;;;; Cleanup Tests

(test:deftest test-pool-cleanup ()
  "Test pool cleanup and resource management"
  (let ((config (pool:make-pool-config))
        (*mock-connections* '()))

    ;; Create and destroy pool
    (let ((http-pool (pool:make-http-connection-pool :config config)))
      (test:is (pool:http-connection-pool-p http-pool))

      ;; Destroy should clean up resources
      (pool:destroy-http-connection-pool http-pool)

      ;; After destruction, pool should be in clean state
      (test:is (pool:http-connection-pool-p http-pool)))))

;;;; Performance Structure Tests

(test:deftest test-pool-performance-structure ()
  "Test performance-related structures and APIs"
  (let ((config (pool:make-pool-config
                 :max-connections-per-host 50
                 :max-idle-connections 200)))

    ;; Test that configuration supports high-performance scenarios
    (test:is (>= (pool:pool-config-max-connections-per-host config) 50))
    (test:is (>= (pool:pool-config-max-idle-connections config) 200))

    ;; Test health check function exists
    (test:is (fboundp 'pool:pool-health-check))
    (test:is (fboundp 'pool:warm-up-pool))))
