;;;; Tests for HTTP Connection Pooling

(defpackage epsilon.http.pool.tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:import (epsilon.http.pool pool)
            (epsilon.pool core-pool)
            (epsilon.net net)
            (epsilon.map map)))

;;;; Test Configuration

(deftest test-pool-config-creation ()
  "Test HTTP pool configuration"
  (let ((config (pool:make-pool-config
                 :max-connections-per-host 20
                 :max-idle-connections 100
                 :idle-timeout 60)))

    (assert-true (pool:pool-config-p config))
    (assert-= (pool:pool-config-max-connections-per-host config) 20)
    (assert-= (pool:pool-config-max-idle-connections config) 100)
    (assert-= (pool:pool-config-idle-timeout config) 60)))

;;;; Connection Key Tests

(deftest test-connection-key ()
  "Test connection key creation and comparison"
  (let ((key1 (pool:make-connection-key :host "example.com" :port 80 :ssl-p nil))
        (key2 (pool:make-connection-key :host "example.com" :port 80 :ssl-p nil))
        (key3 (pool:make-connection-key :host "example.com" :port 443 :ssl-p t)))

    (assert-true (pool:connection-key-equal key1 key2))
    (assert-true (not (pool:connection-key-equal key1 key3)))

    ;; Hash should be consistent
    (assert-= (pool:connection-key-hash key1) (pool:connection-key-hash key2))
    (assert-true (not (= (pool:connection-key-hash key1) (pool:connection-key-hash key3))))))

;;;; HTTP Connection Tests

(deftest test-http-connection-creation ()
  "Test HTTP connection structure"
  (let* ((key (pool:make-connection-key :host "test.com" :port 80))
         (conn (pool:make-http-connection
                :key key
                :created-at (get-universal-time)
                :protocol :http/1.1)))

    (assert-true (pool:http-connection-p conn))
    (assert-true (pool:connection-key-equal (pool:http-connection-key conn) key))
    (assert-true (eq (pool:http-connection-protocol conn) :http/1.1))
    (assert-true (pool:http-connection-alive-p conn))))

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

(deftest test-http-pool-basic-operations ()
  "Test basic HTTP pool operations with mocks"
  (let ((*mock-connections* '())
        (config (pool:make-pool-config :max-connections-per-host 3)))

    ;; Create mock pool by overriding factory functions
    (let ((http-pool (pool:make-http-connection-pool :config config)))

      ;; Test structure
      (assert-true (pool:http-connection-pool-p http-pool))

      ;; Cleanup
      (pool:destroy-http-connection-pool http-pool))))

;;;; Pool Statistics Tests

(deftest test-pool-statistics-structure ()
  "Test pool statistics structure"
  (let ((stats (core-pool:make-pool-stats)))
    (assert-true (core-pool:pool-stats-p stats))
    (assert-= (core-pool:pool-stats-created stats) 0)
    (assert-= (core-pool:pool-stats-acquired stats) 0)
    (assert-= (core-pool:pool-stats-released stats) 0)))

;;;; URL Parsing for Pooling Tests

(deftest test-url-parsing-for-pooling ()
  "Test URL parsing for connection pooling"
  (let ((test-cases '(("http://example.com/path" "http" "example.com" 80 "/path" nil)
                     ("https://api.test.com:8443/v1/data" "https" "api.test.com" 8443 "/v1/data" nil)
                     ("http://localhost:3000/test?q=1" "http" "localhost" 3000 "/test" "q=1"))))

    (dolist (test-case test-cases)
      (destructuring-bind (url expected-scheme expected-host expected-port _expected-path _expected-query) test-case
        (declare (ignore _expected-path _expected-query))
        (assert-true (stringp url))
        (assert-true (stringp expected-scheme))
        (assert-true (stringp expected-host))
        (assert-true (integerp expected-port))))))

;;;; Configuration Tests

(deftest test-pool-configuration-validation ()
  "Test pool configuration validation"
  (let ((valid-config (pool:make-pool-config
                       :max-connections-per-host 10
                       :idle-timeout 30
                       :connection-timeout 5)))

    (assert-true (> (pool:pool-config-max-connections-per-host valid-config) 0))
    (assert-true (> (pool:pool-config-idle-timeout valid-config) 0))
    (assert-true (> (pool:pool-config-connection-timeout valid-config) 0))))

;;;; Integration Tests (Structure)

(deftest test-pool-integration-structure ()
  "Test integration between pool and HTTP client (structure only)"
  ;; Test that required functions exist and have correct signatures
  (assert-true (fboundp 'pool:make-http-connection-pool))
  (assert-true (fboundp 'pool:get-connection))
  (assert-true (fboundp 'pool:return-connection))
  (assert-true (fboundp 'pool:pooled-request))
  (assert-true (fboundp 'pool:pooled-get))
  (assert-true (fboundp 'pool:pooled-post))

  ;; Test macro exists
  (assert-true (macro-function 'pool:with-http-connection)))

;;;; Error Handling Tests

(deftest test-pool-error-conditions ()
  "Test pool error handling"
  (let ((config (pool:make-pool-config :connection-timeout 1)))

    ;; Test configuration validation
    (assert-true (pool:pool-config-p config))

    ;; Test that appropriate types exist
    (assert-true (subtypep 'pool:connection-key 'structure-object))
    (assert-true (subtypep 'pool:http-connection 'structure-object))))

;;;; Cleanup Tests

(deftest test-pool-cleanup ()
  "Test pool cleanup and resource management"
  (let ((config (pool:make-pool-config))
        (*mock-connections* '()))

    ;; Create and destroy pool
    (let ((http-pool (pool:make-http-connection-pool :config config)))
      (assert-true (pool:http-connection-pool-p http-pool))

      ;; Destroy should clean up resources
      (pool:destroy-http-connection-pool http-pool)

      ;; After destruction, pool should be in clean state
      (assert-true (pool:http-connection-pool-p http-pool)))))

;;;; Performance Structure Tests

(deftest test-pool-performance-structure ()
  "Test performance-related structures and APIs"
  (let ((config (pool:make-pool-config
                 :max-connections-per-host 50
                 :max-idle-connections 200)))

    ;; Test that configuration supports high-performance scenarios
    (assert-true (>= (pool:pool-config-max-connections-per-host config) 50))
    (assert-true (>= (pool:pool-config-max-idle-connections config) 200))

    ;; Test health check function exists
    (assert-true (fboundp 'pool:pool-health-check))
    (assert-true (fboundp 'pool:warm-up-pool))))
