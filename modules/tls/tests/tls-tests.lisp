(defpackage :epsilon.tls.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:tls #:epsilon.tls)
   (#:map #:epsilon.map)))

(in-package :epsilon.tls.tests)

(deftest test-tls-context-creation ()
  "Test TLS context creation"
  (let ((client-ctx (tls:create-tls-context :server-p nil))
        (server-ctx (tls:create-tls-context :server-p t)))
    (is (tls:tls-context-p client-ctx))
    (is (tls:tls-context-p server-ctx))
    (is-not (tls:tls-context-server-p client-ctx))
    (is (tls:tls-context-server-p server-ctx))))

(deftest test-tls-context-configuration ()
  "Test TLS context configuration"
  (let ((ctx (tls:create-tls-context :server-p t)))
    (tls:load-cert-file ctx "/path/to/cert.pem")
    (tls:load-key-file ctx "/path/to/key.pem")
    (tls:set-verify-mode ctx tls:+tls-verify-none+)
    
    (is-equal "/path/to/cert.pem" (tls:tls-context-cert-file ctx))
    (is-equal "/path/to/key.pem" (tls:tls-context-key-file ctx))
    (is-equal tls:+tls-verify-none+ (tls:tls-context-verify-mode ctx))))

(deftest test-tls-connection-creation ()
  "Test TLS connection creation (stubbed)"
  (let* ((ctx (tls:create-tls-context :server-p nil))
         (mock-socket :mock-socket)
         (conn (tls:tls-connect mock-socket ctx)))
    (is (tls:tls-connection-p conn))
    (is (tls:tls-connection-connected-p conn))
    (is (tls:tls-connection-handshake-complete-p conn))
    (is-equal :mock-socket (tls:tls-connection-socket conn))))

(deftest test-tls-handshake-status ()
  "Test TLS handshake status checking"
  (let* ((ctx (tls:create-tls-context :server-p nil))
         (mock-socket :mock-socket)
         (conn (tls:tls-connect mock-socket ctx)))
    (is-equal :complete (tls:tls-handshake conn))))

(deftest test-tls-version-and-cipher ()
  "Test TLS version and cipher information"
  (let* ((ctx (tls:create-tls-context :server-p nil))
         (mock-socket :mock-socket)
         (conn (tls:tls-connect mock-socket ctx)))
    (is (stringp (tls:tls-version conn)))
    (is (stringp (tls:tls-cipher conn)))
    (is (search "TLS" (tls:tls-version conn)))
    (is (search "AES" (tls:tls-cipher conn)))))

(deftest test-peer-certificate ()
  "Test peer certificate information"
  (let* ((ctx (tls:create-tls-context :server-p nil))
         (mock-socket :mock-socket)
         (conn (tls:tls-connect mock-socket ctx))
         (cert-info (tls:get-peer-certificate conn)))
    (is (map:map-p cert-info))
    (is (map:get cert-info "subject"))
    (is (map:get cert-info "issuer"))
    (is (search "CN=" (map:get cert-info "subject")))))

(deftest test-verify-constants ()
  "Test TLS verification constants"
  (is (numberp tls:+tls-verify-none+))
  (is (numberp tls:+tls-verify-peer+))
  (is-not (= tls:+tls-verify-none+ tls:+tls-verify-peer+)))

;; Enhanced Mock Tests

(deftest test-mock-tls-handshake-failure ()
  "Test mock TLS handshake failure simulation"
  (tls:with-mock-tls (:simulate-handshake-failure t)
    (let* ((ctx (tls:create-tls-context :server-p nil))
           (conn (tls:tls-connect :mock-socket ctx)))
      (is (tls:mock-tls-connection-p conn))
      (is-not (tls:mock-tls-connection-handshake-complete-p conn))
      (is-equal :failed (tls:simulate-tls-handshake conn)))))

(deftest test-mock-certificate-error ()
  "Test mock certificate verification error"
  (tls:with-mock-tls (:simulate-cert-error t)
    (let* ((ctx (tls:create-tls-context :server-p nil))
           (conn (tls:tls-connect :mock-socket ctx)))
      (is (tls:mock-tls-connection-p conn))
      (is-equal :cert-error (tls:simulate-tls-handshake conn)))))

(deftest test-mock-network-error ()
  "Test mock network error simulation" 
  (tls:with-mock-tls (:simulate-network-error t)
    (let* ((ctx (tls:create-tls-context :server-p nil))
           (conn (tls:tls-connect :mock-socket ctx)))
      (is-thrown (error) (tls:mock-tls-read conn (make-array 100))))))

(deftest test-mock-data-injection ()
  "Test mock data injection and retrieval"
  (tls:with-mock-tls ()
    (let* ((ctx (tls:create-tls-context :server-p nil))
           (conn (tls:tls-connect :mock-socket ctx))
           (test-data "Hello, TLS World!")
           (buffer (make-array 100 :element-type '(unsigned-byte 8))))
      
      ;; Inject test data
      (tls:inject-mock-data conn test-data)
      
      ;; Read it back
      (let ((bytes-read (tls:mock-tls-read conn buffer 0 (length test-data))))
        (is-equal (length test-data) bytes-read)
        (is-equal test-data (map 'string #'code-char (subseq buffer 0 bytes-read)))))))

(deftest test-mock-write-operations ()
  "Test mock write operations and data capture"
  (tls:with-mock-tls ()
    (let* ((ctx (tls:create-tls-context :server-p nil))
           (conn (tls:tls-connect :mock-socket ctx))
           (test-data "TLS write test data"))
      
      ;; Write test data
      (tls:mock-tls-write conn (map 'vector #'char-code test-data))
      
      ;; Retrieve written data
      (let ((written (tls:get-mock-written-data conn :as-string t)))
        (is-equal test-data written)))))

(deftest test-custom-mock-certificate ()
  "Test custom mock certificate data"
  (let ((custom-cert (map:make-map 
                      "subject" "CN=custom.test.com"
                      "issuer" "CN=Custom Test CA"
                      "serial" "999888777")))
    (tls:with-mock-tls (:custom-cert-data custom-cert)
      (let* ((ctx (tls:create-tls-context :server-p nil))
             (conn (tls:tls-connect :mock-socket ctx))
             (cert-info (tls:get-mock-certificate-data)))
        (is-equal "CN=custom.test.com" (map:get cert-info "subject"))
        (is-equal "CN=Custom Test CA" (map:get cert-info "issuer"))
        (is-equal "999888777" (map:get cert-info "serial"))))))

(deftest test-mock-performance-metrics ()
  "Test mock performance metrics collection"
  (tls:with-mock-tls ()
    (let* ((ctx (tls:create-tls-context :server-p nil))
           (conn (tls:tls-connect :mock-socket ctx)))
      
      ;; Perform some mock I/O
      (tls:inject-mock-data conn "test")
      (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
        (tls:mock-tls-read conn buffer))
      (tls:mock-tls-write conn #(1 2 3 4))
      
      ;; Check metrics
      (let ((metrics (tls:mock-performance-metrics conn)))
        (is (map:map-p metrics))
        (is-equal 4 (map:get metrics "bytes-read"))
        (is-equal 4 (map:get metrics "bytes-written"))
        (is (numberp (map:get metrics "handshake-time-ms")))))))

(deftest test-mock-buffer-management ()
  "Test mock buffer clear and management"
  (tls:with-mock-tls ()
    (let* ((ctx (tls:create-tls-context :server-p nil))
           (conn (tls:tls-connect :mock-socket ctx)))
      
      ;; Add data to both buffers
      (tls:inject-mock-data conn "read data")
      (tls:mock-tls-write conn (map 'vector #'char-code "write data"))
      
      ;; Verify data is there
      (is (> (length (tls:get-mock-written-data conn)) 0))
      
      ;; Clear buffers
      (tls:clear-mock-buffers conn)
      
      ;; Verify buffers are empty
      (is-equal 0 (length (tls:get-mock-written-data conn))))))

(deftest test-custom-tls-version-and-cipher ()
  "Test custom TLS version and cipher in mock mode"
  (tls:with-mock-tls (:custom-version "TLS 1.2" :custom-cipher "ECDHE-RSA-AES128-GCM-SHA256")
    (let* ((ctx (tls:create-tls-context :server-p nil))
           (conn (tls:tls-connect :mock-socket ctx)))
      (is-equal "TLS 1.2" (tls:tls-version conn))
      (is-equal "ECDHE-RSA-AES128-GCM-SHA256" (tls:tls-cipher conn)))))