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