;;;; Debug Accept Key Generation

(defpackage epsilon.websocket.debug.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (handshake epsilon.websocket.handshake)))

(in-package epsilon.websocket.debug.tests)

(deftest test-accept-key-generation
    "Debug accept key generation"
  (let* ((test-key "dGhlIHNhbXBsZSBub25jZQ==")
         (expected "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
         (actual (handshake:generate-accept-key test-key)))
    (format t "~%Test key: ~A~%" test-key)
    (format t "Expected: ~A~%" expected)
    (format t "Actual:   ~A~%" actual)
    (is-equal expected actual)))