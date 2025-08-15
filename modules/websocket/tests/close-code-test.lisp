;;;; Close Code Encoding Test

(defpackage epsilon.websocket.close-code.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (ws epsilon.websocket)
   (str epsilon.string)))

(in-package epsilon.websocket.close-code.tests)

(deftest test-close-code-1000
    "Test encoding of close code 1000 (normal closure)"
  (let ((payload (ws:encode-close-payload ws:+close-normal+ "test")))
    ;; First two bytes should be 1000 in big-endian
    (is-equal 6 (length payload)) ; 2 bytes for code + 4 for "test"
    (is-equal #x03 (aref payload 0)) ; 1000 >> 8 = 3
    (is-equal #xE8 (aref payload 1)) ; 1000 & 0xFF = 232
    (is-equal 1000 (+ (* 256 (aref payload 0)) (aref payload 1)))
    (is-equal "test" (str:octets-to-string (subseq payload 2)))))

(deftest test-close-code-1001
    "Test encoding of close code 1001 (going away)"
  (let ((payload (ws:encode-close-payload ws:+close-going-away+)))
    (is-equal 2 (length payload)) ; Just the code, no reason
    (is-equal #x03 (aref payload 0)) ; 1001 >> 8 = 3
    (is-equal #xE9 (aref payload 1)) ; 1001 & 0xFF = 233
    (is-equal 1001 (+ (* 256 (aref payload 0)) (aref payload 1)))))

(deftest test-close-code-1002
    "Test encoding of close code 1002 (protocol error)"
  (let ((payload (ws:encode-close-payload ws:+close-protocol-error+ "Bad frame")))
    (is (> (length payload) 2))
    (is-equal #x03 (aref payload 0)) ; 1002 >> 8 = 3
    (is-equal #xEA (aref payload 1)) ; 1002 & 0xFF = 234
    (is-equal 1002 (+ (* 256 (aref payload 0)) (aref payload 1)))
    (is-equal "Bad frame" (str:octets-to-string (subseq payload 2)))))