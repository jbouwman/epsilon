;;;; Basic WebSocket Tests
;;;;
;;;; Simplified tests that work with the current implementation

(defpackage :epsilon.websocket.basic-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:ws #:epsilon.websocket)))

(in-package :epsilon.websocket.basic-tests)

;;; Basic structure tests

(deftest test-websocket-version ()
  "Test version string"
  (let ((version (ws:websocket-version)))
    (is (stringp version))
    (is (> (length version) 0))))

(deftest test-connection-pool-creation ()
  "Test connection pool creation"
  (let ((pool (ws:make-connection-pool :max-connections 10)))
    (is pool)
    (is-equal 10 (ws:connection-pool-max-connections pool))
    (is-equal 0 (ws:connection-pool-size pool))
    (ws:close-pool pool)))

(deftest test-metrics-collector ()
  "Test metrics collector"
  (let ((metrics (ws:make-metrics-collector)))
    (is metrics)
    (ws:record-connection-opened metrics)
    (is-equal 1 (ws:metrics-total-connections metrics))
    (is-equal 1 (ws:metrics-active-connections metrics))
    (ws:record-connection-closed metrics)
    (is-equal 0 (ws:metrics-active-connections metrics))))

(deftest test-frame-opcodes ()
  "Test frame opcode constants"
  (is-equal 0 ws:+opcode-continuation+)
  (is-equal 1 ws:+opcode-text+)
  (is-equal 2 ws:+opcode-binary+)
  (is-equal 8 ws:+opcode-close+)
  (is-equal 9 ws:+opcode-ping+)
  (is-equal 10 ws:+opcode-pong+))

(deftest test-close-codes ()
  "Test close code constants"
  (is-equal 1000 ws:+close-normal+)
  (is-equal 1001 ws:+close-going-away+)
  (is-equal 1002 ws:+close-protocol-error+)
  (is-equal 1003 ws:+close-unsupported-data+)
  (is-equal 1007 ws:+close-invalid-frame-payload+)
  (is-equal 1008 ws:+close-policy-violation+)
  (is-equal 1009 ws:+close-message-too-big+)
  (is-equal 1010 ws:+close-mandatory-extension+)
  (is-equal 1011 ws:+close-internal-error+))

(deftest test-timeout-error ()
  "Test timeout error condition"
  (handler-case
      (error 'ws:timeout-error :message "Test timeout")
    (ws:timeout-error (e)
      (is-equal "Test timeout" (ws:timeout-error-message e))
      (is t "Caught timeout error"))))

(deftest test-pool-exhausted-error ()
  "Test pool exhausted error condition"
  (handler-case
      (error 'ws:pool-exhausted-error :message "Pool full")
    (ws:pool-exhausted-error (e)
      (is (stringp (princ-to-string e)))
      (is t "Caught pool exhausted error"))))

;;; Run all tests
(defun run-tests ()
  "Run all basic WebSocket tests"
  (run-package-tests :epsilon.websocket.basic-tests))