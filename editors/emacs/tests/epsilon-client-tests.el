;;; epsilon-client-tests.el --- Tests for epsilon-client -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Epsilon protocol client.

;;; Code:

(require 'ert)
(require 'epsilon-client)

;;; Frame Codec Tests

(ert-deftest epsilon-test-encode-frame-simple ()
  "Test encoding a simple JSON message."
  (let* ((json "{\"id\":\"1\"}")
         (frame (epsilon--encode-frame json)))
    ;; Frame should be 4 bytes length + JSON
    (should (= (length frame) (+ 4 (length json))))
    ;; First 4 bytes should be length in big-endian
    (should (= (aref frame 0) 0))
    (should (= (aref frame 1) 0))
    (should (= (aref frame 2) 0))
    (should (= (aref frame 3) (length json)))))

(ert-deftest epsilon-test-encode-frame-utf8 ()
  "Test encoding a JSON message with UTF-8 content."
  (let* ((json "{\"text\":\"hello\"}")
         (frame (epsilon--encode-frame json))
         (expected-len (length (encode-coding-string json 'utf-8))))
    ;; Frame should be 4 bytes + UTF-8 encoded length
    (should (= (length frame) (+ 4 expected-len)))))

(ert-deftest epsilon-test-decode-frame-length ()
  "Test decoding frame length from 4-byte header."
  (should (= (epsilon--decode-frame-length (unibyte-string 0 0 0 10)) 10))
  (should (= (epsilon--decode-frame-length (unibyte-string 0 0 1 0)) 256))
  (should (= (epsilon--decode-frame-length (unibyte-string 0 1 0 0)) 65536))
  (should (= (epsilon--decode-frame-length (unibyte-string 1 0 0 0)) 16777216)))

(ert-deftest epsilon-test-decode-frame-incomplete ()
  "Test that incomplete frames return nil."
  ;; Too short for header
  (should-not (epsilon--decode-frame "abc"))
  ;; Header says 10 bytes but only 5 provided
  (should-not (epsilon--decode-frame (concat (unibyte-string 0 0 0 10) "hello"))))

(ert-deftest epsilon-test-frame-roundtrip ()
  "Test frame encoding/decoding roundtrip."
  (let* ((json "{\"id\":\"1\",\"type\":\"request\"}")
         (frame (epsilon--encode-frame json))
         (decoded (epsilon--decode-frame frame)))
    (should decoded)
    (should (equal (car decoded) json))
    (should (equal (cdr decoded) ""))))

(ert-deftest epsilon-test-frame-roundtrip-with-remainder ()
  "Test frame decoding with remaining data."
  (let* ((json1 "{\"id\":\"1\"}")
         (json2 "{\"id\":\"2\"}")
         (frame1 (epsilon--encode-frame json1))
         (frame2 (epsilon--encode-frame json2))
         (combined (concat frame1 frame2))
         (decoded (epsilon--decode-frame combined)))
    (should decoded)
    (should (equal (car decoded) json1))
    (should (equal (cdr decoded) frame2))))

;;; Connection State Tests

(ert-deftest epsilon-test-connected-p-initial ()
  "Test that connected-p returns nil when not connected."
  (let ((epsilon--connection nil))
    (should-not (epsilon-connected-p))))

;;; Utility Function Tests

(ert-deftest epsilon-test-symbol-at-point ()
  "Test symbol extraction at point."
  (with-temp-buffer
    (insert "(defun foo (x) x)")
    (goto-char 8) ; on "foo"
    (should (equal (epsilon--symbol-at-point) "foo"))))

(ert-deftest epsilon-test-current-package ()
  "Test package detection."
  (with-temp-buffer
    (insert "(in-package :my-package)\n(defun foo () nil)")
    (should (equal (epsilon--current-package) "my-package")))
  (with-temp-buffer
    (insert "(defun foo () nil)")
    (should (equal (epsilon--current-package) "epsilon.user"))))

(ert-deftest epsilon-test-sexp-before-point ()
  "Test sexp extraction before point."
  (with-temp-buffer
    (lisp-mode)
    (insert "(+ 1 2)")
    (should (equal (epsilon--sexp-before-point) "(+ 1 2)"))))

(ert-deftest epsilon-test-enclosing-function ()
  "Test enclosing function detection."
  (with-temp-buffer
    (lisp-mode)
    (insert "(mapcar (lambda (x) |) list)")
    (goto-char (- (point-max) 7)) ; inside lambda
    (should (equal (epsilon--enclosing-function) "lambda"))))

;;; Request ID Generation

(ert-deftest epsilon-test-request-id-increment ()
  "Test that request IDs increment."
  (let ((epsilon--next-id 0))
    (should (equal (format "%d" (cl-incf epsilon--next-id)) "1"))
    (should (equal (format "%d" (cl-incf epsilon--next-id)) "2"))
    (should (equal (format "%d" (cl-incf epsilon--next-id)) "3"))))

;;; Notification Handler Tests

(ert-deftest epsilon-test-notification-handler-registration ()
  "Test notification handler registration."
  (let ((epsilon--notification-handlers (make-hash-table :test 'equal))
        (called nil))
    (epsilon-register-notification-handler
     "test" "notify"
     (lambda (payload) (setq called payload)))
    (should (gethash (cons "test" "notify") epsilon--notification-handlers))
    ;; Simulate dispatch
    (epsilon--dispatch-notification
     '((type . "notification")
       (channel . "test")
       (op . "notify")
       (payload . ((message . "hello")))))
    (should called)
    (should (equal (alist-get 'message called) "hello"))))

(ert-deftest epsilon-test-notification-handler-unregistration ()
  "Test notification handler unregistration."
  (let ((epsilon--notification-handlers (make-hash-table :test 'equal)))
    (epsilon-register-notification-handler "test" "notify" #'ignore)
    (should (gethash (cons "test" "notify") epsilon--notification-handlers))
    (epsilon-unregister-notification-handler "test" "notify")
    (should-not (gethash (cons "test" "notify") epsilon--notification-handlers))))

;;; Message Dispatch Tests

(ert-deftest epsilon-test-response-dispatch ()
  "Test response message dispatch to callbacks."
  (let ((epsilon--pending-requests (make-hash-table :test 'equal))
        (result nil))
    (puthash "42" (lambda (r) (setq result r)) epsilon--pending-requests)
    (epsilon--handle-response
     '((type . "response")
       (id . "42")
       (payload . ((status . "ok") (value . 123)))))
    (should result)
    (should (equal (alist-get 'status result) "ok"))
    (should (equal (alist-get 'value result) 123))
    ;; Callback should be removed
    (should-not (gethash "42" epsilon--pending-requests))))

(provide 'epsilon-client-tests)

;;; epsilon-client-tests.el ends here
