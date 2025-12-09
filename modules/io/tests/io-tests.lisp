;;;; Test suite for epsilon.io

(defpackage :epsilon.io.tests
  (:use :cl :epsilon.test :epsilon.io))

(in-package :epsilon.io.tests)

;;;; Buffer Tests

(deftest test-buffer-allocation
  "Test basic buffer allocation"
  (let ((buf (allocate-buffer 1024)))
    (is (buffer-p buf))
    (is-= 1024 (buffer-remaining buf))))

(deftest test-buffer-string-conversion
  "Test string to buffer conversion and back"
  (let* ((test-string "Hello, epsilon.io!")
         (buf (string-to-buffer test-string))
         (result-string (buffer-to-string buf)))
    (is (string= test-string result-string))
    (is-= (length test-string) (buffer-remaining buf))))

;;;; Buffer Pool Tests

(deftest test-buffer-pool-creation
  "Test buffer pool creation"
  (let ((pool (create-buffer-pool :size 512 :count 10)))
    (is (buffer-pool-p pool))
    (is-= 512 (buffer-pool-size pool))
    (is-= 10 (buffer-pool-count pool))))

(deftest test-buffer-pool-acquisition
  "Test buffer acquisition from pool"
  (let ((pool (create-buffer-pool :size 256 :count 5)))
    (let ((buf (acquire-buffer pool)))
      (is (buffer-p buf))
      (is-= 256 (buffer-remaining buf))
      (release-buffer pool buf))))

(deftest test-pooled-buffer-macro
  "Test with-pooled-buffer macro"
  (let ((pool (create-buffer-pool :size 128 :count 3)))
    (with-pooled-buffer (buf pool)
      (is (buffer-p buf))
      (is-= 128 (buffer-remaining buf)))))

;;;; I/O Context Tests

(deftest test-io-context-creation
  "Test I/O context creation"
  (let ((ctx (create-io-context)))
    (is (io-context-p ctx))
    (is (io-context-running-p ctx))
    (close-io-context ctx)))

(deftest test-io-context-macro
  "Test with-io-context macro"
  (with-io-context (ctx)
    (is (io-context-p ctx))
    (is (io-context-running-p ctx))))

;;;; Async Operation Tests

(deftest test-async-read-creation
  "Test async read operation creation"
  ;; Skip: stdin (fd 0) cannot be registered with kqueue in non-TTY context
  (skip "Cannot register stdin with kqueue in test environment"))

(deftest test-async-write-creation
  "Test async write operation creation"
  (with-io-context (ctx)
    (let* ((buf (string-to-buffer "test data"))
	   (op (async-write 1 buf :context ctx)))
      (is (epsilon.async:async-operation-p op))
      (is-= 1 (epsilon.async:async-operation-fd op))
      (is (eq :write (epsilon.async:async-operation-type op))))))

;;;; Integration Tests

(deftest test-buffer-with-context
  "Test buffer operations with I/O context"
  (with-io-context (ctx)
    (let ((pool (create-buffer-pool :size 64)))
      (with-pooled-buffer (buf pool)
        (let ((data "Hello, World!"))
          (let ((test-buf (string-to-buffer data)))
            (is (string= data (buffer-to-string test-buf)))))))))

(deftest test-async-system-initialization
  "Test that async system initializes properly"
  (unwind-protect
       (progn
         (epsilon.async:ensure-async-system)
         ;; Should not error
         (is t))
    ;; Clean up the async system after test
    (epsilon.async:stop-async-system)))

(deftest test-set-nonblocking
  "Test set-nonblocking utility"
  (let ((result (set-nonblocking 1)))
    ;; Should return the fd
    (is-= 1 result)))

;;;; Platform Integration Tests

(deftest test-platform-detection
  "Test that platform-specific async is loaded"
  ;; Should have epsilon.async loaded via the platform module
  (is (find-package :epsilon.async)))

(deftest test-async-operations-export
  "Test that async operations are properly exported"
  (is (fboundp 'epsilon.async:async-read))
  (is (fboundp 'epsilon.async:async-write))
  (is (fboundp 'epsilon.async:ensure-async-system))
  (is (fboundp 'epsilon.async:poll-completions)))
