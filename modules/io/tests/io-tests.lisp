;;;; Tests for the IO stream system.

(defpackage epsilon.io-tests
  (:use :cl :epsilon.test :epsilon.io)
  (:enter t))

;;; ============================================================================
;;; Buffer Tests
;;; ============================================================================

(deftest test-buf-creation
  "Test buffer creation and initial state"
  (let ((buf (make-buf 1024)))
    (assert-true (buf-p buf))
    (assert-= 1024 (buf-capacity buf))
    (assert-= 0 (buf-position buf))
    (assert-= 0 (buf-limit buf))
    (assert-= 0 (buf-remaining buf))
    (assert-= 1024 (buf-space buf))
    (assert-true (buf-empty-p buf))
    (assert-true (not (buf-full-p buf)))))

(deftest test-buf-from-bytes
  "Test buffer creation from byte vector"
  (let* ((bytes #(1 2 3 4 5))
         (buf (buf-from-bytes bytes)))
    (assert-= 5 (buf-remaining buf))
    (assert-= 0 (buf-position buf))
    (assert-= 5 (buf-limit buf))))

(deftest test-buf-wrap
  "Test buffer wrap (no copy)"
  (let* ((bytes (make-array 5 :element-type '(unsigned-byte 8)
                              :initial-contents '(1 2 3 4 5)))
         (buf (buf-wrap bytes)))
    (assert-= 5 (buf-remaining buf))
    ;; Mutation should affect original
    (buf-get-byte buf)
    (assert-= 4 (buf-remaining buf))))

(deftest test-buf-get-byte
  "Test reading single bytes"
  (let ((buf (buf-from-bytes #(10 20 30))))
    (assert-= 10 (buf-get-byte buf))
    (assert-= 20 (buf-get-byte buf))
    (assert-= 30 (buf-get-byte buf))
    (assert-true (buf-empty-p buf))))

(deftest test-buf-peek-byte
  "Test peeking without advancing"
  (let ((buf (buf-from-bytes #(1 2 3))))
    (assert-= 1 (buf-peek-byte buf))
    (assert-= 1 (buf-peek-byte buf))  ; Still same
    (assert-= 2 (buf-peek-byte buf 1))
    (assert-= 3 (buf-remaining buf))))

(deftest test-buf-put-byte
  "Test writing single bytes"
  (let ((buf (make-buf 3)))
    (buf-put-byte buf 10)
    (buf-put-byte buf 20)
    (assert-= 2 (buf-limit buf))
    (assert-= 0 (buf-position buf))
    (assert-= 2 (buf-remaining buf))))

(deftest test-buf-flip
  "Test flip operation"
  (let ((buf (make-buf 10)))
    (buf-put-byte buf 1)
    (buf-put-byte buf 2)
    (buf-put-byte buf 3)
    (assert-= 0 (buf-position buf))
    (assert-= 3 (buf-limit buf))
    ;; Simulate writing mode: position moves with writes
    (setf (buf-position buf) 3)
    (buf-flip buf)
    (assert-= 0 (buf-position buf))
    (assert-= 3 (buf-limit buf))))

(deftest test-buf-clear
  "Test clear operation"
  (let ((buf (buf-from-bytes #(1 2 3))))
    (buf-get-byte buf)
    (buf-clear buf)
    (assert-= 0 (buf-position buf))
    (assert-= 0 (buf-limit buf))
    (assert-true (buf-empty-p buf))))

(deftest test-buf-compact
  "Test compact operation"
  (let ((buf (buf-from-bytes #(1 2 3 4 5))))
    (buf-get-byte buf)
    (buf-get-byte buf)
    ;; Position is 2, remaining is [3,4,5]
    (buf-compact buf)
    (assert-= 0 (buf-position buf))
    (assert-= 3 (buf-limit buf))
    (assert-= 3 (buf-get-byte buf))))

(deftest test-buf-slice
  "Test zero-copy slicing"
  (let* ((buf (buf-from-bytes #(1 2 3 4 5)))
         (slice (buf-slice buf 1 3)))
    (assert-= 2 (buf-remaining slice))
    (assert-= 2 (buf-get-byte slice))
    (assert-= 3 (buf-get-byte slice))))

(deftest test-buf-pool
  "Test buffer pool acquire/release"
  (let ((pool (make-buf-pool :default-size 256)))
    (let ((buf1 (buf-pool-acquire pool)))
      (assert-true (buf-p buf1))
      (assert-= 256 (buf-capacity buf1))
      (buf-pool-release pool buf1))
    ;; Acquire again should get same buffer (from pool)
    (let ((buf2 (buf-pool-acquire pool)))
      (assert-true (buf-p buf2)))))

(deftest test-with-buf-macro
  "Test with-buf convenience macro"
  (let ((pool (make-buf-pool :default-size 128)))
    (with-buf (buf nil pool)
      (assert-true (buf-p buf))
      (assert-= 128 (buf-capacity buf)))))

;;; ============================================================================
;;; Byte Stream Tests
;;; ============================================================================

(deftest test-byte-reader
  "Test byte-reader from byte vector"
  (let ((reader (make-byte-reader #(10 20 30 40))))
    (assert-true (byte-reader-p reader))
    (assert-= 10 (read-byte* reader))
    (assert-= 20 (read-byte* reader))
    (assert-= 30 (read-byte* reader))
    (assert-= 40 (read-byte* reader))
    (assert-true (null (read-byte* reader)))))  ; EOF

(deftest test-byte-reader-read-into
  "Test byte-reader read-into"
  (let ((reader (make-byte-reader #(1 2 3 4 5)))
        (buf (make-array 3 :element-type '(unsigned-byte 8))))
    (assert-= 3 (read-into reader buf))
    (assert-= 1 (aref buf 0))
    (assert-= 2 (aref buf 1))
    (assert-= 3 (aref buf 2))
    (assert-= 2 (read-into reader buf))  ; Only 2 remaining
    (assert-= 0 (read-into reader buf))))  ; EOF

(deftest test-byte-writer
  "Test byte-writer"
  (let ((writer (make-byte-writer)))
    (write-byte* writer 10)
    (write-byte* writer 20)
    (write-byte* writer 30)
    (let ((bytes (byte-writer-bytes writer)))
      (assert-= 3 (length bytes))
      (assert-= 10 (aref bytes 0))
      (assert-= 20 (aref bytes 1))
      (assert-= 30 (aref bytes 2)))))

(deftest test-byte-writer-grow
  "Test byte-writer auto-growth"
  (let ((writer (make-byte-writer :initial-size 4)))
    ;; Write more than initial size
    (dotimes (i 100)
      (write-byte* writer (mod i 256)))
    (let ((bytes (byte-writer-bytes writer)))
      (assert-= 100 (length bytes)))))

(deftest test-null-reader
  "Test null reader returns EOF"
  (assert-= 0 (read-into *null-reader* (make-array 10 :element-type '(unsigned-byte 8))))
  (assert-true (null (read-byte* *null-reader*))))

(deftest test-null-writer
  "Test null writer discards"
  (assert-= 5 (write-from *null-writer*
                      (make-array 5 :element-type '(unsigned-byte 8)))))

(deftest test-read-all
  "Test read-all convenience"
  (let* ((data #(1 2 3 4 5))
         (reader (make-byte-reader data))
         (result (read-all reader)))
    (assert-= 5 (length result))
    (assert-= 1 (aref result 0))
    (assert-= 5 (aref result 4))))

(deftest test-copy-stream
  "Test copy-stream"
  (let ((reader (make-byte-reader #(1 2 3 4 5)))
        (writer (make-byte-writer)))
    (assert-= 5 (copy-stream reader writer))
    (let ((result (byte-writer-bytes writer)))
      (assert-= 5 (length result)))))

;;; ============================================================================
;;; Buffered IO Tests
;;; ============================================================================

(deftest test-buffered-reader
  "Test buffered-reader wrapping"
  (let* ((source (make-byte-reader #(1 2 3 4 5)))
         (reader (make-buffered-reader source)))
    (assert-true (buffered-reader-p reader))
    (assert-= 1 (read-byte* reader))
    (assert-= 2 (read-byte* reader))))

(deftest test-buffered-reader-peek
  "Test buffered-reader peeking"
  (let* ((source (make-byte-reader #(10 20 30)))
         (reader (make-buffered-reader source)))
    (assert-= 10 (buffered-reader-peek reader))
    (assert-= 10 (buffered-reader-peek reader))  ; Still same
    (assert-= 20 (buffered-reader-peek reader 1))
    (read-byte* reader)
    (assert-= 20 (buffered-reader-peek reader))))

(deftest test-buffered-reader-skip
  "Test buffered-reader skip"
  (let* ((source (make-byte-reader #(1 2 3 4 5)))
         (reader (make-buffered-reader source)))
    (assert-= 2 (buffered-reader-skip reader 2))
    (assert-= 3 (read-byte* reader))))

(deftest test-buffered-reader-read-until
  "Test read-until delimiter"
  (let* ((source (make-byte-reader #(104 101 108 108 111 10 119 111 114 108 100)))  ; "hello\nworld"
         (reader (make-buffered-reader source)))
    (multiple-value-bind (bytes found-p)
        (buffered-reader-read-until reader 10)  ; LF
      (assert-true found-p)
      (assert-= 5 (length bytes))  ; "hello"
      (assert-= 104 (aref bytes 0)))))

(deftest test-buffered-reader-read-line
  "Test read-line"
  (let* ((text "first line
second line
third")
         (reader (make-buffered-reader (string-to-reader text))))
    (multiple-value-bind (line more-p)
        (buffered-reader-read-line reader)
      (assert-true more-p)
      (assert-true (string= "first line" line)))
    (multiple-value-bind (line more-p)
        (buffered-reader-read-line reader)
      (assert-true more-p)
      (assert-true (string= "second line" line)))
    (multiple-value-bind (line more-p)
        (buffered-reader-read-line reader)
      (assert-true more-p)  ; Has data even though no trailing newline
      (assert-true (string= "third" line)))))

(deftest test-buffered-writer
  "Test buffered-writer"
  (let* ((sink (make-byte-writer))
         (writer (make-buffered-writer sink)))
    (write-byte* writer 1)
    (write-byte* writer 2)
    (write-byte* writer 3)
    (flush writer)
    (let ((result (byte-writer-bytes sink)))
      (assert-= 3 (length result)))))

(deftest test-buffered-writer-write-string
  "Test buffered-writer-write-string"
  (let* ((sink (make-byte-writer))
         (writer (make-buffered-writer sink)))
    (buffered-writer-write-string writer "hello")
    (flush writer)
    (assert-true (string= "hello" (byte-writer-string sink)))))

;;; ============================================================================
;;; Combinator Tests
;;; ============================================================================

(deftest test-limit-reader
  "Test limit-reader"
  (let* ((source (make-byte-reader #(1 2 3 4 5)))
         (reader (make-limit-reader source 3)))
    (assert-= 1 (read-byte* reader))
    (assert-= 2 (read-byte* reader))
    (assert-= 3 (read-byte* reader))
    (assert-true (null (read-byte* reader)))  ; Hit limit
    (assert-true (limit-reader-exhausted-p reader))))

(deftest test-tee-reader
  "Test tee-reader copies to writer"
  (let* ((source (make-byte-reader #(1 2 3)))
         (sink (make-byte-writer))
         (reader (make-tee-reader source sink)))
    (read-all reader)
    (flush sink)
    (let ((copied (byte-writer-bytes sink)))
      (assert-= 3 (length copied))
      (assert-= 1 (aref copied 0)))))

(deftest test-multi-writer
  "Test multi-writer writes to all"
  (let* ((sink1 (make-byte-writer))
         (sink2 (make-byte-writer))
         (writer (make-multi-writer sink1 sink2)))
    (write-from writer #(1 2 3))
    (assert-= 3 (length (byte-writer-bytes sink1)))
    (assert-= 3 (length (byte-writer-bytes sink2)))))

(deftest test-chain-reader
  "Test chain-reader concatenates"
  (let* ((r1 (make-byte-reader #(1 2)))
         (r2 (make-byte-reader #(3 4)))
         (r3 (make-byte-reader #(5)))
         (reader (make-chain-reader r1 r2 r3)))
    (assert-= 1 (read-byte* reader))
    (assert-= 2 (read-byte* reader))
    (assert-= 3 (read-byte* reader))
    (assert-= 4 (read-byte* reader))
    (assert-= 5 (read-byte* reader))
    (assert-true (null (read-byte* reader)))))

(deftest test-counting-reader
  "Test counting-reader tracks bytes"
  (let* ((source (make-byte-reader #(1 2 3 4 5)))
         (reader (make-counting-reader source)))
    (read-byte* reader)
    (read-byte* reader)
    (assert-= 2 (counting-reader-count reader))
    (read-all reader)
    (assert-= 5 (counting-reader-count reader))))

(deftest test-counting-writer
  "Test counting-writer tracks bytes"
  (let* ((sink (make-byte-writer))
         (writer (make-counting-writer sink)))
    (write-byte* writer 1)
    (write-byte* writer 2)
    (assert-= 2 (counting-writer-count writer))))

(deftest test-transform-reader
  "Test transform-reader applies function"
  (let* ((source (make-byte-reader #(1 2 3)))
         (reader (make-transform-reader source (lambda (b) (+ b 10)))))
    (assert-= 11 (read-byte* reader))
    (assert-= 12 (read-byte* reader))
    (assert-= 13 (read-byte* reader))))

(deftest test-offset-reader
  "Test offset-reader skips bytes"
  (let* ((source (make-byte-reader #(1 2 3 4 5)))
         (reader (make-offset-reader source 2)))
    (assert-= 3 (read-byte* reader))
    (assert-= 4 (read-byte* reader))
    (assert-= 5 (read-byte* reader))))

(deftest test-convenience-combinators
  "Test convenience functions"
  (let* ((source (make-byte-reader #(1 2 3 4 5)))
         (limited (limit source 3 :direction :input)))
    (assert-= 3 (length (read-all limited)))))

;;; ============================================================================
;;; Protocol Tests
;;; ============================================================================

(deftest test-read-exact
  "Test read-exact reads all or errors"
  (let* ((source (make-byte-reader #(1 2 3)))
         (buf (make-array 3 :element-type '(unsigned-byte 8))))
    (assert-= 3 (read-exact source buf))
    (assert-= 1 (aref buf 0))
    (assert-= 3 (aref buf 2))))

(deftest test-write-all
  "Test write-all writes all"
  (let ((writer (make-byte-writer)))
    (write-all writer #(1 2 3 4 5))
    (assert-= 5 (length (byte-writer-bytes writer)))))

(deftest test-seek-byte-reader
  "Test seeking in byte-reader"
  (let ((reader (make-byte-reader #(10 20 30 40 50))))
    (assert-= 10 (read-byte* reader))
    (seek* reader 3 :start)
    (assert-= 40 (read-byte* reader))
    (seek* reader -2 :current)
    (assert-= 30 (read-byte* reader))))

(deftest test-close-idempotent
  "Test close* is idempotent"
  (let ((reader (make-byte-reader #(1 2 3))))
    (assert-true (close* reader))
    (assert-true (null (close* reader)))
    (assert-true (not (open-p reader)))))

;;; ============================================================================
;;; IO Context Tests
;;; ============================================================================

(deftest test-io-context-creation
  "Test IO context creation"
  (let ((ctx (make-io-context)))
    (assert-true (io-context-p ctx))
    (assert-true (not (io-context-closed-p ctx)))
    (io-context-close ctx)
    (assert-true (io-context-closed-p ctx))))

(deftest test-with-io-context-macro
  "Test with-io-context macro"
  (with-io-context (ctx)
    (assert-true (io-context-p ctx))
    (assert-true (not (io-context-closed-p ctx)))))

;;; ============================================================================
;;; Error Condition Tests
;;; ============================================================================

(deftest test-short-read-error
  "Test short-read-error is signaled"
  (let* ((source (make-byte-reader #(1 2)))
         (buf (make-array 5 :element-type '(unsigned-byte 8))))
    (assert-true (handler-case
            (progn (read-exact source buf) nil)
          (epsilon.io.conditions:short-read-error () t)))))

(deftest test-buffer-underflow-error
  "Test buffer-underflow-error on empty buffer"
  (let ((buf (make-buf 10)))
    (assert-true (handler-case
            (progn (buf-get-byte buf) nil)
          (epsilon.io.conditions:buffer-underflow-error () t)))))

(deftest test-buffer-overflow-error
  "Test buffer-overflow-error on full buffer"
  (let ((buf (make-buf 2)))
    (buf-put-byte buf 1)
    (buf-put-byte buf 2)
    (assert-true (handler-case
            (progn (buf-put-byte buf 3) nil)
          (epsilon.io.conditions:buffer-overflow-error () t)))))

(deftest test-closed-error
  "Test closed-error on closed stream"
  (let ((reader (make-byte-reader #(1 2 3))))
    (close* reader)
    (assert-true (handler-case
            (progn (read-byte* reader) nil)
          (epsilon.io.conditions:closed-error () t)))))

;;; ============================================================================
;;; Platform Integration Tests
;;; ============================================================================

(deftest test-platform-detection
  "Test that platform-specific async is loaded"
  (assert-true (find-package :epsilon.async)))

(deftest test-async-operations-export
  "Test that async operations are properly exported"
  (assert-true (fboundp 'epsilon.async:async-read))
  (assert-true (fboundp 'epsilon.async:async-write))
  (assert-true (fboundp 'epsilon.async:ensure-async-system))
  (assert-true (fboundp 'epsilon.async:poll-completions)))

(deftest test-set-nonblocking
  "Test set-nonblocking utility"
  (let ((result (set-nonblocking 1)))
    (assert-= 1 result)))
