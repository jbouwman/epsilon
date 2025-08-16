;;;; Comprehensive test suite for epsilon.io

(defpackage :epsilon.io.tests
  (:use :cl :epsilon.test :epsilon.io))

(in-package :epsilon.io.tests)

;;;; Test Utilities

(defun make-test-data (size &optional (byte 42))
  "Create test data array"
  (make-array size :element-type '(unsigned-byte 8)
                  :initial-element byte))

(defmacro with-test-buffer ((var size) &body body)
  "Create test buffer for testing"
  `(let ((,var (allocate-buffer ,size)))
     (unwind-protect
         (progn ,@body)
       (free-buffer ,var))))

;;;; Buffer Management Tests

(deftest test-buffer-allocation
  "Test buffer allocation and properties"
  (let ((buffer (allocate-buffer 4096)))
    (is (= 4096 (buffer-capacity buffer)))
    (is (= 0 (buffer-position buffer)))
    (is (= 4096 (buffer-limit buffer)))
    (is (= -1 (buffer-mark buffer)))
    (is (null (buffer-owner buffer)))
    (is (not (buffer-pinned-p buffer)))
    (free-buffer buffer)))

(deftest test-aligned-buffer-allocation
  "Test aligned buffer allocation"
  (let ((buffer (allocate-buffer 4096 :alignment 4096)))
    (is (= 4096 (buffer-capacity buffer)))
    ;; Address should be aligned
    (when (> (buffer-address buffer) 0)
      (is (zerop (mod (buffer-address buffer) 4096))))
    (free-buffer buffer)))

(deftest test-buffer-operations
  "Test buffer read/write operations"
  (with-test-buffer (buffer 100)
    ;; Test put
    (buffer-put buffer 65)
    (buffer-put buffer 66)
    (buffer-put buffer 67)
    (is (= 3 (buffer-position buffer)))
    
    ;; Test flip
    (buffer-flip buffer)
    (is (= 0 (buffer-position buffer)))
    (is (= 3 (buffer-limit buffer)))
    
    ;; Test get
    (is (= 65 (buffer-get buffer)))
    (is (= 66 (buffer-get buffer)))
    (is (= 67 (buffer-get buffer)))
    (is (= 3 (buffer-position buffer)))
    
    ;; Test rewind
    (buffer-rewind buffer)
    (is (= 0 (buffer-position buffer)))
    (is (= 65 (buffer-get buffer)))))

(deftest test-buffer-bulk-operations
  "Test bulk get/put operations"
  (with-test-buffer (buffer 100)
    (let ((data (make-test-data 50)))
      ;; Put data
      (buffer-put-bytes buffer data)
      (is (= 50 (buffer-position buffer)))
      
      ;; Flip and get data
      (buffer-flip buffer)
      (let ((output (make-array 50 :element-type '(unsigned-byte 8))))
        (buffer-get-bytes buffer output)
        (is (equalp data output))))))

(deftest test-buffer-overflow-protection
  "Test buffer bounds checking"
  (with-test-buffer (buffer 10)
    (setf (buffer-limit buffer) 10)
    ;; Writing past limit should signal error
    (dotimes (i 10)
      (buffer-put buffer i))
    (is-thrown 'buffer-overflow-error
               (buffer-put buffer 10))))

(deftest test-buffer-pool-operations
  "Test buffer pool allocation and return"
  (let ((pool (make-buffer-pool :size 4096 :count 10)))
    (is (<= 10 (buffer-pool-available pool)))
    
    ;; Allocate buffer
    (let ((buf1 (get-buffer pool)))
      (is (= 4096 (buffer-capacity buf1)))
      
      ;; Return buffer
      (return-buffer pool buf1))
    
    ;; Test with-pooled-buffer macro
    (with-pooled-buffer (buf pool)
      (is (= 4096 (buffer-capacity buf))))))

(deftest test-buffer-utilities
  "Test buffer utility functions"
  (let* ((text "Hello, World!")
         (buffer (string-to-buffer text)))
    (is (string= text (buffer-to-string buffer)))
    
    ;; Test copy
    (let ((copy (copy-buffer buffer)))
      (is (compare-buffers buffer copy)))
    
    (free-buffer buffer)))

;;;; Async I/O Tests

(deftest test-io-context-creation
  "Test I/O context creation and cleanup"
  (with-io-context (ctx)
    (is (io-context-p ctx))
    (is (io-context-running-p ctx))))

(deftest test-cancellation-token
  "Test cancellation token operations"
  (let ((token (make-cancellation-token))
        (cancelled nil))
    (is (not (token-cancelled-p token)))
    
    ;; Register callback
    (register-cancellation token 
      (make-io-completion :cancel-callback (lambda () (setf cancelled t))))
    
    ;; Cancel token
    (cancel-token token)
    (is (token-cancelled-p token))
    (sleep 0.1)
    (is cancelled)))

(deftest test-completion-await
  "Test synchronous completion waiting"
  (let ((completion (make-io-completion :operation :test)))
    ;; Complete in another thread
    (thread:make-thread
     (lambda ()
       (sleep 0.1)
       (complete completion 42)))
    
    (let ((result (await completion :timeout 1000)))
      (is (= 42 result)))))

(deftest test-completion-error
  "Test completion error handling"
  (let ((completion (make-io-completion :operation :test)))
    ;; Fail in another thread
    (thread:make-thread
     (lambda ()
       (sleep 0.1)
       (fail completion (make-condition 'io-error 
                                       :operation :test
                                       :message "Test error"))))
    
    (is-thrown 'io-error
               (await completion :timeout 1000))))

;;;; Pipeline Tests

(deftest test-simple-pipeline
  "Test basic stream pipeline"
  (let* ((input-data "Hello, Pipeline!")
         (sink (make-string-sink)))
    
    (run-pipeline 
     (make-pipeline :source (make-string-source :string input-data)
                   :sink sink))
    
    (is (string= input-data (close-sink sink)))))

(deftest test-transform-pipeline
  "Test pipeline with transformations"
  (let* ((input "hello world")
         (sink (make-string-sink)))
    
    (let ((pipeline (make-pipeline 
                    :source (make-string-source :string input)
                    :sink sink)))
      ;; Add uppercase transform
      (add-transform pipeline 
                    (map-transform #'string-upcase))
      (run-pipeline pipeline))
    
    (is (string= "HELLO WORLD" (close-sink sink)))))

(deftest test-filter-transform
  "Test filter transformation"
  (let* ((numbers '(1 2 3 4 5 6 7 8 9 10))
         (sink (make-buffer-sink)))
    
    (let ((pipeline (make-pipeline
                    :source (make-string-source 
                            :string (format nil "~{~A~%~}" numbers))
                    :sink sink)))
      ;; Filter even numbers
      (add-transform pipeline (line-split-transform))
      (add-transform pipeline 
                    (filter-transform 
                     (lambda (line)
                       (when (and line (> (length line) 0))
                         (evenp (parse-integer line :junk-allowed t))))))
      (run-pipeline pipeline))
    
    ;; Check filtered results
    (let ((results (buffer-sink-buffers sink)))
      (is (> (length results) 0)))))

;;;; TLS/BIO Tests

(deftest test-bio-creation
  "Test BIO creation and initialization"
  (let ((bio (make-bio)))
    (is (bio-p bio))
    (is (eq :unconnected (bio-state bio)))))

(deftest test-bio-state-transitions
  "Test valid BIO state transitions"
  (let ((bio (make-bio)))
    ;; Valid transitions
    (transition-bio-state bio :connecting)
    (is (eq :connecting (bio-state bio)))
    
    (transition-bio-state bio :handshaking)
    (is (eq :handshaking (bio-state bio)))
    
    (transition-bio-state bio :connected)
    (is (eq :connected (bio-state bio)))
    
    (transition-bio-state bio :shutting-down)
    (is (eq :shutting-down (bio-state bio)))
    
    (transition-bio-state bio :closed)
    (is (eq :closed (bio-state bio)))))

(deftest test-invalid-bio-transitions
  "Test invalid BIO state transitions"
  (let ((bio (make-bio)))
    (transition-bio-state bio :connected)
    ;; Cannot go from connected to connecting
    (is-thrown 'io-error
               (transition-bio-state bio :connecting))))

;;;; Flow Control Tests

(deftest test-flow-controller-creation
  "Test flow controller creation"
  (let ((controller (make-flow-controller :high-water 100 :low-water 50)))
    (is (flow-controller-p controller))
    (is (not (flow-controller-paused-p controller)))
    (is (= 0 (flow-controller-current-level controller)))))

(deftest test-flow-controller-pause-resume
  "Test flow controller pause/resume behavior"
  (let ((controller (make-flow-controller :high-water 100 :low-water 50)))
    ;; Initially not paused
    (is (not (flow-controller-paused-p controller)))
    
    ;; Add data until high water mark
    (flow-controller-add controller 101)
    (is (flow-controller-paused-p controller))
    
    ;; Drain to low water mark
    (flow-controller-drain controller 52)
    (is (not (flow-controller-paused-p controller)))))

(deftest test-rate-limiter
  "Test token bucket rate limiter"
  (let ((limiter (make-rate-limiter :rate 100 :burst 10)))
    ;; Should have initial burst tokens
    (is (acquire-tokens limiter 5 :wait-p nil))
    (is (acquire-tokens limiter 5 :wait-p nil))
    
    ;; Should be exhausted
    (is (not (acquire-tokens limiter 5 :wait-p nil)))
    
    ;; Wait for refill
    (sleep 0.1)
    (is (acquire-tokens limiter 5 :wait-p nil))))

;;;; Transcoding Tests

(deftest test-transcoder-creation
  "Test transcoder creation"
  (let ((transcoder (make-transcoder :from :utf-8 :to :utf-16le)))
    (is (transcoder-p transcoder))
    (is (eq :utf-8 (transcoder-from-encoding transcoder)))
    (is (eq :utf-16le (transcoder-to-encoding transcoder)))))

(deftest test-utf8-to-ascii-transcoding
  "Test UTF-8 to ASCII transcoding"
  (let ((transcoder (make-transcoder :from :utf-8 :to :ascii)))
    (let ((result (transcode-string "Hello" transcoder)))
      (is (string= "Hello" result)))))

(deftest test-utf16-encoding
  "Test UTF-16 encoding and decoding"
  (let* ((text "Hello, 世界!")
         (utf16le (encode-utf16le text))
         (utf16be (encode-utf16be text)))
    
    ;; Test round-trip
    (is (string= text (decode-utf16le utf16le)))
    (is (string= text (decode-utf16be utf16be)))
    
    ;; Test byte order
    (is (not (equalp utf16le utf16be)))))

(deftest test-transcoding-error-handling
  "Test transcoding error handling modes"
  ;; Strict mode
  (let ((transcoder (make-transcoder :from :utf-8 
                                     :to :ascii
                                     :on-malformed :strict)))
    (is-thrown 'transcoding-error
               (transcode-string "Hello 世界" transcoder)))
  
  ;; Replace mode
  (let ((transcoder (make-transcoder :from :utf-8
                                     :to :ascii
                                     :on-malformed :replace)))
    (let ((result (transcode-string "Hello 世界" transcoder)))
      ;; Should contain replacement characters
      (is (search "Hello" result)))))

(deftest test-transcoding-stream
  "Test streaming transcoder"
  (let ((transcoder (make-transcoder :from :utf-8 :to :utf-16le)))
    (with-transcoding-stream (stream transcoder)
      (write-to-transcoding-stream stream 
                                   (sb-ext:string-to-octets "Hello"))
      (write-to-transcoding-stream stream
                                   (sb-ext:string-to-octets " World"))
      
      (let ((output (get-transcoded-bytes stream)))
        (is (> (length output) 0))
        ;; Should be UTF-16LE encoded
        (is (string= "Hello World" 
                    (decode-utf16le output)))))))

;;;; Integration Tests

(deftest test-pipeline-with-backpressure
  "Test pipeline with flow control"
  (let* ((controller (make-flow-controller :high-water 100 :low-water 50))
         (sink (make-string-sink))
         (pipeline (make-pipeline
                   :source (make-string-source 
                           :string (make-string 1000 :initial-element #\A))
                   :sink sink
                   :controller controller)))
    
    (run-pipeline pipeline)
    
    ;; Should have processed all data despite backpressure
    (is (= 1000 (length (close-sink sink))))))

(deftest test-buffer-pool-with-async-io
  "Test buffer pool integration with async I/O"
  (with-io-context (ctx)
    (let ((pool (make-buffer-pool :size 1024 :count 5)))
      (with-pooled-buffer (buffer pool)
        ;; Buffer should be from pool
        (is (= 1024 (buffer-capacity buffer)))
        ;; Simulate async operation
        (setf (buffer-owner buffer) ctx
              (buffer-pinned-p buffer) t)
        ;; Clear ownership
        (setf (buffer-owner buffer) nil
              (buffer-pinned-p buffer) nil)))))

;;;; Performance Tests

(deftest test-buffer-allocation-performance
  "Benchmark buffer allocation"
  (let ((iterations 1000)
        (size 4096))
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations)
        (free-buffer (allocate-buffer size)))
      (let ((elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
        ;; Should allocate at least 1000 buffers per second
        (is (< elapsed 1.0))))))

(deftest test-buffer-pool-performance
  "Benchmark buffer pool vs direct allocation"
  (let ((pool (make-buffer-pool :size 4096 :count 100))
        (iterations 10000))
    
    ;; Test pool allocation
    (let ((pool-start (get-internal-real-time)))
      (dotimes (i iterations)
        (let ((buf (get-buffer pool)))
          (return-buffer pool buf)))
      (let ((pool-time (/ (- (get-internal-real-time) pool-start)
                         internal-time-units-per-second)))
        
        ;; Test direct allocation
        (let ((alloc-start (get-internal-real-time)))
          (dotimes (i iterations)
            (free-buffer (allocate-buffer 4096)))
          (let ((alloc-time (/ (- (get-internal-real-time) alloc-start)
                              internal-time-units-per-second)))
            
            ;; Pool should be significantly faster
            (is (< pool-time (* 0.5 alloc-time)))))))))