;;;; Tests for epsilon.channel

(defpackage :epsilon.channel.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:channel #:epsilon.channel)
   (#:seq #:epsilon.sequence)))

(in-package :epsilon.channel.tests)

;;;; Channel Tests

(deftest test-channel-creation ()
  "Test basic channel creation"
  (let ((unbounded (channel:create-channel))
        (bounded (channel:create-channel :capacity 5)))
    
    (is (channel:channel-p unbounded))
    (is (channel:channel-p bounded))
    (is (not (channel:channel-closed-p unbounded)))
    (is (not (channel:channel-closed-p bounded)))))

(deftest test-channel-send-receive ()
  "Test basic send and receive operations"
  (let ((ch (channel:create-channel :capacity 3)))
    
    ;; Send values
    (is (channel:send ch "hello"))
    (is (channel:send ch "world"))
    (is (channel:send ch 42))
    
    ;; Receive values
    (multiple-value-bind (value received-p) (channel:receive ch)
      (is received-p)
      (is (string= value "hello")))
    
    (multiple-value-bind (value received-p) (channel:receive ch)
      (is received-p)
      (is (string= value "world")))
    
    (multiple-value-bind (value received-p) (channel:receive ch)
      (is received-p)
      (is-= value 42))))

(deftest test-channel-try-operations ()
  "Test non-blocking try-send and try-receive"
  (let ((ch (channel:create-channel :capacity 1)))
    
    ;; Should succeed when empty
    (is (channel:try-send ch "test"))
    
    ;; Should fail when full
    (is (not (channel:try-send ch "overflow")))
    
    ;; Should receive successfully
    (multiple-value-bind (value received-p) (channel:try-receive ch)
      (is received-p)
      (is (string= value "test")))
    
    ;; Should fail when empty
    (multiple-value-bind (value received-p) (channel:try-receive ch)
      (is (not received-p))
      (is (null value)))))

(deftest test-channel-close ()
  "Test channel closing behavior"
  (let ((ch (channel:create-channel)))
    
    ;; Send some values
    (channel:send ch "before-close")
    
    ;; Close channel
    (channel:close-channel ch)
    (is (channel:channel-closed-p ch))
    
    ;; Should be able to receive existing values
    (multiple-value-bind (value received-p) (channel:receive ch)
      (is received-p)
      (is (string= value "before-close")))
    
    ;; Further receives should indicate closed
    (multiple-value-bind (value received-p) (channel:receive ch)
      (is (not received-p))
      (is (null value)))
    
    ;; Sends should error
    (is-thrown (channel:channel-closed-error)
      (channel:send ch "after-close"))))

;;;; Stream Tests

(deftest test-stream-from-sequence ()
  "Test creating stream from sequence"
  (let ((stream (channel:from-sequence '(1 2 3 4 5))))
    
    (is (channel:data-stream-p stream))
    
    ;; Read all values
    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (is more-p)
      (is-= value 1))
    
    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (is more-p)
      (is-= value 2))
    
    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (is more-p)
      (is-= value 3))
    
    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (is more-p)
      (is-= value 4))
    
    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (is (not more-p)) ; Last value, no more
      (is-= value 5))
    
    ;; Should be exhausted
    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (is (not more-p))
      (is (null value)))))

(deftest test-stream-map ()
  "Test stream mapping operation"
  (let* ((source (channel:from-sequence '(1 2 3 4)))
         (mapped (channel:map-stream source (lambda (x) (* x 2)))))
    
    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (is more-p)
      (is-= value 2))
    
    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (is more-p)
      (is-= value 4))
    
    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (is more-p)
      (is-= value 6))
    
    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (is (not more-p))
      (is-= value 8))))

(deftest test-stream-filter ()
  "Test stream filtering operation"
  (let* ((source (channel:from-sequence '(1 2 3 4 5 6)))
         (filtered (channel:filter-stream source #'evenp)))
    
    (multiple-value-bind (value more-p) (channel:stream-next filtered)
      (is more-p)
      (is-= value 2))
    
    (multiple-value-bind (value more-p) (channel:stream-next filtered)
      (is more-p)
      (is-= value 4))
    
    (multiple-value-bind (value more-p) (channel:stream-next filtered)
      (is (not more-p))
      (is-= value 6))))

(deftest test-stream-take ()
  "Test stream take operation"
  (let* ((source (channel:from-sequence '(1 2 3 4 5)))
         (taken (channel:take-stream source 3)))
    
    (multiple-value-bind (value more-p) (channel:stream-next taken)
      (is more-p)
      (is-= value 1))
    
    (multiple-value-bind (value more-p) (channel:stream-next taken)
      (is more-p)
      (is-= value 2))
    
    (multiple-value-bind (value more-p) (channel:stream-next taken)
      (is (not more-p)) ; Last of the 3 taken
      (is-= value 3))
    
    ;; Should be finished
    (multiple-value-bind (value more-p) (channel:stream-next taken)
      (is (not more-p))
      (is (null value)))))

(deftest test-stream-drop ()
  "Test stream drop operation"
  (let* ((source (channel:from-sequence '(1 2 3 4 5)))
         (dropped (channel:drop-stream source 2)))
    
    (multiple-value-bind (value more-p) (channel:stream-next dropped)
      (is more-p)
      (is-= value 3)) ; First value after dropping 1,2
    
    (multiple-value-bind (value more-p) (channel:stream-next dropped)
      (is more-p)
      (is-= value 4))
    
    (multiple-value-bind (value more-p) (channel:stream-next dropped)
      (is (not more-p))
      (is-= value 5))))

(deftest test-stream-collect ()
  "Test collecting stream into list"
  (let* ((source (channel:from-sequence '(1 2 3 4)))
         (mapped (channel:map-stream source (lambda (x) (* x x))))
         (result (channel:collect-stream mapped)))
    
    (is (equal result '(1 4 9 16)))))

(deftest test-stream-fold ()
  "Test folding stream into single value"
  (let* ((source (channel:from-sequence '(1 2 3 4 5)))
         (sum (channel:fold-stream source 0 #'+)))
    
    (is-= sum 15)))

(deftest test-stream-zip ()
  "Test zipping two streams"
  (let* ((stream1 (channel:from-sequence '(1 2 3)))
         (stream2 (channel:from-sequence '("a" "b" "c")))
         (zipped (channel:zip-stream stream1 stream2)))
    
    (multiple-value-bind (value more-p) (channel:stream-next zipped)
      (is more-p)
      (is (equal value '(1 "a"))))
    
    (multiple-value-bind (value more-p) (channel:stream-next zipped)
      (is more-p)
      (is (equal value '(2 "b"))))
    
    (multiple-value-bind (value more-p) (channel:stream-next zipped)
      (is (not more-p))
      (is (equal value '(3 "c"))))))

;;;; Advanced Stream Tests

(deftest test-stream-chain ()
  "Test chaining multiple stream operations"
  (let* ((source (channel:from-sequence '(1 2 3 4 5 6 7 8 9 10)))
         (processed (channel:take-stream
                     (channel:map-stream
                      (channel:filter-stream source #'oddp)
                      (lambda (x) (* x x)))
                     3))
         (result (channel:collect-stream processed)))
    
    ;; Should be squares of first 3 odd numbers: 1, 9, 25
    (is (equal result '(1 9 25)))))

(deftest test-stream-for-each ()
  "Test stream for-each operation"
  (let ((source (channel:from-sequence '(1 2 3)))
        (collected '()))
    
    (channel:for-each-stream source 
                            (lambda (x) (push (* x 2) collected)))
    
    (is (equal (reverse collected) '(2 4 6)))))

;;;; Channel Integration Tests

(deftest test-channel-stream-integration ()
  "Test integration between channels and streams"
  (let ((ch (channel:create-channel :capacity 5)))
    
    ;; Send values to channel
    (channel:send ch 1)
    (channel:send ch 2)
    (channel:send ch 3)
    (channel:close-channel ch)
    
    ;; Create stream from channel
    (let* ((stream (channel:from-channel ch))
           (result (channel:collect-stream stream)))
      
      (is (equal result '(1 2 3))))))

;;;; Concurrent Tests (basic, since we can't easily test real concurrency)

(deftest test-channel-concurrent-simulation ()
  "Test channel behavior in simulated concurrent scenario"
  (let ((ch (channel:create-channel :capacity 2)))
    
    ;; Simulate producer
    (channel:send ch "item1")
    (channel:send ch "item2")
    
    ;; Channel should be full now
    (is (not (channel:try-send ch "item3")))
    
    ;; Simulate consumer
    (multiple-value-bind (value received-p) (channel:receive ch)
      (is received-p)
      (is (string= value "item1")))
    
    ;; Now should be able to send again
    (is (channel:try-send ch "item3"))
    
    ;; Receive remaining items
    (multiple-value-bind (value received-p) (channel:receive ch)
      (is received-p)
      (is (string= value "item2")))
    
    (multiple-value-bind (value received-p) (channel:receive ch)
      (is received-p)
      (is (string= value "item3")))))

;;;; Error Handling Tests

(deftest test-stream-error-propagation ()
  "Test error handling in stream operations"
  (let* ((source (channel:from-sequence '(1 2 0 4)))
         (mapped (channel:map-stream source (lambda (x) (/ 10 x)))))
    
    ;; First two should work
    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (is more-p)
      (is-= value 10))
    
    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (is more-p)
      (is-= value 5))
    
    ;; Third should error due to division by zero
    (is-thrown (division-by-zero)
      (channel:stream-next mapped))))