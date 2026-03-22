;;;; Tests for epsilon.channel

(defpackage epsilon.channel-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.channel channel)
            (epsilon.sequence seq))
  (:enter t))

;;;; Channel Tests

(deftest test-channel-creation ()
  "Test basic channel creation"
  (let ((unbounded (channel:make-channel))
        (bounded (channel:make-channel :capacity 5)))

    (assert-true (channel:channel-p unbounded))
    (assert-true (channel:channel-p bounded))
    (assert-true (not (channel:channel-closed-p unbounded)))
    (assert-true (not (channel:channel-closed-p bounded)))))

(deftest test-channel-send-receive ()
  "Test basic send and receive operations"
  (let ((ch (channel:make-channel :capacity 3)))

    ;; Send values
    (assert-true (channel:send ch "hello"))
    (assert-true (channel:send ch "world"))
    (assert-true (channel:send ch 42))

    ;; Receive values
    (multiple-value-bind (value received-p) (channel:receive ch)
      (assert-true received-p)
      (assert-true (string= value "hello")))

    (multiple-value-bind (value received-p) (channel:receive ch)
      (assert-true received-p)
      (assert-true (string= value "world")))

    (multiple-value-bind (value received-p) (channel:receive ch)
      (assert-true received-p)
      (assert-= value 42))))

(deftest test-channel-try-operations ()
  "Test non-blocking try-send and try-receive"
  (let ((ch (channel:make-channel :capacity 1)))

    ;; Should succeed when empty
    (assert-true (channel:try-send ch "test"))

    ;; Should fail when full
    (assert-true (not (channel:try-send ch "overflow")))

    ;; Should receive successfully
    (multiple-value-bind (value received-p) (channel:try-receive ch)
      (assert-true received-p)
      (assert-true (string= value "test")))

    ;; Should fail when empty
    (multiple-value-bind (value received-p) (channel:try-receive ch)
      (assert-true (not received-p))
      (assert-true (null value)))))

(deftest test-channel-close ()
  "Test channel closing behavior"
  (let ((ch (channel:make-channel)))

    ;; Send some values
    (channel:send ch "before-close")

    ;; Close channel
    (channel:close-channel ch)
    (assert-true (channel:channel-closed-p ch))

    ;; Should be able to receive existing values
    (multiple-value-bind (value received-p) (channel:receive ch)
      (assert-true received-p)
      (assert-true (string= value "before-close")))

    ;; Further receives should indicate closed
    (multiple-value-bind (value received-p) (channel:receive ch)
      (assert-true (not received-p))
      (assert-true (null value)))

    ;; Sends should error
    (assert-condition (channel:channel-closed-error)
      (channel:send ch "after-close"))))

;;;; Stream Tests

(deftest test-stream-from-sequence ()
  "Test creating stream from sequence"
  (let ((stream (channel:from-sequence '(1 2 3 4 5))))

    (assert-true (channel:data-stream-p stream))

    ;; Read all values
    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (assert-true more-p)
      (assert-= value 1))

    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (assert-true more-p)
      (assert-= value 2))

    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (assert-true more-p)
      (assert-= value 3))

    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (assert-true more-p)
      (assert-= value 4))

    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (assert-true (not more-p)) ; Last value, no more
      (assert-= value 5))

    ;; Should be exhausted
    (multiple-value-bind (value more-p) (channel:stream-next stream)
      (assert-true (not more-p))
      (assert-true (null value)))))

(deftest test-stream-map ()
  "Test stream mapping operation"
  (let* ((source (channel:from-sequence '(1 2 3 4)))
         (mapped (channel:map-stream source (lambda (x) (* x 2)))))

    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (assert-true more-p)
      (assert-= value 2))

    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (assert-true more-p)
      (assert-= value 4))

    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (assert-true more-p)
      (assert-= value 6))

    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (assert-true (not more-p))
      (assert-= value 8))))

(deftest test-stream-filter ()
  "Test stream filtering operation"
  (let* ((source (channel:from-sequence '(1 2 3 4 5 6)))
         (filtered (channel:filter-stream source #'evenp)))

    (multiple-value-bind (value more-p) (channel:stream-next filtered)
      (assert-true more-p)
      (assert-= value 2))

    (multiple-value-bind (value more-p) (channel:stream-next filtered)
      (assert-true more-p)
      (assert-= value 4))

    (multiple-value-bind (value more-p) (channel:stream-next filtered)
      (assert-true (not more-p))
      (assert-= value 6))))

(deftest test-stream-take ()
  "Test stream take operation"
  (let* ((source (channel:from-sequence '(1 2 3 4 5)))
         (taken (channel:take-stream source 3)))

    (multiple-value-bind (value more-p) (channel:stream-next taken)
      (assert-true more-p)
      (assert-= value 1))

    (multiple-value-bind (value more-p) (channel:stream-next taken)
      (assert-true more-p)
      (assert-= value 2))

    (multiple-value-bind (value more-p) (channel:stream-next taken)
      (assert-true (not more-p)) ; Last of the 3 taken
      (assert-= value 3))

    ;; Should be finished
    (multiple-value-bind (value more-p) (channel:stream-next taken)
      (assert-true (not more-p))
      (assert-true (null value)))))

(deftest test-stream-drop ()
  "Test stream drop operation"
  (let* ((source (channel:from-sequence '(1 2 3 4 5)))
         (dropped (channel:drop-stream source 2)))

    (multiple-value-bind (value more-p) (channel:stream-next dropped)
      (assert-true more-p)
      (assert-= value 3)) ; First value after dropping 1,2

    (multiple-value-bind (value more-p) (channel:stream-next dropped)
      (assert-true more-p)
      (assert-= value 4))

    (multiple-value-bind (value more-p) (channel:stream-next dropped)
      (assert-true (not more-p))
      (assert-= value 5))))

(deftest test-stream-collect ()
  "Test collecting stream into list"
  (let* ((source (channel:from-sequence '(1 2 3 4)))
         (mapped (channel:map-stream source (lambda (x) (* x x))))
         (result (channel:collect-stream mapped)))

    (assert-true (equal result '(1 4 9 16)))))

(deftest test-stream-fold ()
  "Test folding stream into single value"
  (let* ((source (channel:from-sequence '(1 2 3 4 5)))
         (sum (channel:fold-stream source 0 #'+)))

    (assert-= sum 15)))

(deftest test-stream-zip ()
  "Test zipping two streams"
  (let* ((stream1 (channel:from-sequence '(1 2 3)))
         (stream2 (channel:from-sequence '("a" "b" "c")))
         (zipped (channel:zip-stream stream1 stream2)))

    (multiple-value-bind (value more-p) (channel:stream-next zipped)
      (assert-true more-p)
      (assert-true (equal value '(1 "a"))))

    (multiple-value-bind (value more-p) (channel:stream-next zipped)
      (assert-true more-p)
      (assert-true (equal value '(2 "b"))))

    (multiple-value-bind (value more-p) (channel:stream-next zipped)
      (assert-true (not more-p))
      (assert-true (equal value '(3 "c"))))))

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
    (assert-true (equal result '(1 9 25)))))

(deftest test-stream-for-each ()
  "Test stream for-each operation"
  (let ((source (channel:from-sequence '(1 2 3)))
        (collected '()))

    (channel:for-each-stream source
                            (lambda (x) (push (* x 2) collected)))

    (assert-true (equal (reverse collected) '(2 4 6)))))

;;;; Channel Integration Tests

(deftest test-channel-stream-integration ()
  "Test integration between channels and streams"
  (let ((ch (channel:make-channel :capacity 5)))

    ;; Send values to channel
    (channel:send ch 1)
    (channel:send ch 2)
    (channel:send ch 3)
    (channel:close-channel ch)

    ;; Create stream from channel
    (let* ((stream (channel:from-channel ch))
           (result (channel:collect-stream stream)))

      (assert-true (equal result '(1 2 3))))))

;;;; Concurrent Tests (basic, since we can't easily test real concurrency)

(deftest test-channel-concurrent-simulation ()
  "Test channel behavior in simulated concurrent scenario"
  (let ((ch (channel:make-channel :capacity 2)))

    ;; Simulate producer
    (channel:send ch "item1")
    (channel:send ch "item2")

    ;; Channel should be full now
    (assert-true (not (channel:try-send ch "item3")))

    ;; Simulate consumer
    (multiple-value-bind (value received-p) (channel:receive ch)
      (assert-true received-p)
      (assert-true (string= value "item1")))

    ;; Now should be able to send again
    (assert-true (channel:try-send ch "item3"))

    ;; Receive remaining items
    (multiple-value-bind (value received-p) (channel:receive ch)
      (assert-true received-p)
      (assert-true (string= value "item2")))

    (multiple-value-bind (value received-p) (channel:receive ch)
      (assert-true received-p)
      (assert-true (string= value "item3")))))

;;;; Error Handling Tests

(deftest test-stream-error-propagation ()
  "Test error handling in stream operations"
  (let* ((source (channel:from-sequence '(1 2 0 4)))
         (mapped (channel:map-stream source (lambda (x) (/ 10 x)))))

    ;; First two should work
    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (assert-true more-p)
      (assert-= value 10))

    (multiple-value-bind (value more-p) (channel:stream-next mapped)
      (assert-true more-p)
      (assert-= value 5))

    ;; Third should error due to division by zero
    (assert-condition (division-by-zero)
      (channel:stream-next mapped))))

;;;; Concurrency Tests

(deftest test-channel-concurrent-producers-consumers ()
  "Test multiple producer and consumer threads on a bounded channel.
   Verifies all messages are delivered exactly once with no data loss."
  (let* ((ch (channel:make-channel :capacity 10))
         (n-producers 4)
         (n-consumers 4)
         (items-per-producer 100)
         (total-items (* n-producers items-per-producer))
         (received (make-array total-items :initial-element nil))
         (received-lock (epsilon.sys.lock:make-lock "received-lock"))
         (threads nil))
    ;; Start producer threads
    (dotimes (p n-producers)
      (let ((p p))  ; capture before closure
        (push (epsilon.sys.thread:make-thread
               (lambda ()
                 (dotimes (i items-per-producer)
                   (channel:send ch (+ (* p items-per-producer) i))))
               :name (format nil "producer-~D" p))
              threads)))
    ;; Start consumer threads
    (dotimes (c n-consumers)
      (push (epsilon.sys.thread:make-thread
             (lambda ()
               (loop
                 (multiple-value-bind (value ok) (channel:receive ch :timeout 2)
                   (unless ok (return))
                   (epsilon.sys.lock:with-lock (received-lock)
                     (setf (aref received value) t)))))
             :name (format nil "consumer-~D" c))
            threads))
    ;; Wait for producers to finish
    (dolist (th threads)
      (when (search "producer" (epsilon.sys.thread:thread-name th))
        (epsilon.sys.thread:join-thread th)))
    ;; Close channel to signal consumers
    (channel:close-channel ch)
    ;; Wait for consumers to finish
    (dolist (th threads)
      (when (search "consumer" (epsilon.sys.thread:thread-name th))
        (epsilon.sys.thread:join-thread th)))
    ;; Verify all items received
    (assert-= (count t received) total-items)))
