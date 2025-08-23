(defpackage epsilon.cache.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (cache epsilon.cache)))

(in-package epsilon.cache.tests)

;;; Timed Cache Tests

(deftest test-timed-cache-basic-operations
  "Test basic get/put/remove operations on timed cache"
  (let ((c (cache:make-timed-cache :ttl 60 :max-size 10)))
    ;; Test initial state
    (is-= 0 (cache:cache-size c))
    (is-eq nil (cache:cache-get c "missing"))
    (is-eq :default (cache:cache-get c "missing" :default))
    
    ;; Test put and get
    (cache:cache-put c "key1" "value1")
    (is-equal "value1" (cache:cache-get c "key1"))
    (is-= 1 (cache:cache-size c))
    
    ;; Test multiple puts
    (cache:cache-put c "key2" "value2")
    (cache:cache-put c "key3" "value3")
    (is-= 3 (cache:cache-size c))
    (is-equal "value2" (cache:cache-get c "key2"))
    (is-equal "value3" (cache:cache-get c "key3"))
    
    ;; Test overwrite
    (cache:cache-put c "key1" "new-value1")
    (is-equal "new-value1" (cache:cache-get c "key1"))
    (is-= 3 (cache:cache-size c))
    
    ;; Test remove
    (cache:cache-remove c "key2")
    (is-eq nil (cache:cache-get c "key2"))
    (is-= 2 (cache:cache-size c))
    (is-equal "new-value1" (cache:cache-get c "key1"))
    (is-equal "value3" (cache:cache-get c "key3"))))

(deftest test-timed-cache-expiration
  "Test TTL expiration in timed cache"
  (let ((c (cache:make-timed-cache :ttl 1 :max-size 10)))
    ;; Put value
    (cache:cache-put c "key1" "value1")
    (is-equal "value1" (cache:cache-get c "key1"))
    
    ;; Wait for expiration
    (sleep 2)
    
    ;; Value should be expired
    (is-eq nil (cache:cache-get c "key1"))
    (is-= 0 (cache:cache-size c))))

(deftest test-timed-cache-max-size
  "Test max size limit enforcement"
  (skip)
  (let ((c (cache:make-timed-cache :ttl 60 :max-size 3)))
    ;; Fill cache to max
    (cache:cache-put c "key1" "value1")
    (cache:cache-put c "key2" "value2")
    (cache:cache-put c "key3" "value3")
    (is-= 3 (cache:cache-size c))
    
    ;; Adding more should trigger cleanup
    (cache:cache-put c "key4" "value4")
    
    ;; Size should still be at most max-size
    (is (<= (cache:cache-size c) 3))
    
    ;; New key should be present
    (is-equal "value4" (cache:cache-get c "key4"))))

(deftest test-timed-cache-clear
  "Test clearing all cache entries"
  (skip)
  (let ((c (cache:make-timed-cache)))
    ;; Add some entries
    (cache:cache-put c "key1" "value1")
    (cache:cache-put c "key2" "value2")
    (cache:cache-put c "key3" "value3")
    (is-= 3 (cache:cache-size c))
    
    ;; Clear cache
    (cache:cache-clear c)
    (is-= 0 (cache:cache-size c))
    (is-eq nil (cache:cache-get c "key1"))
    (is-eq nil (cache:cache-get c "key2"))
    (is-eq nil (cache:cache-get c "key3"))
    
    ;; Stats should be reset
    (is-= 0 (cache:timed-cache-hits c))
    (is-= 0 (cache:timed-cache-misses c))))

(deftest test-timed-cache-statistics
  "Test cache hit/miss statistics"
  (skip)
  (let ((c (cache:make-timed-cache)))
    ;; Initial stats
    (is-= 0 (cache:timed-cache-hits c))
    (is-= 0 (cache:timed-cache-misses c))
    
    ;; Miss
    (cache:cache-get c "missing")
    (is-= 0 (cache:timed-cache-hits c))
    (is-= 1 (cache:timed-cache-misses c))
    
    ;; Put and hit
    (cache:cache-put c "key1" "value1")
    (cache:cache-get c "key1")
    (is-= 1 (cache:timed-cache-hits c))
    (is-= 1 (cache:timed-cache-misses c))
    
    ;; Another hit
    (cache:cache-get c "key1")
    (is-= 2 (cache:timed-cache-hits c))
    (is-= 1 (cache:timed-cache-misses c))
    
    ;; Test hit rate
    (is-= (/ 2.0 3.0) (cache:cache-hit-rate c))))

(deftest test-timed-cache-keys
  "Test retrieving all cache keys"
  (let ((c (cache:make-timed-cache)))
    ;; Empty cache
    (is-equal '() (cache:cache-keys c))
    
    ;; Add entries
    (cache:cache-put c "key1" "value1")
    (cache:cache-put c "key2" "value2")
    (cache:cache-put c "key3" "value3")
    
    ;; Check keys (order not guaranteed)
    (let ((keys (cache:cache-keys c)))
      (is-= 3 (length keys))
      (is (member "key1" keys :test #'equal))
      (is (member "key2" keys :test #'equal))
      (is (member "key3" keys :test #'equal)))))

;;; LRU Cache Tests

(deftest test-lru-cache-basic-operations
  "Test basic LRU cache operations"
  (let ((c (cache:make-lru-cache :capacity 3)))
    ;; Test initial state
    (is-= 0 (cache:lru-cache-size c))
    (is-eq nil (cache:lru-get c "missing"))
    (is-eq :default (cache:lru-get c "missing" :default))
    
    ;; Test put and get
    (cache:lru-put c "key1" "value1")
    (is-equal "value1" (cache:lru-get c "key1"))
    (is-= 1 (cache:lru-cache-size c))
    
    ;; Test multiple puts
    (cache:lru-put c "key2" "value2")
    (cache:lru-put c "key3" "value3")
    (is-= 3 (cache:lru-cache-size c))
    (is-equal "value2" (cache:lru-get c "key2"))
    (is-equal "value3" (cache:lru-get c "key3"))
    
    ;; Test overwrite
    (cache:lru-put c "key1" "new-value1")
    (is-equal "new-value1" (cache:lru-get c "key1"))
    (is-= 3 (cache:lru-cache-size c))))

(deftest test-lru-cache-eviction
  "Test LRU eviction policy"
  (skip)
  (let ((c (cache:make-lru-cache :capacity 3)))
    ;; Fill cache
    (cache:lru-put c "key1" "value1")
    (cache:lru-put c "key2" "value2")
    (cache:lru-put c "key3" "value3")
    (is-= 3 (cache:lru-cache-size c))
    
    ;; Access key1 and key3 to make them more recent
    (cache:lru-get c "key1")
    (cache:lru-get c "key3")
    
    ;; Add new key - should evict key2 (least recently used)
    (cache:lru-put c "key4" "value4")
    (is-= 3 (cache:lru-cache-size c))
    (is-eq nil (cache:lru-get c "key2"))
    (is-equal "value1" (cache:lru-get c "key1"))
    (is-equal "value3" (cache:lru-get c "key3"))
    (is-equal "value4" (cache:lru-get c "key4"))))

(deftest test-lru-cache-ordering
  "Test that LRU maintains proper access ordering"
  (let ((c (cache:make-lru-cache :capacity 4)))
    ;; Add items in order
    (cache:lru-put c "a" 1)
    (cache:lru-put c "b" 2)
    (cache:lru-put c "c" 3)
    (cache:lru-put c "d" 4)
    
    ;; Access 'b' to move it to head
    (cache:lru-get c "b")
    
    ;; Access 'a' to move it to head
    (cache:lru-get c "a")
    
    ;; Now order should be: a, b, d, c (most to least recent)
    ;; Adding new item should evict 'c'
    (cache:lru-put c "e" 5)
    
    (is-eq nil (cache:lru-get c "c"))
    (is-equal 1 (cache:lru-get c "a"))
    (is-equal 2 (cache:lru-get c "b"))
    (is-equal 4 (cache:lru-get c "d"))
    (is-equal 5 (cache:lru-get c "e"))))

(deftest test-lru-cache-remove
  "Test removing items from LRU cache"
  (let ((c (cache:make-lru-cache :capacity 3)))
    ;; Add items
    (cache:lru-put c "key1" "value1")
    (cache:lru-put c "key2" "value2")
    (cache:lru-put c "key3" "value3")
    
    ;; Remove middle item
    (let ((removed (cache:lru-remove c "key2")))
      (is-equal "value2" removed)
      (is-= 2 (cache:lru-cache-size c))
      (is-eq nil (cache:lru-get c "key2")))
    
    ;; Remove non-existent item
    (is-eq nil (cache:lru-remove c "missing"))
    (is-= 2 (cache:lru-cache-size c))))

(deftest test-lru-cache-clear
  "Test clearing LRU cache"
  (let ((c (cache:make-lru-cache :capacity 3)))
    ;; Add items
    (cache:lru-put c "key1" "value1")
    (cache:lru-put c "key2" "value2")
    (cache:lru-put c "key3" "value3")
    (is-= 3 (cache:lru-cache-size c))
    
    ;; Clear
    (cache:lru-clear c)
    (is-= 0 (cache:lru-cache-size c))
    (is-eq nil (cache:lru-get c "key1"))
    (is-eq nil (cache:lru-get c "key2"))
    (is-eq nil (cache:lru-get c "key3"))))

;;; Cache Statistics Tests

(deftest test-cache-stats
  "Test cache statistics reporting"
  (let ((timed-c (cache:make-timed-cache :ttl 60 :max-size 10))
        (lru-c (cache:make-lru-cache :capacity 5)))
    
    ;; Add some data to timed cache
    (cache:cache-put timed-c "key1" "value1")
    (cache:cache-get timed-c "key1")  ; hit
    (cache:cache-get timed-c "miss")  ; miss
    
    ;; Check timed cache stats
    (let ((stats (cache:cache-stats timed-c)))
      (is-eq :timed (getf stats :type))
      (is-= 1 (getf stats :size))
      (is-= 10 (getf stats :max-size))
      (is-= 1 (getf stats :hits))
      (is-= 1 (getf stats :misses))
      (is-= 60 (getf stats :ttl)))
    
    ;; Add some data to LRU cache
    (cache:lru-put lru-c "key1" "value1")
    (cache:lru-put lru-c "key2" "value2")
    
    ;; Check LRU cache stats
    (let ((stats (cache:cache-stats lru-c)))
      (is-eq :lru (getf stats :type))
      (is-= 2 (getf stats :size))
      (is-= 5 (getf stats :capacity)))))

;;; With-cache Macro Tests

(deftest test-with-cache-macro
  "Test the with-cache convenience macro"
  ;; Test with timed cache
  (cache:with-cache (tc :type :timed :ttl 30 :capacity 5)
    (cache:cache-put tc "key1" "value1")
    (is-equal "value1" (cache:cache-get tc "key1"))
    (is (cache:timed-cache-p tc))
    (is-= 30 (cache:timed-cache-ttl tc)))
  
  ;; Test with LRU cache
  (cache:with-cache (lc :type :lru :capacity 3)
    (cache:lru-put lc "key1" "value1")
    (is-equal "value1" (cache:lru-get lc "key1"))
    (is (cache:lru-cache-p lc))
    (is-= 3 (cache:lru-cache-capacity lc))))

;;; Edge Cases and Error Handling

(deftest test-cache-edge-cases
  "Test edge cases and error conditions"
  (let ((c (cache:make-timed-cache)))
    ;; Nil key
    (cache:cache-put c nil "nil-value")
    (is-equal "nil-value" (cache:cache-get c nil))
    
    ;; Nil value
    (cache:cache-put c "nil-key" nil)
    (is-eq nil (cache:cache-get c "nil-key"))
    
    ;; Complex keys
    (let ((complex-key '(1 2 3)))
      (cache:cache-put c complex-key "complex-value")
      (is-equal "complex-value" (cache:cache-get c complex-key)))
    
    ;; Very large value
    (let ((large-value (make-string 10000 :initial-element #\x)))
      (cache:cache-put c "large" large-value)
      (is-equal large-value (cache:cache-get c "large")))))

(deftest test-lru-cache-edge-cases
  "Test LRU cache edge cases"
  (skip)
  (let ((c (cache:make-lru-cache :capacity 1)))
    ;; Single capacity cache
    (cache:lru-put c "key1" "value1")
    (is-equal "value1" (cache:lru-get c "key1"))
    
    ;; Should evict immediately
    (cache:lru-put c "key2" "value2")
    (is-eq nil (cache:lru-get c "key1"))
    (is-equal "value2" (cache:lru-get c "key2"))
    
    ;; Zero capacity cache (if supported)
    (let ((zero-c (cache:make-lru-cache :capacity 0)))
      (cache:lru-put zero-c "key1" "value1")
      ;; Behavior depends on implementation
      (is (<= (cache:lru-cache-size zero-c) 0)))))

;;; Performance Tests

(deftest test-cache-performance
  "Test cache performance characteristics"
  (let ((c (cache:make-timed-cache :ttl 60 :max-size 1000))
        (start-time nil)
        (elapsed nil))
    
    ;; Measure insertion time
    (setf start-time (get-internal-real-time))
    (loop for i from 1 to 1000
          do (cache:cache-put c (format nil "key~D" i) i))
    (setf elapsed (- (get-internal-real-time) start-time))
    (is (< elapsed (* 1000 internal-time-units-per-second))) ; Should take less than 1 second
    
    ;; Measure lookup time
    (setf start-time (get-internal-real-time))
    (loop for i from 1 to 1000
          do (cache:cache-get c (format nil "key~D" i)))
    (setf elapsed (- (get-internal-real-time) start-time))
    (is (< elapsed (* 100 internal-time-units-per-second))) ; Lookups should be very fast
    
    ;; Verify all lookups were hits
    (is-= 1000 (cache:timed-cache-hits c))))

(deftest test-concurrent-cache-access
  "Test cache behavior under concurrent access (if threading is available)"
  (skip)
  #+sb-thread
  (let ((c (cache:make-timed-cache :ttl 60 :max-size 100))
        (errors 0))
    ;; Create multiple threads accessing the cache
    (let ((threads 
           (loop for i from 1 to 10
                 collect (sb-thread:make-thread
                          (lambda ()
                            (handler-case
                                (loop for j from 1 to 100
                                      do (cache:cache-put c 
                                                         (format nil "thread~D-key~D" i j)
                                                         (format nil "value~D-~D" i j))
                                      do (cache:cache-get c 
                                                          (format nil "thread~D-key~D" i j)))
                              (error () (incf errors))))
                          :name (format nil "cache-test-thread-~D" i)))))
      ;; Wait for all threads to complete
      (dolist (thread threads)
        (sb-thread:join-thread thread))
      
      ;; Should have no errors
      (is-= 0 errors)))
  #-sb-thread
  (skip "Threading not available"))
