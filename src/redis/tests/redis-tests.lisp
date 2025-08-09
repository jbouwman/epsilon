;;;; Redis Client Tests
;;;;
;;;; Test suite for the Redis client implementation

(defpackage :epsilon.redis.tests
  (:use :cl :epsilon.test)
  (:import-from #:epsilon.redis
   ;; Import all the symbols we need for testing
   #:with-redis #:connect #:disconnect #:ping #:echo
   #:redis-get #:redis-set #:del #:exists #:keys
   #:lpush #:lpop #:lrange
   #:sadd #:smembers
   #:with-transaction #:multi #:exec
   #:redis-connection-error)
  (:local-nicknames
   (#:protocol #:epsilon.redis.protocol)))

(in-package :epsilon.redis.tests)

;;; Test helpers

(defparameter *test-redis-host* "localhost")
(defparameter *test-redis-port* 6379)

(defmacro with-test-redis ((var) &body body)
  "Execute body with a test Redis connection"
  `(with-redis (,var (format nil "redis://~A:~D" *test-redis-host* *test-redis-port*))
     ;; Clean up test keys
     (dolist (key (keys ,var "test:*"))
       (del ,var key))
     ,@body))

;;; Protocol tests

(deftest test-protocol-encoding
  "Test RESP3 protocol encoding"
  ;; Simple command
  (is-equal "*2\r\n$3\r\nGET\r\n$3\r\nkey\r\n"
            (protocol:encode-command "GET" "key"))
  
  ;; Command with multiple args
  (is-equal "*4\r\n$3\r\nSET\r\n$3\r\nkey\r\n$5\r\nvalue\r\n$2\r\nEX\r\n"
            (protocol:encode-command "SET" "key" "value" "EX"))
  
  ;; Command with integer
  (is-equal "*3\r\n$6\r\nINCRBY\r\n$7\r\ncounter\r\n$2\r\n10\r\n"
            (protocol:encode-command "INCRBY" "counter" 10)))

;;; Connection tests

(deftest test-connection-lifecycle
  "Test connection creation and disconnection"
  (let ((conn nil))
    ;; Connect
    (setf conn (connect *test-redis-host* :port *test-redis-port*))
    (is-true (not (null conn)))
    
    ;; Ping
    (is-equal "PONG" (ping conn))
    
    ;; Disconnect
    (disconnect conn)))

;;; Basic command tests

(deftest test-basic-commands
  "Test basic Redis commands"
  (with-test-redis (redis)
    ;; PING
    (is-equal "PONG" (ping redis))
    (is-equal "hello" (ping redis "hello"))
    
    ;; ECHO
    (is-equal "test message" (echo redis "test message"))
    
    ;; SET/GET
    (is-equal "OK" (redis-set redis "test:key" "value"))
    (is-equal "value" (redis-get redis "test:key"))
    
    ;; Non-existent key
    (is-equal nil (redis-get redis "test:nonexistent"))))

;;; String command tests

(deftest test-string-commands
  "Test string manipulation commands"
  (with-test-redis (redis)
    ;; Basic operations
    (redis-set redis "test:str" "hello")
    (is-equal "hello" (redis-get redis "test:str"))
    
    ;; DEL
    (is-equal 1 (del redis "test:str"))
    (is-equal nil (redis-get redis "test:str"))
    
    ;; EXISTS
    (redis-set redis "test:key1" "value1")
    (is-equal 1 (exists redis "test:key1"))
    (is-equal 0 (exists redis "test:nonexistent"))))

;;; List command tests

(deftest test-list-commands
  "Test list manipulation commands"
  (with-test-redis (redis)
    ;; LPUSH/LPOP
    (is-equal 1 (lpush redis "test:list" "first"))
    (is-equal 2 (lpush redis "test:list" "second"))
    (is-equal "second" (lpop redis "test:list"))
    
    ;; LRANGE
    (del redis "test:list")
    (lpush redis "test:list" "a")
    (lpush redis "test:list" "b") 
    (lpush redis "test:list" "c")
    (is-equal '("c" "b" "a") (lrange redis "test:list" 0 -1))))

;;; Set command tests

(deftest test-set-commands
  "Test set manipulation commands"
  (with-test-redis (redis)
    ;; SADD
    (is-equal 1 (sadd redis "test:set" "a"))
    (is-equal 1 (sadd redis "test:set" "b"))
    (is-equal 0 (sadd redis "test:set" "a"))  ; Already exists
    
    ;; SMEMBERS
    (let ((members (smembers redis "test:set")))
      (is-equal 2 (length members))
      (is-true (member "a" members :test #'equal))
      (is-true (member "b" members :test #'equal)))))

;;; Transaction tests

(deftest test-transactions
  "Test transaction support"
  (with-test-redis (redis)
    ;; Basic transaction
    (let ((results (with-transaction (redis)
                     (redis-set redis "test:tx1" "value1")
                     (redis-set redis "test:tx2" "value2")
                     (redis-get redis "test:tx1")
                     (redis-get redis "test:tx2"))))
      (is-equal 4 (length results))
      (is-equal "OK" (first results))
      (is-equal "OK" (second results))
      (is-equal "value1" (third results))
      (is-equal "value2" (fourth results)))))

;;; Error handling tests

(deftest test-error-handling
  "Test error conditions"
  ;; Connection errors
  (is-thrown 'redis-connection-error
    (connect "nonexistent.host" :port 6379)))

;;; Run all tests

(defun run-redis-tests ()
  "Run all Redis client tests"
  (run-package-tests :epsilon.redis.tests))