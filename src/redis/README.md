# Epsilon Redis Client

A high-performance Redis client for Epsilon with connection pooling, pipelining, transactions, and pub/sub support.

## Features

- **Full Redis Command Support**: All standard Redis commands implemented as Lisp functions
- **Connection Pooling**: Automatic connection management with configurable pool sizes
- **Pipelining**: Batch multiple commands for improved performance
- **Transactions**: MULTI/EXEC transaction support with automatic command queuing
- **Pub/Sub**: Full publish/subscribe support with pattern subscriptions
- **RESP3 Protocol**: Complete implementation of Redis Serialization Protocol v3
- **Type Safety**: Automatic type conversion between Lisp and Redis types
- **Error Handling**: Comprehensive error handling with specific condition types
- **Thread Safe**: Connection pool with thread-safe operations

## Quick Start

### Basic Usage

```lisp
(use-package :epsilon.redis)

;; Connect to Redis
(with-redis (redis "redis://localhost:6379")
  ;; Set and get values
  (redis-set redis "key" "value")
  (redis-get redis "key"))  ; => "value"

;; Using connection string with auth and database
(with-redis (redis "redis://:password@localhost:6379/2")
  (ping redis))  ; => "PONG"
```

### Connection Pooling

```lisp
;; Create a connection pool
(defparameter *pool* 
  (create-redis-pool :host "localhost"
                     :port 6379
                     :max-size 20
                     :idle-timeout 300))

;; Use pooled connections
(with-pooled-redis (redis *pool*)
  (incr redis "counter"))
```

### Pipelining

```lisp
;; Execute multiple commands in a pipeline
(with-redis (redis "redis://localhost")
  (with-pipeline (redis)
    (redis-set redis "key1" "value1")
    (redis-set redis "key2" "value2")
    (redis-get redis "key1")
    (redis-get redis "key2")))
;; => ("OK" "OK" "value1" "value2")
```

### Transactions

```lisp
;; Execute commands in a transaction
(with-redis (redis "redis://localhost")
  (with-transaction (redis)
    (incr redis "counter1")
    (incr redis "counter2")
    (incr redis "counter3")))
;; => (1 2 3)
```

## API Reference

### Connection Management

**`connect`** `(host &key port password database timeout)`

Connect to Redis server. Returns a connection object.

```lisp
(let ((conn (connect "localhost" :port 6379 :password "secret")))
  (unwind-protect
       (ping conn)
    (disconnect conn)))
```

**`with-redis`** `((var connection-spec) &body body)`

Execute body with a Redis connection. Automatically handles connection/disconnection.

```lisp
(with-redis (redis "redis://localhost:6379")
  (redis-set redis "foo" "bar"))
```

**`with-pooled-redis`** `((var pool) &body body)`

Execute body with a connection from pool.

```lisp
(with-pooled-redis (redis *my-pool*)
  (redis-get redis "key"))
```

### String Commands

**`redis-get`** / **`redis-set`** - Get/set string values

```lisp
(redis-set redis "name" "Alice")
(redis-get redis "name")  ; => "Alice"

;; With expiration
(redis-set redis "temp" "data" :ex 60)  ; Expires in 60 seconds
```

**`mget`** / **`mset`** - Get/set multiple values

```lisp
(mset redis "k1" "v1" "k2" "v2" "k3" "v3")
(mget redis "k1" "k2" "k3")  ; => ("v1" "v2" "v3")
```

**`incr`** / **`decr`** - Increment/decrement counters

```lisp
(incr redis "counter")     ; => 1
(incrby redis "counter" 5) ; => 6
(decr redis "counter")     ; => 5
```

### List Commands

```lisp
;; Push/pop operations
(lpush redis "list" "first" "second" "third")
(rpush redis "list" "last")
(lpop redis "list")   ; => "third"
(rpop redis "list")   ; => "last"

;; Range operations
(lrange redis "list" 0 -1)  ; Get all elements
(llen redis "list")         ; Get length
```

### Set Commands

```lisp
;; Add/remove members
(sadd redis "set" "a" "b" "c")
(srem redis "set" "b")
(smembers redis "set")  ; => ("a" "c")

;; Set operations
(sadd redis "set1" "a" "b" "c")
(sadd redis "set2" "b" "c" "d")
(sinter redis "set1" "set2")  ; => ("b" "c")
(sunion redis "set1" "set2")  ; => ("a" "b" "c" "d")
```

### Hash Commands

```lisp
;; Set/get hash fields
(hset redis "user:123" "name" "Bob" "age" "30")
(hget redis "user:123" "name")     ; => "Bob"
(hgetall redis "user:123")         ; => map with name->Bob, age->30

;; Multiple fields
(hmset redis "user:456" "name" "Alice" "age" "25")
(hmget redis "user:456" "name" "age")  ; => ("Alice" "25")
```

### Sorted Set Commands

```lisp
;; Add members with scores
(zadd redis "scores" 100 "alice" 95 "bob" 87 "charlie")

;; Get by rank
(zrange redis "scores" 0 -1)                ; => ("charlie" "bob" "alice")
(zrange redis "scores" 0 -1 :withscores t)  ; => ("charlie" "87" "bob" "95" "alice" "100")

;; Get by score
(zrangebyscore redis "scores" 90 100)  ; => ("bob" "alice")
```

### Pub/Sub

```lisp
;; Publishing
(with-redis (redis "redis://localhost")
  (publish redis "news" "Breaking news!"))

;; Subscribing (blocking)
(with-redis (redis "redis://localhost")
  (with-pubsub (redis)
    (subscribe redis "news" "updates")
    (loop for message = (receive-pubsub-message redis)
          do (format t "Received: ~A~%" message))))
```

### Transactions

```lisp
;; Basic transaction
(with-transaction (redis)
  (redis-set redis "key1" "value1")
  (redis-set redis "key2" "value2"))

;; With WATCH for optimistic locking
(watch redis "balance")
(let ((balance (parse-integer (redis-get redis "balance"))))
  (with-transaction (redis)
    (redis-set redis "balance" (princ-to-string (+ balance 100)))))
```

### Pipeline

```lisp
;; Batch commands for performance
(with-pipeline (redis)
  (dotimes (i 1000)
    (redis-set redis (format nil "key~D" i) i)))
```

## Connection Pool Configuration

```lisp
(create-redis-pool &key host port password database 
                        max-size timeout idle-timeout)
```

- `max-size`: Maximum connections in pool (default: 10)
- `timeout`: Connection acquisition timeout in seconds (default: 5)
- `idle-timeout`: Idle connection timeout in seconds (default: 300)

### Pool Statistics

```lisp
(let ((stats (pool-statistics *pool*)))
  (format t "Connections created: ~D~%" 
          (pool-stats-connections-created stats))
  (format t "Pool hits: ~D~%" 
          (pool-stats-pool-hits stats)))
```

## Error Handling

```lisp
(handler-case
    (with-redis (redis "redis://localhost:6379")
      (redis-get redis "key"))
  (redis-connection-error (e)
    (format t "Connection failed: ~A~%" (error-message e)))
  (redis-command-error (e)
    (format t "Command failed: ~A~%" (error-message e)))
  (redis-protocol-error (e)
    (format t "Protocol error: ~A~%" (error-message e))))
```

## Performance Tips

1. **Use Connection Pooling**: Reuse connections instead of creating new ones
2. **Pipeline Commands**: Batch multiple commands to reduce round trips
3. **Use Binary Data**: Pass byte arrays directly for binary data
4. **Avoid Large Keys**: Redis performance degrades with very large keys/values
5. **Monitor Pool Stats**: Track connection usage to optimize pool size

## Thread Safety

- Connection pools are thread-safe
- Individual connections should not be shared between threads
- Use `with-pooled-redis` for concurrent access

## Limitations

- Cluster mode not yet supported
- Redis Streams API not yet implemented
- Lua scripting not yet implemented
- Redis 6+ ACL commands not yet implemented

## Examples

### Cache Implementation

```lisp
(defun cached-get (key compute-fn &key (ttl 3600))
  (with-pooled-redis (redis *cache-pool*)
    (or (redis-get redis key)
        (let ((value (funcall compute-fn)))
          (redis-set redis key value :ex ttl)
          value))))
```

### Rate Limiter

```lisp
(defun rate-limit-check (key limit window)
  (with-pooled-redis (redis *pool*)
    (with-pipeline (redis)
      (incr redis key)
      (expire redis key window))
    (let ((count (first (pipeline-execute redis))))
      (<= count limit))))
```

### Session Store

```lisp
(defun save-session (session-id data &key (ttl 1800))
  (with-pooled-redis (redis *session-pool*)
    (redis-set redis (format nil "session:~A" session-id)
         (encode-session-data data)
         :ex ttl)))

(defun load-session (session-id)
  (with-pooled-redis (redis *session-pool*)
    (let ((data (redis-get redis (format nil "session:~A" session-id))))
      (when data
        (decode-session-data data)))))
```