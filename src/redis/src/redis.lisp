;;;; Redis Client Main Package
;;;;
;;;; Unifies all Redis sub-packages and re-exports their functionality

(defpackage :epsilon.redis
  (:use :cl)
  (:import-from #:epsilon.redis.connection
   #:redis-connection
   #:connect
   #:disconnect
   #:connection-alive-p
   #:with-redis
   #:redis-error
   #:redis-connection-error
   #:redis-command-error)
  (:import-from #:epsilon.redis.commands
   ;; Connection commands
   #:ping #:echo #:quit
   ;; Key commands
   #:del #:exists #:expire #:expireat #:ttl #:pttl #:persist
   #:keys #:scan #:redis-type #:rename #:renamenx
   ;; String commands
   #:redis-get #:redis-set #:getset #:mget #:mset #:strlen #:redis-append
   #:getrange #:setrange #:incr #:incrby #:incrbyfloat #:decr #:decrby
   ;; List commands
   #:lpush #:rpush #:lpop #:rpop #:llen #:lrange #:lindex
   #:lset #:lrem #:ltrim #:linsert
   ;; Set commands
   #:sadd #:srem #:smembers #:sismember #:scard #:sdiff
   #:sinter #:sunion #:smove #:spop #:srandmember
   ;; Sorted set commands
   #:zadd #:zrem #:zrange #:zrevrange #:zrangebyscore
   #:zrank #:zrevrank #:zcard #:zcount #:zscore #:zincrby
   ;; Hash commands
   #:hset #:hget #:hdel #:hexists #:hgetall #:hkeys #:hvals
   #:hlen #:hmget #:hmset #:hincrby #:hincrbyfloat #:hsetnx
   ;; Transaction commands
   #:multi #:exec #:discard #:watch #:unwatch
   #:with-transaction #:with-pipeline #:pipeline-execute
   ;; Pub/Sub commands
   #:publish #:subscribe #:unsubscribe #:psubscribe #:punsubscribe)
  (:import-from #:epsilon.redis.pool
   #:create-redis-pool
   #:with-pooled-redis
   #:*default-redis-pool*
   #:pool-stats)
  ;; Re-export everything
  (:export
   ;; Connection management
   #:redis-connection
   #:connect
   #:disconnect
   #:with-redis
   #:connection-alive-p
   
   ;; Basic commands
   #:ping
   #:echo
   #:quit
   
   ;; String operations
   #:redis-get
   #:redis-set
   #:getset
   #:mget
   #:mset
   #:strlen
   #:redis-append
   #:getrange
   #:setrange
   #:incr
   #:incrby
   #:incrbyfloat
   #:decr
   #:decrby
   
   ;; Key operations
   #:del
   #:exists
   #:expire
   #:expireat
   #:ttl
   #:pttl
   #:persist
   #:keys
   #:scan
   #:redis-type
   #:rename
   #:renamenx
   
   ;; List operations
   #:lpush
   #:rpush
   #:lpop
   #:rpop
   #:llen
   #:lrange
   #:lindex
   #:lset
   #:lrem
   #:ltrim
   #:linsert
   
   ;; Set operations
   #:sadd
   #:srem
   #:smembers
   #:sismember
   #:scard
   #:sdiff
   #:sinter
   #:sunion
   #:smove
   #:spop
   #:srandmember
   
   ;; Sorted set operations
   #:zadd
   #:zrem
   #:zrange
   #:zrevrange
   #:zrangebyscore
   #:zrank
   #:zrevrank
   #:zcard
   #:zcount
   #:zscore
   #:zincrby
   
   ;; Hash operations
   #:hset
   #:hget
   #:hdel
   #:hexists
   #:hgetall
   #:hkeys
   #:hvals
   #:hlen
   #:hmget
   #:hmset
   #:hincrby
   #:hincrbyfloat
   #:hsetnx
   
   ;; Pub/Sub
   #:publish
   #:subscribe
   #:unsubscribe
   #:psubscribe
   #:punsubscribe
   
   ;; Transactions
   #:multi
   #:exec
   #:discard
   #:watch
   #:unwatch
   #:with-transaction
   
   ;; Pipeline
   #:with-pipeline
   #:pipeline-execute
   
   ;; Connection pool
   #:create-redis-pool
   #:with-pooled-redis
   #:*default-redis-pool*
   
   ;; Conditions
   #:redis-error
   #:redis-connection-error
   #:redis-command-error))

(in-package :epsilon.redis)