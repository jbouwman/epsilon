;;;; Redis Commands Implementation
;;;;
;;;; Implements all standard Redis commands as Lisp functions

(defpackage :epsilon.redis.commands
  (:use :cl)
  (:local-nicknames
   (#:conn #:epsilon.redis.connection)
   (#:map #:epsilon.map))
  (:export
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
   #:publish #:subscribe #:unsubscribe #:psubscribe #:punsubscribe))

(in-package :epsilon.redis.commands)

;;; Connection commands

(defun ping (connection &optional message)
  "Ping the Redis server"
  (if message
      (conn:execute-command connection "PING" message)
      (conn:execute-command connection "PING")))

(defun echo (connection message)
  "Echo a message"
  (conn:execute-command connection "ECHO" message))

(defun quit (connection)
  "Close the connection"
  (conn:execute-command connection "QUIT"))

;;; Key commands

(defun del (connection &rest keys)
  "Delete one or more keys"
  (apply #'conn:execute-command connection "DEL" keys))

(defun exists (connection &rest keys)
  "Check if keys exist"
  (apply #'conn:execute-command connection "EXISTS" keys))

(defun expire (connection key seconds)
  "Set a key's time to live in seconds"
  (conn:execute-command connection "EXPIRE" key (princ-to-string seconds)))

(defun expireat (connection key timestamp)
  "Set the expiration for a key as a UNIX timestamp"
  (conn:execute-command connection "EXPIREAT" key (princ-to-string timestamp)))

(defun ttl (connection key)
  "Get the time to live for a key in seconds"
  (conn:execute-command connection "TTL" key))

(defun pttl (connection key)
  "Get the time to live for a key in milliseconds"
  (conn:execute-command connection "PTTL" key))

(defun persist (connection key)
  "Remove the expiration from a key"
  (conn:execute-command connection "PERSIST" key))

(defun keys (connection pattern)
  "Find all keys matching the pattern"
  (conn:execute-command connection "KEYS" pattern))

(defun scan (connection cursor &key match count)
  "Incrementally iterate the keyspace"
  (let ((args (list "SCAN" (princ-to-string cursor))))
    (when match
      (push "MATCH" args)
      (push match args))
    (when count
      (push "COUNT" args)
      (push (princ-to-string count) args))
    (apply #'conn:execute-command connection (nreverse args))))

(defun redis-type (connection key)
  "Determine the type stored at key"
  (conn:execute-command connection "TYPE" key))

(defun rename (connection key newkey)
  "Rename a key"
  (conn:execute-command connection "RENAME" key newkey))

(defun renamenx (connection key newkey)
  "Rename a key, only if the new key does not exist"
  (= 1 (conn:execute-command connection "RENAMENX" key newkey)))

;;; String commands

(defun redis-get (connection key)
  "Get the value of a key"
  (conn:execute-command connection "GET" key))

(defun redis-set (connection key value &key ex px exat pxat nx xx)
  "Set the string value of a key"
  (let ((args (list "SET" key value)))
    ;; Add optional arguments
    (when ex
      (push "EX" args)
      (push (princ-to-string ex) args))
    (when px
      (push "PX" args)
      (push (princ-to-string px) args))
    (when exat
      (push "EXAT" args)
      (push (princ-to-string exat) args))
    (when pxat
      (push "PXAT" args)
      (push (princ-to-string pxat) args))
    (when nx
      (push "NX" args))
    (when xx
      (push "XX" args))
    (apply #'conn:execute-command connection (nreverse args))))

(defun getset (connection key value)
  "Set the string value and return the old value"
  (conn:execute-command connection "GETSET" key value))

(defun mget (connection &rest keys)
  "Get the values of multiple keys"
  (apply #'conn:execute-command connection "MGET" keys))

(defun mset (connection &rest key-value-pairs)
  "Set multiple keys to multiple values"
  (apply #'conn:execute-command connection "MSET" key-value-pairs))

(defun strlen (connection key)
  "Get the length of a string value"
  (conn:execute-command connection "STRLEN" key))

(defun redis-append (connection key value)
  "Append a value to a key"
  (conn:execute-command connection "APPEND" key value))

(defun getrange (connection key start end)
  "Get a substring of the string stored at key"
  (conn:execute-command connection "GETRANGE" key 
                   (princ-to-string start) 
                   (princ-to-string end)))

(defun setrange (connection key offset value)
  "Overwrite part of a string at key starting at offset"
  (conn:execute-command connection "SETRANGE" key 
                   (princ-to-string offset) 
                   value))

(defun incr (connection key)
  "Increment the integer value of a key by one"
  (conn:execute-command connection "INCR" key))

(defun incrby (connection key increment)
  "Increment the integer value of a key by the given amount"
  (conn:execute-command connection "INCRBY" key (princ-to-string increment)))

(defun incrbyfloat (connection key increment)
  "Increment the float value of a key by the given amount"
  (conn:execute-command connection "INCRBYFLOAT" key (princ-to-string increment)))

(defun decr (connection key)
  "Decrement the integer value of a key by one"
  (conn:execute-command connection "DECR" key))

(defun decrby (connection key decrement)
  "Decrement the integer value of a key by the given amount"
  (conn:execute-command connection "DECRBY" key (princ-to-string decrement)))

;;; List commands

(defun lpush (connection key &rest values)
  "Prepend values to a list"
  (apply #'conn:execute-command connection "LPUSH" key values))

(defun rpush (connection key &rest values)
  "Append values to a list"
  (apply #'conn:execute-command connection "RPUSH" key values))

(defun lpop (connection key &optional count)
  "Remove and get the first elements in a list"
  (if count
      (conn:execute-command connection "LPOP" key (princ-to-string count))
      (conn:execute-command connection "LPOP" key)))

(defun rpop (connection key &optional count)
  "Remove and get the last elements in a list"
  (if count
      (conn:execute-command connection "RPOP" key (princ-to-string count))
      (conn:execute-command connection "RPOP" key)))

(defun llen (connection key)
  "Get the length of a list"
  (conn:execute-command connection "LLEN" key))

(defun lrange (connection key start stop)
  "Get a range of elements from a list"
  (conn:execute-command connection "LRANGE" key 
                   (princ-to-string start) 
                   (princ-to-string stop)))

(defun lindex (connection key index)
  "Get an element from a list by its index"
  (conn:execute-command connection "LINDEX" key (princ-to-string index)))

(defun lset (connection key index value)
  "Set the value of an element in a list by its index"
  (conn:execute-command connection "LSET" key (princ-to-string index) value))

(defun lrem (connection key count value)
  "Remove elements from a list"
  (conn:execute-command connection "LREM" key (princ-to-string count) value))

(defun ltrim (connection key start stop)
  "Trim a list to the specified range"
  (conn:execute-command connection "LTRIM" key 
                   (princ-to-string start) 
                   (princ-to-string stop)))

(defun linsert (connection key before-after pivot value)
  "Insert an element before or after another element in a list"
  (conn:execute-command connection "LINSERT" key 
                   (string-upcase (string before-after)) 
                   pivot value))

;;; Set commands

(defun sadd (connection key &rest members)
  "Add members to a set"
  (apply #'conn:execute-command connection "SADD" key members))

(defun srem (connection key &rest members)
  "Remove members from a set"
  (apply #'conn:execute-command connection "SREM" key members))

(defun smembers (connection key)
  "Get all members in a set"
  (conn:execute-command connection "SMEMBERS" key))

(defun sismember (connection key member)
  "Determine if a given value is a member of a set"
  (= 1 (conn:execute-command connection "SISMEMBER" key member)))

(defun scard (connection key)
  "Get the number of members in a set"
  (conn:execute-command connection "SCARD" key))

(defun sdiff (connection &rest keys)
  "Subtract multiple sets"
  (apply #'conn:execute-command connection "SDIFF" keys))

(defun sinter (connection &rest keys)
  "Intersect multiple sets"
  (apply #'conn:execute-command connection "SINTER" keys))

(defun sunion (connection &rest keys)
  "Add multiple sets"
  (apply #'conn:execute-command connection "SUNION" keys))

(defun smove (connection source destination member)
  "Move a member from one set to another"
  (= 1 (conn:execute-command connection "SMOVE" source destination member)))

(defun spop (connection key &optional count)
  "Remove and return random members from a set"
  (if count
      (conn:execute-command connection "SPOP" key (princ-to-string count))
      (conn:execute-command connection "SPOP" key)))

(defun srandmember (connection key &optional count)
  "Get random members from a set"
  (if count
      (conn:execute-command connection "SRANDMEMBER" key (princ-to-string count))
      (conn:execute-command connection "SRANDMEMBER" key)))

;;; Sorted set commands

(defun zadd (connection key &rest score-member-pairs)
  "Add members to a sorted set"
  (apply #'conn:execute-command connection "ZADD" key 
         (loop for (score member) on score-member-pairs by #'cddr
               collect (princ-to-string score)
               collect member)))

(defun zrem (connection key &rest members)
  "Remove members from a sorted set"
  (apply #'conn:execute-command connection "ZREM" key members))

(defun zrange (connection key start stop &key withscores)
  "Return a range of members in a sorted set"
  (let ((args (list "ZRANGE" key 
                    (princ-to-string start) 
                    (princ-to-string stop))))
    (when withscores
      (push "WITHSCORES" args))
    (apply #'conn:execute-command connection (nreverse args))))

(defun zrevrange (connection key start stop &key withscores)
  "Return a range of members in a sorted set, by index, in reverse order"
  (let ((args (list "ZREVRANGE" key 
                    (princ-to-string start) 
                    (princ-to-string stop))))
    (when withscores
      (push "WITHSCORES" args))
    (apply #'conn:execute-command connection (nreverse args))))

(defun zrangebyscore (connection key min max &key withscores limit)
  "Return a range of members in a sorted set, by score"
  (let ((args (list "ZRANGEBYSCORE" key 
                    (princ-to-string min) 
                    (princ-to-string max))))
    (when withscores
      (push "WITHSCORES" args))
    (when limit
      (push "LIMIT" args)
      (push (princ-to-string (first limit)) args)
      (push (princ-to-string (second limit)) args))
    (apply #'conn:execute-command connection (nreverse args))))

(defun zrank (connection key member)
  "Determine the index of a member in a sorted set"
  (conn:execute-command connection "ZRANK" key member))

(defun zrevrank (connection key member)
  "Determine the index of a member in a sorted set, in reverse order"
  (conn:execute-command connection "ZREVRANK" key member))

(defun zcard (connection key)
  "Get the number of members in a sorted set"
  (conn:execute-command connection "ZCARD" key))

(defun zcount (connection key min max)
  "Count members in a sorted set with scores within the given range"
  (conn:execute-command connection "ZCOUNT" key 
                   (princ-to-string min) 
                   (princ-to-string max)))

(defun zscore (connection key member)
  "Get the score associated with a member in a sorted set"
  (conn:execute-command connection "ZSCORE" key member))

(defun zincrby (connection key increment member)
  "Increment the score of a member in a sorted set"
  (conn:execute-command connection "ZINCRBY" key 
                   (princ-to-string increment) 
                   member))

;;; Hash commands

(defun hset (connection key &rest field-value-pairs)
  "Set fields in a hash"
  (apply #'conn:execute-command connection "HSET" key field-value-pairs))

(defun hget (connection key field)
  "Get the value of a hash field"
  (conn:execute-command connection "HGET" key field))

(defun hdel (connection key &rest fields)
  "Delete hash fields"
  (apply #'conn:execute-command connection "HDEL" key fields))

(defun hexists (connection key field)
  "Determine if a hash field exists"
  (= 1 (conn:execute-command connection "HEXISTS" key field)))

(defun hgetall (connection key)
  "Get all fields and values in a hash"
  (let ((flat-list (conn:execute-command connection "HGETALL" key)))
    ;; Convert flat list to map
    (let ((result (map:make-map)))
      (loop for (field value) on flat-list by #'cddr
            do (setf result (map:assoc result field value)))
      result)))

(defun hkeys (connection key)
  "Get all fields in a hash"
  (conn:execute-command connection "HKEYS" key))

(defun hvals (connection key)
  "Get all values in a hash"
  (conn:execute-command connection "HVALS" key))

(defun hlen (connection key)
  "Get the number of fields in a hash"
  (conn:execute-command connection "HLEN" key))

(defun hmget (connection key &rest fields)
  "Get the values of multiple hash fields"
  (apply #'conn:execute-command connection "HMGET" key fields))

(defun hmset (connection key &rest field-value-pairs)
  "Set multiple hash fields (deprecated, use HSET)"
  (apply #'conn:execute-command connection "HMSET" key field-value-pairs))

(defun hincrby (connection key field increment)
  "Increment the integer value of a hash field"
  (conn:execute-command connection "HINCRBY" key field (princ-to-string increment)))

(defun hincrbyfloat (connection key field increment)
  "Increment the float value of a hash field"
  (conn:execute-command connection "HINCRBYFLOAT" key field 
                   (princ-to-string increment)))

(defun hsetnx (connection key field value)
  "Set the value of a hash field, only if the field does not exist"
  (= 1 (conn:execute-command connection "HSETNX" key field value)))

;;; Transaction commands

(defun multi (connection)
  "Mark the start of a transaction block"
  (conn:execute-command connection "MULTI"))

(defun exec (connection)
  "Execute all commands issued after MULTI"
  (setf (conn:connection-in-transaction-p connection) nil)
  (conn:execute-command connection "EXEC"))

(defun discard (connection)
  "Discard all commands issued after MULTI"
  (setf (conn:connection-in-transaction-p connection) nil)
  (conn:execute-command connection "DISCARD"))

(defun watch (connection &rest keys)
  "Watch keys to determine execution of the MULTI/EXEC block"
  (apply #'conn:execute-command connection "WATCH" keys))

(defun unwatch (connection)
  "Forget about all watched keys"
  (conn:execute-command connection "UNWATCH"))

;;; Pub/Sub commands

(defun publish (connection channel message)
  "Post a message to a channel"
  (conn:execute-command connection "PUBLISH" channel message))

(defun subscribe (connection &rest channels)
  "Subscribe to channels"
  (apply #'conn:execute-command connection "SUBSCRIBE" channels))

(defun unsubscribe (connection &rest channels)
  "Unsubscribe from channels"
  (if channels
      (apply #'conn:execute-command connection "UNSUBSCRIBE" channels)
      (conn:execute-command connection "UNSUBSCRIBE")))

(defun psubscribe (connection &rest patterns)
  "Subscribe to channels matching patterns"
  (apply #'conn:execute-command connection "PSUBSCRIBE" patterns))

(defun punsubscribe (connection &rest patterns)
  "Unsubscribe from patterns"
  (if patterns
      (apply #'conn:execute-command connection "PUNSUBSCRIBE" patterns)
      (conn:execute-command connection "PUNSUBSCRIBE")))

;;; Transaction and pipeline macros

(defmacro with-transaction ((connection) &body body)
  "Execute commands in a transaction"
  `(conn:with-transaction (,connection)
     ,@body))

(defmacro with-pipeline ((connection) &body body)
  "Execute commands in pipeline mode"
  `(conn:with-pipeline (,connection)
     ,@body))

(defun pipeline-execute (connection)
  "Execute all queued pipeline commands"
  (conn:pipeline-execute connection))