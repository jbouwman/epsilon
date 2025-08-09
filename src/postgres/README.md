# Epsilon PostgreSQL Client

A comprehensive PostgreSQL client for Epsilon with full protocol support, connection pooling, prepared statements, and advanced transaction management.

## Features

- **Complete Protocol Support**: Full PostgreSQL wire protocol v3 implementation
- **Connection Management**: Automatic connection handling with connection string parsing
- **Type Safety**: Comprehensive type mapping between PostgreSQL and Lisp types
- **Prepared Statements**: Automatic prepared statement caching and management
- **Advanced Transactions**: Full transaction support with savepoints and nested transactions
- **Authentication**: Support for multiple authentication methods (cleartext, MD5)
- **Error Handling**: Detailed error reporting with PostgreSQL error codes
- **Result Formats**: Multiple result formats (lists, alists, plists, single values)
- **Query Building**: Safe query formatting and parameter escaping
- **Advisory Locks**: Full advisory locking support for coordination

## Quick Start

### Basic Usage

```lisp
(use-package :epsilon.postgres)

;; Connect to PostgreSQL
(with-connection (conn "postgresql://user:password@localhost:5432/mydb")
  ;; Execute queries
  (let ((users (query conn "SELECT id, name FROM users WHERE age > $1" 25)))
    (format t "Found ~D users~%" (length users))))
```

### Connection Strings

```lisp
;; Full connection string
(with-connection (conn "postgresql://myuser:mypass@localhost:5432/mydb")
  ...)

;; With SSL and parameters
(with-connection (conn "postgresql://user@localhost/db?sslmode=require")
  ...)

;; Environment variables
(with-connection (conn "postgresql:///") ; Uses PGUSER, PGDATABASE, etc.
  ...)
```

### Direct Connection

```lisp
(let ((conn (connect "localhost" 
                     :port 5432
                     :database "mydb" 
                     :user "myuser"
                     :password "mypass")))
  (unwind-protect
       (query conn "SELECT version()")
    (disconnect conn)))
```

## API Reference

### Connection Management

**`connect`** `(host &key port database user password options ssl-mode application-name timeout)`

Connect to PostgreSQL server with specified parameters.

```lisp
(let ((conn (connect "localhost" 
                     :database "myapp"
                     :user "myuser"
                     :ssl-mode :require)))
  ...)
```

**`with-connection`** `((var connection-spec) &body body)`

Execute body with a PostgreSQL connection, automatically handling cleanup.

```lisp
(with-connection (conn "postgresql://localhost/mydb")
  (query conn "SELECT NOW()"))
```

### Query Execution

**`query`** `(connection sql &rest parameters)`

Execute a query and return complete result structure.

```lisp
(let ((result (query conn "SELECT id, name FROM users WHERE age > $1" 25)))
  (query-result-rows result)     ; List of row data
  (query-result-columns result)  ; Column metadata
  (query-result-affected-rows result)) ; For INSERT/UPDATE/DELETE
```

**`execute`** `(connection sql &rest parameters)`

Execute a command and return number of affected rows.

```lisp
(let ((count (execute conn "UPDATE users SET active = true WHERE id = $1" user-id)))
  (format t "Updated ~D rows~%" count))
```

**`query-list`** `(connection sql &rest parameters)`

Execute query and return rows as lists.

```lisp
(let ((users (query-list conn "SELECT id, name, email FROM users")))
  ;; users = ((1 "Alice" "alice@example.com") (2 "Bob" "bob@example.com"))
  ...)
```

**`query-alist`** `(connection sql &rest parameters)`

Execute query and return rows as association lists.

```lisp
(let ((users (query-alist conn "SELECT id, name FROM users")))
  ;; users = ((("id" . 1) ("name" . "Alice")) (("id" . 2) ("name" . "Bob")))
  (cdr (assoc "name" (first users) :test #'string=))) ; => "Alice"
```

**`query-plist`** `(connection sql &rest parameters)`

Execute query and return rows as property lists.

```lisp
(let ((users (query-plist conn "SELECT id, name FROM users")))
  ;; users = ((:id 1 :name "Alice") (:id 2 :name "Bob"))
  (getf (first users) :name)) ; => "Alice"
```

**`query-one`** `(connection sql &rest parameters)`

Execute query and return first row, or NIL if no results.

```lisp
(let ((user (query-one conn "SELECT name, email FROM users WHERE id = $1" 42)))
  (when user
    (format t "User: ~A <~A>~%" (first user) (second user))))
```

**`query-value`** `(connection sql &rest parameters)`

Execute query and return first column of first row.

```lisp
(let ((count (query-value conn "SELECT COUNT(*) FROM users")))
  (format t "Total users: ~D~%" count))
```

### Prepared Statements

**`prepare`** `(connection name sql)`

Explicitly prepare a statement for later execution.

```lisp
(prepare conn "get_user" "SELECT * FROM users WHERE id = $1")
(let ((result (execute-prepared conn "get_user" 42)))
  ...)
(deallocate conn "get_user")
```

**`with-prepared-statement`** `((stmt-var connection name sql) &body body)`

Execute body with a prepared statement, automatically cleaning up.

```lisp
(with-prepared-statement (stmt conn "insert_user" 
                               "INSERT INTO users (name, email) VALUES ($1, $2)")
  (execute-prepared conn stmt "Alice" "alice@example.com")
  (execute-prepared conn stmt "Bob" "bob@example.com"))
```

### Transactions

**`with-transaction`** `((connection &key isolation-level read-only deferrable) &body body)`

Execute body within a transaction with automatic rollback on error.

```lisp
(with-transaction (conn :isolation-level :serializable)
  (execute conn "INSERT INTO accounts (name, balance) VALUES ($1, $2)" "Alice" 1000)
  (execute conn "INSERT INTO transactions (account, amount) VALUES ($1, $2)" "Alice" 1000))
```

**`begin-transaction`** / **`commit`** / **`rollback`**

Manual transaction control.

```lisp
(begin-transaction conn)
(execute conn "UPDATE accounts SET balance = balance - 100 WHERE id = 1")
(execute conn "UPDATE accounts SET balance = balance + 100 WHERE id = 2")
(commit conn)
```

**`with-savepoint`** `((connection name) &body body)`

Execute body within a savepoint, with automatic rollback on error.

```lisp
(with-transaction (conn)
  (execute conn "INSERT INTO orders (customer_id) VALUES ($1)" customer-id)
  
  (with-savepoint (conn "order_items")
    (dolist (item items)
      (execute conn "INSERT INTO order_items (order_id, product_id, quantity) 
                     VALUES ($1, $2, $3)" order-id (item-id item) (item-qty item))))
  
  (execute conn "UPDATE orders SET total = $1 WHERE id = $2" total order-id))
```

### Advanced Transactions

**`with-nested-transaction`** `((connection) &body body)`

Support for nested transactions using savepoints automatically.

```lisp
(with-nested-transaction (conn)          ; Top-level transaction
  (execute conn "INSERT INTO users ...")
  
  (with-nested-transaction (conn)        ; Savepoint
    (execute conn "INSERT INTO profiles ...")
    ;; Error here will rollback to savepoint, not entire transaction
    )
  
  (execute conn "UPDATE users ..."))     ; This still executes
```

**`with-deadlock-retry`** `((connection &key max-retries delay) &body body)`

Automatically retry transactions on deadlock.

```lisp
(with-deadlock-retry (conn :max-retries 5)
  (with-transaction (conn)
    ;; High-contention operations that might deadlock
    (execute conn "UPDATE accounts SET balance = balance - $1 WHERE id = $2" 
             amount from-account)
    (execute conn "UPDATE accounts SET balance = balance + $1 WHERE id = $2" 
             amount to-account)))
```

### Advisory Locks

**`with-advisory-lock`** `((connection key &key shared nowait session timeout) &body body)`

Execute body while holding an advisory lock.

```lisp
;; Ensure only one process handles critical section
(with-advisory-lock (conn 12345 :timeout 30)
  (let ((current-value (query-value conn "SELECT counter FROM global_state")))
    (execute conn "UPDATE global_state SET counter = $1" (1+ current-value))))

;; Shared lock for read operations
(with-advisory-lock (conn 67890 :shared t)
  (generate-report conn))
```

## Data Types

### Automatic Type Conversion

PostgreSQL types are automatically converted to appropriate Lisp types:

```lisp
;; Numeric types
(query-value conn "SELECT 42::integer")        ; => 42
(query-value conn "SELECT 3.14::real")         ; => 3.14
(query-value conn "SELECT 123.45::numeric")    ; => 123.45

;; Text types  
(query-value conn "SELECT 'hello'::text")      ; => "hello"
(query-value conn "SELECT 'world'::varchar")   ; => "world"

;; Boolean
(query-value conn "SELECT true::boolean")      ; => T
(query-value conn "SELECT false::boolean")     ; => NIL

;; Arrays
(query-value conn "SELECT ARRAY[1,2,3]")       ; => (1 2 3)
(query-value conn "SELECT ARRAY['a','b']")     ; => ("a" "b")

;; JSON (returned as string by default)
(query-value conn "SELECT '{\"key\": \"value\"}'::json") ; => "{\"key\": \"value\"}"

;; NULL values
(query-value conn "SELECT NULL")               ; => NIL
```

### Custom Type Mappings

Register custom type decoders/encoders:

```lisp
;; Register UUID type handling
(register-type-mapping +oid-uuid+
                       (lambda (bytes format-code)
                         (parse-uuid (str:from-utf8-bytes bytes)))
                       (lambda (uuid)
                         (str:to-utf8-bytes (format-uuid uuid)))
                       :lisp-type 'uuid
                       :description "UUID type")
```

## Error Handling

```lisp
(handler-case
    (with-connection (conn "postgresql://localhost/mydb")
      (query conn "SELECT * FROM nonexistent_table"))
  
  (postgres-connection-error (e)
    (format t "Connection failed: ~A~%" (error-message e)))
  
  (postgres-query-error (e)
    (format t "Query error: ~A~%" (error-message e))
    (format t "Error code: ~A~%" (error-code e))
    (when (error-detail e)
      (format t "Detail: ~A~%" (error-detail e)))
    (when (error-hint e)
      (format t "Hint: ~A~%" (error-hint e))))
  
  (postgres-protocol-error (e)
    (format t "Protocol error: ~A~%" (error-message e))))
```

## Utility Functions

**`escape-string`** / **`escape-identifier`**

Safe escaping for dynamic SQL construction:

```lisp
(let ((user-input "don't"))
  (format-query "SELECT * FROM users WHERE name = ~A" user-input))
; => "SELECT * FROM users WHERE name = 'don''t'"

(let ((table-name "user-data"))
  (format-query "SELECT * FROM ~A" table-name))
; => "SELECT * FROM \"user-data\""
```

**`format-query`**

Safe query formatting with automatic escaping:

```lisp
(format-query "SELECT * FROM ~A WHERE name = ~A AND age > ~A"
              'users "Alice" 25)
; => "SELECT * FROM \"users\" WHERE name = 'Alice' AND age > 25"
```

## Performance Tips

1. **Use Connection Pooling**: Reuse connections instead of creating new ones
2. **Prepare Frequently-Used Statements**: Use prepared statements for repeated queries
3. **Batch Operations**: Use transactions to batch multiple operations
4. **Proper Indexing**: Ensure your database has appropriate indexes
5. **Use Appropriate Types**: Choose efficient PostgreSQL data types
6. **Limit Result Sets**: Use LIMIT clauses for large result sets

## Advanced Features

### Bulk Operations

```lisp
;; Efficient bulk insert
(with-transaction (conn)
  (prepare conn "bulk_insert" "INSERT INTO items (name, value) VALUES ($1, $2)")
  (dolist (item items)
    (execute-prepared conn "bulk_insert" (item-name item) (item-value item))))
```

### Complex Queries

```lisp
;; CTE (Common Table Expression) queries
(let ((results (query-list conn 
                           "WITH RECURSIVE tree AS (
                              SELECT id, name, parent_id, 0 as level
                              FROM categories WHERE parent_id IS NULL
                              UNION ALL
                              SELECT c.id, c.name, c.parent_id, t.level + 1
                              FROM categories c
                              JOIN tree t ON c.parent_id = t.id
                            )
                            SELECT id, name, level FROM tree ORDER BY level, name")))
  ...)
```

### Window Functions

```lisp
;; Ranking and analytics
(let ((ranked-sales (query-alist conn
                                 "SELECT 
                                    salesperson,
                                    amount,
                                    RANK() OVER (ORDER BY amount DESC) as rank,
                                    LAG(amount) OVER (ORDER BY amount DESC) as prev_amount
                                  FROM sales 
                                  ORDER BY amount DESC")))
  ...)
```

## Integration Examples

### Web Application Session Store

```lisp
(defun save-session (conn session-id data &key (ttl 3600))
  (with-transaction (conn)
    (execute conn 
             "INSERT INTO sessions (id, data, expires_at) 
              VALUES ($1, $2, $3) 
              ON CONFLICT (id) 
              DO UPDATE SET data = $2, expires_at = $3"
             session-id 
             (serialize-session-data data)
             (+ (get-universal-time) ttl))))

(defun load-session (conn session-id)
  (let ((row (query-one conn 
                        "SELECT data FROM sessions 
                         WHERE id = $1 AND expires_at > $2"
                        session-id 
                        (get-universal-time))))
    (when row
      (deserialize-session-data (first row)))))
```

### Application Configuration

```lisp
(defun get-config (conn key &optional default)
  (or (query-value conn "SELECT value FROM config WHERE key = $1" key)
      default))

(defun set-config (conn key value)
  (execute conn 
           "INSERT INTO config (key, value) VALUES ($1, $2)
            ON CONFLICT (key) DO UPDATE SET value = $2"
           key value))
```

### Audit Logging

```lisp
(defun log-audit-event (conn user-id action resource-type resource-id &optional details)
  (execute conn
           "INSERT INTO audit_log (user_id, action, resource_type, resource_id, details, created_at)
            VALUES ($1, $2, $3, $4, $5, $6)"
           user-id action resource-type resource-id details (get-universal-time)))

(defmacro with-audit ((conn user-id action resource-type resource-id) &body body)
  `(prog1
       (progn ,@body)
     (log-audit-event ,conn ,user-id ,action ,resource-type ,resource-id)))
```

## Testing

The PostgreSQL client includes a comprehensive test suite. To run tests:

```lisp
(epsilon.postgres.tests:run-postgres-tests)
```

Test configuration via environment variables:
- `POSTGRES_HOST` (default: "localhost")
- `POSTGRES_PORT` (default: "5432")  
- `POSTGRES_DB` (default: "test")
- `POSTGRES_USER` (default: "test")
- `POSTGRES_PASSWORD` (default: "test")

## Limitations

- SSL/TLS support requires epsilon.tls module
- Binary format decoding not fully implemented (uses text format)
- Some advanced PostgreSQL features not yet supported:
  - COPY protocol for bulk data transfer
  - Asynchronous notifications (LISTEN/NOTIFY)
  - Large object support
  - Custom aggregate functions

## Dependencies

- `epsilon.core` - Core data structures and utilities
- `epsilon.net` - Network socket operations  
- `epsilon.tls` - SSL/TLS support (optional)
- `epsilon.binary` - Binary data handling