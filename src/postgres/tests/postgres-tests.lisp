;;;; PostgreSQL Client Tests
;;;;
;;;; Comprehensive test suite for the PostgreSQL client implementation

(defpackage :epsilon.postgres.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:conn #:epsilon.postgres.connection)
   (#:query #:epsilon.postgres.query)
   (#:types #:epsilon.postgres.types)
   (#:trans #:epsilon.postgres.transactions)
   (#:env #:epsilon.sys.env)
   (#:map #:epsilon.map)))

(in-package :epsilon.postgres.tests)

;;; Test configuration

(defparameter *test-postgres-host* (or (env:getenv "POSTGRES_HOST") "localhost"))
(defparameter *test-postgres-port* (parse-integer (or (env:getenv "POSTGRES_PORT") "5432")))
(defparameter *test-postgres-database* (or (env:getenv "POSTGRES_DB") "test"))
(defparameter *test-postgres-user* (or (env:getenv "POSTGRES_USER") "test"))
(defparameter *test-postgres-password* (or (env:getenv "POSTGRES_PASSWORD") "test"))

(defmacro with-test-connection ((var) &body body)
  "Execute body with a test PostgreSQL connection"
  `(with-connection (,var (format nil "postgresql://~A:~A@~A:~D/~A"
                                  *test-postgres-user*
                                  *test-postgres-password*
                                  *test-postgres-host*
                                  *test-postgres-port*
                                  *test-postgres-database*))
     ;; Clean up test tables
     (ignore-errors 
       (execute ,var "DROP TABLE IF EXISTS test_users CASCADE"))
     (ignore-errors
       (execute ,var "DROP TABLE IF EXISTS test_products CASCADE"))
     (ignore-errors
       (execute ,var "DROP SEQUENCE IF EXISTS test_seq CASCADE"))
     ,@body))

;;; Connection tests

(deftest test-connection-string-parsing
  "Test PostgreSQL connection string parsing"
  ;; Basic connection
  (multiple-value-bind (host port database user password params)
      (parse-connection-string "postgresql://user:pass@localhost:5432/mydb")
    (is-equal "localhost" host)
    (is-equal 5432 port)
    (is-equal "mydb" database)  
    (is-equal "user" user)
    (is-equal "pass" password))
  
  ;; Without password
  (multiple-value-bind (host port database user password params)
      (parse-connection-string "postgresql://user@localhost/mydb")
    (is-equal "localhost" host)
    (is-equal 5432 port)
    (is-equal "mydb" database)
    (is-equal "user" user)
    (is-equal nil password))
  
  ;; With parameters
  (multiple-value-bind (host port database user password params)
      (parse-connection-string "postgresql://user@localhost/mydb?sslmode=require&connect_timeout=10")
    (is-equal "localhost" host)
    (is-equal "mydb" database)
    (is-equal "require" (map:get params "sslmode"))
    (is-equal "10" (map:get params "connect_timeout"))))

(deftest test-connection-lifecycle
  "Test connection creation and disconnection"
  (let ((conn nil))
    ;; Connect
    (is-no-error
     (setf conn (connect *test-postgres-host*
                         :port *test-postgres-port*
                         :database *test-postgres-database*
                         :user *test-postgres-user*
                         :password *test-postgres-password*)))
    
    ;; Check connected
    (is-true (connection-connected-p conn))
    (is-true (connection-alive-p conn))
    
    ;; Check connection properties
    (is-equal *test-postgres-host* (connection-host conn))
    (is-equal *test-postgres-port* (connection-port conn))
    (is-equal *test-postgres-database* (connection-database conn))
    (is-equal *test-postgres-user* (connection-user conn))
    
    ;; Disconnect
    (disconnect conn)
    (is-false (connection-connected-p conn))))

;;; Query execution tests

(deftest test-simple-queries
  "Test simple query execution"
  (with-test-connection (conn)
    ;; Basic SELECT
    (let ((result (query conn "SELECT 1 as test_column")))
      (is-equal 1 (length (query-result-rows result)))
      (is-equal 1 (length (query-result-columns result)))
      (is-equal "test_column" (column-info-name (first (query-result-columns result))))
      (is-equal '(1) (first (query-result-rows result))))
    
    ;; Multiple columns
    (let ((result (query conn "SELECT 42 as num, 'hello' as text, true as flag")))
      (is-equal 1 (length (query-result-rows result)))
      (is-equal 3 (length (query-result-columns result)))
      (let ((row (first (query-result-rows result))))
        (is-equal 42 (first row))
        (is-equal "hello" (second row))
        (is-equal t (third row))))
    
    ;; NULL values
    (let ((result (query conn "SELECT NULL as null_col, 'not null' as text_col")))
      (let ((row (first (query-result-rows result))))
        (is-equal nil (first row))
        (is-equal "not null" (second row))))))

(deftest test-data-types
  "Test PostgreSQL data type handling"
  (with-test-connection (conn)
    ;; Integer types
    (is-equal 32767 (query-value conn "SELECT 32767::smallint"))
    (is-equal 2147483647 (query-value conn "SELECT 2147483647::integer"))
    (is-equal 9223372036854775807 (query-value conn "SELECT 9223372036854775807::bigint"))
    
    ;; Floating point
    (is-equal 3.14 (query-value conn "SELECT 3.14::real") 0.001)
    (is-equal 2.718281828 (query-value conn "SELECT 2.718281828::double precision") 0.000000001)
    
    ;; Boolean
    (is-equal t (query-value conn "SELECT true"))
    (is-equal nil (query-value conn "SELECT false"))
    
    ;; Text types
    (is-equal "Hello World" (query-value conn "SELECT 'Hello World'::text"))
    (is-equal "Fixed" (query-value conn "SELECT 'Fixed    '::char(8)") "Fixed    ")
    (is-equal "Variable" (query-value conn "SELECT 'Variable'::varchar(50)"))
    
    ;; Arrays
    (let ((int-array (query-value conn "SELECT ARRAY[1,2,3,4]")))
      (is-equal '(1 2 3 4) int-array))
    
    (let ((text-array (query-value conn "SELECT ARRAY['a','b','c']")))
      (is-equal '("a" "b" "c") text-array))))

(deftest test-table-operations
  "Test table creation and manipulation"
  (with-test-connection (conn)
    ;; Create table
    (execute conn "CREATE TABLE test_users (
                     id SERIAL PRIMARY KEY,
                     name VARCHAR(100) NOT NULL,
                     email VARCHAR(255) UNIQUE,
                     age INTEGER,
                     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                   )")
    
    ;; Insert data
    (let ((affected (execute conn "INSERT INTO test_users (name, email, age) 
                                   VALUES ('Alice', 'alice@example.com', 25)")))
      (is-equal 1 affected))
    
    (let ((affected (execute conn "INSERT INTO test_users (name, email, age) 
                                   VALUES ('Bob', 'bob@example.com', 30),
                                          ('Charlie', 'charlie@example.com', 35)")))
      (is-equal 2 affected))
    
    ;; Query data
    (let ((users (query-list conn "SELECT name, age FROM test_users ORDER BY age")))
      (is-equal 3 (length users))
      (is-equal "Alice" (first (first users)))
      (is-equal 25 (second (first users))))
    
    ;; Update data
    (let ((affected (execute conn "UPDATE test_users SET age = age + 1 WHERE name = 'Alice'")))
      (is-equal 1 affected))
    
    (is-equal 26 (query-value conn "SELECT age FROM test_users WHERE name = 'Alice'"))
    
    ;; Delete data
    (let ((affected (execute conn "DELETE FROM test_users WHERE age > 30")))
      (is-equal 2 affected))
    
    (is-equal 1 (query-value conn "SELECT COUNT(*) FROM test_users"))))

;;; Prepared statement tests

(deftest test-prepared-statements
  "Test prepared statement functionality"
  (with-test-connection (conn)
    ;; Create test table
    (execute conn "CREATE TABLE test_products (
                     id SERIAL PRIMARY KEY,
                     name VARCHAR(100),
                     price NUMERIC(10,2),
                     in_stock BOOLEAN
                   )")
    
    ;; Test parameterized queries
    (let ((affected (execute conn "INSERT INTO test_products (name, price, in_stock) 
                                   VALUES ($1, $2, $3)"
                             "Widget" 19.99 t)))
      (is-equal 1 affected))
    
    (let ((affected (execute conn "INSERT INTO test_products (name, price, in_stock) 
                                   VALUES ($1, $2, $3)"
                             "Gadget" 29.99 nil)))
      (is-equal 1 affected))
    
    ;; Query with parameters
    (let ((product (query-one conn "SELECT name, price FROM test_products WHERE price > $1"
                              15.0)))
      (is-equal "Widget" (first product))
      (is-equal "19.99" (second product)))  ; Numeric comes back as string
    
    ;; Multiple parameter types
    (let ((products (query-list conn "SELECT name FROM test_products 
                                      WHERE price BETWEEN $1 AND $2 
                                      AND in_stock = $3"
                                20.0 40.0 nil)))
      (is-equal 1 (length products))
      (is-equal "Gadget" (first (first products))))
    
    ;; Explicit prepared statement
    (let ((stmt-name (prepare conn "get_product" 
                              "SELECT * FROM test_products WHERE id = $1")))
      (let ((result (execute-prepared conn stmt-name 1)))
        (is-equal 1 (length (query-result-rows result))))
      (deallocate conn stmt-name))))

;;; Transaction tests

(deftest test-transactions
  "Test transaction support"
  (with-test-connection (conn)
    ;; Create test table
    (execute conn "CREATE TABLE test_users (
                     id SERIAL PRIMARY KEY,
                     name VARCHAR(100)
                   )")
    
    ;; Test successful transaction
    (with-transaction (conn)
      (execute conn "INSERT INTO test_users (name) VALUES ('User1')")
      (execute conn "INSERT INTO test_users (name) VALUES ('User2')")
      (is-equal 2 (query-value conn "SELECT COUNT(*) FROM test_users")))
    
    ;; Verify commit
    (is-equal 2 (query-value conn "SELECT COUNT(*) FROM test_users"))
    
    ;; Test rollback on error
    (is-thrown 'postgres-query-error
      (with-transaction (conn)
        (execute conn "INSERT INTO test_users (name) VALUES ('User3')")
        (execute conn "INSERT INTO test_users (invalid_column) VALUES ('Invalid')")))
    
    ;; Verify rollback
    (is-equal 2 (query-value conn "SELECT COUNT(*) FROM test_users"))
    
    ;; Test manual rollback
    (begin-transaction conn)
    (execute conn "INSERT INTO test_users (name) VALUES ('TempUser')")
    (is-equal 3 (query-value conn "SELECT COUNT(*) FROM test_users"))
    (rollback conn)
    (is-equal 2 (query-value conn "SELECT COUNT(*) FROM test_users"))))

(deftest test-savepoints
  "Test savepoint functionality"
  (with-test-connection (conn)
    ;; Create test table
    (execute conn "CREATE TABLE test_users (
                     id SERIAL PRIMARY KEY,
                     name VARCHAR(100)
                   )")
    
    (with-transaction (conn)
      (execute conn "INSERT INTO test_users (name) VALUES ('User1')")
      
      (with-savepoint (conn "sp1")
        (execute conn "INSERT INTO test_users (name) VALUES ('User2')")
        (is-equal 2 (query-value conn "SELECT COUNT(*) FROM test_users"))
        
        ;; This will cause rollback to savepoint
        (is-thrown 'postgres-query-error
          (execute conn "INSERT INTO test_users (invalid_column) VALUES ('Invalid')")))
      
      ;; Should be back to 1 user (savepoint rollback)
      (is-equal 1 (query-value conn "SELECT COUNT(*) FROM test_users"))
      
      (execute conn "INSERT INTO test_users (name) VALUES ('User3')"))
    
    ;; Transaction committed with 2 users
    (is-equal 2 (query-value conn "SELECT COUNT(*) FROM test_users"))))

;;; Query result format tests

(deftest test-query-result-formats
  "Test different query result formats"
  (with-test-connection (conn)
    ;; Create test data
    (execute conn "CREATE TABLE test_data (
                     id INTEGER,
                     name VARCHAR(50),
                     value NUMERIC(8,2)
                   )")
    
    (execute conn "INSERT INTO test_data VALUES 
                   (1, 'Alpha', 10.5),
                   (2, 'Beta', 20.75),
                   (3, 'Gamma', 30.25)")
    
    ;; Test query-list (default)
    (let ((rows (query-list conn "SELECT id, name FROM test_data ORDER BY id")))
      (is-equal 3 (length rows))
      (is-equal '(1 "Alpha") (first rows))
      (is-equal '(2 "Beta") (second rows)))
    
    ;; Test query-one
    (let ((row (query-one conn "SELECT name, value FROM test_data WHERE id = 2")))
      (is-equal "Beta" (first row))
      (is-equal "20.75" (second row)))
    
    ;; Test query-alist
    (let ((alist-rows (query-alist conn "SELECT id, name FROM test_data WHERE id <= 2 ORDER BY id")))
      (is-equal 2 (length alist-rows))
      (let ((first-row (first alist-rows)))
        (is-equal 1 (cdr (assoc "id" first-row :test #'string=)))
        (is-equal "Alpha" (cdr (assoc "name" first-row :test #'string=)))))
    
    ;; Test query-plist
    (let ((plist-rows (query-plist conn "SELECT id, name FROM test_data WHERE id = 1")))
      (is-equal 1 (length plist-rows))
      (let ((row (first plist-rows)))
        (is-equal 1 (getf row :id))
        (is-equal "Alpha" (getf row :name))))
    
    ;; Test query-value
    (is-equal 3 (query-value conn "SELECT COUNT(*) FROM test_data"))
    (is-equal "Beta" (query-value conn "SELECT name FROM test_data WHERE id = 2"))))

;;; Error handling tests

(deftest test-error-handling
  "Test error condition handling"
  (with-test-connection (conn)
    ;; Syntax error
    (is-thrown 'postgres-query-error
      (query conn "INVALID SQL SYNTAX"))
    
    ;; Table doesn't exist
    (is-thrown 'postgres-query-error
      (query conn "SELECT * FROM nonexistent_table"))
    
    ;; Type mismatch
    (execute conn "CREATE TABLE test_types (id INTEGER)")
    (is-thrown 'postgres-query-error
      (execute conn "INSERT INTO test_types VALUES ('not a number')"))
    
    ;; Constraint violation
    (execute conn "CREATE TABLE test_unique (
                     id INTEGER UNIQUE,
                     name VARCHAR(50)
                   )")
    (execute conn "INSERT INTO test_unique VALUES (1, 'First')")
    (is-thrown 'postgres-query-error
      (execute conn "INSERT INTO test_unique VALUES (1, 'Duplicate')"))))

;;; Type mapping tests

(deftest test-type-mappings
  "Test custom type mappings"
  (with-test-connection (conn)
    ;; Test built-in type decoders
    (let ((result (query-one conn "SELECT 
                                     42::integer as int_val,
                                     3.14::real as float_val,
                                     'hello'::text as text_val,
                                     true::boolean as bool_val,
                                     NULL as null_val")))
      (is-equal 42 (nth 0 result))
      (is-equal 3.14 (nth 1 result) 0.001)
      (is-equal "hello" (nth 2 result))
      (is-equal t (nth 3 result))
      (is-equal nil (nth 4 result)))
    
    ;; Test array handling
    (let ((int-array (query-value conn "SELECT ARRAY[1,2,3,NULL,5]")))
      (is-equal '(1 2 3 nil 5) int-array))
    
    ;; Test JSON (returned as string by default)
    (let ((json-val (query-value conn "SELECT '{\"key\": \"value\"}'::json")))
      (is-equal "{\"key\": \"value\"}" json-val))))

;;; Utility function tests

(deftest test-utility-functions
  "Test utility functions"
  ;; Test string escaping
  (is-equal "don''t" (escape-string "don't"))
  (is-equal "''" (escape-string "'"))
  (is-equal "normal" (escape-string "normal"))
  
  ;; Test identifier escaping
  (is-equal "\"table\"" (escape-identifier "table"))
  (is-equal "\"user\"\"name\"" (escape-identifier "user\"name"))
  (is-equal "\"normal_name\"" (escape-identifier "normal_name"))
  
  ;; Test query formatting
  (is-equal "SELECT * FROM users WHERE name = 'Alice' AND age > 25"
            (format-query "SELECT * FROM ~A WHERE name = ~A AND age > ~A"
                          'users "Alice" 25)))

;;; Integration tests

(deftest test-integration-scenario
  "Test realistic usage scenario"
  (with-test-connection (conn)
    ;; Create schema
    (execute conn "CREATE TABLE users (
                     id SERIAL PRIMARY KEY,
                     username VARCHAR(50) UNIQUE NOT NULL,
                     email VARCHAR(100) UNIQUE NOT NULL,
                     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                   )")
    
    (execute conn "CREATE TABLE posts (
                     id SERIAL PRIMARY KEY,
                     user_id INTEGER REFERENCES users(id),
                     title VARCHAR(200) NOT NULL,
                     content TEXT,
                     published BOOLEAN DEFAULT false,
                     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                   )")
    
    ;; Insert test data with transaction
    (with-transaction (conn)
      (let ((user-id (query-value conn 
                                  "INSERT INTO users (username, email) 
                                   VALUES ($1, $2) RETURNING id"
                                  "testuser" "test@example.com")))
        (execute conn "INSERT INTO posts (user_id, title, content, published)
                       VALUES ($1, $2, $3, $4)"
                 user-id "First Post" "This is the content" t)
        
        (execute conn "INSERT INTO posts (user_id, title, content, published)
                       VALUES ($1, $2, $3, $4)"
                 user-id "Draft Post" "Draft content" nil)))
    
    ;; Query with joins
    (let ((posts (query-alist conn 
                              "SELECT u.username, p.title, p.published
                               FROM users u 
                               JOIN posts p ON u.id = p.user_id
                               WHERE u.username = $1
                               ORDER BY p.created_at"
                              "testuser")))
      (is-equal 2 (length posts))
      (is-equal "testuser" (cdr (assoc "username" (first posts) :test #'string=)))
      (is-equal t (cdr (assoc "published" (first posts) :test #'string=)))
      (is-equal nil (cdr (assoc "published" (second posts) :test #'string=))))
    
    ;; Update with conditions
    (let ((affected (execute conn "UPDATE posts SET published = true 
                                   WHERE user_id IN (
                                     SELECT id FROM users WHERE username = $1
                                   )"
                             "testuser")))
      (is-equal 2 affected))
    
    ;; Verify update
    (is-equal 2 (query-value conn "SELECT COUNT(*) FROM posts WHERE published = true"))))

;;; Run all tests

(defun run-postgres-tests ()
  "Run all PostgreSQL client tests"
  (run-package-tests :epsilon.postgres.tests))