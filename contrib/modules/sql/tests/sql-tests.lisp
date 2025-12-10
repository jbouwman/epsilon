;;;; SQL Module Tests
;;;;
;;;; Tests for the common SQL interface and SQLite backend

(defpackage epsilon.sql.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (sql epsilon.sql)
   (sqlite epsilon.sql.sqlite)
   (str epsilon.string)))

(in-package epsilon.sql.tests)

;;; Basic Connection Tests

(deftest test-sqlite-connection
  "Test basic SQLite connection"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (is (sql:connection-p conn))
    (is (eq :sqlite (sql:backend-name (sql:connection-backend conn))))))

(deftest test-sqlite-version
  "Test SQLite version retrieval"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (let ((result (sql:query-one conn "SELECT sqlite_version()")))
      (is (stringp (first result)))
      (is (str:starts-with-p (first result) "3.")))))

;;; Table Creation and Basic Operations

(deftest test-create-table
  "Test table creation"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE users (
                         id INTEGER PRIMARY KEY,
                         name TEXT NOT NULL,
                         email TEXT UNIQUE,
                         age INTEGER)")
    (let ((result (sql:query conn "SELECT name FROM sqlite_master WHERE type='table'")))
      (is (= 1 (length result)))
      (is (string= "users" (first (first result)))))))

(deftest test-insert-and-select
  "Test basic insert and select operations"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)")
    (sql:execute conn "INSERT INTO test (value) VALUES (?)" '("hello"))
    (sql:execute conn "INSERT INTO test (value) VALUES (?)" '("world"))
    
    (let ((results (sql:query conn "SELECT value FROM test ORDER BY id")))
      (is (= 2 (length results)))
      (is (string= "hello" (first (first results))))
      (is (string= "world" (first (second results)))))))

;;; Parameterized Queries

(deftest test-parameterized-queries
  "Test queries with parameters"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE numbers (n INTEGER)")
    
    ;; Insert multiple values
    (loop for i from 1 to 5
          do (sql:execute conn "INSERT INTO numbers VALUES (?)" (list i)))
    
    ;; Query with parameter
    (let ((result (sql:query conn "SELECT n FROM numbers WHERE n > ?" '(3))))
      (is (= 2 (length result)))
      (is (= 4 (first (first result))))
      (is (= 5 (first (second result)))))))

(deftest test-null-handling
  "Test NULL value handling"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE nullable (id INTEGER, value TEXT)")
    (sql:execute conn "INSERT INTO nullable VALUES (1, NULL)")
    (sql:execute conn "INSERT INTO nullable VALUES (2, 'not null')")
    
    (let ((results (sql:query conn "SELECT value FROM nullable ORDER BY id")))
      (is (sql:sql-null-p (first (first results))))
      (is (string= "not null" (first (second results)))))))

;;; Prepared Statements

(deftest test-prepared-statements
  "Test prepared statement usage"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE data (key TEXT, value INTEGER)")
    
    (sql:with-statement (stmt conn "INSERT INTO data VALUES (?, ?)")
      (sql:bind stmt "a" 1)
      (sql:fetch stmt)
      (sql:bind stmt "b" 2)
      (sql:fetch stmt)
      (sql:bind stmt "c" 3)
      (sql:fetch stmt))
    
    (let ((results (sql:query conn "SELECT * FROM data ORDER BY key")))
      (is (= 3 (length results)))
      (is (equal '("a" 1) (first results)))
      (is (equal '("b" 2) (second results)))
      (is (equal '("c" 3) (third results))))))

;;; Transaction Tests

(deftest test-transactions-commit
  "Test transaction commit"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE trans (id INTEGER)")
    
    (sql:with-transaction (conn)
      (sql:execute conn "INSERT INTO trans VALUES (1)")
      (sql:execute conn "INSERT INTO trans VALUES (2)"))
    
    (let ((results (sql:query conn "SELECT COUNT(*) FROM trans")))
      (is (= 2 (first (first results)))))))

(deftest test-transactions-rollback
  "Test transaction rollback on error"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE trans (id INTEGER PRIMARY KEY)")
    (sql:execute conn "INSERT INTO trans VALUES (1)")
    
    (handler-case
        (sql:with-transaction (conn)
          (sql:execute conn "INSERT INTO trans VALUES (2)")
          ;; This should fail due to primary key constraint
          (sql:execute conn "INSERT INTO trans VALUES (1)"))
      (sql:sql-constraint-error () nil))
    
    ;; Should only have the original row
    (let ((results (sql:query conn "SELECT COUNT(*) FROM trans")))
      (is (= 1 (first (first results)))))))

;;; Data Type Tests

(deftest test-data-types
  "Test various data types"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE types (
                         i INTEGER,
                         r REAL,
                         t TEXT,
                         b BLOB)")
    
    (let ((test-blob #(1 2 3 4 5)))
      (sql:execute conn "INSERT INTO types VALUES (?, ?, ?, ?)"
                   (list 42 3.14159d0 "hello" test-blob))
      
      (let ((result (first (sql:query conn "SELECT * FROM types"))))
        (is (= 42 (first result)))
        (is (< (abs (- 3.14159d0 (second result))) 1e-5))
        (is (string= "hello" (third result)))
        (is (equalp test-blob (fourth result)))))))

;;; Error Handling Tests

(deftest test-syntax-error
  "Test SQL syntax error handling"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (is-thrown (sql:sql-error)
      (sql:execute conn "INVALID SQL"))))

(deftest test-constraint-error
  "Test constraint violation error"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE unique_test (id INTEGER PRIMARY KEY)")
    (sql:execute conn "INSERT INTO unique_test VALUES (1)")
    (is-thrown (sql:sql-constraint-error)
      (sql:execute conn "INSERT INTO unique_test VALUES (1)"))))

;;; Utility Function Tests

(deftest test-escape-identifier
  "Test identifier escaping"
  (is (string= "\"table\"" (sql:escape-identifier "table")))
  (is (string= "\"my\"\"table\"" (sql:escape-identifier "my\"table"))))

(deftest test-escape-string
  "Test string escaping"
  (is (string= "'hello'" (sql:escape-string "hello")))
  (is (string= "'it''s'" (sql:escape-string "it's"))))

;;; Performance Test

(deftest test-bulk-insert-performance
  "Test performance of bulk inserts"
  (sql:with-connection (conn :backend :sqlite :database ":memory:")
    (sql:execute conn "CREATE TABLE perf (id INTEGER, value TEXT)")
    
    ;; Use transaction for better performance
    (let ((start-time (get-internal-real-time)))
      (sql:with-transaction (conn)
        (sql:with-statement (stmt conn "INSERT INTO perf VALUES (?, ?)")
          (loop for i from 1 to 1000
                do (progn
                     (sql:bind stmt i (format nil "value-~D" i))
                     (sql:fetch stmt)))))
      
      (let* ((end-time (get-internal-real-time))
             (elapsed (/ (- end-time start-time) internal-time-units-per-second))
             (count (first (first (sql:query conn "SELECT COUNT(*) FROM perf")))))
        (is (= 1000 count))
        ;; Should complete in under 1 second
        (is (< elapsed 1.0))))))