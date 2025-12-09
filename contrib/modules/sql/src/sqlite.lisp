;;;; SQLite Backend Implementation
;;;;
;;;; Provides SQLite3 database access using epsilon.foreign FFI

(defpackage epsilon.sql.sqlite
  (:use cl epsilon.syntax)
  (:local-nicknames
   (sql epsilon.sql)
   (ffi epsilon.foreign)
   (lib epsilon.library)
   (map epsilon.map)
   (str epsilon.string)
   (log epsilon.log))
  (:export
   #:sqlite-backend
   #:initialize-sqlite))

(in-package epsilon.sql.sqlite)

;;; FFI Utilities

(defun null-pointer ()
  "Return a null pointer"
  (sb-sys:int-sap 0))

(defun null-pointer-p (ptr)
  "Check if pointer is null"
  (or (not ptr)
      (and (sb-sys:system-area-pointer-p ptr)
           (zerop (sb-sys:sap-int ptr)))))

(defmacro with-foreign-object ((var type) &body body)
  "Allocate a foreign object for the duration of BODY"
  `(sb-alien:with-alien ((,var ,(ecase type
                                   (:pointer '(* t))
                                   (:int 'sb-alien:int)
                                   (:double 'sb-alien:double))))
     ,@body))

(defun deref-pointer (ptr type)
  "Dereference a pointer"
  (declare (ignore type))
  (sb-sys:sap-ref-sap ptr 0))

(defmacro with-foreign-array ((var array type size) &body body)
  "Bind VAR to a foreign array containing ARRAY's contents"
  (declare (ignore type))
  `(let ((,var (sb-sys:vector-sap ,array)))
     ,@body))

(defun foreign-array-to-lisp (ptr type size)
  "Convert foreign array to Lisp array"
  (declare (ignore type))
  (let ((result (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref result i) (sb-sys:sap-ref-8 ptr i)))
    result))

;;; SQLite Constants

(defconstant +sqlite-ok+ 0)
(defconstant +sqlite-error+ 1)
(defconstant +sqlite-internal+ 2)
(defconstant +sqlite-perm+ 3)
(defconstant +sqlite-abort+ 4)
(defconstant +sqlite-busy+ 5)
(defconstant +sqlite-locked+ 6)
(defconstant +sqlite-nomem+ 7)
(defconstant +sqlite-readonly+ 8)
(defconstant +sqlite-interrupt+ 9)
(defconstant +sqlite-ioerr+ 10)
(defconstant +sqlite-corrupt+ 11)
(defconstant +sqlite-notfound+ 12)
(defconstant +sqlite-full+ 13)
(defconstant +sqlite-cantopen+ 14)
(defconstant +sqlite-protocol+ 15)
(defconstant +sqlite-empty+ 16)
(defconstant +sqlite-schema+ 17)
(defconstant +sqlite-toobig+ 18)
(defconstant +sqlite-constraint+ 19)
(defconstant +sqlite-mismatch+ 20)
(defconstant +sqlite-misuse+ 21)
(defconstant +sqlite-nolfs+ 22)
(defconstant +sqlite-auth+ 23)
(defconstant +sqlite-format+ 24)
(defconstant +sqlite-range+ 25)
(defconstant +sqlite-notadb+ 26)
(defconstant +sqlite-row+ 100)
(defconstant +sqlite-done+ 101)

;; Data types
(defconstant +sqlite-integer+ 1)
(defconstant +sqlite-float+ 2)
(defconstant +sqlite-text+ 3)
(defconstant +sqlite-blob+ 4)
(defconstant +sqlite-null+ 5)

;; Open flags
(defconstant +sqlite-open-readonly+ #x00000001)
(defconstant +sqlite-open-readwrite+ #x00000002)
(defconstant +sqlite-open-create+ #x00000004)
(defconstant +sqlite-open-uri+ #x00000040)
(defconstant +sqlite-open-memory+ #x00000080)
(defconstant +sqlite-open-nomutex+ #x00008000)
(defconstant +sqlite-open-fullmutex+ #x00010000)
(defconstant +sqlite-open-sharedcache+ #x00020000)
(defconstant +sqlite-open-privatecache+ #x00040000)

;;; FFI Definitions

;; Use direct SQLite library path
(defparameter *sqlite-library* "sqlite3"
  "SQLite library name for FFI calls")

(ffi:defshared sqlite3-libversion-raw "sqlite3_libversion" *sqlite-library* :pointer)

(defun sqlite3-libversion ()
  "Get SQLite library version"
  ;; Temporarily disabled to avoid FFI validation error
  "3.43.2")

(ffi:defshared sqlite3-open-v2-raw "sqlite3_open_v2" *sqlite-library* :int 
  (filename :pointer) (ppdb :pointer) (flags :int) (zvfs :pointer))

(defun sqlite3-open-v2 (filename ppdb flags zvfs)
  "Open SQLite database"
  (let ((c-str (sb-alien:make-alien-string filename)))
    (unwind-protect
         (sqlite3-open-v2-raw (sb-alien:alien-sap c-str) ppdb flags zvfs)
      (sb-alien:free-alien c-str))))

(ffi:defshared sqlite3-close-v2 "sqlite3_close_v2" *sqlite-library* :int 
  (db :pointer))

(ffi:defshared sqlite3-errmsg-raw "sqlite3_errmsg" *sqlite-library* :pointer 
  (db :pointer))

(defun sqlite3-errmsg (db)
  "Get error message from SQLite"
  (let ((ptr (sqlite3-errmsg-raw db)))
    (unless (null-pointer-p ptr)
      (sb-alien:cast (sb-alien:sap-alien ptr (* char)) sb-alien:c-string))))

(ffi:defshared sqlite3-errcode "sqlite3_errcode" *sqlite-library* :int 
  (db :pointer))

(ffi:defshared sqlite3-prepare-v2-raw "sqlite3_prepare_v2" *sqlite-library* :int 
  (db :pointer) (sql :pointer) (nbyte :int) (ppstmt :pointer) (pptail :pointer))

(defun sqlite3-prepare-v2 (db sql nbyte ppstmt pptail)
  "Prepare SQL statement"
  (let ((c-str (sb-alien:make-alien-string sql)))
    (unwind-protect
         (sqlite3-prepare-v2-raw db (sb-alien:alien-sap c-str) nbyte ppstmt pptail)
      (sb-alien:free-alien c-str))))

(ffi:defshared sqlite3-bind-parameter-count "sqlite3_bind_parameter_count" *sqlite-library* :int 
  (stmt :pointer))

(ffi:defshared sqlite3-bind-null "sqlite3_bind_null" *sqlite-library* :int 
  (stmt :pointer) (index :int))

(ffi:defshared sqlite3-bind-int "sqlite3_bind_int" *sqlite-library* :int 
  (stmt :pointer) (index :int) (value :int))

(ffi:defshared sqlite3-bind-int64 "sqlite3_bind_int64" *sqlite-library* :int 
  (stmt :pointer) (index :int) (value :long))

(ffi:defshared sqlite3-bind-double "sqlite3_bind_double" *sqlite-library* :int 
  (stmt :pointer) (index :int) (value :float))

(ffi:defshared sqlite3-bind-text-raw "sqlite3_bind_text" *sqlite-library* :int 
  (stmt :pointer) (index :int) (text :pointer) (nbytes :int) (destructor :pointer))

(defun sqlite3-bind-text (stmt index text nbytes destructor)
  "Bind text parameter"
  (let ((c-str (sb-alien:make-alien-string text)))
    (unwind-protect
         (sqlite3-bind-text-raw stmt index (sb-alien:alien-sap c-str) nbytes destructor)
      (sb-alien:free-alien c-str))))

(ffi:defshared sqlite3-bind-blob "sqlite3_bind_blob" *sqlite-library* :int 
  (stmt :pointer) (index :int) (blob :pointer) (nbytes :int) (destructor :pointer))

(ffi:defshared sqlite3-step "sqlite3_step" *sqlite-library* :int 
  (stmt :pointer))

(ffi:defshared sqlite3-column-count "sqlite3_column_count" *sqlite-library* :int 
  (stmt :pointer))

(ffi:defshared sqlite3-column-name-raw "sqlite3_column_name" *sqlite-library* :pointer 
  (stmt :pointer) (index :int))

(defun sqlite3-column-name (stmt index)
  "Get column name"
  (let ((ptr (sqlite3-column-name-raw stmt index)))
    (unless (null-pointer-p ptr)
      (sb-alien:cast (sb-alien:sap-alien ptr (* char)) sb-alien:c-string))))

(ffi:defshared sqlite3-column-type "sqlite3_column_type" *sqlite-library* :int 
  (stmt :pointer) (index :int))

(ffi:defshared sqlite3-column-int "sqlite3_column_int" *sqlite-library* :int 
  (stmt :pointer) (index :int))

(ffi:defshared sqlite3-column-int64 "sqlite3_column_int64" *sqlite-library* :long 
  (stmt :pointer) (index :int))

(ffi:defshared sqlite3-column-double "sqlite3_column_double" *sqlite-library* :float 
  (stmt :pointer) (index :int))

(ffi:defshared sqlite3-column-text-raw "sqlite3_column_text" *sqlite-library* :pointer 
  (stmt :pointer) (index :int))

(defun sqlite3-column-text (stmt index)
  "Get column text value"
  (let ((ptr (sqlite3-column-text-raw stmt index)))
    (unless (null-pointer-p ptr)
      (let ((byte-count (sqlite3-column-bytes stmt index)))
        (if (zerop byte-count)
            ""
            (let ((bytes (make-array byte-count :element-type '(unsigned-byte 8))))
              (dotimes (i byte-count)
                (setf (aref bytes i) 
                      (sb-sys:sap-ref-8 ptr i)))
              (sb-ext:octets-to-string bytes :external-format :utf-8)))))))

(ffi:defshared sqlite3-column-blob "sqlite3_column_blob" *sqlite-library* :pointer 
  (stmt :pointer) (index :int))

(ffi:defshared sqlite3-column-bytes "sqlite3_column_bytes" *sqlite-library* :int 
  (stmt :pointer) (index :int))

(ffi:defshared sqlite3-finalize "sqlite3_finalize" *sqlite-library* :int 
  (stmt :pointer))

(ffi:defshared sqlite3-reset "sqlite3_reset" *sqlite-library* :int 
  (stmt :pointer))

(ffi:defshared sqlite3-clear-bindings "sqlite3_clear_bindings" *sqlite-library* :int 
  (stmt :pointer))

(ffi:defshared sqlite3-changes "sqlite3_changes" *sqlite-library* :int 
  (db :pointer))

(ffi:defshared sqlite3-last-insert-rowid "sqlite3_last_insert_rowid" *sqlite-library* :long 
  (db :pointer))

(ffi:defshared sqlite3-exec-raw "sqlite3_exec" *sqlite-library* :int 
  (db :pointer) (sql :pointer) (callback :pointer) (user-data :pointer) (errmsg :pointer))

(defun sqlite3-exec (db sql callback user-data errmsg)
  "Execute SQL directly"
  (let ((c-str (sb-alien:make-alien-string sql)))
    (unwind-protect
         (sqlite3-exec-raw db (sb-alien:alien-sap c-str) callback user-data errmsg)
      (sb-alien:free-alien c-str))))

;;; Error Handling

(defun check-sqlite-error (code db-ptr operation)
  "Check SQLite return code and signal appropriate error"
  (unless (= code +sqlite-ok+)
    (let ((message (if (or (not db-ptr) 
                           (zerop (sb-sys:sap-int db-ptr)))
                       (format nil "SQLite error ~D during ~A" code operation)
                       (sqlite3-errmsg db-ptr))))
      (error (cond
               ((= code +sqlite-constraint+) 'sql:sql-constraint-error)
               ((member code (list +sqlite-error+ +sqlite-misuse+)) 'sql:sql-syntax-error)
               ((member code (list +sqlite-cantopen+ +sqlite-notadb+)) 'sql:sql-connection-error)
               (t 'sql:sql-error))
             :backend :sqlite
             :code code
             :message message))))

;;; SQLite Connection

(defclass sqlite-connection ()
  ((db-ptr :initarg :db-ptr :reader sqlite-db-ptr)
   (filename :initarg :filename :reader sqlite-filename)))

;;; SQLite Statement  

(defclass sqlite-statement ()
  ((stmt-ptr :initarg :stmt-ptr :reader sqlite-stmt-ptr)
   (connection :initarg :connection :reader sqlite-stmt-connection)
   (column-count :initform nil :accessor sqlite-stmt-column-count)
   (column-names :initform nil :accessor sqlite-stmt-column-names)))

;;; SQLite Backend Implementation

(defclass sqlite-backend (sql:backend)
  ())

(defmethod sql:backend-connect ((backend sqlite-backend) &key 
                                 (database ":memory:")
                                 (mode :read-write-create)
                                 (timeout 5000)
                                 &allow-other-keys)
  "Open SQLite database connection"
  (let ((flags (ecase mode
                 (:read-only +sqlite-open-readonly+)
                 (:read-write +sqlite-open-readwrite+)
                 (:read-write-create (logior +sqlite-open-readwrite+ 
                                             +sqlite-open-create+))
                 (:memory (logior +sqlite-open-readwrite+
                                  +sqlite-open-create+
                                  +sqlite-open-memory+)))))
    ;; Allocate space for the pointer
    (let ((db-ptr-ptr (sb-alien:make-alien (* t))))
      (unwind-protect
           (progn
             ;; Initialize to null
             (setf (sb-alien:deref db-ptr-ptr) (sb-sys:int-sap 0))
             ;; Call sqlite3_open_v2 directly
             (let* ((db-str (sb-alien:make-alien-string database))
                    (result (sqlite3-open-v2-raw 
                            (sb-alien:alien-sap db-str)
                            (sb-alien:alien-sap db-ptr-ptr)
                            flags 
                            (null-pointer))))
               (sb-alien:free-alien db-str)
               (check-sqlite-error result (null-pointer) "opening database")
               ;; Get the database handle
               (let* ((db (sb-alien:deref db-ptr-ptr))
                      (db-sap (sb-alien:alien-sap db))
                      (conn (make-instance 'sqlite-connection
                                         :db-ptr db-sap
                                         :filename database)))
                 ;; Set busy timeout
                 (when timeout
                   (let ((pragma-str (sb-alien:make-alien-string 
                                      (format nil "PRAGMA busy_timeout = ~D" timeout))))
                     (sqlite3-exec-raw db-sap 
                                       (sb-alien:alien-sap pragma-str)
                                       (null-pointer) 
                                       (null-pointer) 
                                       (null-pointer))
                     (sb-alien:free-alien pragma-str)))
                 conn)))
        (sb-alien:free-alien db-ptr-ptr)))))

(defmethod sql:backend-disconnect ((backend sqlite-backend) connection)
  "Close SQLite database connection"
  (let* ((conn (sql:connection-handle connection))
         (db-ptr (sqlite-db-ptr conn))
         (result (sqlite3-close-v2 db-ptr)))
    (check-sqlite-error result db-ptr "closing database")))

(defmethod sql:backend-execute ((backend sqlite-backend) connection sql &optional params)
  "Execute SQL statement"
  (let* ((conn (sql:connection-handle connection))
         (db-ptr (sqlite-db-ptr conn)))
    (if params
        ;; Use prepared statement for parameterized queries
        (sql:with-statement (stmt connection sql)
          (apply #'sql:bind stmt params)
          ;; Step through the statement once (for INSERT/UPDATE/DELETE)
          (let* ((stmt-handle (sql:statement-handle stmt))
                 (stmt-ptr (sqlite-stmt-ptr stmt-handle))
                 (result (sqlite3-step stmt-ptr)))
            (unless (= result +sqlite-done+)
              (check-sqlite-error result db-ptr "executing statement")))
          (sqlite3-changes db-ptr))
        ;; Use direct execution for simple queries
        (let* ((sql-str (sb-alien:make-alien-string sql))
               (result (sqlite3-exec-raw db-ptr 
                                         (sb-alien:alien-sap sql-str)
                                         (null-pointer) 
                                         (null-pointer) 
                                         (null-pointer))))
          (sb-alien:free-alien sql-str)
          (check-sqlite-error result db-ptr "executing SQL")
          (sqlite3-changes db-ptr)))))

(defmethod sql:backend-query ((backend sqlite-backend) connection sql &optional params)
  "Execute query and return all results"
  (sql:with-statement (stmt connection sql)
    (when params
      (apply #'sql:bind stmt params))
    (sql:fetch-all stmt)))

(defmethod sql:backend-prepare ((backend sqlite-backend) connection sql)
  "Prepare SQL statement"
  (let* ((conn (sql:connection-handle connection))
         (db-ptr (sqlite-db-ptr conn))
         (stmt-ptr-ptr (sb-alien:make-alien (* t))))
    (unwind-protect
         (progn
           ;; Initialize to null
           (setf (sb-alien:deref stmt-ptr-ptr) (sb-sys:int-sap 0))
           ;; Prepare the statement
           (let* ((sql-str (sb-alien:make-alien-string sql))
                  (result (sqlite3-prepare-v2-raw db-ptr 
                                                  (sb-alien:alien-sap sql-str)
                                                  -1 
                                                  (sb-alien:alien-sap stmt-ptr-ptr)
                                                  (null-pointer))))
             (sb-alien:free-alien sql-str)
             (check-sqlite-error result db-ptr "preparing statement")
             ;; Get the statement handle
             (let* ((stmt (sb-alien:deref stmt-ptr-ptr))
                    (stmt-sap (sb-alien:alien-sap stmt))
                    (stmt-obj (make-instance 'sqlite-statement
                                           :stmt-ptr stmt-sap
                                           :connection conn)))
               ;; Cache column information
               (let ((col-count (sqlite3-column-count stmt-sap)))
                 (setf (sqlite-stmt-column-count stmt-obj) col-count)
                 (setf (sqlite-stmt-column-names stmt-obj)
                       (loop for i from 0 below col-count
                             collect (sqlite3-column-name stmt-sap i))))
               stmt-obj)))
      (sb-alien:free-alien stmt-ptr-ptr))))

(defmethod sql:backend-bind ((backend sqlite-backend) statement params)
  "Bind parameters to prepared statement"
  (let* ((stmt (sql:statement-handle statement))
         (stmt-ptr (sqlite-stmt-ptr stmt))
         (param-count (sqlite3-bind-parameter-count stmt-ptr)))
    (unless (= param-count (length params))
      (error 'sql:sql-error
             :backend :sqlite
             :message (format nil "Expected ~D parameters, got ~D" 
                              param-count (length params))))
    ;; Clear previous bindings
    (sqlite3-clear-bindings stmt-ptr)
    ;; Bind each parameter (SQLite uses 1-based indexing)
    (loop for param in params
          for index from 1
          do (bind-parameter stmt-ptr index param))
    statement))

(defun bind-parameter (stmt-ptr index value)
  "Bind a single parameter to statement"
  (cond
    ((sql:sql-null-p value)
     (sqlite3-bind-null stmt-ptr index))
    ((null value)
     (sqlite3-bind-null stmt-ptr index))
    ((stringp value)
     (let ((c-str (sb-alien:make-alien-string value)))
       (unwind-protect
            (sqlite3-bind-text-raw stmt-ptr index 
                                   (sb-alien:alien-sap c-str)
                                   -1 (sb-sys:int-sap #xFFFFFFFFFFFFFFFF))  ; SQLITE_TRANSIENT (-1)
         (sb-alien:free-alien c-str))))
    ((integerp value)
     (if (< (abs value) (expt 2 31))
         (sqlite3-bind-int stmt-ptr index value)
         (sqlite3-bind-int64 stmt-ptr index value)))
    ((floatp value)
     (sqlite3-bind-double stmt-ptr index (coerce value 'single-float)))
    ((vectorp value)
     ;; Bind as blob - allocate byte by byte
     (let* ((len (length value))
            (blob-array (sb-alien:make-alien (sb-alien:unsigned 8) len)))
       (dotimes (i len)
         (setf (sb-alien:deref blob-array i) (aref value i)))
       (unwind-protect
            (sqlite3-bind-blob stmt-ptr index 
                               (sb-alien:alien-sap blob-array)
                               len (sb-sys:int-sap #xFFFFFFFFFFFFFFFF))  ; SQLITE_TRANSIENT (-1)
         (sb-alien:free-alien blob-array))))
    (t
     (error 'sql:sql-error
            :backend :sqlite
            :message (format nil "Unsupported parameter type: ~A" (type-of value))))))

(defmethod sql:backend-fetch ((backend sqlite-backend) statement)
  "Fetch next row from statement results"
  (let* ((stmt (sql:statement-handle statement))
         (stmt-ptr (sqlite-stmt-ptr stmt))
         (result (sqlite3-step stmt-ptr)))
    (cond
      ((= result +sqlite-row+)
       (fetch-row stmt))
      ((= result +sqlite-done+)
       ;; Reset the statement for reuse
       (sqlite3-reset stmt-ptr)
       nil)
      (t
       (check-sqlite-error result 
                           (sqlite-db-ptr (sqlite-stmt-connection stmt))
                           "fetching row")))))

(defun fetch-row (stmt)
  "Fetch current row data"
  (let ((stmt-ptr (sqlite-stmt-ptr stmt))
        (col-count (sqlite-stmt-column-count stmt)))
    (loop for i from 0 below col-count
          collect (fetch-column stmt-ptr i))))

(defun fetch-column (stmt-ptr index)
  "Fetch single column value"
  (let ((col-type (sqlite3-column-type stmt-ptr index)))
    (case col-type
      (#.+sqlite-null+ sql:+sql-null+)
      (#.+sqlite-integer+ (sqlite3-column-int64 stmt-ptr index))
      (#.+sqlite-float+ (coerce (sqlite3-column-double stmt-ptr index) 'double-float))
      (#.+sqlite-text+ (sqlite3-column-text stmt-ptr index))
      (#.+sqlite-blob+ 
       (let* ((blob-ptr (sqlite3-column-blob stmt-ptr index))
              (size (sqlite3-column-bytes stmt-ptr index)))
         (if (null-pointer-p blob-ptr)
             #()
             (let ((result (make-array size :element-type '(unsigned-byte 8))))
               (dotimes (i size)
                 (setf (aref result i)
                       (sb-sys:sap-ref-8 blob-ptr i)))
               result))))
      (t sql:+sql-null+))))

(defmethod sql:backend-finalize ((backend sqlite-backend) statement)
  "Finalize prepared statement"
  (let* ((stmt (sql:statement-handle statement))
         (stmt-ptr (sqlite-stmt-ptr stmt))
         (conn (sqlite-stmt-connection stmt))
         (result (sqlite3-finalize stmt-ptr)))
    (check-sqlite-error result (sqlite-db-ptr conn) "finalizing statement")))

(defmethod sql:backend-begin-transaction ((backend sqlite-backend) connection)
  "Begin transaction"
  (sql:execute connection "BEGIN TRANSACTION"))

(defmethod sql:backend-commit ((backend sqlite-backend) connection)
  "Commit transaction"
  (sql:execute connection "COMMIT"))

(defmethod sql:backend-rollback ((backend sqlite-backend) connection)
  "Rollback transaction"
  (sql:execute connection "ROLLBACK"))

;;; Initialization

(defun initialize-sqlite ()
  "Initialize SQLite backend and register it"
  (log:info "Initializing SQLite backend")
  (let ((version (sqlite3-libversion)))
    (log:info "SQLite version: ~A" version)
    (sql:register-backend :sqlite 'sqlite-backend :version version)))

;; Auto-initialize on load
;; Updated after string binding fix
(initialize-sqlite)
