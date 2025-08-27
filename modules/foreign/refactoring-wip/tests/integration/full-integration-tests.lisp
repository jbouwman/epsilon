;;;; Full Integration Tests for epsilon.foreign Refactoring
;;;;
;;;; End-to-end tests ensuring complete functionality

(defpackage epsilon.foreign.integration-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (#:ffi epsilon.foreign.core)
   (#:backend epsilon.foreign.backend.protocol)
   (#:types epsilon.foreign.types)
   (#:fast epsilon.foreign.fast-path)
   (#:compat epsilon.foreign.compat)
   (#:fw epsilon.foreign.test-framework)))

(in-package epsilon.foreign.integration-tests)

;;;; Real Library Integration Tests

(deftest test-openssl-integration ()
  "Test integration with OpenSSL library"
  ;; Define OpenSSL functions
  (ffi:defshared ssl-library-init "SSL_library_init" "libssl" :int ())
  (ffi:defshared ssl-ctx-new "SSL_CTX_new" "libssl" :pointer ((method :pointer)))
  (ffi:defshared ssl-ctx-free "SSL_CTX_free" "libssl" :void ((ctx :pointer)))
  
  ;; Initialize SSL
  (is (>= (ssl-library-init) 0))
  
  ;; Create and free context (basic smoke test)
  (let ((ctx (ssl-ctx-new (sb-sys:int-sap 0))))
    (when (and ctx (not (sb-sys:sap= ctx (sb-sys:int-sap 0))))
      (ssl-ctx-free ctx)
      (is t "SSL context created and freed successfully"))))

(deftest test-sqlite-integration ()
  "Test integration with SQLite library"
  ;; Define SQLite functions
  (ffi:defshared sqlite3-libversion "sqlite3_libversion" "libsqlite3" :string ())
  (ffi:defshared sqlite3-open "sqlite3_open" "libsqlite3" :int 
    ((filename :string) (db-ptr :pointer)))
  (ffi:defshared sqlite3-close "sqlite3_close" "libsqlite3" :int ((db :pointer)))
  
  ;; Get version
  (let ((version (sqlite3-libversion)))
    (is (stringp version))
    (is (> (length version) 0)))
  
  ;; Open and close database
  (ffi:with-foreign-memory (db-ptr 8)
    (let ((result (sqlite3-open ":memory:" db-ptr)))
      (is (zerop result))
      (let ((db (sb-sys:sap-ref-sap db-ptr 0)))
        (unless (sb-sys:sap= db (sb-sys:int-sap 0))
          (is (zerop (sqlite3-close db))))))))

(deftest test-math-library-integration ()
  "Test integration with math library"
  ;; Define math functions
  (ffi:defshared c-sin "sin" "libm" :double ((x :double)))
  (ffi:defshared c-cos "cos" "libm" :double ((x :double)))
  (ffi:defshared c-sqrt "sqrt" "libm" :double ((x :double)))
  (ffi:defshared c-pow "pow" "libm" :double ((x :double) (y :double)))
  
  ;; Test trigonometric functions
  (is (< (abs (- (c-sin 0.0d0) 0.0d0)) 0.0001))
  (is (< (abs (- (c-cos 0.0d0) 1.0d0)) 0.0001))
  
  ;; Test sqrt
  (is (< (abs (- (c-sqrt 4.0d0) 2.0d0)) 0.0001))
  (is (< (abs (- (c-sqrt 9.0d0) 3.0d0)) 0.0001))
  
  ;; Test pow
  (is (< (abs (- (c-pow 2.0d0 3.0d0) 8.0d0)) 0.0001)))

;;;; Complex Struct Tests

(deftest test-complex-struct-handling ()
  "Test handling of complex nested structures"
  ;; Define nested structures
  (ffi:define-c-struct 'vec3
    '((x :float)
      (y :float)
      (z :float)))
  
  (ffi:define-c-struct 'matrix3x3
    '((row1 vec3)
      (row2 vec3)
      (row3 vec3)))
  
  ;; Test struct operations
  (ffi:with-c-struct (mat 'matrix3x3)
    ;; Set identity matrix
    (setf (ffi:struct-ref (ffi:struct-ref mat 'row1) 'x) 1.0)
    (setf (ffi:struct-ref (ffi:struct-ref mat 'row2) 'y) 1.0)
    (setf (ffi:struct-ref (ffi:struct-ref mat 'row3) 'z) 1.0)
    
    ;; Verify
    (is (= (ffi:struct-ref (ffi:struct-ref mat 'row1) 'x) 1.0))
    (is (= (ffi:struct-ref (ffi:struct-ref mat 'row2) 'y) 1.0))
    (is (= (ffi:struct-ref (ffi:struct-ref mat 'row3) 'z) 1.0))))

;;;; Callback Integration Tests

(deftest test-qsort-with-callback ()
  "Test callbacks with qsort from libc"
  ;; Define qsort
  (ffi:defshared qsort "qsort" "libc" :void
    ((base :pointer) (nmemb :size-t) (size :size-t) (compar :pointer)))
  
  ;; Define comparison callback
  (ffi:defcallback int-comparator :int ((a :pointer) (b :pointer))
    (let ((val-a (sb-sys:sap-ref-32 a 0))
          (val-b (sb-sys:sap-ref-32 b 0)))
      (cond ((< val-a val-b) -1)
            ((> val-a val-b) 1)
            (t 0))))
  
  ;; Create and sort array
  (let ((array-size 10)
        (int-size 4))
    (ffi:with-foreign-memory (array (* array-size int-size))
      ;; Initialize with random values
      (dotimes (i array-size)
        (setf (sb-sys:sap-ref-32 array (* i int-size))
              (- 50 (* i 7))))
      
      ;; Sort using callback
      (qsort array array-size int-size 
             (ffi:callback-pointer 'int-comparator))
      
      ;; Verify sorted
      (loop for i from 1 below array-size
            for prev = (sb-sys:sap-ref-32 array (* (1- i) int-size))
            for curr = (sb-sys:sap-ref-32 array (* i int-size))
            do (is (<= prev curr))))))

;;;; Performance Validation Tests

(deftest test-performance-requirements ()
  "Verify performance meets requirements"
  ;; Test simple call performance
  (ffi:defshared perf-getpid "getpid" "libc" :int ())
  
  (let ((result (fw:benchmark-operation "getpid-performance"
                  (lambda () (perf-getpid))
                  :iterations 100000
                  :warmup-iterations 1000)))
    ;; Should be very fast (< 100ns per call)
    (is (< (fw:benchmark-result-average-time result) 100)))
  
  ;; Test string function performance
  (ffi:defshared perf-strlen "strlen" "libc" :size-t ((s :string)))
  
  (let ((test-string "Performance test string"))
    (let ((result (fw:benchmark-operation "strlen-performance"
                    (lambda () (perf-strlen test-string))
                    :iterations 50000)))
      ;; Should be fast (< 500ns per call with conversion)
      (is (< (fw:benchmark-result-average-time result) 500)))))

;;;; Backend Integration Tests

(deftest test-backend-switching ()
  "Test dynamic backend switching works correctly"
  ;; Register test backends
  (let ((fast-backend (make-instance 'backend:trampoline-backend
                                      :name "fast-test"))
        (full-backend (make-instance 'backend:libffi-backend
                                      :name "full-test")))
    
    (backend:register-backend :fast-test fast-backend)
    (backend:register-backend :full-test full-backend)
    
    ;; Test with fast backend
    (backend:with-backend fast-backend
      (ffi:defshared fast-abs "abs" "libc" :int ((x :int)))
      (is (= (fast-abs -42) 42)))
    
    ;; Test with full backend
    (backend:with-backend full-backend
      (ffi:defshared full-abs "abs" "libc" :int ((x :int)))
      (is (= (full-abs -42) 42)))
    
    ;; Clean up
    (backend:unregister-backend :fast-test)
    (backend:unregister-backend :full-test)))

;;;; Migration Compatibility Tests

(deftest test-migration-compatibility ()
  "Test that migrated code works correctly"
  ;; Enable compatibility mode
  (compat:enable-compatibility-mode)
  
  ;; Test old API still works (with warnings suppressed)
  (let ((compat:*suppress-deprecation-warnings* t))
    ;; Old style defshared should work
    (eval '(epsilon.foreign:defshared-fast old-style-strlen 
            "strlen" "libc" :unsigned-long ((str :string))))
    (is (= (funcall 'old-style-strlen "test") 4))
    
    ;; Old style shared-call variants should work
    (is (= (epsilon.foreign:shared-call-unified 
            "strlen" :unsigned-long '(:string) "hello") 5))
    (is (= (epsilon.foreign:shared-call-fast 
            "strlen" :unsigned-long '(:string) "world") 5)))
  
  ;; Generate migration report
  (let ((report (with-output-to-string (s)
                  (let ((*standard-output* s))
                    (compat:generate-migration-report)))))
    (is (search "Migration Report" report))))

;;;; Thread Safety Tests

(deftest test-thread-safety-comprehensive ()
  "Comprehensive thread safety test"
  (ffi:defshared thread-strlen "strlen" "libc" :size-t ((s :string)))
  (ffi:defshared thread-getpid "getpid" "libc" :int ())
  
  (let* ((thread-count 20)
         (iterations 1000)
         (errors (make-array thread-count :initial-element nil))
         (threads
          (loop for i below thread-count
                collect (sb-thread:make-thread
                         (lambda (idx)
                           (handler-case
                               (dotimes (j iterations)
                                 ;; Mix of different operations
                                 (when (zerop (mod j 3))
                                   (thread-strlen "thread test"))
                                 (when (zerop (mod j 5))
                                   (thread-getpid))
                                 (when (zerop (mod j 7))
                                   (ffi:with-foreign-memory (ptr 128)
                                     (setf (sb-sys:sap-ref-64 ptr 0) idx))))
                             (error (e)
                               (setf (aref errors idx) e))))
                         :arguments (list i)))))
    
    ;; Wait for all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    
    ;; Check for errors
    (loop for i below thread-count
          do (is (null (aref errors i))
                 (format nil "Thread ~D had error: ~A" 
                         i (aref errors i))))))

;;;; Memory Safety Tests

(deftest test-memory-safety ()
  "Test memory safety and leak prevention"
  ;; Test that memory is properly freed
  (let ((baseline-memory (sb-ext:get-bytes-consed)))
    ;; Allocate and free many times
    (dotimes (i 1000)
      (ffi:with-foreign-memory (ptr 1024)
        (setf (sb-sys:sap-ref-32 ptr 0) i)))
    
    ;; Check minimal memory growth
    (let ((memory-growth (- (sb-ext:get-bytes-consed) baseline-memory)))
      (is (< memory-growth 1000000) ; Less than 1MB growth
          "Memory usage should be stable")))
  
  ;; Test string conversion doesn't leak
  (fw:detect-memory-leak
   (lambda ()
     (ffi:with-c-string (ptr "no memory leaks")
       (ffi:defshared test-strlen "strlen" "libc" :size-t ((s :pointer)))
       (test-strlen ptr)))
   :iterations 1000
   :threshold 0.05))

;;;; Error Handling Tests

(deftest test-comprehensive-error-handling ()
  "Test error handling in various scenarios"
  ;; Test invalid library
  (handler-case
      (ffi:lib-open "nonexistent_library_xyz.so")
    (error (e)
      (is (typep e 'error))))
  
  ;; Test invalid function
  (handler-case
      (ffi:shared-call "nonexistent_function_xyz" :int '())
    (ffi:foreign-error (e)
      (is (typep e 'ffi:foreign-error))))
  
  ;; Test with-foreign-error-handler
  (let ((handled nil))
    (ffi:with-foreign-error-handler
        (ffi:shared-call "bad_function" :int '())
      (:on-error (e)
        (setf handled t)))
    (is handled)))

;;;; Full System Test

(deftest test-full-system-integration ()
  "Test complete system with all components"
  ;; This test exercises all major components together
  
  ;; 1. Backend system
  (is backend:*default-backend*)
  
  ;; 2. Type system  
  (types:define-foreign-type :test-handle
    :base-type :pointer
    :size 8)
  (is (types:get-type-info :test-handle))
  
  ;; 3. Core API
  (ffi:defshared system-strlen "strlen" "libc" :size-t ((s :string)))
  (is (= (system-strlen "system test") 11))
  
  ;; 4. Optimizations
  (when fast:*enable-jit*
    (dotimes (i 200) ; Trigger JIT
      (system-strlen "jit test"))
    (is (>= (gethash '("strlen" "libc") fast:*call-statistics* 0) 200)))
  
  ;; 5. Compatibility
  (let ((compat:*suppress-deprecation-warnings* t))
    (is (fboundp 'epsilon.foreign:shared-call)))
  
  (is t "Full system integration successful"))