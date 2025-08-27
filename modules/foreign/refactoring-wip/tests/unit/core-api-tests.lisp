;;;; Core API Tests for epsilon.foreign
;;;;
;;;; Tests for the minimal 22-function public API

(defpackage epsilon.foreign.core-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (#:ffi epsilon.foreign.core)
   (#:fw epsilon.foreign.test-framework)))

(in-package epsilon.foreign.core-tests)

;;;; Function Definition Tests

(deftest test-defshared-macro ()
  "Test defshared macro expansion and function definition"
  ;; Test simple function definition
  (ffi:defshared strlen "strlen" "libc" :size-t ((str :string))
    :documentation "Get string length")
  
  (is (fboundp 'strlen))
  (is (string= (documentation 'strlen 'function) "Get string length"))
  
  ;; Test calling the function
  (is (= (strlen "hello") 5))
  (is (= (strlen "") 0))
  
  ;; Test function with multiple arguments
  (ffi:defshared memcmp "memcmp" "libc" :int 
    ((s1 :pointer) (s2 :pointer) (n :size-t))
    :documentation "Compare memory")
  
  (is (fboundp 'memcmp))
  
  ;; Test with no arguments
  (ffi:defshared getpid "getpid" "libc" :int ()
    :documentation "Get process ID")
  
  (is (integerp (getpid)))
  (is (> (getpid) 0)))

(deftest test-shared-call ()
  "Test direct shared-call function"
  ;; Test with string
  (is (= (ffi:shared-call "strlen" :size-t '(:string) "test") 4))
  
  ;; Test with library specification
  (is (= (ffi:shared-call '("strlen" "libc") :size-t '(:string) "hello") 5))
  
  ;; Test void function
  (is (null (ffi:shared-call "free" :void '(:pointer) (sb-sys:int-sap 0))))
  
  ;; Test error handling
  (is-error (ffi:shared-call "nonexistent_function" :int '())
            'ffi:foreign-error))

;;;; Library Management Tests

(deftest test-library-management ()
  "Test library loading and function lookup"
  ;; Test opening library
  (let ((handle (ffi:lib-open "libc")))
    (is handle) ; May be nil for libc (already loaded)
    
    ;; Test function lookup
    (let ((fn-ptr (ffi:lib-function handle "strlen")))
      (is (sb-sys:system-area-pointer-p fn-ptr))
      (is (not (sb-sys:sap= fn-ptr (sb-sys:int-sap 0)))))
    
    ;; Test closing library
    (is (ffi:lib-close handle)))
  
  ;; Test with non-existent library
  (is-error (ffi:lib-open "nonexistent_library_xyz")
            'error))

;;;; Memory Management Tests

(deftest test-memory-allocation ()
  "Test foreign memory allocation and deallocation"
  ;; Test basic allocation
  (let ((ptr (ffi:foreign-alloc 1024)))
    (is (sb-sys:system-area-pointer-p ptr))
    (is (not (sb-sys:sap= ptr (sb-sys:int-sap 0))))
    
    ;; Write and read back
    (setf (sb-sys:sap-ref-32 ptr 0) #xDEADBEEF)
    (is (= (sb-sys:sap-ref-32 ptr 0) #xDEADBEEF))
    
    ;; Test deallocation
    (ffi:foreign-free ptr))
  
  ;; Test with-foreign-memory macro
  (ffi:with-foreign-memory (ptr 256)
    (is (sb-sys:system-area-pointer-p ptr))
    ;; Write to ensure it's valid memory
    (setf (sb-sys:sap-ref-64 ptr 0) 42)
    (is (= (sb-sys:sap-ref-64 ptr 0) 42)))
  ;; Memory should be freed automatically
  
  ;; Test zero allocation
  (let ((ptr (ffi:foreign-alloc 0)))
    (is ptr) ; Should still return valid pointer
    (ffi:foreign-free ptr)))

;;;; Type Conversion Tests

(deftest test-type-conversions ()
  "Test type conversion functions"
  ;; Test integer conversions
  (is (= (ffi:convert-to-foreign 42 :int) 42))
  (is (= (ffi:convert-from-foreign 42 :int) 42))
  
  ;; Test float conversions
  (is (= (ffi:convert-to-foreign 3.14 :float) 3.14))
  (is (typep (ffi:convert-to-foreign 3.14d0 :float) 'single-float))
  
  ;; Test pointer conversions
  (let ((ptr (sb-sys:int-sap #x1000)))
    (is (sb-sys:sap= (ffi:convert-to-foreign ptr :pointer) ptr)))
  
  ;; Test boolean conversions
  (is (= (ffi:convert-to-foreign t :bool) 1))
  (is (= (ffi:convert-to-foreign nil :bool) 0))
  (is (eq (ffi:convert-from-foreign 1 :bool) t))
  (is (eq (ffi:convert-from-foreign 0 :bool) nil)))

(deftest test-string-conversions ()
  "Test string conversion with memory management"
  ;; Test with-c-string macro
  (ffi:with-c-string (ptr "hello world")
    (is (sb-sys:system-area-pointer-p ptr))
    ;; Verify content
    (is (= (sb-sys:sap-ref-8 ptr 0) (char-code #\h)))
    (is (= (sb-sys:sap-ref-8 ptr 5) (char-code #\space))))
  ;; Memory should be freed
  
  ;; Test null string
  (ffi:with-c-string (ptr nil)
    (is (sb-sys:sap= ptr (sb-sys:int-sap 0))))
  
  ;; Test empty string
  (ffi:with-c-string (ptr "")
    (is (= (sb-sys:sap-ref-8 ptr 0) 0))))

(deftest test-custom-type-definition ()
  "Test defining custom types"
  ;; Define a custom type
  (ffi:define-c-type :my-int 4 :alignment 4 :base-type :int)
  
  ;; Use the custom type
  (ffi:defshared test-func "abs" "libc" :my-int ((x :my-int)))
  (is (= (test-func -42) 42)))

;;;; Structure Tests

(deftest test-structure-operations ()
  "Test structure definition and manipulation"
  ;; Define a simple struct
  (ffi:define-c-struct 'point
    '((x :int)
      (y :int)))
  
  ;; Test allocation and access
  (ffi:with-c-struct (pt 'point)
    (setf (ffi:struct-ref pt 'x) 10)
    (setf (ffi:struct-ref pt 'y) 20)
    (is (= (ffi:struct-ref pt 'x) 10))
    (is (= (ffi:struct-ref pt 'y) 20)))
  
  ;; Test struct size
  (is (= (ffi:struct-size 'point) 8)))

(deftest test-nested-structures ()
  "Test nested structure handling"
  ;; Define nested structs
  (ffi:define-c-struct 'rect
    '((top-left point)
      (bottom-right point)))
  
  (ffi:with-c-struct (r 'rect)
    (setf (ffi:struct-ref (ffi:struct-ref r 'top-left) 'x) 0)
    (setf (ffi:struct-ref (ffi:struct-ref r 'top-left) 'y) 0)
    (setf (ffi:struct-ref (ffi:struct-ref r 'bottom-right) 'x) 100)
    (setf (ffi:struct-ref (ffi:struct-ref r 'bottom-right) 'y) 50)
    
    (is (= (ffi:struct-ref (ffi:struct-ref r 'bottom-right) 'x) 100))))

;;;; Callback Tests

(deftest test-callback-definition ()
  "Test callback definition and usage"
  ;; Define a simple callback
  (ffi:defcallback my-comparator :int ((a :pointer) (b :pointer))
    (let ((val-a (sb-sys:sap-ref-32 a 0))
          (val-b (sb-sys:sap-ref-32 b 0)))
      (cond ((< val-a val-b) -1)
            ((> val-a val-b) 1)
            (t 0))))
  
  ;; Get callback pointer
  (let ((ptr (ffi:callback-pointer 'my-comparator)))
    (is (sb-sys:system-area-pointer-p ptr))
    (is (not (sb-sys:sap= ptr (sb-sys:int-sap 0)))))
  
  ;; Test with-callback macro for temporary callbacks
  (ffi:with-callback (adder :int ((x :int) (y :int))
                       (+ x y))
    (is (sb-sys:system-area-pointer-p adder))))

;;;; Error Handling Tests

(deftest test-error-conditions ()
  "Test error handling and conditions"
  ;; Test foreign-error condition
  (handler-case
      (ffi:shared-call "nonexistent" :int '())
    (ffi:foreign-error (e)
      (is e)
      (is (typep e 'error))))
  
  ;; Test with-foreign-error-handler
  (ffi:with-foreign-error-handler
      (ffi:shared-call "bad_function" :int '())
    (:on-error (e)
      (is (typep e 'ffi:foreign-error)))))

;;;; Integration Tests

(deftest test-real-library-usage ()
  "Test with real library functions"
  ;; Test math functions
  (ffi:defshared sqrt-ffi "sqrt" "libm" :double ((x :double)))
  (is (= (sqrt-ffi 4.0d0) 2.0d0))
  (is (= (sqrt-ffi 9.0d0) 3.0d0))
  
  ;; Test string functions
  (ffi:defshared strchr "strchr" "libc" :pointer ((s :string) (c :int)))
  (ffi:with-c-string (str "hello")
    (let ((result (strchr str (char-code #\l))))
      (is (sb-sys:system-area-pointer-p result))
      (is (not (sb-sys:sap= result (sb-sys:int-sap 0)))))))

(deftest test-thread-safety ()
  "Test thread-safe operations"
  (let ((results (make-array 10)))
    ;; Multiple threads calling FFI functions
    (let ((threads
           (loop for i below 10
                 collect (sb-thread:make-thread
                          (lambda (idx)
                            (ffi:defshared getpid-local "getpid" "libc" :int ())
                            (setf (aref results idx) (getpid-local)))
                          :arguments (list i)))))
      
      ;; Wait for all threads
      (dolist (thread threads)
        (sb-thread:join-thread thread))
      
      ;; All should get same PID
      (let ((first-pid (aref results 0)))
        (loop for i from 1 below 10
              do (is (= (aref results i) first-pid)))))))