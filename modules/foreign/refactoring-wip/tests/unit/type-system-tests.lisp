;;;; Type System Tests for epsilon.foreign
;;;;
;;;; Tests for the new type conversion system - written before implementation (TDD)

(defpackage epsilon.foreign.type-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (#:types epsilon.foreign.types)
   (#:fw epsilon.foreign.test-framework)))

(in-package epsilon.foreign.type-tests)

;;;; Type Registration Tests

(deftest test-primitive-type-registration ()
  "Test registration of primitive types"
  ;; Test built-in types are registered
  (is (types:get-type-info :int))
  (is (types:get-type-info :pointer))
  (is (types:get-type-info :string))
  (is (types:get-type-info :double))
  
  ;; Test type properties
  (let ((int-type (types:get-type-info :int)))
    (is (= (types:type-size int-type) 4))
    (is (= (types:type-alignment int-type) 4))
    (is (eq (types:type-sbcl-type int-type) 'sb-alien:int)))
  
  ;; Test non-existent type
  (is (null (types:get-type-info :nonexistent))))

(deftest test-custom-type-registration ()
  "Test defining custom types"
  ;; Define a custom type
  (types:define-foreign-type :my-handle
    :base-type :pointer
    :size 8
    :alignment 8
    :to-foreign (lambda (x) (types:handle-to-pointer x))
    :from-foreign (lambda (x) (types:pointer-to-handle x)))
  
  (let ((handle-type (types:get-type-info :my-handle)))
    (is handle-type)
    (is (= (types:type-size handle-type) 8))
    (is (eq (types:type-base-type handle-type) :pointer)))
  
  ;; Test type aliasing
  (types:define-type-alias :size-t :unsigned-long)
  (is (eq (types:resolve-type-alias :size-t) :unsigned-long)))

;;;; Type Conversion Tests

(deftest test-primitive-conversions ()
  "Test conversion of primitive types"
  ;; Integer conversions
  (is (= (types:convert-to-foreign 42 :int) 42))
  (is (= (types:convert-from-foreign 42 :int) 42))
  
  ;; Boolean conversions
  (is (= (types:convert-to-foreign t :bool) 1))
  (is (= (types:convert-to-foreign nil :bool) 0))
  (is (eq (types:convert-from-foreign 1 :bool) t))
  (is (eq (types:convert-from-foreign 0 :bool) nil))
  
  ;; Float conversions
  (is (= (types:convert-to-foreign 3.14 :float) 3.14))
  (is (typep (types:convert-to-foreign 3.14d0 :float) 'single-float))
  
  ;; Pointer conversions
  (let ((ptr (sb-sys:int-sap #x1000)))
    (is (sb-sys:sap= (types:convert-to-foreign ptr :pointer) ptr))
    (is (sb-sys:sap= (types:convert-from-foreign ptr :pointer) ptr))))

(deftest test-string-conversions ()
  "Test string conversion with proper memory management"
  ;; Basic string conversion
  (let ((foreign-str (types:convert-to-foreign "hello" :string)))
    (is (sb-sys:system-area-pointer-p foreign-str))
    
    ;; Read back the string
    (is (string= (types:convert-from-foreign foreign-str :string) "hello"))
    
    ;; Clean up
    (types:free-converted-object foreign-str :string))
  
  ;; Test null string handling
  (is (sb-sys:sap= (types:convert-to-foreign nil :string) 
                    (sb-sys:int-sap 0)))
  (is (null (types:convert-from-foreign (sb-sys:int-sap 0) :string)))
  
  ;; Test with-c-string macro
  (types:with-c-string (ptr "test string")
    (is (string= (types:convert-from-foreign ptr :string) "test string")))
  ;; Ensure cleanup happened
  )

(deftest test-array-conversions ()
  "Test array type conversions"
  ;; Define array type
  (types:define-array-type :int-array :int)
  
  ;; Convert Lisp array to foreign
  (let* ((lisp-array #(1 2 3 4 5))
         (foreign-array (types:convert-to-foreign lisp-array :int-array)))
    
    ;; Check individual elements
    (dotimes (i 5)
      (is (= (sb-sys:sap-ref-32 foreign-array (* i 4))
             (aref lisp-array i))))
    
    ;; Convert back
    (let ((converted-back (types:convert-from-foreign 
                           foreign-array :int-array 5)))
      (is (equalp converted-back lisp-array)))
    
    ;; Clean up
    (types:free-converted-object foreign-array :int-array)))

;;;; Type Size and Alignment Tests

(deftest test-type-sizes ()
  "Test type size calculations"
  (is (= (types:sizeof :char) 1))
  (is (= (types:sizeof :short) 2))
  (is (= (types:sizeof :int) 4))
  (is (= (types:sizeof :long) 8))  ; On 64-bit
  (is (= (types:sizeof :pointer) 8)) ; On 64-bit
  (is (= (types:sizeof :double) 8))
  
  ;; Test array sizes
  (is (= (types:sizeof '(:array :int 10)) 40))
  
  ;; Test struct size (once structs are defined)
  )

(deftest test-type-alignment ()
  "Test type alignment calculations"
  (is (= (types:alignof :char) 1))
  (is (= (types:alignof :short) 2))
  (is (= (types:alignof :int) 4))
  (is (= (types:alignof :long) 8))
  (is (= (types:alignof :pointer) 8))
  (is (= (types:alignof :double) 8))
  
  ;; Test that alignment is power of 2
  (dolist (type '(:char :short :int :long :pointer :double))
    (let ((align (types:alignof type)))
      (is (= (logand align (1- align)) 0)))))

;;;; Type Checking Tests

(deftest test-type-validation ()
  "Test type validation and error handling"
  ;; Invalid type conversions should signal errors
  (is-error (types:convert-to-foreign "not-a-number" :int)
            'type-error)
  
  (is-error (types:convert-to-foreign 'symbol :double)
            'type-error)
  
  ;; Overflow detection
  (is-error (types:convert-to-foreign (1+ most-positive-fixnum) :int)
            'types:overflow-error)
  
  ;; Type compatibility checking
  (is (types:compatible-types-p :int :long))
  (is (types:compatible-types-p :pointer :pointer))
  (is (not (types:compatible-types-p :int :pointer))))

;;;; Compound Type Tests

(deftest test-pointer-to-type ()
  "Test pointer type construction"
  ;; Create pointer types
  (let ((int-ptr-type (types:pointer-to :int))
        (void-ptr-type (types:pointer-to :void)))
    
    (is (eq (types:type-base-type int-ptr-type) :pointer))
    (is (eq (types:pointer-target-type int-ptr-type) :int))
    
    ;; Void pointer is special
    (is (types:void-pointer-p void-ptr-type))
    
    ;; Test dereferencing
    (sb-sys:with-pinned-objects (42)
      (let ((ptr (types:address-of 42)))
        (is (= (types:dereference ptr :int) 42))))))

(deftest test-function-pointer-types ()
  "Test function pointer type handling"
  ;; Define a function pointer type
  (types:define-function-type :comparator
    :return-type :int
    :arg-types '(:pointer :pointer))
  
  (let ((cmp-type (types:get-type-info :comparator)))
    (is cmp-type)
    (is (types:function-pointer-p cmp-type))
    (is (eq (types:function-return-type cmp-type) :int))
    (is (equal (types:function-arg-types cmp-type) '(:pointer :pointer)))))

;;;; Type Caching and Performance Tests

(deftest test-type-conversion-caching ()
  "Test that type conversions are cached appropriately"
  ;; Measure conversion performance
  (let* ((iterations 10000)
         (uncached-time 
          (fw:benchmark-operation "uncached-conversion"
            (lambda ()
              (types:clear-conversion-cache)
              (types:convert-to-foreign 42 :int))
            :iterations iterations))
         (cached-time
          (fw:benchmark-operation "cached-conversion"
            (lambda ()
              (types:convert-to-foreign 42 :int))
            :iterations iterations)))
    
    ;; Cached should be faster
    (fw:assert-better-performance 
     (fw:benchmark-result-average-time cached-time)
     (fw:benchmark-result-average-time uncached-time)
     :threshold 0.5)))

(deftest test-type-table-lookup-performance ()
  "Test that type table lookups are fast"
  (let ((lookup-time
         (fw:benchmark-operation "type-lookup"
           (lambda ()
             (types:get-type-info :int))
           :iterations 100000)))
    
    ;; Should be very fast (< 100ns per lookup)
    (is (< (fw:benchmark-result-average-time lookup-time)
           100))))

;;;; Memory Management Tests

(deftest test-conversion-memory-management ()
  "Test proper memory management in conversions"
  ;; Test that temporary conversions are cleaned up
  (types:with-foreign-values ((i 42 :int)
                               (s "test" :string)
                               (p (sb-sys:int-sap #x1000) :pointer))
    ;; Values should be accessible
    (is (= i 42))
    (is (string= (types:convert-from-foreign s :string) "test"))
    (is (sb-sys:sap= p (sb-sys:int-sap #x1000))))
  ;; Memory should be freed after block
  
  ;; Test memory leak detection
  (fw:detect-memory-leak
   (lambda ()
     (let ((str (types:convert-to-foreign "test" :string)))
       (types:free-converted-object str :string)))
   :iterations 1000))

;;;; Thread Safety Tests

(deftest test-type-system-thread-safety ()
  "Test thread-safe type operations"
  (let ((results (make-array 10)))
    ;; Multiple threads doing conversions
    (let ((threads
           (loop for i below 10
                 collect (sb-thread:make-thread
                          (lambda (index)
                            (setf (aref results index)
                                  (types:convert-to-foreign 
                                   (* index 10) :int)))
                          :arguments (list i)))))
      
      ;; Wait for all threads
      (dolist (thread threads)
        (sb-thread:join-thread thread))
      
      ;; Verify results
      (loop for i below 10
            do (is (= (aref results i) (* i 10)))))))