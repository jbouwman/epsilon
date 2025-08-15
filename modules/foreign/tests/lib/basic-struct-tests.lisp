;;;; basic-struct-tests.lisp - Basic tests for struct functionality

(defpackage epsilon.foreign.basic-struct-tests
  (:use cl epsilon.syntax epsilon.test)
  (:local-nicknames
   (struct epsilon.foreign.struct)))

(in-package epsilon.foreign.basic-struct-tests)

;;;; Basic struct functionality tests

(deftest test-basic-struct-definition
  "Test defining a simple struct"
  (struct:define-c-struct 'test-point
    '((x :int)
      (y :int)))
  
  (let ((layout (struct:get-struct-layout 'test-point)))
    (is (struct:struct-layout-p layout) "Should create a valid layout")
    (is (>= (struct:struct-layout-size layout) 8) "Should have size at least 8 bytes")
    (is (struct:struct-has-field-p layout 'x) "Should have field x")
    (is (struct:struct-has-field-p layout 'y) "Should have field y")))

(deftest test-struct-instance-creation
  "Test creating and accessing struct instances"
  ;; Define struct with package-qualified symbol
  (struct:define-c-struct 'epsilon.foreign.struct::test-point2
    '((x :int)
      (y :int)))
  
  ;; Verify the struct was registered
  (let ((layout (struct:get-struct-layout 'epsilon.foreign.struct::test-point2)))
    (is layout "Struct should be registered")
    (when layout
      (is (struct:struct-layout-p layout) "Should be a valid layout")))
  
  (when (struct:get-struct-layout 'epsilon.foreign.struct::test-point2)
    (struct:with-c-struct (pt epsilon.foreign.struct::test-point2)
      ;; Set values
      (setf (struct:struct-ref pt 'x) 100)
      (setf (struct:struct-ref pt 'y) 200)
      
      ;; Read values back
      (is (= (struct:struct-ref pt 'x) 100) "Should read x value correctly")
      (is (= (struct:struct-ref pt 'y) 200) "Should read y value correctly"))))

(deftest test-struct-with-different-types
  "Test struct with various field types"
  (struct:define-c-struct 'test-mixed
    '((byte-val :unsigned-char)
      (int-val :int)
      (long-val :long)
      (float-val :float)
      (double-val :double)))
  
  (struct:with-c-struct (s 'test-mixed)
    (setf (struct:struct-ref s 'byte-val) 255)
    (setf (struct:struct-ref s 'int-val) -1000)
    (setf (struct:struct-ref s 'long-val) 9999999)
    (setf (struct:struct-ref s 'float-val) 3.14)
    (setf (struct:struct-ref s 'double-val) 2.71828)
    
    (is (= (struct:struct-ref s 'byte-val) 255) "Byte value should match")
    (is (= (struct:struct-ref s 'int-val) -1000) "Int value should match")
    (is (= (struct:struct-ref s 'long-val) 9999999) "Long value should match")
    (is (< (abs (- (struct:struct-ref s 'float-val) 3.14)) 0.001) "Float value should be close")
    (is (< (abs (- (struct:struct-ref s 'double-val) 2.71828)) 0.00001) "Double value should be close")))

(deftest test-nested-struct
  "Test struct containing another struct"
  (struct:define-c-struct 'test-inner
    '((a :int)
      (b :int)))
  
  (struct:define-c-struct 'test-outer
    '((x :int)
      (inner (:struct test-inner))
      (y :int)))
  
  (let ((layout (struct:get-struct-layout 'test-outer)))
    (is (struct:struct-layout-p layout) "Should create valid nested struct layout")
    (is (struct:struct-has-field-p layout 'x) "Should have field x")
    (is (struct:struct-has-field-p layout 'inner) "Should have field inner")
    (is (struct:struct-has-field-p layout 'y) "Should have field y")))

(deftest test-struct-memory-allocation
  "Test explicit struct memory allocation"
  (struct:define-c-struct 'test-alloc-point
    '((x :int)
      (y :int)))
  
  (let* ((layout (struct:get-struct-layout 'test-alloc-point))
         (size (struct:struct-layout-size layout))
         (ptr (struct:foreign-alloc size)))
    (unwind-protect
         (progn
           (is (sb-sys:system-area-pointer-p ptr) "Should allocate a pointer")
           (is (not (sb-sys:sap= ptr (sb-sys:int-sap 0))) "Should not be null"))
      (struct:foreign-free ptr))))

(deftest test-struct-field-offsets
  "Test that field offsets are calculated correctly"
  (struct:define-c-struct 'test-offsets
    '((a :char)
      (b :int)
      (c :char)
      (d :long)))
  
  (let ((layout (struct:get-struct-layout 'test-offsets)))
    ;; Check that offsets exist and are increasing
    (let ((offset-a (struct:struct-field-offset layout 'a))
          (offset-b (struct:struct-field-offset layout 'b))
          (offset-c (struct:struct-field-offset layout 'c))
          (offset-d (struct:struct-field-offset layout 'd)))
      
      (is (= offset-a 0) "First field should be at offset 0")
      (is (> offset-b offset-a) "Field b should come after a")
      (is (> offset-c offset-b) "Field c should come after b")
      (is (> offset-d offset-c) "Field d should come after c")
      
      ;; Check alignment - int should be aligned
      (is (zerop (mod offset-b 4)) "Int field should be 4-byte aligned")
      ;; Long should be aligned (8 bytes on 64-bit)
      (is (zerop (mod offset-d 8)) "Long field should be 8-byte aligned"))))

(deftest test-struct-union
  "Test union definition and usage"
  (struct:define-c-union 'test-union
    '((int-val :int)
      (float-val :float)
      (bytes (:array :unsigned-char 4))))
  
  (let ((layout (struct:get-struct-layout 'test-union)))
    (is (struct:struct-layout-p layout) "Should create valid union layout")
    ;; All fields in a union should have offset 0
    (is (= (struct:struct-field-offset layout 'int-val) 0) "Union field should be at offset 0")
    (is (= (struct:struct-field-offset layout 'float-val) 0) "Union field should be at offset 0")
    (is (= (struct:struct-field-offset layout 'bytes) 0) "Union field should be at offset 0")))

(deftest test-struct-pointer-field
  "Test struct with pointer fields"
  (struct:define-c-struct 'test-with-pointer
    '((value :int)
      (next :pointer)))
  
  (struct:with-c-struct (node 'test-with-pointer)
    (setf (struct:struct-ref node 'value) 42)
    (setf (struct:struct-ref node 'next) (sb-sys:int-sap 0))
    
    (is (= (struct:struct-ref node 'value) 42) "Should store int value")
    (is (sb-sys:sap= (struct:struct-ref node 'next) (sb-sys:int-sap 0)) 
        "Should store null pointer")))