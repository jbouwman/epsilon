(defpackage epsilon.foreign.struct-basic-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)))

(in-package epsilon.foreign.struct-basic-tests)

;;;; Basic tests for struct functionality

(deftest test-simple-struct-definition
  "Test defining a simple struct"
  ;; Define a simple point struct
  (lib:define-c-struct 'test-point
    '((x :int)
      (y :int)))
  
  ;; Get the layout
  (let ((layout (lib:get-struct-layout 'test-point)))
    (is (lib:struct-layout-p layout))
    (is (= (lib:struct-layout-size layout) 8))  ; 2 ints = 8 bytes
    (is (= (lib:struct-layout-alignment layout) 4))))  ; int alignment

(deftest test-struct-field-access
  "Test basic field access"
  ;; Define struct if not already defined
  (lib:define-c-struct 'test-point2
    '((x :int)
      (y :int)))
  
  ;; Test field info
  (let ((layout (lib:get-struct-layout 'test-point2)))
    (is (= (lib:struct-field-offset layout 'x) 0))
    (is (= (lib:struct-field-offset layout 'y) 4))
    (is (eq (lib:struct-field-type layout 'x) :int))
    (is (eq (lib:struct-field-type layout 'y) :int))))

(deftest test-struct-instance-creation
  "Test creating struct instances"
  ;; Define struct
  (lib:define-c-struct 'test-point3
    '((x :int)
      (y :int)))
  
  ;; Create and use instance
  (lib:with-c-struct (pt 'test-point3)
    ;; Initially zeroed
    (is (= (lib:struct-ref pt 'x) 0))
    (is (= (lib:struct-ref pt 'y) 0))
    
    ;; Set values
    (setf (lib:struct-ref pt 'x) 42)
    (setf (lib:struct-ref pt 'y) 100)
    
    ;; Read back
    (is (= (lib:struct-ref pt 'x) 42))
    (is (= (lib:struct-ref pt 'y) 100))
    
    ;; Check we have a pointer
    (is (sb-sys:system-area-pointer-p (lib:struct-pointer pt)))))

(deftest test-struct-with-different-types
  "Test struct with various field types"
  (lib:define-c-struct 'mixed-struct
    '((byte-val :unsigned-char)
      (short-val :short)
      (int-val :int)
      (long-val :long)
      (float-val :float)
      (double-val :double)
      (ptr-val :pointer)))
  
  (let ((layout (lib:get-struct-layout 'mixed-struct)))
    ;; Basic checks
    (is (lib:struct-layout-p layout))
    ;; Check some offsets are reasonable
    (is (>= (lib:struct-field-offset layout 'double-val) 
            (lib:struct-field-offset layout 'float-val)))
    (is (>= (lib:struct-field-offset layout 'ptr-val)
            (lib:struct-field-offset layout 'double-val)))))

(deftest test-struct-alignment
  "Test struct alignment calculations"
  ;; Struct with padding
  (lib:define-c-struct 'aligned-struct
    '((a :char)      ; 1 byte
      (b :int)       ; 4 bytes, needs alignment
      (c :char)      ; 1 byte
      (d :double)))  ; 8 bytes, needs alignment
  
  (let ((layout (lib:get-struct-layout 'aligned-struct)))
    ;; Check alignment-based offsets
    (is (= (lib:struct-field-offset layout 'a) 0))
    (is (= (lib:struct-field-offset layout 'b) 4))  ; Aligned to 4
    (is (= (lib:struct-field-offset layout 'c) 8))
    (is (= (lib:struct-field-offset layout 'd) 16)) ; Aligned to 8
    ;; Total size should be aligned
    (is (= (lib:struct-layout-size layout) 24))))

(deftest test-union-definition
  "Test defining unions"
  (lib:define-c-union 'test-union
    '((i :int)
      (f :float)
      (c (:array :char 4))))
  
  (let ((layout (lib:get-struct-layout 'test-union)))
    (is (lib:struct-layout-p layout))
    ;; All fields at offset 0 (union)
    (is (= (lib:struct-field-offset layout 'i) 0))
    (is (= (lib:struct-field-offset layout 'f) 0))
    (is (= (lib:struct-field-offset layout 'c) 0))
    ;; Size is max of all fields
    (is (= (lib:struct-layout-size layout) 4))))

(deftest test-struct-serialization
  "Test converting structs to/from bytes"
  (lib:define-c-struct 'serializable
    '((magic :unsigned-int)
      (version :unsigned-char)
      (flags :unsigned-char)))
  
  (lib:with-c-struct (s 'serializable)
    ;; Set values
    (setf (lib:struct-ref s 'magic) #xDEADBEEF)
    (setf (lib:struct-ref s 'version) 1)
    (setf (lib:struct-ref s 'flags) #xFF)
    
    ;; Convert to bytes
    (let ((bytes (lib:struct-to-bytes s)))
      (is (typep bytes '(vector (unsigned-byte 8))))
      ;; Check magic number bytes (little-endian)
      (is (= (aref bytes 0) #xEF))
      (is (= (aref bytes 1) #xBE))
      (is (= (aref bytes 2) #xAD))
      (is (= (aref bytes 3) #xDE))
      ;; Check version
      (is (= (aref bytes 4) 1))
      ;; Check flags
      (is (= (aref bytes 5) #xFF)))))