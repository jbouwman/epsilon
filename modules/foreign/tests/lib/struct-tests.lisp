(defpackage epsilon.foreign.struct-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)))

(in-package epsilon.foreign.struct-tests)

;;;; Tests for Phase 3: Struct Layout Discovery and Support

(deftest test-struct-layout-discovery
  "Test automatic discovery of C struct layouts"
  ;; Define a struct and discover its layout
  (lib:define-c-struct-auto 'timespec
    "struct timespec {
       time_t tv_sec;
       long   tv_nsec;
     };")
  
  (let ((layout (lib:get-struct-layout 'timespec)))
    (is (lib:struct-layout-p layout))
    (is (= (lib:struct-layout-size layout) 16)) ; On 64-bit
    (is (= (lib:struct-layout-alignment layout) 8))
    
    ;; Check field offsets
    (is (= (lib:struct-field-offset layout 'tv-sec) 0))
    (is (= (lib:struct-field-offset layout 'tv-nsec) 8))
    
    ;; Check field types
    (is (eq (lib:struct-field-type layout 'tv-sec) :time-t))
    (is (eq (lib:struct-field-type layout 'tv-nsec) :long))))

(deftest test-struct-creation-and-access
  "Test creating and accessing struct instances"
  ;; Define a simple point struct
  (lib:define-c-struct 'point
    '((x :int)
      (y :int)))
  
  ;; Create an instance
  (lib:with-c-struct (pt 'point)
    ;; Set values
    (setf (lib:struct-ref pt 'x) 10)
    (setf (lib:struct-ref pt 'y) 20)
    
    ;; Read values
    (is (= (lib:struct-ref pt 'x) 10))
    (is (= (lib:struct-ref pt 'y) 20))
    
    ;; Get pointer for passing to C
    (is (sb-sys:system-area-pointer-p (lib:struct-pointer pt)))))

(deftest test-nested-struct-support
  "Test structs containing other structs"
  ;; Define nested structs
  (lib:define-c-struct 'inner
    '((value :int)))
  
  (lib:define-c-struct 'outer
    '((id :int)
      (data (:struct inner))
      (count :int)))
  
  (lib:with-c-struct (obj 'outer)
    ;; Access nested fields
    (setf (lib:struct-ref obj 'id) 1)
    (setf (lib:struct-ref obj '(data value)) 42)
    (setf (lib:struct-ref obj 'count) 100)
    
    (is (= (lib:struct-ref obj 'id) 1))
    (is (= (lib:struct-ref obj '(data value)) 42))
    (is (= (lib:struct-ref obj 'count) 100))))

(deftest test-struct-array-fields
  "Test structs containing arrays"
  (lib:define-c-struct 'buffer-struct
    '((size :int)
      (data (:array :unsigned-char 256))))
  
  (lib:with-c-struct (buf 'buffer-struct)
    (setf (lib:struct-ref buf 'size) 10)
    
    ;; Set array elements
    (loop for i from 0 below 10
          do (setf (lib:struct-ref buf `(data ,i)) i))
    
    ;; Read array elements
    (loop for i from 0 below 10
          do (is (= (lib:struct-ref buf `(data ,i)) i)))))

(deftest test-struct-pointer-fields
  "Test structs containing pointers"
  (lib:define-c-struct 'node
    '((value :int)
      (next (:pointer (:struct node)))))
  
  (lib:with-c-struct (node1 'node)
    (lib:with-c-struct (node2 'node)
      (setf (lib:struct-ref node1 'value) 10)
      (setf (lib:struct-ref node2 'value) 20)
      
      ;; Link nodes
      (setf (lib:struct-ref node1 'next) (lib:struct-pointer node2))
      
      ;; Follow pointer
      (let ((next-ptr (lib:struct-ref node1 'next)))
        (when (not (sb-sys:sap= next-ptr (sb-sys:int-sap 0)))
          (is (= (lib:struct-ref-ptr next-ptr 'node 'value) 20)))))))

(deftest test-struct-bit-fields
  "Test struct bit fields"
  (lib:define-c-struct 'flags
    '((enabled :bit 1)
      (mode :bit 3)
      (reserved :bit 4)
      (data :unsigned-char)))
  
  (lib:with-c-struct (f 'flags)
    ;; Set bit fields
    (setf (lib:struct-ref f 'enabled) 1)
    (setf (lib:struct-ref f 'mode) 5)
    (setf (lib:struct-ref f 'data) 255)
    
    (is (= (lib:struct-ref f 'enabled) 1))
    (is (= (lib:struct-ref f 'mode) 5))
    (is (= (lib:struct-ref f 'data) 255))))

(deftest test-struct-union-support
  "Test C unions"
  (lib:define-c-union 'variant
    '((i :int)
      (f :float)
      (p :pointer)))
  
  (lib:with-c-union (v 'variant)
    ;; Set as integer
    (setf (lib:union-ref v 'i) 42)
    (is (= (lib:union-ref v 'i) 42))
    
    ;; Set as float (overwrites integer)
    (setf (lib:union-ref v 'f) 3.14)
    (is (< (abs (- (lib:union-ref v 'f) 3.14)) 0.001))
    
    ;; The integer view is now garbage
    (is (not (= (lib:union-ref v 'i) 42)))))

(deftest test-struct-alignment-and-padding
  "Test that struct alignment and padding are correct"
  ;; This struct should have padding
  (lib:define-c-struct 'padded
    '((a :char)      ; offset 0, size 1
      (b :int)       ; offset 4 (padded), size 4
      (c :char)      ; offset 8, size 1
      (d :double)))  ; offset 16 (padded), size 8
  
  (let ((layout (lib:get-struct-layout 'padded)))
    ;; Check offsets account for alignment
    (is (= (lib:struct-field-offset layout 'a) 0))
    (is (= (lib:struct-field-offset layout 'b) 4))  ; Aligned to 4
    (is (= (lib:struct-field-offset layout 'c) 8))
    (is (= (lib:struct-field-offset layout 'd) 16)) ; Aligned to 8
    (is (= (lib:struct-layout-size layout) 24))))   ; Total size with padding

(deftest test-struct-from-c-header
  "Test parsing struct definitions from C headers"
  (let ((header "
    struct person {
        char name[32];
        int age;
        float height;
        struct address {
            char street[64];
            int number;
        } addr;
    };"))
    
    (lib:parse-c-struct 'person header)
    
    (let ((layout (lib:get-struct-layout 'person)))
      (is (lib:struct-layout-p layout))
      ;; Check parsed fields
      (is (lib:struct-has-field-p layout 'name))
      (is (lib:struct-has-field-p layout 'age))
      (is (lib:struct-has-field-p layout 'height))
      (is (lib:struct-has-field-p layout 'addr))
      
      ;; Check nested struct
      (is (eq (lib:struct-field-type layout 'addr) '(:struct address))))))

(deftest test-struct-zero-copy-access
  "Test zero-copy struct access from foreign memory"
  ;; Simulate getting a struct pointer from C
  (let* ((size 16)
         (foreign-mem (lib:foreign-alloc size)))
    (unwind-protect
         (progn
           ;; Write some test data
           (setf (sb-sys:sap-ref-32 foreign-mem 0) 42)    ; First int
           (setf (sb-sys:sap-ref-32 foreign-mem 4) 100)   ; Second int
           (setf (sb-sys:sap-ref-64 foreign-mem 8) 999)   ; Long
           
           ;; Define struct matching this layout
           (lib:define-c-struct 'test-struct
             '((a :int)
               (b :int)
               (c :long)))
           
           ;; Create zero-copy view
           (lib:with-struct-view (s foreign-mem 'test-struct)
             (is (= (lib:struct-ref s 'a) 42))
             (is (= (lib:struct-ref s 'b) 100))
             (is (= (lib:struct-ref s 'c) 999))
             
             ;; Modifications should affect original memory
             (setf (lib:struct-ref s 'a) 84))
           
           ;; Verify change in original memory
           (is (= (sb-sys:sap-ref-32 foreign-mem 0) 84)))
      (lib:foreign-free foreign-mem))))

(deftest test-struct-as-function-argument
  "Test passing structs to C functions"
  ;; Define a struct that C functions expect
  (lib:define-c-struct 'stat-struct
    '((st-dev :dev-t)
      (st-ino :ino-t)
      (st-mode :mode-t)
      (st-size :off-t)))
  
  ;; Define stat function that takes struct pointer
  (lib:defshared stat-func "stat" "libc" :int
    (path :string)
    (buf (:pointer (:struct stat-struct))))
  
  ;; Use it
  (lib:with-c-struct (statbuf 'stat-struct)
    (let ((result (stat-func "/tmp" (lib:struct-pointer statbuf))))
      (when (zerop result)
        ;; Successfully got file stats
        (is (> (lib:struct-ref statbuf 'st-size) 0))
        (is (> (lib:struct-ref statbuf 'st-mode) 0))))))

(deftest test-struct-as-return-value
  "Test receiving structs from C functions"
  ;; Some C functions return struct pointers
  (lib:defshared localtime-func "localtime" "libc" 
    (:pointer (:struct tm))
    (timep (:pointer :time-t)))
  
  ;; Define the tm struct
  (lib:define-c-struct 'tm
    '((tm-sec :int)
      (tm-min :int)
      (tm-hour :int)
      (tm-mday :int)
      (tm-mon :int)
      (tm-year :int)))
  
  ;; Get current time
  (lib:with-foreign-object (time-val :time-t)
    (lib:defshared time-func "time" "libc" :time-t 
      (tloc (:pointer :time-t)))
    (time-func time-val)
    
    ;; Convert to struct tm
    (let ((tm-ptr (localtime-func time-val)))
      (when (not (sb-sys:sap= tm-ptr (sb-sys:int-sap 0)))
        ;; Read fields from returned struct
        (is (>= (lib:struct-ref-ptr tm-ptr 'tm 'tm-year) 100)) ; Years since 1900
        (is (>= (lib:struct-ref-ptr tm-ptr 'tm 'tm-mon) 0))
        (is (<= (lib:struct-ref-ptr tm-ptr 'tm 'tm-mon) 11))))))

(deftest test-struct-serialization
  "Test struct serialization and deserialization"
  (lib:define-c-struct 'data-packet
    '((version :unsigned-int)
      (flags :unsigned-int)
      (payload (:array :unsigned-char 64))))
  
  (lib:with-c-struct (packet 'data-packet)
    ;; Set some data
    (setf (lib:struct-ref packet 'version) 1)
    (setf (lib:struct-ref packet 'flags) #x1234)
    (loop for i from 0 below 10
          do (setf (lib:struct-ref packet `(payload ,i)) i))
    
    ;; Serialize to bytes
    (let ((bytes (lib:struct-to-bytes packet)))
      (is (typep bytes '(vector (unsigned-byte 8))))
      (is (>= (length bytes) 72)) ; At least version + flags + some payload
      
      ;; Deserialize back
      (lib:with-c-struct (packet2 'data-packet)
        (lib:bytes-to-struct bytes packet2)
        (is (= (lib:struct-ref packet2 'version) 1))
        (is (= (lib:struct-ref packet2 'flags) #x1234))
        (loop for i from 0 below 10
              do (is (= (lib:struct-ref packet2 `(payload ,i)) i)))))))

(deftest test-struct-pretty-printing
  "Test pretty printing of struct contents"
  (lib:define-c-struct 'point3d
    '((x :float)
      (y :float)
      (z :float)))
  
  (lib:with-c-struct (pt 'point3d)
    (setf (lib:struct-ref pt 'x) 1.0)
    (setf (lib:struct-ref pt 'y) 2.0)
    (setf (lib:struct-ref pt 'z) 3.0)
    
    ;; Should produce readable representation
    (let ((str (lib:struct-to-string pt)))
      (is (stringp str))
      (is (search "x" str))
      (is (search "1.0" str))
      (is (search "y" str))
      (is (search "2.0" str)))))