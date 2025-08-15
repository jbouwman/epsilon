(defpackage epsilon.foreign.struct-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)
   (struct epsilon.foreign.struct)))

(in-package epsilon.foreign.struct-tests)

;;;; Tests for Phase 3: Struct Layout Discovery and Support

(deftest test-struct-layout-discovery
  "Test automatic discovery of C struct layouts"
  ;; Define a struct and discover its layout
  (struct:define-c-struct-auto 'timespec
    "struct timespec {
       time_t tv_sec;
       long   tv_nsec;
     };")
  
  (let ((layout (struct:get-struct-layout 'timespec)))
    (is (struct:struct-layout-p layout))
    (is (= (struct:struct-layout-size layout) 16)) ; On 64-bit
    (is (= (struct:struct-layout-alignment layout) 8))
    
    ;; Check field offsets
    (is (= (struct:struct-field-offset layout 'tv-sec) 0))
    (is (= (struct:struct-field-offset layout 'tv-nsec) 8))
    
    ;; Check field types
    (is (eq (struct:struct-field-type layout 'tv-sec) :time-t))
    (is (eq (struct:struct-field-type layout 'tv-nsec) :long))))

(deftest test-struct-creation-and-access
  "Test creating and accessing struct instances"
  ;; Define a simple point struct
  (struct:define-c-struct 'point
    '((x :int)
      (y :int)))
  
  ;; Create an instance
  (struct:with-c-struct (pt point)
    ;; Set values
    (setf (struct:struct-ref pt 'x) 10)
    (setf (struct:struct-ref pt 'y) 20)
    
    ;; Read values
    (is (= (struct:struct-ref pt 'x) 10))
    (is (= (struct:struct-ref pt 'y) 20))
    
    ;; Get pointer for passing to C
    (is (sb-sys:system-area-pointer-p (struct:struct-pointer pt)))))

(deftest test-nested-struct-support
  "Test structs containing other structs"
  ;; Define nested structs
  (struct:define-c-struct 'inner
    '((value :int)))
  
  (struct:define-c-struct 'outer
    '((id :int)
      (data (:struct inner))
      (count :int)))
  
  (struct:with-c-struct (obj outer)
    ;; Access nested fields
    (setf (struct:struct-ref obj 'id) 1)
    (setf (struct:struct-ref obj '(data value)) 42)
    (setf (struct:struct-ref obj 'count) 100)
    
    (is (= (struct:struct-ref obj 'id) 1))
    (is (= (struct:struct-ref obj '(data value)) 42))
    (is (= (struct:struct-ref obj 'count) 100))))

(deftest test-struct-array-fields
  "Test structs containing arrays"
  (struct:define-c-struct 'buffer-struct
    '((size :int)
      (data (:array :unsigned-char 256))))
  
  (struct:with-c-struct (buf buffer-struct)
    (setf (struct:struct-ref buf 'size) 10)
    
    ;; Set array elements
    (loop for i from 0 below 10
          do (setf (struct:struct-ref buf `(data ,i)) i))
    
    ;; Read array elements
    (loop for i from 0 below 10
          do (is (= (struct:struct-ref buf `(data ,i)) i)))))

(deftest test-struct-pointer-fields
  "Test structs containing pointers"
  (struct:define-c-struct 'node
    '((value :int)
      (next (:pointer (:struct node)))))
  
  (struct:with-c-struct (node1 node)
    (struct:with-c-struct (node2 node)
      (setf (struct:struct-ref node1 'value) 10)
      (setf (struct:struct-ref node2 'value) 20)
      
      ;; Link nodes
      (setf (struct:struct-ref node1 'next) (struct:struct-pointer node2))
      
      ;; Follow pointer
      (let ((next-ptr (struct:struct-ref node1 'next)))
        (when (not (sb-sys:sap= next-ptr (sb-sys:int-sap 0)))
          (is (= (struct:struct-ref-ptr next-ptr 'node 'value) 20)))))))

(deftest test-struct-bit-fields
  "Test struct bit fields"
  (struct:define-c-struct 'flags
    '((enabled :bit 1)
      (mode :bit 3)
      (reserved :bit 4)
      (data :unsigned-char)))
  
  (struct:with-c-struct (f flags)
    ;; Set bit fields
    (setf (struct:struct-ref f 'enabled) 1)
    (setf (struct:struct-ref f 'mode) 5)
    (setf (struct:struct-ref f 'data) 255)
    
    (is (= (struct:struct-ref f 'enabled) 1))
    (is (= (struct:struct-ref f 'mode) 5))
    (is (= (struct:struct-ref f 'data) 255))))

(deftest test-struct-union-support
  "Test C unions"
  (struct:define-c-union 'variant
    '((i :int)
      (f :float)
      (p :pointer)))
  
  (struct:with-c-union (v 'variant)
    ;; Set as integer
    (setf (struct:union-ref v 'i) 42)
    (is (= (struct:union-ref v 'i) 42))
    
    ;; Set as float (overwrites integer)
    (setf (struct:union-ref v 'f) 3.14)
    (is (< (abs (- (struct:union-ref v 'f) 3.14)) 0.001))
    
    ;; The integer view is now garbage
    (is (not (= (struct:union-ref v 'i) 42)))))

(deftest test-struct-alignment-and-padding
  "Test that struct alignment and padding are correct"
  ;; This struct should have padding
  (struct:define-c-struct 'padded
    '((a :char)      ; offset 0, size 1
      (b :int)       ; offset 4 (padded), size 4
      (c :char)      ; offset 8, size 1
      (d :double)))  ; offset 16 (padded), size 8
  
  (let ((layout (struct:get-struct-layout 'padded)))
    ;; Check offsets account for alignment
    (is (= (struct:struct-field-offset layout 'a) 0))
    (is (= (struct:struct-field-offset layout 'b) 4))  ; Aligned to 4
    (is (= (struct:struct-field-offset layout 'c) 8))
    (is (= (struct:struct-field-offset layout 'd) 16)) ; Aligned to 8
    (is (= (struct:struct-layout-size layout) 24))))   ; Total size with padding

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
    
    (struct:parse-c-struct 'person header)
    
    (let ((layout (struct:get-struct-layout 'person)))
      (is (struct:struct-layout-p layout))
      ;; Check parsed fields
      (is (struct:struct-has-field-p layout 'name))
      (is (struct:struct-has-field-p layout 'age))
      (is (struct:struct-has-field-p layout 'height))
      (is (struct:struct-has-field-p layout 'addr))
      
      ;; Check nested struct
      (is (eq (struct:struct-field-type layout 'addr) '(:struct address))))))

(deftest test-struct-zero-copy-access
  "Test zero-copy struct access from foreign memory"
  ;; Simulate getting a struct pointer from C
  (let* ((size 16)
         (foreign-mem (struct:foreign-alloc size)))
    (unwind-protect
         (progn
           ;; Write some test data
           (setf (sb-sys:sap-ref-32 foreign-mem 0) 42)    ; First int
           (setf (sb-sys:sap-ref-32 foreign-mem 4) 100)   ; Second int
           (setf (sb-sys:sap-ref-64 foreign-mem 8) 999)   ; Long
           
           ;; Define struct matching this layout
           (struct:define-c-struct 'test-struct
             '((a :int)
               (b :int)
               (c :long)))
           
           ;; Create zero-copy view
           (struct:with-struct-view (s foreign-mem 'test-struct)
             (is (= (struct:struct-ref s 'a) 42))
             (is (= (struct:struct-ref s 'b) 100))
             (is (= (struct:struct-ref s 'c) 999))
             
             ;; Modifications should affect original memory
             (setf (struct:struct-ref s 'a) 84))
           
           ;; Verify change in original memory
           (is (= (sb-sys:sap-ref-32 foreign-mem 0) 84)))
      (struct:foreign-free foreign-mem))))

(deftest test-struct-as-function-argument
  "Test passing structs to C functions"
  ;; Define a struct that C functions expect
  (struct:define-c-struct 'stat-struct
    '((st-dev :dev-t)
      (st-ino :ino-t)
      (st-mode :mode-t)
      (st-size :off-t)))
  
  ;; Define stat function that takes struct pointer
  (lib:defshared stat-func "stat" "libc" :int
    (path :string)
    (buf (:pointer (:struct stat-struct))))
  
  ;; Use it
  (struct:with-c-struct (statbuf stat-struct)
    (let ((result (stat-func "/tmp" (struct:struct-pointer statbuf))))
      (when (zerop result)
        ;; Successfully got file stats
        (is (> (struct:struct-ref statbuf 'st-size) 0))
        (is (> (struct:struct-ref statbuf 'st-mode) 0))))))

(deftest test-struct-as-return-value
  "Test receiving structs from C functions"
  ;; Some C functions return struct pointers
  (lib:defshared localtime-func "localtime" "libc" 
    (:pointer (:struct tm))
    (timep (:pointer :time-t)))
  
  ;; Define the tm struct
  (struct:define-c-struct 'tm
    '((tm-sec :int)
      (tm-min :int)
      (tm-hour :int)
      (tm-mday :int)
      (tm-mon :int)
      (tm-year :int)))
  
  ;; Get current time
  (struct:with-foreign-object (time-val :time-t)
    (lib:defshared time-func "time" "libc" :time-t 
      (tloc (:pointer :time-t)))
    (time-func time-val)
    
    ;; Convert to struct tm
    (let ((tm-ptr (localtime-func time-val)))
      (when (not (sb-sys:sap= tm-ptr (sb-sys:int-sap 0)))
        ;; Read fields from returned struct
        (is (>= (struct:struct-ref-ptr tm-ptr 'tm 'tm-year) 100)) ; Years since 1900
        (is (>= (struct:struct-ref-ptr tm-ptr 'tm 'tm-mon) 0))
        (is (<= (struct:struct-ref-ptr tm-ptr 'tm 'tm-mon) 11))))))

(deftest test-struct-serialization
  "Test struct serialization and deserialization"
  (struct:define-c-struct 'data-packet
    '((version :unsigned-int)
      (flags :unsigned-int)
      (payload (:array :unsigned-char 64))))
  
  (struct:with-c-struct (packet data-packet)
    ;; Set some data
    (setf (struct:struct-ref packet 'version) 1)
    (setf (struct:struct-ref packet 'flags) #x1234)
    (loop for i from 0 below 10
          do (setf (struct:struct-ref packet `(payload ,i)) i))
    
    ;; Serialize to bytes
    (let ((bytes (struct:struct-to-bytes packet)))
      (is (typep bytes '(vector (unsigned-byte 8))))
      (is (>= (length bytes) 72)) ; At least version + flags + some payload
      
      ;; Deserialize back
      (struct:with-c-struct (packet2 data-packet)
        (struct:bytes-to-struct bytes packet2)
        (is (= (struct:struct-ref packet2 'version) 1))
        (is (= (struct:struct-ref packet2 'flags) #x1234))
        (loop for i from 0 below 10
              do (is (= (struct:struct-ref packet2 `(payload ,i)) i)))))))

(deftest test-struct-pretty-printing
  "Test pretty printing of struct contents"
  (struct:define-c-struct 'point3d
    '((x :float)
      (y :float)
      (z :float)))
  
  (struct:with-c-struct (pt point3d)
    (setf (struct:struct-ref pt 'x) 1.0)
    (setf (struct:struct-ref pt 'y) 2.0)
    (setf (struct:struct-ref pt 'z) 3.0)
    
    ;; Should produce readable representation
    (let ((str (struct:struct-to-string pt)))
      (is (stringp str))
      (is (search "x" str))
      (is (search "1.0" str))
      (is (search "y" str))
      (is (search "2.0" str)))))