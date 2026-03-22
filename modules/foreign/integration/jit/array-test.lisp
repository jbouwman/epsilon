;;;; array-test.lisp - Tests for array/buffer passing support

(defpackage :epsilon.foreign.jit.array.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (:jit :epsilon.foreign.jit)
   (:arr :epsilon.foreign.jit.array))
   (:enter t))

;;; ============================================================================
;;; Type Mapping Tests
;;; ============================================================================

(deftest c-type-to-lisp-element-type-integers
  "c-type-to-lisp-element-type works for integer types"
  (assert-equal '(signed-byte 8) (arr:c-type-to-lisp-element-type :char))
  (assert-equal '(unsigned-byte 8) (arr:c-type-to-lisp-element-type :uchar))
  (assert-equal '(unsigned-byte 8) (arr:c-type-to-lisp-element-type :unsigned-char))
  (assert-equal '(signed-byte 16) (arr:c-type-to-lisp-element-type :short))
  (assert-equal '(unsigned-byte 16) (arr:c-type-to-lisp-element-type :ushort))
  (assert-equal '(signed-byte 32) (arr:c-type-to-lisp-element-type :int))
  (assert-equal '(unsigned-byte 32) (arr:c-type-to-lisp-element-type :uint))
  (assert-equal '(signed-byte 64) (arr:c-type-to-lisp-element-type :long))
  (assert-equal '(unsigned-byte 64) (arr:c-type-to-lisp-element-type :ulong)))

(deftest c-type-to-lisp-element-type-floats
  "c-type-to-lisp-element-type works for float types"
  (assert-equal 'single-float (arr:c-type-to-lisp-element-type :float))
  (assert-equal 'double-float (arr:c-type-to-lisp-element-type :double)))

(deftest c-type-to-lisp-element-type-pointer
  "c-type-to-lisp-element-type works for pointer type"
  (assert-equal '(unsigned-byte 64) (arr:c-type-to-lisp-element-type :pointer)))

(deftest c-type-to-lisp-element-type-unknown
  "c-type-to-lisp-element-type signals error for unknown type"
  (assert-condition (error) (arr:c-type-to-lisp-element-type :unknown-type)))

(deftest lisp-element-type-to-c-type-integers
  "lisp-element-type-to-c-type works for integer types"
  (assert-equal :char (arr:lisp-element-type-to-c-type '(signed-byte 8)))
  (assert-equal :uchar (arr:lisp-element-type-to-c-type '(unsigned-byte 8)))
  (assert-equal :short (arr:lisp-element-type-to-c-type '(signed-byte 16)))
  (assert-equal :int (arr:lisp-element-type-to-c-type '(signed-byte 32)))
  (assert-equal :long (arr:lisp-element-type-to-c-type '(signed-byte 64))))

(deftest lisp-element-type-to-c-type-floats
  "lisp-element-type-to-c-type works for float types"
  (assert-equal :float (arr:lisp-element-type-to-c-type 'single-float))
  (assert-equal :double (arr:lisp-element-type-to-c-type 'double-float)))

(deftest element-size-correct
  "element-size returns correct sizes"
  (assert-equal 1 (arr:element-size :char))
  (assert-equal 1 (arr:element-size :uchar))
  (assert-equal 2 (arr:element-size :short))
  (assert-equal 2 (arr:element-size :ushort))
  (assert-equal 4 (arr:element-size :int))
  (assert-equal 4 (arr:element-size :uint))
  (assert-equal 8 (arr:element-size :long))
  (assert-equal 8 (arr:element-size :ulong))
  (assert-equal 4 (arr:element-size :float))
  (assert-equal 8 (arr:element-size :double))
  (assert-equal 8 (arr:element-size :pointer)))

;;; ============================================================================
;;; C-Array Allocation Tests
;;; ============================================================================

(deftest make-c-array-double
  "make-c-array creates double-float arrays"
  (let ((arr (arr:make-c-array :double 10)))
    (assert-true (arrayp arr))
    (assert-equal 10 (length arr))
    (assert-equal 'double-float (array-element-type arr))))

(deftest make-c-array-int
  "make-c-array creates int arrays"
  (let ((arr (arr:make-c-array :int 10)))
    (assert-true (arrayp arr))
    (assert-equal 10 (length arr))
    (assert-equal '(signed-byte 32) (array-element-type arr))))

(deftest make-c-array-with-initial-element
  "make-c-array supports initial-element"
  (let ((arr (arr:make-c-array :double 5 :initial-element 3.14d0)))
    (assert-equal 5 (length arr))
    (dotimes (i 5)
      (assert-equal 3.14d0 (aref arr i)))))

(deftest make-c-array-uchar
  "make-c-array creates unsigned byte arrays"
  (let ((arr (arr:make-c-array :uchar 256 :initial-element 0)))
    (assert-equal 256 (length arr))
    (assert-equal '(unsigned-byte 8) (array-element-type arr))))

(deftest c-array-length-works
  "c-array-length returns array length"
  (let ((arr (arr:make-c-array :double 42)))
    (assert-equal 42 (arr:c-array-length arr))))

;;; ============================================================================
;;; Array Pinning Tests
;;; ============================================================================

(deftest with-pinned-array-basic
  "with-pinned-array provides valid pointer"
  (let ((data (arr:make-c-array :double 10 :initial-element 0.0d0)))
    (arr:with-pinned-array (ptr data :element-type :double)
      ;; ptr should be an integer (address)
      (assert-true (integerp ptr))
      (assert-true (> ptr 0)))))

(deftest with-pinned-array-access-data
  "with-pinned-array allows reading/writing via SAP"
  (let ((data (arr:make-c-array :double 5 :initial-element 0.0d0)))
    ;; Write values via Lisp
    (setf (aref data 0) 1.0d0)
    (setf (aref data 1) 2.0d0)
    (setf (aref data 2) 3.0d0)
    (arr:with-pinned-array (ptr data :element-type :double)
      ;; Read via SAP
      (let ((sap (sb-sys:int-sap ptr)))
        (assert-equal 1.0d0 (sb-sys:sap-ref-double sap 0))
        (assert-equal 2.0d0 (sb-sys:sap-ref-double sap 8))
        (assert-equal 3.0d0 (sb-sys:sap-ref-double sap 16))))))

(deftest with-pinned-array-integer-data
  "with-pinned-array works with integer arrays"
  (let ((data (arr:make-c-array :int 3)))
    (setf (aref data 0) 100)
    (setf (aref data 1) 200)
    (setf (aref data 2) 300)
    (arr:with-pinned-array (ptr data :element-type :int)
      (let ((sap (sb-sys:int-sap ptr)))
        (assert-equal 100 (sb-sys:signed-sap-ref-32 sap 0))
        (assert-equal 200 (sb-sys:signed-sap-ref-32 sap 4))
        (assert-equal 300 (sb-sys:signed-sap-ref-32 sap 8))))))

(deftest with-pinned-arrays-multiple
  "with-pinned-arrays pins multiple arrays"
  (let ((arr1 (arr:make-c-array :double 5 :initial-element 1.0d0))
        (arr2 (arr:make-c-array :int 5 :initial-element 42)))
    (arr:with-pinned-arrays ((ptr1 arr1 :element-type :double)
                             (ptr2 arr2 :element-type :int))
      (assert-true (integerp ptr1))
      (assert-true (integerp ptr2))
      (assert-true (not (= ptr1 ptr2)))
      ;; Both should be valid
      (assert-true (> ptr1 0))
      (assert-true (> ptr2 0)))))

;;; ============================================================================
;;; Output Array Tests
;;; ============================================================================

(deftest with-output-array-basic
  "with-output-array creates and pins array"
  (arr:with-output-array (ptr results 10 :element-type :double :initial-element 0.0d0)
    ;; ptr should be valid
    (assert-true (integerp ptr))
    (assert-true (> ptr 0))
    ;; results should be the array
    (assert-true (arrayp results))
    (assert-equal 10 (length results))))

(deftest with-output-array-receives-data
  "with-output-array array contains data written via pointer"
  (arr:with-output-array (ptr results 5 :element-type :double :initial-element 0.0d0)
    ;; Write via SAP
    (let ((sap (sb-sys:int-sap ptr)))
      (setf (sb-sys:sap-ref-double sap 0) 1.0d0)
      (setf (sb-sys:sap-ref-double sap 8) 2.0d0)
      (setf (sb-sys:sap-ref-double sap 16) 3.0d0))
    ;; Read via array
    (assert-equal 1.0d0 (aref results 0))
    (assert-equal 2.0d0 (aref results 1))
    (assert-equal 3.0d0 (aref results 2))))

;;; ============================================================================
;;; Array Copy Utilities Tests
;;; ============================================================================

(deftest copy-to-c-array-works
  "copy-to-c-array copies elements"
  (let ((source '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0))
        (dest (arr:make-c-array :double 5)))
    (arr:copy-to-c-array source dest)
    (assert-equal 1.0d0 (aref dest 0))
    (assert-equal 2.0d0 (aref dest 1))
    (assert-equal 3.0d0 (aref dest 2))
    (assert-equal 4.0d0 (aref dest 3))
    (assert-equal 5.0d0 (aref dest 4))))

(deftest copy-to-c-array-with-range
  "copy-to-c-array respects start/end"
  (let ((source '(10 20 30 40 50))
        (dest (arr:make-c-array :int 3)))
    (arr:copy-to-c-array source dest :start 1 :end 4)
    (assert-equal 20 (aref dest 0))
    (assert-equal 30 (aref dest 1))
    (assert-equal 40 (aref dest 2))))

(deftest copy-from-c-array-works
  "copy-from-c-array copies elements"
  (let ((source (arr:make-c-array :double 3))
        (dest (make-list 3 :initial-element 0.0d0)))
    (setf (aref source 0) 1.5d0)
    (setf (aref source 1) 2.5d0)
    (setf (aref source 2) 3.5d0)
    (arr:copy-from-c-array source dest)
    (assert-equal 1.5d0 (first dest))
    (assert-equal 2.5d0 (second dest))
    (assert-equal 3.5d0 (third dest))))

(deftest zero-c-array-doubles
  "zero-c-array zeros double arrays"
  (let ((arr (arr:make-c-array :double 5 :initial-element 99.0d0)))
    (arr:zero-c-array arr)
    (dotimes (i 5)
      (assert-equal 0.0d0 (aref arr i)))))

(deftest zero-c-array-integers
  "zero-c-array zeros integer arrays"
  (let ((arr (arr:make-c-array :int 5 :initial-element 999)))
    (arr:zero-c-array arr)
    (dotimes (i 5)
      (assert-equal 0 (aref arr i)))))

;;; ============================================================================
;;; SAP-based Memory Access Tests
;;; ============================================================================

(deftest read-write-c-array-element-double
  "read/write-c-array-element works for doubles"
  (let ((arr (arr:make-c-array :double 3)))
    (arr:with-pinned-array (ptr arr :element-type :double)
      (epsilon.foreign.jit.array::write-c-array-element ptr 0 :double 1.5d0)
      (epsilon.foreign.jit.array::write-c-array-element ptr 1 :double 2.5d0)
      (epsilon.foreign.jit.array::write-c-array-element ptr 2 :double 3.5d0)
      (assert-equal 1.5d0 (epsilon.foreign.jit.array::read-c-array-element ptr 0 :double))
      (assert-equal 2.5d0 (epsilon.foreign.jit.array::read-c-array-element ptr 1 :double))
      (assert-equal 3.5d0 (epsilon.foreign.jit.array::read-c-array-element ptr 2 :double)))))

(deftest read-write-c-array-element-int
  "read/write-c-array-element works for ints"
  (let ((arr (arr:make-c-array :int 3)))
    (arr:with-pinned-array (ptr arr :element-type :int)
      (epsilon.foreign.jit.array::write-c-array-element ptr 0 :int 100)
      (epsilon.foreign.jit.array::write-c-array-element ptr 1 :int -200)
      (epsilon.foreign.jit.array::write-c-array-element ptr 2 :int 300)
      (assert-equal 100 (epsilon.foreign.jit.array::read-c-array-element ptr 0 :int))
      (assert-equal -200 (epsilon.foreign.jit.array::read-c-array-element ptr 1 :int))
      (assert-equal 300 (epsilon.foreign.jit.array::read-c-array-element ptr 2 :int)))))

(deftest read-write-c-array-element-uchar
  "read/write-c-array-element works for unsigned chars"
  (let ((arr (arr:make-c-array :uchar 4)))
    (arr:with-pinned-array (ptr arr :element-type :uchar)
      (epsilon.foreign.jit.array::write-c-array-element ptr 0 :uchar 0)
      (epsilon.foreign.jit.array::write-c-array-element ptr 1 :uchar 127)
      (epsilon.foreign.jit.array::write-c-array-element ptr 2 :uchar 128)
      (epsilon.foreign.jit.array::write-c-array-element ptr 3 :uchar 255)
      (assert-equal 0 (epsilon.foreign.jit.array::read-c-array-element ptr 0 :uchar))
      (assert-equal 127 (epsilon.foreign.jit.array::read-c-array-element ptr 1 :uchar))
      (assert-equal 128 (epsilon.foreign.jit.array::read-c-array-element ptr 2 :uchar))
      (assert-equal 255 (epsilon.foreign.jit.array::read-c-array-element ptr 3 :uchar)))))

;;; ============================================================================
;;; Integration Tests - Use with actual C functions
;;; ============================================================================

(defun get-foreign-symbol-address (name)
  "Get the address of a foreign symbol as an integer."
  (let ((result (sb-sys:find-foreign-symbol-address name)))
    (etypecase result
      (integer result)
      (sb-sys:system-area-pointer (sb-sys:sap-int result))
      (null nil))))

(deftest integration-memcpy-with-pinned-arrays
  "can use pinned arrays with memcpy"
  (when (jit:platform-supported-p)
    (let ((memcpy-addr (get-foreign-symbol-address "memcpy")))
      (when memcpy-addr
        (let ((src (arr:make-c-array :uchar 10))
              (dst (arr:make-c-array :uchar 10 :initial-element 0)))
          ;; Initialize source
          (dotimes (i 10)
            (setf (aref src i) (+ i 65)))  ; A, B, C, ...
          ;; Call memcpy
          (arr:with-pinned-arrays ((src-ptr src :element-type :uchar)
                                   (dst-ptr dst :element-type :uchar))
            (let ((memcpy-fn (jit:make-jit-caller memcpy-addr :pointer
                                                  '(:pointer :pointer :ulong))))
              (funcall memcpy-fn dst-ptr src-ptr 10)))
          ;; Verify copy
          (dotimes (i 10)
            (assert-equal (aref src i) (aref dst i))))))))

(deftest integration-memset-with-pinned-array
  "can use pinned array with memset"
  (when (jit:platform-supported-p)
    (let ((memset-addr (get-foreign-symbol-address "memset")))
      (when memset-addr
        (let ((arr (arr:make-c-array :uchar 20 :initial-element 0)))
          (arr:with-pinned-array (ptr arr :element-type :uchar)
            (let ((memset-fn (jit:make-jit-caller memset-addr :pointer
                                                  '(:pointer :int :ulong))))
              (funcall memset-fn ptr 42 20)))
          ;; All bytes should be 42
          (dotimes (i 20)
            (assert-equal 42 (aref arr i))))))))

(deftest integration-qsort-with-pinned-array
  "can use pinned array with qsort"
  (when (jit:platform-supported-p)
    (let ((qsort-addr (get-foreign-symbol-address "qsort")))
      (when qsort-addr
        ;; Create array of ints to sort
        (let ((arr (arr:make-c-array :int 5)))
          (setf (aref arr 0) 50)
          (setf (aref arr 1) 10)
          (setf (aref arr 2) 40)
          (setf (aref arr 3) 20)
          (setf (aref arr 4) 30)
          ;; Get int comparison callback
          ;; Note: This test requires the callback module
          ;; For now, just verify we can pass the pinned array
          (arr:with-pinned-array (ptr arr :element-type :int)
            (assert-true (integerp ptr))
            (assert-true (> ptr 0))))))))
