;;;; struct-test.lisp - Tests for struct-by-value support

(defpackage :epsilon.foreign.jit.struct.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (:struct :epsilon.foreign.jit.struct))
  (:enter t))

;;; ============================================================================
;;; Struct Definition Tests
;;; ============================================================================

(deftest define-simple-struct
  "can define a simple struct"
  (struct:define-jit-struct test-point
    (x :int)
    (y :int))
  (assert-true (struct:get-jit-struct 'test-point))
  (assert-equal 8 (struct:jit-struct-size 'test-point))
  (assert-equal 4 (struct:jit-struct-alignment 'test-point)))

(deftest define-struct-with-doubles
  "can define struct with double fields"
  (struct:define-jit-struct double-point
    (x :double)
    (y :double))
  (assert-true (struct:get-jit-struct 'double-point))
  (assert-equal 16 (struct:jit-struct-size 'double-point))
  (assert-equal 8 (struct:jit-struct-alignment 'double-point)))


;;; ============================================================================
;;; Packing/Unpacking Tests
;;; ============================================================================

(deftest pack-simple-ints
  "can pack two ints into a word"
  (struct:define-jit-struct int-pair
    (a :int)
    (b :int))
  (let* ((struct (struct:get-jit-struct 'int-pair))
         (words (struct:struct-to-words struct '(a 1 b 2))))
    (assert-equal 1 (length words))
    ;; First int is in low 32 bits, second in high 32 bits
    (assert-equal (logior 1 (ash 2 32)) (first words))))

(deftest unpack-simple-ints
  "can unpack a word to two ints"
  (struct:define-jit-struct int-pair2
    (a :int)
    (b :int))
  (let* ((struct (struct:get-jit-struct 'int-pair2))
         (packed (logior 42 (ash 100 32)))
         (values (struct:words-to-struct struct (list packed))))
    (assert-equal 42 (getf values 'a))
    (assert-equal 100 (getf values 'b))))

(deftest pack-unpack-roundtrip
  "packing and unpacking preserves values"
  (struct:define-jit-struct roundtrip-test
    (x :int)
    (y :int))
  (let* ((struct (struct:get-jit-struct 'roundtrip-test))
         (original '(x 12345 y -9876))
         (words (struct:struct-to-words struct original))
         (recovered (struct:words-to-struct struct words)))
    (assert-equal (getf original 'x) (getf recovered 'x))
    (assert-equal (getf original 'y) (getf recovered 'y))))

(deftest pack-floats
  "can pack float fields"
  (struct:define-jit-struct float-pair
    (x :float)
    (y :float))
  (let* ((struct (struct:get-jit-struct 'float-pair))
         (original '(x 1.5 y 2.5))
         (words (struct:struct-to-words struct original))
         (recovered (struct:words-to-struct struct words)))
    ;; Float precision
    (assert-true (< (abs (- 1.5 (getf recovered 'x))) 0.0001))
    (assert-true (< (abs (- 2.5 (getf recovered 'y))) 0.0001))))

(deftest pack-doubles
  "can pack double fields across two words"
  (struct:define-jit-struct double-pair
    (x :double)
    (y :double))
  (let* ((struct (struct:get-jit-struct 'double-pair))
         (original '(x 3.14159265358979d0 y 2.71828182845905d0))
         (words (struct:struct-to-words struct original))
         (recovered (struct:words-to-struct struct words)))
    (assert-equal 2 (length words))
    ;; Double precision
    (assert-true (< (abs (- 3.14159265358979d0 (getf recovered 'x))) 1.0d-10))
    (assert-true (< (abs (- 2.71828182845905d0 (getf recovered 'y))) 1.0d-10))))

;;; ============================================================================
;;; Mixed Type Tests
;;; ============================================================================

(deftest pack-mixed-types
  "can pack mixed int and float"
  (struct:define-jit-struct mixed-struct
    (count :int)
    (value :float))
  (let* ((struct (struct:get-jit-struct 'mixed-struct))
         (original '(count 42 value 3.14))
         (words (struct:struct-to-words struct original))
         (recovered (struct:words-to-struct struct words)))
    (assert-equal 42 (getf recovered 'count))
    (assert-true (< (abs (- 3.14 (getf recovered 'value))) 0.001))))

(deftest struct-with-chars
  "can handle char fields"
  (struct:define-jit-struct char-struct
    (a :char)
    (b :char)
    (c :char)
    (d :char))
  (let* ((struct (struct:get-jit-struct 'char-struct))
         (original '(a 65 b 66 c 67 d 68))  ; ABCD
         (words (struct:struct-to-words struct original))
         (recovered (struct:words-to-struct struct words)))
    (assert-equal 1 (length words))
    (assert-equal 65 (getf recovered 'a))
    (assert-equal 66 (getf recovered 'b))
    (assert-equal 67 (getf recovered 'c))
    (assert-equal 68 (getf recovered 'd))))

;;; ============================================================================
;;; Alignment Tests
;;; ============================================================================

(deftest struct-alignment-padding
  "struct respects field alignment"
  (struct:define-jit-struct aligned-struct
    (a :char)    ; offset 0, size 1
    (b :int))    ; offset 4 (aligned), size 4
  (let* ((struct (struct:get-jit-struct 'aligned-struct))
         (fields (epsilon.foreign.jit.struct::jit-struct-def-fields struct)))
    ;; char at 0, 3 bytes padding, int at 4 = total size 8
    (assert-equal 8 (struct:jit-struct-size 'aligned-struct))
    (assert-equal 4 (struct:jit-struct-alignment 'aligned-struct))
    ;; Check that b is at offset 4 (not 1)
    (let ((b-field (find 'b fields :key #'first)))
      (assert-true (not (null b-field)))
      ;; b should be at offset 4 due to alignment
      (assert-equal 4 (second b-field)))))


;;; ============================================================================
;;; Field Accessor Tests
;;; ============================================================================

(deftest struct-field-ref-read
  "can read fields via struct-field-ref"
  (struct:define-jit-struct field-test
    (x :int)
    (y :int))
  ;; Create a buffer with known values
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Write x=42 at offset 0
    (setf (aref buffer 0) 42)
    ;; Write y=100 at offset 4
    (setf (aref buffer 4) 100)
    (sb-sys:with-pinned-objects (buffer)
      (let ((ptr (sb-sys:sap-int (sb-sys:vector-sap buffer))))
        (assert-equal 42 (epsilon.foreign.jit.struct::struct-field-ref ptr 'field-test 'x))
        (assert-equal 100 (epsilon.foreign.jit.struct::struct-field-ref ptr 'field-test 'y))))))

(deftest struct-field-ref-write
  "can write fields via struct-field-ref"
  (struct:define-jit-struct field-write-test
    (a :int)
    (b :int))
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((ptr (sb-sys:sap-int (sb-sys:vector-sap buffer))))
        (setf (epsilon.foreign.jit.struct::struct-field-ref ptr 'field-write-test 'a) 123)
        (setf (epsilon.foreign.jit.struct::struct-field-ref ptr 'field-write-test 'b) 456)
        (assert-equal 123 (epsilon.foreign.jit.struct::struct-field-ref ptr 'field-write-test 'a))
        (assert-equal 456 (epsilon.foreign.jit.struct::struct-field-ref ptr 'field-write-test 'b))))))

(deftest struct-field-ref-double
  "can read/write double fields"
  (struct:define-jit-struct double-field-test
    (value :double))
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((ptr (sb-sys:sap-int (sb-sys:vector-sap buffer))))
        (setf (epsilon.foreign.jit.struct::struct-field-ref ptr 'double-field-test 'value) 3.14159d0)
        (assert-true (< (abs (- 3.14159d0
                       (epsilon.foreign.jit.struct::struct-field-ref ptr 'double-field-test 'value)))
               1.0d-10))))))
