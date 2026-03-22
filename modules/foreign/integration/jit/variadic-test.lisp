;;;; variadic-test.lisp - Tests for variadic function support

(defpackage :epsilon.foreign.jit.variadic.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (:var :epsilon.foreign.jit.variadic)
   (:jit :epsilon.foreign.jit))
   (:enter t))

;;; ============================================================================
;;; Type Inference Tests
;;; ============================================================================

(deftest infer-double-float
  "infers double-float as :double"
  (assert-equal :double (var:infer-variadic-type 1.0d0)))

(deftest infer-single-float-promoted
  "infers single-float as :double (C promotion)"
  (assert-equal :double (var:infer-variadic-type 1.0)))

(deftest infer-fixnum
  "infers fixnum as :long"
  (assert-equal :long (var:infer-variadic-type 42)))

(deftest infer-bignum
  "infers bignum as :long"
  (assert-equal :long (var:infer-variadic-type 12345678901234567890)))

(deftest infer-string
  "infers string as :string"
  (assert-equal :string (var:infer-variadic-type "hello")))

(deftest infer-sap
  "infers system-area-pointer as :pointer"
  (assert-equal :pointer (var:infer-variadic-type (sb-sys:int-sap 0))))

(deftest infer-byte-array
  "infers unsigned-byte array as :pointer"
  (let ((arr (make-array 10 :element-type '(unsigned-byte 8))))
    (assert-equal :pointer (var:infer-variadic-type arr))))

;;; ============================================================================
;;; Make Variadic Caller Tests
;;; ============================================================================

(deftest make-printf-caller-returns-function
  "make-printf-caller returns a function"
  (let ((caller (var:make-printf-caller)))
    (assert-true (functionp caller))))

(deftest make-snprintf-caller-returns-function
  "make-snprintf-caller returns a function"
  (let ((caller (var:make-snprintf-caller)))
    (assert-true (functionp caller))))

(deftest make-sprintf-caller-returns-function
  "make-sprintf-caller returns a function"
  (let ((caller (var:make-sprintf-caller)))
    (assert-true (functionp caller))))

(deftest make-variadic-caller-with-address
  "can create variadic caller from address"
  (let* ((addr (sb-sys:find-foreign-symbol-address "snprintf"))
         (caller (var:make-variadic-caller addr :int '(:pointer :long :string))))
    (assert-true (functionp caller))))

;;; ============================================================================
;;; snprintf Tests (Safe for Testing)
;;; ============================================================================

(deftest snprintf-simple-string
  "snprintf with no format args"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((n (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "Hello, World!")))
        (assert-true (= 13 n))
        (assert-equal "Hello, World!"
                  (sb-ext:octets-to-string buffer :end (position 0 buffer)))))))

(deftest snprintf-single-int
  "snprintf with one integer"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((n (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "Value: %d" 42)))
        (assert-true (= 9 n))  ; "Value: 42" = 9 chars
        (assert-equal "Value: 42"
                  (sb-ext:octets-to-string buffer :end (position 0 buffer)))))))

(deftest snprintf-two-ints
  "snprintf with two integers"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((n (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "%d + %d = %d" 1 2 3)))
        (assert-true (= 9 n))
        (assert-equal "1 + 2 = 3"
                  (sb-ext:octets-to-string buffer :end (position 0 buffer)))))))

(deftest snprintf-float
  "snprintf with a float (promoted to double)"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "Pi: %.2f" 3.14159d0)
      (assert-equal "Pi: 3.14"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

;; TODO: mixed int/float variadic args not handled correctly
(deftest snprintf-mixed-types
  "snprintf with mixed int and float"
  (skip "mixed int/float variadic args not handled correctly")
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "Count: %d, Value: %.1f" 5 2.5d0)
      (assert-equal "Count: 5, Value: 2.5"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

(deftest snprintf-string-arg
  "snprintf with string argument"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "Hello, %s!" "World")
      (assert-equal "Hello, World!"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

(deftest snprintf-truncation
  "snprintf truncates when buffer too small"
  (let ((buffer (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      ;; snprintf returns what WOULD have been written (excluding null)
      (let ((n (var:variadic-snprintf (sb-sys:vector-sap buffer) 10 "Hello, World!")))
        (assert-true (= 13 n))  ; Would have written 13 chars
        ;; But only wrote 9 + null
        (assert-equal "Hello, Wo"
                  (sb-ext:octets-to-string buffer :end (position 0 buffer)))))))

(deftest snprintf-hex-format
  "snprintf with hex format"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "0x%x" 255)
      (assert-equal "0xff"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

(deftest snprintf-pointer-format
  "snprintf with pointer format"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "%p" #x12345678)
      ;; Result depends on platform, but should contain the hex digits
      (let ((result (sb-ext:octets-to-string buffer :end (position 0 buffer))))
        (assert-true (search "12345678" result))))))

(deftest snprintf-negative-int
  "snprintf with negative integer"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "Value: %d" -42)
      (assert-equal "Value: -42"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

(deftest snprintf-large-int
  "snprintf with large integer"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "Value: %ld" 9223372036854775807)
      (assert-equal "Value: 9223372036854775807"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

;;; ============================================================================
;;; sprintf-to-string Tests
;;; ============================================================================

(deftest sprintf-to-string-basic
  "sprintf-to-string returns formatted string"
  (let ((result (var:sprintf-to-string "Hello, %s!" "World")))
    (assert-equal "Hello, World!" result)))

(deftest sprintf-to-string-multiple-args
  "sprintf-to-string handles multiple args"
  (let ((result (var:sprintf-to-string "%d + %d = %d" 2 3 5)))
    (assert-equal "2 + 3 = 5" result)))

(deftest sprintf-to-string-float
  "sprintf-to-string handles floats"
  (let ((result (var:sprintf-to-string "Value: %.2f" 3.14159d0)))
    (assert-equal "Value: 3.14" result)))

;;; ============================================================================
;;; defvariadic Macro Tests
;;; ============================================================================

(var:defvariadic test-printf "printf" :int
  ((format :string)))

(var:defvariadic test-snprintf "snprintf" :int
  ((buffer :pointer)
   (size :long)
   (format :string)))

(deftest defvariadic-creates-function
  "defvariadic creates a callable function"
  (assert-true (fboundp 'test-printf))
  (assert-true (fboundp 'test-snprintf)))

(deftest defvariadic-snprintf-works
  "defvariadic snprintf works correctly"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (test-snprintf (sb-sys:vector-sap buffer) 100 "Test: %d" 123)
      (assert-equal "Test: 123"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(deftest snprintf-empty-format
  "snprintf with empty format string"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((n (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "")))
        (assert-true (= 0 n))
        (assert-equal "" (sb-ext:octets-to-string buffer :end (position 0 buffer)))))))

(deftest snprintf-percent-literal
  "snprintf with %% literal"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "100%% complete")
      (assert-equal "100% complete"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

(deftest snprintf-zero-size
  "snprintf with zero size writes nothing"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 65)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((n (var:variadic-snprintf (sb-sys:vector-sap buffer) 0 "Hello")))
        (assert-true (= 5 n))  ; Would have written 5 chars
        ;; Buffer should be unchanged (still 'A' = 65)
        (assert-true (= 65 (aref buffer 0)))))))

(deftest snprintf-width-specifier
  "snprintf with width specifier"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "%5d" 42)
      (assert-equal "   42"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))

(deftest snprintf-zero-padded
  "snprintf with zero padding"
  (let ((buffer (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (var:variadic-snprintf (sb-sys:vector-sap buffer) 100 "%05d" 42)
      (assert-equal "00042"
                (sb-ext:octets-to-string buffer :end (position 0 buffer))))))
