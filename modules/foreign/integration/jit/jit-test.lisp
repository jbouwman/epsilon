;;;; jit-test.lisp - Tests for JIT-compiled FFI stubs

(defpackage :epsilon.foreign.jit.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (:jit :epsilon.foreign.jit))
  (:enter t))

(deftest platform-supported-p-returns-boolean
  "platform-supported-p returns boolean"
  (let ((result (jit:platform-supported-p)))
    (assert-true (typep result 'boolean))))

(deftest current-platform-is-detected
  "current-platform is detected"
  (assert-true (member jit:*current-platform* '(:arm64 :x86-64 :unknown))))

(deftest arm64-stub-generation
  "ARM64 stub generation produces correct size code"
  (when (eq jit:*current-platform* :arm64)
    (let ((code (epsilon.foreign.jit::generate-arm64-stub
                 #x123456789ABCDEF0 :int '(:int))))
      (assert-true (vectorp code))
      (assert-true (> (length code) 0))
      ;; Check for expected instruction count (prologue + load + call + epilogue)
      ;; 2 (prologue) + 4 (movz/movk) + 1 (blr) + 3 (epilogue) = 10 instructions = 40 bytes
      (assert-equal 40 (length code)))))

(deftest arm64-stub-encoding
  "ARM64 stub has correct instruction encoding"
  (when (eq jit:*current-platform* :arm64)
    (let ((code (epsilon.foreign.jit::generate-arm64-stub
                 #x0000000012345678 :int '())))
      ;; First instruction should be SUB SP, SP, #16 (prologue)
      ;; Encoding: 0xD10043FF
      (let ((first-insn (logior (aref code 0)
                                 (ash (aref code 1) 8)
                                 (ash (aref code 2) 16)
                                 (ash (aref code 3) 24))))
        (assert-equal #xD10043FF first-insn)))))

(deftest x86-64-stub-generation
  "x86-64 stub generation produces correct size code"
  (when (eq jit:*current-platform* :x86-64)
    (let ((code (epsilon.foreign.jit::generate-x86-64-stub
                 #x123456789ABCDEF0 :int '(:int))))
      (assert-true (vectorp code))
      (assert-true (> (length code) 0))
      ;; mov r11, imm64 (10 bytes) + call r11 (3 bytes) + ret (1 byte) = 14 bytes
      (assert-equal 14 (length code)))))

(deftest x86-64-stub-encoding
  "x86-64 stub has correct instruction encoding"
  (when (eq jit:*current-platform* :x86-64)
    (let ((code (epsilon.foreign.jit::generate-x86-64-stub
                 #x0000000012345678 :int '())))
      ;; First two bytes should be REX.WB (0x49) + MOV r11 (0xBB)
      (assert-equal #x49 (aref code 0))
      (assert-equal #xBB (aref code 1))
      ;; Last byte should be RET (0xC3)
      (assert-equal #xC3 (aref code (1- (length code)))))))

(deftest stub-cache-initially-empty
  "stub cache initially empty"
  (jit:clear-stub-cache)
  (let ((stats (jit:stub-cache-stats)))
    (assert-equal 0 (getf stats :stub-count))))

(deftest ffi-type-conversion
  "FFI type to alien type conversion works"
  (assert-equal 'sb-alien:int (epsilon.foreign.jit::ffi-type-to-alien :int))
  (assert-equal 'sb-alien:double (epsilon.foreign.jit::ffi-type-to-alien :double))
  (assert-equal 'sb-alien:void (epsilon.foreign.jit::ffi-type-to-alien :void))
  ;; Pointers use unsigned-long to allow passing integer addresses directly
  (assert-equal 'sb-alien:unsigned-long (epsilon.foreign.jit::ffi-type-to-alien :pointer)))

(deftest executable-memory-allocation
  "executable memory can be allocated and freed"
  (when (jit:platform-supported-p)
    (let ((region (jit:make-executable-memory 4096)))
      (assert-true (not (null region)))
      (assert-true (not (null (epsilon.foreign.jit::executable-region-address region))))
      (assert-equal 4096 (epsilon.foreign.jit::executable-region-size region))
      (jit:free-executable-memory region)
      (assert-true (null (epsilon.foreign.jit::executable-region-address region))))))

(deftest write-code-to-executable-memory
  "code can be written to executable memory using jit-memcpy"
  (when (jit:platform-supported-p)
    (let* ((region (jit:make-executable-memory 4096))
           ;; Generate a simple stub
           (code (jit:generate-call-stub #x123456789ABC :int '())))
      (assert-true (> (length code) 0))
      ;; Write it to executable memory - this uses sb-vm::jit-memcpy on ARM64
      (epsilon.foreign.jit::write-to-executable region 0 code)
      ;; Verify the bytes were written
      (let ((addr (epsilon.foreign.jit::executable-region-address region)))
        (assert-equal (aref code 0) (sb-sys:sap-ref-8 addr 0))
        (assert-equal (aref code 1) (sb-sys:sap-ref-8 addr 1)))
      (jit:free-executable-memory region))))

(deftest compile-stub-creates-callable
  "compile-stub creates a jit-stub structure"
  (when (jit:platform-supported-p)
    (jit:clear-stub-cache)
    (let ((stub (epsilon.foreign.jit::compile-stub #x123456789ABC :int '(:int))))
      (assert-true (not (null stub)))
      (assert-true (not (null (epsilon.foreign.jit::jit-stub-address stub))))
      (assert-true (> (epsilon.foreign.jit::jit-stub-size stub) 0))
      ;; Check cache stats
      (let ((stats (jit:stub-cache-stats)))
        (assert-true (> (getf stats :region-used) 0))))
    (jit:clear-stub-cache)))

;;; ============================================================================
;;; Execution Tests - Call real C functions through JIT stubs
;;; ============================================================================

(defun get-foreign-symbol-address (name)
  "Get the address of a foreign symbol as an integer."
  ;; On some platforms this returns an integer directly, on others a SAP
  (let ((result (sb-sys:find-foreign-symbol-address name)))
    (etypecase result
      (integer result)
      (sb-sys:system-area-pointer (sb-sys:sap-int result))
      (null nil))))

(deftest jit-stub-calls-abs
  "JIT stub can call libc abs()"
  (when (jit:platform-supported-p)
    (let ((abs-addr (get-foreign-symbol-address "abs")))
      (when abs-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub abs-addr :int '(:int))))
          (assert-equal 42 (jit:call-stub stub -42))
          (assert-equal 0 (jit:call-stub stub 0))
          (assert-equal 123 (jit:call-stub stub 123)))
        (jit:clear-stub-cache)))))

(deftest jit-stub-calls-strlen
  "JIT stub can call libc strlen()"
  (when (jit:platform-supported-p)
    (let ((strlen-addr (get-foreign-symbol-address "strlen")))
      (when strlen-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub strlen-addr :long '(:pointer))))
          ;; Create a null-terminated C string (SBCL strings aren't null-terminated)
          (let ((test-string (make-array 6 :element-type '(unsigned-byte 8)
                                           :initial-contents '(104 101 108 108 111 0)))) ; "hello\0"
            (sb-sys:with-pinned-objects (test-string)
              (let ((str-addr (sb-sys:sap-int (sb-sys:vector-sap test-string))))
                (assert-equal 5 (jit:call-stub stub str-addr))))))
        (jit:clear-stub-cache)))))

(deftest jit-stub-calls-getpid
  "JIT stub can call getpid() with no arguments"
  (when (jit:platform-supported-p)
    (let ((getpid-addr (get-foreign-symbol-address "getpid")))
      (when getpid-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub getpid-addr :int '())))
          ;; getpid should return a positive integer
          (let ((pid (jit:call-stub stub)))
            (assert-true (integerp pid))
            (assert-true (> pid 0))
            ;; Should return same value as sb-posix:getpid
            (assert-equal (sb-posix:getpid) pid)))
        (jit:clear-stub-cache)))))

(deftest jit-stub-calls-multi-arg
  "JIT stub can call function with multiple arguments"
  (when (jit:platform-supported-p)
    ;; Use strcmp which takes 2 pointer arguments
    (let ((strcmp-addr (get-foreign-symbol-address "strcmp")))
      (when strcmp-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub strcmp-addr :int '(:pointer :pointer))))
          ;; Create null-terminated C strings
          (let ((str1 (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(97 98 99 0)))   ; "abc\0"
                (str2 (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(97 98 99 0)))   ; "abc\0"
                (str3 (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(97 98 100 0)))) ; "abd\0"
            (sb-sys:with-pinned-objects (str1 str2 str3)
              (let ((addr1 (sb-sys:sap-int (sb-sys:vector-sap str1)))
                    (addr2 (sb-sys:sap-int (sb-sys:vector-sap str2)))
                    (addr3 (sb-sys:sap-int (sb-sys:vector-sap str3))))
                ;; Equal strings should return 0
                (assert-equal 0 (jit:call-stub stub addr1 addr2))
                ;; "abc" < "abd" should return negative
                (assert-true (< (jit:call-stub stub addr1 addr3) 0))
                ;; "abd" > "abc" should return positive
                (assert-true (> (jit:call-stub stub addr3 addr1) 0))))))
        (jit:clear-stub-cache)))))

;;; ============================================================================
;;; Floating Point Tests - Call math functions through JIT stubs
;;; ============================================================================

(defun approximately-equal (a b &optional (tolerance 1.0d-10))
  "Check if two floating point numbers are approximately equal."
  (< (abs (- a b)) tolerance))

(deftest jit-stub-calls-sin
  "JIT stub can call sin() with double argument"
  (when (jit:platform-supported-p)
    (let ((sin-addr (get-foreign-symbol-address "sin")))
      (when sin-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub sin-addr :double '(:double))))
          ;; sin(0) = 0
          (assert-true (approximately-equal 0.0d0 (jit:call-stub stub 0.0d0)))
          ;; sin(pi/2) = 1
          (assert-true (approximately-equal 1.0d0 (jit:call-stub stub (/ pi 2))))
          ;; sin(pi) = 0
          (assert-true (approximately-equal 0.0d0 (jit:call-stub stub pi)))
          ;; sin(-pi/2) = -1
          (assert-true (approximately-equal -1.0d0 (jit:call-stub stub (- (/ pi 2))))))
        (jit:clear-stub-cache)))))

(deftest jit-stub-calls-cos
  "JIT stub can call cos() with double argument"
  (when (jit:platform-supported-p)
    (let ((cos-addr (get-foreign-symbol-address "cos")))
      (when cos-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub cos-addr :double '(:double))))
          ;; cos(0) = 1
          (assert-true (approximately-equal 1.0d0 (jit:call-stub stub 0.0d0)))
          ;; cos(pi/2) = 0
          (assert-true (approximately-equal 0.0d0 (jit:call-stub stub (/ pi 2))))
          ;; cos(pi) = -1
          (assert-true (approximately-equal -1.0d0 (jit:call-stub stub pi))))
        (jit:clear-stub-cache)))))

(deftest jit-stub-calls-floor
  "JIT stub can call floor() with double argument"
  (when (jit:platform-supported-p)
    (let ((floor-addr (get-foreign-symbol-address "floor")))
      (when floor-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub floor-addr :double '(:double))))
          ;; floor(3.7) = 3.0
          (assert-true (approximately-equal 3.0d0 (jit:call-stub stub 3.7d0)))
          ;; floor(-3.7) = -4.0
          (assert-true (approximately-equal -4.0d0 (jit:call-stub stub -3.7d0)))
          ;; floor(5.0) = 5.0
          (assert-true (approximately-equal 5.0d0 (jit:call-stub stub 5.0d0))))
        (jit:clear-stub-cache)))))

(deftest jit-stub-calls-hypot
  "JIT stub can call hypot() with two double arguments"
  (when (jit:platform-supported-p)
    (let ((hypot-addr (get-foreign-symbol-address "hypot")))
      (when hypot-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub hypot-addr :double '(:double :double))))
          ;; hypot(3, 4) = 5 (classic 3-4-5 triangle)
          (assert-true (approximately-equal 5.0d0 (jit:call-stub stub 3.0d0 4.0d0)))
          ;; hypot(5, 12) = 13
          (assert-true (approximately-equal 13.0d0 (jit:call-stub stub 5.0d0 12.0d0)))
          ;; hypot(1, 1) = sqrt(2)
          (assert-true (approximately-equal (sqrt 2.0d0) (jit:call-stub stub 1.0d0 1.0d0))))
        (jit:clear-stub-cache)))))

(deftest jit-stub-calls-pow
  "JIT stub can call pow() with two double arguments"
  (when (jit:platform-supported-p)
    (let ((pow-addr (get-foreign-symbol-address "pow")))
      (when pow-addr
        (jit:clear-stub-cache)
        (let ((stub (epsilon.foreign.jit::compile-stub pow-addr :double '(:double :double))))
          ;; pow(2, 3) = 8
          (assert-true (approximately-equal 8.0d0 (jit:call-stub stub 2.0d0 3.0d0)))
          ;; pow(10, 2) = 100
          (assert-true (approximately-equal 100.0d0 (jit:call-stub stub 10.0d0 2.0d0)))
          ;; pow(2, 0.5) = sqrt(2)
          (assert-true (approximately-equal (sqrt 2.0d0) (jit:call-stub stub 2.0d0 0.5d0))))
        (jit:clear-stub-cache)))))

(deftest jit-make-caller-float
  "make-jit-caller works with float-returning functions"
  (when (jit:platform-supported-p)
    (let ((sin-addr (get-foreign-symbol-address "sin")))
      (when sin-addr
        (jit:clear-stub-cache)
        (let ((sin-fn (jit:make-jit-caller sin-addr :double '(:double))))
          ;; Should return a function
          (assert-true (functionp sin-fn))
          ;; Call it directly
          (assert-true (approximately-equal 1.0d0 (funcall sin-fn (/ pi 2))))
          (assert-true (approximately-equal 0.0d0 (funcall sin-fn 0.0d0))))
        (jit:clear-stub-cache)))))

;;; ============================================================================
;;; Symbol Resolution Tests
;;; ============================================================================

(deftest find-symbol-address-works
  "find-symbol-address returns correct addresses for known symbols"
  (let ((abs-addr (get-foreign-symbol-address "abs"))
        (sin-addr (get-foreign-symbol-address "sin")))
    (assert-true (integerp abs-addr))
    (assert-true (integerp sin-addr))
    (assert-true (not (= abs-addr sin-addr)))))

(deftest find-symbol-address-returns-nil-for-unknown
  "find-symbol-address returns nil for unknown symbols"
  (assert-true (null (get-foreign-symbol-address "this_symbol_does_not_exist_12345"))))
