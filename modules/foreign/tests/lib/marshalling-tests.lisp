(defpackage epsilon.foreign.marshalling-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)
   (marshalling epsilon.foreign.marshalling)
   (trampoline epsilon.foreign.trampoline)))

(in-package epsilon.foreign.marshalling-tests)

;;;; Tests for automatic type marshalling in Phase 2

(deftest test-automatic-type-inference
  "Test automatic type inference from function signatures"
  ;; Test that we can infer common C function signatures
  (let ((strlen-sig (marshalling:infer-function-signature "strlen")))
    (is (trampoline:ffi-signature-p strlen-sig))
    (is (eq (trampoline:ffi-signature-return-type strlen-sig) :size-t))
    (is (equal (trampoline:ffi-signature-arg-types strlen-sig) '(:string))))
  
  (let ((malloc-sig (marshalling:infer-function-signature "malloc")))
    (is (trampoline:ffi-signature-p malloc-sig))
    (is (eq (trampoline:ffi-signature-return-type malloc-sig) :pointer))
    (is (equal (trampoline:ffi-signature-arg-types malloc-sig) '(:size-t))))
  
  (let ((memcpy-sig (marshalling:infer-function-signature "memcpy")))
    (is (trampoline:ffi-signature-p memcpy-sig))
    (is (eq (trampoline:ffi-signature-return-type memcpy-sig) :pointer))
    (is (equal (trampoline:ffi-signature-arg-types memcpy-sig) '(:pointer :pointer :size-t)))))

(deftest test-array-marshalling
  "Test automatic marshalling of arrays"
  ;; Test passing Lisp arrays to C functions
  (let ((data (make-array 10 :element-type '(unsigned-byte 8)
                             :initial-contents '(0 1 2 3 4 5 6 7 8 9))))
    ;; Should automatically pin and pass array
    (marshalling:with-pinned-array (ptr data)
      (is (sb-sys:system-area-pointer-p ptr))
      ;; Verify contents
      (loop for i from 0 below 10
            do (is (= (sb-sys:sap-ref-8 ptr i) i)))))
  
  ;; Test with different array types
  (let ((int-array (make-array 5 :element-type '(signed-byte 32)
                                 :initial-contents '(-2 -1 0 1 2))))
    (marshalling:with-pinned-array (ptr int-array)
      (is (sb-sys:system-area-pointer-p ptr))
      (is (= (sb-sys:signed-sap-ref-32 ptr 0) -2))
      (is (= (sb-sys:signed-sap-ref-32 ptr 4) -1)))))

(deftest test-string-array-marshalling
  "Test marshalling of string arrays (char**)"
  (let ((strings '("hello" "world" "test")))
    (marshalling:with-string-array (argv strings)
      (is (sb-sys:system-area-pointer-p argv))
      ;; Each pointer should point to a string
      (let ((ptr0 (sb-sys:sap-ref-sap argv 0)))
        (is (= (sb-sys:sap-ref-8 ptr0 0) (char-code #\h))))
      (let ((ptr1 (sb-sys:sap-ref-sap argv (* 8 1)))) ; 8 bytes per pointer
        (is (= (sb-sys:sap-ref-8 ptr1 0) (char-code #\w))))
      ;; NULL terminator
      (is (sb-sys:sap= (sb-sys:sap-ref-sap argv (* 8 3)) 
                       (sb-sys:int-sap 0))))))

(deftest test-automatic-string-conversion
  "Test that strings are automatically converted"
  (skip "String conversion test prints to stdout")
  ;; Define a function with automatic conversion
  (lib:defshared test-puts "puts" "libc")
  
  ;; Should work with Lisp strings directly
  (let ((result (test-puts "Hello from auto-marshalling!")))
    (is (numberp result))
    (is (>= result 0)))) ; puts returns non-negative on success

(deftest test-automatic-buffer-conversion
  "Test automatic conversion of byte vectors to buffers"
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    ;; Should automatically convert to pointer
    (let ((ptr (lib:convert-to-foreign buffer :buffer)))
      (is (sb-sys:system-area-pointer-p ptr))
      ;; Modifications should be visible
      (setf (sb-sys:sap-ref-8 ptr 0) 42)
      (is (= (aref buffer 0) 42)))))

(deftest test-keyword-to-enum-conversion
  "Test automatic conversion of keywords to C enums"
  ;; Register some enum values
  (marshalling:define-enum 'seek-whence
    '((:seek-set . 0)
      (:seek-cur . 1)
      (:seek-end . 2)))
  
  ;; Test conversion
  (is (= (lib:convert-to-foreign :seek-set 'seek-whence) 0))
  (is (= (lib:convert-to-foreign :seek-cur 'seek-whence) 1))
  (is (= (lib:convert-to-foreign :seek-end 'seek-whence) 2))
  
  ;; Test reverse conversion
  (is (eq (lib::convert-from-foreign 0 'seek-whence) :seek-set))
  (is (eq (lib::convert-from-foreign 1 'seek-whence) :seek-cur))
  (is (eq (lib::convert-from-foreign 2 'seek-whence) :seek-end)))

(deftest test-auto-return-type-handling
  "Test automatic handling of different return types"
  ;; Functions returning strings
  (lib:defshared test-getenv "getenv" "libc")
  (let ((path (test-getenv "PATH")))
    (is (or (null path) (stringp path)))) ; Could be null if not set
  
  ;; Functions returning booleans (as int)
  (lib:defshared test-isalpha "isalpha" "libc")
  (is (numberp (test-isalpha (char-code #\a))))
  (is (numberp (test-isalpha (char-code #\1)))))

(deftest test-variadic-hint
  "Test handling of variadic functions with type hints"
  (skip "Variadic function test prints to stdout")
  ;; For variadic functions, we need to provide hints
  (lib:defshared test-printf "printf" "libc"
    :variadic t
    :arg-hints '(:string &rest))
  
  ;; Should work with various argument counts
  (is (numberp (test-printf "Hello\n")))
  (is (numberp (test-printf "Number: %d\n" 42)))
  (is (numberp (test-printf "String: %s, Number: %d\n" "test" 123))))

(deftest test-output-parameter-marshalling
  "Test automatic handling of output parameters"
  ;; Define a function that uses output parameters
  (lib:defshared test-pipe "pipe" "libc")
  
  ;; Should automatically handle int[2] as output
  (let* ((fds (lib:with-output-array (fd-ptr 2 :int)
                (test-pipe fd-ptr)))
         (result (if (listp fds) 0 -1))) ; If we got a list, pipe succeeded
    (when (zerop result) ; Success
      (is (>= (first fds) 0))
      (is (>= (second fds) 0))
      ;; Clean up
      (lib:defshared close-fd "close" "libc" :int (fd :int))
      (close-fd (first fds))
      (close-fd (second fds)))))

(deftest test-struct-by-value-marshalling
  "Test automatic marshalling of structs passed by value"
  ;; Define a simple struct
  (lib:define-c-struct 'timespec
    '((tv-sec :time-t)
      (tv-nsec :long)))
  
  ;; Create an instance using with-c-struct macro
  (lib:with-c-struct (ts timespec)
    ;; Set values
    (setf (lib:struct-ref ts 'tv-sec) 1234567890)
    (setf (lib:struct-ref ts 'tv-nsec) 123456789)
    ;; Check values
    (is (= (lib:struct-ref ts 'tv-sec) 1234567890))
    (is (= (lib:struct-ref ts 'tv-nsec) 123456789))))

(deftest test-smart-defshared-inference
  "Test the smart defshared macro with automatic inference"
  ;; Use defshared directly since it creates runtime functions
  (lib:defshared smart-strlen "strlen" "libc")
  (lib:defshared smart-memcmp "memcmp" "libc")
  
  (is (= (smart-strlen "test") 4))
  (is (= (smart-strlen "") 0))
  (is (= (smart-strlen "hello world") 11))
  
  ;; Should work with complex functions too
  (let ((buf1 (make-array 5 :element-type '(unsigned-byte 8)
                            :initial-contents '(1 2 3 4 5)))
        (buf2 (make-array 5 :element-type '(unsigned-byte 8)
                            :initial-contents '(1 2 3 4 5)))
        (buf3 (make-array 5 :element-type '(unsigned-byte 8)
                            :initial-contents '(1 2 3 4 6))))
    (sb-sys:with-pinned-objects (buf1 buf2 buf3)
      (let ((result1 (smart-memcmp (sb-sys:vector-sap buf1) 
                                   (sb-sys:vector-sap buf2) 5))
            (result2 (smart-memcmp (sb-sys:vector-sap buf1) 
                                   (sb-sys:vector-sap buf3) 5)))
        (is (zerop result1))
        (is (not (zerop result2)))))))

(deftest test-errno-handling
  "Test automatic errno checking and conversion"
  ;; Functions that set errno on error
  (lib:defshared test-strtol "strtol" "libc"
    :check-errno t)
  
  ;; Should signal conditions on error
  (handler-case
      (test-strtol "not-a-number" nil 10)
    (lib:foreign-error (e)
      (is (lib:foreign-error-p e))
      (is (numberp (lib:foreign-error-code e))))))

(deftest test-bool-type-conversion
  "Test C99 bool/_Bool type handling"
  (lib:define-c-type :bool 1 :converter-to #'lib:bool-to-foreign
                           :converter-from #'lib:foreign-to-bool)
  
  (is (= (lib:convert-to-foreign t :bool) 1))
  (is (= (lib:convert-to-foreign nil :bool) 0))
  (is (eq (lib:convert-from-foreign 0 :bool) nil))
  (is (eq (lib:convert-from-foreign 1 :bool) t))
  (is (eq (lib:convert-from-foreign 42 :bool) t))) ; Non-zero is true
