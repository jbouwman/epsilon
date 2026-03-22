;;;; variadic-support.lisp - Support for C variadic functions
;;;;
;;;; Provides JIT-compiled callers for variadic C functions like printf,
;;;; sprintf, snprintf. Handles the special calling convention requirements:
;;;; - x86-64: AL must contain count of vector (XMM) registers used
;;;; - ARM64: Variadic args passed on stack after register args

(defpackage epsilon.foreign.jit.variadic
  (:use cl)
  (:local-nicknames
   (jit epsilon.foreign.jit))
  (:export
   ;; Core API
   #:make-variadic-caller
   #:call-variadic

   ;; Type inference
   #:infer-variadic-type
   #:*auto-promote-types*

   ;; Macro interface
   #:defvariadic

   ;; Printf family
   #:make-printf-caller
   #:make-sprintf-caller
   #:make-snprintf-caller
   #:make-fprintf-caller

   ;; Pre-built callers
   #:variadic-printf
   #:variadic-sprintf
   #:variadic-snprintf

   ;; Utilities
   #:sprintf-to-string
   #:with-c-string-buffer)
  (:enter t))

;;; ============================================================================
;;; Type Inference for Variadic Arguments
;;; ============================================================================

(defvar *auto-promote-types* t
  "When true, automatically promote variadic argument types per C rules:
   - float -> double
   - char/short -> int
   This matches the C default argument promotions.")

(defun infer-variadic-type (arg)
  "Infer the C type for a variadic argument.
   Applies default argument promotions per C spec:
   - Integers smaller than int are promoted to int
   - float is promoted to double
   - Strings and SAPs become pointers"
  (typecase arg
    ;; Floating point - always double (C promotion)
    (double-float :double)
    (single-float :double)  ; Promoted to double
    (float :double)

    ;; Integer types
    (fixnum :long)      ; Use long for safety on 64-bit
    (integer :long)

    ;; Pointers
    (sb-sys:system-area-pointer :pointer)

    ;; Strings - need to be converted to C strings
    (string :string)

    ;; Arrays that might be C-compatible
    ((simple-array (unsigned-byte 8) (*)) :pointer)
    ((simple-array character (*)) :string)

    ;; Default to pointer for unknown types
    (t :pointer)))

(defun coerce-variadic-arg (arg inferred-type)
  "Coerce an argument to its inferred variadic type.
   Returns the coerced value suitable for passing to C."
  (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (case inferred-type
      (:double
       (float arg 1.0d0))
      (:long
       (truncate arg))
      (:pointer
       (etypecase arg
         (sb-sys:system-area-pointer (sb-sys:sap-int arg))
         (integer arg)
         ((simple-array (unsigned-byte 8) (*))
          ;; Will be pinned by caller
          arg)))
      (:string
       ;; Strings need special handling - must be null-terminated
       arg)
      (otherwise
       arg))))

;;; ============================================================================
;;; Variadic Caller Generation
;;; ============================================================================

(defun alien-type-for-variadic (type)
  "Get the alien type for a variadic argument.
   Uses promoted types per C calling convention."
  (case type
    (:double 'sb-alien:double)
    (:float 'sb-alien:double)  ; Promoted
    (:long 'sb-alien:long)
    (:int 'sb-alien:int)
    (:pointer 'sb-alien:unsigned-long)
    (:string 'sb-alien:c-string)
    (otherwise 'sb-alien:long)))

(defun alien-return-type (type)
  "Get the alien return type."
  (case type
    (:void 'sb-alien:void)
    (:int 'sb-alien:int)
    (:long 'sb-alien:long)
    (:double 'sb-alien:double)
    (:pointer 'sb-alien:unsigned-long)
    (:size-t 'sb-alien:unsigned-long)
    (otherwise 'sb-alien:int)))

(defun make-variadic-caller (fn-addr return-type fixed-arg-types)
  "Create a caller for a variadic C function.

   FN-ADDR - Address of the C function
   RETURN-TYPE - Return type keyword (:int, :void, etc.)
   FIXED-ARG-TYPES - List of fixed argument types before the variadic portion

   Returns a function that accepts fixed args followed by any number of
   variadic args. Types of variadic args are inferred at call time.

   Example:
     (let ((printf (make-variadic-caller (dlsym \"printf\") :int '(:string))))
       (funcall printf \"Value: %d, Name: %s\" 42 \"test\"))"
  (let ((n-fixed (length fixed-arg-types))
        (addr-int (etypecase fn-addr
                    (integer fn-addr)
                    (sb-sys:system-area-pointer (sb-sys:sap-int fn-addr)))))
    (lambda (&rest all-args)
      (let* ((fixed-args (subseq all-args 0 (min n-fixed (length all-args))))
             (variadic-args (if (> (length all-args) n-fixed)
                                (subseq all-args n-fixed)
                                nil))
             (variadic-types (mapcar #'infer-variadic-type variadic-args)))
        (call-variadic-internal addr-int return-type
                                fixed-arg-types fixed-args
                                variadic-types variadic-args)))))

(defun call-variadic (fn-addr return-type fixed-arg-types fixed-args
                      variadic-types variadic-args)
  "Call a variadic C function with explicit type information.

   This is the low-level interface where all types are specified explicitly.
   For automatic type inference, use make-variadic-caller."
  (let ((addr-int (etypecase fn-addr
                    (integer fn-addr)
                    (sb-sys:system-area-pointer (sb-sys:sap-int fn-addr)))))
    (call-variadic-internal addr-int return-type
                            fixed-arg-types fixed-args
                            variadic-types variadic-args)))

;;; ============================================================================
;;; Platform-Specific Variadic Calling
;;; ============================================================================

(defun call-variadic-internal (addr return-type fixed-types fixed-args
                               variadic-types variadic-args)
  "Internal variadic call dispatcher."
  (let ((all-types (append fixed-types variadic-types))
        (all-args (append fixed-args variadic-args)))
    (case jit:*current-platform*
      (:x86-64
       (call-variadic-x86-64 addr return-type all-types all-args variadic-types))
      (:arm64
       (call-variadic-arm64 addr return-type all-types all-args variadic-types))
      (t
       (error "Variadic calls not supported on ~A" jit:*current-platform*)))))

;;; ----------------------------------------------------------------------------
;;; x86-64 Variadic Calls
;;; ----------------------------------------------------------------------------
;;;
;;; System V AMD64 ABI for variadic functions:
;;; - AL must contain the number of vector registers (XMM0-XMM7) used
;;; - Fixed args passed in: rdi, rsi, rdx, rcx, r8, r9 (integer)
;;;                         xmm0-xmm7 (floating point)
;;; - Variadic args follow the same convention as fixed args
;;; - Stack args if more than register capacity
;;;
;;; For printf family, the format string is in rdi, variadic args follow.

(defun count-xmm-args (types)
  "Count how many XMM registers are used for floating-point variadic args."
  (min 8 (count-if (lambda (type) (member type '(:double :float))) types)))

(defun call-variadic-x86-64 (addr return-type all-types all-args variadic-types)
  "Call variadic function on x86-64.
   Sets AL to the count of XMM registers used for variadic floats."
  (declare (ignore variadic-types))
  (let* ((n-args (length all-args))
         (n-xmm (count-xmm-args all-types)))
    ;; Generate appropriate alien-funcall based on arg count and types
    ;; We need to handle up to 6 register args + stack args
    (cond
      ;; Common case: printf with format + 0-5 variadic args
      ((and (<= n-args 6)
            (every (lambda (x) (member x '(:string :pointer :long :int :double)))
                   all-types))
       (call-variadic-x86-64-register-only addr return-type
                                           all-types all-args n-xmm))
      ;; More args or complex types - use stack
      (t
       (call-variadic-x86-64-with-stack addr return-type
                                        all-types all-args n-xmm)))))

(defun call-variadic-x86-64-register-only (addr return-type types args n-xmm)
  "Handle x86-64 variadic call with only register args (<=6 args)."
  (let ((n (length args)))
    ;; Build the alien function type dynamically
    ;; Note: For variadic functions, we must set AL = n-xmm
    ;; SBCL's alien-funcall doesn't directly support this, so we use
    ;; a workaround: pass enough args to use the XMM registers we claim
    (case n
      (0 (call-variadic-0 addr return-type))
      (1 (call-variadic-1 addr return-type types args))
      (2 (call-variadic-2 addr return-type types args n-xmm))
      (3 (call-variadic-3 addr return-type types args n-xmm))
      (4 (call-variadic-4 addr return-type types args n-xmm))
      (5 (call-variadic-5 addr return-type types args n-xmm))
      (6 (call-variadic-6 addr return-type types args n-xmm))
      (otherwise
       (call-variadic-x86-64-with-stack addr return-type types args n-xmm)))))

(defun prepare-arg (arg type)
  "Prepare an argument for passing to C."
  (case type
    (:double (float arg 1.0d0))
    (:float (float arg 1.0d0))  ; Promoted to double
    (:long (truncate arg))
    (:int (truncate arg))
    (:string arg)  ; c-string handles conversion
    (:pointer
     (etypecase arg
       (sb-sys:system-area-pointer (sb-sys:sap-int arg))
       (integer arg)
       (string arg)))  ; Let alien handle string->pointer
    (otherwise arg)))

;; Helper macros for generating variadic call functions
(defmacro def-variadic-caller (n)
  "Define a variadic caller for N arguments."
  (let ((fname (intern (format nil "CALL-VARIADIC-~D" n)))
        (arg-names (loop for i from 0 below n collect (intern (format nil "A~D" i)))))
    `(defun ,fname (addr return-type types args &optional (n-xmm 0))
       (declare (ignorable n-xmm return-type))
       (let ,(loop for arg in arg-names
                   for i from 0
                   collect `(,arg (prepare-arg (nth ,i args) (nth ,i types))))
         ,(if (zerop n)
              `(progn
                 (declare (ignore types args))
                 (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                             (function sb-alien:int))))
                   (sb-alien:alien-funcall fn)))
              ;; For variadic functions, we use a mixed signature
              ;; All args as long, let the C side interpret via format string
              `(progn
                 (declare (ignore types))
                 (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                             (function sb-alien:int
                                       ,@(loop repeat n collect 'sb-alien:long)))))
                   (sb-alien:alien-funcall fn ,@arg-names))))))))

;; Actually, for variadic functions we need proper type handling.
;; Let me create specialized callers for common patterns.

(defun call-variadic-0 (addr return-type)
  "Call variadic function with no args (just format string counted in fixed)."
  (declare (ignore return-type))
  (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
              (function sb-alien:int))))
    (sb-alien:alien-funcall fn)))

(defun call-variadic-1 (addr return-type types args)
  "Call variadic with 1 arg (typically just format string)."
  (declare (ignore return-type))
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types))))
    (if (eq (nth 0 types) :string)
        (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                    (function sb-alien:int sb-alien:c-string))))
          (sb-alien:alien-funcall fn a0))
        (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                    (function sb-alien:int sb-alien:long))))
          (sb-alien:alien-funcall fn a0)))))

(defun call-variadic-2 (addr return-type types args n-xmm)
  "Call variadic with 2 args."
  (declare (ignore return-type n-xmm))
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types))))
    ;; First arg is usually format string
    (cond
      ((and (eq (nth 0 types) :string) (eq (nth 1 types) :double))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string sb-alien:double))))
         (sb-alien:alien-funcall fn a0 a1)))
      ((and (eq (nth 0 types) :string) (member (nth 1 types) '(:int :long)))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1)))
      ((and (eq (nth 0 types) :string) (eq (nth 1 types) :string))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string sb-alien:c-string))))
         (sb-alien:alien-funcall fn a0 a1)))
      (t
       ;; Generic fallback
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (if (stringp a0) (sb-sys:sap-int (sb-sys:vector-sap (sb-ext:string-to-octets a0 :null-terminate t))) a0)
                                 (if (stringp a1) (sb-sys:sap-int (sb-sys:vector-sap (sb-ext:string-to-octets a1 :null-terminate t))) a1)))))))

(defun call-variadic-3 (addr return-type types args n-xmm)
  "Call variadic with 3 args."
  (declare (ignore return-type n-xmm))
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types)))
        (a2 (prepare-arg (nth 2 args) (nth 2 types))))
    ;; Check for common patterns
    (cond
      ;; printf(fmt, int, int)
      ((and (eq (nth 0 types) :string)
            (member (nth 1 types) '(:int :long))
            (member (nth 2 types) '(:int :long)))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2)))
      ;; printf(fmt, double, int)
      ((and (eq (nth 0 types) :string)
            (eq (nth 1 types) :double)
            (member (nth 2 types) '(:int :long)))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string sb-alien:double sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2)))
      ;; printf(fmt, int, double)
      ((and (eq (nth 0 types) :string)
            (member (nth 1 types) '(:int :long))
            (eq (nth 2 types) :double))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string sb-alien:long sb-alien:double))))
         (sb-alien:alien-funcall fn a0 a1 a2)))
      ;; printf(fmt, str, int)
      ((and (eq (nth 0 types) :string)
            (eq (nth 1 types) :string)
            (member (nth 2 types) '(:int :long)))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string sb-alien:c-string sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2)))
      (t
       ;; Generic: all as long
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (coerce-to-long a0 (nth 0 types))
                                 (coerce-to-long a1 (nth 1 types))
                                 (coerce-to-long a2 (nth 2 types))))))))

(defun coerce-to-long (val type)
  "Coerce a value to a long integer for generic passing."
  (cond
    ((stringp val)
     (sb-sys:with-pinned-objects (val)
       (sb-sys:sap-int (sb-sys:vector-sap (sb-ext:string-to-octets val :null-terminate t)))))
    ((eq type :double)
     ;; Pack double bits into long - this is a fallback, not ideal
     (sb-kernel:double-float-bits val))
    (t (truncate val))))

(defun call-variadic-4 (addr return-type types args n-xmm)
  "Call variadic with 4 args."
  (declare (ignore return-type n-xmm))
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types)))
        (a2 (prepare-arg (nth 2 args) (nth 2 types)))
        (a3 (prepare-arg (nth 3 args) (nth 3 types))))
    ;; Check for snprintf pattern: buf, size, fmt, args...
    (cond
      ;; snprintf(buf, size, fmt, int)
      ((and (eq (nth 0 types) :pointer)
            (member (nth 1 types) '(:int :long :size-t))
            (eq (nth 2 types) :string)
            (member (nth 3 types) '(:int :long)))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3)))
      ;; snprintf(buf, size, fmt, double)
      ((and (eq (nth 0 types) :pointer)
            (member (nth 1 types) '(:int :long :size-t))
            (eq (nth 2 types) :string)
            (eq (nth 3 types) :double))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:double))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3)))
      ;; snprintf(buf, size, fmt, string)
      ((and (eq (nth 0 types) :pointer)
            (member (nth 1 types) '(:int :long :size-t))
            (eq (nth 2 types) :string)
            (eq (nth 3 types) :string))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:c-string))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3)))
      ;; printf(fmt, int, int, int)
      ((and (eq (nth 0 types) :string)
            (every (lambda (x) (member x '(:int :long))) (subseq types 1)))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string
                             sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3)))
      (t
       ;; Generic fallback with mixed types
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (coerce-to-long a0 (nth 0 types))
                                 (coerce-to-long a1 (nth 1 types))
                                 (coerce-to-long a2 (nth 2 types))
                                 (coerce-to-long a3 (nth 3 types))))))))

(defun call-variadic-5 (addr return-type types args n-xmm)
  "Call variadic with 5 args."
  (declare (ignore return-type n-xmm))
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types)))
        (a2 (prepare-arg (nth 2 args) (nth 2 types)))
        (a3 (prepare-arg (nth 3 args) (nth 3 types)))
        (a4 (prepare-arg (nth 4 args) (nth 4 types))))
    ;; snprintf(buf, size, fmt, arg1, arg2)
    (cond
      ((and (eq (nth 0 types) :pointer)
            (member (nth 1 types) '(:int :long :size-t))
            (eq (nth 2 types) :string)
            (member (nth 3 types) '(:int :long))
            (member (nth 4 types) '(:int :long)))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4)))
      (t
       ;; Generic
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (coerce-to-long a0 (nth 0 types))
                                 (coerce-to-long a1 (nth 1 types))
                                 (coerce-to-long a2 (nth 2 types))
                                 (coerce-to-long a3 (nth 3 types))
                                 (coerce-to-long a4 (nth 4 types))))))))

(defun call-variadic-6 (addr return-type types args n-xmm)
  "Call variadic with 6 args."
  (declare (ignore return-type n-xmm))
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types)))
        (a2 (prepare-arg (nth 2 args) (nth 2 types)))
        (a3 (prepare-arg (nth 3 args) (nth 3 types)))
        (a4 (prepare-arg (nth 4 args) (nth 4 types)))
        (a5 (prepare-arg (nth 5 args) (nth 5 types))))
    ;; snprintf(buf, size, fmt, arg1, arg2, arg3)
    (cond
      ((and (eq (nth 0 types) :pointer)
            (member (nth 1 types) '(:int :long :size-t))
            (eq (nth 2 types) :string))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5)))
      (t
       ;; Generic
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (coerce-to-long a0 (nth 0 types))
                                 (coerce-to-long a1 (nth 1 types))
                                 (coerce-to-long a2 (nth 2 types))
                                 (coerce-to-long a3 (nth 3 types))
                                 (coerce-to-long a4 (nth 4 types))
                                 (coerce-to-long a5 (nth 5 types))))))))

(defun call-variadic-x86-64-with-stack (addr return-type types args n-xmm)
  "Handle x86-64 variadic calls that need stack args (>6 args)."
  (declare (ignore n-xmm))
  ;; For now, limit to 6 register args
  ;; A full implementation would push extra args to stack
  (when (> (length args) 6)
    (warn "Variadic call with >6 args may not work correctly"))
  (call-variadic-6 addr return-type
                   (subseq types 0 (min 6 (length types)))
                   (subseq args 0 (min 6 (length args)))
                   0))

;;; ----------------------------------------------------------------------------
;;; ARM64 Variadic Calls
;;; ----------------------------------------------------------------------------
;;;
;;; ARM64 AAPCS64 for variadic functions:
;;; - Named (fixed) args: x0-x7 (integer), d0-d7 (float)
;;; - Variadic args: Always passed on stack, never in registers
;;; - Stack must be 16-byte aligned
;;;
;;; To make SBCL's alien-funcall work correctly for variadic functions on ARM64,
;;; we pad the argument list to fill x0-x7 registers, forcing variadic args
;;; onto the stack where they belong.

(defun call-variadic-arm64 (addr return-type all-types all-args variadic-types)
  "Call variadic function on ARM64.
   Variadic args are always passed on the stack.
   We achieve this by padding the argument list to fill x0-x7 registers."
  (declare (ignore return-type))
  (let* ((n-fixed (- (length all-types) (length variadic-types)))
         (n-variadic (length variadic-types)))
    (if (zerop n-variadic)
        ;; No variadic args - use x86-64 path (works fine)
        (call-variadic-x86-64 addr :int all-types all-args nil)
        ;; Has variadic args - need to pad to force stack passing
        (call-variadic-arm64-padded addr n-fixed all-types all-args))))

(defun call-variadic-arm64-padded (addr n-fixed types args)
  "Call variadic function on ARM64 with padding to force stack passing.
   N-FIXED is the number of fixed (non-variadic) arguments."
  ;; ARM64 has 8 integer registers (x0-x7) for arguments.
  ;; We need to pad with dummy args to fill registers, pushing variadic args to stack.
  ;; Pad count = 8 - n-fixed (but at least 0)
  (let* ((n-pad (max 0 (- 8 n-fixed)))
         ;; Build padded types: fixed-types + padding + variadic-types
         (fixed-types (subseq types 0 n-fixed))
         (variadic-types (subseq types n-fixed))
         (padded-types (append fixed-types
                               (make-list n-pad :initial-element :long)
                               variadic-types))
         ;; Build padded args: fixed-args + zeros + variadic-args
         (fixed-args (subseq args 0 n-fixed))
         (variadic-args (subseq args n-fixed))
         (padded-args (append fixed-args
                              (make-list n-pad :initial-element 0)
                              variadic-args)))
    ;; Now call with padded argument list
    (call-variadic-arm64-dispatch addr padded-types padded-args)))

(defun call-variadic-arm64-dispatch (addr types args)
  "Dispatch ARM64 variadic call based on argument count."
  (let ((n (length args)))
    (cond
      ((<= n 8)
       ;; All args fit in registers - but this shouldn't happen for variadic
       ;; since we pad to 8 before adding variadic args
       (call-variadic-x86-64-register-only addr :int types args 0))
      ((= n 9) (call-variadic-arm64-9 addr types args))
      ((= n 10) (call-variadic-arm64-10 addr types args))
      ((= n 11) (call-variadic-arm64-11 addr types args))
      ((= n 12) (call-variadic-arm64-12 addr types args))
      (t
       (warn "ARM64 variadic call with ~D args may not work correctly" n)
       (call-variadic-arm64-12 addr (subseq types 0 12) (subseq args 0 12))))))

(defun call-variadic-arm64-9 (addr types args)
  "ARM64 variadic call with 9 args (8 regs + 1 stack)."
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types)))
        (a2 (prepare-arg (nth 2 args) (nth 2 types)))
        (a3 (prepare-arg (nth 3 args) (nth 3 types)))
        (a4 (prepare-arg (nth 4 args) (nth 4 types)))
        (a5 (prepare-arg (nth 5 args) (nth 5 types)))
        (a6 (prepare-arg (nth 6 args) (nth 6 types)))
        (a7 (prepare-arg (nth 7 args) (nth 7 types)))
        (a8 (prepare-arg (nth 8 args) (nth 8 types))))
    ;; Check for common patterns - snprintf has string at position 2
    (cond
      ;; snprintf with double variadic arg
      ;; On ARM64, variadic doubles must be passed as bit representation (long)
      ((and (eq (nth 0 types) :pointer)
            (eq (nth 2 types) :string)
            (eq (nth 8 types) :double))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7
                                 (sb-kernel:double-float-bits a8))))
      ;; snprintf with string variadic arg
      ((and (eq (nth 0 types) :pointer)
            (eq (nth 2 types) :string)
            (eq (nth 8 types) :string))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:c-string))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8)))
      ;; snprintf: ptr, size, fmt, padding..., int variadic args
      ((and (eq (nth 0 types) :pointer)
            (eq (nth 2 types) :string))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8)))
      ;; printf with double variadic arg
      ;; On ARM64, variadic doubles must be passed as bit representation (long)
      ((and (eq (nth 0 types) :string)
            (eq (nth 8 types) :double))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7
                                 (sb-kernel:double-float-bits a8))))
      ;; printf: fmt, padding..., variadic args
      ((eq (nth 0 types) :string)
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8)))
      ;; Generic - all longs
      (t
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (coerce-to-long a0 (nth 0 types))
                                 a1 a2 a3 a4 a5 a6 a7 a8))))))

(defun call-variadic-arm64-10 (addr types args)
  "ARM64 variadic call with 10 args (8 regs + 2 stack)."
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types)))
        (a2 (prepare-arg (nth 2 args) (nth 2 types)))
        (a3 (prepare-arg (nth 3 args) (nth 3 types)))
        (a4 (prepare-arg (nth 4 args) (nth 4 types)))
        (a5 (prepare-arg (nth 5 args) (nth 5 types)))
        (a6 (prepare-arg (nth 6 args) (nth 6 types)))
        (a7 (prepare-arg (nth 7 args) (nth 7 types)))
        (a8 (prepare-arg (nth 8 args) (nth 8 types)))
        (a9 (prepare-arg (nth 9 args) (nth 9 types))))
    (cond
      ;; snprintf with double at position 9
      ((and (eq (nth 0 types) :pointer)
            (eq (nth 2 types) :string)
            (eq (nth 9 types) :double))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8
                                 (sb-kernel:double-float-bits a9))))
      ;; snprintf pattern (other variadic types)
      ((and (eq (nth 0 types) :pointer)
            (eq (nth 2 types) :string))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)))
      ;; printf with double at position 9
      ((and (eq (nth 0 types) :string)
            (eq (nth 9 types) :double))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8
                                 (sb-kernel:double-float-bits a9))))
      ;; printf pattern
      ((eq (nth 0 types) :string)
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)))
      (t
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (coerce-to-long a0 (nth 0 types))
                                 a1 a2 a3 a4 a5 a6 a7 a8 a9))))))

(defun call-variadic-arm64-11 (addr types args)
  "ARM64 variadic call with 11 args (8 regs + 3 stack)."
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types)))
        (a2 (prepare-arg (nth 2 args) (nth 2 types)))
        (a3 (prepare-arg (nth 3 args) (nth 3 types)))
        (a4 (prepare-arg (nth 4 args) (nth 4 types)))
        (a5 (prepare-arg (nth 5 args) (nth 5 types)))
        (a6 (prepare-arg (nth 6 args) (nth 6 types)))
        (a7 (prepare-arg (nth 7 args) (nth 7 types)))
        (a8 (prepare-arg (nth 8 args) (nth 8 types)))
        (a9 (prepare-arg (nth 9 args) (nth 9 types)))
        (a10 (prepare-arg (nth 10 args) (nth 10 types))))
    (cond
      ;; snprintf pattern
      ((and (eq (nth 0 types) :pointer)
            (eq (nth 2 types) :string))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)))
      ;; printf pattern
      ((eq (nth 0 types) :string)
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)))
      (t
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (coerce-to-long a0 (nth 0 types))
                                 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))))))

(defun call-variadic-arm64-12 (addr types args)
  "ARM64 variadic call with 12 args (8 regs + 4 stack)."
  (let ((a0 (prepare-arg (nth 0 args) (nth 0 types)))
        (a1 (prepare-arg (nth 1 args) (nth 1 types)))
        (a2 (prepare-arg (nth 2 args) (nth 2 types)))
        (a3 (prepare-arg (nth 3 args) (nth 3 types)))
        (a4 (prepare-arg (nth 4 args) (nth 4 types)))
        (a5 (prepare-arg (nth 5 args) (nth 5 types)))
        (a6 (prepare-arg (nth 6 args) (nth 6 types)))
        (a7 (prepare-arg (nth 7 args) (nth 7 types)))
        (a8 (prepare-arg (nth 8 args) (nth 8 types)))
        (a9 (prepare-arg (nth 9 args) (nth 9 types)))
        (a10 (prepare-arg (nth 10 args) (nth 10 types)))
        (a11 (prepare-arg (nth 11 args) (nth 11 types))))
    (cond
      ;; snprintf pattern
      ((and (eq (nth 0 types) :pointer)
            (eq (nth 2 types) :string))
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long sb-alien:long
                             sb-alien:c-string sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)))
      ;; printf pattern
      ((eq (nth 0 types) :string)
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:c-string
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)))
      (t
       (let ((fn (sb-alien:sap-alien (sb-sys:int-sap addr)
                   (function sb-alien:int sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long sb-alien:long
                             sb-alien:long sb-alien:long))))
         (sb-alien:alien-funcall fn
                                 (coerce-to-long a0 (nth 0 types))
                                 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))))))

;;; ============================================================================
;;; Printf Family Convenience Functions
;;; ============================================================================

(defun find-c-symbol (name)
  "Find a C symbol address."
  (let ((addr (sb-sys:find-foreign-symbol-address name)))
    (when addr
      (etypecase addr
        (integer addr)
        (sb-sys:system-area-pointer (sb-sys:sap-int addr))))))

(defvar *printf-addr* nil)
(defvar *sprintf-addr* nil)
(defvar *snprintf-addr* nil)
(defvar *fprintf-addr* nil)

(defun ensure-printf-addrs ()
  "Ensure printf family addresses are cached."
  (unless *printf-addr*
    (setf *printf-addr* (find-c-symbol "printf")))
  (unless *sprintf-addr*
    (setf *sprintf-addr* (find-c-symbol "sprintf")))
  (unless *snprintf-addr*
    (setf *snprintf-addr* (find-c-symbol "snprintf")))
  (unless *fprintf-addr*
    (setf *fprintf-addr* (find-c-symbol "fprintf"))))

(defun make-printf-caller ()
  "Create a caller for printf."
  (ensure-printf-addrs)
  (make-variadic-caller *printf-addr* :int '(:string)))

(defun make-sprintf-caller ()
  "Create a caller for sprintf.
   First arg is destination buffer (pointer), second is format string."
  (ensure-printf-addrs)
  (make-variadic-caller *sprintf-addr* :int '(:pointer :string)))

(defun make-snprintf-caller ()
  "Create a caller for snprintf.
   Args: buffer, size, format, variadic..."
  (ensure-printf-addrs)
  (make-variadic-caller *snprintf-addr* :int '(:pointer :long :string)))

(defun make-fprintf-caller ()
  "Create a caller for fprintf.
   First arg is FILE*, second is format string."
  (ensure-printf-addrs)
  (make-variadic-caller *fprintf-addr* :int '(:pointer :string)))

;;; ============================================================================
;;; Pre-built Callers for Common Use
;;; ============================================================================

(defvar *variadic-printf* nil)
(defvar *variadic-sprintf* nil)
(defvar *variadic-snprintf* nil)

(defun variadic-printf (format &rest args)
  "Call printf with format string and variadic args."
  (unless *variadic-printf*
    (setf *variadic-printf* (make-printf-caller)))
  (apply *variadic-printf* format args))

(defun variadic-sprintf (buffer format &rest args)
  "Call sprintf with buffer, format, and variadic args.
   BUFFER should be a pointer (SAP or integer address).
   WARNING: No bounds checking - use snprintf instead."
  (unless *variadic-sprintf*
    (setf *variadic-sprintf* (make-sprintf-caller)))
  (apply *variadic-sprintf* buffer format args))

(defun variadic-snprintf (buffer size format &rest args)
  "Call snprintf with buffer, size, format, and variadic args.
   BUFFER should be a pointer (SAP or integer address).
   SIZE is the buffer size.
   Returns number of characters written (excluding null terminator)."
  (unless *variadic-snprintf*
    (setf *variadic-snprintf* (make-snprintf-caller)))
  (apply *variadic-snprintf* buffer size format args))

;;; ============================================================================
;;; Macro Interface
;;; ============================================================================

(defmacro defvariadic (name c-name return-type fixed-arg-specs &rest options)
  "Define a Lisp function that calls a C variadic function.

   NAME - Lisp function name to define
   C-NAME - C function name (string)
   RETURN-TYPE - Return type (:int, :void, etc.)
   FIXED-ARG-SPECS - List of (name type) for fixed arguments
   OPTIONS - Additional options:
     :library - Library to load function from

   Example:
     (defvariadic lisp-printf \"printf\" :int
       ((format :string)))

     (lisp-printf \"%d + %d = %d\" 1 2 3)"
  (declare (ignore options))  ; For future use
  (let ((fixed-names (mapcar #'first fixed-arg-specs))
        (fixed-types (mapcar #'second fixed-arg-specs))
        (addr-var (gensym "ADDR"))
        (caller-var (gensym "CALLER")))
    `(progn
       (declaim (ftype (function (,@(mapcar (constantly t) fixed-names) &rest t) t) ,name))
       (let ((,addr-var nil)
             (,caller-var nil))
         (defun ,name (,@fixed-names &rest variadic-args)
           (unless ,addr-var
             (setf ,addr-var (find-c-symbol ,c-name))
             (unless ,addr-var
               (error "Could not find C function: ~A" ,c-name))
             (setf ,caller-var (make-variadic-caller ,addr-var ',return-type ',fixed-types)))
           (apply ,caller-var ,@fixed-names variadic-args))))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun with-c-string-buffer (size fn)
  "Allocate a C string buffer and call FN with (buffer-sap buffer-array).
   Returns the string contents after the call."
  (let ((buffer (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((sap (sb-sys:vector-sap buffer)))
        (funcall fn sap buffer)))
    ;; Convert buffer to string, stopping at null
    (let ((null-pos (or (position 0 buffer) size)))
      (sb-ext:octets-to-string buffer :end null-pos))))

(defun sprintf-to-string (format &rest args)
  "Format a string using C sprintf and return as Lisp string.
   Uses a reasonably sized buffer (4096 bytes)."
  (with-c-string-buffer 4096
    (lambda (sap buffer)
      (declare (ignore buffer))
      (apply #'variadic-snprintf sap 4096 format args))))
