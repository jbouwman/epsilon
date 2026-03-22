;;;; jit.lisp - JIT-compiled FFI stubs
;;;;
;;;; Generates native machine code for FFI calls.

(defpackage epsilon.foreign.jit
  (:use cl)
  (:local-nicknames
   (lib epsilon.library))
  (:export
   ;; Stub cache
   #:*stub-cache*
   #:get-or-create-stub
   #:clear-stub-cache
   #:stub-cache-stats

   ;; Low-level
   #:make-executable-memory
   #:free-executable-memory
   #:call-stub

   ;; Stub generation
   #:generate-call-stub
   #:compile-stub

   ;; Debug/introspection
   #:disassemble-stub
   #:make-jit-caller

   ;; Platform detection
   #:*current-platform*
   #:platform-supported-p)
  (:enter t))

;;; ============================================================================
;;; Platform Detection
;;; ============================================================================

(defvar *current-platform*
  #+ARM64 :arm64
  #+X86-64 :x86-64
  #-(or ARM64 X86-64) :unknown
  "Current CPU architecture")

(defun platform-supported-p ()
  "Check if JIT compilation is supported on this platform."
  (not (null (member *current-platform* '(:arm64 :x86-64)))))

;;; ============================================================================
;;; Executable Memory Management
;;; ============================================================================

;; Use mmap to allocate memory with execute permission
;; On macOS, we need MAP_JIT for hardened runtime compatibility

(defconstant +page-size+ 4096)

;; mmap constants
(defconstant +prot-read+ 1)
(defconstant +prot-write+ 2)
(defconstant +prot-exec+ 4)
(defconstant +map-private+ 2)
(defconstant +map-anonymous+ #+LINUX #x20 #+DARWIN #x1000)
(defconstant +map-jit+ #+DARWIN #x800 #-DARWIN 0)

(sb-alien:define-alien-routine "mmap"
    sb-alien:system-area-pointer
  (addr sb-alien:system-area-pointer)
  (length sb-alien:unsigned-long)
  (prot sb-alien:int)
  (flags sb-alien:int)
  (fd sb-alien:int)
  (offset sb-alien:long))

(sb-alien:define-alien-routine "munmap"
    sb-alien:int
  (addr sb-alien:system-area-pointer)
  (length sb-alien:unsigned-long))

(sb-alien:define-alien-routine "mprotect"
    sb-alien:int
  (addr sb-alien:system-area-pointer)
  (length sb-alien:unsigned-long)
  (prot sb-alien:int))

;; ARM64: instruction cache must be invalidated after writing code
#+arm64
(sb-alien:define-alien-routine ("sys_icache_invalidate" sys-icache-invalidate)
    sb-alien:void
  (start sb-alien:system-area-pointer)
  (size sb-alien:unsigned-long))

;; macOS ARM64: SBCL provides sb-vm::jit-memcpy which handles W^X correctly
;; It uses pthread_jit_write_protect_np internally in a signal-safe way

(defstruct executable-region
  "A region of executable memory"
  (address nil :type (or null sb-sys:system-area-pointer))
  (size 0 :type fixnum)
  (used 0 :type fixnum))

(defun make-executable-memory (size)
  "Allocate SIZE bytes of executable memory. Returns an executable-region."
  (let* ((aligned-size (* +page-size+ (ceiling size +page-size+)))
         (prot (logior +prot-read+ +prot-write+ +prot-exec+))
         (flags (logior +map-private+ +map-anonymous+ +map-jit+))
         (addr (mmap (sb-sys:int-sap 0) aligned-size prot flags -1 0)))
    ;; Check for MAP_FAILED (returns -1 cast to pointer, which is all 1s)
    (when (= (sb-sys:sap-int addr) (1- (ash 1 64)))
      (error "mmap failed to allocate executable memory"))
    (make-executable-region :address addr :size aligned-size :used 0)))

(defun free-executable-memory (region)
  "Free an executable memory region."
  (when (executable-region-address region)
    (munmap (executable-region-address region)
            (executable-region-size region))
    (setf (executable-region-address region) nil)))

(defun write-to-executable (region offset bytes)
  "Write BYTES to REGION at OFFSET.
   Uses SBCL's jit-memcpy on ARM64 which handles W^X correctly."
  (let ((dest (sb-sys:sap+ (executable-region-address region) offset))
        (len (length bytes)))
    ;; On ARM64, sb-vm::jit-memcpy handles pthread_jit_write_protect_np
    ;; in a signal-safe way. It requires a simple-array, so coerce if needed.
    #+ARM64
    (let ((simple-bytes (if (typep bytes '(simple-array (unsigned-byte 8) (*)))
                            bytes
                            (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
      (sb-sys:with-pinned-objects (simple-bytes)
        (sb-vm::jit-memcpy dest (sb-sys:vector-sap simple-bytes) len)))
    #-ARM64
    (loop for i from 0 below len
          do (setf (sb-sys:sap-ref-8 dest i) (aref bytes i))))
  ;; Memory barrier to ensure writes are visible before execution
  (sb-thread:barrier (:memory))
  ;; ARM64: invalidate instruction cache so CPU fetches the new code
  #+arm64
  (sys-icache-invalidate (sb-sys:sap+ (executable-region-address region) offset)
                         (length bytes))
  (values))

;;; ============================================================================
;;; Stub Cache
;;; ============================================================================

(defstruct jit-stub
  "A compiled JIT stub"
  (address nil :type (or null sb-sys:system-area-pointer))
  (signature nil :type list)  ; (return-type . arg-types)
  (size 0 :type fixnum)
  (region nil)  ; Parent executable-region
  (caller nil :type (or null function)))  ; Compiled specialized caller

(defvar *stub-cache* (make-hash-table :test 'equal)
  "Cache of compiled stubs keyed by signature")

(defvar *stub-region* nil
  "Current executable memory region for stubs")

(defvar *stub-region-size* (* 64 1024)
  "Size of stub region (64KB)")

(defvar *stub-lock* (sb-thread:make-mutex :name "jit-stub-lock")
  "Mutex protecting stub cache and region from concurrent access.")

(defun ensure-stub-region ()
  "Ensure we have an executable region for stubs."
  (unless (and *stub-region*
               (executable-region-address *stub-region*))
    (setf *stub-region* (make-executable-memory *stub-region-size*))))

;; Eagerly allocate the stub region at load time to avoid
;; allocation failures after libclang callbacks corrupt the heap.
;; See IMPL-224 for background on libclang/GC interactions.
(when (platform-supported-p)
  (ensure-stub-region))

(defun get-or-create-stub (fn-addr return-type arg-types)
  "Get a cached stub or create a new one for the given signature.
   Thread-safe: all stub allocation and compilation is serialized."
  (let ((key (list* fn-addr return-type arg-types)))
    (sb-thread:with-mutex (*stub-lock*)
      (or (gethash key *stub-cache*)
          (let ((stub (allocate-stub fn-addr return-type arg-types)))
            ;; Compile specialized caller under lock - SBCL's compiler
            ;; uses shared mutable state that is not thread-safe.
            (setf (jit-stub-caller stub)
                  (make-specialized-caller (sb-sys:sap-int (jit-stub-address stub))
                                           return-type arg-types))
            (setf (gethash key *stub-cache*) stub)
            stub)))))

(defun clear-stub-cache ()
  "Clear the stub cache and free memory."
  (sb-thread:with-mutex (*stub-lock*)
    (clrhash *stub-cache*)
    (when *stub-region*
      (free-executable-memory *stub-region*)
      (setf *stub-region* nil))))

(defun stub-cache-stats ()
  "Return statistics about the stub cache."
  (list :stub-count (hash-table-count *stub-cache*)
        :region-size (if *stub-region* (executable-region-size *stub-region*) 0)
        :region-used (if *stub-region* (executable-region-used *stub-region*) 0)))

;;; ============================================================================
;;; Specialized Caller Generation
;;; ============================================================================

(defun make-specialized-caller (stub-addr return-type arg-types)
  "Compile a specialized caller function for the given stub address and signature.
   This eliminates runtime type dispatch and argument marshalling overhead."
  (let* ((n-args (length arg-types))
         (arg-names (loop for i from 0 below n-args
                          collect (intern (format nil "A~D" i))))
         (alien-ret (ffi-type-to-alien return-type))
         (alien-args (mapcar #'ffi-type-to-alien arg-types)))
    (compile nil
      `(lambda ,arg-names
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (sb-alien:alien-funcall
           (sb-alien:sap-alien (sb-sys:int-sap ,stub-addr)
             (function ,alien-ret ,@alien-args))
           ,@arg-names)))))

;;; ============================================================================
;;; Stub Compilation
;;; ============================================================================

(defun compile-stub (fn-addr return-type arg-types)
  "Compile a call stub for the given signature.
   Allocates machine code and creates a specialized caller function.
   Thread-safe."
  (sb-thread:with-mutex (*stub-lock*)
    (let ((stub (allocate-stub fn-addr return-type arg-types)))
      (setf (jit-stub-caller stub)
            (make-specialized-caller (sb-sys:sap-int (jit-stub-address stub))
                                     return-type arg-types))
      stub)))

(defun allocate-stub (fn-addr return-type arg-types)
  "Allocate and write machine code for a call stub. Must be called under *stub-lock*.
   Returns a jit-stub without a compiled caller."
  (unless (platform-supported-p)
    (error "JIT compilation not supported on ~A" *current-platform*))

  (ensure-stub-region)

  (let* ((code (generate-call-stub fn-addr return-type arg-types))
         (offset (executable-region-used *stub-region*))
         (stub-addr (sb-sys:sap+ (executable-region-address *stub-region*) offset)))

    ;; Check if we have room
    (when (> (+ offset (length code)) (executable-region-size *stub-region*))
      (error "Stub region exhausted"))

    ;; Write the code
    (write-to-executable *stub-region* offset code)

    ;; Update region usage
    (incf (executable-region-used *stub-region*) (length code))

    ;; Return stub without caller - caller compiled outside lock
    (make-jit-stub :address stub-addr
                   :signature (cons return-type arg-types)
                   :size (length code)
                   :region *stub-region*
                   :caller nil)))

(defun generate-call-stub (fn-addr return-type arg-types)
  "Generate machine code for a call stub."
  (case *current-platform*
    (:arm64 (generate-arm64-stub fn-addr return-type arg-types))
    (:x86-64 (generate-x86-64-stub fn-addr return-type arg-types))
    (t (error "Unsupported platform: ~A" *current-platform*))))

;;; ============================================================================
;;; ARM64 Code Generation
;;; ============================================================================
;;;
;;; ARM64 calling convention (AAPCS64):
;;; - x0-x7: integer/pointer arguments
;;; - d0-d7: floating point arguments
;;; - x0 or d0: return value
;;; - x30 (LR): link register
;;; - x16, x17: intra-procedure-call scratch registers
;;;
;;; Strategy for JIT stubs:
;;; The stub is a small wrapper that:
;;; 1. Saves the link register (since we'll BLR)
;;; 2. Loads the target function address into x16
;;; 3. Branches to it with BLR
;;; 4. Restores the link register
;;; 5. Returns
;;;
;;; Arguments are passed through unchanged from caller to callee.
;;; The stub acts as a trampoline with a hardcoded function address.

(defun generate-arm64-stub (fn-addr return-type arg-types)
  "Generate ARM64 machine code for a call stub.
   The stub loads the function address and branches to it."
  (declare (ignore return-type arg-types))

  (let ((code (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0)))

    ;; Prologue: Save LR to stack
    ;; SUB sp, sp, #16    ; Allocate 16 bytes (stack must be 16-byte aligned)
    ;; STR x30, [sp]      ; Save link register
    (emit-arm64-sub-imm code 31 31 16)  ; sub sp, sp, #16
    (emit-arm64-str code 30 31 0)       ; str x30, [sp]

    ;; Load function address into x16 (4 instructions for 64-bit immediate)
    (let ((addr fn-addr))
      (emit-arm64-movz code 16 (logand addr #xFFFF) 0)
      (emit-arm64-movk code 16 (logand (ash addr -16) #xFFFF) 1)
      (emit-arm64-movk code 16 (logand (ash addr -32) #xFFFF) 2)
      (emit-arm64-movk code 16 (logand (ash addr -48) #xFFFF) 3))

    ;; BLR x16 - Branch with link to address in x16
    (emit-arm64-word code (logior #xD63F0000 (ash 16 5)))

    ;; Epilogue: Restore LR and return
    ;; LDR x30, [sp]      ; Restore link register
    ;; ADD sp, sp, #16    ; Deallocate stack
    ;; RET
    (emit-arm64-ldr code 30 31 0)       ; ldr x30, [sp]
    (emit-arm64-add-imm code 31 31 16)  ; add sp, sp, #16
    (emit-arm64-word code #xD65F03C0)   ; ret

    code))

(defun emit-arm64-word (code word)
  "Emit a 32-bit ARM64 instruction (little-endian)."
  (vector-push-extend (logand word #xFF) code)
  (vector-push-extend (logand (ash word -8) #xFF) code)
  (vector-push-extend (logand (ash word -16) #xFF) code)
  (vector-push-extend (logand (ash word -24) #xFF) code))

(defun emit-arm64-movz (code rd imm16 shift)
  "Emit MOVZ Xd, #imm16, LSL #(shift*16)"
  ;; MOVZ: 1 10 100101 hw imm16 Rd
  ;; hw = shift (0-3)
  (let ((insn (logior #xD2800000
                      (ash shift 21)
                      (ash imm16 5)
                      rd)))
    (emit-arm64-word code insn)))

(defun emit-arm64-movk (code rd imm16 shift)
  "Emit MOVK Xd, #imm16, LSL #(shift*16)"
  ;; MOVK: 1 11 100101 hw imm16 Rd
  (let ((insn (logior #xF2800000
                      (ash shift 21)
                      (ash imm16 5)
                      rd)))
    (emit-arm64-word code insn)))

(defun emit-arm64-add-imm (code rd rn imm12)
  "Emit ADD Xd, Xn, #imm12"
  ;; ADD (immediate): 1 0 0 10001 sh imm12 Rn Rd
  ;; sh=0 for no shift
  (let ((insn (logior #x91000000
                      (ash (logand imm12 #xFFF) 10)
                      (ash rn 5)
                      rd)))
    (emit-arm64-word code insn)))

(defun emit-arm64-sub-imm (code rd rn imm12)
  "Emit SUB Xd, Xn, #imm12"
  ;; SUB (immediate): 1 1 0 10001 sh imm12 Rn Rd
  (let ((insn (logior #xD1000000
                      (ash (logand imm12 #xFFF) 10)
                      (ash rn 5)
                      rd)))
    (emit-arm64-word code insn)))

(defun emit-arm64-str (code rt rn offset)
  "Emit STR Xt, [Xn, #offset] - store 64-bit register"
  ;; STR (immediate, unsigned offset): 11 111 0 01 00 imm12 Rn Rt
  ;; imm12 is offset/8 for 64-bit variant
  (let ((insn (logior #xF9000000
                      (ash (floor offset 8) 10)
                      (ash rn 5)
                      rt)))
    (emit-arm64-word code insn)))

(defun emit-arm64-ldr (code rt rn offset)
  "Emit LDR Xt, [Xn, #offset] - load 64-bit register"
  ;; LDR (immediate, unsigned offset): 11 111 0 01 01 imm12 Rn Rt
  (let ((insn (logior #xF9400000
                      (ash (floor offset 8) 10)
                      (ash rn 5)
                      rt)))
    (emit-arm64-word code insn)))

;;; ============================================================================
;;; x86-64 Code Generation
;;; ============================================================================
;;;
;;; System V AMD64 ABI (Linux, macOS):
;;; - rdi, rsi, rdx, rcx, r8, r9: integer/pointer arguments
;;; - xmm0-xmm7: floating point arguments
;;; - rax: integer return value
;;; - xmm0: floating point return value
;;; - r10, r11: scratch registers (caller-saved)
;;;
;;; Strategy:
;;; Use r11 as scratch to hold the function address, then call it.
;;; Arguments pass through unchanged from caller to callee.

(defun generate-x86-64-stub (fn-addr return-type arg-types)
  "Generate x86-64 machine code for a call stub."
  (declare (ignore return-type arg-types))

  (let ((code (make-array 32 :element-type '(unsigned-byte 8) :fill-pointer 0)))

    ;; MOV r11, imm64 - load function address into r11
    ;; REX.WB prefix (49) + MOV r/m64, imm64 (BB+rd)
    ;; 49 BB <8 bytes little-endian>
    (vector-push-extend #x49 code)  ; REX.WB
    (vector-push-extend #xBB code)  ; MOV r11, imm64

    ;; Emit 64-bit address in little-endian
    (loop for i from 0 below 8
          do (vector-push-extend (logand (ash fn-addr (* i -8)) #xFF) code))

    ;; CALL r11 (indirect call through register)
    ;; REX.B prefix (41) + FF /2 (call r/m64)
    ;; 41 FF D3 = call r11
    (vector-push-extend #x41 code)  ; REX.B
    (vector-push-extend #xFF code)  ; CALL opcode
    (vector-push-extend #xD3 code)  ; ModR/M: mod=11, reg=2 (call), r/m=3 (r11)

    ;; RET
    (vector-push-extend #xC3 code)

    code))

;;; ============================================================================
;;; Stub Invocation
;;; ============================================================================

(defun call-stub (stub &rest args)
  "Call a JIT stub with the given arguments.
   Uses the pre-compiled specialized caller for optimal performance."
  (let ((caller (jit-stub-caller stub)))
    (if caller
        ;; Fast path: use pre-compiled specialized caller
        (apply caller args)
        ;; Slow path: fall back to generic invocation
        (let ((addr (jit-stub-address stub)))
          (when (null addr)
            (error "Stub has no address"))
          (let* ((signature (jit-stub-signature stub))
                 (return-type (car signature))
                 (arg-types (cdr signature)))
            (call-stub-with-signature addr return-type arg-types args))))))

(defun call-stub-with-signature (stub-addr return-type arg-types args)
  "Call a stub at STUB-ADDR with the given signature and arguments.
   The stub is called as a C function - it internally calls the target.
   ARGS should be a list of arguments."
  (declare (ignore arg-types))
  ;; For stubs that pass arguments through unchanged, we use a generic
  ;; calling approach. The stub handles forwarding args to the real function.
  ;;
  ;; Since the stub is a simple trampoline (load addr, call, ret), we can
  ;; call it directly with the same calling convention as the target.
  (case return-type
    (:void
     (apply #'invoke-stub-void stub-addr args))
    ((:int :uint :long :ulong :pointer)
     ;; For integer/pointer returns, use a generic integer return type
     (apply #'invoke-stub-int stub-addr args))
    (:float
     (apply #'invoke-stub-float stub-addr args))
    (:double
     (apply #'invoke-stub-double stub-addr args))
    (otherwise
     (apply #'invoke-stub-int stub-addr args))))

(defun invoke-stub-void (stub-addr &rest args)
  "Invoke stub returning void."
  (declare (ignore args))
  (let ((fn (sb-alien:sap-alien stub-addr (function sb-alien:void))))
    (sb-alien:alien-funcall fn)))

(defun invoke-stub-int (stub-addr &rest args)
  "Invoke stub returning integer. Uses internal SBCL mechanism."
  ;; Create a funcallable alien function
  (let ((fn (sb-alien:sap-alien stub-addr
                                (function sb-alien:long
                                          sb-alien:long sb-alien:long
                                          sb-alien:long sb-alien:long
                                          sb-alien:long sb-alien:long))))
    ;; Call with up to 6 args (matches register args on both ARM64 and x86-64)
    (let ((a (if (> (length args) 0) (nth 0 args) 0))
          (b (if (> (length args) 1) (nth 1 args) 0))
          (c (if (> (length args) 2) (nth 2 args) 0))
          (d (if (> (length args) 3) (nth 3 args) 0))
          (e (if (> (length args) 4) (nth 4 args) 0))
          (f (if (> (length args) 5) (nth 5 args) 0)))
      (sb-alien:alien-funcall fn a b c d e f))))

(defun invoke-stub-double (stub-addr &rest args)
  "Invoke stub returning double. Handles 0-6 double arguments."
  (let ((n (length args)))
    (case n
      (0
       (let ((fn (sb-alien:sap-alien stub-addr (function sb-alien:double))))
         (sb-alien:alien-funcall fn)))
      (1
       (let ((fn (sb-alien:sap-alien stub-addr
                                     (function sb-alien:double sb-alien:double))))
         (sb-alien:alien-funcall fn (float (nth 0 args) 1.0d0))))
      (2
       (let ((fn (sb-alien:sap-alien stub-addr
                                     (function sb-alien:double
                                               sb-alien:double sb-alien:double))))
         (sb-alien:alien-funcall fn
                                 (float (nth 0 args) 1.0d0)
                                 (float (nth 1 args) 1.0d0))))
      (3
       (let ((fn (sb-alien:sap-alien stub-addr
                                     (function sb-alien:double
                                               sb-alien:double sb-alien:double
                                               sb-alien:double))))
         (sb-alien:alien-funcall fn
                                 (float (nth 0 args) 1.0d0)
                                 (float (nth 1 args) 1.0d0)
                                 (float (nth 2 args) 1.0d0))))
      (otherwise
       ;; 4+ args: use 6-arg form, padding with zeros
       (let ((fn (sb-alien:sap-alien stub-addr
                                     (function sb-alien:double
                                               sb-alien:double sb-alien:double
                                               sb-alien:double sb-alien:double
                                               sb-alien:double sb-alien:double))))
         (sb-alien:alien-funcall fn
                                 (if (> n 0) (float (nth 0 args) 1.0d0) 0.0d0)
                                 (if (> n 1) (float (nth 1 args) 1.0d0) 0.0d0)
                                 (if (> n 2) (float (nth 2 args) 1.0d0) 0.0d0)
                                 (if (> n 3) (float (nth 3 args) 1.0d0) 0.0d0)
                                 (if (> n 4) (float (nth 4 args) 1.0d0) 0.0d0)
                                 (if (> n 5) (float (nth 5 args) 1.0d0) 0.0d0)))))))

(defun invoke-stub-float (stub-addr &rest args)
  "Invoke stub returning float. Handles 0-6 float arguments."
  (let ((n (length args)))
    (case n
      (0
       (let ((fn (sb-alien:sap-alien stub-addr (function sb-alien:float))))
         (sb-alien:alien-funcall fn)))
      (1
       (let ((fn (sb-alien:sap-alien stub-addr
                                     (function sb-alien:float sb-alien:float))))
         (sb-alien:alien-funcall fn (float (nth 0 args) 1.0))))
      (2
       (let ((fn (sb-alien:sap-alien stub-addr
                                     (function sb-alien:float
                                               sb-alien:float sb-alien:float))))
         (sb-alien:alien-funcall fn
                                 (float (nth 0 args) 1.0)
                                 (float (nth 1 args) 1.0))))
      (otherwise
       ;; 3+ args: use 6-arg form
       (let ((fn (sb-alien:sap-alien stub-addr
                                     (function sb-alien:float
                                               sb-alien:float sb-alien:float
                                               sb-alien:float sb-alien:float
                                               sb-alien:float sb-alien:float))))
         (sb-alien:alien-funcall fn
                                 (if (> n 0) (float (nth 0 args) 1.0) 0.0)
                                 (if (> n 1) (float (nth 1 args) 1.0) 0.0)
                                 (if (> n 2) (float (nth 2 args) 1.0) 0.0)
                                 (if (> n 3) (float (nth 3 args) 1.0) 0.0)
                                 (if (> n 4) (float (nth 4 args) 1.0) 0.0)
                                 (if (> n 5) (float (nth 5 args) 1.0) 0.0)))))))

(defun ffi-type-to-alien (type)
  "Convert FFI type keyword to sb-alien type specifier.
   Note: Pointers are represented as integers to allow passing integer addresses
   directly without requiring explicit SAP conversion."
  (case type
    (:void 'sb-alien:void)
    (:int 'sb-alien:int)
    (:uint 'sb-alien:unsigned-int)
    (:long 'sb-alien:long)
    (:ulong 'sb-alien:unsigned-long)
    (:short 'sb-alien:short)
    (:ushort 'sb-alien:unsigned-short)
    (:char 'sb-alien:char)
    (:uchar 'sb-alien:unsigned-char)
    (:float 'sb-alien:float)
    (:double 'sb-alien:double)
    ;; Use unsigned-long for pointers - allows passing integer addresses directly
    ;; This matches how the slow path (invoke-stub-int) handles pointers
    (:pointer 'sb-alien:unsigned-long)
    (:size-t 'sb-alien:unsigned-long)
    (:ssize-t 'sb-alien:long)
    (otherwise 'sb-alien:int)))

;;; ============================================================================
;;; High-level Interface
;;; ============================================================================

(defun make-jit-caller (fn-addr return-type arg-types)
  "Create a JIT-compiled function caller for the given foreign function.
   Returns the specialized caller function directly for optimal performance."
  (let ((stub (get-or-create-stub fn-addr return-type arg-types)))
    (or (jit-stub-caller stub)
        ;; Fallback wrapper if no specialized caller
        (lambda (&rest args)
          (apply #'call-stub stub args)))))

(defun disassemble-stub (stub &optional (stream *standard-output*))
  "Print a hex dump of the stub's machine code."
  (let* ((addr (jit-stub-address stub))
         (size (jit-stub-size stub)))
    (format stream "~&Stub at ~X, ~D bytes:~%" (sb-sys:sap-int addr) size)
    (format stream "  Platform: ~A~%" *current-platform*)
    (format stream "  Signature: ~S~%" (jit-stub-signature stub))
    (format stream "  Code:~%")
    (loop for i from 0 below size
          do (format stream "~:[~;~%~]~2,'0X "
                     (zerop (mod i 16))
                     (sb-sys:sap-ref-8 addr i)))
    (terpri stream)))
