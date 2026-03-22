;;;; integration.lisp - Integration between libclang signatures and JIT compilation
;;;;
;;;; Provides high-level macros for defining JIT-compiled foreign functions
;;;; with automatic signature discovery from C headers.

(defpackage epsilon.foreign.jit.integration
  (:use cl)
  (:local-nicknames
   (jit epsilon.foreign.jit))
  (:export
   ;; High-level macros
   #:defjit
   #:defjit-inline

   ;; Runtime helpers
   #:jit-funcall
   #:get-jit-caller
   #:with-jit-library

   ;; Signature helpers
   #:discover-signature
   #:validate-signature)
  (:enter t))

;;; ============================================================================
;;; Library and Symbol Resolution
;;; ============================================================================

(defvar *current-library* nil
  "Currently bound library for symbol resolution")

(defun find-symbol-address (name &optional library)
  "Find the address of a foreign symbol.
   NAME - String name of the symbol
   LIBRARY - Optional library path (currently unused, uses default search)"
  (declare (ignore library))
  (let ((result (sb-sys:find-foreign-symbol-address name)))
    (etypecase result
      (integer result)
      (sb-sys:system-area-pointer (sb-sys:sap-int result))
      (null nil))))

(defmacro with-jit-library (library &body body)
  "Execute BODY with LIBRARY as the default for symbol resolution."
  `(let ((*current-library* ,library))
     ,@body))

;;; ============================================================================
;;; Runtime JIT Calling
;;; ============================================================================

(defun get-jit-caller (name return-type arg-types &optional library)
  "Get or create a JIT-compiled caller for the named function.
   Returns a function that can be called with the appropriate arguments."
  (let ((addr (find-symbol-address name library)))
    (unless addr
      (error "Foreign symbol not found: ~A" name))
    (jit:make-jit-caller addr return-type arg-types)))

(defun jit-funcall (name return-type arg-types &rest args)
  "Call a foreign function by name with JIT compilation.
   This is the dynamic calling path - slower than defjit but more flexible."
  (let ((caller (get-jit-caller name return-type arg-types)))
    (apply caller args)))

;;; ============================================================================
;;; defjit Macro - Main Interface
;;; ============================================================================

(defmacro defjit (lisp-name c-name return-type (&rest args) &key library documentation)
  "Define a JIT-compiled foreign function.

   LISP-NAME - Symbol naming the Lisp function
   C-NAME - String name of the C function
   RETURN-TYPE - Return type keyword (:int, :double, :void, :pointer, etc.)
   ARGS - List of (name type) pairs for arguments
   :LIBRARY - Optional library to search for the symbol
   :DOCUMENTATION - Optional documentation string

   Example:
   (defjit lisp-sin \"sin\" :double ((x :double))
     :documentation \"Compute sine of X\")

   The generated function calls the C function through a JIT-compiled stub,
   achieving near-native performance after the first call."
  (let* ((arg-names (mapcar #'first args))
         (arg-types (mapcar #'second args))
         (caller-var (gensym "CALLER")))
    `(let ((,caller-var nil))
       (defun ,lisp-name ,arg-names
         ,@(when documentation (list documentation))
         ;; Lazy initialization of JIT caller
         (unless ,caller-var
           (let ((addr (find-symbol-address ,c-name ,library)))
             (unless addr
               (error "Foreign symbol not found: ~A" ,c-name))
             (setf ,caller-var (jit:make-jit-caller addr ,return-type ',arg-types))))
         (funcall ,caller-var ,@arg-names)))))

(defmacro defjit-inline (lisp-name c-name return-type (&rest args) &key library documentation)
  "Define a JIT-compiled foreign function with inline stub creation.

   Like DEFJIT but creates the JIT caller at macro expansion time.
   This provides the fastest possible call path but requires the foreign
   symbol to be available at load time.

   Example:
   (defjit-inline fast-sin \"sin\" :double ((x :double)))

   Note: This macro evaluates at load time, so the library must be loaded
   before the definition is evaluated."
  (let* ((arg-names (mapcar #'first args))
         (arg-types (mapcar #'second args))
         (addr (find-symbol-address c-name library)))
    (unless addr
      (error "Foreign symbol not found at macro expansion time: ~A" c-name))
    `(let ((caller (jit:make-jit-caller ,addr ,return-type ',arg-types)))
       (defun ,lisp-name ,arg-names
         ,@(when documentation (list documentation))
         (funcall caller ,@arg-names)))))

;;; ============================================================================
;;; Signature Discovery (Integration with libclang when available)
;;; ============================================================================

(defun discover-signature (function-name &key headers)
  "Attempt to discover the signature of a C function.

   FUNCTION-NAME - String name of the C function
   HEADERS - List of header files to search

   Returns (values return-type arg-types) or NIL if not found.

   Note: Use epsilon.foreign.signatures:discover-signature-auto instead,
   which provides full libclang-based signature discovery."
  (declare (ignore function-name headers))
  nil)

(defun validate-signature (return-type arg-types)
  "Validate that a signature uses supported types.
   Signals an error if any type is not supported."
  (flet ((validate-type (type context)
           (unless (member type '(:void :int :uint :long :ulong
                                  :short :ushort :char :uchar
                                  :float :double :pointer
                                  :size-t :ssize-t))
             (error "Unsupported ~A type: ~A" context type))))
    (validate-type return-type "return")
    (dolist (arg-type arg-types)
      (validate-type arg-type "argument")))
  t)
