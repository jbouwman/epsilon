(defpackage epsilon.foreign
  (:use cl)
  (:local-nicknames
   (sym epsilon.symbol)
   (map epsilon.map)
   (seq epsilon.sequence)
   (path epsilon.path)
   (lib epsilon.library)
   (log epsilon.log)
   (trampoline epsilon.foreign.trampoline)
   (marshalling epsilon.foreign.marshalling)
   (callback epsilon.foreign.callback)
   ;; Struct support (JIT-based)
   (jit-struct epsilon.foreign.jit.struct))
  (:export
   ;; Core FFI
   shared-call
   lib-open
   lib-close
   lib-function
   defshared

   ;; Utilities
   diagnose-ffi-call
   resolve-function-address
   ffi-call

   ;; Type Management
   define-foreign-struct
   define-ffi-struct
   with-foreign-struct
   foreign-array

   ;; Memory Management
   foreign-alloc
   foreign-free
   with-foreign-memory
   register-finalizer

   ;; Type Mapping
   def-type-map
   *primitive-type-map*
   *custom-type-map*

   ;; Trampoline-based interface
   defshared-fast
   shared-call-fast

   ;; Re-export from trampoline module
   make-ffi-trampoline
   get-or-create-trampoline
   c-type
   c-type-p
   c-type-base
   c-type-size
   c-type-alignment
   c-type-signed-p
   get-c-type
   ffi-signature
   ffi-signature-p
   ffi-signature-return-type
   ffi-signature-arg-types
   ffi-signature-trampoline
   register-signature
   get-signature
   clear-signature-registry
   convert-to-foreign
   convert-from-foreign

   ;; Re-export from marshalling module
   infer-function-signature
   with-pinned-array
   with-string-array
   define-enum
   enum-value
   enum-keyword
   define-c-type
   foreign-error
   foreign-error-p
   foreign-error-code
   foreign-error-function
   bool-to-foreign
   foreign-to-bool

   ;; Re-exported from callback module via sym:reexport at end of file

   ;; Public API and configuration
   ffi-system-status)
  (:enter t))

;;;; Core FFI Functions

;;; shared-call: Main entry point for FFI calls
;;;
;;; Parameters:
;;;   function-designator - Either a symbol or (symbol library-name)
;;;   return-type - Lisp representation of C return type
;;;   arg-types - List of argument types
;;;   &rest args - Actual arguments to pass
;;;
;;; Example: (shared-call (:printf "libc") :int (:string) "Hello %s\n" "world")

;; Global state for library management

;; Library management delegated to epsilon.library
;; Library management functions
(defvar *open-libraries* (map:make-map)
  "Map of opened library handles")

;; Library management functions are now delegated to epsilon.library
(defun lib-open (library-name &key local paths)
  "Opens a shared library and returns a handle - delegates to epsilon.library"
  (lib:lib-open library-name :local local :paths paths))

(defun lib-close (library-handle)
  "Closes a previously opened library - delegates to epsilon.library"
  (lib:lib-close library-handle))

(defun lib-function (library-handle function-name)
  "Get function pointer from library - delegates to epsilon.library"
  (lib:lib-function library-handle function-name))

;; Removed find-library-in-paths - now using epsilon.library version

(defun platform-library-name (base-name)
  "Convert base library name to platform-specific format"
  (lib:platform-library-name base-name))

(defun pathname-absolute-p (path)
  "Check if a pathname string is absolute"
  (and (stringp path) (> (length path) 0) (char= (char path 0) #\/)))

(defun resolve-function-address (function-designator)
  "Resolve function designator to memory address."
  (etypecase function-designator
    (string
     ;; String function name, assume libc
     (lib:lib-function (lib:lib-open "libc") function-designator))
    (symbol
     (lib:lib-function (lib:lib-open "libc") (string function-designator)))
    (list
     (destructuring-bind (fn-name lib-name) function-designator
       (lib:lib-function (lib:lib-open lib-name) (string fn-name))))
    (integer
     function-designator))) ; Already an address

(defun shared-call (function-designator return-type arg-types &rest args)
  "Main entry point for FFI calls using SBCL's native trampoline system"
  ;; Resolve function address
  (let ((function-address (resolve-function-address function-designator)))
    (unless function-address
      (error "Could not find function ~A" function-designator))
    ;; Use trampoline system for the call
    (trampoline:call-with-trampoline function-address return-type arg-types args)))

;; Type conversion and mapping system

(defvar *primitive-type-map*
  (map:make-map :char 'sb-alien:char
                :unsigned-char 'sb-alien:unsigned-char
                :short 'sb-alien:short
                :unsigned-short 'sb-alien:unsigned-short
                :int 'sb-alien:int
                :unsigned-int 'sb-alien:unsigned-int
                :long 'sb-alien:long
                :unsigned-long 'sb-alien:unsigned-long
                :float 'sb-alien:single-float
                :double 'sb-alien:double-float
                :pointer 'sb-alien:system-area-pointer
                :string 'sb-alien:c-string
                :void 'sb-alien:void)
  "Map of Lisp type keywords to SBCL alien types")

(defun lisp-type-to-alien (type)
  "Convert Lisp type specifier to SBCL alien type"
  (cond
    ((keywordp type)
     (or (map:get *primitive-type-map* type)
         (error "Unknown primitive type: ~A" type)))
    ((and (listp type) (eq (first type) :pointer))
     `(sb-alien:* ,(lisp-type-to-alien (second type))))
    ((and (listp type) (eq (first type) :array))
     `(sb-alien:array ,(lisp-type-to-alien (second type)) ,(third type)))
    (t
     (error "Unsupported type specifier: ~A" type))))

;; Now delegating to trampoline:convert-to-foreign
(defun convert-to-foreign (value type)
  "Convert Lisp value to foreign representation"
  (trampoline:convert-to-foreign value type))

;;; defshared: Defines a Lisp function that calls a C function
;;; Creates optimized calling paths based on type information
;;;
;;; Parameters:
;;;   lisp-name - Symbol to define in Lisp
;;;   c-name - String naming the C function
;;;   library - Library name or handle
;;;   return-type - Return type specification
;;;   arg-specs - List of (arg-name arg-type) pairs
;;;   &key documentation - Optional docstring
;;;
;;; Example:
;;; (defshared my-printf "printf" "libc" :int (format :string)
;;;            :documentation "Calls C printf function")

(defmacro defshared (lisp-name c-name library return-type &rest args)
  "Defines a Lisp function that calls a C function"
  (let* ((doc-pos (position :documentation args))
         (documentation (when doc-pos (nth (1+ doc-pos) args)))
         (arg-specs (if doc-pos
                        (append (subseq args 0 doc-pos)
                                (subseq args (+ doc-pos 2)))
                        args))
         ;; Filter out empty lists which represent no arguments
         (arg-specs (remove-if (lambda (spec) (and (listp spec) (null spec))) arg-specs))
         (arg-names (when arg-specs (mapcar #'first arg-specs)))
         (arg-types (when arg-specs (mapcar #'second arg-specs))))
    `(defun ,lisp-name ,arg-names
       ,@(when documentation (list documentation))
       (shared-call (list ,c-name ,library) ,return-type ',arg-types ,@arg-names))))

;;;; Type Management

;;; define-foreign-struct: Defines a foreign structure layout
;;; Delegates to epsilon.foreign.jit.struct:define-jit-struct for
;;; efficient struct passing.
;;;
;;; Parameters:
;;;   name - Symbol naming the structure
;;;   &rest fields - List of field definitions (name type)
;;;
;;; Example:
;;; (define-foreign-struct timespec
;;;   (sec :long)
;;;   (nsec :long))

(defmacro define-foreign-struct (name &rest fields)
  "Define a foreign struct for use with FFI.

   Delegates to the JIT struct system which handles:
   - Layout calculation with proper alignment
   - Efficient struct passing (register packing for small structs)
   - Field accessors

   Example:
     (define-foreign-struct point
       (x :double)
       (y :double))"
  `(jit-struct:define-jit-struct ,name ,@fields))

(defmacro define-ffi-struct (name &rest fields)
  "Define a struct for FFI with BOTH jit-struct metadata AND sb-alien type.

   This is the recommended way to define structs when you need struct-by-value
   returns. The sb-alien type enables correct calling convention handling.

   Creates:
   - jit-struct definition for layout queries and field access
   - sb-alien struct type for use in sb-alien:define-alien-routine

   Example:
     (define-ffi-struct xxh128-hash-t
       (low64 :unsigned-long)
       (high64 :unsigned-long))

     ;; Then use with sb-alien:define-alien-routine:
     (sb-alien:define-alien-routine (\"XXH3_128bits\" %xxh3-128bits)
         (sb-alien:struct xxh128-hash-t)
       (input sb-alien:system-area-pointer)
       (length sb-alien:unsigned-long))"
  `(jit-struct:define-ffi-struct ,name ,@fields))

;;; with-foreign-struct: Allocates a foreign structure and binds it
;;;
;;; Parameters:
;;;   (var type) - Binding and structure type
;;;   &body body - Code to execute with binding
;;;
;;; Example:
;;; (with-foreign-struct (ts timespec)
;;;   (setf (struct-slot ts 'sec) 10)
;;;   (my-function ts))

(defmacro with-foreign-struct ((var type) &body body)
  "Allocate and bind a foreign struct for the duration of BODY.

   The struct is allocated as a byte array that can be passed to foreign
   functions. Use struct-field-ref to access fields.

   Example:
     (define-foreign-struct point (x :double) (y :double))
     (with-foreign-struct (p point)
       (setf (jit-struct:struct-field-ref p 'point 'x) 1.0d0)
       (my-function p))"
  (let ((size-var (gensym "SIZE"))
        (buffer-var (gensym "BUFFER")))
    `(let* ((,size-var (or (jit-struct:jit-struct-size ',type)
                           (error "Unknown struct type: ~A" ',type)))
            (,buffer-var (make-array ,size-var
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
       (sb-sys:with-pinned-objects (,buffer-var)
         (let ((,var (sb-sys:sap-int (sb-sys:vector-sap ,buffer-var))))
           ,@body)))))

;;; foreign-array: Creates an array suitable for foreign code
;;;
;;; Parameters:
;;;   element-type - Type of array elements (:float, :double, :int, etc.)
;;;   dimensions - Array dimensions
;;;   &key initial-contents - Initial data
;;;        initial-element - Initial element value
;;;
;;; Returns: A simple-array that can be passed to foreign code

(defun foreign-array (element-type dimensions &key initial-contents initial-element)
  "Create an array suitable for passing to foreign code.

   ELEMENT-TYPE - FFI type keyword (:float, :double, :int, etc.)
   DIMENSIONS - Array dimensions (integer or list)
   INITIAL-CONTENTS - Optional initial data
   INITIAL-ELEMENT - Optional initial element value

   Returns a simple-array with appropriate element type.

   Example:
     (let ((arr (foreign-array :double '(100))))
       (sb-sys:with-pinned-objects (arr)
         (my-function (sb-sys:vector-sap arr))))"
  (let ((lisp-element-type (case element-type
                             ((:float :single-float) 'single-float)
                             ((:double :double-float) 'double-float)
                             ((:char :int8) '(signed-byte 8))
                             ((:uchar :uint8 :unsigned-char) '(unsigned-byte 8))
                             ((:short :int16) '(signed-byte 16))
                             ((:ushort :uint16) '(unsigned-byte 16))
                             ((:int :int32) '(signed-byte 32))
                             ((:uint :uint32) '(unsigned-byte 32))
                             ((:long :int64) '(signed-byte 64))
                             ((:ulong :uint64) '(unsigned-byte 64))
                             ((:pointer) '(unsigned-byte 64))
                             (otherwise t)))
        (dims (if (listp dimensions) dimensions (list dimensions))))
    (cond
      (initial-contents
       (make-array dims
                   :element-type lisp-element-type
                   :initial-contents initial-contents))
      (initial-element
       (make-array dims
                   :element-type lisp-element-type
                   :initial-element (coerce initial-element lisp-element-type)))
      (t
       (make-array dims
                   :element-type lisp-element-type
                   :initial-element (coerce 0 lisp-element-type))))))

;;;; Memory Management

;; Global hash table to track allocated memory
(defvar *allocated-memory* (make-hash-table :test 'eql)
  "Maps SAPs to their underlying alien pointers for proper cleanup")

(defvar *allocated-memory-mutex* (sb-thread:make-mutex :name "allocated-memory")
  "Mutex for thread-safe access to *allocated-memory*")

(defun foreign-alloc (type-or-size &key count initial-element initial-contents finalize)
  "Allocates foreign memory and returns a system area pointer"
  (let* ((element-size (if (keywordp type-or-size)
                           (alien-type-size type-or-size)
                           type-or-size))
         (total-size (* element-size (or count 1)))
         (alien-ptr (sb-alien:make-alien sb-alien:char total-size))
         (sap (sb-alien:alien-sap alien-ptr)))

    ;; Store the alien pointer for later cleanup
    (sb-thread:with-mutex (*allocated-memory-mutex*)
      (setf (gethash (sb-sys:sap-int sap) *allocated-memory*) alien-ptr))

    ;; Initialize memory if requested
    (cond
      (initial-contents
       (loop for i from 0
             for value in (coerce initial-contents 'list)
             while (< i total-size)
             do (setf (sb-alien:deref alien-ptr i) value)))
      (initial-element
       (loop for i from 0 below total-size
             do (setf (sb-alien:deref alien-ptr i) initial-element)))
      (t ;; Zero-initialize by default for safety
       (loop for i from 0 below total-size
             do (setf (sb-alien:deref alien-ptr i) 0))))

    ;; Register finalizer if requested
    (when finalize
      (let ((sap-int (sb-sys:sap-int sap)))
        (sb-ext:finalize sap (lambda ()
                               (sb-thread:with-mutex (*allocated-memory-mutex*)
                                 (let ((ptr (gethash sap-int *allocated-memory*)))
                                   (when ptr
                                     (sb-alien:free-alien ptr)
                                     (remhash sap-int *allocated-memory*))))))))

    ;; Return the system area pointer
    sap))

(defun foreign-free (pointer)
  "Explicitly frees foreign memory"
  (when pointer
    (let* ((sap-int (if (sb-sys:system-area-pointer-p pointer)
                        (sb-sys:sap-int pointer)
                        pointer)))
      (sb-thread:with-mutex (*allocated-memory-mutex*)
        (let ((alien-ptr (gethash sap-int *allocated-memory*)))
          (when alien-ptr
            ;; Free the alien memory
            (sb-alien:free-alien alien-ptr)
            ;; Remove from tracking table
            (remhash sap-int *allocated-memory*)
            t))))))

(defun alien-type-size (type)
  "Get size in bytes of alien type"
  (case type
    (:char 1)
    (:short 2)
    (:int 4)
    (:long 8)
    (:float 4)
    (:double 8)
    (:pointer 8)
    (otherwise 1)))

(defmacro with-foreign-memory (bindings &body body)
  "Allocates memory for the duration of body"
  (let ((cleanup-forms '())
        (binding-forms '()))
    (dolist (binding bindings)
      (destructuring-bind (var type-or-size &rest keys) binding
        (push `(,var (foreign-alloc ,type-or-size ,@keys)) binding-forms)
        (push `(foreign-free ,var) cleanup-forms)))
    `(let ,(reverse binding-forms)
       (unwind-protect
            (progn ,@body)
         ,@cleanup-forms))))

(defun register-finalizer (object function)
  "Registers a function to run when object is GC'd"
  (sb-ext:finalize object function))

;;;; Type Mapping

;;; Custom type map for user-defined type conversions

(defvar *custom-type-map* (make-hash-table :test 'eq)
  "Map of custom type conversions.
   Key: lisp type symbol
   Value: plist with :c-type :to-foreign :from-foreign :direct")

;;; def-type-map: Defines mapping between Lisp and C types
;;; Creates conversion functions for custom types
;;;
;;; Parameters:
;;;   lisp-type - Lisp type specifier (symbol)
;;;   c-type - C type specifier (:int, :pointer, etc.)
;;;   &key to-foreign - Function to convert Lisp->C
;;;        from-foreign - Function to convert C->Lisp
;;;        direct - If true, indicates no conversion needed
;;;
;;; Example:
;;; (def-type-map my-string :pointer
;;;   :to-foreign (lambda (s) (sb-ext:string-to-octets s :null-terminate t))
;;;   :from-foreign (lambda (p) (sb-ext:octets-to-string (pointer-to-octets p))))

(defmacro def-type-map (lisp-type c-type &key to-foreign from-foreign direct)
  "Define a custom type mapping for FFI conversions.

   LISP-TYPE - Symbol naming the Lisp type
   C-TYPE - FFI type keyword (:int, :pointer, :double, etc.)
   TO-FOREIGN - Function (lisp-value) -> foreign-value
   FROM-FOREIGN - Function (foreign-value) -> lisp-value
   DIRECT - If true, no conversion needed (types are compatible)

   Example:
     ;; Define a boolean type that maps to C int
     (def-type-map my-bool :int
       :to-foreign (lambda (v) (if v 1 0))
       :from-foreign (lambda (v) (not (zerop v))))

     ;; Use in defshared
     (defshared my-func \"my_func\" \"mylib\" my-bool (x my-bool))"
  `(setf (gethash ',lisp-type *custom-type-map*)
         (list :c-type ,c-type
               :to-foreign ,to-foreign
               :from-foreign ,from-foreign
               :direct ,direct)))

(defun get-type-map (lisp-type)
  "Get the type mapping for a Lisp type."
  (gethash lisp-type *custom-type-map*))

(defun convert-custom-to-foreign (value type)
  "Convert a value using a custom type map."
  (let ((map (get-type-map type)))
    (if map
        (let ((converter (getf map :to-foreign))
              (direct (getf map :direct)))
          (cond
            (direct value)
            (converter (funcall converter value))
            (t value)))
        value)))

(defun convert-custom-from-foreign (value type)
  "Convert a value from foreign using a custom type map."
  (let ((map (get-type-map type)))
    (if map
        (let ((converter (getf map :from-foreign))
              (direct (getf map :direct)))
          (cond
            (direct value)
            (converter (funcall converter value))
            (t value)))
        value)))

;;; *primitive-type-map* - Variable holding primitive type mappings
;;; Maps Lisp primitive types to their C equivalents
;;; Used by shared-call for efficient type conversion

(defun ffi-call (function-address return-type arg-types &rest args)
  "Direct FFI call with function address using the trampoline system"
  (trampoline:call-with-trampoline function-address return-type arg-types args))

;;;; Trampoline-based FFI

(defun shared-call-fast (function-designator return-type arg-types &rest args)
  "Fast FFI call using compiled trampolines instead of eval"
  (let ((function-address
          (etypecase function-designator
            (symbol (lib:lib-function
                     (lib:lib-open "libc")
                     (string function-designator)))
            (list (destructuring-bind (fn-name lib-name) function-designator
                    (lib:lib-function
                     (lib:lib-open lib-name)
                     (string fn-name)))))))
    (unless function-address
      (error "Could not find function ~A" function-designator))
    ;; Use trampoline system
    (trampoline:call-with-trampoline function-address return-type arg-types args)))

(defmacro defshared-fast (lisp-name c-name library return-type &rest args)
  "Fast version of defshared using trampolines"
  (let* ((doc-pos (position :documentation args))
         (documentation (when doc-pos (nth (1+ doc-pos) args)))
         (arg-specs (if doc-pos
                        (append (subseq args 0 doc-pos)
                                (subseq args (+ doc-pos 2)))
                        args))
         ;; Filter out empty lists which represent no arguments
         (arg-specs (remove-if (lambda (spec) (and (listp spec) (null spec))) arg-specs))
         (arg-names (when arg-specs (mapcar #'first arg-specs)))
         (arg-types (when arg-specs (mapcar #'second arg-specs))))
    `(progn
       ;; Register the signature
       (trampoline:register-signature ',lisp-name ,return-type ',arg-types)
       ;; Define the function
       (defun ,lisp-name ,arg-names
         ,@(when documentation (list documentation))
         (shared-call-fast (list ,c-name ,library) ,return-type ',arg-types ,@arg-names)))))

;; Helper functions for type conversion - these are actually needed
(defun convert-from-foreign (value type)
  "Convert foreign value to Lisp representation"
  (trampoline:convert-from-foreign value type))

;; Re-export bool conversion functions from marshalling
(defun bool-to-foreign (value &optional type)
  "Convert Lisp boolean to C bool"
  (marshalling:bool-to-foreign value type))

(defun foreign-to-bool (value &optional type)
  "Convert C bool to Lisp boolean"
  (marshalling:foreign-to-bool value type))

;; Re-export error handling from marshalling
(defun foreign-error-p (condition)
  "Check if condition is a foreign error"
  (typep condition 'marshalling:foreign-error))

(defun foreign-error-code (condition)
  "Get error code from foreign error"
  (marshalling:foreign-error-code condition))

(defun foreign-error-function (condition)
  "Get function name from foreign error"
  (marshalling:foreign-error-function condition))

;; Re-export the condition type
(deftype foreign-error ()
  'marshalling:foreign-error)

;; Re-export define-c-type
(defun define-c-type (name size &rest args)
  "Define a new C type"
  (apply #'marshalling:define-c-type name size args))

;; Re-export smart defshared macro - now handled by smart-ffi module


;;;; System status and diagnostics

(defun ffi-system-status ()
  "Display FFI system status"
  (format t "~%Epsilon FFI System Status~%")
  (format t "=========================~%~%")

  ;; Core system
  (format t "Core System:~%")
  (format t "  SBCL version: ~A~%" (lisp-implementation-version))
  (format t "  Platform: ~A ~A~%" (machine-type) (machine-version))
  (format t "  FFI backend: SBCL native trampoline~%")

  ;; Signature system - check authoritative libclang-based system first
  (format t "~%Signature Discovery:~%")

  ;; Check signature discovery system
  (if (find-package :epsilon.foreign.signatures)
      (let ((cache-symbol (find-symbol "*SIGNATURE-CACHE*" :epsilon.foreign.signatures)))
        (if cache-symbol
            (format t "  libclang-based (authoritative): Available~%~
                       ~4TCached signatures: ~D~%"
                    (hash-table-count (symbol-value cache-symbol)))
            (format t "  libclang-based (authoritative): Package found but cache unavailable~%")))
      (format t "  libclang-based (authoritative): Not available~%"))

  (format t "~%"))

;;;; Re-export from trampoline module
(sym:reexport :epsilon.foreign.trampoline
              '(make-ffi-trampoline get-or-create-trampoline
                c-type c-type-p c-type-base c-type-size c-type-alignment c-type-signed-p
                get-c-type
                ffi-signature ffi-signature-p ffi-signature-return-type
                ffi-signature-arg-types ffi-signature-trampoline
                register-signature get-signature clear-signature-registry
                convert-to-foreign convert-from-foreign))

;;;; Re-export from marshalling module
(sym:reexport :epsilon.foreign.marshalling
              '(infer-function-signature
                with-pinned-array with-string-array with-output-array
                define-enum enum-value enum-keyword
                define-c-type
                foreign-error foreign-error-p foreign-error-code foreign-error-function
                bool-to-foreign foreign-to-bool))

;;;; Re-export from callback module
(sym:reexport :epsilon.foreign.callback
              '(make-callback call-callback callback-pointer
                register-callback unregister-callback get-callback list-callbacks))

(sym:reexport-types :epsilon.foreign.callback
                    '("CALLBACK-INFO"))

