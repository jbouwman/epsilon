(defpackage :epsilon.sys.lib
  (:use :cl)
  (:export
   ;; Core FFI
   #:shared-call
   #:lib-open
   #:lib-close
   #:lib-function
   #:defshared
   
   ;; Type Management
   #:define-foreign-struct
   #:with-foreign-struct
   #:map-struct
   #:foreign-array
   #:with-zero-copy
   
   ;; Memory Management
   #:foreign-alloc
   #:foreign-free
   #:with-foreign-memory
   #:register-finalizer
   
   ;; Structure Discovery
   #:grovel-struct
   #:grovel-lib
   #:parse-header
   
   ;; Type Mapping
   #:def-type-map
   #:*primitive-type-map*))

(in-package :epsilon.sys.lib)

;;;; Core FFI Functions

;;; shared-call: Main entry point for efficient FFI calls
;;; Makes direct FFI calls with minimal overhead
;;; Uses type information to optimize call paths
;;; Performs direct LLVM-level integration where possible
;;; 
;;; Parameters:
;;;   function-designator - Either a symbol or (symbol library-name)
;;;   return-type - Lisp representation of C return type
;;;   arg-types - List of argument types
;;;   &rest args - Actual arguments to pass
;;;
;;; Example: (shared-call (:printf "libc") :int (:string) "Hello %s\n" "world")

(defun shared-call (function-designator return-type arg-types &rest args))

;;; lib-open: Opens a shared library and returns a handle
;;; Handles platform-specific library naming conventions
;;; Caches opened libraries to prevent redundant loading
;;;
;;; Parameters:
;;;   library-name - Name of the library (e.g., "libc" or "libc.so.6")
;;;   &key local - If true, searches only in local paths
;;;        paths - Additional paths to search
;;;
;;; Returns: Library handle for use in other functions

(defun lib-open (library-name &key local paths))

;;; lib-close: Closes a previously opened library
;;;
;;; Parameters:
;;;   library-handle - Handle returned from lib-open

(defun lib-close (library-handle))

;;; lib-function: Gets a pointer to a function in a library
;;; Optimized to cache function lookups for repeated use
;;;
;;; Parameters:
;;;   library-handle - Handle returned from lib-open
;;;   function-name - String or symbol naming the function
;;;
;;; Returns: Function pointer usable with shared-call

(defun lib-function (library-handle function-name))

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

(defmacro defshared (lisp-name c-name library return-type &rest arg-specs &key documentation))

;;;; Type Management

;;; define-foreign-struct: Defines a foreign structure layout
;;; Uses clang to determine actual memory layout
;;; Creates optimized accessors for structure fields
;;; Handles alignment and packing automatically
;;;
;;; Parameters:
;;;   name - Symbol naming the structure
;;;   &rest fields - List of field definitions (name type &key offset)
;;;   &key from-header - C header file to extract from
;;;        c-name - Name in C if different from Lisp name
;;;
;;; Example:
;;; (define-foreign-struct timespec
;;;   (sec :long)
;;;   (nsec :long)
;;;   :from-header "time.h")

(defmacro define-foreign-struct (name &rest fields &key from-header c-name))

;;; with-foreign-struct: Allocates a foreign structure and binds it
;;; Zero-copy where possible for efficiency
;;;
;;; Parameters:
;;;   (var type) - Binding and structure type
;;;   &body body - Code to execute with binding
;;;
;;; Example:
;;; (with-foreign-struct (ts 'timespec)
;;;   (setf (struct-slot ts 'sec) 10)
;;;   (my-function ts))

(defmacro with-foreign-struct ((var type) &body body))

;;; map-struct: Maps a Lisp structure to/from a foreign structure
;;; Enables zero-copy sharing of structure data
;;;
;;; Parameters:
;;;   lisp-value - Lisp structure or object
;;;   foreign-type - Foreign structure type
;;;   direction - :to-foreign or :from-foreign or :bidirectional
;;;
;;; Returns: Foreign structure pointer or updated Lisp value

(defun map-struct (lisp-value foreign-type &optional (direction :bidirectional)))

;;; foreign-array: Creates or maps an array for foreign code
;;; Zero-copy array sharing with foreign code
;;; Handles multi-dimensional arrays
;;;
;;; Parameters:
;;;   element-type - Type of array elements
;;;   dimensions - List of array dimensions
;;;   &key initial-contents - Initial data
;;;        existing-array - Lisp array to share
;;;        foreign-pointer - Foreign memory to use
;;;
;;; Returns: Array object that can be passed to foreign code

(defun foreign-array (element-type dimensions &key initial-contents existing-array foreign-pointer))

;;; with-zero-copy: Executes body with zero-copy foreign data sharing
;;; Optimizes data transfer between Lisp and C
;;;
;;; Parameters:
;;;   bindings - List of (var lisp-value foreign-type) triplets
;;;   &body body - Code to execute with bindings
;;;
;;; Example:
;;; (with-zero-copy ((farr my-array :float-array)
;;;                  (fstruct my-struct 'mystruct))
;;;   (my-c-function farr fstruct))

(defmacro with-zero-copy (bindings &body body))

;;;; Memory Management

;;; foreign-alloc: Allocates foreign memory
;;; Integrates with SBCL's GC for better memory management
;;;
;;; Parameters:
;;;   type-or-size - Foreign type or byte count
;;;   &key count - Number of elements for arrays
;;;        initial-element - Value to initialize with
;;;        initial-contents - Sequence to copy
;;;        finalize - Whether to auto-free memory
;;;
;;; Returns: Pointer to allocated memory

(defun foreign-alloc (type-or-size &key count initial-element initial-contents finalize))

;;; foreign-free: Explicitly frees foreign memory
;;;
;;; Parameters:
;;;   pointer - Foreign memory pointer to free

(defun foreign-free (pointer))

;;; with-foreign-memory: Allocates memory for the duration of body
;;; Ensures memory is freed when body exits
;;;
;;; Parameters:
;;;   bindings - List of (var type-or-size &key ...) as in foreign-alloc
;;;   &body body - Code to execute with bindings
;;;
;;; Example:
;;; (with-foreign-memory ((buffer :char :count 1024)
;;;                        (ints :int :count 10 :initial-element 0))
;;;   (use-buffers buffer ints))

(defmacro with-foreign-memory (bindings &body body))

;;; register-finalizer: Registers a function to run when object is GC'd
;;; Helps manage foreign resources with Lisp's garbage collector
;;;
;;; Parameters:
;;;   object - Lisp object to attach finalizer to
;;;   function - Function to run when object is collected
;;;
;;; Example:
;;; (register-finalizer 
;;;   my-wrapper-object 
;;;   (lambda (obj) (foreign-free (wrapper-pointer obj))))

(defun register-finalizer (object function))

;;;; Structure Discovery

;;; grovel-struct: Extracts structure layout from C
;;; Uses clang to determine exact memory layout
;;;
;;; Parameters:
;;;   struct-name - Name of C structure
;;;   &key headers - List of headers to include
;;;        include-dirs - Additional include directories
;;;
;;; Returns: Structure description for use with define-foreign-struct

(defun grovel-struct (struct-name &key headers include-dirs))

;;; grovel-lib: Extracts function information from a library
;;; Uses various tools (nm, objdump, clang) to get function signatures
;;;
;;; Parameters:
;;;   library-name - Name of library to analyze
;;;   &key headers - Related headers to parse
;;;        include-dirs - Additional include directories
;;;
;;; Returns: List of function descriptions

(defun grovel-lib (library-name &key headers include-dirs))

;;; parse-header: Extracts type information from C header files
;;; Creates Lisp-accessible descriptions of C types
;;;
;;; Parameters:
;;;   header-file - Header file to parse
;;;   &key include-dirs - Additional include directories
;;;        types - Specific types to extract (nil for all)
;;;        recursive - Whether to follow included headers
;;;
;;; Returns: Hash table mapping C types to their descriptions

(defun parse-header (header-file &key include-dirs types recursive))

;;;; Type Mapping

;;; def-type-map: Defines mapping between Lisp and C types
;;; Creates optimized conversion paths for types
;;;
;;; Parameters:
;;;   lisp-type - Lisp type specifier
;;;   c-type - C type specifier
;;;   &key to-foreign - Function to convert Lisp->C
;;;        from-foreign - Function to convert C->Lisp
;;;        direct - If true, indicates no conversion needed
;;;
;;; Example:
;;; (def-type-map 'string '(:pointer :char)
;;;   :to-foreign #'string-to-c-string
;;;   :from-foreign #'c-string-to-string)

(defmacro def-type-map (lisp-type c-type &key to-foreign from-foreign direct))

;;; *primitive-type-map* - Variable holding primitive type mappings
;;; Maps Lisp primitive types to their C equivalents
;;; Used by shared-call for efficient type conversion

(defvar *primitive-type-map*)

