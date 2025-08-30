(defpackage epsilon.foreign
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (seq epsilon.sequence)
   (path epsilon.path)
   (lib epsilon.library)
   (trampoline epsilon.foreign.trampoline)
   (marshalling epsilon.foreign.marshalling)
   (struct epsilon.foreign.struct)
   (callback epsilon.foreign.callback)
   (callback-impl epsilon.foreign.callback-impl)
   (clang-sigs epsilon.clang.signatures))
  (:export
   ;; Core FFI
   shared-call
   shared-call-unified
   lib-open
   lib-close
   lib-function
   defshared

   ;; Utilities
   diagnose-ffi-call
   test-libffi-integration
   
   ;; Configuration
   *use-libffi-calls*
   *libffi-function-whitelist*
   *libffi-function-blacklist*
   *track-call-performance*
   
   ;; Module utilities
   get-module-root
   
   ;; libffi bridge functions
   load-libffi-extension
   libffi-call
   *libffi-library*
   resolve-function-address
   ffi-call
   ffi-call-auto
   
   ;; Type Management
   define-foreign-struct
   with-foreign-struct
   map-struct
   foreign-array
   with-zero-copy
   
   ;; Memory Management
   foreign-alloc
   foreign-free
   with-foreign-memory
   register-finalizer
   
   ;; Structure Discovery
   grovel-struct
   grovel-lib
   parse-header
   
   ;; Type Mapping
   def-type-map
   *primitive-type-map*
   
   ;; New trampoline-based interface
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
   with-output-array
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
   
   ;; Re-export from struct module
   define-c-struct
   define-c-struct-auto
   define-c-union
   parse-c-struct
   struct-layout-p
   get-struct-layout
   struct-layout-size
   struct-layout-alignment
   struct-field-offset
   struct-field-type
   struct-field-size
   struct-has-field-p
   with-c-struct
   with-c-union
   with-struct-view
   with-foreign-object
   struct-ref
   struct-ref-ptr
   union-ref
   struct-pointer
   struct-to-bytes
   bytes-to-struct
   struct-to-string
   
   ;; Re-export from callback module
   make-callback
   call-callback
   callback-pointer
   register-callback
   unregister-callback
   get-callback
   list-callbacks
   defcallback
   with-callback
   with-callback-scope
   callback-info
   callback-info-p
   callback-info-function
   callback-info-signature
   callback-info-pointer
   
   ;; Public API and configuration
   ffi-system-status
   get-call-statistics))

(in-package epsilon.foreign)

;;;; libffi Integration
;;;; SBCL bridge to libffi C extension, enabling real callback creation

;;; Type mapping constants (must match C extension)
(defconstant +epsilon-type-void+          0)
(defconstant +epsilon-type-int+           1)
(defconstant +epsilon-type-long+          2)
(defconstant +epsilon-type-float+         3)
(defconstant +epsilon-type-double+        4)
(defconstant +epsilon-type-pointer+       5)
(defconstant +epsilon-type-string+        6)
(defconstant +epsilon-type-bool+          7)
(defconstant +epsilon-type-unsigned-int+  8)
(defconstant +epsilon-type-unsigned-long+ 9)

;;; Global state
(defvar *libffi-library* nil
  "Handle to the loaded libffi extension library")

(defvar *libffi-callbacks* (map:make-map)
  "Registry mapping callback IDs to Lisp callback information")

(defvar *libffi-mutex* (sb-thread:make-mutex :name "libffi-callbacks")
  "Mutex for thread-safe callback operations")

;;; Module path resolution
(defun get-module-root ()
  "Get the root directory of the current module"
  (or 
   ;; Method 1: Use load/compile pathname - go up from src/foreign.lisp to module root
   (when (or *load-pathname* *compile-file-pathname*)
     (let* ((path (or *load-pathname* *compile-file-pathname*))
            (dir (pathname-directory path)))
       ;; Check if we're in a fasl directory structure
       (if (member "fasl" dir :test #'string=)
           ;; In fasl dir: find "modules" in path and construct from there
           (let ((modules-pos (position "modules" dir :test #'string= :from-end t)))
             (when modules-pos
               (make-pathname 
                :directory (append (subseq dir 0 (1+ modules-pos)) '("foreign")))))
           ;; Normal source path: go up two levels from src/foreign.lisp
           (make-pathname 
            :directory (butlast (butlast dir))))))
   ;; Method 2: Try relative to current directory
   (probe-file #P"modules/foreign/")
   ;; Method 3: Error if we can't find it
   (error "Cannot determine module directory")))

;;; Library loading
(defun load-libffi-extension ()
  "Load the libffi C extension library"
  (when *libffi-library*
    (return-from load-libffi-extension *libffi-library*))
  
  (let* ((module-root (get-module-root))
         ;; Convert SBCL's "X86-64" to standard "x86_64"
         (arch (string-downcase (substitute #\_ #\- (machine-type))))
         (platform-lib (format nil "lib/libepsilon-libffi-~A-~A.~A"
                              #+linux "linux" #+darwin "darwin" #-(or linux darwin) "unknown"
                              arch
                              #+linux "so" #+darwin "dylib" #-(or linux darwin) "so"))
         (extension-path (merge-pathnames platform-lib module-root)))
    
    (unless (probe-file extension-path)
      (error "libffi extension not found at ~A" extension-path))
    
    (setf *libffi-library* (sb-alien:load-shared-object extension-path))
    (format t "Loaded libffi extension from ~A~%" extension-path)
    *libffi-library*))

;;; Foreign function definitions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-libffi-function (name return-type &rest args)
    "Define a foreign function from the libffi extension"
    `(sb-alien:define-alien-routine ,name ,return-type ,@args)))

(define-libffi-function epsilon-libffi-test sb-alien:int)

(define-libffi-function epsilon-create-callback sb-alien:int
  (lisp-function sb-alien:system-area-pointer)
  (return-type sb-alien:int)
  (arg-types (* sb-alien:int))
  (nargs sb-alien:int))

(define-libffi-function epsilon-get-callback-pointer sb-alien:system-area-pointer
  (callback-id sb-alien:int))

(define-libffi-function epsilon-destroy-callback sb-alien:int
  (callback-id sb-alien:int))

(define-libffi-function epsilon-cleanup-all-callbacks sb-alien:void)

(define-libffi-function epsilon-get-callback-count sb-alien:int)

(define-libffi-function epsilon-get-last-error sb-alien:c-string)

;;; Function call support
(define-libffi-function epsilon-prep-cif sb-alien:int
  (return-type sb-alien:int)
  (arg-types (* sb-alien:int))
  (nargs sb-alien:int)
  (cif-ptr (* sb-alien:system-area-pointer)))

(define-libffi-function epsilon-call-function sb-alien:int
  (cif sb-alien:system-area-pointer)
  (function-ptr sb-alien:system-area-pointer)
  (args (* sb-alien:system-area-pointer))
  (result sb-alien:system-area-pointer))

(define-libffi-function epsilon-free-cif sb-alien:void
  (cif sb-alien:system-area-pointer))

;;; Type conversion utilities
(defun lisp-type-to-epsilon-type (lisp-type)
  "Convert Lisp type symbols to epsilon type constants"
  (case lisp-type
    ((:void void) +epsilon-type-void+)
    ((:int int integer) +epsilon-type-int+)
    ((:long long) +epsilon-type-long+)
    ((:unsigned-int unsigned-int) +epsilon-type-unsigned-int+)
    ((:unsigned-long unsigned-long) +epsilon-type-unsigned-long+)
    ((:float float single-float) +epsilon-type-float+)
    ((:double double double-float) +epsilon-type-double+)
    ((:pointer pointer sb-alien:system-area-pointer) +epsilon-type-pointer+)
    ((:string :c-string string c-string) +epsilon-type-string+)
    ((:bool boolean) +epsilon-type-bool+)
    (t (error "Unsupported type for libffi callback: ~A" lisp-type))))

(defun make-epsilon-type-array (arg-types)
  "Create C array of epsilon type constants from Lisp type list"
  (let* ((count (length arg-types))
         (array (sb-alien:make-alien sb-alien:int count)))
    (loop for i from 0
          for type in arg-types
          do (setf (sb-alien:deref array i) (lisp-type-to-epsilon-type type)))
    array))

;;; Callback registry management
(defstruct callback-info
  id
  function
  return-type
  arg-types
  pointer)

(defun register-libffi-callback (id function return-type arg-types pointer)
  "Register a callback in the Lisp-side registry"
  (sb-thread:with-mutex (*libffi-mutex*)
    (setf *libffi-callbacks* (map:assoc *libffi-callbacks* id
                                               (make-callback-info :id id
                                                                   :function function
                                                                   :return-type return-type
                                                                   :arg-types arg-types
                                                                   :pointer pointer)))))

(defun unregister-libffi-callback (id)
  "Remove a callback from the Lisp-side registry"
  (sb-thread:with-mutex (*libffi-mutex*)
    (setf *libffi-callbacks* (map:dissoc *libffi-callbacks* id))))

(defun get-libffi-callback (id)
  "Retrieve callback info by ID"
  (sb-thread:with-mutex (*libffi-mutex*)
    (map:get *libffi-callbacks* id)))

;;; High-level callback creation interface
(defun make-libffi-callback (function return-type arg-types)
  "Create a real callback using libffi
   Returns the C function pointer or signals an error"
  (unless *libffi-library*
    (load-libffi-extension))
  
  ;; Validate arguments
  (unless (functionp function)
    (error "First argument must be a function"))
  
  (unless (and (listp arg-types) (every #'symbolp arg-types))
    (error "arg-types must be a list of type symbols"))
  
  (let* ((epsilon-return-type (lisp-type-to-epsilon-type return-type))
         (nargs (length arg-types))
         (epsilon-arg-types (make-epsilon-type-array arg-types))
         (lisp-function-ptr (sb-sys:int-sap 
                             (sb-kernel:get-lisp-obj-address function))))
    
    (unwind-protect
         (let ((callback-id (epsilon-create-callback lisp-function-ptr
                                                     epsilon-return-type
                                                     epsilon-arg-types
                                                     nargs)))
           (if (< callback-id 0)
               (error "Failed to create libffi callback: ~A" 
                      (epsilon-get-last-error))
               (let ((pointer (epsilon-get-callback-pointer callback-id)))
                 (if (zerop (sb-sys:sap-int pointer))
                     (progn
                       (epsilon-destroy-callback callback-id)
                       (error "Failed to get callback pointer: ~A"
                              (epsilon-get-last-error)))
                     (progn
                       (register-libffi-callback callback-id function 
                                                 return-type arg-types pointer)
                       pointer)))))
      ;; Clean up the temporary array
      (sb-alien:free-alien epsilon-arg-types))))

;;; Cleanup utilities
(defun cleanup-libffi-callbacks ()
  "Clean up all libffi callbacks"
  (when *libffi-library*
    (epsilon-cleanup-all-callbacks)
    (sb-thread:with-mutex (*libffi-mutex*)
      (setf *libffi-callbacks* (map:make-map)))))

;;; Test and diagnostic functions
(defun test-libffi-extension ()
  "Test if libffi extension is working"
  (unless *libffi-library*
    (load-libffi-extension))
  
  (let ((result (epsilon-libffi-test)))
    (if (= result 42)
        (format t "libffi extension test passed (returned ~A)~%" result)
        (format t "libffi extension test failed (returned ~A, expected 42)~%" result))
    (= result 42)))

(defun libffi-callback-count ()
  "Get count of active libffi callbacks"
  (if *libffi-library*
      (epsilon-get-callback-count)
      0))

;;; CIF (Call Interface) management for function calls

(defvar *cif-cache* (make-hash-table :test 'equal)
  "Cache of prepared call interfaces")

(defvar *cif-mutex* (sb-thread:make-mutex :name "libffi-cif")
  "Mutex for thread-safe CIF operations")

(defstruct cif-info
  pointer
  return-type
  arg-types
  ref-count)

(defun get-or-create-cif (return-type arg-types)
  "Get cached or create new call interface"
  ;; Ensure extension is loaded
  (unless *libffi-library*
    (load-libffi-extension))
  
  (let ((signature (list return-type arg-types)))
    (sb-thread:with-mutex (*cif-mutex*)
      (let ((existing (gethash signature *cif-cache*)))
        (if existing
            (progn
              (incf (cif-info-ref-count existing))
              existing)
            (let ((cif-info (prepare-cif return-type arg-types)))
              (setf (gethash signature *cif-cache*) cif-info)
              cif-info))))))

(defun prepare-cif (return-type arg-types)
  "Prepare a call interface for the given signature"
  (let* ((epsilon-return-type (lisp-type-to-epsilon-type return-type))
         (nargs (length arg-types))
         (epsilon-arg-types (make-epsilon-type-array arg-types))
         (cif-ptr-holder (sb-alien:make-alien sb-alien:system-area-pointer)))
    
    (unwind-protect
         (let ((result (epsilon-prep-cif epsilon-return-type
                                        epsilon-arg-types
                                        nargs
                                        cif-ptr-holder)))
           (if (< result 0)
               (error "Failed to prepare CIF: ~A" (epsilon-get-last-error))
               (let ((cif-ptr (sb-alien:deref cif-ptr-holder)))
                 (make-cif-info :pointer cif-ptr
                               :return-type return-type
                               :arg-types arg-types
                               :ref-count 1))))
      ;; Clean up the temporary array
      (sb-alien:free-alien epsilon-arg-types))))

(defun release-cif (cif-info)
  "Release a call interface (reference counted)"
  (sb-thread:with-mutex (*cif-mutex*)
    (when (zerop (decf (cif-info-ref-count cif-info)))
      ;; Remove from cache when ref count reaches zero
      (let ((signature (list (cif-info-return-type cif-info)
                            (cif-info-arg-types cif-info))))
        (remhash signature *cif-cache*)
        (epsilon-free-cif (cif-info-pointer cif-info))))))

;;; High-level function call interface

(defun libffi-call (function-address return-type arg-types args)
  "Make FFI call using libffi instead of hardcoded signatures"
  (let ((cif-info (get-or-create-cif return-type arg-types)))
    (unwind-protect
         (multiple-value-bind (converted-args holders)
             (convert-args-to-pointers args arg-types)
           ;; Keep holders alive during the entire call by binding them
           (unwind-protect
                (let* ((result-holder (allocate-result-holder return-type))
                       (fn-sap (if (sb-sys:system-area-pointer-p function-address)
                                  function-address
                                  (sb-sys:int-sap function-address)))
                       (call-result (epsilon-call-function (cif-info-pointer cif-info)
                                                           fn-sap
                                                           converted-args
                                                           result-holder)))
                  (if (< call-result 0)
                      (error "FFI call failed: ~A" (epsilon-get-last-error))
                      (extract-result result-holder return-type)))
             ;; holders keeps memory alive through lexical binding
             (when holders nil)))
      (release-cif cif-info))))

(defun convert-args-to-pointers (args arg-types)
  "Convert Lisp arguments to array of pointers for libffi"
  (let* ((nargs (length args))
         (arg-array (sb-alien:make-alien sb-alien:system-area-pointer nargs))
         ;; Keep holders alive during the call
         (holders '()))
    (loop for i from 0
          for arg in args
          for type in arg-types
          do (multiple-value-bind (converted holder) 
                 (convert-arg-to-pointer arg type)
               (setf (sb-alien:deref arg-array i) converted)
               (when holder
                 (push holder holders))))
    ;; Return both array and holders to keep them alive
    (values arg-array holders)))

(defun convert-arg-to-pointer (arg type)
  "Convert single argument to pointer for libffi call. Returns (values pointer holder-to-keep-alive)"
  (case type
    (:int (let ((holder (sb-alien:make-alien sb-alien:int)))
            (setf (sb-alien:deref holder) arg)
            (values (sb-alien:alien-sap holder) holder)))
    (:long (let ((holder (sb-alien:make-alien sb-alien:long)))
             (setf (sb-alien:deref holder) arg)
             (values (sb-alien:alien-sap holder) holder)))
    (:unsigned-int (let ((holder (sb-alien:make-alien sb-alien:unsigned-int)))
                    (setf (sb-alien:deref holder) arg)
                    (values (sb-alien:alien-sap holder) holder)))
    (:unsigned-long (let ((holder (sb-alien:make-alien sb-alien:unsigned-long)))
                     (setf (sb-alien:deref holder) arg)
                     (values (sb-alien:alien-sap holder) holder)))
    (:float (let ((holder (sb-alien:make-alien sb-alien:single-float)))
              (setf (sb-alien:deref holder) (coerce arg 'single-float))
              (values (sb-alien:alien-sap holder) holder)))
    (:pointer (let* ((ptr (if (sb-sys:system-area-pointer-p arg)
                              arg
                              (sb-sys:int-sap (if (integerp arg) arg 0))))
                    (holder (sb-alien:make-alien sb-alien:system-area-pointer)))
                (setf (sb-alien:deref holder) ptr)
                (values (sb-alien:alien-sap holder) holder)))
    ((:string :c-string) (if (stringp arg)
                 ;; Convert Lisp string to C string and wrap in pointer
                 (let* ((c-string (sb-alien:make-alien-string arg))
                        (string-sap (sb-alien:alien-sap c-string))
                        (holder (sb-alien:make-alien sb-alien:system-area-pointer)))
                   (setf (sb-alien:deref holder) string-sap)
                   ;; Return both pointer and objects to keep alive
                   (values (sb-alien:alien-sap holder) (list holder c-string)))
                 (error "Expected string for :string type")))
    (otherwise (error "Unsupported argument type: ~A" type))))

(defun allocate-result-holder (return-type)
  "Allocate memory to hold function result"
  (case return-type
    (:void (sb-sys:int-sap 0))
    (:int (sb-alien:alien-sap (sb-alien:make-alien sb-alien:int)))
    (:long (sb-alien:alien-sap (sb-alien:make-alien sb-alien:long)))
    (:unsigned-int (sb-alien:alien-sap (sb-alien:make-alien sb-alien:unsigned-int)))
    (:unsigned-long (sb-alien:alien-sap (sb-alien:make-alien sb-alien:unsigned-long)))
    (:float (sb-alien:alien-sap (sb-alien:make-alien sb-alien:single-float)))
    (:pointer (sb-alien:alien-sap (sb-alien:make-alien sb-alien:system-area-pointer)))
    (otherwise (error "Unsupported return type: ~A" return-type))))

(defun extract-result (result-holder return-type)
  "Extract result from libffi call"
  (case return-type
    (:void nil)
    (:int (sb-alien:deref (sb-alien:sap-alien result-holder (* sb-alien:int))))
    (:long (sb-alien:deref (sb-alien:sap-alien result-holder (* sb-alien:long))))
    (:unsigned-int (sb-alien:deref (sb-alien:sap-alien result-holder (* sb-alien:unsigned-int))))
    (:unsigned-long (sb-alien:deref (sb-alien:sap-alien result-holder (* sb-alien:unsigned-long))))
    (:float (sb-alien:deref (sb-alien:sap-alien result-holder (* sb-alien:single-float))))
    (:pointer (sb-alien:deref (sb-alien:sap-alien result-holder (* sb-alien:system-area-pointer))))
    (otherwise (error "Unsupported return type: ~A" return-type))))

;;; Integration with existing FFI infrastructure

(defun libffi-available-for-calls-p ()
  "Check if libffi is available for function calls (not just callbacks)"
  (and *libffi-library*
       (let ((addr (sb-sys:find-dynamic-foreign-symbol-address "epsilon_prep_cif")))
         (and addr (not (zerop addr))))))

;;; Ensure foreign functions are available
(defun ensure-libffi-functions ()
  "Ensure libffi foreign functions are properly linked"
  (when *libffi-library*
    ;; Check if functions are available
    (let ((test-addr (sb-sys:find-dynamic-foreign-symbol-address "epsilon_libffi_test")))
      (when (and test-addr (not (zerop test-addr)))
        t))))

;;; Initialize on load
(eval-when (:load-toplevel :execute)
  (load-libffi-extension)
  (ensure-libffi-functions))

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

(defun shared-call (function-designator return-type arg-types &rest args)
  "Main entry point for FFI calls using libffi"
  ;; Delegate to the libffi implementation
  (apply #'shared-call-unified function-designator return-type arg-types args))

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
       (shared-call (list ,c-name ,library) ,return-type ',(or arg-types '()) ,@arg-names))))

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

(defmacro define-foreign-struct (name &rest fields &key from-header c-name)
  "Stub implementation - define a foreign struct"
  (declare (ignore fields from-header c-name))
  `(defstruct ,name))

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

(defmacro with-foreign-struct ((var type) &body body)
  "Stub implementation - allocate foreign struct"
  (declare (ignore type))
  `(let ((,var (foreign-alloc 64))) ; Allocate 64 bytes for any struct
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

;;; map-struct: Maps a Lisp structure to/from a foreign structure
;;; Enables zero-copy sharing of structure data
;;;
;;; Parameters:
;;;   lisp-value - Lisp structure or object
;;;   foreign-type - Foreign structure type
;;;   direction - :to-foreign or :from-foreign or :bidirectional
;;;
;;; Returns: Foreign structure pointer or updated Lisp value

(defun map-struct (lisp-value foreign-type &optional (direction :bidirectional))
  "Stub implementation - map between Lisp and foreign structs"
  (declare (ignore foreign-type direction))
  lisp-value)

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

(defun foreign-array (element-type dimensions &key initial-contents existing-array foreign-pointer)
  "Stub implementation - create foreign array"
  (declare (ignore element-type initial-contents existing-array foreign-pointer))
  (apply #'make-array dimensions))

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

(defmacro with-zero-copy (bindings &body body)
  "Stub implementation - zero-copy data sharing"
  (let ((var-bindings (mapcar (lambda (binding)
                                `(,(first binding) ,(second binding)))
                              bindings)))
    `(let ,var-bindings
       ,@body)))

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

(defun grovel-struct (struct-name &key headers include-dirs)
  "Stub implementation - extract C struct layout"
  (declare (ignore struct-name headers include-dirs))
  nil)

;;; grovel-lib: Extracts function information from a library
;;; Uses various tools (nm, objdump, clang) to get function signatures
;;;
;;; Parameters:
;;;   library-name - Name of library to analyze
;;;   &key headers - Related headers to parse
;;;        include-dirs - Additional include directories
;;;
;;; Returns: List of function descriptions

(defun grovel-lib (library-name &key headers include-dirs)
  "Stub implementation - extract library functions"
  (declare (ignore library-name headers include-dirs))
  nil)

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

(defun parse-header (header-file &key include-dirs types recursive)
  "Stub implementation - parse C headers"
  (declare (ignore header-file include-dirs types recursive))
  map:+empty+)

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

(defmacro def-type-map (lisp-type c-type &key to-foreign from-foreign direct)
  "Stub implementation - define type mapping"
  (declare (ignore lisp-type c-type to-foreign from-foreign direct))
  nil)

;;; *primitive-type-map* - Variable holding primitive type mappings
;;; Maps Lisp primitive types to their C equivalents
;;; Used by shared-call for efficient type conversion

;;; Control variables
(defvar *use-libffi-calls* nil
  "When true, use libffi for function calls instead of direct alien calls")

(defvar *track-call-performance* nil
  "When true, track performance statistics for FFI calls")

;;; Core FFI Functions

(defun call-with-signature (function-address return-type arg-types args)
  "Call function using signature-specific optimized paths"
  (cond
    ;; No arguments - simple function calls
    ((null arg-types)
     (cond
       ((eq return-type :int)
        (eval `(sb-alien:alien-funcall 
                (sb-alien:sap-alien 
                 (sb-sys:int-sap ,function-address)
                 (sb-alien:function sb-alien:int)))))
       ((eq return-type :void)
        (eval `(sb-alien:alien-funcall 
                (sb-alien:sap-alien 
                 (sb-sys:int-sap ,function-address)
                 (sb-alien:function sb-alien:void)))))
       ((eq return-type :pointer)
        (eval `(sb-alien:alien-funcall 
                (sb-alien:sap-alien 
                 (sb-sys:int-sap ,function-address)
                 (sb-alien:function sb-alien:system-area-pointer)))))
       (t (error "Return type ~A not yet implemented for zero-argument functions" return-type))))
    
    ;; String functions - strlen style: unsigned-long fn(string)
    ((and (eq return-type :unsigned-long) (equal arg-types '(:string)))
     (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                   args arg-types)))
       (eval `(sb-alien:alien-funcall 
               (sb-alien:sap-alien 
                (sb-sys:int-sap ,function-address)
                (sb-alien:function sb-alien:unsigned-long sb-alien:c-string))
               ,@converted-args))))
    
    ;; Memory allocation - malloc style: pointer fn(unsigned-long)
    ((and (eq return-type :pointer) (equal arg-types '(:unsigned-long)))
     (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                   args arg-types)))
       (eval `(sb-alien:alien-funcall 
               (sb-alien:sap-alien 
                (sb-sys:int-sap ,function-address)
                (sb-alien:function sb-alien:system-area-pointer sb-alien:unsigned-long))
               ,@converted-args))))
    
    ;; Simple system calls - getpid style: int fn()
    ((and (eq return-type :int) (null arg-types))
     (eval `(sb-alien:alien-funcall 
             (sb-alien:sap-alien 
              (sb-sys:int-sap ,function-address)
              (sb-alien:function sb-alien:int)))))
    
    ;; Pointer-returning functions with no args - like sqlite3_libversion: pointer fn()
    ((and (eq return-type :pointer) (null arg-types))
     (eval `(sb-alien:alien-funcall 
             (sb-alien:sap-alien 
              (sb-sys:int-sap ,function-address)
              (sb-alien:function sb-alien:system-area-pointer)))))

    (t
     (error "Function signature ~A ~A not yet implemented in call-with-signature" 
            return-type arg-types))))

;; shared-call-unified is defined in libffi-calls.lisp

;; Removed duplicate libffi-call definition to avoid conflicts

;; shared-call is defined above - use shared-call-unified directly

;; Missing stub functions
(defvar *libffi-library* nil
  "Handle to the libffi extension library")

(defun resolve-function-address (function-designator)
  "Resolve function address from designator"
  (etypecase function-designator
    (symbol (lib:lib-function 
             (lib:lib-open "libc") 
             (string function-designator)))
    (list (destructuring-bind (fn-name lib-name) function-designator
            (lib:lib-function 
             (lib:lib-open lib-name) 
             (string fn-name))))))

(defun ffi-call (function-address return-type arg-types &rest args)
  "Direct FFI call with function address"
  (call-with-signature function-address return-type arg-types args))

(defun ffi-call-auto (function-designator &rest args)
  "Auto-discovering FFI call"
  ;; Auto-discovery not implemented
  (error "ffi-call-auto not yet implemented for ~A" function-designator))

(defvar *primitive-type-map*)

;;;; New Trampoline-based Fast FFI

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
       (trampoline:register-signature ',lisp-name ,return-type ',(or arg-types '()))
       ;; Define the function
       (defun ,lisp-name ,arg-names
         ,@(when documentation (list documentation))
         (shared-call-fast (list ,c-name ,library) ,return-type ',(or arg-types '()) ,@arg-names)))))

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

;; Re-export array handling macros
(defmacro with-output-array ((var count type) &body body)
  "Create an array for output parameters"
  `(marshalling:with-output-array (,var ,count ,type) ,@body))

;; Re-export struct functions
(defun define-c-struct (name fields)
  "Define a C struct"
  (struct:define-c-struct name fields))

(defmacro with-c-struct ((var type) &body body)
  "Allocate a C struct"
  `(struct:with-c-struct (,var ,type) ,@body))

(defun struct-ref (struct field)
  "Get struct field value"
  (struct:struct-ref struct field))

(defun (setf struct-ref) (value struct field)
  "Set struct field value"
  (setf (struct:struct-ref struct field) value))

;;;; Configuration management

(defvar *replace-shared-call* nil
  "Whether to replace the original shared-call with unified version")

(defun enable-libffi-by-default ()
  "Configure system to use libffi by default"
  (setf *use-libffi-calls* t)
  (setf *libffi-function-whitelist* nil)  ; Allow all functions
  (setf *libffi-function-blacklist* '())  ; Block none
  (format t "libffi enabled by default for all functions~%"))

(defun enable-conservative-libffi ()
  "Configure system to use libffi only for tested functions"
  (setf *use-libffi-calls* t)
  (setf *libffi-function-whitelist* 
        '("strlen" "malloc" "free" "getpid" "close" "read" "write"))
  (setf *libffi-function-blacklist* '())
  (format t "libffi enabled for conservative function set~%"))

(defun disable-libffi ()
  "Disable libffi and use SBCL fallback"
  (setf *use-libffi-calls* nil)
  (format t "libffi disabled, using SBCL fallback~%"))

;;;; Development helpers

(defmacro with-libffi-testing (&body body)
  "Execute body with optimal settings for libffi testing"
  `(let ((*use-libffi-calls* t)
         (*track-call-performance* t)
         (*libffi-function-whitelist* nil)
         (*libffi-function-blacklist* '()))
     (format t "libffi testing mode enabled~%")
     ,@body))

;;;; System status and diagnostics

(defun ffi-system-status ()
  "Display  FFI system status"
  (format t "~%Epsilon FFI System Status~%")
  (format t "=========================~%~%")
  
  ;; Core system
  (format t "Core System:~%")
  (format t "  SBCL version: ~A~%" (lisp-implementation-version))
  (format t "  Platform: ~A ~A~%" (machine-type) (machine-version))
  
  ;; libffi status
  (format t "~%libffi Integration:~%")
  (format t "  libffi available: ~A~%" (and (boundp '*libffi-library*) *libffi-library* t))
  (format t "  Function calls: ~A~%" (if (fboundp 'libffi-available-for-calls-p)
                                         (libffi-available-for-calls-p)
                                         "Unknown"))
  (format t "  Callbacks: ~A~%" (if (fboundp 'libffi-callback-count)
                                    (> (libffi-callback-count) -1)
                                    "Unknown"))
  
  ;; Signature system
  (format t "~%Signature System:~%")
  (if (find-package :epsilon.clang.signatures)
      (let ((db-symbol (find-symbol "*SIGNATURE-DATABASE*" :epsilon.clang.signatures)))
        (if db-symbol
            (format t "  Clang integration: Available~%  Cached signatures: ~D~%" 
                    (hash-table-count (symbol-value db-symbol)))
            (format t "  Clang integration: Package found but database unavailable~%")))
      (format t "  Clang integration: Not available~%"))
  
  ;; Performance tracking
  (format t "~%Performance:~%")
  (format t "  Call tracking: ~A~%" *track-call-performance*)
  (format t "  Tracked functions: ~D~%" (hash-table-count *call-statistics*))
  
  ;; Configuration
  (format t "~%Configuration:~%")
  (format t "  Use libffi: ~A~%" *use-libffi-calls*)
  (format t "  Whitelist: ~A~%" (if *libffi-function-whitelist* 
                                  (length *libffi-function-whitelist*)
                                  "All functions"))
  (format t "  Blacklist: ~D functions~%" (length *libffi-function-blacklist*))
  
  (format t "~%"))

;;;; Initialize public API

(defun initialize-public-api ()
  "Initialize the public API system"
  (unless (boundp '*use-libffi-calls*)
    (setf *use-libffi-calls* t)))

;; Initialize when loaded
(eval-when (:load-toplevel :execute)
  (initialize-public-api))
