;;;; libffi-bridge.lisp - SBCL bridge to libffi C extension
;;;;
;;;; This module provides the Lisp-side interface to the libffi C extension,
;;;; enabling real callback creation to overcome SBCL's alien-lambda limitations.

(in-package #:epsilon.foreign)

;;; Local nickname for epsilon.map
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "EPSILON.MAP")
    (error "epsilon.map package not found")))

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

(defvar *libffi-available-p* nil
  "Whether libffi integration is available")

(defvar *libffi-callbacks* (epsilon.map:make-map)
  "Registry mapping callback IDs to Lisp callback information")

(defvar *libffi-mutex* (sb-thread:make-mutex :name "libffi-callbacks")
  "Mutex for thread-safe callback operations")

;;; Library loading
(defun load-libffi-extension ()
  "Load the libffi C extension library with automatic compilation if needed"
  (when *libffi-library*
    (return-from load-libffi-extension *libffi-library*))
  
  ;; Try to build/ensure the C extension using the builder
  (let ((extension-path nil))
    (handler-case
        (progn
          ;; Check if builder package is available
          (when (find-package :epsilon.foreign.c-extension-builder)
            (let ((ensure-fn (find-symbol "ENSURE-C-EXTENSION" :epsilon.foreign.c-extension-builder)))
              (when ensure-fn
                (setf extension-path (funcall ensure-fn)))))
          
          ;; If builder not available or didn't produce a path, try traditional approach
          (unless extension-path
            (let* ((module-root 
                    ;; Try multiple ways to find the module root
                    (or 
                     ;; Method 1: Use load/compile pathname
                     (when (or *load-pathname* *compile-file-pathname*)
                       (let ((path (or *load-pathname* *compile-file-pathname*)))
                         (make-pathname 
                          :directory (butlast (butlast (pathname-directory path))))))
                     ;; Method 2: Use known absolute path
                     #P"/home/jbouwman/git/epsilon-8/modules/foreign/"
                     ;; Method 3: Error if we can't find it
                     (error "Cannot determine module directory")))
                   ;; Try platform-specific name in lib directory
                   ;; Convert SBCL's "X86-64" to standard "x86_64"
                   (arch (string-downcase (substitute #\_ #\- (machine-type))))
                   (platform-lib (format nil "lib/libepsilon-libffi-~A-~A.~A"
                                        #+linux "linux" #+darwin "darwin" #-(or linux darwin) "unknown"
                                        arch
                                        #+linux "so" #+darwin "dylib" #-(or linux darwin) "so"))
                   (platform-path (merge-pathnames platform-lib module-root))
                   ;; Also try target directory as fallback
                   (target-lib (format nil "target/package/fasl/c/libepsilon-libffi-~A-~A.~A"
                                      #+linux "linux" #+darwin "darwin" #-(or linux darwin) "unknown"
                                      arch
                                      #+linux "so" #+darwin "dylib" #-(or linux darwin) "so"))
                   (target-path (merge-pathnames target-lib module-root)))
              
              ;; Try lib directory first, then target directory
              (cond
                ((probe-file platform-path)
                 (setf extension-path platform-path))
                ((probe-file target-path)
                 (setf extension-path target-path))
                (t
                 (setf extension-path (merge-pathnames 
                                      #P"lib/libepsilon-libffi.so"
                                      module-root))))))
          
          ;; Load the extension
          (when (and extension-path (probe-file extension-path))
            (setf *libffi-library* (sb-alien:load-shared-object extension-path))
            (setf *libffi-available-p* t)
            (format t "Loaded libffi extension from ~A~%" extension-path)
            *libffi-library*))
      (error (e)
        (warn "Failed to load libffi extension: ~A" e)
        (setf *libffi-available-p* nil)
        nil))))

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
    ((:string string) +epsilon-type-string+)
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
    (setf *libffi-callbacks* (epsilon.map:assoc *libffi-callbacks* id
                                               (make-callback-info :id id
                                                                   :function function
                                                                   :return-type return-type
                                                                   :arg-types arg-types
                                                                   :pointer pointer)))))

(defun unregister-libffi-callback (id)
  "Remove a callback from the Lisp-side registry"
  (sb-thread:with-mutex (*libffi-mutex*)
    (setf *libffi-callbacks* (epsilon.map:dissoc *libffi-callbacks* id))))

(defun get-libffi-callback (id)
  "Retrieve callback info by ID"
  (sb-thread:with-mutex (*libffi-mutex*)
    (epsilon.map:get *libffi-callbacks* id)))

;;; High-level callback creation interface
(defun make-libffi-callback (function return-type arg-types)
  "Create a real callback using libffi
   Returns the C function pointer or signals an error"
  (unless *libffi-available-p*
    (load-libffi-extension))
  
  (unless *libffi-available-p*
    (error "libffi extension not available"))
  
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
                 (if (sb-alien:null-alien pointer)
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

;;; Enhanced callback creation with fallback
(defun make-callback-with-libffi (function return-type arg-types)
  "Create callback with libffi, fall back to existing implementation"
  (handler-case
      (make-libffi-callback function return-type arg-types)
    (error (e)
      (warn "libffi callback creation failed: ~A~%Falling back to SBCL implementation" e)
      ;; Fall back to existing implementation
      (funcall (find-symbol "MAKE-SBCL-CALLBACK" '#:epsilon.foreign.callback)
               function return-type arg-types))))

;;; Cleanup utilities
(defun cleanup-libffi-callbacks ()
  "Clean up all libffi callbacks"
  (when *libffi-available-p*
    (epsilon-cleanup-all-callbacks)
    (sb-thread:with-mutex (*libffi-mutex*)
      (setf *libffi-callbacks* (epsilon.map:make-map)))))

;;; Integration with existing callback infrastructure
(defun enhanced-make-callback (function return-type arg-types)
  "Enhanced callback creation that tries libffi first, falls back to existing"
  (if *libffi-available-p*
      (make-callback-with-libffi function return-type arg-types)
      (funcall (find-symbol "MAKE-SBCL-CALLBACK" '#:epsilon.foreign.callback)
               function return-type arg-types)))

;;; Test and diagnostic functions
(defun test-libffi-extension ()
  "Test if libffi extension is working"
  (unless *libffi-available-p*
    (load-libffi-extension))
  
  (if *libffi-available-p*
      (let ((result (epsilon-libffi-test)))
        (if (= result 42)
            (format t "libffi extension test passed (returned ~A)~%" result)
            (format t "libffi extension test failed (returned ~A, expected 42)~%" result))
        (= result 42))
      (progn
        (format t "libffi extension not available~%")
        nil)))

(defun libffi-callback-count ()
  "Get count of active libffi callbacks"
  (if *libffi-available-p*
      (epsilon-get-callback-count)
      0))

;;; Function call support (not just callbacks)
;;; Already defined above after epsilon-get-last-error

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
  (unless *libffi-available-p*
    (load-libffi-extension))
  
  ;; Re-check after potential reload
  (unless *libffi-available-p*
    (error "libffi extension not available for function calls"))
  
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
           (let* ((result-holder (allocate-result-holder return-type))
                  (call-result (epsilon-call-function (cif-info-pointer cif-info)
                                                      (sb-sys:int-sap function-address)
                                                      converted-args
                                                      result-holder)))
             ;; Keep holders alive during the call
             (declare (ignore holders))
             (if (< call-result 0)
                 (error "FFI call failed: ~A" (epsilon-get-last-error))
                 (extract-result result-holder return-type))))
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
    (:pointer (let* ((ptr (if (sb-sys:system-area-pointer-p arg)
                              arg
                              (sb-sys:int-sap (if (integerp arg) arg 0))))
                    (holder (sb-alien:make-alien sb-alien:system-area-pointer)))
                (setf (sb-alien:deref holder) ptr)
                (values (sb-alien:alien-sap holder) holder)))
    (:string (if (stringp arg)
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
    (:pointer (sb-alien:deref (sb-alien:sap-alien result-holder (* sb-alien:system-area-pointer))))
    (otherwise (error "Unsupported return type: ~A" return-type))))

;;; Integration with existing FFI infrastructure

(defun libffi-available-for-calls-p ()
  "Check if libffi is available for function calls (not just callbacks)"
  (and *libffi-available-p*
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