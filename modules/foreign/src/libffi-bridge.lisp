;;;; libffi-bridge.lisp - SBCL bridge to libffi C extension
;;;;
;;;; This module provides the Lisp-side interface to the libffi C extension,
;;;; enabling real callback creation to overcome SBCL's alien-lambda limitations.

(in-package #:epsilon.foreign)

;;; Type mapping constants (must match C extension)
(defconstant +epsilon-type-void+    0)
(defconstant +epsilon-type-int+     1)
(defconstant +epsilon-type-long+    2)
(defconstant +epsilon-type-float+   3)
(defconstant +epsilon-type-double+  4)
(defconstant +epsilon-type-pointer+ 5)
(defconstant +epsilon-type-string+  6)
(defconstant +epsilon-type-bool+    7)

;;; Global state
(defvar *libffi-library* nil
  "Handle to the loaded libffi extension library")

(defvar *libffi-available-p* nil
  "Whether libffi integration is available")

(defvar *libffi-callbacks* (make-hash-table)
  "Registry mapping callback IDs to Lisp callback information")

(defvar *libffi-mutex* (sb-thread:make-mutex :name "libffi-callbacks")
  "Mutex for thread-safe callback operations")

;;; Library loading
(defun load-libffi-extension ()
  "Load the libffi C extension library"
  (when *libffi-library*
    (return-from load-libffi-extension *libffi-library*))
  
  (let* ((current-path (or *load-pathname* 
                          *compile-file-pathname* 
                          (error "Cannot determine module directory")))
         (module-root (make-pathname 
                      :directory (butlast (butlast (pathname-directory current-path)))))
         (lib-path (merge-pathnames 
                   #P"c/libepsilon-libffi.so"
                   module-root)))
    (handler-case
        (progn
          (setf *libffi-library* (sb-alien:load-shared-object lib-path))
          (setf *libffi-available-p* t)
          *libffi-library*)
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

;;; Type conversion utilities
(defun lisp-type-to-epsilon-type (lisp-type)
  "Convert Lisp type symbols to epsilon type constants"
  (case lisp-type
    ((:void void) +epsilon-type-void+)
    ((:int int integer) +epsilon-type-int+)
    ((:long long) +epsilon-type-long+)
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
    (setf (gethash id *libffi-callbacks*)
          (make-callback-info :id id
                              :function function
                              :return-type return-type
                              :arg-types arg-types
                              :pointer pointer))))

(defun unregister-libffi-callback (id)
  "Remove a callback from the Lisp-side registry"
  (sb-thread:with-mutex (*libffi-mutex*)
    (remhash id *libffi-callbacks*)))

(defun get-libffi-callback (id)
  "Retrieve callback info by ID"
  (sb-thread:with-mutex (*libffi-mutex*)
    (gethash id *libffi-callbacks*)))

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
      (clrhash *libffi-callbacks*))))

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

;;; Initialize on load
(eval-when (:load-toplevel :execute)
  (load-libffi-extension))