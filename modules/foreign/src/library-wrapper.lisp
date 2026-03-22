;;;; library-wrapper.lisp - Automatic C library wrapping
;;;;
;;;; Provides macros and utilities for wrapping entire C libraries
;;;; with JIT-compiled FFI bindings.

(defpackage epsilon.foreign.jit.library
  (:use cl)
  (:local-nicknames
   (jit epsilon.foreign.jit)
   (sigs epsilon.foreign.signatures)
   (lc epsilon.foreign.libclang))
  (:export
   ;; Library definition
   #:deflibrary
   #:define-library

   ;; Library management
   #:get-library
   #:library-functions
   #:library-function-count
   #:unload-library

   ;; Library info
   #:library-wrapper
   #:library-wrapper-p
   #:library-wrapper-name
   #:library-wrapper-package
   #:library-wrapper-headers
   #:library-wrapper-functions

   ;; Utilities
   #:list-library-functions
   #:describe-library)
  (:enter t))

;;; ============================================================================
;;; Library Registry
;;; ============================================================================

(defvar *libraries* (make-hash-table :test 'eq)
  "Registry of defined library wrappers")

(defstruct library-wrapper
  "Definition of a wrapped C library"
  (name nil :type symbol)
  (package nil :type (or null package))
  (headers nil :type list)
  (library nil :type (or null string symbol))
  (prefix "" :type string)
  (include-functions nil :type list)
  (exclude-functions nil :type list)
  (include-paths nil :type list)
  (lazy nil :type boolean)
  (functions (make-hash-table :test 'equal) :type hash-table)
  (callers (make-hash-table :test 'equal) :type hash-table))

(defun get-library (name)
  "Get a library wrapper by name"
  (gethash name *libraries*))

(defun register-library (wrapper)
  "Register a library wrapper"
  (setf (gethash (library-wrapper-name wrapper) *libraries*) wrapper))

(defun unload-library (name)
  "Unload a library wrapper and remove its package"
  (let ((wrapper (get-library name)))
    (when wrapper
      (let ((pkg (library-wrapper-package wrapper)))
        (when pkg
          (delete-package pkg)))
      (remhash name *libraries*)
      t)))

;;; ============================================================================
;;; Function Discovery
;;; ============================================================================

(defun should-include-function-p (name include-list exclude-list)
  "Check if a function should be included based on include/exclude lists"
  (and (or (null include-list)
           (member name include-list :test #'string=))
       (not (member name exclude-list :test #'string=))))

(defun discover-library-functions (wrapper)
  "Discover all functions from library headers.
   Returns a list of function info plists."
  (let ((functions '())
        (seen (make-hash-table :test 'equal)))
    (dolist (header (library-wrapper-headers wrapper))
      (let ((full-path (sigs:find-header header (library-wrapper-include-paths wrapper))))
        (when full-path
          (lc:with-parsed-header (tu full-path
                                     :include-paths (library-wrapper-include-paths wrapper))
            (dolist (decl (lc:extract-all-declarations tu))
              (when (eq (getf decl :kind) :function)
                (let ((name (getf decl :name)))
                  (when (and (not (gethash name seen))
                             (should-include-function-p
                              name
                              (library-wrapper-include-functions wrapper)
                              (library-wrapper-exclude-functions wrapper)))
                    (setf (gethash name seen) t)
                    ;; Get full signature
                    (let ((sig (lc:extract-function-info tu name)))
                      (when sig
                        (push sig functions)))))))))))
    (nreverse functions)))

;;; ============================================================================
;;; Name Generation
;;; ============================================================================

(defun c-name-to-lisp-name (c-name &optional prefix)
  "Convert C function name to Lisp symbol name.
   Example: 'my_function' -> 'MY-FUNCTION'"
  (let* ((lisp-name (substitute #\- #\_ (string-upcase c-name)))
         (full-name (if (and prefix (> (length prefix) 0))
                        (concatenate 'string (string-upcase prefix) lisp-name)
                        lisp-name)))
    (intern full-name)))

(defun library-package-name (library-name)
  "Generate package name for a library"
  (intern (string-upcase (symbol-name library-name)) :keyword))

;;; ============================================================================
;;; Caller Generation
;;; ============================================================================

(defun make-library-caller (wrapper c-name signature)
  "Create a JIT caller for a library function"
  (declare (ignore wrapper))
  (multiple-value-bind (return-type arg-types)
      (sigs:signature-to-jit-types signature)
    (let ((addr (sb-sys:find-foreign-symbol-address c-name)))
      (when addr
        (let ((addr-int (etypecase addr
                          (integer addr)
                          (sb-sys:system-area-pointer (sb-sys:sap-int addr)))))
          (jit:make-jit-caller addr-int return-type arg-types))))))

(defun get-or-create-caller (wrapper c-name)
  "Get cached caller or create new one for a function"
  (let ((callers (library-wrapper-callers wrapper)))
    (or (gethash c-name callers)
        (let ((sig (gethash c-name (library-wrapper-functions wrapper))))
          (when sig
            (let ((caller (make-library-caller wrapper c-name sig)))
              (when caller
                (setf (gethash c-name callers) caller))
              caller))))))

;;; ============================================================================
;;; Function Wrapper Generation
;;; ============================================================================

(defun generate-function-wrapper (wrapper c-name lisp-name signature lazy)
  "Generate a wrapper function form"
  (let ((params (loop for i from 0 below (length (getf signature :params))
                      collect (intern (format nil "ARG~D" i)))))
    (if lazy
        ;; Lazy: create caller on first call
        `(let ((caller nil))
           (defun ,lisp-name ,params
             (unless caller
               (setf caller (get-or-create-caller
                             (get-library ',(library-wrapper-name wrapper))
                             ,c-name)))
             (if caller
                 (funcall caller ,@params)
                 (error "Could not create caller for ~A" ,c-name))))
        ;; Eager: create caller immediately
        `(let ((caller (make-library-caller
                        (get-library ',(library-wrapper-name wrapper))
                        ,c-name
                        ',signature)))
           (defun ,lisp-name ,params
             (if caller
                 (funcall caller ,@params)
                 (error "Could not create caller for ~A" ,c-name)))))))

;;; ============================================================================
;;; Library Definition Macro
;;; ============================================================================

(defun %define-library (name headers library prefix include-functions
                        exclude-functions include-paths lazy)
  "Internal function to define a library wrapper"
  (let* ((pkg-name (library-package-name name))
         (wrapper (make-library-wrapper
                   :name name
                   :headers (if (listp headers) headers (list headers))
                   :library library
                   :prefix (or prefix "")
                   :include-functions include-functions
                   :exclude-functions exclude-functions
                   :include-paths include-paths
                   :lazy lazy)))

    ;; Discover functions
    (let ((functions (discover-library-functions wrapper)))
      (dolist (sig functions)
        (setf (gethash (getf sig :name) (library-wrapper-functions wrapper)) sig))

      ;; Create package first, then create exports in that package
      (let ((pkg (or (find-package pkg-name)
                     (make-package pkg-name :use nil))))
        (setf (library-wrapper-package wrapper) pkg)
        ;; Create export symbols in the target package
        (let ((exports (mapcar (lambda (sig)
                                 (let ((name (symbol-name
                                               (c-name-to-lisp-name (getf sig :name)
                                                                    (library-wrapper-prefix wrapper)))))
                                   (intern name pkg)))
                               functions)))
          (export exports pkg)

          ;; Register library
          (register-library wrapper)

          ;; Generate wrapper functions
          (dolist (sig functions)
            (let* ((c-name (getf sig :name))
                   (lisp-name (intern (symbol-name
                                       (c-name-to-lisp-name c-name
                                                           (library-wrapper-prefix wrapper)))
                                      pkg)))
              (eval (generate-function-wrapper wrapper c-name lisp-name sig lazy))))

          (values name (length functions)))))))

(defmacro deflibrary (name &key headers library prefix
                               include-functions exclude-functions
                               include-paths lazy)
  "Define a JIT-wrapped C library.

   NAME - Symbol naming the library (becomes package name)
   HEADERS - Header file(s) to parse for function signatures
   LIBRARY - Library name or path (for documentation, not used for linking)
   PREFIX - Optional prefix for generated Lisp function names
   INCLUDE-FUNCTIONS - List of function names to include (nil = all)
   EXCLUDE-FUNCTIONS - List of function names to exclude
   INCLUDE-PATHS - Additional include paths for header parsing
   LAZY - If true, create callers on first use (faster load time)

   Example:
   (deflibrary math-lib
     :headers (\"math.h\")
     :library :libm
     :prefix \"m-\"
     :include-functions (\"sin\" \"cos\" \"tan\" \"sqrt\" \"pow\"))

   After definition:
   (math-lib:m-sin 1.0d0)  ; => 0.8414...
   (math-lib:m-sqrt 2.0d0) ; => 1.4142..."
  `(progn
     (unless (lc:libclang-available-p)
       (error "libclang not available - cannot parse headers"))
     (%define-library ',name
                      ',headers
                      ',library
                      ,prefix
                      ',include-functions
                      ',exclude-functions
                      ',include-paths
                      ,lazy)))

(defmacro define-library (name &rest args)
  "Alias for deflibrary"
  `(deflibrary ,name ,@args))

;;; ============================================================================
;;; Library Introspection
;;; ============================================================================

(defun library-functions (name)
  "Get list of function names in a library"
  (let ((wrapper (get-library name)))
    (when wrapper
      (let ((functions '()))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (push k functions))
                 (library-wrapper-functions wrapper))
        (sort functions #'string<)))))

(defun library-function-count (name)
  "Get number of functions in a library"
  (let ((wrapper (get-library name)))
    (when wrapper
      (hash-table-count (library-wrapper-functions wrapper)))))

(defun list-library-functions (name &key (limit 20))
  "Print functions in a library"
  (let ((wrapper (get-library name)))
    (if wrapper
        (let ((count 0)
              (total (library-function-count name)))
          (format t "~%Library: ~A (~D functions)~%" name total)
          (format t "Package: ~A~%" (package-name (library-wrapper-package wrapper)))
          (format t "Headers: ~{~A~^, ~}~%" (library-wrapper-headers wrapper))
          (format t "~%Functions:~%")
          (dolist (fn (library-functions name))
            (when (and limit (>= count limit))
              (format t "  ... (~D more)~%" (- total limit))
              (return))
            (let ((lisp-name (c-name-to-lisp-name fn (library-wrapper-prefix wrapper))))
              (format t "  ~A -> ~A~%" fn lisp-name))
            (incf count))
          total)
        (format t "Library ~A not found~%" name))))

(defun describe-library (name)
  "Describe a library wrapper in detail"
  (let ((wrapper (get-library name)))
    (if wrapper
        (progn
          (format t "~%Library: ~A~%" (library-wrapper-name wrapper))
          (format t "  Package: ~A~%" (when (library-wrapper-package wrapper)
                                         (package-name (library-wrapper-package wrapper))))
          (format t "  Headers: ~{~A~^, ~}~%" (library-wrapper-headers wrapper))
          (format t "  Library: ~A~%" (library-wrapper-library wrapper))
          (format t "  Prefix: ~S~%" (library-wrapper-prefix wrapper))
          (format t "  Lazy: ~A~%" (library-wrapper-lazy wrapper))
          (format t "  Functions: ~D~%" (library-function-count name))
          (format t "  Cached callers: ~D~%"
                  (hash-table-count (library-wrapper-callers wrapper)))
          wrapper)
        (format t "Library ~A not found~%" name))))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun wrap-function (library-name c-name)
  "Manually wrap a single function from a library.
   Returns the wrapper function."
  (let ((wrapper (get-library library-name)))
    (unless wrapper
      (error "Library ~A not defined" library-name))
    (let ((sig (gethash c-name (library-wrapper-functions wrapper))))
      (unless sig
        (error "Function ~A not found in library ~A" c-name library-name))
      (get-or-create-caller wrapper c-name))))
