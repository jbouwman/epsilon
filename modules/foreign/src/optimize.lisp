;;;; optimize.lisp - Compiler macro optimizations for FFI calls
;;;;
;;;; This module is loaded AFTER epsilon.foreign is fully defined,
;;;; allowing it to define compiler macros without circular dependencies.

(defpackage epsilon.foreign.optimize
  (:use cl)
  (:local-nicknames
   (ffi epsilon.foreign)
   (trampoline epsilon.foreign.trampoline))
  (:export
   #:enable-ffi-optimizations
   #:disable-ffi-optimizations
   #:*optimization-enabled*
   ;; Optimized functions
   #:%strlen-optimized
   #:%memcpy-optimized
   #:%malloc-optimized
   #:%free-optimized))

(in-package epsilon.foreign.optimize)

(defvar *optimization-enabled* t
  "When true, compiler macros are active")

;;; Helper functions

(defun constantp-all (forms)
  "Check if all forms are compile-time constants"
  (every #'constantp forms))

(defun can-inline-p (return-type arg-types)
  "Check if this signature can be inlined"
  (and *optimization-enabled*
       ;; Only inline simple types
       (member return-type '(:void :int :unsigned-int :long :unsigned-long
                            :pointer :string :char :unsigned-char
                            :short :unsigned-short :float :double))
       ;; Limit to reasonable number of args
       (<= (length arg-types) 6)
       ;; All arg types must be simple
       (every (lambda (type)
                (member type '(:int :unsigned-int :long :unsigned-long
                              :pointer :string :char :unsigned-char
                              :short :unsigned-short :float :double)))
              arg-types)))

(defun lisp-type-to-alien-type (type)
  "Convert our type keyword to sb-alien type"
  (case type
    (:void 'sb-alien:void)
    (:char 'sb-alien:char)
    (:unsigned-char 'sb-alien:unsigned-char)
    (:short 'sb-alien:short)
    (:unsigned-short 'sb-alien:unsigned-short)
    (:int 'sb-alien:int)
    (:unsigned-int 'sb-alien:unsigned-int)
    (:long 'sb-alien:long)
    (:unsigned-long 'sb-alien:unsigned-long)
    (:float 'sb-alien:float)
    (:double 'sb-alien:double)
    (:pointer 'sb-alien:system-area-pointer)
    (:string 'sb-alien:c-string)
    (t (error "Unknown type for alien: ~A" type))))

;;; Compiler macro for shared-call optimization

(define-compiler-macro ffi:shared-call (&whole form function-designator return-type arg-types &rest args)
  "Optimize shared-call at compile time when possible"
  (cond
    ;; Case 1: Everything is constant - generate direct alien-funcall
    ((and (constantp function-designator)
          (constantp return-type) 
          (constantp arg-types)
          (listp (eval function-designator))
          (stringp (first (eval function-designator)))
          (can-inline-p (eval return-type) (eval arg-types)))
     (let* ((fn-name (first (eval function-designator)))
            (lib-name (second (eval function-designator)))
            (ret-type (eval return-type))
            (arg-type-list (eval arg-types))
            (alien-ret-type (lisp-type-to-alien-type ret-type))
            (alien-arg-types (mapcar #'lisp-type-to-alien-type arg-type-list)))
       ;; For libc functions, generate direct alien-funcall
       (if (or (null lib-name) (equal lib-name "libc"))
           `(sb-alien:alien-funcall
             (sb-alien:extern-alien ,fn-name
                                   (sb-alien:function ,alien-ret-type ,@alien-arg-types))
             ,@args)
           ;; For other libraries, still use shared-call (library loading needed)
           form)))
    
    ;; Case 2: Function name and types constant, but args not - still optimize structure
    ((and (constantp function-designator)
          (constantp return-type)
          (constantp arg-types)
          (listp (eval function-designator))
          (stringp (first (eval function-designator))))
     (let* ((fn-name (first (eval function-designator)))
            (lib-name (second (eval function-designator)))
            (ret-type (eval return-type))
            (arg-type-list (eval arg-types)))
       ;; Pre-compute trampoline at compile time if possible
       (if (and (or (null lib-name) (equal lib-name "libc"))
                (can-inline-p ret-type arg-type-list))
           (let ((alien-ret-type (lisp-type-to-alien-type ret-type))
                 (alien-arg-types (mapcar #'lisp-type-to-alien-type arg-type-list)))
             `(sb-alien:alien-funcall
               (sb-alien:extern-alien ,fn-name
                                     (sb-alien:function ,alien-ret-type ,@alien-arg-types))
               ,@args))
           form)))
    
    ;; Default: use original form
    (t form)))

;;; Compiler macro for defshared functions

(define-compiler-macro ffi:shared-call (&whole form function-designator return-type arg-types &rest args)
  "Optimize shared-call at compile time"
  ;; Apply same optimizations as shared-call
  (cond
    ((and (constantp function-designator)
          (constantp return-type)
          (constantp arg-types)
          (listp (eval function-designator))
          (stringp (first (eval function-designator)))
          (equal (second (eval function-designator)) "libc")
          (can-inline-p (eval return-type) (eval arg-types)))
     (let* ((fn-name (first (eval function-designator)))
            (ret-type (eval return-type))
            (arg-type-list (eval arg-types))
            (alien-ret-type (lisp-type-to-alien-type ret-type))
            (alien-arg-types (mapcar #'lisp-type-to-alien-type arg-type-list)))
       `(sb-alien:alien-funcall
         (sb-alien:extern-alien ,fn-name
                               (sb-alien:function ,alien-ret-type ,@alien-arg-types))
         ,@args)))
    (t form)))

;;; Control functions

(defun enable-ffi-optimizations ()
  "Enable FFI compiler optimizations"
  (setf *optimization-enabled* t))

(defun disable-ffi-optimizations ()
  "Disable FFI compiler optimizations"
  (setf *optimization-enabled* nil))

;;; Optimization hints

(defmacro with-ffi-optimization ((&key (level 3)) &body body)
  "Execute body with specified optimization level"
  `(locally (declare (optimize (speed ,level) (safety 0) (debug 0)))
     ,@body))

;;; Precompiled common functions
;;; These bypass the FFI entirely for maximum speed

(declaim (inline %strlen-optimized))
(defun %strlen-optimized (string)
  "Hyper-optimized strlen"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "strlen"
                         (sb-alien:function sb-alien:unsigned-long
                                           sb-alien:c-string))
   string))

(declaim (inline %memcpy-optimized))
(defun %memcpy-optimized (dest src size)
  "Hyper-optimized memcpy"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "memcpy"
                         (sb-alien:function sb-alien:system-area-pointer
                                           sb-alien:system-area-pointer
                                           sb-alien:system-area-pointer
                                           sb-alien:unsigned-long))
   dest src size))

(declaim (inline %malloc-optimized))
(defun %malloc-optimized (size)
  "Hyper-optimized malloc"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 64) size))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "malloc"
                         (sb-alien:function sb-alien:system-area-pointer
                                           sb-alien:unsigned-long))
   size))

(declaim (inline %free-optimized))
(defun %free-optimized (ptr)
  "Hyper-optimized free"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
                         (sb-alien:function sb-alien:void
                                           sb-alien:system-area-pointer))
   ptr))

;;; Initialize optimizations
(enable-ffi-optimizations)