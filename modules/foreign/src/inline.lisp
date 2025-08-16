;;;; inline.lisp - Inline expansion for high-performance FFI calls
;;;;
;;;; This module provides macros and compiler optimizations to inline
;;;; simple FFI calls, eliminating function call overhead for hot paths.

(defpackage epsilon.foreign.inline
  (:use cl)
  (:local-nicknames
   (:map :epsilon.map))
  (:export
   #:definline-foreign
   #:inline-foreign-call
   #:*inline-threshold*
   #:*inline-aggressive*
   #:with-inline-ffi
   #:optimize-ffi-calls
   ;; Export example inlined functions
   #:%strlen
   #:%memcpy
   #:%getpid))

(in-package epsilon.foreign.inline)

;;; Inline optimization for frequently called FFI functions

(defun lisp-type-to-alien-type (type)
  "Convert Lisp type keyword to SB-ALIEN type"
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
    (t 'sb-alien:system-area-pointer)))

(defvar *inline-threshold* 3
  "Maximum number of arguments for automatic inlining")

(defvar *inline-aggressive* nil
  "When true, inline even complex calls")

(defvar *inlined-functions* map:+empty+
  "Cache of inlined function definitions")

(defmacro definline-foreign (name c-name library return-type &rest args)
  "Define an inlined foreign function call"  
  (let ((arg-names (mapcar #'first args))
        (arg-types (mapcar #'second args)))
    `(progn
       ;; Create inline version
       (defmacro ,name ,arg-names
         `(sb-alien:alien-funcall
           (sb-alien:extern-alien ,,c-name
                                 (sb-alien:function 
                                  ,(lisp-type-to-alien-type ',return-type)
                                  ,@(mapcar #'lisp-type-to-alien-type ',arg-types)))
           ,,@arg-names))
       ;; Store in registry
       (setf *inlined-functions* (map:assoc *inlined-functions* ',name
                                            '(,c-name ,library ,return-type ,arg-types))))))

(defmacro inline-foreign-call (c-name return-type arg-types &rest args)
  "Make an inline foreign function call"
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,c-name
                          (sb-alien:function 
                           ,(lisp-type-to-alien-type return-type)
                           ,@(mapcar #'lisp-type-to-alien-type arg-types)))
    ,@args))

(defmacro with-inline-ffi (&body body)
  "Enable aggressive inlining within body"
  `(let ((*inline-aggressive* t))
     ,@body))

(defun optimize-ffi-calls (form)
  "Optimize FFI calls in a form by inlining where appropriate"
  (if (and (listp form)
           (eq (first form) 'epsilon.foreign:shared-call)
           (< (length (fourth form)) *inline-threshold*))
      ;; Convert to inline call
      (let ((c-name (second (second form)))
            (return-type (third form))
            (arg-types (fourth form))
            (args (cddddr form)))
        `(inline-foreign-call ,c-name ,return-type ,arg-types ,@args))
      ;; Return unchanged
      form))

;;; Example inlined functions

(declaim (inline %strlen))
(defun %strlen (str)
  "Inlined strlen call"
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "strlen"
                         (sb-alien:function sb-alien:unsigned-long
                                          sb-alien:c-string))
   str))

(declaim (inline %memcpy))
(defun %memcpy (dest src size)
  "Inlined memcpy call"
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "memcpy"
                         (sb-alien:function sb-alien:system-area-pointer
                                          sb-alien:system-area-pointer
                                          sb-alien:system-area-pointer
                                          sb-alien:unsigned-long))
   dest src size))

(declaim (inline %getpid))
(defun %getpid ()
  "Inlined getpid call"
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "getpid"
                         (sb-alien:function sb-alien:int))))