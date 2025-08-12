(defpackage epsilon.foreign.trampoline
  (:use cl)
  (:local-nicknames
   (map epsilon.map))
  (:export
   ;; Core trampoline system
   #:make-ffi-trampoline
   #:get-or-create-trampoline
   #:clear-trampoline-cache
   
   ;; Signature management
   #:ffi-signature
   #:ffi-signature-p
   #:make-ffi-signature
   #:ffi-signature-return-type
   #:ffi-signature-arg-types
   #:ffi-signature-trampoline
   #:register-signature
   #:get-signature
   #:clear-signature-registry
   #:register-c-type
   
   ;; Type system
   #:c-type
   #:c-type-p
   #:c-type-base
   #:c-type-size
   #:c-type-alignment
   #:c-type-signed-p
   #:get-c-type
   
   ;; Type conversion
   #:convert-to-foreign
   #:convert-from-foreign
   
   ;; Integration
   #:call-with-trampoline))

(in-package :epsilon.foreign.trampoline)

;;;; FFI Trampoline System - Compiled function calls instead of eval

;;; Type descriptors

(defstruct c-type
  "Descriptor for a C type"
  base          ; :int, :pointer, :struct, etc.
  size          ; Size in bytes
  alignment     ; Alignment requirement
  signed-p      ; For integers
  converter-to  ; Lisp -> C converter function
  converter-from) ; C -> Lisp converter function

(defvar *c-type-registry* (make-hash-table :test 'eq)
  "Registry of C type descriptors")

(defun register-c-type (name &key base size alignment signed-p converter-to converter-from)
  "Register a C type descriptor"
  (setf (gethash name *c-type-registry*)
        (make-c-type :base (or base name)
                     :size size
                     :alignment alignment
                     :signed-p signed-p
                     :converter-to converter-to
                     :converter-from converter-from)))

;; Register primitive types
(register-c-type :void :size 0 :alignment 1)
(register-c-type :char :size 1 :alignment 1 :signed-p t)
(register-c-type :unsigned-char :size 1 :alignment 1 :signed-p nil)
(register-c-type :short :size 2 :alignment 2 :signed-p t)
(register-c-type :unsigned-short :size 2 :alignment 2 :signed-p nil)
(register-c-type :int :size 4 :alignment 4 :signed-p t)
(register-c-type :unsigned-int :size 4 :alignment 4 :signed-p nil)
(register-c-type :long :size 8 :alignment 8 :signed-p t)
(register-c-type :unsigned-long :size 8 :alignment 8 :signed-p nil)
(register-c-type :float :size 4 :alignment 4)
(register-c-type :double :size 8 :alignment 8)
(register-c-type :pointer :size 8 :alignment 8) ; 64-bit assumption
(register-c-type :string :base :pointer :size 8 :alignment 8)
(register-c-type :size :base :unsigned-long :size 8 :alignment 8)

;; POSIX/C type aliases
(register-c-type :size-t :base :unsigned-long :size 8 :alignment 8)
(register-c-type :ssize-t :base :long :size 8 :alignment 8)
(register-c-type :time-t :base :long :size 8 :alignment 8)
(register-c-type :clock-t :base :long :size 8 :alignment 8)
(register-c-type :pid-t :base :int :size 4 :alignment 4)
(register-c-type :uid-t :base :unsigned-int :size 4 :alignment 4)
(register-c-type :gid-t :base :unsigned-int :size 4 :alignment 4)
(register-c-type :mode-t :base :unsigned-int :size 4 :alignment 4)
(register-c-type :off-t :base :long :size 8 :alignment 8)

(defun get-c-type (name)
  "Get a C type descriptor"
  (or (gethash name *c-type-registry*)
      (error "Unknown C type: ~A" name)))

;;; Type conversion

(defgeneric convert-to-foreign (value type)
  (:documentation "Convert a Lisp value to foreign representation"))

(defmethod convert-to-foreign (value (type (eql :int)))
  value)

(defmethod convert-to-foreign (value (type (eql :unsigned-int)))
  value)

(defmethod convert-to-foreign (value (type (eql :long)))
  value)

(defmethod convert-to-foreign (value (type (eql :unsigned-long)))
  value)

(defmethod convert-to-foreign (value (type (eql :short)))
  value)

(defmethod convert-to-foreign (value (type (eql :unsigned-short)))
  value)

(defmethod convert-to-foreign (value (type (eql :char)))
  (if (characterp value)
      (char-code value)
      value))

(defmethod convert-to-foreign (value (type (eql :unsigned-char)))
  (if (characterp value)
      (char-code value)
      value))

(defmethod convert-to-foreign (value (type (eql :float)))
  (float value 1.0))

(defmethod convert-to-foreign (value (type (eql :double)))
  (float value 1.0d0))

(defmethod convert-to-foreign ((value string) (type (eql :string)))
  ;; Create a null-terminated C string
  (sb-alien:make-alien-string value))

(defmethod convert-to-foreign ((value null) (type (eql :pointer)))
  (sb-sys:int-sap 0))

(defmethod convert-to-foreign ((value integer) (type (eql :pointer)))
  (sb-sys:int-sap value))

(defmethod convert-to-foreign (value (type (eql :pointer)))
  ;; Assume it's already a SAP
  value)

(defmethod convert-to-foreign (value (type (eql :void)))
  nil)

(defgeneric convert-from-foreign (value type)
  (:documentation "Convert a foreign value to Lisp representation"))

(defmethod convert-from-foreign (value type)
  ;; Default: return as-is
  value)

(defmethod convert-from-foreign (value (type (eql :string)))
  ;; Convert C string to Lisp string
  (if (sb-sys:sap= value (sb-sys:int-sap 0))
      nil
      ;; Read the string manually from the SAP
      (let ((chars '()))
        (loop for i from 0
              for byte = (sb-sys:sap-ref-8 value i)
              until (zerop byte)
              do (push (code-char byte) chars))
        (coerce (nreverse chars) 'string))))

(defmethod convert-from-foreign (value (type (eql :void)))
  nil)

;;; FFI Signatures

(defstruct ffi-signature
  "Descriptor for a foreign function signature"
  return-type
  arg-types
  trampoline)

(defvar *signature-registry* (make-hash-table :test 'equal)
  "Registry of function signatures")

(defun register-signature (name return-type arg-types)
  "Register a function signature"
  (setf (gethash name *signature-registry*)
        (make-ffi-signature :return-type return-type
                           :arg-types arg-types
                           :trampoline nil)))

(defun get-signature (name)
  "Get a registered signature"
  (gethash name *signature-registry*))

(defun clear-signature-registry ()
  "Clear all registered signatures"
  (clrhash *signature-registry*))

;;; Trampoline generation

(defvar *trampoline-cache* (make-hash-table :test 'equal)
  "Cache of compiled trampolines keyed by (return-type . arg-types)")

(defun make-alien-type (type)
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
    (:size 'sb-alien:unsigned-long)
    ;; POSIX/C types
    (:size-t 'sb-alien:unsigned-long)
    (:ssize-t 'sb-alien:long)
    (:time-t 'sb-alien:long)
    (:clock-t 'sb-alien:long)
    (:pid-t 'sb-alien:int)
    (:uid-t 'sb-alien:unsigned-int)
    (:gid-t 'sb-alien:unsigned-int)
    (:mode-t 'sb-alien:unsigned-int)
    (:off-t 'sb-alien:long)
    (:buffer 'sb-alien:system-area-pointer)
    (t (error "Unknown type for alien: ~A" type))))

(defun make-ffi-trampoline (return-type arg-types)
  "Create a compiled trampoline for a specific function signature"
  (let* ((arg-names (loop for i from 0 below (length arg-types)
                          collect (intern (format nil "ARG~D" i))))
         (alien-return-type (make-alien-type return-type))
         (alien-arg-types (mapcar #'make-alien-type arg-types))
         (converter-forms (loop for name in arg-names
                                for type in arg-types
                                collect `(convert-to-foreign ,name ',type))))
    (compile nil
             `(lambda (fn-addr ,@arg-names)
                (declare (optimize (speed 3) (safety 0) (debug 0)))
                (let ((result
                       (sb-alien:alien-funcall
                        (sb-alien:sap-alien 
                         (sb-sys:int-sap fn-addr)
                         (sb-alien:function ,alien-return-type ,@alien-arg-types))
                        ,@converter-forms)))
                  (convert-from-foreign result ',return-type))))))

(defun get-or-create-trampoline (return-type arg-types)
  "Get a cached trampoline or create a new one"
  (let ((key (cons return-type arg-types)))
    (or (gethash key *trampoline-cache*)
        (setf (gethash key *trampoline-cache*)
              (make-ffi-trampoline return-type arg-types)))))

(defun clear-trampoline-cache ()
  "Clear the trampoline cache"
  (clrhash *trampoline-cache*))

;;; Integration with epsilon.foreign

(defun call-with-trampoline (fn-addr return-type arg-types args)
  "Call a foreign function using a trampoline"
  (let ((trampoline (get-or-create-trampoline return-type arg-types)))
    (apply trampoline fn-addr args)))