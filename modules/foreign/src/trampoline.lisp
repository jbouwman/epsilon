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
   #:sap-to-lisp-string

   ;; Integration
   #:call-with-trampoline)
  (:enter t))

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

(defvar *c-type-registry* map:+empty+
  "Registry of C type descriptors")

(defun register-c-type (name &key base size alignment signed-p converter-to converter-from)
  "Register a C type descriptor"
  (setf *c-type-registry*
        (map:assoc *c-type-registry* name
                   (make-c-type :base (or base name)
                                :size size
                                :alignment alignment
                                :signed-p signed-p
                                :converter-to converter-to
                                :converter-from converter-from))))

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
(register-c-type :string-out :base :pointer :size 8 :alignment 8) ; C string return
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
  (or (map:get *c-type-registry* name)
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
  ;; Note: String allocation is now handled by make-string-handling-trampoline
  ;; This method exists for edge cases where convert-to-foreign is called directly
  ;; In trampoline calls, strings are allocated/freed with proper cleanup
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

;; Default method for unspecialized types (e.g., enum types)
(defmethod convert-to-foreign (value type)
  ;; Check if it's a registered type with a converter
  (let ((type-info (map:get *c-type-registry* type)))
    (if (and type-info (c-type-converter-to type-info))
        (funcall (c-type-converter-to type-info) value type)
        value)))

(defgeneric convert-from-foreign (value type)
  (:documentation "Convert a foreign value to Lisp representation"))

(defmethod convert-from-foreign (value type)
  ;; Default: check for registered converter, else return as-is
  (let ((type-info (map:get *c-type-registry* type)))
    (if (and type-info (c-type-converter-from type-info))
        (funcall (c-type-converter-from type-info) value type)
        value)))

(defun sap-to-lisp-string (sap)
  "Convert a SAP pointing to a null-terminated C string to a Lisp string.
   Returns nil for null pointers."
  (cond
    ;; Already a string (shouldn't happen but handle it)
    ((stringp sap) sap)
    ;; Null pointer
    ((or (null sap)
         (and (sb-sys:system-area-pointer-p sap)
              (zerop (sb-sys:sap-int sap))))
     nil)
    ;; Read the null-terminated string from memory
    (t
     (let ((chars '()))
       (loop for i from 0
             for byte = (sb-sys:sap-ref-8 sap i)
             until (zerop byte)
             do (push (code-char byte) chars))
       (coerce (nreverse chars) 'string)))))

(defmethod convert-from-foreign (value (type (eql :string)))
  "Convert C string pointer to Lisp string"
  (sap-to-lisp-string value))

(defmethod convert-from-foreign (value (type (eql :string-out)))
  "Convert C string pointer to Lisp string (alias for :string return)"
  (sap-to-lisp-string value))

(defmethod convert-from-foreign (value (type (eql :void)))
  nil)

;;; FFI Signatures

(defstruct ffi-signature
  "Descriptor for a foreign function signature"
  return-type
  arg-types
  trampoline)

(defvar *signature-registry* map:+empty+
  "Registry of function signatures")

(defun register-signature (name return-type arg-types)
  "Register a function signature"
  (setf *signature-registry*
        (map:assoc *signature-registry* name
                   (make-ffi-signature :return-type return-type
                                       :arg-types arg-types
                                       :trampoline nil))))

(defun get-signature (name)
  "Get a registered signature"
  (map:get *signature-registry* name))

(defun clear-signature-registry ()
  "Clear all registered signatures"
  (setf *signature-registry* map:+empty+))

;;; Trampoline generation

(defvar *trampoline-cache* map:+empty+
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
    ;; String types pass as pointers at the C level
    (:string 'sb-alien:system-area-pointer)
    (:string-out 'sb-alien:system-area-pointer)
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

(defun string-arg-p (type)
  "Check if type is a string input argument requiring allocation"
  (eq type :string))

(defun string-return-p (type)
  "Check if return type is a string requiring conversion"
  (member type '(:string :string-out)))

(defun make-ffi-trampoline (return-type arg-types)
  "Create a compiled trampoline for a specific function signature.

   Handles automatic string memory management:
   - :string arguments are allocated, passed as SAP, and freed after the call
   - :string/:string-out returns are converted from C string pointers to Lisp strings"
  (let* ((arg-names (loop for i from 0 below (length arg-types)
                          collect (intern (format nil "ARG~D" i))))
         (string-args (loop for name in arg-names
                            for type in arg-types
                            when (string-arg-p type)
                            collect (cons name (intern (format nil "~A-CSTR" name)))))
         (alien-return-type (make-alien-type return-type))
         (alien-arg-types (mapcar #'make-alien-type arg-types)))
    (if string-args
        ;; Generate trampoline with string allocation/cleanup
        (make-string-handling-trampoline return-type arg-types arg-names
                                          string-args alien-return-type alien-arg-types)
        ;; Simple case: no string arguments
        (make-simple-trampoline return-type arg-types arg-names
                                alien-return-type alien-arg-types))))

(defun make-simple-trampoline (return-type arg-types arg-names alien-return-type alien-arg-types)
  "Create a simple trampoline without string handling"
  (let ((converter-forms (loop for name in arg-names
                               for type in arg-types
                               collect `(convert-to-foreign ,name ',type))))
    ;; Muffle compiler notes about type coercions - these are expected in FFI code
    (handler-bind ((sb-ext:compiler-note #'muffle-warning))
      (compile nil
               `(lambda (fn-addr ,@arg-names)
                  (declare (optimize (speed 3) (safety 0) (debug 0)))
                  (let ((result
                         (sb-alien:alien-funcall
                          (sb-alien:sap-alien
                           (sb-sys:int-sap fn-addr)
                           (sb-alien:function ,alien-return-type ,@alien-arg-types))
                          ,@converter-forms)))
                    (convert-from-foreign result ',return-type)))))))

(defun make-string-handling-trampoline (return-type arg-types arg-names
                                        string-args alien-return-type alien-arg-types)
  "Create a trampoline that handles string allocation/cleanup automatically.

   String arguments are:
   1. Allocated as C strings using sb-alien:make-alien-string
   2. Passed as SAPs to the foreign function
   3. Freed in unwind-protect cleanup"
  (let* ((cstr-bindings (loop for (arg-name . cstr-name) in string-args
                              collect `(,cstr-name (sb-alien:make-alien-string ,arg-name))))
         (cstr-cleanup (loop for (nil . cstr-name) in string-args
                             collect `(sb-alien:free-alien ,cstr-name)))
         (string-arg-names (mapcar #'car string-args))
         (call-args (loop for name in arg-names
                          for type in arg-types
                          collect (if (string-arg-p type)
                                      ;; Use the allocated C string's SAP
                                      `(sb-alien:alien-sap ,(cdr (assoc name string-args)))
                                      ;; Normal conversion
                                      `(convert-to-foreign ,name ',type)))))
    ;; Muffle compiler notes about type coercions - these are expected in FFI code
    (handler-bind ((sb-ext:compiler-note #'muffle-warning))
      (compile nil
               `(lambda (fn-addr ,@arg-names)
                  (declare (optimize (speed 3) (safety 0) (debug 0))
                           (ignorable ,@string-arg-names))
                  (let ,cstr-bindings
                    (unwind-protect
                         (let ((result
                                (sb-alien:alien-funcall
                                 (sb-alien:sap-alien
                                  (sb-sys:int-sap fn-addr)
                                  (sb-alien:function ,alien-return-type ,@alien-arg-types))
                                 ,@call-args)))
                           (convert-from-foreign result ',return-type))
                      ,@cstr-cleanup)))))))

(defun get-or-create-trampoline (return-type arg-types)
  "Get a cached trampoline or create a new one"
  (let ((key (cons return-type arg-types)))
    (or (map:get *trampoline-cache* key)
        (let ((trampoline (make-ffi-trampoline return-type arg-types)))
          (setf *trampoline-cache* (map:assoc *trampoline-cache* key trampoline))
          trampoline))))

(defun clear-trampoline-cache ()
  "Clear the trampoline cache"
  (setf *trampoline-cache* map:+empty+))

;;; Integration with epsilon.foreign

(defun call-with-trampoline (fn-addr return-type arg-types args)
  "Call a foreign function using a trampoline"
  (let ((trampoline (get-or-create-trampoline return-type arg-types)))
    (apply trampoline fn-addr args)))
