;;;; type-database.lisp - C Type Information Database
;;;;
;;;; Stores extracted C type information in a structured format for use
;;;; by the JIT FFI system.

(defpackage epsilon.foreign.type-database
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (log epsilon.log))
  (:export
   ;; Type info structures
   #:c-type-info
   #:make-c-type-info
   #:c-type-info-name
   #:c-type-info-kind
   #:c-type-info-size
   #:c-type-info-alignment
   #:c-type-info-fields
   #:c-type-info-base-type
   #:c-type-info-return-type
   #:c-type-info-param-types
   #:c-type-info-constants
   #:c-field-info
   #:make-c-field-info
   #:c-field-info-name
   #:c-field-info-type
   #:c-field-info-type-kind
   #:c-field-info-offset
   #:c-field-info-size
   #:c-field-info-is-pointer
   #:c-enum-constant
   #:make-c-enum-constant
   #:c-enum-constant-name
   #:c-enum-constant-value
   ;; Database operations
   #:*type-database*
   #:clear-database
   #:register-type
   #:lookup-type
   #:all-types
   #:types-by-kind
   ;; Type conversions
   #:c-type-to-ffi-type
   #:ffi-type-to-sbcl-alien
   ;; Query functions
   #:struct-field-offset
   #:struct-size
   #:struct-alignment
   #:enum-value
   #:database-statistics)
  (:enter t))

;;; Type Information Structures

(defstruct c-type-info
  "Complete information about a C type."
  (name nil :type (or string null))
  (kind nil :type keyword)  ; :struct :union :enum :typedef :function :primitive
  (size 0 :type integer)
  (alignment 0 :type integer)
  (fields nil :type list)        ; List of c-field-info for structs/unions
  (base-type nil)                ; For typedefs, the underlying type
  (return-type nil)              ; For functions
  (param-types nil :type list)   ; For functions
  (constants nil :type list)     ; For enums, list of c-enum-constant
  (source-file nil :type (or string null))
  (source-line 0 :type integer))

(defstruct c-field-info
  "Information about a struct/union field."
  (name nil :type (or string null))
  (type nil :type (or string null))
  (type-kind nil :type keyword)
  (offset 0 :type integer)       ; Bit offset from struct start
  (size 0 :type integer)         ; Size in bytes
  (bit-width nil :type (or integer null))  ; For bit fields
  (is-pointer nil :type boolean)
  (array-size nil :type (or integer null)))

(defstruct c-enum-constant
  "Information about an enum constant."
  (name nil :type (or string null))
  (value 0 :type integer))

;;; Type Database

(defvar *type-database* (map:make-map)
  "Global database of known C types, keyed by qualified name")

(defvar *header-cache* (map:make-map)
  "Cache of parsed headers to avoid re-parsing")

(defun clear-database ()
  "Clear the type database."
  (setf *type-database* (map:make-map)
        *header-cache* (map:make-map)))

(defun register-type (type-info)
  "Register a type in the database."
  (when (c-type-info-name type-info)
    (setf *type-database*
          (map:assoc *type-database*
                     (c-type-info-name type-info)
                     type-info))
    (log:debug "Registered type: ~A" (c-type-info-name type-info)))
  type-info)

(defun lookup-type (name)
  "Look up a type by name in the database."
  (map:get *type-database* name))

(defun all-types ()
  "Return all registered types."
  (map:vals *type-database*))

(defun types-by-kind (kind)
  "Return all types of a specific kind."
  (remove-if-not (lambda (ti) (eq (c-type-info-kind ti) kind))
                 (map:vals *type-database*)))

;;; Primitive Types

(defun register-primitive-types ()
  "Register standard C primitive types."
  (dolist (type-spec '(("void" :void 0 1)
                       ("_Bool" :bool 1 1)
                       ("char" :char 1 1)
                       ("signed char" :char 1 1)
                       ("unsigned char" :uchar 1 1)
                       ("short" :short 2 2)
                       ("unsigned short" :ushort 2 2)
                       ("int" :int 4 4)
                       ("unsigned int" :uint 4 4)
                       ("long" :long 8 8)
                       ("unsigned long" :ulong 8 8)
                       ("long long" :long-long 8 8)
                       ("unsigned long long" :ulong-long 8 8)
                       ("float" :float 4 4)
                       ("double" :double 8 8)
                       ("long double" :long-double 16 16)))
    (register-type
     (make-c-type-info
      :name (first type-spec)
      :kind :primitive
      :size (third type-spec)
      :alignment (fourth type-spec)))))

;; Register primitives on load
(register-primitive-types)

;;; Type Conversions

(defun c-type-to-ffi-type (type-info)
  "Convert a c-type-info to an FFI type specifier."
  (case (c-type-info-kind type-info)
    (:primitive
     (let ((name (c-type-info-name type-info)))
       (cond
         ((string= name "void") :void)
         ((string= name "_Bool") :bool)
         ((or (string= name "char") (string= name "signed char")) :char)
         ((string= name "unsigned char") :unsigned-char)
         ((string= name "short") :short)
         ((string= name "unsigned short") :unsigned-short)
         ((string= name "int") :int)
         ((string= name "unsigned int") :unsigned-int)
         ((string= name "long") :long)
         ((string= name "unsigned long") :unsigned-long)
         ((string= name "long long") :long-long)
         ((string= name "unsigned long long") :unsigned-long-long)
         ((string= name "float") :float)
         ((string= name "double") :double)
         (t :int))))  ; Default fallback
    (:pointer :pointer)
    (:struct (intern (format nil "STRUCT-~A" (c-type-info-name type-info)) :keyword))
    (:enum :int)  ; Enums are typically int-sized
    (:typedef
     (let ((base (lookup-type (c-type-info-base-type type-info))))
       (if base
           (c-type-to-ffi-type base)
           :int)))
    (otherwise :int)))

(defun ffi-type-to-sbcl-alien (ffi-type)
  "Convert an FFI type to SBCL alien type specifier."
  (case ffi-type
    (:void 'sb-alien:void)
    (:bool 'sb-alien:boolean)
    (:char 'sb-alien:char)
    (:unsigned-char 'sb-alien:unsigned-char)
    (:short 'sb-alien:short)
    (:unsigned-short 'sb-alien:unsigned-short)
    (:int 'sb-alien:int)
    (:unsigned-int 'sb-alien:unsigned-int)
    (:long 'sb-alien:long)
    (:unsigned-long 'sb-alien:unsigned-long)
    (:long-long '(sb-alien:signed 64))
    (:unsigned-long-long '(sb-alien:unsigned 64))
    (:float 'sb-alien:single-float)
    (:double 'sb-alien:double-float)
    (:pointer 'sb-alien:system-area-pointer)
    (otherwise 'sb-alien:int)))

;;; Struct Field Offset Calculation

(defun calculate-struct-layout (fields)
  "Calculate offsets and total size for struct fields.
   Returns (values fields-with-offsets total-size alignment)."
  (let ((offset 0)
        (max-align 1)
        (result nil))
    (dolist (field fields)
      (let* ((field-align (or (c-field-info-size field) 1))
             (aligned-offset (align-to offset field-align)))
        (setf (c-field-info-offset field) aligned-offset)
        (push field result)
        (setf offset (+ aligned-offset (c-field-info-size field)))
        (setf max-align (max max-align field-align))))
    ;; Final size must be aligned to max alignment
    (values (nreverse result)
            (align-to offset max-align)
            max-align)))

(defun align-to (offset alignment)
  "Align OFFSET to ALIGNMENT boundary."
  (let ((remainder (mod offset alignment)))
    (if (zerop remainder)
        offset
        (+ offset (- alignment remainder)))))

;;; Query Functions

(defun struct-field-offset (struct-name field-name)
  "Get the byte offset of a field in a struct."
  (let ((type-info (lookup-type struct-name)))
    (when (and type-info (eq (c-type-info-kind type-info) :struct))
      (let ((field (find field-name (c-type-info-fields type-info)
                         :key #'c-field-info-name
                         :test #'string=)))
        (when field
          (floor (c-field-info-offset field) 8))))))

(defun struct-size (struct-name)
  "Get the size of a struct in bytes."
  (let ((type-info (lookup-type struct-name)))
    (when type-info
      (c-type-info-size type-info))))

(defun struct-alignment (struct-name)
  "Get the alignment of a struct in bytes."
  (let ((type-info (lookup-type struct-name)))
    (when type-info
      (c-type-info-alignment type-info))))

(defun enum-value (enum-name constant-name)
  "Get the value of an enum constant."
  (let ((type-info (lookup-type enum-name)))
    (when (and type-info (eq (c-type-info-kind type-info) :enum))
      (let ((constant (find constant-name (c-type-info-constants type-info)
                            :key #'c-enum-constant-name
                            :test #'string=)))
        (when constant
          (c-enum-constant-value constant))))))

;;; Statistics

(defun database-statistics ()
  "Return statistics about the type database."
  (let ((types (map:vals *type-database*)))
    (list :total-types (length types)
          :structs (count :struct types :key #'c-type-info-kind)
          :unions (count :union types :key #'c-type-info-kind)
          :enums (count :enum types :key #'c-type-info-kind)
          :typedefs (count :typedef types :key #'c-type-info-kind)
          :functions (count :function types :key #'c-type-info-kind)
          :primitives (count :primitive types :key #'c-type-info-kind))))
