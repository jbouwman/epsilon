(defpackage :epsilon.sys.ffi.sys
  (:use
   :cl
   :epsilon.lib.array
   :epsilon.lib.syntax
   :epsilon.lib.function
   :epsilon.lib.list
   :epsilon.lib.symbol
   :epsilon.lib.type
   :sb-alien)
  (:local-nicknames
   (:map :epsilon.lib.map))
  (:export
   ;; C ABI utils
   :canonicalize-symbol-name-case
   :defcfun-helper-forms

   ;; Pointers
   :foreign-pointer
   :pointerp
   :pointer-eq
   :null-pointer
   :null-pointer-p
   :inc-pointer
   :make-pointer
   :pointer-address

   ;; Memory operators
   :%mem-ref
   :%mem-set

   ;; Foreign symbols
   :%foreign-symbol-pointer

   ;; Memory management
   :%foreign-alloc
   :foreign-free
   :with-foreign-pointer

   ;; Foreign functions
   :%foreign-funcall
   :%foreign-funcall-pointer
   :%foreign-funcall-varargs
   :%foreign-funcall-pointer-varargs
   :%foreign-type-alignment

   ;; Foreign types
   :%foreign-type-size

   ;; Foreign libraries
   :%load-foreign-library
   :%close-foreign-library

   ;; Callbacks
   :%defcallback
   :%callback

   ;; Shareable vectors
   :make-shareable-byte-vector
   :with-pointer-to-vector-data

   ;; Compiler macro utils
   :constant-form-p
   :constant-form-value))

(defpackage :epsilon.sys.ffi
  (:use
   :common-lisp
   :epsilon.sys.ffi.sys
   :epsilon.lib.array
   :epsilon.lib.syntax
   :epsilon.lib.char
   :epsilon.lib.condition
   :epsilon.lib.function
   :epsilon.lib.list
   :epsilon.lib.symbol
   :epsilon.lib.vector
   :epsilon.sys.env)
  (:local-nicknames (:map :epsilon.lib.map))
  (:export
   ;; Types.
   :foreign-pointer

   ;; FIXME: the following types are undocumented. They should
   ;; probably be replaced with a proper type introspection API
   ;; though.
   :*built-in-foreign-types*
   :*other-builtin-types*
   :*built-in-integer-types*
   :*built-in-float-types*

   ;; Primitive pointer operations.
   :foreign-free
   :foreign-alloc
   :mem-aptr
   :mem-aref
   :mem-ref
   :pointerp
   :pointer-eq
   :null-pointer
   :null-pointer-p
   :inc-pointer
   :incf-pointer
   :with-foreign-pointer
   :make-pointer
   :pointer-address

   ;; Shareable vectors.
   :make-shareable-byte-vector
   :with-pointer-to-vector-data

   ;; Foreign string operations.
   :*default-foreign-encoding*
   :foreign-string-alloc
   :foreign-string-free
   :foreign-string-to-lisp
   :lisp-string-to-foreign
   :with-foreign-string
   :with-foreign-strings
   :with-foreign-pointer-as-string

   ;; Foreign array operations.
   ;; TODO: document these
   :foreign-array-alloc
   :foreign-array-free
   :foreign-array-to-lisp
   :lisp-array-to-foreign
   :with-foreign-array
   :foreign-aref

   ;; Foreign function operations.
   :defcfun
   :foreign-funcall
   :foreign-funcall-pointer
   :foreign-funcall-varargs
   :foreign-funcall-pointer-varargs
   :translate-camelcase-name
   :translate-name-from-foreign
   :translate-name-to-foreign
   :translate-underscore-separated-name

   ;; Foreign library operations.
   :*foreign-library-directories*
   :*darwin-framework-directories*
   :foreign-library
   :foreign-library-load-state
   :foreign-library-name
   :foreign-library-pathname
   :foreign-library-type
   :foreign-library-loaded-p
   :list-foreign-libraries
   :define-foreign-library
   :load-foreign-library
   :load-foreign-library-error
   :use-foreign-library
   :close-foreign-library
   :reload-foreign-libraries

   ;; Callbacks.
   :callback
   :get-callback
   :defcallback

   ;; Foreign type operations.
   :defcstruct
   :defcunion
   :defctype
   :defcenum
   :defbitfield
   :define-foreign-type
   :define-parse-method
   :define-c-struct-wrapper
   :foreign-enum-keyword
   :foreign-enum-keyword-list
   :foreign-enum-value
   :foreign-bitfield-symbol-list
   :foreign-bitfield-symbols
   :foreign-bitfield-value
   :foreign-slot-pointer
   :foreign-slot-value
   :foreign-slot-type
   :foreign-slot-offset
   :foreign-slot-count
   :foreign-slot-names
   :foreign-type-alignment
   :foreign-type-size
   :with-foreign-object
   :with-foreign-objects
   :with-foreign-slots
   :convert-to-foreign
   :convert-from-foreign
   :convert-into-foreign-memory
   :free-converted-object
   :translation-forms-for-class

   ;; Extensible foreign type operations.
   :define-translation-method          ; FIXME: undocumented
   :translate-to-foreign
   :translate-from-foreign
   :translate-into-foreign-memory
   :free-translated-object
   :expand-to-foreign-dyn
   :expand-to-foreign
   :expand-from-foreign
   :expand-into-foreign-memory

   ;; Foreign globals.
   :defcvar
   :get-var-pointer
   :foreign-symbol-pointer))
