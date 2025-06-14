(defpackage :epsilon.sys.ffi
  (:use
   :cl
   :epsilon.lib.array
   :epsilon.lib.char
   :epsilon.lib.condition
   :epsilon.lib.function
   :epsilon.lib.list
   :epsilon.lib.symbol
   :epsilon.lib.syntax
   :epsilon.lib.type
   :epsilon.lib.vector
   :epsilon.sys.env
   :sb-alien)
  (:local-nicknames
   (:char :epsilon.lib.char)
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
   :constant-form-value
   
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

(in-package :epsilon.sys.ffi)

(eval-when (:compile-toplevel :load-toplevel)

(defun quoted-form-p (form)
  (and (proper-list-p form)
       (= 2 (length form))
       (eql 'quote (car form))))

(defun constant-form-p (form &optional env)
  (let ((form (if (symbolp form)
                  (macroexpand form env)
                  form)))
    (or (quoted-form-p form)
        (constantp form env))))

(defun constant-form-value (form &optional env)
  (declare (ignorable env))
  (cond
    ((quoted-form-p form)
     (second form))
    (t
     (sb-int:constant-form-value form env))))

)

;;;# Symbol Case

(declaim (inline canonicalize-symbol-name-case))
(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Basic Pointer Operations

(deftype foreign-pointer ()
  'sb-sys:system-area-pointer)

(declaim (inline pointerp))
(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (sb-sys:system-area-pointer-p ptr))

(declaim (inline pointer-eq))
(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (declare (type system-area-pointer ptr1 ptr2))
  (sb-sys:sap= ptr1 ptr2))

(declaim (inline null-pointer))
(defun null-pointer ()
  "Construct and return a null pointer."
  (sb-sys:int-sap 0))

(declaim (inline null-pointer-p))
(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (declare (type system-area-pointer ptr))
  (zerop (sb-sys:sap-int ptr)))

(declaim (inline inc-pointer))
(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (declare (type system-area-pointer ptr)
           (type integer offset))
  (sb-sys:sap+ ptr offset))

(declaim (inline make-pointer))
(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  ;; (declare (type (unsigned-byte 32) address))
  (sb-sys:int-sap address))

(declaim (inline pointer-address))
(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (declare (type system-area-pointer ptr))
  (sb-sys:sap-int ptr))

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main FFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common usage
;;; when the memory has dynamic extent.

(declaim (inline %foreign-alloc))
(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  ;; (declare (type (unsigned-byte 32) size))
  (alien-sap (make-alien (unsigned 8) size)))

(declaim (inline foreign-free))
(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (declare (type system-area-pointer ptr))
  (free-alien (sap-alien ptr (* (unsigned 8)))))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  ;; If the size is constant we can stack-allocate.
  (if (constantp size)
      (let ((alien-var (gensym "ALIEN")))
        `(with-alien ((,alien-var (array (unsigned 8) ,(eval size))))
           (let ((,size-var ,(eval size))
                 (,var (alien-sap ,alien-var)))
             (declare (ignorable ,size-var))
             ,@body)))
      `(let* ((,size-var ,size)
              (,var (%foreign-alloc ,size-var)))
         (unwind-protect
              (progn ,@body)
           (foreign-free ,var)))))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(declaim (inline make-shareable-byte-vector))
(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes that can be passed to
WITH-POINTER-TO-VECTOR-DATA."
  ; (declare (type sb-int:index size))
  (make-array size :element-type 'u8))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (let ((vector-var (gensym "VECTOR")))
    `(let ((,vector-var ,vector))
       (declare (type (sb-kernel:simple-unboxed-array (*)) ,vector-var))
       (sb-sys:with-pinned-objects (,vector-var)
         (let ((,ptr-var (sb-sys:vector-sap ,vector-var)))
           ,@body)))))

;;;# Dereferencing

;;; Define the %MEM-REF and %MEM-SET functions, as well as compiler
;;; macros that optimize the case where the type keyword is constant
;;; at compile-time.
(defmacro define-mem-accessors (&body pairs)
  `(progn
     (defun %mem-ref (ptr type &optional (offset 0))
       (ecase type
         ,@(loop for (keyword fn) in pairs
                 collect `(,keyword (,fn ptr offset)))))
     (defun %mem-set (value ptr type &optional (offset 0))
       (ecase type
         ,@(loop for (keyword fn) in pairs
                 collect `(,keyword (setf (,fn ptr offset) value)))))
     (define-compiler-macro %mem-ref
         (&whole form ptr type &optional (offset 0))
       (if (constantp type)
           (ecase (eval type)
             ,@(loop for (keyword fn) in pairs
                     collect `(,keyword `(,',fn ,ptr ,offset))))
           form))
     (define-compiler-macro %mem-set
         (&whole form value ptr type &optional (offset 0))
       (if (constantp type)
           (once-only (value)
             (ecase (eval type)
               ,@(loop for (keyword fn) in pairs
                       collect `(,keyword `(setf (,',fn ,ptr ,offset)
                                                 ,value)))))
           form))))

;;; Look up alien type information and build both define-mem-accessors form
;;; and convert-foreign-type function definition.
(defmacro define-type-mapping (accessor-table alien-table)
  (let* ((accessible-types
           (remove 'void alien-table :key #'second))
         (size-and-signedp-forms
           (mapcar (lambda (name)
                     (list (eval `(alien-size ,(second name)))
                           (typep -1 `(alien ,(second name)))))
                   accessible-types)))
    `(progn
       (define-mem-accessors
         ,@(loop for (ffi-keyword alien-type fixed-accessor)
                   in accessible-types
                 and (alien-size signedp)
                   in size-and-signedp-forms
                 for (signed-ref unsigned-ref)
                   = (cdr (assoc alien-size accessor-table))
                 collect
                 `(,ffi-keyword
                   ,(or fixed-accessor
                        (if signedp signed-ref unsigned-ref)
                        (error "No accessor found for ~S"
                               alien-type)))))
       (defun convert-foreign-type (type-keyword)
         (ecase type-keyword
           ,@(loop for (ffi-keyword alien-type) in alien-table
                   collect `(,ffi-keyword (quote ,alien-type))))))))

(define-type-mapping
    ((8  sb-sys:signed-sap-ref-8  sb-sys:sap-ref-8)
     (16 sb-sys:signed-sap-ref-16 sb-sys:sap-ref-16)
     (32 sb-sys:signed-sap-ref-32 sb-sys:sap-ref-32)
     (64 sb-sys:signed-sap-ref-64 sb-sys:sap-ref-64))
    ((:char               char)
     (:unsigned-char      unsigned-char)
     (:short              short)
     (:unsigned-short     unsigned-short)
     (:int                int)
     (:unsigned-int       unsigned-int)
     (:long               long)
     (:unsigned-long      unsigned-long)
     (:long-long          long-long)
     (:unsigned-long-long unsigned-long-long)
     (:float              single-float
                          sb-sys:sap-ref-single)
     (:double             double-float
                          sb-sys:sap-ref-double)
     (:pointer            system-area-pointer
                          sb-sys:sap-ref-sap)
     (:void               void)))

;;;# Calling Foreign Functions

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (/ (sb-alien-internals:alien-type-bits
      (sb-alien-internals:parse-alien-type
       (convert-foreign-type type-keyword) nil)) 8))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  #+(and darwin ppc (not ppc64))
  (case type-keyword
    ((:double :long-long :unsigned-long-long)
     (return-from %foreign-type-alignment 8)))
  ;; No override necessary for other types...
  (/ (sb-alien-internals:alien-type-alignment
      (sb-alien-internals:parse-alien-type
       (convert-foreign-type type-keyword) nil)) 8))

(defun foreign-funcall-type-and-args (args)
  "Return an SB-ALIEN function type for ARGS."
  (let ((return-type 'void)
        types
        fargs)
    (loop while args
          do (let ((type (pop args)))
               (cond ((eq type '&optional)
                      (push type types))
                     ((not args)
                      (setf return-type (convert-foreign-type type)))
                     (t
                      (push (convert-foreign-type type) types)
                      (push (pop args) fargs)))))
    (values (nreverse types)
            (nreverse fargs)
            return-type)))

(defmacro %%foreign-funcall (name types fargs rettype)
  "Internal guts of %FOREIGN-FUNCALL."
  `(alien-funcall
    (extern-alien ,name (function ,rettype ,@types))
    ,@fargs))

(defmacro %foreign-funcall (name args &key library convention)
  "Perform a foreign function call, document it more later."
  (declare (ignore library convention))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(%%foreign-funcall ,name ,types ,fargs ,rettype)))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  "Funcall a pointer to a foreign function."
  (declare (ignore convention))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    (with-unique-names (function)
      `(with-alien ((,function (* (function ,rettype ,@types)) ,ptr))
         (alien-funcall ,function ,@fargs)))))

(defmacro %foreign-funcall-varargs (name fixed-args varargs
                                    &rest args &key convention library)
  (declare (ignore convention library))
  `(%foreign-funcall ,name ,(append fixed-args (and varargs
                                                    ;; All SBCL platforms would understand this
                                                    ;; but this is the only one where it's required.
                                                    ;; Omitting elsewhere makes it work on older
                                                    ;; versions of SBCL.
                                                    (append #+(and darwin arm64)
                                                            '(&optional)
                                                            varargs)))
                     ,@args))

(defmacro %foreign-funcall-pointer-varargs (pointer fixed-args varargs
                                            &rest args &key convention)
  (declare (ignore convention))
  `(%foreign-funcall-pointer ,pointer ,(append fixed-args
                                               (and varargs
                                                    (append #+(and darwin arm64)
                                                            '(&optional)
                                                            varargs)))
                             ,@args))


;;;# Callbacks

;;; The *CALLBACKS* hash table contains a direct mapping of FFI
;;; callback names to SYSTEM-AREA-POINTERs obtained by ALIEN-LAMBDA.
;;; SBCL will maintain the addresses of the callbacks across saved
;;; images, so it is safe to store the pointers directly.
(defvar *callbacks* (make-hash-table))

(defmacro %defcallback (name rettype arg-names arg-types body
                        &key convention)
  (check-type convention (member :stdcall :cdecl))
  `(setf (gethash ',name *callbacks*)
         (alien-sap
          (sb-alien::alien-lambda
            (,convention ,(convert-foreign-type rettype))
            ,(mapcar (lambda (sym type)
                       (list sym (convert-foreign-type type)))
               arg-names arg-types)
            ,body))))

(defun %callback (name)
  (or (gethash name *callbacks*)
      (error "Undefined callback: ~S" name)))

;;;# Loading and Closing Foreign Libraries

#+darwin
(defun call-within-initial-thread (fn &rest args)
  (if (eq sb-thread:*current-thread*
          sb-thread::*initial-thread*)
      (apply fn args)
      (let (result
            error
            (sem (sb-thread:make-semaphore)))
        (sb-thread:interrupt-thread
         sb-thread::*initial-thread*
         (lambda ()
           (sb-sys:with-interrupts
             (multiple-value-setq (result error)
               (ignore-errors (apply fn args))))
           (sb-thread:signal-semaphore sem)))
        (sb-thread:wait-on-semaphore sem)
        (if error
            (signal error)
            result))))

(declaim (inline %load-foreign-library))
(defun %load-foreign-library (name path)
  "Load a foreign library."
  (declare (ignore name))
  ;; As of MacOS X 10.6.6, loading things like CoreFoundation from a
  ;; thread other than the initial one results in a crash.
  #+(and darwin sb-thread) (call-within-initial-thread #'load-shared-object path)
  #-(and darwin sb-thread) (load-shared-object path))

;;; SBCL 1.0.21.15 renamed SB-ALIEN::SHARED-OBJECT-FILE but introduced
;;; SB-ALIEN:UNLOAD-SHARED-OBJECT which we can use instead.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unload-shared-object-present-p ()
    (multiple-value-bind (foundp kind)
        (find-symbol "UNLOAD-SHARED-OBJECT" "SB-ALIEN")
      (if (and foundp (eq kind :external))
          '(:and)
          '(:or)))))

(defun %close-foreign-library (handle)
  "Closes a foreign library."
  (sb-alien:unload-shared-object handle))

;;;# Foreign Globals

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (declare (ignore library))
  (when-let (address (sb-sys:find-foreign-symbol-address name))
    (sb-sys:int-sap address)))

(defun single-bit-p (integer)
  "Answer whether INTEGER, which must be an integer, is a single
set twos-complement bit."
  (if (<= integer 0)
      nil                            ; infinite set bits for negatives
      (loop until (logbitp 0 integer)
            do (setf integer (ash integer -1))
            finally (return (zerop (ash integer -1))))))

;;; This function is here because it needs to be defined early. It's
;;; used by DEFINE-PARSE-METHOD and DEFCTYPE to warn users when
;;; they're defining types whose names belongs to the KEYWORD or CL
;;; packages.  CFFI itself gets to use keywords without a warning.

(eval-when (:compile-toplevel :load-toplevel)
  
(defun warn-if-kw-or-belongs-to-cl (name)
  (let ((package (symbol-package name)))
    (when (and (not (eq *package* (find-package '#:epsilon.sys.ffi)))
               (member package '(#:common-lisp #:keyword)
                       :key #'find-package))
      (warn "Defining a foreign type named ~S.  This symbol belongs to the ~A ~
             package and that may interfere with other code using FFI."
            name (package-name package)))))

)

(defun split-if (test seq &optional (dir :before))
  (remove-if #'(lambda (x) (equal x (subseq seq 0 0)))
             (loop for start fixnum = 0
                     then (if (eq dir :before)
                              stop
                              (the fixnum (1+ (the fixnum stop))))
                   while (< start (length seq))
                   for stop = (position-if test seq
                                           :start (if (eq dir :elide)
                                                      start
                                                      (the fixnum (1+ start))))
                   collect (subseq seq start
                                   (if (and stop (eq dir :after))
                                       (the fixnum (1+ (the fixnum stop)))
                                       stop))
                   while stop)))

;;;# Foreign Types
;;;
;;; Type specifications are of the form (type {args}*). The type
;;; parser can specify how its arguments should look like through a
;;; lambda list.
;;;
;;; "type" is a shortcut for "(type)", ie, no args were specified.
;;;
;;; Examples of such types: boolean, (boolean), (boolean :int) If the
;;; boolean type parser specifies the lambda list: &optional
;;; (base-type :int), then all of the above three type specs would be
;;; parsed to an identical type.
;;;
;;; Type parsers, defined with DEFINE-PARSE-METHOD should return a
;;; subtype of the foreign-type class.

(defvar *default-type-parsers* (make-hash-table)
  "Hash table for :DEFAULT namespace")
(defvar *struct-type-parsers* (make-hash-table)
  "Hash table for :STRUCT namespace")
(defvar *union-type-parsers* (make-hash-table)
  "Hash table for :UNION namespace")

(define-condition cffi-error (error)
  ())

(define-condition foreign-type-error (cffi-error)
  ((type-name :initarg :type-name
              :initform (error "Must specify TYPE-NAME.")
              :accessor foreign-type-error/type-name)
   (namespace :initarg :namespace
              :initform :default
              :accessor foreign-type-error/namespace)))

(defun foreign-type-error/compound-name (e)
  (let ((name (foreign-type-error/type-name e))
        (namespace (foreign-type-error/namespace e)))
    (if (eq namespace :default)
        name
        `(,namespace ,name))))

(define-condition simple-foreign-type-error (simple-error foreign-type-error)
  ())

(defun simple-foreign-type-error (type-name namespace format-control &rest format-arguments)
  (error 'simple-foreign-type-error
         :type-name type-name :namespace namespace
         :format-control format-control :format-arguments format-arguments))

(define-condition undefined-foreign-type-error (foreign-type-error)
  ()
  (:report (lambda (e stream)
             (format stream "Unknown CFFI type ~S" (foreign-type-error/compound-name e)))))

(defun undefined-foreign-type-error (type-name &optional (namespace :default))
  (error 'undefined-foreign-type-error :type-name type-name :namespace namespace))

;; TODO this is not according to the C namespace rules,
;; see bug: https://github.com/cffi/cffi/issues/266
(deftype c-namespace-name ()
  '(member :default :struct :union))

(defun namespace-table (namespace)
  (ecase namespace
    (:default *default-type-parsers*)
    (:struct *struct-type-parsers*)
    (:union *union-type-parsers*)))

;; for C namespaces read: https://stackoverflow.com/questions/12579142/type-namespace-in-c
;; (section 6.2.3 Name spaces of identifiers)
;; NOTE: :struct is probably an unfortunate name for the tagged (?) namespace
(defun find-type-parser (symbol &optional (namespace :default))
  "Return the type parser for SYMBOL. NAMESPACE is either :DEFAULT (for
variables, functions, and typedefs) or :STRUCT (for structs, unions, and enums)."
  (check-type symbol (and symbol (not null)))
  (or (gethash symbol (namespace-table namespace))
      (undefined-foreign-type-error symbol namespace)))

(defun find-default-type-parser (symbol)
  (check-type symbol (and symbol (not null)))
  (or (gethash symbol *default-type-parsers*)
      (undefined-foreign-type-error symbol :default)))

(defun (setf find-type-parser) (func symbol &optional (namespace :default))
  "Set the type parser for SYMBOL."
  (check-type symbol (and symbol (not null)))
  ;; TODO Shall we signal a redefinition warning here?
  (setf (gethash symbol (namespace-table namespace)) func))

(defun undefine-foreign-type (symbol &optional (namespace :default))
  (remhash symbol (namespace-table namespace))
  (values))

;;; Using a generic function would have been nicer but generates lots
;;; of style warnings in SBCL.  (Silly reason, yes.)
(defmacro define-parse-method (name lambda-list &body body)
  "Define a type parser on NAME and lists whose CAR is NAME."
  (discard-docstring body)
  (warn-if-kw-or-belongs-to-cl name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (find-type-parser ',name)
           (lambda ,lambda-list ,@body))
     ',name))

;;; Utility function for the simple case where the type takes no
;;; arguments.
(defun notice-foreign-type (name type &optional (namespace :default))
  (setf (find-type-parser name namespace) (lambda () type))
  name)

;;;# Generic Functions on Types

(defgeneric canonicalize (foreign-type)
  (:documentation
   "Return the most primitive foreign type for FOREIGN-TYPE, either a built-in
type--a keyword--or a struct/union type--a list of the form (:STRUCT/:UNION name).
Signals an error if FOREIGN-TYPE is undefined."))

(defgeneric aggregatep (foreign-type)
  (:documentation
   "Return true if FOREIGN-TYPE is an aggregate type."))

(defgeneric foreign-type-alignment (foreign-type)
  (:documentation
   "Return the structure alignment in bytes of a foreign type."))

(defgeneric foreign-type-size (foreign-type)
  (:documentation
   "Return the size in bytes of a foreign type."))

(define-compiler-macro foreign-type-size (&whole form foreign-type)
  (if (constant-form-p foreign-type)
      (foreign-type-size (constant-form-value foreign-type))
      form))

(defgeneric unparse-type (foreign-type)
  (:documentation
   "Unparse FOREIGN-TYPE to a type specification (symbol or list)."))

;;;# Foreign Types

(defclass foreign-type ()
  ()
  (:documentation "Base class for all foreign types."))

(defmethod make-load-form ((type foreign-type) &optional env)
  "Return the form used to dump types to a FASL file."
  (declare (ignore env))
  `(parse-type ',(unparse-type type)))

(defmethod foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (foreign-type-size (parse-type type)))

(defclass named-foreign-type (foreign-type)
  ((name
    ;; Name of this foreign type, a symbol.
    :initform (error "Must specify a NAME.")
    :initarg :name
    :accessor name)))

(defmethod print-object ((type named-foreign-type) stream)
  "Print a FOREIGN-TYPEDEF instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (name type))))

;;; Return the type's name which can be passed to PARSE-TYPE.  If
;;; that's not the case for some subclass of NAMED-FOREIGN-TYPE then
;;; it should specialize UNPARSE-TYPE.
(defmethod unparse-type ((type named-foreign-type))
  (name type))

;;;# Built-In Foreign Types

(defclass foreign-built-in-type (foreign-type)
  ((type-keyword
    ;; Keyword in SYS.FFI representing this type.
    :initform (error "A type keyword is required.")
    :initarg :type-keyword
    :accessor type-keyword))
  (:documentation "A built-in foreign type."))

(defmethod canonicalize ((type foreign-built-in-type))
  "Return the built-in type keyword for TYPE."
  (type-keyword type))

(defmethod aggregatep ((type foreign-built-in-type))
  "Returns false, built-in types are never aggregate types."
  nil)

(defmethod foreign-type-alignment ((type foreign-built-in-type))
  "Return the alignment of a built-in type."
  (%foreign-type-alignment (type-keyword type)))

(defmethod foreign-type-size ((type foreign-built-in-type))
  "Return the size of a built-in type."
  (%foreign-type-size (type-keyword type)))

(defmethod unparse-type ((type foreign-built-in-type))
  "Returns the symbolic representation of a built-in type."
  (type-keyword type))

(defmethod print-object ((type foreign-built-in-type) stream)
  "Print a FOREIGN-TYPE instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (type-keyword type))))

(defvar *built-in-foreign-types* nil)

(defmacro define-built-in-foreign-type (keyword)
  "Defines a built-in foreign-type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew ,keyword *built-in-foreign-types*)
     (notice-foreign-type
      ,keyword (make-instance 'foreign-built-in-type :type-keyword ,keyword))))

;;;# Foreign Pointer Types

(defclass foreign-pointer-type (foreign-built-in-type)
  ((pointer-type
    ;; Type of object pointed at by this pointer, or nil for an
    ;; untyped (void) pointer.
    :initform nil
    :initarg :pointer-type
    :accessor pointer-type))
  (:default-initargs :type-keyword :pointer))

;;; Define the type parser for the :POINTER type.  If no type argument
;;; is provided, a void pointer will be created.
(let ((void-pointer (make-instance 'foreign-pointer-type)))
  (define-parse-method :pointer (&optional type)
    (if type
        (make-instance 'foreign-pointer-type :pointer-type (parse-type type))
        ;; A bit of premature optimization here.
        void-pointer)))

;;; Unparse a foreign pointer type when dumping to a fasl.
(defmethod unparse-type ((type foreign-pointer-type))
  (if (pointer-type type)
      `(:pointer ,(unparse-type (pointer-type type)))
      :pointer))

;;; Print a foreign pointer type unreadably in unparsed form.
(defmethod print-object ((type foreign-pointer-type) stream)
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (unparse-type type))))

;;;# Structure Type

(defgeneric bare-struct-type-p (foreign-type)
  (:documentation
   "Return true if FOREIGN-TYPE is a bare struct type or an alias of a bare struct type. "))

(defmethod bare-struct-type-p ((type foreign-type))
  "Return true if FOREIGN-TYPE is a bare struct type or an alias of a bare struct type. "
  nil)

(defclass foreign-struct-type (named-foreign-type)
  ((slots
    ;; Hash table of slots in this structure, keyed by name.
    :initform (make-hash-table)
    :initarg :slots
    :accessor slots)
   (size
    ;; Cached size in bytes of this structure.
    :initarg :size
    :accessor size)
   (alignment
    ;; This struct's alignment requirements
    :initarg :alignment
    :accessor alignment)
   (bare
    ;; we use this flag to support the (old, deprecated) semantics of
    ;; bare struct types. FOO means (:POINTER (:STRUCT FOO) in
    ;; functions declarations whereas FOO in a structure definition is
    ;; a proper aggregate type: (:STRUCT FOO), etc.
    :initform nil
    :initarg :bare
    :reader bare-struct-type-p)))

(defun slots-in-order (structure-type)
  "A list of the structure's slots in order."
  (sort (loop for slots being the hash-value of (structure-slots structure-type)
              collect slots)
        #'<
        :key 'slot-offset))

(defmethod canonicalize ((type foreign-struct-type))
  (if (bare-struct-type-p type)
      :pointer
      `(:struct ,(name type))))

(defmethod unparse-type ((type foreign-struct-type))
  (if (bare-struct-type-p type)
      (name type)
      (canonicalize type)))

(defmethod aggregatep ((type foreign-struct-type))
  "Returns true, structure types are aggregate."
  t)

(defmethod foreign-type-size ((type foreign-struct-type))
  "Return the size in bytes of a foreign structure type."
  (size type))

(defmethod foreign-type-alignment ((type foreign-struct-type))
  "Return the alignment requirements for this struct."
  (alignment type))

(defclass foreign-union-type (foreign-struct-type) ())

(defmethod canonicalize ((type foreign-union-type))
  (if (bare-struct-type-p type)
      :pointer
      `(:union ,(name type))))

;;;# Foreign Typedefs

(defclass foreign-type-alias (foreign-type)
  ((actual-type
    ;; The FOREIGN-TYPE instance this type is an alias for.
    :initarg :actual-type
    :accessor actual-type
    :initform (error "Must specify an ACTUAL-TYPE.")))
  (:documentation "A type that aliases another type."))

(defmethod canonicalize ((type foreign-type-alias))
  "Return the built-in type keyword for TYPE."
  (canonicalize (actual-type type)))

(defmethod aggregatep ((type foreign-type-alias))
  "Return true if TYPE's actual type is aggregate."
  (aggregatep (actual-type type)))

(defmethod foreign-type-alignment ((type foreign-type-alias))
  "Return the alignment of a foreign typedef."
  (foreign-type-alignment (actual-type type)))

(defmethod foreign-type-size ((type foreign-type-alias))
  "Return the size in bytes of a foreign typedef."
  (foreign-type-size (actual-type type)))

(defclass foreign-typedef (foreign-type-alias named-foreign-type)
  ())

(defun follow-typedefs (type)
  (if (typep type 'foreign-typedef)
      (follow-typedefs (actual-type type))
      type))

(defmethod bare-struct-type-p ((type foreign-typedef))
  (bare-struct-type-p (follow-typedefs type)))

(defun structure-slots (type)
  "The hash table of slots for the structure type."
  (slots (follow-typedefs type)))

;;;# Type Translators
;;;
;;; Type translation is done with generic functions at runtime for
;;; subclasses of TRANSLATABLE-FOREIGN-TYPE.
;;;
;;; The main interface for defining type translations is through the
;;; generic functions TRANSLATE-{TO,FROM}-FOREIGN and
;;; FREE-TRANSLATED-OBJECT.

(defclass translatable-foreign-type (foreign-type) ())

;;; ENHANCED-FOREIGN-TYPE is used to define translations on top of
;;; previously defined foreign types.
(defclass enhanced-foreign-type (translatable-foreign-type
                                 foreign-type-alias)
  ((unparsed-type :accessor unparsed-type)))

;;; If actual-type isn't parsed already, let's parse it.  This way we
;;; don't have to export PARSE-TYPE and users don't have to worry
;;; about this in DEFINE-FOREIGN-TYPE or DEFINE-PARSE-METHOD.
(defmethod initialize-instance :after ((type enhanced-foreign-type) &key)
  (unless (typep (actual-type type) 'foreign-type)
    (setf (actual-type type) (parse-type (actual-type type)))))

(defmethod unparse-type ((type enhanced-foreign-type))
  (unparsed-type type))

;;; Checks NAMEs, not object identity.
(defun check-for-typedef-cycles (type)
  (labels ((%check (cur-type seen)
             (when (typep cur-type 'foreign-typedef)
               (when (member (name cur-type) seen)
                 (simple-foreign-type-error type :default
                                            "Detected cycle in type ~S." type))
               (%check (actual-type cur-type)
                       (cons (name cur-type) seen)))))
    (%check type nil)))

;;; Only now we define PARSE-TYPE because it needs to do some extra
;;; work for ENHANCED-FOREIGN-TYPES.
(defun parse-type (type)
  (let* ((spec (ensure-list type))
         (ptype (apply (find-default-type-parser (car spec)) (cdr spec))))
    (when (typep ptype 'foreign-typedef)
      (check-for-typedef-cycles ptype))
    (when (typep ptype 'enhanced-foreign-type)
      (setf (unparsed-type ptype) type))
    ptype))

(defun ensure-parsed-base-type (type)
  (follow-typedefs
   (if (typep type 'foreign-type)
       type
       (parse-type type))))

(defun canonicalize-foreign-type (type)
  "Convert TYPE to a built-in type by following aliases.
Signals an error if the type cannot be resolved."
  (canonicalize (parse-type type)))

;;; Translate VALUE to a foreign object of the type represented by
;;; TYPE, which will be a subclass of TRANSLATABLE-FOREIGN-TYPE.
;;; Returns the foreign value and an optional second value which will
;;; be passed to FREE-TRANSLATED-OBJECT as the PARAM argument.
(defgeneric translate-to-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    value))

(defgeneric translate-into-foreign-memory (value type pointer)
  (:documentation
   "Translate the Lisp value into the foreign memory location given by pointer.  Return value is not used.")
  (:argument-precedence-order type value pointer))

;;; Similar to TRANSLATE-TO-FOREIGN, used exclusively by
;;; (SETF FOREIGN-STRUCT-SLOT-VALUE).
(defgeneric translate-aggregate-to-foreign (ptr value type))

;;; Translate the foreign object VALUE from the type repsented by
;;; TYPE, which will be a subclass of TRANSLATABLE-FOREIGN-TYPE.
;;; Returns the converted Lisp value.
(defgeneric translate-from-foreign (value type)
  (:argument-precedence-order type value)
  (:method (value type)
    (declare (ignore type))
    value))

;;; Free an object allocated by TRANSLATE-TO-FOREIGN.  VALUE is a
;;; foreign object of the type represented by TYPE, which will be a
;;; TRANSLATABLE-FOREIGN-TYPE subclass.  PARAM, if present, contains
;;; the second value returned by TRANSLATE-TO-FOREIGN, and is used to
;;; communicate between the two functions.
;;;
;;; FIXME: I don't think this PARAM argument is necessary anymore
;;; because the TYPE object can contain that information. [2008-12-31 LO]
(defgeneric free-translated-object (value type param)
  (:method (value type param)
    (declare (ignore value type param))))

;;;## Macroexpansion Time Translation
;;;
;;; The following EXPAND-* generic functions are similar to their
;;; TRANSLATE-* counterparts but are usually called at macroexpansion
;;; time. They offer a way to optimize the runtime translators.

;;; This special variable is bound by the various :around methods
;;; below to the respective form generated by the above %EXPAND-*
;;; functions.  This way, an expander can "bail out" by calling the
;;; next method.  All 6 of the below-defined GFs have a default method
;;; that simply answers the rtf bound by the default :around method.
(defvar *runtime-translator-form*)

;;; EXPAND-FROM-FOREIGN

(defgeneric expand-from-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    value))

(defmethod expand-from-foreign :around (value (type translatable-foreign-type))
  (let ((*runtime-translator-form* `(translate-from-foreign ,value ,type)))
    (call-next-method)))

(defmethod expand-from-foreign (value (type translatable-foreign-type))
  (declare (ignore value))
  *runtime-translator-form*)

;;; EXPAND-TO-FOREIGN

;; The second return value is used to tell EXPAND-TO-FOREIGN-DYN that
;; an unspecialized method was called.
(defgeneric expand-to-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    (values value t)))

(defmethod expand-to-foreign :around (value (type translatable-foreign-type))
  (let ((*runtime-translator-form* `(translate-to-foreign ,value ,type)))
    (call-next-method)))

(defmethod expand-to-foreign (value (type translatable-foreign-type))
  (declare (ignore value))
  (values *runtime-translator-form* t))

;;; EXPAND-INTO-FOREIGN-MEMORY

(defgeneric expand-into-foreign-memory (value type ptr)
  (:method (value type ptr)
    (declare (ignore type ptr))
    value))

(defmethod expand-into-foreign-memory :around
    (value (type translatable-foreign-type) ptr)
  (let ((*runtime-translator-form*
         `(translate-into-foreign-memory ,value ,type ,ptr)))
    (call-next-method)))

(defmethod expand-into-foreign-memory (value (type translatable-foreign-type) ptr)
  (declare (ignore value ptr))
  *runtime-translator-form*)

;;; EXPAND-TO-FOREIGN-DYN

(defgeneric expand-to-foreign-dyn (value var body type)
  (:method (value var body type)
    (declare (ignore type))
    `(let ((,var ,value)) ,@body)))

(defmethod expand-to-foreign-dyn :around
    (value var body (type enhanced-foreign-type))
  (let ((*runtime-translator-form*
         (with-unique-names (param)
           `(multiple-value-bind (,var ,param)
                (translate-to-foreign ,value ,type)
              (unwind-protect
                   (progn ,@body)
                (free-translated-object ,var ,type ,param))))))
    (call-next-method)))

;;; If this method is called it means the user hasn't defined a
;;; to-foreign-dyn expansion, so we use the to-foreign expansion.
;;;
;;; However, we do so *only* if there's a specialized
;;; EXPAND-TO-FOREIGN for TYPE because otherwise we want to use the
;;; above *RUNTIME-TRANSLATOR-FORM* which includes a call to
;;; FREE-TRANSLATED-OBJECT.  (Or else there would occur no translation
;;; at all.)
(defun foreign-expand-runtime-translator-or-binding (value var body type)
  (multiple-value-bind (expansion default-etp-p)
      (expand-to-foreign value type)
    (if default-etp-p
        *runtime-translator-form*
        `(let ((,var ,expansion))
           ,@body))))

(defmethod expand-to-foreign-dyn (value var body (type enhanced-foreign-type))
  (foreign-expand-runtime-translator-or-binding value var body type))

;;; EXPAND-TO-FOREIGN-DYN-INDIRECT
;;; Like expand-to-foreign-dyn, but always give form that returns a
;;; pointer to the object, even if it's directly representable in
;;; CL, e.g. numbers.

(defgeneric expand-to-foreign-dyn-indirect (value var body type)
  (:method (value var body type)
    (declare (ignore type))
    `(let ((,var ,value)) ,@body)))

(defmethod expand-to-foreign-dyn-indirect :around
    (value var body (type translatable-foreign-type))
  (let ((*runtime-translator-form*
          `(with-foreign-object (,var ',(unparse-type type))
             (translate-into-foreign-memory ,value ,type ,var)
             ,@body)))
    (call-next-method)))

(defmethod expand-to-foreign-dyn-indirect
    (value var body (type foreign-pointer-type))
  `(with-foreign-object (,var :pointer)
     (translate-into-foreign-memory ,value ,type ,var)
     ,@body))

(defmethod expand-to-foreign-dyn-indirect
    (value var body (type foreign-built-in-type))
  `(with-foreign-object (,var ,type)
     (translate-into-foreign-memory ,value ,type ,var)
     ,@body))

(defmethod expand-to-foreign-dyn-indirect
    (value var body (type translatable-foreign-type))
  (foreign-expand-runtime-translator-or-binding value var body type))

(defmethod expand-to-foreign-dyn-indirect (value var body (type foreign-type-alias))
  (expand-to-foreign-dyn-indirect value var body (actual-type type)))

;;; User interface for converting values from/to foreign using the
;;; type translators.  The compiler macros use the expanders when
;;; possible.

(defun convert-to-foreign (value type)
  (translate-to-foreign value (parse-type type)))

(define-compiler-macro convert-to-foreign (value type)
  (if (constantp type)
      (expand-to-foreign value (parse-type (eval type)))
      `(translate-to-foreign ,value (parse-type ,type))))

(defun convert-from-foreign (value type)
  (translate-from-foreign value (parse-type type)))

(define-compiler-macro convert-from-foreign (value type)
  (if (constantp type)
      (expand-from-foreign value (parse-type (eval type)))
      `(translate-from-foreign ,value (parse-type ,type))))

(defun convert-into-foreign-memory (value type ptr)
  (translate-into-foreign-memory value (parse-type type) ptr))

(define-compiler-macro convert-into-foreign-memory (value type ptr)
  (if (constantp type)
      (expand-into-foreign-memory value (parse-type (eval type)) ptr)
      `(translate-into-foreign-memory ,value (parse-type ,type) ,ptr)))

(defun free-converted-object (value type param)
  (free-translated-object value (parse-type type) param))

;;;# Enhanced typedefs

(defclass enhanced-typedef (foreign-typedef)
  ())

(defmethod translate-to-foreign (value (type enhanced-typedef))
  (translate-to-foreign value (actual-type type)))

(defmethod translate-into-foreign-memory (value (type enhanced-typedef) pointer)
  (translate-into-foreign-memory value (actual-type type) pointer))

(defmethod translate-from-foreign (value (type enhanced-typedef))
  (translate-from-foreign value (actual-type type)))

(defmethod free-translated-object (value (type enhanced-typedef) param)
  (free-translated-object value (actual-type type) param))

(defmethod expand-from-foreign (value (type enhanced-typedef))
  (expand-from-foreign value (actual-type type)))

(defmethod expand-to-foreign (value (type enhanced-typedef))
  (expand-to-foreign value (actual-type type)))

(defmethod expand-to-foreign-dyn (value var body (type enhanced-typedef))
  (expand-to-foreign-dyn value var body (actual-type type)))

(defmethod expand-into-foreign-memory (value (type enhanced-typedef) ptr)
  (expand-into-foreign-memory value (actual-type type) ptr))

;;;# User-defined Types and Translations.

(defmacro define-foreign-type (name supers slots &rest options)
  (multiple-value-bind (new-options simple-parser actual-type initargs)
      (let ((keywords '(:simple-parser :actual-type :default-initargs)))
        (apply #'values
               (remove-if (lambda (opt) (member (car opt) keywords)) options)
               (mapcar (lambda (kw) (cdr (assoc kw options))) keywords)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name ,(or supers '(enhanced-foreign-type))
         ,slots
         (:default-initargs ,@(when actual-type `(:actual-type ',actual-type))
             ,@initargs)
         ,@new-options)
       ,(when simple-parser
          `(define-parse-method ,(car simple-parser) (&rest args)
             (apply #'make-instance ',name args)))
       ',name)))

(defmacro defctype (name base-type &optional documentation)
  "Utility macro for simple C-like typedefs."
  (declare (ignore documentation))
  (warn-if-kw-or-belongs-to-cl name)
  (let* ((btype (parse-type base-type))
         (dtype (if (typep btype 'enhanced-foreign-type)
                    'enhanced-typedef
                    'foreign-typedef)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (notice-foreign-type
        ',name (make-instance ',dtype :name ',name :actual-type ,btype)))))

;;; For Verrazano.  We memoize the type this way to help detect cycles.
(defmacro defctype* (name base-type)
  "Like DEFCTYPE but defers instantiation until parse-time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let (memoized-type)
       (define-parse-method ,name ()
         (unless memoized-type
           (setf memoized-type (make-instance 'foreign-typedef :name ',name
                                              :actual-type nil)
                 (actual-type memoized-type) (parse-type ',base-type)))
         memoized-type))))

;;;# Accessing Foreign Globals

;;; Called by FOREIGN-OPTIONS in functions.lisp.
(defun parse-defcvar-options (options)
  (destructuring-bind (&key (library :default) read-only) options
    (list :library library :read-only read-only)))

(defun get-var-pointer (symbol)
  "Return a pointer to the foreign global variable relative to SYMBOL."
  (foreign-symbol-pointer (get symbol 'foreign-var-name)
                          :library (get symbol 'foreign-var-library)))

;;; Note: this will lookup not only variables but also functions.
(defun foreign-symbol-pointer (name &key (library :default))
  (check-type name string)
  (%foreign-symbol-pointer
   name (if (eq library :default)
            :default
            (foreign-library-handle
             (get-foreign-library library)))))

(defun fs-pointer-or-lose (foreign-name library)
  "Like foreign-symbol-ptr but throws an error instead of
returning nil when foreign-name is not found."
  (or (foreign-symbol-pointer foreign-name :library library)
      (error "Trying to access undefined foreign variable ~S." foreign-name)))

(defmacro defcvar (name-and-options type &optional documentation)
  "Define a foreign global variable."
  (multiple-value-bind (lisp-name foreign-name options)
      (parse-name-and-options name-and-options t)
    (let ((fn (symbolicate '#:%var-accessor- lisp-name))
          (read-only (getf options :read-only))
          (library (getf options :library)))
      ;; We can't really setf an aggregate type.
      (when (aggregatep (parse-type type))
        (setq read-only t))
      `(progn
         (setf (documentation ',lisp-name 'variable) ,documentation)
         ;; Save foreign-name and library for posterior access by
         ;; GET-VAR-POINTER.
         (setf (get ',lisp-name 'foreign-var-name) ,foreign-name)
         (setf (get ',lisp-name 'foreign-var-library) ',library)
         ;; Getter
         (defun ,fn ()
           (mem-ref (fs-pointer-or-lose ,foreign-name ',library) ',type))
         ;; Setter
         (defun (setf ,fn) (value)
           ,(if read-only '(declare (ignore value)) (values))
           ,(if read-only
                `(error ,(format nil
                                 "Trying to modify read-only foreign var: ~A."
                                 lisp-name))
                `(setf (mem-ref (fs-pointer-or-lose ,foreign-name ',library)
                                ',type)
                       value)))
         ;; While most Lisps already expand DEFINE-SYMBOL-MACRO to an
         ;; EVAL-WHEN form like this, that is not required by the
         ;; standard so we do it ourselves.
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (define-symbol-macro ,lisp-name (,fn)))))))
