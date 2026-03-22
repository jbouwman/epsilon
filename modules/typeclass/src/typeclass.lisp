;;;; Type class system for Epsilon
;;;;
;;;; Provides Haskell-inspired type classes as a macro layer over CLOS.
;;;; This is the sole ad-hoc polymorphism system in Epsilon.
;;;;
;;;; Features:
;;;;   - Compile-time completeness checking (all required methods implemented)
;;;;   - Superclass enforcement (superclass instances required)
;;;;   - Default method implementations
;;;;   - Derives support for automatic instance generation
;;;;   - extend-type / extend-typeclass sugar for grouping convenience
;;;;
;;;; Design principles:
;;;;   - Compile-time registry, runtime-invisible. Expanded code is plain
;;;;     defgeneric/defmethod.
;;;;   - EQL specialization for format dispatch following repr's pattern.

(defpackage :epsilon.typeclass
  (:use :cl)
  (:local-nicknames
   (:mmap :epsilon.mutable-map))
  (:export
   ;; Type class definition
   #:deftypeclass
   ;; Instance implementation
   #:definstance
   ;; Sugar macros for grouping convenience
   #:extend-type
   #:extend-typeclass
   ;; Introspection (compile-time only, for macro expansion)
   #:find-typeclass
   #:find-instance
   #:typeclass-methods
   #:typeclass-superclasses
   #:typeclass-instances
   ;; Validation (used internally by definstance, exported for testing)
   #:check-instance-completeness))

(in-package :epsilon.typeclass)

;;; ============================================================
;;; Compile-time Registry
;;; ============================================================
;;;
;;; These structures exist only during macroexpansion and at load time
;;; for validation. They do not affect runtime dispatch.

(defstruct tc-method
  "A method required by a type class."
  name            ; Symbol: method name
  lambda-list     ; List: method arguments (first arg is the dispatched one)
  documentation   ; String or NIL
  default-body)   ; List or NIL: default implementation body

(defstruct typeclass-info
  "Compile-time metadata for a type class."
  name            ; Symbol: type class name
  superclasses    ; List of symbols: required superclass type classes
  methods         ; List of tc-method
  documentation   ; String or NIL
  instances)      ; List of (type . instance-info) -- accumulated at load time

(defstruct instance-info
  "Compile-time record of a definstance form."
  typeclass       ; Symbol: type class name
  type            ; Type specifier the instance is for
  methods)        ; List of method names implemented

;;; Global registry of type classes (mutable, compile-time only)
(defvar *typeclass-registry* (mmap:make-map :test 'equal)
  "Mutable map from type class name (string) to typeclass-info.")

(defun register-typeclass (info)
  "Register a type class in the compile-time registry."
  (mmap:put! *typeclass-registry* (symbol-name (typeclass-info-name info)) info))

(defun find-typeclass (name)
  "Find a type class by name. Returns typeclass-info or NIL."
  (mmap:get *typeclass-registry* (symbol-name name)))

(defun typeclass-methods (name)
  "Return the list of method names for type class NAME."
  (let ((tc (find-typeclass name)))
    (when tc
      (mapcar #'tc-method-name (typeclass-info-methods tc)))))

(defun typeclass-superclasses (name)
  "Return the list of superclass names for type class NAME."
  (let ((tc (find-typeclass name)))
    (when tc
      (typeclass-info-superclasses tc))))

(defun typeclass-instances (name)
  "Return the list of types that have instances for type class NAME."
  (let ((tc (find-typeclass name)))
    (when tc
      (mapcar #'car (typeclass-info-instances tc)))))

(defun register-instance (typeclass-name type method-names)
  "Record that TYPE has an instance of TYPECLASS-NAME."
  (let ((tc (find-typeclass typeclass-name)))
    (when tc
      (push (cons type (make-instance-info
                        :typeclass typeclass-name
                        :type type
                        :methods method-names))
            (typeclass-info-instances tc)))))

(defun find-instance (typeclass-name type)
  "Check if TYPE has a registered instance of TYPECLASS-NAME."
  (let ((tc (find-typeclass typeclass-name)))
    (when tc
      (cdr (assoc type (typeclass-info-instances tc) :test #'equal)))))

;;; ============================================================
;;; Method Parsing
;;; ============================================================

(defun parse-typeclass-body (body)
  "Parse the body of a deftypeclass form.
   Returns (values documentation superclasses methods).

   Body format:
     [\"docstring\"]
     (method-name (args...) [\"doc\"] [:default body...])
     ..."
  (let ((doc nil)
        (methods nil)
        (rest body))
    ;; Optional documentation string
    (when (stringp (first rest))
      (setf doc (first rest)
            rest (cdr rest)))
    ;; Parse method specs
    (dolist (spec rest)
      (unless (listp spec)
        (error "Invalid type class method spec: ~S (expected list)" spec))
      (let* ((method-name (first spec))
             (lambda-list (second spec))
             (rest-spec (cddr spec))
             (method-doc nil)
             (default-body nil))
        (unless (symbolp method-name)
          (error "Invalid method name in type class: ~S" method-name))
        (unless (listp lambda-list)
          (error "Invalid lambda list for method ~A: ~S" method-name lambda-list))
        ;; Optional doc string
        (when (stringp (first rest-spec))
          (setf method-doc (first rest-spec)
                rest-spec (cdr rest-spec)))
        ;; Optional :default keyword followed by body
        (when (eq (first rest-spec) :default)
          (setf default-body (cdr rest-spec)))
        (push (make-tc-method
               :name method-name
               :lambda-list lambda-list
               :documentation method-doc
               :default-body default-body)
              methods)))
    (values doc (nreverse methods))))

;;; ============================================================
;;; deftypeclass macro
;;; ============================================================

(defmacro deftypeclass (name superclasses &body body)
  "Define a type class with required methods and optional defaults.

   Syntax:
   (deftypeclass name (superclass...)
     \"Optional documentation\"
     (method-name (args...) \"Optional doc\" [:default body...])
     ...)

   Example:
   (deftypeclass show ()
     \"Types with a string representation.\"
     (show-value (object) \"Return a string representation.\"))

   (deftypeclass ord (eq-class)
     \"Totally ordered types.\"
     (compare (a b) \"Return :lt, :eq, or :gt.\"))

   Methods with :default provide a fallback implementation that instances
   can override. Methods without :default must be implemented by every instance.

   At macroexpansion time, the type class is registered in *typeclass-registry*
   for use by definstance. At runtime, only defgeneric forms remain."
  (unless (symbolp name)
    (error "Type class name must be a symbol: ~S" name))
  (unless (listp superclasses)
    (error "Superclasses must be a list: ~S" superclasses))
  (multiple-value-bind (doc methods) (parse-typeclass-body body)
    ;; Validate superclasses exist (when possible)
    (dolist (sc superclasses)
      (unless (symbolp sc)
        (error "Superclass must be a symbol: ~S" sc)))
    `(progn
       ;; Mark symbol as a typeclass so auto-export picks it up
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name :typeclass-p) t))
       ;; Register at load time for subsequent definstance validation
       (register-typeclass
        (make-typeclass-info
         :name ',name
         :superclasses ',superclasses
         :documentation ,doc
         :instances nil
         :methods (list ,@(mapcar (lambda (m)
                                    `(make-tc-method
                                      :name ',(tc-method-name m)
                                      :lambda-list ',(tc-method-lambda-list m)
                                      :documentation ,(tc-method-documentation m)
                                      :default-body ',(tc-method-default-body m)))
                                  methods))))
       ;; Define generic functions for each method
       ;; Use eval-when so defgeneric is visible at compile time.
       ;; This prevents undefined-function warnings when default methods
       ;; call other methods defined in the same typeclass.
       ,@(mapcar (lambda (m)
                   (let ((mname (tc-method-name m))
                         (args (tc-method-lambda-list m))
                         (mdoc (tc-method-documentation m))
                         (default (tc-method-default-body m)))
                     (if default
                         ;; Generic with a default method
                         `(progn
                            (eval-when (:compile-toplevel :load-toplevel :execute)
                              (unless (fboundp ',mname)
                                (defgeneric ,mname ,args
                                  ,@(when mdoc `((:documentation ,mdoc))))))
                            ;; Install default as a method on T
                            (defmethod ,mname ,args ,@default))
                         ;; Generic without default -- no base method
                         `(eval-when (:compile-toplevel :load-toplevel :execute)
                            (unless (fboundp ',mname)
                              (defgeneric ,mname ,args
                                ,@(when mdoc `((:documentation ,mdoc)))))))))
                 methods)
       ',name)))

;;; ============================================================
;;; definstance macro
;;; ============================================================

(defun check-instance-completeness (typeclass-name type-spec method-names)
  "Check that all required methods of TYPECLASS-NAME are provided.
   Signals an error if any required method (without default) is missing."
  (let ((tc (find-typeclass typeclass-name)))
    (unless tc
      (error "Unknown type class: ~A" typeclass-name))
    ;; Check superclass instances exist
    (dolist (sc (typeclass-info-superclasses tc))
      (unless (find-instance sc type-spec)
        (error "Type class ~A requires superclass ~A instance for ~S, ~
                but none is registered. Define (definstance ~A ~S ...) first."
               typeclass-name sc type-spec sc type-spec)))
    ;; Check method completeness
    (let ((required (remove-if #'tc-method-default-body (typeclass-info-methods tc))))
      (dolist (m required)
        (unless (member (tc-method-name m) method-names)
          (error "Incomplete instance: ~A for ~S is missing required method ~A"
                 typeclass-name type-spec (tc-method-name m)))))))

(defun specializer-for-type (param type-spec)
  "Create a specialized parameter for PARAM with TYPE-SPEC.
   Supports EQL specializers: (eql :keyword) becomes ((param (eql :keyword)))."
  (cond
    ((and (listp type-spec) (eq (car type-spec) 'eql))
     `(,param ,type-spec))
    (t
     `(,param ,type-spec))))

(defmacro definstance (typeclass-name type-spec &body methods)
  "Define an instance of a type class for a specific type.

   Syntax:
   (definstance typeclass-name type-spec
     (method-name (args...) body...)
     ...)

   Examples:
   (definstance show string
     (show-value (s) s))

   (definstance show list
     (show-value (xs) (format nil \"(~{~A~^ ~})\" (mapcar #'show-value xs))))

   ;; EQL specializer for format dispatch:
   (definstance serialize (eql :json)
     (encode (fmt obj) (json:encode obj))
     (decode (fmt data) (json:decode data)))

   Compile-time checks:
   1. Type class must be defined (via deftypeclass)
   2. All required methods (those without :default) must be implemented
   3. All declared superclass instances must already exist"
  (let ((method-names (mapcar #'first methods)))
    `(progn
       ;; Compile-time validation
       (check-instance-completeness ',typeclass-name ',type-spec ',method-names)
       ;; Register instance for subsequent superclass checks
       (register-instance ',typeclass-name ',type-spec ',method-names)
       ;; Generate defmethod forms
       ,@(mapcar (lambda (method-def)
                   (let* ((method-name (first method-def))
                          (lambda-list (second method-def))
                          (body (cddr method-def))
                          ;; Specialize the first parameter on the type
                          (typed-lambda-list
                            (cons (specializer-for-type (first lambda-list) type-spec)
                                  (rest lambda-list))))
                     `(defmethod ,method-name ,typed-lambda-list
                        ,@body)))
                 methods)
       ;; Return instance description
       '(,typeclass-name ,type-spec))))

;;; ============================================================
;;; Sugar macros for grouping convenience
;;; ============================================================

(defmacro extend-type (type &body typeclass-impls)
  "Implement multiple type classes for a single type.

   Syntax:
   (extend-type list
     monad-ops
     (m-bind (m f) (mapcan f m))
     (m-return (v _) (declare (ignore _)) (list v))

     show
     (show-value (xs) (format nil \"(~{~A~^ ~})\" xs)))

   Expands to multiple definstance forms."
  (let ((instances '())
        (current-tc nil)
        (current-methods nil))
    (dolist (item typeclass-impls)
      (cond
        ((symbolp item)
         (when current-tc
           (push (cons current-tc (nreverse current-methods)) instances))
         (setf current-tc item
               current-methods nil))
        ((listp item)
         (push item current-methods))))
    (when current-tc
      (push (cons current-tc (nreverse current-methods)) instances))
    `(progn
       ,@(loop for (tc . methods) in (nreverse instances)
               collect `(definstance ,tc ,type ,@methods)))))

(defmacro extend-typeclass (typeclass-name &body type-impls)
  "Implement one type class for multiple types.

   Syntax:
   (extend-typeclass show
     list
     (show-value (xs) (format nil \"(~{~A~^ ~})\" xs))

     vector
     (show-value (v) (format nil \"#(~{~A~^ ~})\" (coerce v 'list))))

   Expands to multiple definstance forms."
  (let ((instances '())
        (current-type nil)
        (current-methods nil))
    (dolist (item type-impls)
      (cond
        ((and (symbolp item) (not (keywordp item)))
         (when current-type
           (push (cons current-type (nreverse current-methods)) instances))
         (setf current-type item
               current-methods nil))
        ((listp item)
         (push item current-methods))))
    (when current-type
      (push (cons current-type (nreverse current-methods)) instances))
    `(progn
       ,@(loop for (type . methods) in (nreverse instances)
               collect `(definstance ,typeclass-name ,type ,@methods)))))
