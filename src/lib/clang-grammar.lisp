(defpackage :epsilon.lib.clang-grammar
  (:use :cl)
  (:local-nicknames 
   (:p :epsilon.lib.parser)
   (:c :epsilon.lib.clang-combinators)
   (:seq :epsilon.lib.sequence))
  (:export
   ;; Main parsers
   :translation-unit
   :external-declaration
   :declaration
   :function-definition
   
   ;; Declaration parsers
   :typedef-declaration
   :struct-declaration
   :union-declaration
   :enum-declaration
   :variable-declaration
   :function-declaration
   
   ;; Type parsers
   :type-specifier
   :declarator
   :abstract-declarator
   :parameter-declaration
   
   ;; AST constructors
   :make-c-declaration
   :make-c-typedef
   :make-c-struct
   :make-c-union
   :make-c-enum
   :make-c-function
   :make-c-variable))

(in-package :epsilon.lib.clang-grammar)

;; AST node constructors
(defun make-c-declaration (type &rest args)
  "Create a C declaration AST node."
  (list* :type type args))

(defun make-c-typedef (name underlying-type)
  "Create typedef AST node."
  (make-c-declaration :typedef :name name :underlying-type underlying-type))

(defun make-c-struct (name fields)
  "Create struct AST node."
  (make-c-declaration :struct :name name :fields fields))

(defun make-c-union (name fields)
  "Create union AST node."
  (make-c-declaration :union :name name :fields fields))

(defun make-c-enum (name values)
  "Create enum AST node."
  (make-c-declaration :enum :name name :values values))

(defun make-c-function (name return-type parameters)
  "Create function AST node."
  (make-c-declaration :function :name name :return-type return-type :parameters parameters))

(defun make-c-variable (name type)
  "Create variable AST node."
  (make-c-declaration :variable :name name :type type))

;; Forward declarations for recursive grammar
(defvar *type-specifier* nil)
(defvar *declarator* nil)
(defvar *declaration* nil)

;; Type specifiers
(defun basic-type-specifier ()
  "Parse basic type specifiers."
  (p:choice
   (c:type-specifier)
   (struct-or-union-specifier)
   (enum-specifier)
   (typedef-name)))

(defun struct-or-union-specifier ()
  "Parse struct or union specifier."
  (p:choice
   (struct-specifier)
   (union-specifier)))

(defun struct-specifier ()
  "Parse struct specifier."
  (p:bind (c:c-keyword "struct")
          (lambda (_)
            (p:bind (p:optional (c:c-identifier))
                    (lambda (name)
                      (p:choice
                       ;; struct name { fields }
                       (p:bind (c:braces (struct-declaration-list))
                               (lambda (fields)
                                 (p:return (make-c-struct name fields))))
                       ;; struct name (forward declaration or reference)
                       (p:return (make-c-struct name '()))))))))

(defun union-specifier ()
  "Parse union specifier."
  (p:bind (c:c-keyword "union")
          (lambda (_)
            (p:bind (p:optional (c:c-identifier))
                    (lambda (name)
                      (p:choice
                       ;; union name { fields }
                       (p:bind (c:braces (struct-declaration-list))
                               (lambda (fields)
                                 (p:return (make-c-union name fields))))
                       ;; union name
                       (p:return (make-c-union name '()))))))))

(defun enum-specifier ()
  "Parse enum specifier."
  (p:bind (c:c-keyword "enum")
          (lambda (_)
            (p:bind (p:optional (c:c-identifier))
                    (lambda (name)
                      (p:choice
                       ;; enum name { values }
                       (p:bind (c:braces (enumerator-list))
                               (lambda (values)
                                 (p:return (make-c-enum name values))))
                       ;; enum name
                       (p:return (make-c-enum name '()))))))))

(defun typedef-name ()
  "Parse typedef name (simplified - just identifier for now)."
  (c:c-identifier))

(defun struct-declaration-list ()
  "Parse list of struct declarations."
  (p:many (struct-declaration)))

(defun struct-declaration ()
  "Parse single struct field declaration."
  (p:bind (c:declaration-specifiers)
          (lambda (spec)
            (p:bind (p:sepBy1 (struct-declarator) (c:comma))
                    (lambda (declarators)
                      (p:bind (c:semicolon)
                              (lambda (_)
                                (p:return (list :field-declaration 
                                                :specifiers spec 
                                                :declarators declarators)))))))))

(defun struct-declarator ()
  "Parse struct declarator (simplified)."
  (p:choice
   (simple-declarator)
   ;; Bit fields: declarator : constant-expression
   (p:bind (simple-declarator)
           (lambda (decl)
             (p:bind (c:symbol ":")
                     (lambda (_)
                       (p:bind (constant-expression)
                               (lambda (width)
                                 (p:return (list :bit-field :declarator decl :width width))))))))))

(defun enumerator-list ()
  "Parse comma-separated list of enumerators."
  (p:sepBy1 (enumerator) (c:comma)))

(defun enumerator ()
  "Parse single enumerator."
  (p:bind (c:c-identifier)
          (lambda (name)
            (p:choice
             ;; name = value
             (p:bind (c:symbol "=")
                     (lambda (_)
                       (p:bind (constant-expression)
                               (lambda (value)
                                 (p:return (list name value))))))
             ;; just name
             (p:return (list name nil))))))

;; Declarators
(defun simple-declarator ()
  "Parse simple declarator (just identifier for now)."
  (c:c-identifier))

(defun parameter-type-list ()
  "Parse parameter type list."
  (p:sepBy1 (parameter-declaration) (c:comma)))

(defun parameter-declaration ()
  "Parse parameter declaration."
  (p:bind (c:declaration-specifiers)
          (lambda (spec)
            (p:bind (p:optional (simple-declarator))
                    (lambda (decl)
                      (p:return (list :parameter :specifiers spec :declarator decl)))))))

;; Expressions (simplified)
(defun constant-expression ()
  "Parse constant expression (simplified to just numbers and identifiers)."
  (p:choice
   (c:number-literal)
   (c:c-identifier)))

;; Top-level declarations
(defun type-specifiers-only ()
  "Parse type specifiers and qualifiers (no storage class)."
  (p:many1
   (p:choice
    (c:type-specifier)
    (c:type-qualifier)
    (c:c-identifier)))) ; typedef-name

(defun typedef-declaration ()
  "Parse typedef declaration."
  (p:bind (c:c-keyword "typedef")
          (lambda (_)
            (p:bind (type-specifiers-only)
                    (lambda (spec)
                      (p:bind (p:sepBy1 (simple-declarator) (c:comma))
                              (lambda (names)
                                (p:bind (c:semicolon)
                                        (lambda (_)
                                          (p:return
                                           (mapcar (lambda (name)
                                                     (make-c-typedef name spec))
                                                   names)))))))))))

(defun function-declaration ()
  "Parse function declaration."
  (p:bind (c:declaration-specifiers)
          (lambda (return-type)
            (p:bind (simple-declarator)
                    (lambda (name)
                      (p:bind (c:parameter-list (parameter-declaration))
                              (lambda (params)
                                (p:bind (c:semicolon)
                                        (lambda (_)
                                          (p:return (make-c-function name return-type params)))))))))))

(defun variable-declaration ()
  "Parse variable declaration."
  (p:bind (c:declaration-specifiers)
          (lambda (type)
            (p:bind (p:sepBy1 (simple-declarator) (c:comma))
                    (lambda (names)
                      (p:bind (c:semicolon)
                              (lambda (_)
                                (p:return
                                 (mapcar (lambda (name)
                                           (make-c-variable name type))
                                         names)))))))))

(defun external-declaration ()
  "Parse top-level declaration."
  (p:choice
   (typedef-declaration)
   (struct-specifier)
   (union-specifier) 
   (enum-specifier)
   (function-declaration)
   (variable-declaration)))

(defun translation-unit ()
  "Parse translation unit (sequence of external declarations)."
  (p:many (external-declaration)))

;; Initialize forward references
(setf *type-specifier* (function basic-type-specifier))
(setf *declarator* (function simple-declarator))
(setf *declaration* (function external-declaration))
