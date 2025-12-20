;;;; Protocol system for defining extensible interfaces
;;;;
;;;; This module provides a protocol system based on generic functions
;;;; with support for extending protocols to types after definition,
;;;; similar to Clojure protocols.

(defpackage :epsilon.protocol
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.map))
  (:export
   ;; Protocol definition
   #:defprotocol
   #:define-protocol          ; Alias for compatibility

   ;; Protocol extension
   #:extend-type
   #:extend-protocol

   ;; Protocol class and accessors
   #:protocol
   #:protocol-name
   #:protocol-version
   #:protocol-documentation
   #:protocol-methods
   #:protocol-extends

   ;; Protocol registry
   #:find-protocol
   #:list-protocols
   #:protocol-exists-p

   ;; Type checking
   #:satisfies-p
   #:list-implementations

   ;; Utilities
   #:ensure-protocol-method
   #:protocol-method-p
   #:list-protocol-implementations))

(in-package :epsilon.protocol)

;;; Protocol class

(defclass protocol ()
  ((name :initarg :name
         :reader protocol-name
         :type symbol
         :documentation "The name of the protocol")
   (version :initarg :version
            :reader protocol-version
            :initform "1.0"
            :type string
            :documentation "Protocol version string")
   (documentation :initarg :documentation
                  :reader protocol-documentation
                  :initform nil
                  :type (or null string)
                  :documentation "Protocol documentation")
   (extends :initarg :extends
            :reader protocol-extends
            :initform nil
            :type list
            :documentation "List of protocols this one extends")
   (methods :initarg :methods
            :reader protocol-methods
            :initform '()
            :type list
            :documentation "List of method specs: ((name args doc) ...)"))
  (:documentation "A protocol defines a set of generic functions forming an interface"))

;;; Registries

(defvar *protocols* map:+empty+
  "Global registry of defined protocols")

(defvar *type-implementations* map:+empty+
  "Registry of type -> protocol -> t mappings")

;;; Protocol registry operations

(defun find-protocol (name)
  "Find a protocol by name"
  (map:get *protocols* name))

(defun protocol-exists-p (name)
  "Check if a protocol exists"
  (map:contains-p *protocols* name))

(defun list-protocols ()
  "Return a list of all defined protocol names"
  (map:keys *protocols*))

(defun register-protocol (protocol)
  "Register a protocol in the global registry"
  (setf *protocols* (map:assoc *protocols*
                               (protocol-name protocol)
                               protocol)))

;;; Protocol definition macros

(defmacro defprotocol (name &body specs)
  "Define a protocol with methods that can be implemented by any type.

   Syntax:
   (defprotocol name
     \"Documentation string\"
     (:version \"1.0\")
     (:extends other-protocol ...)
     (method-name (args...) \"doc\")
     ...)

   Example:
   (defprotocol functor
     \"Types that support mapping\"
     (fmap (f container) \"Apply f to contents\"))

   (defprotocol monad
     \"Types that support sequencing\"
     (:extends functor)
     (unit (value type-hint) \"Wrap value\")
     (flatmap (f container) \"Map and flatten\"))"
  (let* ((doc (when (stringp (first specs))
                (first specs)))
         (rest-specs (if doc (rest specs) specs))
         ;; Parse :version option
         (version-spec (find-if (lambda (s)
                                  (and (listp s) (eq (car s) :version)))
                                rest-specs))
         (version (if version-spec (second version-spec) "1.0"))
         ;; Parse :extends option
         (extends-spec (find-if (lambda (s)
                                  (and (listp s) (eq (car s) :extends)))
                                rest-specs))
         (extends (when extends-spec (cdr extends-spec)))
         ;; Filter to get method specs only
         (method-specs (remove-if (lambda (s)
                                    (or (not (listp s))
                                        (member (car s) '(:version :extends :documentation))))
                                  rest-specs)))

    `(progn
       ;; Register protocol
       (register-protocol
        (make-instance 'protocol
                       :name ',name
                       :version ,version
                       :documentation ,doc
                       :extends ',extends
                       :methods ',method-specs))

       ;; Define generic functions for each method
       ,@(loop for spec in method-specs
               for method-name = (first spec)
               for args = (second spec)
               for method-doc = (when (stringp (third spec)) (third spec))
               collect `(unless (fboundp ',method-name)
                          (defgeneric ,method-name ,args
                            ,@(when method-doc `((:documentation ,method-doc))))))

       ',name)))

(defmacro define-protocol (name &body options)
  "Define a new protocol. This is an alias for defprotocol with different syntax.

   Syntax:
   (define-protocol my-protocol
     (:version \"1.0\")
     (:documentation \"My protocol description\")
     (:method foo (x y) \"Do foo operation\")
     (:method bar (x) \"Do bar operation\"))

   Consider using defprotocol instead for cleaner syntax."
  (let ((version "1.0")
        (documentation nil)
        (methods '()))
    ;; Parse options
    (dolist (option options)
      (case (first option)
        (:version (setf version (second option)))
        (:documentation (setf documentation (second option)))
        (:method (push (rest option) methods))))

    `(progn
       ;; Create and register the protocol
       (register-protocol
        (make-instance 'protocol
                       :name ',name
                       :version ,version
                       :documentation ,documentation
                       :methods ',(mapcar (lambda (m)
                                            (list (first m) (second m)
                                                  (when (stringp (third m)) (third m))))
                                          (reverse methods))))

       ;; Define generic functions for each method
       ,@(loop for (method-name lambda-list . method-options) in (reverse methods)
               collect `(defgeneric ,method-name ,lambda-list
                          ,@(when (stringp (first method-options))
                              `((:documentation ,(first method-options))))
                          ,@(when (and (stringp (first method-options))
                                       (rest method-options))
                              (rest method-options))
                          ,@(unless (stringp (first method-options))
                              method-options)))

       ',name)))

;;; Type extension macros

(defmacro extend-type (type &body protocol-impls)
  "Extend protocols to a type.

   Syntax:
   (extend-type type
     protocol-name
     (method-name (args...) body...)
     ...
     another-protocol
     ...)

   Example:
   (extend-type list
     functor
     (fmap (f xs) (mapcar f xs))

     monad
     (unit (x _) (list x))
     (flatmap (f xs) (mapcan f xs)))"
  (let ((implementations '())
        (current-protocol nil))
    ;; Parse protocol/method pairs
    (dolist (item protocol-impls)
      (cond
        ((symbolp item)
         (setf current-protocol item))
        ((listp item)
         (unless current-protocol
           (error "Method definition without protocol: ~S" item))
         (push (list current-protocol item) implementations))))

    ;; Generate method definitions
    `(progn
       ,@(loop for (protocol method-def) in (reverse implementations)
               for method-name = (first method-def)
               for args = (second method-def)
               for body = (cddr method-def)
               for typed-args = (substitute-type-in-args args type)
               collect `(defmethod ,method-name ,typed-args
                          ,@body))

       ;; Register implementation
       ,@(loop for protocol in (remove-duplicates
                                (mapcar #'first implementations))
               collect `(register-implementation ',type ',protocol))

       ',type)))

(defun substitute-type-in-args (args type)
  "Replace the first argument with a typed specializer"
  (cons (list (first args) type)
        (rest args)))

(defun register-implementation (type protocol)
  "Register that TYPE implements PROTOCOL"
  (let ((type-protocols (or (map:get *type-implementations* type)
                            map:+empty+)))
    (setf *type-implementations*
          (map:assoc *type-implementations* type
                     (map:assoc type-protocols protocol t)))))

(defmacro extend-protocol (protocol &body type-impls)
  "Extend a protocol to multiple types.

   Syntax:
   (extend-protocol protocol
     type1
     (method (args...) body...)
     ...
     type2
     ...)

   Example:
   (extend-protocol functor
     list
     (fmap (f xs) (mapcar f xs))

     vector
     (fmap (f v) (map 'vector f v)))"
  (let ((current-type nil)
        (type-methods (make-hash-table)))
    ;; Parse type/method pairs
    (dolist (item type-impls)
      (cond
        ((and (symbolp item) (not (keywordp item)))
         (setf current-type item))
        ((listp item)
         (unless current-type
           (error "Method without type: ~S" item))
         (push item (gethash current-type type-methods)))))

    ;; Generate extend-type calls
    `(progn
       ,@(loop for type being the hash-keys of type-methods
               using (hash-value methods)
               collect `(extend-type ,type
                          ,protocol
                          ,@(reverse methods))))))

;;; Type checking

(defun satisfies-p (obj protocol)
  "Check if OBJ's type implements PROTOCOL"
  (block found
    (map:each (lambda (type protocols)
                (when (and (typep obj type)
                           (map:contains-p protocols protocol))
                  (return-from found t)))
              *type-implementations*)
    nil))

(defun list-implementations (protocol)
  "List all types that implement a protocol"
  (let ((types '()))
    (map:each (lambda (type protocols)
                (when (map:contains-p protocols protocol)
                  (push type types)))
              *type-implementations*)
    types))

;;; Utility functions

(defun ensure-protocol-method (protocol-name method-name)
  "Ensure a method belongs to a protocol"
  (let ((protocol (find-protocol protocol-name)))
    (unless protocol
      (error "Protocol ~S not found" protocol-name))
    (unless (member method-name (mapcar #'first (protocol-methods protocol)))
      (error "Method ~S is not part of protocol ~S"
             method-name protocol-name))
    t))

(defun protocol-method-p (method-name)
  "Check if a generic function is part of any protocol"
  (loop for protocol-name in (list-protocols)
        for protocol = (find-protocol protocol-name)
        thereis (member method-name (mapcar #'first (protocol-methods protocol)))))

(defun list-protocol-implementations (protocol-name)
  "List all classes that implement methods for this protocol"
  (let ((protocol (find-protocol protocol-name))
        (implementations map:+empty+))
    (when protocol
      (dolist (method-spec (protocol-methods protocol))
        (let ((method-name (first method-spec)))
          (when (fboundp method-name)
            (let ((gf (symbol-function method-name)))
              (when (typep gf 'generic-function)
                (dolist (method (sb-mop:generic-function-methods gf))
                  (let* ((specializers (sb-mop:method-specializers method))
                         (class-spec (first specializers)))
                    (when (typep class-spec 'class)
                      (let ((class-name (class-name class-spec)))
                        (setf implementations
                              (map:assoc implementations
                                         class-name
                                         (cons method-name
                                               (map:get implementations class-name)))))))))))))
      implementations)))
