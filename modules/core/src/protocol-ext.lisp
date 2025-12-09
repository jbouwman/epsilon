;;;; protocol-ext.lisp - Enhanced protocol system with extend-type
;;;;
;;;; Extends the base protocol system with support for extending
;;;; protocols to types after definition, similar to Clojure protocols.

(defpackage :epsilon.protocol.ext
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.map)
   (:proto :epsilon.protocol))
  (:export
   #:defprotocol
   #:extend-type
   #:extend-protocol
   #:satisfies-p
   #:protocol-methods
   #:list-implementations))

(in-package :epsilon.protocol.ext)

;;; Enhanced protocol definition

(defvar *protocol-registry* map:+empty+
  "Registry of protocols and their methods")

(defvar *type-implementations* map:+empty+
  "Registry of type -> protocol -> methods mappings")

(defclass enhanced-protocol ()
  ((name :initarg :name :reader protocol-name)
   (extends :initarg :extends :reader protocol-extends :initform nil)
   (methods :initarg :methods :reader protocol-method-specs)
   (documentation :initarg :documentation :reader protocol-documentation))
  (:documentation "Enhanced protocol with extension support"))

(defmacro defprotocol (name &body specs)
  "Define a protocol with methods that can be implemented by any type.

   Syntax:
   (defprotocol name
     \"Documentation string\"
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
         (extends (when (and (listp (first rest-specs))
                             (eq (car (first rest-specs)) :extends))
                    (cdr (first rest-specs))))
         (method-specs (if extends (rest rest-specs) rest-specs)))

    `(progn
       ;; Register protocol
       (setf *protocol-registry*
             (map:assoc *protocol-registry* ',name
                        (make-instance 'enhanced-protocol
                                       :name ',name
                                       :extends ',extends
                                       :documentation ,doc
                                       :methods ',method-specs)))

       ;; Define generic functions for each method
       ,@(loop for spec in method-specs
               for method-name = (first spec)
               for args = (second spec)
               for method-doc = (when (stringp (third spec)) (third spec))
               collect `(unless (fboundp ',method-name)
                          (defgeneric ,method-name ,args
                            ,@(when method-doc `((:documentation ,method-doc))))))

       ',name)))

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

(defun satisfies-p (obj protocol)
  "Check if OBJ's type implements PROTOCOL"
  ;; Check all registered types to see if obj is an instance of any that implement protocol
  (block found
    (map:each (lambda (type protocols)
                (when (and (typep obj type)
                           (map:contains-p protocols protocol))
                  (return-from found t)))
              *type-implementations*)
    nil))

(defun protocol-methods (protocol)
  "Get the method specs for a protocol"
  (let ((p (map:get *protocol-registry* protocol)))
    (when p
      (protocol-method-specs p))))

(defun list-implementations (protocol)
  "List all types that implement a protocol"
  (let ((types '()))
    (map:each (lambda (type protocols)
                (when (map:contains-p protocols protocol)
                  (push type types)))
              *type-implementations*)
    types))
