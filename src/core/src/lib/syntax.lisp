(defpackage :epsilon.syntax
  (:use
   :cl)
  (:export
   :define-constant
   :->
   :->>
   :if-let
   :when-let
   :when-let*
   :while))

(in-package :epsilon.syntax)

(defmacro define-constant (name value &optional doc)
  "Define a constant that can be redefined if the new value is equalp to the old."
  `(defconstant ,name 
     (if (boundp ',name)
         (let ((old (symbol-value ',name)))
           (if (equalp old ,value)
               old
               ,value))
         ,value)
     ,@(when doc (list doc))))

(defmacro while (cond &body body)
  `(do () ((not ,cond)) ,@body))

(defmacro -> (x &rest forms)
  "Thread-first macro.

   Takes a value and a series of forms, threading the value as the
   first argument through each form in succession."
  (if (null forms)
      x
      (let ((form (car forms)))
        (if (listp form)
            `(-> ,(append (list (car form) x) (cdr form))
                 ,@(cdr forms))
            `(-> (,form ,x)
                 ,@(cdr forms))))))

(defmacro ->> (x &rest forms)
  "Thread-last macro.

   Takes a value and a series of forms, threading the value as the
   last argument through each form in succession."
  (if (null forms)
      x
      (let ((form (car forms)))
        (if (listp form)
            `(->> ,(append form (list x))
                 ,@(cdr forms))
            `(->> (,form ,x)
                 ,@(cdr forms))))))

(defmacro if-let (bindings &body (then-form &optional else-form))
    "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defmacro when-let (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as an
implicit PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defmacro when-let* (bindings &body body)
  "Creates new variable bindings, and conditionally executes BODY.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each INITIAL-FORM is executed in turn, and the variable bound to the
corresponding value. INITIAL-FORM expressions can refer to variables
previously bound by the WHEN-LET*.

Execution of WHEN-LET* stops immediately if any INITIAL-FORM evaluates to NIL.
If all INITIAL-FORMs evaluate to true, then BODY is executed as an implicit
PROGN."
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings body)
               (if bindings
                   `(let (,(car bindings))
                      (when ,(caar bindings)
                        ,(bind (cdr bindings) body)))
                   `(progn ,@body))))
      (bind binding-list body))))
