(defpackage #:epsilon.symbol
  (:use
   #:cl)
  (:export
   #:make-gensym
   #:make-gensym-list
   #:with-gensyms
   #:parse-body
   ;; Package re-export utilities
   #:reexport
   #:reexport-types))

(in-package #:epsilon.symbol)

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

(defun maybe-intern (name package)
  (values
   (if package
       (intern name (if (eq t package) *package* package))
       (make-symbol name))))

(defun make-gensym (name)
  "If NAME is a non-negative integer, calls GENSYM using it. Otherwise NAME
must be a string designator, in which case calls GENSYM using the designated
string as the argument."
  (gensym (if (typep name '(integer 0))
              name
              (string name))))

(defun make-gensym-list (length &optional (x "G"))
  "Returns a list of LENGTH gensyms, each generated as if with a call to MAKE-GENSYM,
using the second (optional, defaulting to \"G\") argument."
  (let ((g (if (typep x '(integer 0)) x (string x))))
    (loop repeat length
          collect (gensym g))))

(defmacro with-gensyms (names &body forms)
  "Binds a set of variables to gensyms and evaluates the implicit progn FORMS.

Each element within NAMES is either a symbol SYMBOL or a pair (SYMBOL
STRING-DESIGNATOR). Bare symbols are equivalent to the pair (SYMBOL SYMBOL).

Each pair (SYMBOL STRING-DESIGNATOR) specifies that the variable named by SYMBOL
should be bound to a symbol constructed using GENSYM with the string designated
by STRING-DESIGNATOR being its first argument."
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name) (string (second name)))))
                     `(,symbol (gensym ,string))))
                 names)
     ,@forms))

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

;;;; Package re-export utilities

(defun special-variable-p (symbol)
  "Check if SYMBOL is a special (dynamic) variable.
   Uses the earmuff convention (*name*) to identify special variables."
  (and (boundp symbol)
       (let ((name (symbol-name symbol)))
         (and (> (length name) 2)
              (char= (char name 0) #\*)
              (char= (char name (1- (length name))) #\*)))))

(defun reexport (source-pkg symbols &optional (dest-pkg *package*))
  "Re-export SYMBOLS from SOURCE-PKG into DEST-PKG (default: current package).
   Functions get their definitions copied, while special variables
   are imported directly to preserve their special status."
  (let ((src (find-package source-pkg))
        (dst (if (packagep dest-pkg) dest-pkg (find-package dest-pkg))))
    (unless src
      (error "Source package not found: ~A" source-pkg))
    (unless dst
      (error "Destination package not found: ~A" dest-pkg))
    (dolist (sym-name symbols)
      (let ((src-sym (find-symbol (string sym-name) src)))
        (when src-sym
          (cond
            ;; Handle macros - import directly to preserve macro function
            ((macro-function src-sym)
             (shadowing-import src-sym dst)
             (export src-sym dst))
            ;; Handle special variables - import directly to preserve special status
            ((special-variable-p src-sym)
             (shadowing-import src-sym dst)
             (export src-sym dst))
            ;; Handle functions and their setf counterparts
            ((fboundp src-sym)
             (let ((dst-sym (intern (string sym-name) dst)))
               (setf (fdefinition dst-sym) (fdefinition src-sym))
               ;; Also copy setf function if it exists
               (when (fboundp `(setf ,src-sym))
                 (setf (fdefinition `(setf ,dst-sym))
                       (fdefinition `(setf ,src-sym))))
               (export dst-sym dst)))
            ;; Handle other bound symbols (constants, etc.)
            ((boundp src-sym)
             (shadowing-import src-sym dst)
             (export src-sym dst))
            ;; For symbols that look like special variables but aren't bound yet,
            ;; import them anyway
            (t
             (let ((name (symbol-name src-sym)))
               (when (and (> (length name) 2)
                          (char= (char name 0) #\*)
                          (char= (char name (1- (length name))) #\*))
                 (shadowing-import src-sym dst)
                 (export src-sym dst))))))))))

(defun reexport-types (source-pkg type-names &optional (dest-pkg *package*))
  "Re-export struct/condition types from SOURCE-PKG into DEST-PKG.
   TYPE-NAMES should be strings to avoid premature interning.
   Imports symbols directly so the type association is preserved."
  (let ((src (find-package source-pkg))
        (dst (if (packagep dest-pkg) dest-pkg (find-package dest-pkg))))
    (unless src
      (error "Source package not found: ~A" source-pkg))
    (unless dst
      (error "Destination package not found: ~A" dest-pkg))
    (dolist (type-name type-names)
      (let ((src-sym (find-symbol (string type-name) src)))
        (when src-sym
          (import src-sym dst)
          (export src-sym dst))))))
