(defpackage #:epsilon.lib.symbol
  (:use
   #:cl)
  (:export
   #:ensure-symbol
   #:format-symbol
   #:make-gensym
   #:make-gensym-list
   #:make-keyword
   #:maybe-intern
   #:symbolicate
   #:with-gensyms
   #:with-rebinding
   #:with-unique-names
   #:discard-docstring
   #:once-only
   #:parse-body))
 
(in-package #:epsilon.lib.symbol)

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

(declaim (inline ensure-symbol))
(defun ensure-symbol (name &optional (package *package*))
  "Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of INTERN.

Example:

  (ensure-symbol :cons :cl) => cl:cons, :external
"
  (intern (string name) package))

(defun maybe-intern (name package)
  (values
   (if package
       (intern name (if (eq t package) *package* package))
       (make-symbol name))))

(declaim (inline format-symbol))
(defun format-symbol (package control &rest arguments)
  "Constructs a string by applying ARGUMENTS to string designator CONTROL as
if by FORMAT within WITH-STANDARD-IO-SYNTAX, and then creates a symbol named
by that string.

If PACKAGE is NIL, returns an uninterned symbol, if package is T, returns a
symbol interned in the current package, and otherwise returns a symbol
interned in the package designated by PACKAGE."
  (maybe-intern (with-standard-io-syntax
                  (apply #'format nil (string control) arguments))
                package))

(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (intern (string name) :keyword))

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

(defmacro once-only (names &body forms)
  "Evaluates FORMS with NAMES rebound to temporary variables,
ensuring that each is evaluated only once.

Example:
  (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
  (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
  (let ((gensyms (make-gensym-list (length names) "ONCE-ONLY")))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string n)))) 
                   gensyms names)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ;; bind in user-macro          
          ,(let ,(mapcar #'list names gensyms)
             ,@forms)))))

(defmacro discard-docstring (body-var)
  "Discards the first element of the list in body-var if it's a
string."
  `(when (stringp (car ,body-var))
     (pop ,body-var)))

(defmacro with-unique-names (names &body forms)
  "Binds each variable named by NAMES to a unique symbol."
  `(let ,(mapcar (lambda (name)
                   `(,name (gensym ,(symbol-name name))))
                 names)
     ,@forms))

(defmacro with-rebinding (bindings &body body)
  "WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let* ((length (reduce #'+ things
                         :key (lambda (x) (length (string x)))))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
               (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))

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
