(defpackage #:lib.symbol
  (:use #:cl)
  (:export
   #:make-gensym-list
   #:symbolicate))
 
(in-package #:lib.symbol)

(defun ensure-symbol (name &optional (package *package*))
  "Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there.

Example: (ENSURE-SYMBOL :CONS :CL) => CL:CONS"
  (let ((name (string name)))
    (values (or (find-symbol name package)
                (intern name package)))))

(defun make-formatted-symbol (package name)
  (case package
    ((nil)
     (make-symbol name))
    ((t)
     (intern name))
    (t
     (intern name package))))

(declaim (inline format-symbol))
(defun format-symbol (package control &rest arguments)
  "Constructs a string by applying ARGUMENTS to CONTROL as if by FORMAT, and
then creates a symbol named by that string. If PACKAGE is NIL, returns an
uninterned symbol, if package is T, returns a symbol interned in the current
package, and otherwise returns a symbol interned in the package designated by
PACKAGE."
  (values (make-formatted-symbol package (apply #'format nil control arguments))))

(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (intern (string name) :keyword))

(defun make-gensym-list (length &optional x)
  "Returns a list of LENGTH gensyms, each generated with a call to
GENSYM using (if provided) as the argument."
  (loop repeat length
        collect (gensym x)))

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (flet ((stringify (x)
           (typecase x
             (integer (format nil "~D" x))
             (t (string x)))))
    (let* ((length (reduce #'+ things
                           :key (lambda (x) (length (stringify x)))))
           (name (make-array length :element-type 'character)))
      (let ((index 0))
        (dolist (thing things (values (intern name)))
          (let* ((x (stringify thing))
                 (len (length x)))
            (replace name x :start1 index)
            (incf index len)))))))
