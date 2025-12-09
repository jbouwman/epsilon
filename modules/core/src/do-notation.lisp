;;;; do-notation.lisp - Monadic do-notation
;;;;
;;;; Provides Haskell-style do-notation for monadic composition.
;;;; Works with Option, Result, List, and any type implementing monad-ops.

(defpackage :epsilon.do
  (:use :cl)
  (:local-nicknames
   (:opt :epsilon.option)
   (:result :epsilon.result)
   (:proto :epsilon.protocol.ext))
  (:export
   #:do-m
   #:do-option
   #:do-result
   #:do-list
   #:let-m
   #:m-bind
   #:m-return
   #:monad-ops))

(in-package :epsilon.do)

;;; Protocol for monadic types

(proto:defprotocol monad-ops
  "Operations required for do-notation"
  (m-bind (m f) "Bind: m a -> (a -> m b) -> m b")
  (m-return (value type-hint) "Return/pure: a -> m a"))

;;; Extend Option type

(proto:extend-type epsilon.option::some-type
  monad-ops
  (m-bind (m f) (funcall f (opt:unwrap m)))
  (m-return (v _) (declare (ignore _)) (opt:some v)))

;; None is a gensym symbol, not a struct, so use symbol specialization with runtime check
(defmethod m-bind ((m symbol) f)
  (declare (ignorable f))
  (if (opt:none-p m)
      (opt:none)
      (call-next-method)))

(defmethod m-return ((v t) (hint (eql :option)))
  (opt:some v))

;;; Extend Result type

(proto:extend-type epsilon.result::ok-type
  monad-ops
  (m-bind (m f) (funcall f (result:unwrap m)))
  (m-return (v _) (declare (ignore _)) (result:ok v)))

(proto:extend-type epsilon.result::err-type
  monad-ops
  (m-bind (m f)
    (declare (ignore f))
    m)
  (m-return (v _) (declare (ignore _)) (result:ok v)))

(defmethod m-return ((v t) (hint (eql :result)))
  (result:ok v))

;;; Extend List monad

(proto:extend-type cons
  monad-ops
  (m-bind (m f) (mapcan f m))
  (m-return (v _) (declare (ignore _)) (list v)))

(defmethod m-bind ((m null) f)
  (declare (ignore f))
  nil)

(defmethod m-return ((v t) (hint (eql :list)))
  (list v))

;;; Do-notation macro

(defmacro do-m (monad-type &body exprs)
  "Monadic do-notation.

   Syntax:
   (do-m type
     (var <- monadic-expr)  ; Bind result to var
     (let var expr)         ; Non-monadic let
     (guard test)           ; Guard clause (for types with mzero)
     monadic-expr           ; Execute for effect
     final-expr)            ; Final expression (must be monadic)

   Examples:
   (do-m :option
     (x <- (find-user id))
     (y <- (get-profile x))
     (m-return (user-name y) :option))

   (do-m :result
     (config <- (load-config))
     (let port (config-port config))
     (conn <- (connect port))
     (m-return conn :result))

   (do-m :list
     (x <- '(1 2 3))
     (y <- '(a b))
     (m-return (list x y) :list))  ; => ((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))"
  (expand-do-m monad-type exprs))

(defun arrow-symbol-p (sym)
  "Check if SYM is a <- binding operator (regardless of package)"
  (and (symbolp sym)
       (string= (symbol-name sym) "<-")))

(defun let-symbol-p (sym)
  "Check if SYM is a let keyword (regardless of package)"
  (and (symbolp sym)
       (string= (symbol-name sym) "LET")))

(defun guard-symbol-p (sym)
  "Check if SYM is a guard keyword (regardless of package)"
  (and (symbolp sym)
       (string= (symbol-name sym) "GUARD")))

(defun expand-do-m (type exprs)
  "Expand do-notation to nested binds"
  (cond
    ;; Single expression - return as-is
    ((null (cdr exprs))
     (first exprs))

    ;; Binding form: (var <- expr)
    ((and (listp (first exprs))
          (>= (length (first exprs)) 3)
          (arrow-symbol-p (second (first exprs))))
     (let ((var (first (first exprs)))
           (mexpr (third (first exprs))))
       `(m-bind ,mexpr
                (lambda (,var)
                  ,(expand-do-m type (rest exprs))))))

    ;; Let form: (let var expr)
    ((and (listp (first exprs))
          (>= (length (first exprs)) 3)
          (let-symbol-p (first (first exprs))))
     (let ((var (second (first exprs)))
           (expr (third (first exprs))))
       `(cl:let ((,var ,expr))
          ,(expand-do-m type (rest exprs)))))

    ;; Guard form: (guard test)
    ((and (listp (first exprs))
          (= (length (first exprs)) 2)
          (guard-symbol-p (first (first exprs))))
     (let ((test (second (first exprs))))
       `(if ,test
            ,(expand-do-m type (rest exprs))
            ,(get-mzero type))))

    ;; Regular expression - sequence with bind ignoring result
    (t
     (let ((ignored (gensym "IGNORED")))
       `(m-bind ,(first exprs)
                (lambda (,ignored)
                  (declare (ignore ,ignored))
                  ,(expand-do-m type (rest exprs))))))))

(defun get-mzero (type)
  "Get the zero/empty value for a monad type"
  (case type
    (:option '(epsilon.option:none))
    (:list 'nil)
    (t `(error "guard not supported for ~A" ',type))))

;;; Convenience macros for common monads

(defmacro do-option (&body exprs)
  "Do-notation for Option monad"
  `(do-m :option ,@exprs))

(defmacro do-result (&body exprs)
  "Do-notation for Result monad"
  `(do-m :result ,@exprs))

(defmacro do-list (&body exprs)
  "Do-notation for List monad (list comprehension)"
  `(do-m :list ,@exprs))

;;; Additional utilities

(defmacro let-m (bindings &body body)
  "Monadic let - sequential binding.

   (let-m ((x (get-x))
           (y (get-y x)))
     (m-return (+ x y) :result))

   is equivalent to

   (do-m :result
     (x <- (get-x))
     (y <- (get-y x))
     (m-return (+ x y) :result))"
  (if (null bindings)
      `(progn ,@body)
      (let ((var (caar bindings))
            (expr (cadar bindings)))
        `(m-bind ,expr
                 (lambda (,var)
                   (let-m ,(rest bindings) ,@body))))))
