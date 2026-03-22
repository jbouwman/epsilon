;;;; result.lisp - Success/failure result type
;;;;
;;;; Explicit error handling without exceptions. Errors are values that compose.
;;;; Result values are either Ok(value) or Err(error).

(defpackage :epsilon.result
  (:use :cl)
  (:local-nicknames
   (:opt :epsilon.option))
  (:shadow
   #:map)
  (:export
   ;; Constructors
   #:ok
   #:err
   #:result

   ;; Predicates
   #:ok-p
   #:err-p
   #:result-p

   ;; Accessors
   #:unwrap
   #:unwrap-err
   #:unwrap-or
   #:unwrap-or-else
   #:expect
   #:expect-err

   ;; Transformations
   #:map
   #:map-err
   #:flatmap
   #:flatten

   ;; Combinators
   #:and-then
   #:or-else
   #:transpose

   ;; Conversion
   #:ok-option
   #:err-option
   #:from-condition
   #:try-call))

(in-package :epsilon.result)

;;; Core types

(defstruct (ok-type (:constructor %make-ok (value))
                    (:copier nil)
                    (:predicate ok-type-p))
  "Represents success"
  (value nil :read-only t))

(defstruct (err-type (:constructor %make-err (error))
                     (:copier nil)
                     (:predicate err-type-p))
  "Represents failure"
  (error nil :read-only t))

(defun ok (value)
  "Create a success result.

   Examples:
     (ok 42)           => #<OK 42>
     (ok \"success\")  => #<OK \"success\">
     (ok nil)          => #<OK NIL>"
  (%make-ok value))

(defun err (error)
  "Create an error result.

   Examples:
     (err \"not found\")        => #<ERR \"not found\">
     (err (make-condition ...)) => #<ERR #<CONDITION>>"
  (%make-err error))

;;; Predicates

(defun ok-p (result)
  "True if RESULT is Ok.

   Examples:
     (ok-p (ok 1))   => T
     (ok-p (err e))  => NIL"
  (ok-type-p result))

(defun err-p (result)
  "True if RESULT is Err.

   Examples:
     (err-p (err e))  => T
     (err-p (ok 1))   => NIL"
  (err-type-p result))

(defun result-p (obj)
  "True if OBJ is a Result (Ok or Err).

   Examples:
     (result-p (ok 1))   => T
     (result-p (err e))  => T
     (result-p 42)       => NIL"
  (or (ok-p obj) (err-p obj)))

;;; Accessors

(defun unwrap (result)
  "Extract value from Ok, error on Err.
   Use when you are certain the Result is successful.

   Examples:
     (unwrap (ok 42))         => 42
     (unwrap (err \"fail\"))  => ERROR"
  (if (ok-p result)
      (ok-type-value result)
      (error "Cannot unwrap Err: ~S" (err-type-error result))))

(defun expect (result message)
  "Extract value from Ok, error with MESSAGE on Err.
   Use when you want a descriptive error message.

   Examples:
     (expect (ok 42) \"should work\")  => 42
     (expect (err e) \"parse failed\") => ERROR: parse failed"
  (if (ok-p result)
      (ok-type-value result)
      (error "~A: ~S" message (err-type-error result))))

(defun unwrap-err (result)
  "Extract error from Err, error on Ok.
   Useful in tests to verify error cases.

   Examples:
     (unwrap-err (err \"fail\"))  => \"fail\"
     (unwrap-err (ok 42))         => ERROR"
  (if (err-p result)
      (err-type-error result)
      (error "Cannot unwrap-err Ok: ~S" (ok-type-value result))))

(defun expect-err (result message)
  "Extract error from Err, error with MESSAGE on Ok.

   Examples:
     (expect-err (err e) \"should fail\")  => e
     (expect-err (ok 1) \"should fail\")   => ERROR"
  (if (err-p result)
      (err-type-error result)
      (error "~A: got Ok(~S)" message (ok-type-value result))))

(defun unwrap-or (result default)
  "Extract value from Ok, return DEFAULT for Err.

   Examples:
     (unwrap-or (ok 42) 0)         => 42
     (unwrap-or (err \"fail\") 0)  => 0"
  (if (ok-p result)
      (ok-type-value result)
      default))

(defun unwrap-or-else (result default-fn)
  "Extract value from Ok, call DEFAULT-FN with error for Err.
   DEFAULT-FN receives the error value.

   Examples:
     (unwrap-or-else (ok 42) #'handle-error)    => 42
     (unwrap-or-else (err e) (lambda (e) ...))  => (lambda e)"
  (if (ok-p result)
      (ok-type-value result)
      (funcall default-fn (err-type-error result))))

;;; Transformations

(defun map (f result)
  "Apply F to value if Ok, pass through Err.
   F should be a function of one argument.

   Examples:
     (map #'1+ (ok 1))   => #<OK 2>
     (map #'1+ (err e))  => #<ERR e>"
  (if (ok-p result)
      (%make-ok (funcall f (ok-type-value result)))
      result))

(defun map-err (f result)
  "Apply F to error if Err, pass through Ok.
   Useful for transforming error types.

   Examples:
     (map-err #'wrap-error (err e))  => #<ERR (wrap-error e)>
     (map-err #'wrap-error (ok 1))   => #<OK 1>"
  (if (err-p result)
      (%make-err (funcall f (err-type-error result)))
      result))

(defun flatmap (f result)
  "Apply F (returning Result) to value if Ok, flatten.
   F must return a Result.

   Examples:
     (flatmap (lambda (x) (ok (* x 2))) (ok 5))   => #<OK 10>
     (flatmap (lambda (x) (err \"fail\")) (ok 5)) => #<ERR \"fail\">
     (flatmap #'identity (err e))                  => #<ERR e>"
  (if (ok-p result)
      (let ((inner (funcall f (ok-type-value result))))
        (unless (result-p inner)
          (error "flatmap function must return a Result, got: ~S" inner))
        inner)
      result))

(defun flatten (result-result)
  "Flatten nested Result (Result[Result[T, E], E] -> Result[T, E]).

   Examples:
     (flatten (ok (ok 1)))    => #<OK 1>
     (flatten (ok (err e)))   => #<ERR e>
     (flatten (err e))        => #<ERR e>"
  (if (ok-p result-result)
      (let ((inner (ok-type-value result-result)))
        (if (result-p inner)
            inner
            (error "flatten requires Result[Result[T, E], E], got Result[~S]" (type-of inner))))
      result-result))

;;; Combinators

(defun and-then (result f)
  "Alias for flatmap - apply F if Ok.
   Reads naturally in chains: 'if this succeeds, and then do that'.

   Examples:
     (and-then (ok 5) (lambda (x) (ok (* x 2))))  => #<OK 10>"
  (flatmap f result))

(defun or-else (result f)
  "Apply F to error if Err, returning new Result.
   Use for error recovery. F receives the error and must return a Result.

   Examples:
     (or-else (ok 1) #'recover)              => #<OK 1>
     (or-else (err e) (lambda (e) (ok 0)))   => #<OK 0>
     (or-else (err e) (lambda (e) (err e2))) => #<ERR e2>"
  (if (err-p result)
      (let ((recovery (funcall f (err-type-error result))))
        (unless (result-p recovery)
          (error "or-else function must return a Result, got: ~S" recovery))
        recovery)
      result))

(defun transpose (result)
  "Convert Result[Option[T], E] to Option[Result[T, E]].

   Examples:
     (transpose (ok (some 1)))  => #<SOME #<OK 1>>
     (transpose (ok (none)))    => #<NONE>
     (transpose (err e))        => #<SOME #<ERR e>>"
  (cond
    ((err-p result)
     (opt:some result))
    ((opt:none-p (ok-type-value result))
     (opt:none))
    (t
     (opt:some (%make-ok (opt:unwrap (ok-type-value result)))))))

;;; Conversion

(defun ok-option (result)
  "Convert Ok to Some, Err to None.

   Examples:
     (ok-option (ok 42))  => #<SOME 42>
     (ok-option (err e))  => #<NONE>"
  (if (ok-p result)
      (opt:some (ok-type-value result))
      (opt:none)))

(defun err-option (result)
  "Convert Err to Some, Ok to None.

   Examples:
     (err-option (err e))  => #<SOME e>
     (err-option (ok 42))  => #<NONE>"
  (if (err-p result)
      (opt:some (err-type-error result))
      (opt:none)))

(defmacro from-condition (&body body)
  "Execute BODY, converting conditions to Err results.
   Any error signaled becomes Err(error).

   Examples:
     (from-condition (parse-integer \"42\"))  => #<OK 42>
     (from-condition (parse-integer \"bad\")) => #<ERR #<CONDITION>>"
  `(handler-case
       (%make-ok (progn ,@body))
     (error (e)
       (%make-err e))))

(defmacro try-call (function &rest args)
  "Call FUNCTION with ARGS, returning Result.
   Wraps the call in from-condition.

   Examples:
     (try-call #'parse-integer \"42\")   => #<OK 42>
     (try-call #'parse-integer \"bad\")  => #<ERR #<CONDITION>>"
  `(from-condition (funcall ,function ,@args)))

;;; Print representation

(defmethod print-object ((obj ok-type) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "#<OK ~S>" (ok-type-value obj))))

(defmethod print-object ((obj err-type) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "#<ERR ~S>" (err-type-error obj))))
