;;;; option.lisp - Optional value type
;;;;
;;;; Explicit representation of optional values eliminates null-related bugs
;;;; and enables composition. Option values are either Some(value) or None.

(defpackage :epsilon.option
  (:use :cl)
  (:local-nicknames
   (:seq :epsilon.sequence))
  (:shadow
   #:some
   #:map)
  (:export
   ;; Constructors
   #:some
   #:none
   #:option

   ;; Predicates
   #:some-p
   #:none-p
   #:option-p

   ;; Accessors
   #:unwrap
   #:unwrap-or
   #:unwrap-or-else
   #:expect

   ;; Transformations
   #:map
   #:flatmap
   #:filter
   #:flatten

   ;; Combinators
   #:and-then
   #:or-else
   #:zip
   #:zip-with

   ;; Conversion
   #:to-list
   #:to-seq
   #:from-nullable))

(in-package :epsilon.option)

;;; Core types

(defstruct (some-type (:constructor %make-some (value))
                      (:copier nil)
                      (:predicate some-type-p))
  "Represents a present value"
  (value nil :read-only t))

(defvar +none+ (gensym "NONE")
  "The singleton none value")

(defun some (value)
  "Wrap a value in Some.
   VALUE must not be nil - use (option value) for nullable values.

   Examples:
     (some 42)        => #<SOME 42>
     (some \"hello\") => #<SOME \"hello\">
     (some nil)       => ERROR"
  (when (null value)
    (error "Cannot wrap nil in Some - use (option value) for nullable values"))
  (%make-some value))

(defun none ()
  "Return the None value.

   Examples:
     (none)           => #<NONE>
     (none-p (none))  => T"
  +none+)

(defun option (value)
  "Convert a nullable value to an Option.
   nil becomes None, non-nil becomes Some.

   Examples:
     (option nil)  => #<NONE>
     (option 42)   => #<SOME 42>"
  (if (null value)
      +none+
      (%make-some value)))

;;; Predicates

(defun some-p (opt)
  "True if OPT is Some.

   Examples:
     (some-p (some 1))  => T
     (some-p (none))    => NIL"
  (some-type-p opt))

(defun none-p (opt)
  "True if OPT is None.

   Examples:
     (none-p (none))    => T
     (none-p (some 1))  => NIL"
  (eq opt +none+))

(defun option-p (obj)
  "True if OBJ is an Option (Some or None).

   Examples:
     (option-p (some 1))  => T
     (option-p (none))    => T
     (option-p 42)        => NIL"
  (or (some-p obj) (none-p obj)))

;;; Accessors

(defun unwrap (opt)
  "Extract value from Some, error on None.
   Use when you are certain the Option contains a value.

   Examples:
     (unwrap (some 42))  => 42
     (unwrap (none))     => ERROR"
  (if (some-p opt)
      (some-type-value opt)
      (error "Cannot unwrap None")))

(defun expect (opt message)
  "Extract value from Some, error with MESSAGE on None.
   Use when you want a descriptive error message.

   Examples:
     (expect (some 42) \"expected value\")  => 42
     (expect (none) \"user not found\")     => ERROR: user not found"
  (if (some-p opt)
      (some-type-value opt)
      (error "~A" message)))

(defun unwrap-or (opt default)
  "Extract value from Some, return DEFAULT for None.

   Examples:
     (unwrap-or (some 42) 0)  => 42
     (unwrap-or (none) 0)     => 0"
  (if (some-p opt)
      (some-type-value opt)
      default))

(defun unwrap-or-else (opt default-fn)
  "Extract value from Some, call DEFAULT-FN for None.
   DEFAULT-FN is only called if the Option is None.

   Examples:
     (unwrap-or-else (some 42) #'expensive-default)  => 42
     (unwrap-or-else (none) (lambda () (compute-default)))  => (compute-default)"
  (if (some-p opt)
      (some-type-value opt)
      (funcall default-fn)))

;;; Transformations

(defun map (f opt)
  "Apply F to value if Some, return None otherwise.
   F should be a function of one argument.

   Examples:
     (map #'1+ (some 1))  => #<SOME 2>
     (map #'1+ (none))    => #<NONE>"
  (if (some-p opt)
      (%make-some (funcall f (some-type-value opt)))
      +none+))

(defun flatmap (f opt)
  "Apply F (returning Option) to value if Some, flatten result.
   F must return an Option.

   Examples:
     (flatmap (lambda (x) (some (* x 2))) (some 1))  => #<SOME 2>
     (flatmap (lambda (x) (none)) (some 1))          => #<NONE>
     (flatmap #'identity (none))                      => #<NONE>"
  (if (some-p opt)
      (let ((result (funcall f (some-type-value opt))))
        (unless (option-p result)
          (error "flatmap function must return an Option, got: ~S" result))
        result)
      +none+))

(defun filter (pred opt)
  "Return Some if OPT is Some and predicate matches, None otherwise.

   Examples:
     (filter #'evenp (some 2))  => #<SOME 2>
     (filter #'evenp (some 3))  => #<NONE>
     (filter #'evenp (none))    => #<NONE>"
  (if (and (some-p opt)
           (funcall pred (some-type-value opt)))
      opt
      +none+))

(defun flatten (opt-opt)
  "Flatten nested Option (Option[Option[T]] -> Option[T]).

   Examples:
     (flatten (some (some 1)))  => #<SOME 1>
     (flatten (some (none)))    => #<NONE>
     (flatten (none))           => #<NONE>"
  (if (some-p opt-opt)
      (let ((inner (some-type-value opt-opt)))
        (if (option-p inner)
            inner
            (error "flatten requires Option[Option[T]], got Option[~S]" (type-of inner))))
      +none+))

;;; Combinators

(defun and-then (opt f)
  "Alias for flatmap - apply F if Some.
   Reads better in chains: 'if this succeeds, and then do that'.

   Examples:
     (and-then (some 1) (lambda (x) (some (* x 2))))  => #<SOME 2>"
  (flatmap f opt))

(defun or-else (opt default-opt)
  "Return OPT if Some, DEFAULT-OPT otherwise.

   Examples:
     (or-else (some 1) (some 0))  => #<SOME 1>
     (or-else (none) (some 0))    => #<SOME 0>"
  (if (some-p opt)
      opt
      default-opt))

(defun zip (opt1 opt2)
  "Combine two Options into Option of pair (list).
   Returns None if either is None.

   Examples:
     (zip (some 1) (some 2))  => #<SOME (1 2)>
     (zip (some 1) (none))    => #<NONE>
     (zip (none) (some 2))    => #<NONE>"
  (if (and (some-p opt1) (some-p opt2))
      (%make-some (list (some-type-value opt1)
                        (some-type-value opt2)))
      +none+))

(defun zip-with (f opt1 opt2)
  "Combine two Options with function F.
   Returns None if either is None.

   Examples:
     (zip-with #'+ (some 1) (some 2))  => #<SOME 3>
     (zip-with #'+ (some 1) (none))    => #<NONE>"
  (if (and (some-p opt1) (some-p opt2))
      (%make-some (funcall f
                           (some-type-value opt1)
                           (some-type-value opt2)))
      +none+))

;;; Conversion

(defun to-list (opt)
  "Convert to list: Some -> (value), None -> ().

   Examples:
     (to-list (some 42))  => (42)
     (to-list (none))     => NIL"
  (if (some-p opt)
      (list (some-type-value opt))
      nil))

(defun to-seq (opt)
  "Convert to lazy sequence.

   Examples:
     (to-seq (some 42))  => #<SEQ 42>
     (to-seq (none))     => #<SEQ>"
  (if (some-p opt)
      (seq:seq (list (some-type-value opt)))
      seq:*empty*))

(defun from-nullable (value)
  "Alias for option - convert nullable to Option.
   Provided for API clarity when converting from nullable types.

   Examples:
     (from-nullable nil)  => #<NONE>
     (from-nullable 42)   => #<SOME 42>"
  (option value))

;;; Print representation

(defmethod print-object ((obj some-type) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "#<SOME ~S>" (some-type-value obj))))

;; For None, we use the symbol printer, but provide a way to check
(defun print-none (stream)
  "Helper for printing None in debug contexts"
  (format stream "#<NONE>"))
