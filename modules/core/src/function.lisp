(defpackage #:epsilon.function
  (:use
   #:cl
   #:epsilon.symbol)
  (:export
   #:compose
   #:multiple-value-compose
   #:partial
   #:named-lambda
   ;; Enhanced composition operators
   #:pipe
   #:juxt
   #:complement*
   #:constantly*
   #:flip
   #:curry
   #:rcurry))

(in-package #:epsilon.function)

;;; To propagate return type and allow the compiler to eliminate the IF when
;;; it is known if the argument is function or not.

(declaim (inline ensure-function))

(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(defun compose (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(funcall f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "COMPOSE")))
      `(let ,(loop for f in funs for arg in args
		   collect `(,f (ensure-function ,arg)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun multiple-value-compose (function &rest more-functions)
    "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies
its arguments to each in turn, starting from the rightmost of
MORE-FUNCTIONS, and then calling the next one with all the return values of
the last."
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(multiple-value-call f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro multiple-value-compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(multiple-value-call ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "MV-COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(declaim (inline partial))

(defun partial (function &rest arguments)
  "Returns a function that applies ARGUMENTS and the arguments
it is called with to FUNCTION."
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      (multiple-value-call fn (values-list arguments) (values-list more)))))

(define-compiler-macro partial (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "PARTIAL"))
        (fun (gensym "FUN")))
    `(let ((,fun (ensure-function ,function))
           ,@(mapcar #'list curries arguments))
       (lambda (&rest more)
         (declare (dynamic-extent more))
         (apply ,fun ,@curries more)))))

(declaim (notinline partial))

(defmacro named-lambda (name lambda-list &body body)
  "Expands into a lambda-expression within whose BODY NAME denotes the
corresponding function."
  `(labels ((,name ,lambda-list ,@body))
     #',name))

;;; Enhanced composition operators

(defun pipe (&rest functions)
  "Compose functions left-to-right (opposite of compose).
   (pipe f g h) is equivalent to (compose h g f)
   and computes (h (g (f x))).

   Reads naturally for data transformation pipelines:
     (funcall (pipe #'parse-integer #'1+ #'number-to-string) \"41\")
     => \"42\""
  (if (null functions)
      #'identity
      (reduce (lambda (f g)
                (let ((f (ensure-function f))
                      (g (ensure-function g)))
                  (lambda (&rest args)
                    (declare (dynamic-extent args))
                    (funcall g (apply f args)))))
              functions)))

(define-compiler-macro pipe (&rest functions)
  (if (null functions)
      '#'identity
      (labels ((pipe-1 (funs result)
                 (if (null funs)
                     result
                     (pipe-1 (rest funs) `(funcall ,(first funs) ,result)))))
        (let ((funs (make-gensym-list (length functions) "PIPE")))
          `(let ,(loop for f in funs for arg in functions
                       collect `(,f (ensure-function ,arg)))
             (lambda (&rest arguments)
               (declare (dynamic-extent arguments))
               ,(pipe-1 (rest funs) `(apply ,(first funs) arguments))))))))

(defun juxt (&rest functions)
  "Return a function that returns a list of applying each function to args.
   ((juxt f g h) x) => (list (f x) (g x) (h x))

   Useful for applying multiple transformations at once:
     (funcall (juxt #'first #'last #'length) '(a b c d e))
     => (A E 5)"
  (let ((fns (mapcar #'ensure-function functions)))
    (lambda (&rest args)
      (declare (dynamic-extent args))
      (mapcar (lambda (f) (apply f args)) fns))))

(define-compiler-macro juxt (&rest functions)
  (let ((funs (make-gensym-list (length functions) "JUXT")))
    `(let ,(loop for f in funs for arg in functions
                 collect `(,f (ensure-function ,arg)))
       (lambda (&rest args)
         (declare (dynamic-extent args))
         (list ,@(mapcar (lambda (f) `(apply ,f args)) funs))))))

(defun complement* (function)
  "Return a function that returns the logical NOT of FUNCTION's result.
   Uses function designators (unlike CL:COMPLEMENT which only takes functions).

   Example:
     (funcall (complement* #'evenp) 3)  => T
     (remove-if (complement* #'alpha-char-p) \"a1b2c3\")  => \"abc\""
  (let ((f (ensure-function function)))
    (lambda (&rest args)
      (declare (dynamic-extent args))
      (not (apply f args)))))

(defun constantly* (value)
  "Return a function that always returns VALUE.
   Like CL:CONSTANTLY but provided for API consistency.

   Example:
     (mapcar (constantly* 42) '(a b c))  => (42 42 42)"
  (lambda (&rest args)
    (declare (ignore args))
    value))

(defun flip (function)
  "Return a function with first two arguments reversed.

   Example:
     (funcall (flip #'cons) '(1 2 3) 0)  => (0 1 2 3)
     (funcall (flip #'-) 3 10)           => 7"
  (let ((f (ensure-function function)))
    (lambda (a b &rest args)
      (declare (dynamic-extent args))
      (apply f b a args))))

(defun curry (function &rest args)
  "Alias for partial - curry FUNCTION with ARGS.
   Provided for familiarity with other functional languages.

   Example:
     (funcall (curry #'+ 1 2) 3 4)  => 10"
  (apply #'partial function args))

(defun rcurry (function &rest args)
  "Partial application from the right.
   Like curry/partial but ARGS are appended to the call.

   Example:
     (funcall (rcurry #'- 1) 10)    => 9  ; (- 10 1)
     (funcall (rcurry #'list 3) 1 2) => (1 2 3)"
  (let ((f (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      (apply f (append more args)))))

(define-compiler-macro rcurry (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "RCURRY"))
        (fun (gensym "FUN")))
    `(let ((,fun (ensure-function ,function))
           ,@(mapcar #'list curries arguments))
       (lambda (&rest more)
         (declare (dynamic-extent more))
         (apply ,fun (append more (list ,@curries)))))))
