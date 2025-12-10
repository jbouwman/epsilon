(defpackage #:epsilon.frame.ops
  (:use #:cl)
  (:local-nicknames
   (#:dtype #:epsilon.frame.dtype)
   (#:col #:epsilon.frame.column))
  (:export
   ;; Expression protocol
   #:expression
   #:expression-p
   #:make-expression
   #:expression-op
   #:expression-args
   #:evaluate-expression
   
   ;; Expression builders
   #:expr
   #:col-ref
   #:lit
   #:+expr
   #:-expr
   #:*expr
   #:/expr
   #:>expr
   #:<expr
   #:>=expr
   #:<=expr
   #:=expr
   #:and-expr
   #:or-expr
   #:not-expr
   
   ;; Aggregation functions
   #:sum-expr
   #:mean-expr
   #:min-expr
   #:max-expr
   #:count-expr
   #:std-expr
   #:var-expr))

(in-package #:epsilon.frame.ops)

;;; Expression representation

(defstruct expression
  "Lazy expression tree for column operations"
  (op nil :type symbol)
  (args nil :type list))

(defun expr (op &rest args)
  "Create an expression"
  (make-expression :op op :args args))

(defun col-ref (name)
  "Reference a column by name"
  (expr :col-ref name))

(defun lit (value)
  "Literal value"
  (expr :lit value))

;;; Arithmetic operations

(defun +expr (&rest args)
  "Addition expression"
  (expr :+ args))

(defun -expr (&rest args)
  "Subtraction expression"
  (expr :- args))

(defun *expr (&rest args)
  "Multiplication expression"
  (expr :* args))

(defun /expr (&rest args)
  "Division expression"
  (expr :/ args))

;;; Comparison operations

(defun >expr (left right)
  "Greater than expression"
  (expr :> left right))

(defun <expr (left right)
  "Less than expression"
  (expr :< left right))

(defun =expr (left right)
  "Equality expression"
  (expr := left right))

(defun >=expr (left right)
  "Greater than or equal expression"
  (expr :>= left right))

(defun <=expr (left right)
  "Less than or equal expression"
  (expr :<= left right))

;;; Logical operations

(defun and-expr (&rest args)
  "Logical AND expression"
  (expr :and args))

(defun or-expr (&rest args)
  "Logical OR expression"
  (expr :or args))

(defun not-expr (arg)
  "Logical NOT expression"
  (expr :not arg))

;;; Aggregation operations

(defun sum-expr (arg)
  "Sum aggregation"
  (expr :sum arg))

(defun mean-expr (arg)
  "Mean aggregation"
  (expr :mean arg))

(defun min-expr (arg)
  "Minimum aggregation"
  (expr :min arg))

(defun max-expr (arg)
  "Maximum aggregation"
  (expr :max arg))

(defun count-expr (&optional arg)
  "Count aggregation"
  (expr :count arg))

(defun std-expr (arg)
  "Standard deviation aggregation"
  (expr :std arg))

(defun var-expr (arg)
  "Variance aggregation"
  (expr :var arg))

;;; Expression evaluation

(defgeneric evaluate-expression (expr context)
  (:documentation "Evaluate an expression in a context"))

(defmethod evaluate-expression ((expr expression) context)
  "Evaluate an expression tree"
  (let ((op (expression-op expr))
        (args (expression-args expr)))
    (case op
      (:col-ref 
       (get-column-from-context context (first args)))
      (:lit 
       (make-literal-column context (first args)))
      (:+
       (apply-arithmetic #'+ (mapcar (lambda (arg) (evaluate-expression arg context)) (first args))))
      (:-
       (apply-arithmetic #'- (mapcar (lambda (arg) (evaluate-expression arg context)) (first args))))
      (:*
       (apply-arithmetic #'* (mapcar (lambda (arg) (evaluate-expression arg context)) (first args))))
      (:/
       (apply-arithmetic (lambda (&rest args) (coerce (apply #'/ args) 'double-float)) (mapcar (lambda (arg) (evaluate-expression arg context)) (first args))))
      (:>
       (apply-comparison #'> 
                        (evaluate-expression (first args) context)
                        (evaluate-expression (second args) context)))
      (:<
       (apply-comparison #'< 
                        (evaluate-expression (first args) context)
                        (evaluate-expression (second args) context)))
      (:=
       (apply-comparison #'= 
                        (evaluate-expression (first args) context)
                        (evaluate-expression (second args) context)))
      (:>=
       (apply-comparison #'>= 
                        (evaluate-expression (first args) context)
                        (evaluate-expression (second args) context)))
      (:<=
       (apply-comparison #'<= 
                        (evaluate-expression (first args) context)
                        (evaluate-expression (second args) context)))
      (:and
       (apply-logical #'(lambda (&rest args) (every #'identity args))
                     (mapcar (lambda (arg) (evaluate-expression arg context)) (first args))))
      (:or
       (apply-logical #'(lambda (&rest args) (some #'identity args))
                     (mapcar (lambda (arg) (evaluate-expression arg context)) (first args))))
      (:not
       (apply-logical #'not (list (evaluate-expression (first args) context))))
      (:sum
       (aggregate-sum (evaluate-expression (first args) context)))
      (:mean
       (aggregate-mean (evaluate-expression (first args) context)))
      (:min
       (aggregate-min (evaluate-expression (first args) context)))
      (:max
       (aggregate-max (evaluate-expression (first args) context)))
      (:count
       (if (first args)
           (aggregate-count (evaluate-expression (first args) context))
           (aggregate-count-all context)))
      (:std
       (aggregate-std (evaluate-expression (first args) context)))
      (:var
       (aggregate-var (evaluate-expression (first args) context)))
      (otherwise
       (error "Unknown operation: ~A" op)))))

(defmethod evaluate-expression ((expr symbol) context)
  "Evaluate a symbol as a column reference"
  (get-column-from-context context expr))

(defmethod evaluate-expression ((expr number) context)
  "Evaluate a number as a literal"
  (make-literal-column context expr))

(defmethod evaluate-expression ((expr string) context)
  "Evaluate a string as a literal"
  (make-literal-column context expr))

(defmethod evaluate-expression ((expr list) context)
  "Evaluate a list of expressions"
  (mapcar (lambda (e) (evaluate-expression e context)) expr))

;;; Helper functions for evaluation

(defun get-column-from-context (context name)
  "Get a column from the evaluation context"
  (if (hash-table-p context)
      (or (gethash (string-downcase (string name)) context)
          (error "Column ~A not found in context" name))
      (error "Invalid context: ~A" context)))

(defun make-literal-column (context value)
  "Create a column filled with a literal value"
  (let ((length (if (hash-table-p context)
                    (col:column-length (first (loop for v being the hash-values of context
                                                    collect v)))
                    1)))
    (col:make-column (dtype:infer-dtype value)
                     (make-list length :initial-element value))))

(defun apply-arithmetic (op cols)
  "Apply arithmetic operation to columns"
  (when (null cols)
    (error "Arithmetic operation requires at least one argument"))
  (if (= (length cols) 1)
      (first cols)
      (let* ((first-col (first cols))
             (len (col:column-length first-col))
             (result (make-array len)))
        ;; Apply operation element-wise
        (dotimes (i len)
          (setf (aref result i)
                (apply op (mapcar (lambda (col) 
                                   (if (col:column-p col)
                                       (col:column-get col i)
                                       col))
                                 cols))))
        ;; Create result column with inferred type
        (col:make-column (dtype:infer-dtype (aref result 0)) result))))

(defun apply-comparison (op left-col right-col)
  "Apply comparison operation to columns"
  (let* ((len (col:column-length left-col))
         (result (make-array len)))
    (dotimes (i len)
      (setf (aref result i)
            (funcall op 
                    (col:column-get left-col i)
                    (if (col:column-p right-col)
                        (col:column-get right-col i)
                        right-col))))
    (col:make-column :bool result)))

(defun apply-logical (op cols)
  "Apply logical operation to boolean columns"
  (let* ((first-col (first cols))
         (len (col:column-length first-col))
         (result (make-array len)))
    (dotimes (i len)
      (setf (aref result i)
            (apply op (mapcar (lambda (col) 
                               (col:column-get col i))
                             cols))))
    (col:make-column :bool result)))

(defun aggregate-sum (col)
  "Sum aggregation"
  (let ((sum 0))
    (dotimes (i (col:column-length col))
      (incf sum (col:column-get col i)))
    sum))

(defun aggregate-mean (col)
  "Mean aggregation"
  (/ (aggregate-sum col) (col:column-length col)))

(defun aggregate-min (col)
  "Minimum aggregation"
  (when (= (col:column-length col) 0)
    (return-from aggregate-min nil))
  (let ((min-val (col:column-get col 0)))
    (loop for i from 1 below (col:column-length col)
          do (setf min-val (min min-val (col:column-get col i))))
    min-val))

(defun aggregate-max (col)
  "Maximum aggregation"
  (when (= (col:column-length col) 0)
    (return-from aggregate-max nil))
  (let ((max-val (col:column-get col 0)))
    (loop for i from 1 below (col:column-length col)
          do (setf max-val (max max-val (col:column-get col i))))
    max-val))

(defun aggregate-count (col)
  "Count non-nil values"
  (let ((count 0))
    (dotimes (i (col:column-length col))
      (when (col:column-get col i)
        (incf count)))
    count))

(defun aggregate-count-all (context)
  "Count all rows"
  (if (hash-table-p context)
      (col:column-length (first (loop for v being the hash-values of context
                                      collect v)))
      0))

(defun aggregate-std (col)
  "Standard deviation aggregation"
  (sqrt (aggregate-var col)))

(defun aggregate-var (col)
  "Variance aggregation"
  (let* ((mean (aggregate-mean col))
         (n (col:column-length col))
         (sum-sq 0))
    (dotimes (i n)
      (let ((diff (- (col:column-get col i) mean)))
        (incf sum-sq (* diff diff))))
    (/ sum-sq n)))