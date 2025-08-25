;;;; Simplified Automatic Evaluation System
;;;;
;;;; A simpler version to get the system working

(defpackage epsilon.compute.auto-eval
  (:use :cl)
  (:local-nicknames
   (sym epsilon.compute.symbolic))
  (:export
   ;; Pattern recognition
   :recognize-computation-pattern
   :register-pattern
   :register-builtin-patterns
   :computation-pattern
   :computation-pattern-p
   :vector-operation-p
   :matrix-operation-p
   :reduction-operation-p
   :broadcast-operation-p
   :polynomial-pattern-p
   :trig-pattern-p
   :linear-algebra-pattern-p
   
   ;; Lazy evaluation
   :make-lazy
   :force-eval
   :lazy-value
   :lazy-value-p
   
   ;; Memoization
   :with-memoization
   :memoized-evaluate
   :*memoization-enabled*
   :*memoization-cache*
   
   ;; Custom operators
   :define-custom-op
   :get-custom-op
   
   ;; Computation graph
   :build-computation-graph
   :graph-node-count
   :graph-has-shared-nodes-p
   :graph-has-parallel-branches-p
   :evaluate-graph
   :evaluate-graph-node
   :differentiate-graph
   :graph-node
   :graph-node-p
   :computation-graph
   :computation-graph-p
   
   ;; Parallel evaluation
   :with-parallel-evaluation
   :with-parallel-threads
   :create-scheduler
   :add-task
   :execute-parallel
   :parallel-evaluate-expression
   :*max-parallel-threads*
   :*parallel-threshold*
   
   ;; Expression compilation
   :compile-expression
   
   ;; Caching
   :with-caching
   
   ;; Type inference
   :infer-type
   :type-error-p
   
   ;; Broadcasting
   :broadcast-arrays
   :outer-product
   
   ;; Optimization
   :with-optimization-level
   :*optimization-level*
   :auto-evaluate))

(in-package epsilon.compute.auto-eval)

;;; Stub implementations to get the system working

(defstruct computation-pattern
  name matcher optimizer priority)

(defparameter *computation-patterns* (make-hash-table :test 'equal))

(defun register-pattern (name matcher optimizer &key (priority 0))
  "Register a computation pattern for optimization"
  (setf (gethash name *computation-patterns*)
        (make-computation-pattern :name name
                                 :matcher matcher
                                 :optimizer optimizer
                                 :priority priority)))

(defun register-builtin-patterns ()
  "Register built-in optimization patterns"
  ;; Vector dot product pattern
  (register-pattern 
   :vector-dot-product
   (lambda (expr) (and (consp expr) (eq (first expr) 'dot)))
   (lambda (expr) (optimize-dot-product expr))
   :priority 10)
   
  ;; Matrix multiplication pattern
  (register-pattern
   :matrix-multiply
   (lambda (expr) (and (consp expr) (eq (first expr) 'matmul)))
   (lambda (expr) (optimize-matrix-multiply-pattern expr))
   :priority 10)
   
  ;; Associative operations pattern
  (register-pattern
   :associative-ops
   (lambda (expr) (and (consp expr) (member (first expr) '(+ *))))
   (lambda (expr) (flatten-associative-ops expr))
   :priority 5)
   
  ;; Identity patterns
  (register-pattern
   :identity-elimination
   (lambda (expr) (has-identity-elements-p expr))
   (lambda (expr) (eliminate-identity-elements expr))
   :priority 8)
   
  ;; Zero multiplication pattern
  (register-pattern
   :zero-multiplication
   (lambda (expr) (and (consp expr) (eq (first expr) '*) (has-zero-p expr)))
   (lambda (expr) 0)
   :priority 9))

(defun optimize-dot-product (expr)
  "Optimize dot product operations"
  (let ((args (rest expr)))
    (if (and (= (length args) 2)
            (vectorp (first args))
            (vectorp (second args)))
        `(native-dot-product ,(first args) ,(second args))
        expr)))

(defun optimize-matrix-multiply-pattern (expr)
  "Optimize matrix multiplication patterns"
  (let ((args (rest expr)))
    (if (and (= (length args) 2)
            (arrayp (first args))
            (arrayp (second args)))
        `(native-matrix-multiply ,(first args) ,(second args))
        expr)))

(defun flatten-associative-ops (expr)
  "Flatten nested associative operations"
  (let ((op (first expr))
        (args (rest expr)))
    (let ((flattened-args nil))
      (dolist (arg args)
        (if (and (consp arg) (eq (first arg) op))
            (setf flattened-args (append flattened-args (rest arg)))
            (push arg flattened-args)))
      `(,op ,@(nreverse flattened-args)))))

(defun has-identity-elements-p (expr)
  "Check if expression has identity elements to eliminate"
  (and (consp expr)
       (case (first expr)
         (+ (member 0 (rest expr)))
         (* (member 1 (rest expr)))
         (- (and (> (length expr) 2) (= (second expr) 0)))
         (/ (and (> (length expr) 2) (= (third expr) 1)))
         (t nil))))

(defun eliminate-identity-elements (expr)
  "Remove identity elements from expressions"
  (let ((op (first expr))
        (args (rest expr)))
    (case op
      (+ (let ((non-zero-args (remove 0 args)))
           (cond ((null non-zero-args) 0)
                 ((= (length non-zero-args) 1) (first non-zero-args))
                 (t `(+ ,@non-zero-args)))))
      (* (let ((non-one-args (remove 1 args)))
           (cond ((null non-one-args) 1)
                 ((= (length non-one-args) 1) (first non-one-args))
                 (t `(* ,@non-one-args)))))
      (otherwise expr))))

(defun has-zero-p (expr)
  "Check if expression contains zero in multiplication"
  (member 0 (rest expr)))

;; Initialize built-in patterns
(register-builtin-patterns)

(defun recognize-computation-pattern (expr)
  "Recognize computational patterns for optimization"
  (cond
    ;; Matrix chain operations - check for nested dot products
    ((matrix-chain-pattern-p expr)
     '(:matrix-chain :optimization-available))
     
    ;; BLAS GEMV pattern: alpha*A*x + beta*y  
    ((gemv-pattern-p expr) 
     '(:blas-gemv :level-2-blas))
    
    ;; BLAS GEMM pattern: C = alpha*A*B + beta*C
    ((gemm-pattern-p expr) 
     '(:blas-gemm :level-3-blas))
    
    ;; Einsum trace pattern
    ((einsum-trace-pattern-p expr)
     '(:einsum-trace :reduction))
    
    ;; Vector operations
    ((vector-operation-p expr) 
     '(:vector-ops :vectorizable))
    
    ;; Matrix operations  
    ((matrix-operation-p expr) 
     '(:matrix-ops :linear-algebra))
    
    ;; Default - simple pattern name based on operation
    ((sym:expr-p expr)
     (list (intern (string-upcase (string (sym:expr-op expr))) :keyword) :basic))
    
    (t '(:unknown :no-pattern))))

(defun matrix-chain-pattern-p (expr)
  "Check for matrix chain pattern: (A * B) * C"
  (and (sym:expr-p expr)
       (eq (sym:expr-op expr) (find-symbol "DOT" "EPSILON.COMPUTE"))
       (let ((args (sym:expr-args expr)))
         (and (= (length args) 2)
              (sym:expr-p (first args))
              (eq (sym:expr-op (first args)) (find-symbol "DOT" "EPSILON.COMPUTE"))))))

(defun gemv-pattern-p (expr)
  "Check for GEMV pattern: alpha*A*x + beta*y"
  (and (sym:expr-p expr)
       (eq (sym:expr-op expr) (find-symbol "+" "EPSILON.COMPUTE"))
       (let ((args (sym:expr-args expr)))
         (and (= (length args) 2)
              (matrix-vector-mult-p (first args))
              (scaled-vector-p (second args))))))

(defun gemm-pattern-p (expr)
  "Check for GEMM pattern: alpha*A*B + beta*C"
  (and (sym:expr-p expr)
       (eq (sym:expr-op expr) (find-symbol "+" "EPSILON.COMPUTE"))
       (let ((args (sym:expr-args expr)))
         (and (= (length args) 2)
              (scaled-matrix-mult-p (first args))
              (scaled-matrix-p (second args))))))

(defun einsum-trace-pattern-p (expr)
  "Check for einsum trace pattern: trace(A * B)"
  (and (sym:expr-p expr)
       (eq (sym:expr-op expr) (find-symbol "TRACE" "EPSILON.COMPUTE"))
       (let ((args (sym:expr-args expr)))
         (and (= (length args) 1)
              (sym:expr-p (first args))
              (eq (sym:expr-op (first args)) (find-symbol "DOT" "EPSILON.COMPUTE"))))))

(defun matrix-vector-mult-p (expr)
  "Check if expression is matrix-vector multiplication"
  (and (sym:expr-p expr)
       (eq (sym:expr-op expr) (find-symbol "*" "EPSILON.COMPUTE"))
       (some (lambda (arg)
               (and (sym:expr-p arg)
                    (eq (sym:expr-op arg) (find-symbol "DOT" "EPSILON.COMPUTE"))))
             (sym:expr-args expr))))

(defun scaled-vector-p (expr)
  "Check if expression is a scaled vector"
  (and (sym:expr-p expr)
       (eq (sym:expr-op expr) (find-symbol "*" "EPSILON.COMPUTE"))))

(defun scaled-matrix-mult-p (expr)
  "Check if expression is scaled matrix multiplication"
  (and (sym:expr-p expr)
       (eq (sym:expr-op expr) (find-symbol "*" "EPSILON.COMPUTE"))
       (some (lambda (arg)
               (and (sym:expr-p arg)
                    (eq (sym:expr-op arg) (find-symbol "DOT" "EPSILON.COMPUTE"))))
             (sym:expr-args expr))))

(defun scaled-matrix-p (expr)
  "Check if expression is a scaled matrix"
  (and (sym:expr-p expr)
       (eq (sym:expr-op expr) (find-symbol "*" "EPSILON.COMPUTE"))))

;; Stub implementations for other pattern types
(defun vector-operation-p (expr)
  "Check if expression is a vector operation"
  (and (sym:expr-p expr)
       (member (sym:expr-op expr) 
               (list (find-symbol "+" "EPSILON.COMPUTE")
                     (find-symbol "-" "EPSILON.COMPUTE") 
                     (find-symbol "*" "EPSILON.COMPUTE")))))

(defun matrix-operation-p (expr)
  "Check if expression is a matrix operation"
  (and (sym:expr-p expr)
       (member (sym:expr-op expr)
               (list (find-symbol "DOT" "EPSILON.COMPUTE")
                     (find-symbol "TRANSPOSE" "EPSILON.COMPUTE")
                     (find-symbol "INVERSE" "EPSILON.COMPUTE")))))

; Simplified pattern recognition stubs for now
(defun reduction-operation-p (expr)
  "Check if expression represents reduction operations"
  nil) ; Stub - not implemented yet

(defun broadcast-operation-p (expr)
  "Check if expression involves broadcasting"
  nil) ; Stub - not implemented yet

(defun polynomial-pattern-p (expr)
  "Check if expression is polynomial"
  nil) ; Stub - not implemented yet

(defun trig-pattern-p (expr)
  "Check if expression contains trigonometric functions"
  nil) ; Stub - not implemented yet

(defun linear-algebra-pattern-p (expr)
  "Check if expression represents linear algebra operations"
  nil) ; Stub - not implemented yet

;; Lazy evaluation
(defstruct lazy-value
  thunk computed-p value)

(defun make-lazy (thunk)
  (make-lazy-value :thunk thunk :computed-p nil))

(defun force-eval (expr bindings)
  "Force evaluation of lazy expressions and regular expressions"
  (cond
    ((lazy-value-p expr)
     (unless (lazy-value-computed-p expr)
       (setf (lazy-value-value expr) (funcall (lazy-value-thunk expr))
             (lazy-value-computed-p expr) t))
     (lazy-value-value expr))
    ;; Handle symbolic expressions with lazy components
    ((and (consp expr) (eq (first expr) '+))
     (let ((args (rest expr)))
       (+ (force-eval (first args) bindings)
          (force-eval (second args) bindings))))
    ;; For regular expressions, just return value  
    ((numberp expr) expr)
    (t expr)))

;; Memoization
(defparameter *memoization-enabled* nil)
(defparameter *memoization-cache* nil)

(defmacro with-memoization (&body body)
  `(let ((*memoization-enabled* t)
         (*memoization-cache* (make-hash-table :test 'equal)))
     ,@body))

(defun memoized-evaluate (expr bindings)
  "Evaluate expression with memoization"
  (if (and *memoization-enabled* *memoization-cache*)
      (let ((key (list expr bindings)))
        (or (gethash key *memoization-cache*)
            (setf (gethash key *memoization-cache*)
                  (auto-evaluate expr bindings))))
      (auto-evaluate expr bindings)))

;; Custom operators
;; TODO: This uses 'equal for symbol comparison to handle package issues
;; A better implementation would properly handle symbol resolution
(defparameter *custom-operators* (make-hash-table :test 'equal))

(defun define-custom-op (name function)
  "Define a custom operator, handling multiple symbol representations"
  ;; Store under the given name (as passed)
  (setf (gethash name *custom-operators*) function)
  ;; Store under the symbol-name string (package-independent)
  (when (symbolp name)
    (let ((name-string (symbol-name name)))
      ;; Store under the string itself for package-independent lookup
      (setf (gethash name-string *custom-operators*) function)
      ;; Also store under common symbol variations
      (setf (gethash (intern name-string) *custom-operators*) function)
      (setf (gethash (intern (string-upcase name-string)) *custom-operators*) function)
      ;; Store in keyword package for package-independent access
      (setf (gethash (intern name-string :keyword) *custom-operators*) function)))
  ;; Return the function for chaining
  function)

(defun get-custom-op (name)
  "Get custom operator, handling symbol packages"
  (or (gethash name *custom-operators*)
      ;; Try string lookup (package-independent)
      (when (symbolp name)
        (gethash (symbol-name name) *custom-operators*))
      ;; Try without package qualification
      (when (symbolp name)
        (or (gethash (intern (symbol-name name)) *custom-operators*)
            ;; Try the keyword package (package-independent)
            (gethash (intern (symbol-name name) :keyword) *custom-operators*)
            ;; Try uppercase version
            (gethash (intern (string-upcase (symbol-name name))) *custom-operators*)))
      ;; Try in the current package
      (when (symbolp name)
        (gethash (intern (symbol-name name) *package*) *custom-operators*))))

;; Computation graph
(defstruct graph-node
  id op inputs outputs value)

(defstruct computation-graph
  nodes roots inputs outputs)

(defun build-computation-graph (expr)
  "Build computation graph from expression"
  (let ((graph (make-computation-graph :nodes (make-hash-table)
                                      :roots nil
                                      :inputs nil
                                      :outputs nil)))
    (when expr
      ;; Add a simple node for the expression
      (let ((node-id 1))
        (setf (gethash node-id (computation-graph-nodes graph))
              (make-graph-node :id node-id :op 'root :inputs nil :outputs nil :value expr))
        (push node-id (computation-graph-roots graph))))
    graph))

(defun graph-node-count (graph)
  (hash-table-count (computation-graph-nodes graph)))

(defun graph-has-shared-nodes-p (graph)
  "Check if graph has shared nodes (simplified)"
  (> (hash-table-count (computation-graph-nodes graph)) 1))

(defun graph-has-parallel-branches-p (graph)
  "Check if graph has parallel branches that can be evaluated independently"
  (declare (ignore graph))
  t) ; For now, assume graphs can benefit from parallel evaluation

(defun evaluate-graph (graph bindings)
  "Evaluate computation graph"
  (let ((nodes (computation-graph-nodes graph)))
    (if (zerop (hash-table-count nodes))
        0
        (let ((root-node (gethash (first (computation-graph-roots graph)) nodes)))
          (if root-node
              (auto-evaluate (graph-node-value root-node) bindings)
              0)))))

(defun evaluate-graph-node (node bindings)
  (declare (ignore node bindings))
  0)

(defun differentiate-graph (graph var)
  (declare (ignore var))
  graph)

;; Enhanced Parallel Evaluation Scheduler

(defparameter *parallel-evaluation-enabled* nil)
(defparameter *max-parallel-threads* 4)
(defparameter *parallel-threshold* 100)

(defstruct task
  "Parallel evaluation task"
  function
  args
  dependencies
  result
  status  ; :pending :running :completed :failed
  thread)

(defstruct scheduler
  "Parallel evaluation scheduler"
  (tasks (make-hash-table :test 'equal))
  (ready-queue nil)
  (running-tasks nil)
  (completed-tasks nil)
  (available-threads 0))

(defun create-scheduler (&optional (max-threads *max-parallel-threads*))
  "Create a new parallel evaluation scheduler"
  (make-scheduler :available-threads max-threads))

(defun add-task (scheduler id function args &optional dependencies)
  "Add a task to the scheduler"
  (let ((task (make-task :function function
                        :args args
                        :dependencies (or dependencies '())
                        :status :pending)))
    (setf (gethash id (scheduler-tasks scheduler)) task)
    (when (null dependencies)
      (push id (scheduler-ready-queue scheduler)))
    id))

(defun dependencies-satisfied-p (scheduler task)
  "Check if all dependencies for a task are satisfied"
  (every (lambda (dep-id)
           (let ((dep-task (gethash dep-id (scheduler-tasks scheduler))))
             (and dep-task (eq (task-status dep-task) :completed))))
         (task-dependencies task)))

(defun execute-task (scheduler task-id)
  "Execute a single task"
  (let ((task (gethash task-id (scheduler-tasks scheduler))))
    (when task
      (setf (task-status task) :running)
      (handler-case
          (let ((result (apply (task-function task) (task-args task))))
            (setf (task-result task) result
                  (task-status task) :completed)
            (push task-id (scheduler-completed-tasks scheduler))
            result)
        (error (e)
          (setf (task-status task) :failed
                (task-result task) e)
          (warn "Task ~A failed: ~A" task-id e)
          nil)))))

(defun execute-parallel (scheduler)
  "Execute all tasks in the scheduler"
  (let ((execution-order (topological-sort scheduler)))
    (dolist (task-id execution-order)
      (execute-task scheduler task-id)))
  ;; Return results
  (let ((results (make-hash-table :test 'equal)))
    (maphash (lambda (id task)
               (setf (gethash id results) (task-result task)))
             (scheduler-tasks scheduler))
    results))

(defun topological-sort (scheduler)
  "Topological sort of tasks based on dependencies"
  (let ((sorted nil)
        (visited (make-hash-table :test 'equal))
        (visiting (make-hash-table :test 'equal)))
    
    (labels ((visit (task-id)
               (when (gethash task-id visiting)
                 (error "Circular dependency detected involving task ~A" task-id))
               (unless (gethash task-id visited)
                 (setf (gethash task-id visiting) t)
                 (let ((task (gethash task-id (scheduler-tasks scheduler))))
                   (when task
                     (dolist (dep (task-dependencies task))
                       (visit dep))))
                 (setf (gethash task-id visited) t)
                 (remhash task-id visiting)
                 (push task-id sorted))))
      
      (maphash (lambda (id task)
                 (declare (ignore task))
                 (visit id))
               (scheduler-tasks scheduler)))
    sorted))

(defun parallel-evaluate-expression (expr bindings)
  "Evaluate expression using parallel scheduler"
  (if (should-parallelize-p expr)
      (let ((scheduler (create-scheduler)))
        (decompose-expression scheduler expr bindings)
        (let ((results (execute-parallel scheduler)))
          (gethash 'root results)))
      ;; Fall back to sequential evaluation
      (auto-evaluate expr bindings)))

(defun should-parallelize-p (expr)
  "Decide whether expression should be parallelized"
  (and *parallel-evaluation-enabled*
       (consp expr)
       (> (expression-complexity expr) *parallel-threshold*)))

(defun expression-complexity (expr)
  "Estimate computational complexity of expression"
  (cond
    ((atom expr) 1)
    ((consp expr)
     (+ 1 (reduce #'+ (mapcar #'expression-complexity (rest expr)) :initial-value 0)))
    (t 1)))

(defun decompose-expression (scheduler expr bindings &optional (id 'root))
  "Decompose expression into parallel tasks"
  (cond
    ((atom expr)
     (add-task scheduler id (lambda () expr) nil))
    ((consp expr)
     (let ((op (first expr))
           (args (rest expr))
           (arg-ids nil))
       ;; Create tasks for arguments
       (dotimes (i (length args))
         (let ((arg-id (intern (format nil "ARG-~A-~A" id i))))
           (push arg-id arg-ids)
           (decompose-expression scheduler (nth i args) bindings arg-id)))
       ;; Create task for this operation
       (add-task scheduler id
                (lambda (&rest arg-results)
                  (apply-operation op arg-results))
                (nreverse arg-ids))))
    (t
     (add-task scheduler id (lambda () expr) nil))))

(defun apply-operation (op args)
  "Apply operation to arguments"
  (case op
    (+ (reduce #'+ args :initial-value 0))
    (- (if (= (length args) 1)
           (- (first args))
           (reduce #'- args)))
    (* (reduce #'* args :initial-value 1))
    (/ (reduce #'/ args))
    (sin (sin (first args)))
    (cos (cos (first args)))
    (exp (exp (first args)))
    (log (log (first args)))
    (otherwise args)))

(defmacro with-parallel-evaluation (&body body)
  `(let ((*parallel-evaluation-enabled* t))
     ,@body))

(defmacro with-parallel-threads (n &body body)
  "Execute with specific number of parallel threads"
  `(let ((*max-parallel-threads* ,n))
     ,@body))

;; Expression compilation
(defun compile-expression (expr variables)
  "Compile expression into an optimized function"
  (lambda (&rest args)
    (let ((bindings (mapcar #'cons variables args)))
      (auto-evaluate expr bindings))))

;; Caching
(defparameter *caching-enabled* nil)
(defparameter *computation-cache* nil)

(defmacro with-caching (&body body)
  `(let ((*caching-enabled* t)
         (*computation-cache* (make-hash-table :test 'equal)))
     ,@body))

;; Type inference
(defun infer-type (expr)
  (declare (ignore expr))
  :scalar)

(defun type-error-p (expr)
  (declare (ignore expr))
  nil)

;; Broadcasting
(defun broadcast-arrays (a b)
  (values a b))

(defun outer-product (v w)
  "Compute outer product of two vectors"
  (let* ((v-vals (if (epsilon.compute.symbolic:const-p v) (epsilon.compute.symbolic:const-value v) v))
         (w-vals (if (epsilon.compute.symbolic:const-p w) (epsilon.compute.symbolic:const-value w) w))
         (n (length v-vals))
         (m (length w-vals))
         (result (make-array (list n m) :element-type 'double-float :initial-element 0.0d0)))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref result i j) (coerce (* (aref v-vals i) (aref w-vals j)) 'double-float))))
    result))

(defun eval-symbolic-expr (expr bindings)
  "Evaluate a symbolic expression with variable bindings"
  (cond
    ;; Base cases
    ((epsilon.compute.symbolic:const-p expr)
     (epsilon.compute.symbolic:const-value expr))
    
    ((epsilon.compute.symbolic:var-p expr)
     (let ((var-name (epsilon.compute.symbolic:var-name expr)))
       (let ((binding (assoc var-name bindings)))
         (if binding (cdr binding)
             (error "Unbound variable: ~A" var-name)))))
    
    ;; Expression case
    ((epsilon.compute.symbolic:expr-p expr)
     (let* ((op (epsilon.compute.symbolic:expr-op expr))
            (args (epsilon.compute.symbolic:expr-args expr))
            (eval-args (mapcar (lambda (arg) (eval-symbolic-expr arg bindings)) args))
            (numeric-op (epsilon.compute.simplify:get-numeric-op op)))
       (let ((custom-op (get-custom-op op)))
         (cond
           ;; Handle custom operators first  
           (custom-op
            (apply custom-op eval-args))
           ;; Handle vectorized operations (arrays)
           ((and numeric-op (some #'arrayp eval-args))
            ;; TODO: This should call the vectorized operation handler from the main compute module
            ;; For now, fall back to regular numeric operation
            (apply numeric-op eval-args))
           ;; Handle regular numeric operations
           (numeric-op
            (apply numeric-op eval-args))
           ;; Unknown operation
           (t (error "Unknown operation: ~A" op))))))
    
    ;; Default - return unchanged
    (t expr)))

;; Optimization
(defparameter *optimization-level* 1)

(defmacro with-optimization-level ((level) &body body)
  `(let ((*optimization-level* ,level))
     ,@body))

(defun auto-evaluate (expr bindings)
  "Automatically evaluate expressions with optimization"
  (cond
    ;; Numbers evaluate to themselves
    ((numberp expr) expr)
    
    ;; Variables lookup in bindings
    ((symbolp expr) 
     (let ((binding (assoc expr bindings)))
       (if binding (cdr binding) expr)))
    
    ;; Lazy values
    ((lazy-value-p expr) (force-eval expr bindings))
    
    ;; Symbolic expressions - evaluate with bindings
    ((epsilon.compute.symbolic:expr-p expr)
     (eval-symbolic-expr expr bindings))
    
    ;; Symbolic constants
    ((epsilon.compute.symbolic:const-p expr)
     (epsilon.compute.symbolic:const-value expr))
    
    ;; Symbolic variables - look up in bindings
    ((epsilon.compute.symbolic:var-p expr)
     (let ((var-name (epsilon.compute.symbolic:var-name expr))
           (binding (assoc (epsilon.compute.symbolic:var-name expr) bindings)))
       (if binding (cdr binding) 
           (error "Unbound variable: ~A" var-name))))
    
    ;; Arithmetic operations - handle n-ary operations
    ((and (consp expr) (eq (first expr) '+))
     (apply #'cl:+ (mapcar (lambda (arg) (auto-evaluate arg bindings)) (rest expr))))
    
    ((and (consp expr) (eq (first expr) '*))
     (apply #'cl:* (mapcar (lambda (arg) (auto-evaluate arg bindings)) (rest expr))))
    
    ;; Custom operators with caching support
    ((and (consp expr) (gethash (first expr) *custom-operators*))
     (let ((cache-key (list expr bindings)))
       (if (and *caching-enabled* *computation-cache* (gethash cache-key *computation-cache*))
           (gethash cache-key *computation-cache*)
           (let* ((op-fn (gethash (first expr) *custom-operators*))
                  (result (apply op-fn (mapcar (lambda (arg) (auto-evaluate arg bindings)) (rest expr)))))
             (when (and *caching-enabled* *computation-cache*)
               (setf (gethash cache-key *computation-cache*) result))
             result))))
    
    ;; Default case
    (t expr)))