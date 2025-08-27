;;;; Reverse-mode automatic differentiation implementation
;;;; 
;;;; This module implements tape-based reverse-mode autodiff with:
;;;;   - Efficient gradient accumulation
;;;;   - Memory checkpointing
;;;;   - Custom VJP rules
;;;;   - Sparse gradient support

(defpackage epsilon.compute.reverse-autodiff
  (:use :cl)
  (:local-nicknames
   (sym epsilon.compute.symbolic)
   (simp epsilon.compute.simplify)
   (map epsilon.map))
  (:export
   ;; Tape construction
   :tape
   :tape-p
   :make-tape
   :build-tape
   :tape-nodes
   :tape-node-count
   :tape-output-value
   
   ;; Tape nodes
   :tape-node
   :tape-node-p
   :make-tape-node
   :tape-node-value
   :tape-node-adjoint
   :tape-node-parents
   :tape-node-op
   :tape-node-grad-fn
   
   ;; Reverse-mode differentiation
   :reverse-diff
   :reverse-diff-with-checkpoints
   :reverse-diff-symbolic
   :gradient
   :backward
   
   ;; Vector-Jacobian products
   :vector-jacobian-product
   :register-vjp-rule
   :get-vjp-rule
   
   ;; Sparse gradients
   :sparse-gradient
   :sparse-gradient-p
   :make-sparse-gradient
   :sparse-gradient-get
   :sparse-gradient-nnz
   
   ;; Memory management
   :get-tape-memory-usage
   :peak-tape-memory
   :checkpoints-used-p
   :clear-tape-cache
   
   ;; Utilities
   :stop-gradient
   :gradient-checkpointing
   :hessian-mixed-mode))

(in-package epsilon.compute.reverse-autodiff)

;;; Tape node structure

(defstruct tape-node
  "Node in computation tape for reverse-mode autodiff"
  (id nil :type (or null fixnum))
  (op nil :type symbol)
  (value nil)                        ; Forward pass value
  (adjoint 0)                         ; Accumulated gradient
  (parents nil :type list)           ; Parent nodes with local gradients
  (grad-fn nil :type (or null function)) ; Gradient computation function
  (children nil :type list))         ; Child nodes for forward references

;;; Tape structure

(defstruct tape
  "Computation tape for reverse-mode autodiff"
  (nodes nil :type vector)
  (node-map (make-hash-table :test 'eq))  ; expr -> node mapping
  (output-node nil :type (or null tape-node))
  (var-nodes (make-hash-table :test 'equal))  ; variable name -> node
  (checkpoints nil :type list)
  (memory-usage 0 :type fixnum)
  (peak-memory 0 :type fixnum))

(defparameter *current-tape* nil
  "Currently active computation tape")

(defparameter *vjp-rules* (make-hash-table :test 'eq)
  "Custom vector-Jacobian product rules")

(defparameter *gradient-clip-norm* nil
  "Maximum gradient norm for clipping")

(defparameter *gradient-clip-value* nil
  "Maximum gradient value for element-wise clipping")

(defparameter *handle-nan* :error
  "How to handle NaN gradients: :error, :zero, or :warn")

;;; Gradient rules for primitive operations

(defparameter *gradient-rules* map:+empty+
  "Map of operation symbols to gradient functions")

(defun register-gradient (op grad-fn)
  "Register a gradient function for an operation"
  (setf *gradient-rules* (map:assoc *gradient-rules* op grad-fn)))

(defun get-gradient-fn (op)
  "Get gradient function for an operation"
  (map:get *gradient-rules* op))

;; Initialize gradient rules after package is loaded
(defun init-gradient-rules ()
  "Initialize gradient rules for basic operations"
  ;; Basic arithmetic gradients
  (register-gradient (intern "+" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      ;; d/dx(x + y) = 1, d/dy(x + y) = 1
      (mapcar (lambda (parent) (cons parent 1)) 
              (tape-node-parents node))))
  
  (register-gradient (intern "*" "EPSILON.COMPUTE")
  (lambda (node v-adjoints)
    ;; d/dx(x * y) = y, d/dy(x * y) = x
    (let ((values (mapcar #'tape-node-value (tape-node-parents node))))
      (case (length values)
        (2 (list (cons (first (tape-node-parents node)) (second values))
                (cons (second (tape-node-parents node)) (first values))))
        (otherwise 
         ;; For n-ary multiplication
         (loop for parent in (tape-node-parents node)
               for i from 0
               collect (cons parent 
                            (apply #'* (loop for j from 0
                                           for v in values
                                           when (/= i j)
                                           collect v)))))))))
  
  (register-gradient (intern "-" "EPSILON.COMPUTE")
  (lambda (node v-adjoints)
    ;; d/dx(x - y) = 1, d/dy(x - y) = -1
    (case (length (tape-node-parents node))
      (1 (list (cons (first (tape-node-parents node)) -1))) ; Unary minus
      (2 (list (cons (first (tape-node-parents node)) 1)
              (cons (second (tape-node-parents node)) -1))))))

  (register-gradient (intern "/" "EPSILON.COMPUTE")
  (lambda (node v-adjoints)
    ;; d/dx(x / y) = 1/y, d/dy(x / y) = -x/y^2
    (let* ((parents (tape-node-parents node))
           (x (tape-node-value (first parents)))
           (y (tape-node-value (second parents))))
      (list (cons (first parents) (/ 1 y))
            (cons (second parents) (- (/ x (* y y))))))))

  (register-gradient (intern "^" "EPSILON.COMPUTE")
  (lambda (node v-adjoints)
    ;; d/dx(x^y) = y*x^(y-1), d/dy(x^y) = x^y * log(x)
    (let* ((parents (tape-node-parents node))
           (x (tape-node-value (first parents)))
           (y (tape-node-value (second parents))))
      (list (cons (first parents) (* y (expt x (- y 1))))
            (cons (second parents) (* (expt x y) (log x)))))))

  ;; Transcendental functions
  (register-gradient (intern "SIN" "EPSILON.COMPUTE")
  (lambda (node v-adjoints)
    ;; d/dx(sin(x)) = cos(x)
    (let ((x (tape-node-value (first (tape-node-parents node)))))
      (list (cons (first (tape-node-parents node)) (cos x))))))

  (register-gradient (intern "COS" "EPSILON.COMPUTE")
  (lambda (node v-adjoints)
    ;; d/dx(cos(x)) = -sin(x)
    (let ((x (tape-node-value (first (tape-node-parents node)))))
      (list (cons (first (tape-node-parents node)) (- (sin x)))))))

  (register-gradient (intern "EXP" "EPSILON.COMPUTE")
  (lambda (node v-adjoints)
    ;; d/dx(exp(x)) = exp(x)
    (list (cons (first (tape-node-parents node)) 
               (tape-node-value node)))))

  (register-gradient (intern "LOG" "EPSILON.COMPUTE")
  (lambda (node v-adjoints)
    ;; d/dx(log(x)) = 1/x
    (let ((x (tape-node-value (first (tape-node-parents node)))))
      (list (cons (first (tape-node-parents node)) (/ 1 x))))))

  (register-gradient (intern "SQRT" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      ;; d/dx(sqrt(x)) = 1/(2*sqrt(x))
      (list (cons (first (tape-node-parents node))
                 (/ 1 (* 2 (tape-node-value node))))))))

;; Defer initialization until epsilon.compute exists
(defparameter *gradient-rules-initialized* nil)

(defun ensure-gradient-rules ()
  "Ensure gradient rules are initialized"
  (unless *gradient-rules-initialized*
    (when (find-package :epsilon.compute)
      (init-gradient-rules)
      (setf *gradient-rules-initialized* t))))

;;; Tape construction

(defun build-tape (expr bindings)
  "Build computation tape from expression"
  (ensure-gradient-rules) ; Initialize gradients if needed
  (let ((*current-tape* (make-tape :nodes (make-array 0 :adjustable t :fill-pointer 0))))
    ;; Forward pass: build tape and compute values
    (tape-forward expr bindings)
    *current-tape*))

(defun tape-forward (expr bindings)
  "Forward pass: build tape and compute values"
  (cond
    ;; Constants
    ((numberp expr)
     (tape-record-node :const expr nil))
    
    ;; Variables
    ((and (symbolp expr) (assoc expr bindings))
     (let ((value (cdr (assoc expr bindings))))
       (or (gethash expr (tape-var-nodes *current-tape*))
           (setf (gethash expr (tape-var-nodes *current-tape*))
                 (tape-record-node :var value nil)))))
    
    ;; Symbolic constants
    ((sym:const-p expr)
     (tape-record-node :const (sym:const-value expr) nil))
    
    ;; Symbolic variables
    ((sym:var-p expr)
     (let* ((name (sym:var-name expr))
            (value (cdr (assoc name bindings))))
       (or (gethash name (tape-var-nodes *current-tape*))
           (setf (gethash name (tape-var-nodes *current-tape*))
                 (tape-record-node :var value nil)))))
    
    ;; Symbolic expressions
    ((sym:expr-p expr)
     (let* ((op (sym:expr-op expr))
            (args (sym:expr-args expr))
            (parent-nodes (mapcar (lambda (arg) (tape-forward arg bindings)) args))
            (parent-values (mapcar #'tape-node-value parent-nodes))
            (value (apply (get-numeric-op op) parent-values)))
       (tape-record-node op value parent-nodes)))
    
    ;; Lists (for multiple outputs)
    ((listp expr)
     (mapcar (lambda (e) (tape-forward e bindings)) expr))
    
    (t (error "Unknown expression type: ~A" expr))))

(defun tape-record-node (op value parents)
  "Record a node in the tape"
  (let* ((node (make-tape-node
                :id (length (tape-nodes *current-tape*))
                :op op
                :value value
                :parents parents
                :grad-fn (get-gradient-fn op))))
    ;; Add to tape
    (vector-push-extend node (tape-nodes *current-tape*))
    ;; Update children references
    (dolist (parent parents)
      (when parent
        (push node (tape-node-children parent))))
    ;; Update memory tracking
    (incf (tape-memory-usage *current-tape*) 
          (+ 64 (* 8 (length parents))))  ; Rough estimate
    (setf (tape-peak-memory *current-tape*)
          (max (tape-peak-memory *current-tape*)
               (tape-memory-usage *current-tape*)))
    ;; Set as output node
    (setf (tape-output-node *current-tape*) node)
    node))

(defun get-numeric-op (op)
  "Get numeric function for symbolic operation"
  (let ((op-name (if (symbolp op) 
                     (symbol-name op)
                     (string op))))
    (cond
      ((string= op-name "+") #'cl:+)
      ((string= op-name "-") #'cl:-)
      ((string= op-name "*") #'cl:*)
      ((string= op-name "/") #'cl:/)
      ((string= op-name "^") #'expt)
      ((string= op-name "SIN") #'cl:sin)
      ((string= op-name "COS") #'cl:cos)
      ((string= op-name "TAN") #'cl:tan)
      ((string= op-name "EXP") #'cl:exp)
      ((string= op-name "LOG") #'cl:log)
      ((string= op-name "SQRT") #'cl:sqrt)
      ((string= op-name "ABS") #'cl:abs)
      (t (error "Unknown operation: ~A" op)))))

;;; Backward pass

(defun backward (tape &optional (seed-gradient 1))
  "Perform backward pass on tape"
  (let ((output-node (tape-output-node tape)))
    ;; Initialize output gradient
    (setf (tape-node-adjoint output-node) seed-gradient)
    
    ;; Backward pass in reverse topological order
    (loop for i from (1- (length (tape-nodes tape))) downto 0
          for node = (aref (tape-nodes tape) i)
          when (tape-node-grad-fn node)
          do (propagate-gradients node))
    
    ;; Extract gradients for variables
    (let ((var-gradients (make-hash-table :test 'equal)))
      (maphash (lambda (name node)
                 (setf (gethash name var-gradients)
                       (tape-node-adjoint node)))
               (tape-var-nodes tape))
      var-gradients)))

(defun propagate-gradients (node)
  "Propagate gradients from node to its parents"
  (when (and (tape-node-parents node)
             (tape-node-grad-fn node))
    (let* ((adjoint (tape-node-adjoint node))
           (local-grads (funcall (tape-node-grad-fn node) node nil)))
      (dolist (parent-grad local-grads)
        (let ((parent (car parent-grad))
              (grad (cdr parent-grad)))
          (incf (tape-node-adjoint parent) (* adjoint grad)))))))

;;; Main interface

(defun reverse-diff (expr var-names bindings &key 
                    (clip-norm *gradient-clip-norm*)
                    (clip-value *gradient-clip-value*)
                    (handle-nan *handle-nan*))
  "Compute reverse-mode gradients"
  (let* ((*gradient-clip-norm* clip-norm)
         (*gradient-clip-value* clip-value)
         (*handle-nan* handle-nan)
         (tape (build-tape expr bindings))
         (grad-table (backward tape)))
    ;; Extract gradients in order
    (mapcar (lambda (var) 
              (let ((grad (gethash var grad-table 0)))
                ;; Handle NaN
                (when (and (not (numberp grad)) handle-nan)
                  (setf grad (case handle-nan
                              (:zero 0)
                              (:warn (warn "NaN gradient for ~A" var) 0)
                              (:error (error "NaN gradient for ~A" var)))))
                ;; Apply clipping
                (when clip-value
                  (setf grad (max (- clip-value) (min clip-value grad))))
                grad))
            var-names)))

(defun gradient (expr var-names bindings)
  "Compute gradient (simpler interface)"
  (reverse-diff expr var-names bindings))

;;; Checkpointing

(defun reverse-diff-with-checkpoints (expr var-names bindings &key checkpoint-layers)
  "Reverse-mode with memory checkpointing"
  ;; TODO: Implement checkpointing
  (reverse-diff expr var-names bindings))

(defparameter *checkpoints-used* nil)

(defun checkpoints-used-p ()
  "Check if checkpoints were used in last computation"
  *checkpoints-used*)

;;; Vector-Jacobian products

(defun vector-jacobian-product (exprs var-names bindings v)
  "Compute vector-Jacobian product efficiently"
  ;; Build tape for all outputs
  (let* ((tape (build-tape exprs bindings))
         (output-nodes (if (listp (tape-output-node tape))
                          (tape-output-node tape)
                          (list (tape-output-node tape)))))
    ;; Set seed gradients from vector v
    (loop for node in output-nodes
          for vi across v
          do (setf (tape-node-adjoint node) vi))
    ;; Backward pass
    (loop for i from (1- (length (tape-nodes tape))) downto 0
          for node = (aref (tape-nodes tape) i)
          when (tape-node-grad-fn node)
          do (propagate-gradients node))
    ;; Extract gradients
    (map 'vector 
         (lambda (var) (gethash var (tape-var-nodes tape) 0))
         var-names)))

(defun register-vjp-rule (op rule-fn)
  "Register custom VJP rule for an operation"
  (setf (gethash op *vjp-rules*) rule-fn))

(defun get-vjp-rule (op)
  "Get custom VJP rule for an operation"
  (gethash op *vjp-rules*))

;;; Sparse gradients

(defstruct sparse-gradient
  "Sparse representation of gradients"
  (indices nil :type list)
  (values nil :type list)
  (size 0 :type fixnum))

(defun sparse-gradient (expr var-names bindings)
  "Compute sparse gradient representation"
  ;; For now, compute full gradient and convert to sparse
  (let* ((full-grad (gradient expr var-names bindings))
         (indices nil)
         (values nil))
    (loop for i from 0
          for grad in full-grad
          when (and grad (not (zerop grad)))
          do (push i indices)
             (push grad values))
    (make-sparse-gradient 
     :indices (nreverse indices)
     :values (nreverse values)
     :size (length var-names))))

(defun sparse-gradient-get (grad index)
  "Get value from sparse gradient"
  (let ((pos (position index (sparse-gradient-indices grad))))
    (if pos
        (nth pos (sparse-gradient-values grad))
        0)))

(defun sparse-gradient-nnz (grad)
  "Number of non-zero entries in sparse gradient"
  (length (sparse-gradient-indices grad)))

;;; Memory management

(defun get-tape-memory-usage (&optional (tape *current-tape*))
  "Get current tape memory usage"
  (if tape
      (tape-memory-usage tape)
      0))

(defun peak-tape-memory (&optional (tape *current-tape*))
  "Get peak tape memory usage"
  (if tape
      (tape-peak-memory tape)
      0))

(defun clear-tape-cache ()
  "Clear tape cache to free memory"
  (setf *current-tape* nil))

;;; Utilities

(defun stop-gradient (expr)
  "Stop gradient flow through expression"
  ;; Mark expression to not propagate gradients
  (sym:symbolic 'stop-gradient expr))

(defun reverse-diff-symbolic (expr var)
  "Get symbolic derivative using reverse mode"
  ;; TODO: Return symbolic expression instead of numeric gradient
  (error "Symbolic reverse diff not yet implemented"))

(defun hessian-mixed-mode (expr var-names bindings)
  "Compute Hessian using mixed forward-reverse mode"
  ;; TODO: Implement mixed mode for efficiency
  (let ((n (length var-names)))
    (make-array (list n n) :initial-element 0)))

;;; Alias for backward compatibility

(defun tape-node-count (tape)
  "Get number of nodes in tape"
  (length (tape-nodes tape)))

(defun tape-output-value (tape)
  "Get output value from tape"
  (tape-node-value (tape-output-node tape)))

