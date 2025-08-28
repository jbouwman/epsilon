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
   (map epsilon.map)
   (bc epsilon.compute.broadcasting))
  (:export
   ;; Tape construction
   :tape
   :tape-p
   :make-tape
   :build-tape
   :tape-nodes
   :tape-node-count
   :tape-output-value
   :tape-output-node
   
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
   :hessian-mixed-mode
   
   ;; Initialization
   :ensure-gradient-rules
   :init-gradient-rules))

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

(defmethod print-object ((node tape-node) stream)
  "Custom print method to avoid circular reference issues"
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "id=~A op=~A value=~A"
            (tape-node-id node)
            (tape-node-op node)
            (tape-node-value node))))

;;; Sparse gradient structure

(defstruct sparse-gradient
  "Sparse gradient representation"
  (indices nil :type list)     ; Indices of non-zero entries
  (values nil :type list)      ; Values at those indices  
  (size 0 :type fixnum))       ; Total size of gradient vector

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

(defvar *gradient-rules* nil
  "Map of operation symbols to gradient functions")

;; Initialize if needed
(unless *gradient-rules*
  (setf *gradient-rules* map:+empty+))

(defun register-gradient (op grad-fn)
  "Register a gradient function for an operation"
  (setf *gradient-rules* (map:assoc *gradient-rules* op grad-fn)))

(defun get-gradient-fn (op)
  "Get gradient function for an operation"
  ;; Try multiple ways to find the gradient function
  (or (map:get *gradient-rules* op)
      ;; If op is qualified, try just the symbol name
      (when (symbolp op)
        (map:get *gradient-rules* (intern (symbol-name op) "EPSILON.COMPUTE")))
      ;; Try as a string
      (when (symbolp op)
        (map:get *gradient-rules* (symbol-name op)))))

;; Track if gradient rules have been initialized
(defvar *gradient-rules-initialized* nil)

;; Function to ensure gradient rules are initialized
(defun ensure-gradient-rules ()
  "Ensure gradient rules are initialized once"
  (unless *gradient-rules-initialized*
    (init-gradient-rules)
    (setf *gradient-rules-initialized* t)))

;;; Basic arithmetic gradient rules

(defun init-gradient-rules ()
  "Initialize all gradient rules.
   
   Broadcasting behavior for gradients:
   When operations broadcast inputs to produce outputs, the gradients must be
   'unbroadcast' back to the original input shapes. This follows the chain rule:
   - If forward pass broadcasts x from shape A to B, backward must sum gradient from B to A
   - Unbroadcasting sums gradients along broadcast dimensions
   - Scalar gradients are summed over all elements when broadcast from arrays
   
   Example: x(3,1) * y(1,4) = z(3,4)
   - Gradient wrt x must sum over the broadcast dimension (columns)
   - Gradient wrt y must sum over the broadcast dimension (rows)"
  
  ;; Addition gradient rule
  (register-gradient (intern "+" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(x + y) = 1, d/dy(x + y) = 1
      ;; But gradients need to be unbroadcast to original shapes
      (let* ((parents (tape-node-parents node))
             (values (mapcar #'tape-node-value parents))
             (result-val (tape-node-value node)))
        (case (length values)
          (2 (let* ((x-val (first values))
                    (y-val (second values))
                    ;; Gradient is 1 everywhere in the output shape
                    (output-grad (if (arrayp result-val)
                                    (make-array (array-dimensions result-val)
                                               :initial-element 1)
                                    1))
                    ;; Unbroadcast to match input shapes
                    (grad-x (bc:unbroadcast-gradient 
                            output-grad
                            (if (arrayp x-val) 
                                (array-dimensions x-val)
                                nil)))
                    (grad-y (bc:unbroadcast-gradient
                            output-grad
                            (if (arrayp y-val)
                                (array-dimensions y-val)
                                nil))))
               (list (cons (first parents) grad-x)
                     (cons (second parents) grad-y))))
          ;; Handle single argument (shouldn't happen for +)
          (1 (list (cons (first parents) 1)))
          ;; Multiple arguments
          (otherwise 
           (let ((output-grad (if (arrayp result-val)
                                 (make-array (array-dimensions result-val)
                                            :initial-element 1)
                                 1)))
             (mapcar (lambda (parent)
                      (let ((val (tape-node-value parent)))
                        (cons parent
                              (bc:unbroadcast-gradient
                               output-grad
                               (if (arrayp val)
                                   (array-dimensions val)
                                   nil)))))
                    parents)))))))

  ;; Multiplication gradient rule  
  (register-gradient (intern "*" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(x * y) = y, d/dy(x * y) = x
      ;; When inputs are broadcast, gradients must be unbroadcast
      (let* ((parents (tape-node-parents node))
             (values (mapcar #'tape-node-value parents))
             (result-val (tape-node-value node)))
        (case (length values)
          (2 (let* ((x-val (first values))
                    (y-val (second values))
                    ;; For multiplication: df/dx = y, df/dy = x
                    ;; But if broadcasting occurred, we need to broadcast y and x
                    ;; to the output shape first, then unbroadcast back
                    (grad-x-full (cond
                                  ;; Broadcast y to output shape
                                  ((and (arrayp result-val) (arrayp y-val))
                                   (bc:broadcast-to y-val (array-dimensions result-val)))
                                  ((and (arrayp result-val) (numberp y-val))
                                   (make-array (array-dimensions result-val)
                                              :initial-element y-val))
                                  (t y-val)))
                    (grad-y-full (cond
                                  ;; Broadcast x to output shape
                                  ((and (arrayp result-val) (arrayp x-val))
                                   (bc:broadcast-to x-val (array-dimensions result-val)))
                                  ((and (arrayp result-val) (numberp x-val))
                                   (make-array (array-dimensions result-val)
                                              :initial-element x-val))
                                  (t x-val)))
                    ;; Unbroadcast gradients back to input shapes
                    (grad-x (bc:unbroadcast-gradient 
                            grad-x-full
                            (if (arrayp x-val)
                                (array-dimensions x-val)
                                nil)))
                    (grad-y (bc:unbroadcast-gradient
                            grad-y-full
                            (if (arrayp y-val)
                                (array-dimensions y-val)
                                nil))))
               (list (cons (first parents) grad-x)
                     (cons (second parents) grad-y))))
          (otherwise 
           ;; For n-ary multiplication
           (loop for parent in parents
                 for i from 0
                 collect (cons parent 
                              (apply #'* (loop for j from 0
                                             for v in values
                                             when (/= i j)
                                             collect v)))))))))

  ;; Subtraction gradient rule
  (register-gradient (intern "-" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(x - y) = 1, d/dy(x - y) = -1
      ;; With broadcasting support
      (let* ((parents (tape-node-parents node))
             (values (mapcar #'tape-node-value parents))
             (result-val (tape-node-value node)))
        (case (length parents)
          (1 ;; Unary minus: d/dx(-x) = -1
           (let* ((x-val (first values))
                  (grad (if (arrayp result-val)
                           (make-array (array-dimensions result-val)
                                      :initial-element -1)
                           -1)))
             (list (cons (first parents) 
                        (bc:unbroadcast-gradient 
                         grad 
                         (if (arrayp x-val)
                             (array-dimensions x-val)
                             nil))))))
          (2 ;; Binary subtraction: d/dx(x-y) = 1, d/dy(x-y) = -1
           (let* ((x-val (first values))
                  (y-val (second values))
                  (pos-grad (if (arrayp result-val)
                               (make-array (array-dimensions result-val)
                                          :initial-element 1)
                               1))
                  (neg-grad (if (arrayp result-val)
                               (make-array (array-dimensions result-val)
                                          :initial-element -1)
                               -1)))
             (list (cons (first parents)
                        (bc:unbroadcast-gradient 
                         pos-grad
                         (if (arrayp x-val)
                             (array-dimensions x-val)
                             nil)))
                  (cons (second parents)
                        (bc:unbroadcast-gradient
                         neg-grad
                         (if (arrayp y-val)
                             (array-dimensions y-val)
                             nil))))))))))

  ;; Division gradient rule
  (register-gradient (intern "/" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(x / y) = 1/y, d/dy(x / y) = -x/y^2
      ;; With proper broadcasting support
      (let* ((parents (tape-node-parents node))
             (x-val (tape-node-value (first parents)))
             (y-val (tape-node-value (second parents)))
             (result-val (tape-node-value node))
             ;; Compute gradients in output shape, then unbroadcast
             ;; df/dx = 1/y (broadcast y to output shape)
             (grad-x-full (cond
                           ((arrayp result-val)
                            (let ((y-broadcast (if (arrayp y-val)
                                                  (bc:broadcast-to y-val (array-dimensions result-val))
                                                  (make-array (array-dimensions result-val)
                                                             :initial-element y-val))))
                              (bc:broadcast-binary-op #'/ 1 y-broadcast)))
                           (t (/ 1 y-val))))
             ;; df/dy = -x/y^2 (broadcast x and y to output shape)
             (grad-y-full (cond
                           ((arrayp result-val)
                            (let* ((x-broadcast (if (arrayp x-val)
                                                   (bc:broadcast-to x-val (array-dimensions result-val))
                                                   (make-array (array-dimensions result-val)
                                                              :initial-element x-val)))
                                   (y-broadcast (if (arrayp y-val)
                                                   (bc:broadcast-to y-val (array-dimensions result-val))
                                                   (make-array (array-dimensions result-val)
                                                              :initial-element y-val)))
                                   (y-squared (bc:broadcast-binary-op #'* y-broadcast y-broadcast)))
                              (bc:broadcast-binary-op #'- 0 
                                                     (bc:broadcast-binary-op #'/ x-broadcast y-squared))))
                           (t (- (/ x-val (* y-val y-val))))))
             ;; Unbroadcast gradients back to input shapes
             (grad-x (bc:unbroadcast-gradient 
                     grad-x-full
                     (if (arrayp x-val)
                         (array-dimensions x-val)
                         nil)))
             (grad-y (bc:unbroadcast-gradient
                     grad-y-full
                     (if (arrayp y-val)
                         (array-dimensions y-val)
                         nil))))
        (list (cons (first parents) grad-x)
              (cons (second parents) grad-y)))))

  ;; Power gradient rule  
  (register-gradient (intern "^" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(x^y) = y*x^(y-1), d/dy(x^y) = x^y * log(x)
      (let* ((parents (tape-node-parents node))
             (x-val (tape-node-value (first parents)))
             (y-val (tape-node-value (second parents)))
             ;; Compute gradients with broadcasting support
             (grad-x (cond
                      ;; x is array, y is scalar - apply to each element of x
                      ((and (arrayp x-val) (numberp y-val))
                       (map 'vector (lambda (xi) (* y-val (expt xi (- y-val 1)))) x-val))
                      ;; x is scalar, y is array - sum over all y elements
                      ((and (numberp x-val) (arrayp y-val))
                       (reduce #'+ (map 'vector (lambda (yi) (* yi (expt x-val (- yi 1)))) y-val)))
                      ;; Both arrays - element-wise computation
                      ((and (arrayp x-val) (arrayp y-val))
                       (map 'vector (lambda (xi yi) (* yi (expt xi (- yi 1)))) x-val y-val))
                      ;; Both scalars
                      (t (* y-val (expt x-val (- y-val 1))))))
             (grad-y (cond
                      ;; y is scalar, x is array - sum x^y * log(x) over all x
                      ((and (numberp y-val) (arrayp x-val))
                       (reduce #'+ (map 'vector (lambda (xi) (* (expt xi y-val) (log xi))) x-val)))
                      ;; y is array, x is scalar - apply to each element of y
                      ((and (arrayp y-val) (numberp x-val))
                       (make-array (array-dimensions y-val)
                                  :initial-element (* (expt x-val y-val) (log x-val))))
                      ;; Both arrays - element-wise computation
                      ((and (arrayp x-val) (arrayp y-val))
                       (map 'vector (lambda (xi yi) (* (expt xi yi) (log xi))) x-val y-val))
                      ;; Both scalars
                      (t (* (expt x-val y-val) (log x-val))))))
        (list (cons (first parents) grad-x)
              (cons (second parents) grad-y)))))

  ;; Special operations
  (register-gradient 'stop-gradient
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; Stop gradient - no gradient flows through
      (list (cons (first (tape-node-parents node)) 0))))

  ;; Transcendental functions
  (register-gradient (intern "SIN" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(sin(x)) = cos(x)
      (let ((x (tape-node-value (first (tape-node-parents node)))))
        (list (cons (first (tape-node-parents node)) (cos x))))))

  (register-gradient (intern "COS" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(cos(x)) = -sin(x)
      (let ((x (tape-node-value (first (tape-node-parents node)))))
        (list (cons (first (tape-node-parents node)) (- (sin x)))))))

  (register-gradient (intern "EXP" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(exp(x)) = exp(x)
      (list (cons (first (tape-node-parents node)) 
                 (tape-node-value node)))))

  (register-gradient (intern "LOG" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(log(x)) = 1/x
      (let ((x (tape-node-value (first (tape-node-parents node)))))
        (list (cons (first (tape-node-parents node)) (/ 1 x))))))

  (register-gradient (intern "SQRT" "EPSILON.COMPUTE")
    (lambda (node v-adjoints)
      (declare (ignorable v-adjoints))
      ;; d/dx(sqrt(x)) = 1/(2*sqrt(x))
      (list (cons (first (tape-node-parents node))
                 (/ 1 (* 2 (tape-node-value node)))))))
  
  ;; Also register with strings as fallback (using map:get instead of gethash)
  (let ((plus-fn (map:get *gradient-rules* (intern "+" "EPSILON.COMPUTE"))))
    (when plus-fn (register-gradient "+" plus-fn)))
  (let ((mult-fn (map:get *gradient-rules* (intern "*" "EPSILON.COMPUTE"))))
    (when mult-fn (register-gradient "*" mult-fn)))
  (let ((sub-fn (map:get *gradient-rules* (intern "-" "EPSILON.COMPUTE"))))
    (when sub-fn (register-gradient "-" sub-fn)))
  (let ((div-fn (map:get *gradient-rules* (intern "/" "EPSILON.COMPUTE"))))
    (when div-fn (register-gradient "/" div-fn)))
  (let ((pow-fn (map:get *gradient-rules* (intern "^" "EPSILON.COMPUTE"))))
    (when pow-fn (register-gradient "^" pow-fn)))
  
  ;; Mark as initialized
  t)

;;; Tape construction

(defun build-tape (expr bindings)
  "Build computation tape from expression"
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
  (let* ((grad-fn (get-gradient-fn op))
         (node (make-tape-node
                :id (length (tape-nodes *current-tape*))
                :op op
                :value value
                :parents parents
                :grad-fn grad-fn)))
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
      ((string= op-name "+") (lambda (&rest args) 
                                (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                    (apply (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:+ args)
                                    (apply #'cl:+ args))))
      ((string= op-name "-") (lambda (&rest args) 
                                (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                    (apply (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:- args)
                                    (apply #'cl:- args))))
      ((string= op-name "*") (lambda (&rest args) 
                                (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                    (apply (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:* args)
                                    (apply #'cl:* args))))
      ((string= op-name "/") (lambda (&rest args) 
                                (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                    (apply (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:/ args)
                                    (apply #'cl:/ args))))
      ((string= op-name "^") (lambda (&rest args) 
                                (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                    (apply (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'expt args)
                                    (apply #'expt args))))
      ((string= op-name "SIN") (lambda (x) 
                                  (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                      (funcall (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:sin x)
                                      (cl:sin x))))
      ((string= op-name "COS") (lambda (x) 
                                  (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                      (funcall (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:cos x)
                                      (cl:cos x))))
      ((string= op-name "TAN") (lambda (x) 
                                  (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                      (funcall (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:tan x)
                                      (cl:tan x))))
      ((string= op-name "EXP") (lambda (x) 
                                  (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                      (funcall (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:exp x)
                                      (cl:exp x))))
      ((string= op-name "LOG") (lambda (x) 
                                  (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                      (funcall (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:log x)
                                      (cl:log x))))
      ((string= op-name "SQRT") (lambda (x) 
                                   (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                       (funcall (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:sqrt x)
                                       (cl:sqrt x))))
      ((string= op-name "ABS") (lambda (x) 
                                  (if (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")
                                      (funcall (symbol-function (find-symbol "BROADCAST-OPERATION" "EPSILON.COMPUTE")) #'cl:abs x)
                                      (cl:abs x))))
      ((string= op-name "SUM") (lambda (x)
                                  (if (arrayp x)
                                      (reduce #'+ (make-array (array-total-size x) :displaced-to x))
                                      x)))
      ((string= op-name "STOP-GRADIENT") #'identity) ; Pass through value
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
                       (or (tape-node-adjoint node) 0)))
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
          ;; Multiply adjoint with local gradient using broadcasting
          (let ((grad-contrib 
                 (cond
                   ;; Both arrays - use broadcast-binary-op
                   ((and (arrayp adjoint) (arrayp grad))
                    (bc:broadcast-binary-op #'* adjoint grad))
                   ;; adjoint is array, grad is scalar
                   ((and (arrayp adjoint) (numberp grad))
                    (bc:broadcast-binary-op #'* adjoint grad))
                   ;; adjoint is scalar, grad is array
                   ((and (numberp adjoint) (arrayp grad))
                    (bc:broadcast-binary-op #'* adjoint grad))
                   ;; Both scalars
                   (t (* adjoint grad)))))
            ;; Accumulate gradient using broadcast-aware addition
            (setf (tape-node-adjoint parent)
                  (let ((current (or (tape-node-adjoint parent) 0)))
                    (cond
                      ;; Both arrays
                      ((and (arrayp current) (arrayp grad-contrib))
                       (bc:broadcast-binary-op #'+ current grad-contrib))
                      ;; current is array, grad-contrib is scalar
                      ((and (arrayp current) (numberp grad-contrib))
                       (bc:broadcast-binary-op #'+ current grad-contrib))
                      ;; current is scalar, grad-contrib is array
                      ((and (numberp current) (arrayp grad-contrib))
                       (bc:broadcast-binary-op #'+ current grad-contrib))
                      ;; Both scalars
                      (t (+ current grad-contrib)))))))))))

;;; Main interface

(defun reverse-diff (expr var-names bindings &key 
                    (clip-norm *gradient-clip-norm*)
                    (clip-value *gradient-clip-value*)
                    (handle-nan *handle-nan*))
  "Compute reverse-mode gradients"
  ;; Ensure gradient rules are initialized
  (ensure-gradient-rules)
  (let* ((*gradient-clip-norm* clip-norm)
         (*gradient-clip-value* clip-value)
         (*handle-nan* handle-nan)
         (tape (build-tape expr bindings))
         (grad-table (backward tape)))
    ;; Extract gradients in order
    (mapcar (lambda (var)
              (let ((grad (gethash var grad-table 0)))
                ;; Handle NaN - check for actual NaN values, not just non-numbers
                (when (and handle-nan
                          (or (and (numberp grad) 
                                   (not (= grad grad))) ; NaN test for numbers
                              (and (arrayp grad)
                                   (some (lambda (x) (and (numberp x) (not (= x x))))
                                         (make-array (array-total-size grad) 
                                                    :displaced-to grad)))))
                  (setf grad (case handle-nan
                              (:zero (if (arrayp grad)
                                        (make-array (array-dimensions grad) 
                                                   :initial-element 0)
                                        0))
                              (:warn (warn "NaN gradient for ~A" var) 
                                    (if (arrayp grad)
                                        (make-array (array-dimensions grad) 
                                                   :initial-element 0)
                                        0))
                              (:error (error "NaN gradient for ~A" var)))))
                ;; Apply clipping to arrays or scalars
                (when clip-value
                  (setf grad (if (arrayp grad)
                                (let ((result (make-array (array-dimensions grad))))
                                  (dotimes (i (array-total-size grad))
                                    (setf (row-major-aref result i)
                                          (max (- clip-value) 
                                               (min clip-value 
                                                    (row-major-aref grad i)))))
                                  result)
                                (max (- clip-value) (min clip-value grad)))))
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
  ;; VJP = v^T * J where J is the Jacobian
  ;; Compute as sum of v[i] * grad(f[i])
  (let* ((n-vars (length var-names))
         (result (make-array n-vars :initial-element 0)))
    (loop for i from 0
          for expr in exprs
          for weight = (aref v i)
          do (let ((grad (reverse-diff expr var-names bindings)))
               (loop for j from 0 below n-vars
                     do (incf (aref result j)
                              (* weight (nth j grad))))))
    result))

(defun register-vjp-rule (op rule-fn)
  "Register custom VJP rule for an operation"
  (setf (gethash op *vjp-rules*) rule-fn))

(defun get-vjp-rule (op)
  "Get custom VJP rule for an operation"
  (gethash op *vjp-rules*))

;;; Sparse gradients


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
  "Create a stop-gradient operation that prevents gradient flow"
  (sym:make-expr :op 'stop-gradient :args (list expr)))

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

;;; Reverse-mode differentiation implementation

