;;;; Automatic Differentiation - Main Interface
;;;;
;;;; This module provides the main interface for automatic differentiation,
;;;; delegating to specialized implementations for forward and reverse mode.

(defpackage epsilon.compute.autodiff
  (:use :cl)
  (:local-nicknames
   (rev epsilon.compute.reverse-autodiff)
   (sym epsilon.compute.symbolic))
  (:export
   ;; Forward mode
   :forward-diff
   
   ;; Reverse mode
   :gradient
   :jacobian
   :hessian
   :reverse-diff
   
   ;; Utilities
   :with-autodiff
   :jacobian-forward
   :jacobian-reverse
   
   ;; Tape interface (re-export from reverse-autodiff)
   :build-tape
   :tape-p
   :tape-node-count
   :tape-nodes
   :tape-output-value
   :tape-node-p
   :tape-node-value
   
   ;; Advanced features
   :reverse-diff-with-checkpoints
   :reverse-diff-symbolic
   :sparse-gradient
   :sparse-gradient-p
   :sparse-gradient-get
   :sparse-gradient-nnz
   :vector-jacobian-product
   :register-vjp-rule
   :stop-gradient
   :checkpoints-used-p
   :tape-memory-usage
   :peak-tape-memory
   :get-current-tape-memory
   :get-current-peak-memory
   :hessian-mixed-mode))

(in-package epsilon.compute.autodiff)

;;; Forward to implementations

(defun forward-diff (expr var-name &optional (seed 1))
  "Compute forward-mode derivative"
  ;; Forward-mode differentiation - use symbolic differentiation for now
  ;; Will be replaced with dual number implementation later
  (declare (ignore seed))
  ;; Call the differentiate function which should be loaded
  (funcall (find-symbol "DIFF" "EPSILON.COMPUTE") expr var-name))

(defun gradient (expr var-names bindings)
  "Compute gradient using reverse-mode"
  (rev:gradient expr var-names bindings))

(defun reverse-diff (expr var-names bindings &rest args)
  "Reverse-mode derivatives"
  (apply #'rev:reverse-diff expr var-names bindings args))

(defun jacobian (exprs var-names bindings &key (mode :auto))
  "Compute Jacobian matrix"
  (let ((exprs-list (if (listp exprs) exprs (list exprs))))
    (case mode
      (:forward (jacobian-forward exprs-list var-names bindings))
      (:reverse (jacobian-reverse exprs-list var-names bindings))
      (:auto 
       ;; Choose based on dimensions
       (if (> (length exprs-list) (length var-names))
           (jacobian-reverse exprs-list var-names bindings)
           (jacobian-forward exprs-list var-names bindings))))))

(defun jacobian-forward (exprs var-names bindings)
  "Forward-mode Jacobian computation"
  (let* ((m (length exprs))
         (n (length var-names))
         (jac (make-array (list m n))))
    ;; Compute each column using forward mode
    (loop for j from 0 below n
          for var = (nth j var-names)
          do (loop for i from 0 below m
                   for expr = (nth i exprs)
                   do (setf (aref jac i j)
                           (forward-diff expr var 1))))
    jac))

(defun jacobian-reverse (exprs var-names bindings)
  "Reverse-mode Jacobian computation"
  (let* ((m (length exprs))
         (n (length var-names))
         (jac (make-array (list m n))))
    ;; Compute each row using reverse mode
    (loop for i from 0 below m
          for expr = (nth i exprs)
          for grad = (rev:reverse-diff expr var-names bindings)
          do (loop for j from 0 below n
                   do (setf (aref jac i j) (nth j grad))))
    jac))

(defun hessian (expr var-names bindings)
  "Compute Hessian matrix"
  ;; Use mixed-mode for efficiency
  (rev:hessian-mixed-mode expr var-names bindings))

(defmacro with-autodiff ((&key (mode :reverse)) &body body)
  "Execute with autodiff context"
  `(let ((*autodiff-mode* ,mode))
     ,@body))

;;; Re-export reverse-mode functions

(defun build-tape (expr bindings)
  "Build computation tape for expression"
  (rev:build-tape expr bindings))

(defun tape-p (obj)
  "Check if object is a tape"
  (rev:tape-p obj))

(defun tape-node-count (tape)
  "Get number of nodes in tape"
  (length (rev:tape-nodes tape)))

(defun tape-nodes (tape)
  "Get nodes from tape"
  (rev:tape-nodes tape))

(defun tape-output-value (tape)
  "Get output value from tape"
  (rev:tape-node-value (rev:tape-output-node tape)))

(defun tape-node-p (obj)
  "Check if object is a tape node"
  (rev:tape-node-p obj))

(defun tape-node-value (node)
  "Get value from tape node"
  (rev:tape-node-value node))

(defun reverse-diff-with-checkpoints (expr var-names bindings &rest args)
  "Reverse diff with checkpointing"
  (apply #'rev:reverse-diff-with-checkpoints expr var-names bindings args))

(defun reverse-diff-symbolic (expr var)
  "Symbolic reverse differentiation"
  (rev:reverse-diff-symbolic expr var))

(defun sparse-gradient (expr var-names bindings)
  "Compute sparse gradient"
  (rev:sparse-gradient expr var-names bindings))

(defun sparse-gradient-p (obj)
  "Check if object is sparse gradient"
  (rev:sparse-gradient-p obj))

(defun sparse-gradient-get (grad index)
  "Get value from sparse gradient"
  (rev:sparse-gradient-get grad index))

(defun sparse-gradient-nnz (grad)
  "Get number of nonzeros in sparse gradient"
  (rev:sparse-gradient-nnz grad))

(defun vector-jacobian-product (exprs var-names bindings v)
  "Compute vector-Jacobian product"
  (rev:vector-jacobian-product exprs var-names bindings v))

(defun register-vjp-rule (op rule-fn)
  "Register custom VJP rule"
  (rev:register-vjp-rule op rule-fn))

(defun stop-gradient (expr)
  "Stop gradient flow"
  (rev:stop-gradient expr))

(defun checkpoints-used-p ()
  "Check if checkpoints were used"
  (rev:checkpoints-used-p))

(defun tape-memory-usage (&optional tape)
  "Get tape memory usage"
  (if tape
      (rev:tape-memory-usage tape)
      (rev:get-current-tape-memory)))

(defun peak-tape-memory (&optional tape)
  "Get peak tape memory"
  (if tape
      (rev:tape-peak-memory tape)
      (rev:get-current-peak-memory)))

(defun get-current-tape-memory ()
  "Get current tape memory usage"
  (rev:get-current-tape-memory))

(defun get-current-peak-memory ()
  "Get current peak memory usage"
  (rev:get-current-peak-memory))

(defun hessian-mixed-mode (expr var-names bindings)
  "Compute Hessian using mixed mode"
  (rev:hessian-mixed-mode expr var-names bindings))



(defparameter *autodiff-mode* :reverse
  "Current autodiff mode")

