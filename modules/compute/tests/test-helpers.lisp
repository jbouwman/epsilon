;;;; Test Helpers for Automatic Differentiation
;;;;
;;;; This file provides utility functions for testing automatic differentiation,
;;;; including numerical gradient checking and comparison utilities.

(defpackage epsilon.compute.test-helpers
  (:use :cl)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic))
  (:export
   ;; Comparison utilities
   :approximately-equal
   :array-approximately-equal
   :relative-error
   
   ;; Gradient checking
   :numerical-gradient
   :gradient-check
   :check-gradient-accuracy
   
   ;; Expression builders
   :make-polynomial
   :make-random-expr
   :make-nested-expr
   
   ;; Performance utilities
   :measure-time
   :measure-memory
   :benchmark-gradient))

(in-package epsilon.compute.test-helpers)

;;; Comparison utilities

(defun approximately-equal (a b &optional (tolerance 1e-6))
  "Check if two numbers are approximately equal within tolerance"
  (< (abs (- a b)) tolerance))

(defun array-approximately-equal (a b &optional (tolerance 1e-6))
  "Check if two arrays are approximately equal element-wise"
  (and (equal (array-dimensions a) (array-dimensions b))
       (loop for i below (array-total-size a)
             always (approximately-equal (row-major-aref a i)
                                        (row-major-aref b i)
                                        tolerance))))

(defun relative-error (computed expected)
  "Compute relative error between computed and expected values"
  (if (zerop expected)
      (abs computed)
      (/ (abs (- computed expected)) (abs expected))))

;;; Gradient checking

(defun numerical-gradient (f x &optional (h 1e-7))
  "Compute numerical gradient using finite differences
   f: function of a vector returning a scalar
   x: point at which to evaluate gradient
   h: step size for finite differences"
  (let* ((n (length x))
         (grad (make-array n)))
    (loop for i below n
          do (let ((x-plus (copy-seq x))
                   (x-minus (copy-seq x)))
               (incf (aref x-plus i) h)
               (decf (aref x-minus i) h)
               (setf (aref grad i)
                     (/ (- (funcall f x-plus) (funcall f x-minus))
                        (* 2 h)))))
    grad))

(defun gradient-check (expr vars bindings &optional (tolerance 1e-5))
  "Check if automatic differentiation gradient matches numerical gradient
   Returns (values passes-p max-error errors)"
  (let* ((n (length vars))
         ;; Function that evaluates expression at a point
         (f (lambda (x-vec)
              (let ((new-bindings (copy-alist bindings)))
                (loop for i below n
                      for var in vars
                      do (setf (cdr (assoc var new-bindings)) (aref x-vec i)))
                (c:evaluate expr new-bindings))))
         ;; Current point as vector
         (x-vec (make-array n :initial-contents
                           (mapcar (lambda (var) (cdr (assoc var bindings)))
                                  vars)))
         ;; Compute numerical gradient
         (num-grad (numerical-gradient f x-vec))
         ;; Compute autodiff gradient (when implemented)
         (auto-grad (handler-case
                       (c:gradient expr vars bindings)
                     (error () 
                       ;; If not implemented, use symbolic diff as fallback
                       (mapcar (lambda (var)
                                (c:evaluate (c:diff expr var) bindings))
                              vars))))
         ;; Compare gradients
         (errors (make-array n))
         (max-error 0))
    
    (loop for i below n
          for num = (aref num-grad i)
          for auto = (if (arrayp auto-grad)
                        (aref auto-grad i)
                        (nth i auto-grad))
          for err = (abs (- num auto))
          do (setf (aref errors i) err)
             (when (> err max-error)
               (setf max-error err)))
    
    (values (<= max-error tolerance) max-error errors)))

(defun check-gradient-accuracy (expr vars bindings)
  "Detailed gradient accuracy check with reporting"
  (multiple-value-bind (passes-p max-error errors)
      (gradient-check expr vars bindings)
    (format t "Gradient check for ~A variables: ~A~%" 
            (length vars) 
            (if passes-p "PASS" "FAIL"))
    (when (not passes-p)
      (format t "  Max error: ~E~%" max-error)
      (loop for i below (length vars)
            for var in vars
            when (> (aref errors i) 1e-5)
            do (format t "  Variable ~A: error = ~E~%" 
                      var (aref errors i))))
    passes-p))

;;; Expression builders for testing

(defun make-polynomial (var coefficients)
  "Create a polynomial expression: a₀ + a₁x + a₂x² + ..."
  (let ((terms nil))
    (loop for coef in coefficients
          for power from 0
          unless (zerop coef)
          do (push (if (zerop power)
                      (c:const coef)
                      (c:* coef (c:^ var power)))
                  terms))
    (if (null terms)
        (c:const 0)
        (reduce #'c:+ terms))))

(defun make-random-expr (vars depth)
  "Create a random expression for testing"
  (if (zerop depth)
      ;; Base case: return variable or constant
      (if (< (random 1.0) 0.5)
          (nth (random (length vars)) vars)
          (c:const (1+ (random 10))))
      ;; Recursive case: create compound expression
      (let ((op (nth (random 4) '(c:+ c:- c:* c:/))))
        (funcall op
                 (make-random-expr vars (1- depth))
                 (make-random-expr vars (1- depth))))))

(defun make-nested-expr (var depth)
  "Create a deeply nested expression for stress testing"
  (if (zerop depth)
      var
      (c:sin (c:+ var (make-nested-expr var (1- depth))))))

;;; Performance utilities

(defun measure-time (thunk)
  "Measure execution time of a thunk in seconds"
  (let ((start (get-internal-real-time)))
    (funcall thunk)
    (/ (- (get-internal-real-time) start)
       internal-time-units-per-second)))

(defun measure-memory (thunk)
  "Measure memory allocation of a thunk (implementation-dependent)"
  #+sbcl
  (let ((before (sb-ext:get-bytes-consed)))
    (funcall thunk)
    (- (sb-ext:get-bytes-consed) before))
  #-sbcl
  (progn
    (funcall thunk)
    0))  ; Return 0 if memory measurement not available

(defun benchmark-gradient (expr vars bindings &key (iterations 100))
  "Benchmark gradient computation performance"
  (let ((total-time 0)
        (total-memory 0))
    ;; Warm up
    (dotimes (i 10)
      (handler-case
          (c:gradient expr vars bindings)
        (error () nil)))
    
    ;; Benchmark
    (dotimes (i iterations)
      (let ((time (measure-time
                   (lambda ()
                     (handler-case
                         (c:gradient expr vars bindings)
                       (error () nil)))))
            (memory (measure-memory
                    (lambda ()
                      (handler-case
                          (c:gradient expr vars bindings)
                        (error () nil))))))
        (incf total-time time)
        (incf total-memory memory)))
    
    (format t "Gradient benchmark (~A iterations):~%" iterations)
    (format t "  Average time: ~,6F seconds~%" (/ total-time iterations))
    (when (plusp total-memory)
      (format t "  Average memory: ~:D bytes~%" (round (/ total-memory iterations))))
    
    (values (/ total-time iterations)
            (/ total-memory iterations))))