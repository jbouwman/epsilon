(defpackage #:epsilon.frame.compute
  (:use #:cl)
  (:local-nicknames
   (#:dtype #:epsilon.frame.dtype)
   (#:col #:epsilon.frame.column)
   (#:ops #:epsilon.frame.ops)
   (#:frame #:epsilon.frame))
  (:export
   ;; Main compute function
   #:compute
   #:compute*
   
   ;; Aggregation
   #:agg
   #:aggregate
   
   ;; Frame context
   #:make-frame-context
   #:with-frame-context))

(in-package #:epsilon.frame.compute)

(defun make-frame-context (frame-obj)
  "Create an evaluation context from a frame"
  (frame::frame-columns frame-obj))

(defmacro with-frame-context ((context-var frame) &body body)
  "Bind a frame context for expression evaluation"
  `(let ((,context-var (make-frame-context ,frame)))
     ,@body))

(defun compute (frame-obj name expression)
  "Add a computed column to a frame based on an expression"
  (with-frame-context (context frame-obj)
    (let ((result-column (ops:evaluate-expression expression context)))
      (frame:add-column frame-obj name result-column))))

(defun compute* (frame &rest name-expr-pairs)
  "Add multiple computed columns to a frame"
  (let ((result frame))
    (loop for (name expr) on name-expr-pairs by #'cddr
          do (setf result (compute result name expr)))
    result))

(defun agg (frame-obj &rest name-expr-pairs)
  "Aggregate a frame, returning a new single-row frame"
  (with-frame-context (context frame-obj)
    (let ((col-specs nil))
      (loop for (name expr) on name-expr-pairs by #'cddr
            do (let ((result (ops:evaluate-expression expr context)))
                 (let ((value (if (col:column-p result)
                                 (col:column-get result 0)
                                 result)))
                   (setf col-specs (append col-specs (list name (list value)))))))
      (apply #'frame:frame col-specs))))

(defun aggregate (frame expressions)
  "Aggregate a frame using a list of (name . expression) pairs"
  (apply #'agg frame 
         (loop for (name . expr) in expressions
               append (list name expr))))