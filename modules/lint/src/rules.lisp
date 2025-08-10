;;;; Core linting rules for Epsilon code
;;;;
;;;; This module implements the essential linting rules for Epsilon
;;;; coding standards and best practices.

(defpackage :epsilon.lint.rules
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:seq #:epsilon.sequence)
   (#:parser #:epsilon.lint.parser))
  (:export
   #:lint-issue
   #:issue-type
   #:issue-message
   #:issue-line
   #:issue-column
   #:issue-filename
   #:issue-severity
   #:lint-file
   #:*enabled-rules*
   #:check-header-comment
   #:check-package-structure))

(in-package :epsilon.lint.rules)

;;; Issue reporting structure

(defclass lint-issue ()
  ((type :initarg :type :accessor issue-type)
   (message :initarg :message :accessor issue-message)
   (line :initarg :line :accessor issue-line)
   (column :initarg :column :accessor issue-column)
   (filename :initarg :filename :accessor issue-filename)
   (severity :initarg :severity :accessor issue-severity :initform :warning)))

(defun make-issue (type message line column filename &key (severity :warning))
  "Create a lint issue"
  (make-instance 'lint-issue
                 :type type
                 :message message
                 :line line
                 :column column
                 :filename filename
                 :severity severity))

;;; Rule configuration

(defparameter *enabled-rules* 
  '(:header-comment :indentation :line-length :trailing-whitespace 
    :package-structure :export-consistency)
  "List of enabled linting rules")

;;; Utility functions for analysis

(defun read-file-lines (filename)
  "Read file and return list of lines"
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun count-leading-spaces (line)
  "Count leading spaces in a line"
  (let ((count 0))
    (loop for char across line
          while (char= char #\Space)
          do (incf count))
    count))

(defun has-trailing-whitespace-p (line)
  "Check if line has trailing whitespace"
  (and (> (length line) 0)
       (member (char line (1- (length line))) '(#\Space #\Tab))))

;;; Individual linting rules

(defun check-header-comment (file-ast)
  "Check for proper header comment at top of file"
  (let ((issues '())
        (filename (parser:file-ast-filename file-ast))
        (comments (parser:file-ast-comments file-ast)))
    
    ;; Check if first line is a comment
    (unless (and comments
                 (= (parser:comment-line (first comments)) 1)
                 (>= (parser:comment-level (first comments)) 3))
      (push (make-issue :header-comment
                        "File should start with a header comment (;;;; or more)"
                        1 1 filename :severity :info)
            issues))
    
    issues))

;; NOTE: Indentation checking moved to text-based version

;; NOTE: Line length checking moved to text-based version

;; NOTE: Trailing whitespace checking moved to text-based version

(defun check-package-structure (file-ast)
  "Check package definition structure"
  (let ((issues '())
        (filename (parser:file-ast-filename file-ast))
        (nodes (parser:file-ast-nodes file-ast)))
    
    ;; Find defpackage form
    (let ((defpackage-node (find-if (lambda (node)
                                      (eq (parser:ast-node-type node) :defpackage))
                                    nodes)))
      (when defpackage-node
        (let ((form (parser:ast-node-form defpackage-node)))
          ;; Check basic structure
          (unless (and (listp form)
                       (eq (first form) 'defpackage)
                       (>= (length form) 2)
                       (or (stringp (second form))
                           (keywordp (second form))))
            (push (make-issue :package-structure
                              "Invalid defpackage structure"
                              (parser:ast-node-line defpackage-node)
                              (parser:ast-node-column defpackage-node)
                              filename :severity :error)
                  issues))
          
          ;; Check for required sections
          (let ((has-use nil)
                (has-export nil))
            (dolist (clause (cddr form))
              (when (listp clause)
                (case (first clause)
                  (:use (setf has-use t))
                  (:export (setf has-export t)))))
            
            (unless has-use
              (push (make-issue :package-structure
                                "Package missing :use clause"
                                (parser:ast-node-line defpackage-node)
                                (parser:ast-node-column defpackage-node)
                                filename :severity :warning)
                    issues))
            
            (unless has-export
              (push (make-issue :package-structure
                                "Package missing :export clause"
                                (parser:ast-node-line defpackage-node)
                                (parser:ast-node-column defpackage-node)
                                filename :severity :info)
                    issues))))))
    
    issues))

(defun check-export-consistency (file-ast)
  "Check that exported symbols are actually defined"
  (let ((issues '())
        (filename (parser:file-ast-filename file-ast))
        (nodes (parser:file-ast-nodes file-ast)))
    
    ;; Find exported symbols
    (let ((exported-symbols '())
          (defined-symbols '()))
      
      ;; Extract exports from defpackage
      (dolist (node nodes)
        (when (eq (parser:ast-node-type node) :defpackage)
          (let ((form (parser:ast-node-form node)))
            (dolist (clause (cddr form))
              (when (and (listp clause) (eq (first clause) :export))
                (dolist (symbol (rest clause))
                  (when (or (symbolp symbol) (stringp symbol))
                    (push symbol exported-symbols))))))))
      
      ;; Extract defined symbols
      (dolist (node nodes)
        (let ((form (parser:ast-node-form node)))
          (when (and (listp form) (>= (length form) 2))
            (case (first form)
              ((defun defmacro defgeneric)
               (push (second form) defined-symbols))
              ((defvar defparameter defconstant)
               (push (second form) defined-symbols))
              (defclass
                (push (second form) defined-symbols))
              (defmethod
                (push (second form) defined-symbols))))))
      
      ;; Check for exported but undefined symbols
      (dolist (exported exported-symbols)
        (let ((symbol-name (if (stringp exported)
                               (intern exported)
                               exported)))
          (unless (member symbol-name defined-symbols)
            (push (make-issue :export-consistency
                              (format nil "Exported symbol ~A not defined in file" symbol-name)
                              1 1 filename :severity :warning)
                  issues)))))
    
    issues))

;;; Text-based rules (work without full parsing)

(defun check-indentation-text-based (filename)
  "Check indentation using text-based analysis"
  (let ((issues '())
        (lines (read-file-lines filename)))
    
    (loop for line in lines
          for line-num from 1
          do (let ((leading-spaces (count-leading-spaces line)))
               ;; Only check non-empty lines that aren't comments
               (when (and (> (length (string-trim " " line)) 0)
                          (not (char= (char (string-left-trim " " line) 0) #\;)))
                 ;; Check if indentation is multiple of 2
                 (unless (zerop (mod leading-spaces 2))
                   (push (make-issue :indentation
                                     (format nil "Inconsistent indentation: ~D spaces (expected multiple of 2)"
                                             leading-spaces)
                                     line-num 1 filename :severity :error)
                         issues)))))
    
    issues))

(defun check-line-length-text-based (filename)
  "Check line length using text-based analysis"
  (let ((issues '())
        (lines (read-file-lines filename))
        (max-length 100))
    
    (loop for line in lines
          for line-num from 1
          when (> (length line) max-length)
          do (push (make-issue :line-length
                               (format nil "Line exceeds ~D characters (~D)"
                                       max-length (length line))
                               line-num (1+ max-length) filename :severity :warning)
                   issues))
    
    issues))

(defun check-trailing-whitespace-text-based (filename)
  "Check trailing whitespace using text-based analysis"
  (let ((issues '())
        (lines (read-file-lines filename)))
    
    (loop for line in lines
          for line-num from 1
          when (has-trailing-whitespace-p line)
          do (push (make-issue :trailing-whitespace
                               "Line has trailing whitespace"
                               line-num (length line) filename :severity :info)
                   issues))
    
    issues))

;;; Main linting function

(defun lint-file (filename)
  "Lint a single file and return list of issues"
  (handler-case
      (let ((file-ast (parser:parse-file-with-comments filename))
            (all-issues '()))
        
        ;; Run all enabled rules that require AST parsing
        (when (member :header-comment *enabled-rules*)
          (setf all-issues (append (check-header-comment file-ast) all-issues)))
        
        (when (member :package-structure *enabled-rules*)
          (setf all-issues (append (check-package-structure file-ast) all-issues)))
        
        (when (member :export-consistency *enabled-rules*)
          (setf all-issues (append (check-export-consistency file-ast) all-issues)))
        
        ;; Run text-based rules (these work even with parsing errors)
        (when (member :indentation *enabled-rules*)
          (setf all-issues (append (check-indentation-text-based filename) all-issues)))
        
        (when (member :line-length *enabled-rules*)
          (setf all-issues (append (check-line-length-text-based filename) all-issues)))
        
        (when (member :trailing-whitespace *enabled-rules*)
          (setf all-issues (append (check-trailing-whitespace-text-based filename) all-issues)))
        
        ;; Sort issues by line number
        (sort all-issues (lambda (a b)
                           (< (issue-line a) (issue-line b)))))
    
    (error (e)
      ;; If parsing fails, run only text-based rules
      (let ((all-issues (list (make-issue :parse-error
                                          (format nil "Could not fully parse file (running text-based rules only): ~A" e)
                                          1 1 filename :severity :warning))))
        
        ;; Run text-based rules even when parsing fails
        (when (member :indentation *enabled-rules*)
          (setf all-issues (append (check-indentation-text-based filename) all-issues)))
        
        (when (member :line-length *enabled-rules*)
          (setf all-issues (append (check-line-length-text-based filename) all-issues)))
        
        (when (member :trailing-whitespace *enabled-rules*)
          (setf all-issues (append (check-trailing-whitespace-text-based filename) all-issues)))
        
        ;; Sort issues by line number
        (sort all-issues (lambda (a b)
                           (< (issue-line a) (issue-line b))))))))