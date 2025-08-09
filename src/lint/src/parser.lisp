;;;; Comment-aware Lisp parser for linting
;;;;
;;;; This parser extends the standard Lisp reader to track comments
;;;; and their locations for comprehensive code analysis.

(defpackage :epsilon.lint.parser
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:seq #:epsilon.sequence))
  (:export
   #:parse-file-with-comments
   #:comment
   #:comment-text
   #:comment-line
   #:comment-column
   #:comment-level
   #:ast-node
   #:ast-node-form
   #:ast-node-line
   #:ast-node-column
   #:ast-node-comments
   #:ast-node-type
   #:file-ast
   #:file-ast-nodes
   #:file-ast-comments
   #:file-ast-filename
   #:find-comments-for-form))

(in-package :epsilon.lint.parser)

;;; Data structures for representing parsed code with comments

(defclass comment ()
  ((text :initarg :text :accessor comment-text)
   (line :initarg :line :accessor comment-line)
   (column :initarg :column :accessor comment-column)
   (level :initarg :level :accessor comment-level :initform 1)))

(defclass ast-node ()
  ((form :initarg :form :accessor ast-node-form)
   (line :initarg :line :accessor ast-node-line)
   (column :initarg :column :accessor ast-node-column)
   (comments :initarg :comments :accessor ast-node-comments :initform nil)))

(defclass file-ast ()
  ((filename :initarg :filename :accessor file-ast-filename)
   (nodes :initarg :nodes :accessor file-ast-nodes :initform nil)
   (comments :initarg :comments :accessor file-ast-comments :initform nil)))

;;; Comment parsing utilities

(defun parse-comment-level (comment-text)
  "Determine comment level based on number of semicolons"
  (let ((level 0))
    (loop for char across comment-text
          while (char= char #\;)
          do (incf level))
    level))

(defun parse-comment-line (line line-number)
  "Parse a single line for comments, returning comment objects"
  (let ((comments '())
        (pos 0))
    (loop while (< pos (length line))
          for semi-pos = (position #\; line :start pos)
          when semi-pos
          do (let* ((comment-start semi-pos)
                    (comment-text (string-trim " " (subseq line semi-pos)))
                    (level (parse-comment-level comment-text))
                    (comment (make-instance 'comment
                                            :text comment-text
                                            :line line-number
                                            :column comment-start
                                            :level level)))
               (push comment comments)
               (setf pos (length line))))
    (nreverse comments)))

;;; Source location tracking reader

(defvar *current-line* 1)
(defvar *current-column* 0)
(defvar *source-comments* nil)

(defun tracking-read-char (stream)
  "Read a character while tracking line/column position"
  (let ((char (read-char stream nil nil)))
    (when char
      (if (char= char #\Newline)
          (progn
            (incf *current-line*)
            (setf *current-column* 0))
          (incf *current-column*)))
    char))

(defun skip-whitespace (stream)
  "Skip whitespace characters while tracking position"
  (loop for char = (peek-char nil stream nil nil)
        while (and char (member char '(#\Space #\Tab #\Newline #\Return)))
        do (tracking-read-char stream)))

(defun read-comment (stream)
  "Read a comment from current position to end of line"
  (let ((start-line *current-line*)
        (start-column *current-column*)
        (comment-text (make-string-output-stream)))
    ;; Read until end of line
    (loop for char = (peek-char nil stream nil nil)
          while (and char (not (char= char #\Newline)))
          do (write-char (tracking-read-char stream) comment-text))
    
    (let* ((text (get-output-stream-string comment-text))
           (level (parse-comment-level text))
           (comment (make-instance 'comment
                                   :text text
                                   :line start-line
                                   :column start-column
                                   :level level)))
      (push comment *source-comments*)
      comment)))

(defun tracking-read (stream)
  "Read an S-expression while tracking position and comments"
  (skip-whitespace stream)
  
  (let ((char (peek-char nil stream nil nil)))
    (cond
      ;; EOF
      ((null char) nil)
      
      ;; Comment
      ((char= char #\;)
       (read-comment stream)
       ;; Skip to next form
       (tracking-read stream))
      
      ;; Regular S-expression
      (t
       (let ((start-line *current-line*)
             (start-column *current-column*)
             (form (read stream nil nil)))
         (when form
           (make-instance 'ast-node
                          :form form
                          :line start-line
                          :column start-column)))))))

;;; Main parsing function

(defun parse-file-with-comments (filename)
  "Parse a Lisp file, extracting both code and comments with positions"
  (with-open-file (stream filename :direction :input)
    (let ((*current-line* 1)
          (*current-column* 0)
          (*source-comments* nil)
          (nodes nil))
      
      ;; Parse all forms and comments
      (loop for node = (tracking-read stream)
            while node
            when (typep node 'ast-node)
            do (push node nodes))
      
      ;; Create file AST
      (make-instance 'file-ast
                     :filename filename
                     :nodes (nreverse nodes)
                     :comments (nreverse *source-comments*)))))

;;; Utility functions for AST traversal

(defun find-comments-for-form (ast-node file-ast)
  "Find comments that are associated with a specific form"
  (let ((form-line (ast-node-line ast-node))
        (associated-comments nil))
    
    ;; Find comments on the same line or immediately preceding
    (dolist (comment (file-ast-comments file-ast))
      (let ((comment-line (comment-line comment)))
        (when (or (= comment-line form-line)
                  (= comment-line (1- form-line)))
          (push comment associated-comments))))
    
    (nreverse associated-comments)))

(defun ast-node-type (node)
  "Determine the type of AST node based on its form"
  (let ((form (ast-node-form node)))
    (cond
      ((and (listp form) (symbolp (first form)))
       (case (first form)
         (defpackage :defpackage)
         (defun :defun)
         (defmacro :defmacro)
         (defvar :defvar)
         (defparameter :defparameter)
         (defconstant :defconstant)
         (defclass :defclass)
         (defmethod :defmethod)
         (defgeneric :defgeneric)
         (in-package :in-package)
         (otherwise :list)))
      ((listp form) :list)
      ((symbolp form) :symbol)
      (t :literal))))