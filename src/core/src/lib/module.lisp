(defpackage #:epsilon.module
  (:use #:cl)
  (:export
   #:module
   #:defmodule  ; For backward compatibility
   #:read-module
   #:parse-module
   #:build-module
   #:update-package
   #:module-syntax))

(in-package #:epsilon.module)

(defun read-module (file)
  "Return the first form in FILE that looks like a module declaration."
  (with-open-file (stream file
                          :direction :input
                          :element-type 'character)
    (loop :for form := (read stream nil nil)
          :unless form
            :return nil
          :when (and (consp form)
                     (eql (car form) 'defmodule))
            :return form)))

(define-condition module-syntax (error)
  ((message :initarg :message)
   (form :initarg :form)
   (parent :initarg :parent
           :initform nil))
  (:report (lambda (condition stream)
             (with-slots (message form parent) condition
               (format stream "~A: ~A" message form)
               (when parent
                 (format stream "~%in: ~A" parent))))))

(defun assert-form (form f message &optional parent)
  (unless (funcall f form)
    (error 'module-syntax
           :message message
           :form form
           :parent parent)))

(defun assert-list (form)
  (assert-form form #'consp "not a list"))

(defun assert-symbol (form n sym)
  (unless (string= (symbol-name (parse-symbol form n))
                   (symbol-name sym))
    (error 'module-syntax
           :message "expected"
           :form sym
           :parent form)))

(defun parse-symbol (form n)
  (assert-list form)
  (let ((sym (nth n form)))
    (assert-form sym #'symbolp "not a symbol" form)
    sym))

(defun parse-import-term (term)
  (etypecase term
    (symbol (list :import-all (symbol-name term)))
    (cons `(:import ,(symbol-name (parse-symbol term 0))
                    ,@(mapcar #'symbol-name (cdr term))))))

(defun parse-import-clause (form)
  (mapcar #'parse-import-term form))

(defun parse-export-clause (form)
  `((:export ,@(mapcar #'symbol-name form))))

(defun parse-clause (form)
  (assert-list form)
  (ecase (parse-symbol form 0)
    (import (parse-import-clause (cdr form)))
    (export (parse-export-clause (cdr form)))))

(defun parse-module (form)
  (assert-symbol form 0 'module)
  (cons (symbol-name (parse-symbol form 1))
        (mapcan #'parse-clause (cddr form))))

(defun build-module (parsed-module)
  (destructuring-bind (name &rest clauses) parsed-module
    (let ((module (make-instance 'module :name name)))
      (dolist (clause clauses)
        (destructuring-bind (op &rest args) clause
          (ecase op
            (:import-all (module-import-all module (car args)))
            (:import (module-import-some module (cdr args) (cdr args)))
            (:export (module-export module args)))))
      module)))

(defclass module ()
  ((name :initarg :name)
   (import :initform (make-hash-table :test 'equal))
   (export :initform nil)))

(defun module-import-all (module import-module)
  (with-slots (import) module
    (setf (gethash import-module import) t)))

(defun module-import-some (module import-module import-syms)
  (with-slots (import) module
    (setf (gethash import-module import) import-syms)))

(defun module-export (module export-syms)
  (with-slots (export) module
    (setf export export-syms)))

(defun update-package (module)
  (with-slots (name import export) module
    (let ((package (or (find-package name)
                       (make-package name))))
      package)))

;;; Enhanced Module System 2.0 Implementation

(defmacro module (name &body clauses)
  "Enhanced module definition macro supporting modern import/export syntax.
  
  Usage:
    (module my-app.core
      :use (cl epsilon.syntax)
      :import ((map epsilon.map)
               (str epsilon.string))
      :export (main config start-server))
  
  Clauses:
    :use (packages...)         - Standard CL :use clause
    :import (specs...)         - Import with local nicknames
    :shadow (symbols...)       - Shadow symbols from CL
    :shadowing-import (specs...) - Shadowing imports
    :export (symbols...)       - Export symbols
    :re-export (symbols...)    - Re-export from imported modules
    :when condition            - Conditional compilation
    :features (features...)    - Feature flags"
  (let ((package-name (string name))
        (use-clause nil)
        (local-nicknames nil)
        (shadow-clause nil)
        (shadowing-import-clause nil)
        (export-clause nil)
        (re-export-clause nil)
        (import-from-clause nil)
        (conditional nil))
    
    ;; Parse clauses in pairs
    (loop for (keyword value . rest) on clauses by #'cddr do
      (unless (keywordp keyword)
        (error "Expected keyword in module clause, got: ~A" keyword))
      (case keyword
        (:use 
         (setf use-clause value))
        (:import
         (dolist (spec value)
           (cond
             ((symbolp spec)
              ;; Simple import: (package)
              (push `(,spec ,spec) local-nicknames))
             ((and (consp spec) (= (length spec) 2))
              ;; Aliased import: ((alias package))
              (push spec local-nicknames))
             ((and (consp spec) (>= (length spec) 3) (eq (second (last spec 2)) 'from))
              ;; Selective import: ((sym1 sym2) from package)
              (let ((symbols (butlast spec 2))
                    (package (car (last spec))))
                (push `(:import-from ,package ,@symbols) import-from-clause)))
             (t
              (error "Invalid import specification: ~A" spec)))))
        (:shadow
         (setf shadow-clause value))
        (:shadowing-import
         (setf shadowing-import-clause value))
        (:export
         (setf export-clause value))
        (:re-export
         (setf re-export-clause value))
        (:when
         (setf conditional value))))
    
    ;; Build the defpackage form
    (let ((defpackage-clauses nil))
      
      ;; Add :use clause
      (when use-clause
        (push `(:use ,@use-clause) defpackage-clauses))
      
      ;; Add :local-nicknames
      (when local-nicknames
        (push `(:local-nicknames ,@(reverse local-nicknames)) defpackage-clauses))
      
      ;; Add :shadow
      (when shadow-clause
        (push `(:shadow ,@shadow-clause) defpackage-clauses))
      
      ;; Add :shadowing-import-from
      (when shadowing-import-clause
        (push `(:shadowing-import-from ,@shadowing-import-clause) defpackage-clauses))
      
      ;; Add :import-from clauses
      (dolist (import-spec import-from-clause)
        (push `(:import-from ,(second import-spec) ,@(cddr import-spec)) defpackage-clauses))
      
      ;; Add :export
      (when (or export-clause re-export-clause)
        (push `(:export ,@export-clause ,@re-export-clause) defpackage-clauses))
      
      ;; Generate the final form
      (let ((package-form `(defpackage ,package-name
                             ,@(reverse defpackage-clauses)))
            (in-package-form `(in-package ,package-name)))
        
        ;; Wrap with conditional compilation if specified
        (if conditional
            `(when ,conditional
               ,package-form
               ,in-package-form)
            `(progn
               ,package-form
               ,in-package-form))))))

;; Backward compatibility alias
(defmacro defmodule (name &body clauses)
  "Backward compatibility alias for the original module syntax."
  `(module ,name ,@clauses))
