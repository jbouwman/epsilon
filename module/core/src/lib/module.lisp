(defpackage #:epsilon.lib.module
  (:use #:cl))

(in-package #:epsilon.lib.module)

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
