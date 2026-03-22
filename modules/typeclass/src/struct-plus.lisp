;;;; defstruct+ - Enhanced defstruct with :deriving support
;;;;
;;;; Wraps CL defstruct and adds an optional (:deriving ...) clause that
;;;; auto-generates typeclass instances (show, eq-class, hash-class, ord).
;;;; Shares deriving logic with defrecord via tc-std:generate-deriving-instances.
;;;;
;;;; Usage:
;;;;   (defstruct+ (limit-reader (:constructor %make-limit-reader))
;;;;     "Reader that yields at most N bytes from source."
;;;;     (:deriving show eq-class)
;;;;     (source nil)
;;;;     (remaining 0 :type fixnum)
;;;;     (closed-p nil :type boolean))
;;;;
;;;; Without :deriving, behaves identically to plain defstruct.

(defpackage :epsilon.struct-plus
  (:use :cl)
  (:local-nicknames
   (:tc :epsilon.typeclass)
   (:tc-std :epsilon.typeclass.std))
  (:export #:defstruct+))

(in-package :epsilon.struct-plus)

(defun parse-struct-name-and-options (name-and-options)
  "Parse defstruct name-and-options to extract name and conc-name.
   Returns (values name conc-name-string)."
  (if (symbolp name-and-options)
      (values name-and-options (format nil "~A-" name-and-options))
      (let* ((name (car name-and-options))
             (options (cdr name-and-options))
             (conc-name-opt (find :conc-name options
                                  :key (lambda (opt)
                                         (when (and (listp opt) (not (null opt)))
                                           (car opt))))))
        (values name
                (if conc-name-opt
                    (let ((val (second conc-name-opt)))
                      (cond ((null val) "")
                            ((symbolp val) (format nil "~A" val))
                            ((stringp val) val)
                            (t (format nil "~A" val))))
                    (format nil "~A-" name))))))

(defun extract-struct-fields (body)
  "Extract field names from defstruct slot specifications.
   Skips doc strings, :deriving clauses, and other non-slot forms."
  (loop for item in body
        when (and (symbolp item) (not (keywordp item)))
          collect item
        when (and (listp item) (not (null item))
                  (symbolp (car item)) (not (keywordp (car item))))
          collect (car item)))

(defmacro defstruct+ (name-and-options &body body)
  "Define a struct with optional :deriving for automatic typeclass instances.

   Accepts all standard defstruct options. Additionally supports a
   (:deriving tc1 tc2 ...) clause in the body that generates typeclass
   instances for the listed typeclasses (show, eq-class, hash-class, ord).

   Without :deriving, expands to plain defstruct with no overhead."
  (multiple-value-bind (name conc-name-str)
      (parse-struct-name-and-options name-and-options)
    (let* ((deriving-form (find-if (lambda (item)
                                     (and (listp item) (eq (car item) :deriving)))
                                   body))
           (deriving-list (when deriving-form (cdr deriving-form)))
           (clean-body (remove deriving-form body))
           (fields (extract-struct-fields clean-body))
           (accessors (mapcar (lambda (field)
                                (intern (format nil "~A~A" conc-name-str
                                                (symbol-name field))))
                              fields)))
      (if deriving-list
          `(progn
             (defstruct ,name-and-options ,@clean-body)
             ,@(tc-std:generate-deriving-instances name name fields accessors deriving-list))
          `(defstruct ,name-and-options ,@clean-body)))))
