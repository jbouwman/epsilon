;;;; Parser Shim for Core Module
;;;;
;;;; This file provides backward compatibility for parser support in core
;;;; by delegating to the separate parsing module when available.

(defpackage :epsilon.lib.parser
  (:use :cl)
  (:shadow #:return #:sequence)
  (:export
   ;; Parser type and state
   #:parser
   #:parse-state
   #:make-parse-state
   #:success-p
   #:success-value
   #:failure-message
   #:parse-position
   #:parse-remaining
   #:parse-context
   
   ;; Core combinators
   #:bind
   #:return
   #:fail
   #:satisfy
   #:token
   #:choice
   #:sequence
   #:many
   #:many1
   #:optional
   #:sepBy
   #:sep+
   #:chainl1
   #:between
   #:lookahead
   #:try
   #:eof
   #:label
   #:parse))

(in-package :epsilon.lib.parser)

(defun parser-available-p ()
  "Check if the parser module is loaded and available."
  (find-package :epsilon.lib.parser.impl))

(defun ensure-parser-loaded ()
  "Ensure the parser module is loaded, error if not available."
  (unless (parser-available-p)
    (error "Parser support requires the epsilon.parsing module to be loaded")))

;; Delegate all functions
(defmacro define-delegator (name &optional (args '(&rest args)))
  `(defun ,name ,args
     (ensure-parser-loaded)
     (apply (find-symbol ,(string name) :epsilon.lib.parser.impl)
            ,@(if (equal args '(&rest args))
                  '(args)
                  (mapcar (lambda (arg) 
                           (if (listp arg) (car arg) arg))
                          (remove '&rest args))))))

;; Define delegators for all exports
(define-delegator parser)
(define-delegator parse-state)
(define-delegator make-parse-state)
(define-delegator success-p)
(define-delegator success-value)
(define-delegator failure-message)
(define-delegator parse-position)
(define-delegator parse-remaining)
(define-delegator parse-context)
(define-delegator bind)
(define-delegator return)
(define-delegator fail)
(define-delegator satisfy)
(define-delegator token)
(define-delegator choice)
(define-delegator sequence)
(define-delegator many)
(define-delegator many1)
(define-delegator optional)
(define-delegator sepBy)
(define-delegator sep+)
(define-delegator chainl1)
(define-delegator between)
(define-delegator lookahead)
(define-delegator try)
(define-delegator eof)
(define-delegator label)
(define-delegator parse)

;;; Export a feature to indicate parser shim is loaded
(pushnew :epsilon-parser-shim *features*)