;;;; Lexer Shim for Core Module
;;;;
;;;; This file provides backward compatibility for lexer support in core
;;;; by delegating to the separate parsing module when available.

(defpackage :epsilon.lib.lexer
  (:use :cl)
  (:export
   ;; Core lexer structure
   #:lexer
   #:make-lexer
   
   ;; Character access
   #:peek
   #:next
   #:at-end-p
   
   ;; Position management
   #:lexer-position
   #:save-position
   #:restore-position
   
   ;; Token structure
   #:token
   #:make-token
   #:%make-token
   #:token-type
   #:token-value
   #:token-position
   
   ;; Error handling
   #:lexer-error
   #:lexer-error-message
   #:lexer-error-position
   
   ;; Utilities
   #:skip-while
   #:skip-whitespace
   #:consume-while
   #:consume-string))

(in-package :epsilon.lib.lexer)

(defun lexer-available-p ()
  "Check if the lexer module is loaded and available."
  (find-package :epsilon.lib.lexer.impl))

(defun ensure-lexer-loaded ()
  "Ensure the lexer module is loaded, error if not available."
  (unless (lexer-available-p)
    (error "Lexer support requires the epsilon.parsing module to be loaded")))

;; Delegate all functions
(defmacro define-delegator (name &optional (args '(&rest args)))
  `(defun ,name ,args
     (ensure-lexer-loaded)
     (apply (find-symbol ,(string name) :epsilon.lib.lexer.impl)
            ,@(if (equal args '(&rest args))
                  '(args)
                  (mapcar (lambda (arg) 
                           (if (listp arg) (car arg) arg))
                          (remove '&rest args))))))

;; Define delegators for all exports
(define-delegator lexer)
(define-delegator make-lexer)
(define-delegator peek)
(define-delegator next)
(define-delegator at-end-p)
(define-delegator lexer-position)
(define-delegator save-position)
(define-delegator restore-position)
(define-delegator token)
(define-delegator make-token)
(define-delegator %make-token)
(define-delegator token-type)
(define-delegator token-value)
(define-delegator token-position)
(define-delegator lexer-error)
(define-delegator lexer-error-message)
(define-delegator lexer-error-position)
(define-delegator skip-while)
(define-delegator skip-whitespace)
(define-delegator consume-while)
(define-delegator consume-string)

;;; Export a feature to indicate lexer shim is loaded
(pushnew :epsilon-lexer-shim *features*)