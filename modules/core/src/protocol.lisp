;;;; Protocol system for defining extensible interfaces
;;;;
;;;; This module provides a protocol system based on generic functions
;;;; to support extension points throughout epsilon.

(defpackage :epsilon.protocol
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.map))
  (:export
   ;; Protocol definition
   #:define-protocol
   #:protocol
   #:protocol-name
   #:protocol-version
   #:protocol-documentation
   #:protocol-methods
   
   ;; Protocol registry
   #:find-protocol
   #:list-protocols
   #:protocol-exists-p
   
   ;; Utilities
   #:ensure-protocol-method
   #:protocol-method-p
   #:list-protocol-implementations))

(in-package :epsilon.protocol)

(defclass protocol ()
  ((name :initarg :name
         :reader protocol-name
         :type symbol
         :documentation "The name of the protocol")
   (version :initarg :version
            :reader protocol-version
            :initform "1.0"
            :type string
            :documentation "Protocol version string")
   (documentation :initarg :documentation
                  :reader protocol-documentation
                  :initform nil
                  :type (or null string)
                  :documentation "Protocol documentation")
   (methods :initarg :methods
            :reader protocol-methods
            :initform '()
            :type list
            :documentation "List of generic functions in this protocol"))
  (:documentation "A protocol defines a set of generic functions forming an interface"))

(defvar *protocols* map:+empty+
  "Global registry of defined protocols")

(defun find-protocol (name)
  "Find a protocol by name"
  (map:get *protocols* name))

(defun protocol-exists-p (name)
  "Check if a protocol exists"
  (map:contains-p *protocols* name))

(defun list-protocols ()
  "Return a list of all defined protocol names"
  (map:keys *protocols*))

(defun register-protocol (protocol)
  "Register a protocol in the global registry"
  (setf *protocols* (map:assoc *protocols* 
                               (protocol-name protocol) 
                               protocol)))

(defmacro define-protocol (name &body options)
  "Define a new protocol.
   
   Example:
   (define-protocol my-protocol
     (:version \"1.0\")
     (:documentation \"My protocol description\")
     (:method foo (x y) \"Do foo operation\")
     (:method bar (x) (:optional z) \"Do bar operation\"))"
  (let ((version "1.0")
        (documentation nil)
        (methods '()))
    ;; Parse options
    (dolist (option options)
      (case (first option)
        (:version (setf version (second option)))
        (:documentation (setf documentation (second option)))
        (:method (push (rest option) methods))))
    
    `(progn
       ;; Create and register the protocol
       (register-protocol
        (make-instance 'protocol
                       :name ',name
                       :version ,version
                       :documentation ,documentation
                       :methods ',(mapcar #'first methods)))
       
       ;; Define generic functions for each method
       ,@(loop for (method-name lambda-list . method-options) in (reverse methods)
               collect `(defgeneric ,method-name ,lambda-list
                          ,@(when (stringp (first method-options))
                              `((:documentation ,(first method-options))))
                          ,@(when (and (stringp (first method-options))
                                       (rest method-options))
                              (rest method-options))
                          ,@(unless (stringp (first method-options))
                              method-options)))
       
       ',name)))

(defun ensure-protocol-method (protocol-name method-name)
  "Ensure a method belongs to a protocol"
  (let ((protocol (find-protocol protocol-name)))
    (unless protocol
      (error "Protocol ~S not found" protocol-name))
    (unless (member method-name (protocol-methods protocol))
      (error "Method ~S is not part of protocol ~S" 
             method-name protocol-name))
    t))

(defun protocol-method-p (method-name)
  "Check if a generic function is part of any protocol"
  (loop for protocol-name in (list-protocols)
        for protocol = (find-protocol protocol-name)
        thereis (member method-name (protocol-methods protocol))))

(defun list-protocol-implementations (protocol-name)
  "List all classes that implement methods for this protocol"
  (let ((protocol (find-protocol protocol-name))
        (implementations map:+empty+))
    (when protocol
      (dolist (method-name (protocol-methods protocol))
        (when (fboundp method-name)
          (let ((gf (symbol-function method-name)))
            (when (typep gf 'generic-function)
              (dolist (method (sb-mop:generic-function-methods gf))
                (let* ((specializers (sb-mop:method-specializers method))
                       (class-spec (first specializers)))
                  (when (typep class-spec 'class)
                    (let ((class-name (class-name class-spec)))
                      (setf implementations 
                            (map:assoc implementations 
                                       class-name
                                       (cons method-name
                                             (map:get implementations class-name))))))))))))
      implementations)))
