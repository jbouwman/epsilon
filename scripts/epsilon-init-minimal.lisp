;;;; Minimal Epsilon initialization for binary distribution
;;;; This provides basic functionality without complex module loading

(defpackage #:epsilon.init
  (:use #:cl)
  (:export #:epsilon-toplevel))

(in-package #:epsilon.init)

(defparameter *epsilon-version* "1.0.0-dev"
  "Epsilon version string")

(defun epsilon-banner ()
  "Display Epsilon banner"
  (format t "~&Epsilon ~A~%" *epsilon-version*)
  (format t "A self-contained Common Lisp Programming Environment~%")
  (format t "~%"))

(defun epsilon-toplevel ()
  "Custom toplevel for Epsilon runtime"
  ;; Suppress SBCL's default banner
  (let ((banner-symbol (find-symbol "*BANNER-PRINTED*" "SB-IMPL")))
    (when banner-symbol
      (setf (symbol-value banner-symbol) t)))
  
  ;; Display our banner
  (epsilon-banner)
  
  ;; Run the default SBCL toplevel
  (sb-impl::toplevel-init))

;; Simple module loading stub for now
(defun load-bundled-module (name)
  "Stub for loading bundled modules"
  (format t "Note: Module loading not yet implemented. Requested: ~A~%" name)
  nil)

(defun list-available-modules ()
  "Stub for listing modules"
  (format t "Module listing not yet implemented in this build.~%")
  nil)

;; Export to CL-USER for convenience
(import '(load-bundled-module list-available-modules) :cl-user)
(export '(load-bundled-module list-available-modules) :cl-user)