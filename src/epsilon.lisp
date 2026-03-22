;;;; Bootstrap a minimal build system, then build and load the
;;;; epsilon module, which includes all dev tools.

(load "src/boot.lisp")

(boot)

;; Make the extended 'defpackage' macro available in CL-USER for module compilation.
;; This allows modules to use (defpackage name (:require ...) (:enter t)) syntax.
;; We use shadowing-import to override CL:DEFPACKAGE (a macro used only at file top).
;; IMPORTANT: This must happen BEFORE load-module so non-boot files can use :require.
(shadowing-import 'epsilon.main:defpackage :cl-user)

;; Suppress redefinition and package variance warnings during module loading
(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning)
               (warning (lambda (c)
                          ;; Muffle package variance warnings
                          (when (search "also exports" (princ-to-string c))
                            (muffle-warning c)))))
  (epsilon.loader:load-module (epsilon.loader:environment) "epsilon"))

;; Set the default package to USER
(setf *package* (find-package :cl-user))
