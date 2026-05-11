;;;; Bootstrap a minimal build system, then build and load the
;;;; epsilon module, which includes all dev tools.

(load "src/boot.lisp")

(boot)

;; Make the extended 'defpackage' macro available in CL-USER for module compilation.
;; This allows modules to use (defpackage name (:import ...) ...) syntax.
;; We use shadowing-import to override CL:DEFPACKAGE (a macro used only at file top).
;; IMPORTANT: This must happen BEFORE load-module so non-boot files can use :import.
(shadowing-import 'epsilon.main:defpackage :cl-user)

;; Suppress redefinition and package variance warnings during module loading.
;; The CLI dispatch in epsilon.main:run reapplies the same handler so the
;; suppression covers --module loads after this handler has unwound.
(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning)
               (warning (lambda (c)
                          (let ((msg (princ-to-string c)))
                            (when (or (search "also exports" msg)
                                      (search "also shadows" msg))
                              (muffle-warning c))))))
  (epsilon.loader:environment) ;; ensure initialized
  (epsilon.loader:load-module "epsilon"))

;; Set the default package to USER
(setf *package* (find-package :cl-user))
