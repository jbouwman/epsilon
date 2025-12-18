;;;; Bootstrap a minimal build system, then build and load the
;;;; epsilon.core module, which includes all dev tools.

(load "modules/core/src/boot.lisp")

(boot)

;; Suppress redefinition warnings during module loading
(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
  (epsilon.loader:load-module (epsilon.loader:environment) "epsilon.core"))

;; Make the 'package' macro available in CL-USER for module compilation.
;; This allows modules to use (package name (import ...)) syntax.
;; We use shadowing-import to override the CL:PACKAGE type specifier.
(shadowing-import 'epsilon.main:package :cl-user)
