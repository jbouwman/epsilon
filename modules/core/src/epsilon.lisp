;;;; Bootstrap a minimal build system, then build and load the
;;;; epsilon.core module, which includes all dev tools.

(load "modules/core/src/boot.lisp")

(boot)

;; Suppress redefinition warnings during module loading
(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
  (epsilon.loader:load-module (epsilon.loader:environment) "epsilon.core"))
