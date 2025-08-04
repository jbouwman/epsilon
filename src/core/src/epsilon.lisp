;;;; Bootstrap a minimal build system, then build and load the
;;;; epsilon.core module, which includes all dev tools.

(load "src/core/src/boot.lisp")

(boot)

(epsilon.loader:load-package (epsilon.loader:environment) "epsilon.core")
