;; Bootstrap the build system
(load "scripts/boot.lisp")
(epsilon.tool.boot:boot)

;; Register available modules
(epsilon.tool.build:register-modules)
