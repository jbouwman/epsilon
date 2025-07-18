;; Bootstrap the build system
(load "scripts/boot.lisp")
(epsilon.tool.boot:boot)

;; Register available modules
(epsilon.tool.build:register-modules)

;; Build epsilon.core if needed
(epsilon.tool.build:build "epsilon.core")
(epsilon.tool.build:build "epsilon.regex")
