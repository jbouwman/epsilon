;; Bootstrap the build system
(load "scripts/boot.lisp")
(epsilon.tool.boot:boot)

;; Build epsilon.core if needed
(epsilon.tool.build:build "epsilon.core")
