;; Bootstrap minimal build system
(load "scripts/boot.lisp")
(epsilon.tool.boot:boot)

;; Register available modules
#+win32 (format t ";;; Current directory: ~A~%" (namestring (truename *default-pathname-defaults*)))
(epsilon.tool.build:register-modules)

;; Build and load epsilon.core module (includes dev tools)
(epsilon.tool.build:build "epsilon.core")
