;;;; Epsilon Fast Boot Entry Point
;;;; 
;;;; This script checks for epsilon.core EPK/cache and uses fast boot when available

;; Check if we have a boot cache
(let ((cache-path (merge-pathnames ".epsilon/boot-cache/epsilon.core.boot.fasl" 
                                   (user-homedir-pathname))))
  (if (probe-file cache-path)
      ;; Fast path - use cached boot
      (progn
        (load "scripts/fast-boot.lisp")
        (epsilon.tool.fast-boot:fast-boot))
      ;; Slow path - traditional boot
      (progn
        (load "scripts/boot.lisp")
        (epsilon.tool.boot:boot))))

;; Build epsilon.core if needed (this will be much faster with cache)
(epsilon.tool.build:build "epsilon.core")

;; Check if we should continue with additional modules
(when (member "--all-modules" sb-ext:*posix-argv* :test #'equal)
  ;; Use the package boot system for other modules
  (when (find-package :epsilon.lib.package.boot)
    (let ((boot-pkg (find-package :epsilon.lib.package.boot)))
      (when boot-pkg
        (let ((quick-boot (find-symbol "QUICK-BOOT" boot-pkg)))
          (when (and quick-boot (fboundp quick-boot))
            (funcall quick-boot :verbose t)))))))