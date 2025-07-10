;;;; Epsilon Optimized Entry Point
;;;; 
;;;; Main entry point that uses the fastest available boot method

;; Check for boot flags
(defparameter *boot-verbose* 
  (or (member "--verbose" sb-ext:*posix-argv* :test #'equal)
      (member "-v" sb-ext:*posix-argv* :test #'equal)))

(defparameter *force-rebuild*
  (or (member "--rebuild" sb-ext:*posix-argv* :test #'equal)
      (member "--force" sb-ext:*posix-argv* :test #'equal)))

;; Use optimized boot
(load "scripts/optimized-boot.lisp")

(format t "~&;;; Epsilon Boot System~%")
(format t "~&;;; ===================~%")

;; Boot epsilon.core with optimal strategy
(epsilon.tool.optimized-boot:boot 
  :module "epsilon.core"
  :force *force-rebuild*
  :verbose *boot-verbose*)

;; Build epsilon.core if needed (should be no-op if already loaded)
(when (fboundp 'epsilon.tool.build:build)
  (let ((build-start (get-internal-real-time)))
    (epsilon.tool.build:build "epsilon.core")
    (when *boot-verbose*
      (format t "~&;;; Build check: ~,3F seconds~%"
              (/ (- (get-internal-real-time) build-start)
                 internal-time-units-per-second)))))

;; Boot additional modules if requested
(when (member "--all" sb-ext:*posix-argv* :test #'equal)
  (format t "~&;;; Loading all modules...~%")
  (when (find-package :epsilon.lib.package.boot)
    (funcall (find-symbol "BOOT-MODULES" :epsilon.lib.package.boot)
             (funcall (find-symbol "DISCOVER-MODULES" :epsilon.lib.package))
             :verbose *boot-verbose*)))