;;;; Build script for creating SBCL core image with Epsilon preloaded
;;;;
;;;; This script creates a standalone SBCL core image that includes
;;;; the Epsilon library, making it available for distribution.

(defpackage #:epsilon.build.core
  (:use #:cl)
  (:export #:build-core-image))

(in-package #:epsilon.build.core)

(defun build-core-image (&key (output-file "target/epsilon-core") (verbose t))
  "Build SBCL core image with Epsilon preloaded"
  (when verbose
    (format t "Building Epsilon core image...~%"))
  
  ;; Ensure target directory exists
  (ensure-directories-exist (directory-namestring output-file))
  
  ;; Boot Epsilon first
  (when verbose
    (format t "Loading Epsilon...~%"))
  (load "module/core/src/tool/boot.lisp")
  (funcall (find-symbol "BOOT" "EPSILON.TOOL.BOOT"))
  
  ;; Create core image
  (when verbose
    (format t "Creating core image: ~A~%" output-file))
  (sb-ext:save-lisp-and-die output-file
                            :executable nil
                            :save-runtime-options t
                            :compression t))

;; Auto-run if loaded directly
(when (find-package "EPSILON.TOOL.BOOT")
  (build-core-image))