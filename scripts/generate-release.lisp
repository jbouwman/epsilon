;;;; Generate Release
;;;;
;;;; Replaces CI scripts with modular tool-based approach

(defun generate-release (&key (include-platform nil) (verbose t))
  "Generate complete release with EPKs and index"
  (when verbose
    (format t ";;; Starting release generation~%"))
  
  ;; Ensure modules are registered
  (epsilon.tool.build:register-modules)
  
  ;; Build all modules first
  (when verbose
    (format t ";;; Building all modules~%"))
  (epsilon.tool.build:build-all :include-platform include-platform)
  
  ;; Generate EPKs
  (when verbose
    (format t ";;; Generating EPKs~%"))
  (multiple-value-bind (epks failed)
      (epsilon.tool.release:generate-all-epks :include-platform include-platform
                                             :verbose verbose)
    (when failed
      (format *error-output* ";;; Warning: Failed to generate EPKs for: ~{~A~^, ~}~%" failed))
    
    ;; Create release index
    (when epks
      (when verbose
        (format t ";;; Creating release index~%"))
      (epsilon.tool.release:create-release-index epks))
    
    (when verbose
      (format t ";;; Release generation complete~%")
      (format t ";;;   Generated ~D EPKs~%" (length epks))
      (when failed
        (format t ";;;   Failed: ~D modules~%" (length failed))))
    
    (values epks failed)))

;; If running as script
(when (and (boundp '*load-pathname*) *load-pathname*)
  (generate-release))