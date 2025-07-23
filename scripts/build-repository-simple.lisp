;;;; Simple repository builder that creates metadata for existing modules
;;;; This creates a working repository without complex compilation

(defpackage :epsilon.build-simple
  (:use :cl)
  (:export :build-simple-repository))

(in-package :epsilon.build-simple)

(defun build-simple-repository (&key (output-dir #p"./repository/"))
  "Build a simple repository with module metadata"
  (format t "Building simple repository at ~A~%" output-dir)
  
  (let ((fasls-dir (merge-pathnames "fasls/" output-dir))
        (index '()))
    
    (ensure-directories-exist fasls-dir)
    
    ;; Create entries for modules we know exist (even if we don't compile them)
    (let ((known-modules '(("epsilon.core" "1.0.0" nil ("epsilon.core"))
                          ("epsilon.json" "1.0.0" ("epsilon.core" "epsilon.parsing") ("epsilon.lib.json"))
                          ("epsilon.http" "1.0.0" ("epsilon.core" "epsilon.net") ("epsilon.http.server" "epsilon.http.client"))
                          ("epsilon.parsing" "1.0.0" ("epsilon.core") ("epsilon.lib.parser" "epsilon.lib.lexer")))))
      
      (dolist (module-spec known-modules)
        (destructuring-bind (name version deps provides) module-spec
          (let* ((fasl-name (format nil "~A.fasl" name))
                 (module-info (list :name name
                                   :version version
                                   :fasl fasl-name
                                   :dependencies (or deps '())
                                   :provides (or provides (list name)))))
            
            ;; Add to index
            (push (list name
                       :latest version
                       :versions (list (cons version module-info)))
                  index)
            
            (format t "  Added ~A (~A)~%" name version)))))
    
    ;; Write index file
    (with-open-file (out (merge-pathnames "index.lisp" output-dir)
                        :direction :output
                        :if-exists :supersede)
      (format out ";; Epsilon bundled repository index~%")
      (format out ";; Generated on ~A~%~%" (get-universal-time))
      (write index :stream out :pretty t))
    
    (format t "~%Simple repository build complete!~%")
    (format t "  Modules: ~D~%" (length index))
    (format t "  Index written to: ~A~%" 
            (merge-pathnames "index.lisp" output-dir))))