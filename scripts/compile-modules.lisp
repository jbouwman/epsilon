;;;; Module compilation system for epsilon
;;;; This compiles epsilon modules to FASL files for distribution

(defpackage :epsilon.compile-modules
  (:use :cl)
  (:export :compile-module :compile-all-modules))

(in-package :epsilon.compile-modules)

(defparameter *module-sources*
  '(;; Module name -> source files to load
    ("epsilon.core" . nil) ; Already loaded in epsilon binary
    ("epsilon.parsing" . ("../../src/parsing/src/lib/lexer.lisp"
                         "../../src/parsing/src/lib/parser.lisp"))
    ("epsilon.json" . ("../../src/json/src/lib/json.lisp"
                      "../../src/json/src/lib/api.lisp"))
    ("epsilon.net" . ("../../src/net/src/net.lisp"))
    ("epsilon.http" . ("../../src/http/src/request.lisp"
                      "../../src/http/src/response.lisp"
                      "../../src/http/src/server.lisp"
                      "../../src/http/src/client.lisp"))
    ("epsilon.linux" . ("../../src/linux/src/sys/epoll.lisp"
                       "../../src/linux/src/net/core.lisp"
                       "../../src/linux/src/tls/core.lisp"))))

(defun ensure-package-exists (package-name)
  "Ensure a package exists, creating it if necessary"
  (or (find-package package-name)
      (make-package package-name :use '(:cl))))

(defun compile-module (module-name output-dir)
  "Compile a single module to FASL"
  (let ((sources (cdr (assoc module-name *module-sources* :test #'string=))))
    (cond
      ;; epsilon.core is already in the image
      ((string= module-name "epsilon.core")
       (format t "~&Skipping ~A (already loaded in epsilon binary)~%" module-name)
       t)
      
      ;; No sources found
      ((null sources)
       (format t "~&Warning: No sources defined for ~A~%" module-name)
       nil)
      
      ;; Compile the module
      (t
       (let ((fasl-path (merge-pathnames 
                        (format nil "~A.fasl" module-name)
                        output-dir)))
         (format t "~&Compiling ~A to ~A~%" module-name fasl-path)
         (ensure-directories-exist fasl-path)
         
         (handler-case
             (with-compilation-unit (:policy '(optimize (debug 2) (safety 3)))
               ;; Create a temporary file to hold all module code
               (let ((temp-file (merge-pathnames 
                                (format nil "~A-combined.lisp" module-name)
                                "/tmp/")))
                 
                 ;; Combine all source files
                 (with-open-file (out temp-file 
                                     :direction :output 
                                     :if-exists :supersede)
                   (format out ";;;; Combined sources for ~A~%~%" module-name)
                   
                   ;; Process each source file
                   (dolist (source sources)
                     (let ((source-path (pathname source)))
                       (if (probe-file source-path)
                           (progn
                             (format out ";;;; From ~A~%~%" source)
                             (with-open-file (in source-path)
                               (loop for line = (read-line in nil nil)
                                     while line
                                     do (write-line line out))))
                           (progn
                             (format t "  Warning: Source file not found: ~A~%" source)
                             (return-from compile-module nil))))))
                 
                 ;; Compile the combined file
                 (compile-file temp-file :output-file fasl-path)
                 
                 ;; Clean up
                 (delete-file temp-file)
                 
                 (format t "  Successfully compiled ~A~%" module-name)
                 t))
           
           (error (e)
             (format t "  Error compiling ~A: ~A~%" module-name e)
             nil)))))))

(defun compile-all-modules (&key (output-dir #p"./repository/fasls/"))
  "Compile all modules to FASL files"
  (format t "~&Compiling epsilon modules...~%")
  (ensure-directories-exist output-dir)
  
  (let ((modules '("epsilon.core" 
                  "epsilon.parsing" 
                  "epsilon.json"
                  "epsilon.net"
                  "epsilon.http"
                  #+linux "epsilon.linux"))
        (success-count 0))
    
    (dolist (module modules)
      (when (compile-module module output-dir)
        (incf success-count)))
    
    (format t "~&~%Compilation complete: ~D/~D modules successful~%" 
            success-count (length modules))
    
    (= success-count (length modules))))