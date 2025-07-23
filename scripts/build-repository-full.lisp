;;;; Build bundled repository for epsilon distribution
;;;; This creates a complete repository with all standard library modules

(defpackage :epsilon.build-repository
  (:use :cl)
  (:export :build-repository))

(in-package :epsilon.build-repository)

(defparameter *standard-modules*
  '(;; Core modules
    "epsilon.core"
    "epsilon.net"
    "epsilon.lsp"
    "epsilon.http"
    "epsilon.json"
    "epsilon.yaml"
    "epsilon.msgpack"
    "epsilon.parsing"
    "epsilon.package"
    ;; Additional modules from src/
    "epsilon.gzip"
    "epsilon.inflate"
    "epsilon.zlib"
    "epsilon.bzip"
    "epsilon.foreign"
    "epsilon.regex"
    ;; Platform-specific modules
    #+linux "epsilon.linux"
    #+darwin "epsilon.darwin"
    #+win32 "epsilon.windows"))

(defun module-directory (module-name)
  "Find the source directory for a module"
  (let* ((short-name (if (and (>= (length module-name) 8)
                             (string= "epsilon." (subseq module-name 0 8)))
                        (subseq module-name 8)
                        module-name))
         (possible-dirs (list
                        (format nil "src/~A/" short-name)
                        (format nil "module/~A/" short-name)
                        (format nil "src/lib/~A/" short-name))))
    (find-if #'probe-file possible-dirs)))

(defun get-module-info (module-name)
  "Get package info for a module"
  (let ((dir (module-directory module-name)))
    (when dir
      (let ((package-file (merge-pathnames "package.lisp" dir)))
        (when (probe-file package-file)
          (with-open-file (in package-file)
            (read in)))))))

(defun compile-module-to-fasl (module-name output-dir)
  "Create a simple module entry for FASL format"
  (let ((info (get-module-info module-name)))
    (when info
      (let* ((deps (getf info :dependencies))
             (provides (getf info :provides))
             (fasl-name (format nil "~A.fasl" module-name)))
        
        (format t "  Creating entry for ~A~%" module-name)
        
        ;; For now, just create the metadata without actual compilation
        ;; This will be a placeholder until we fix the compilation issues
        (list :name module-name
              :version (or (getf info :version) "1.0.0")
              :fasl fasl-name
              :dependencies deps
              :provides provides)))))

(defun create-epk-package (module-name module-info packages-dir)
  "Create an EPK package for a module if zip is available"
  (when module-info
    (let* ((version (getf module-info :version))
           (epk-name (format nil "~A-~A.epk" module-name version))
           (epk-path (merge-pathnames epk-name packages-dir))
           (fasl-name (getf module-info :fasl)))
      
      (when fasl-name
        (format t "  Creating EPK package: ~A~%" epk-name)
        (ensure-directories-exist packages-dir)
        
        ;; Create EPK (which is just a zip file with metadata) 
        (let ((temp-dir (merge-pathnames 
                        (format nil "epk-build-~A/" (gensym))
                        "/tmp/")))
          (ensure-directories-exist temp-dir)
          
          ;; Copy FASL to temp directory using SBCL built-ins
          (let ((source (merge-pathnames fasl-name (merge-pathnames "fasls/" *output-dir*)))
                (dest (merge-pathnames fasl-name temp-dir)))
            (with-open-file (in source :direction :input :element-type '(unsigned-byte 8))
              (with-open-file (out dest :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
                (loop for byte = (read-byte in nil nil)
                      while byte do (write-byte byte out)))))
          
          ;; Create manifest
          (with-open-file (out (merge-pathnames "manifest.edn" temp-dir)
                              :direction :output
                              :if-exists :supersede)
            (format out "{:name ~S~%" module-name)
            (format out " :version ~S~%" version)
            (format out " :dependencies ~A~%" 
                    (format nil "[~{~S~^ ~}]" (getf module-info :dependencies)))
            (format out " :provides ~A}~%"
                    (format nil "[~{~S~^ ~}]" (getf module-info :provides))))
          
          ;; Create zip using external command
          (sb-ext:run-program "zip" (list "-r" (namestring epk-path) ".")
                             :directory temp-dir
                             :output nil
                             :error t)
          
          ;; Clean up temp directory
          (let ((temp-files (directory (merge-pathnames "*.*" temp-dir))))
            (dolist (file temp-files)
              (delete-file file))
            (sb-posix:rmdir (namestring temp-dir)))
          
          ;; Update module info with EPK file
          (setf (getf module-info :file) epk-name)
          module-info)))))

(defparameter *output-dir* nil)

(defun build-repository (&key 
                         (output-dir #p"./repository/")
                         (create-epk nil))
  "Build the bundled repository with all standard modules"
  (setf *output-dir* output-dir)
  (format t "Building bundled repository at ~A~%" output-dir)
  
  (let ((fasls-dir (merge-pathnames "fasls/" output-dir))
        (index '()))
    
    (ensure-directories-exist fasls-dir)
    
    ;; Build each module (just try a few core ones for now)
    (dolist (module-name '("epsilon.core"))
      (format t "~%Processing module: ~A~%" module-name)
      (handler-case
          (let ((module-info (compile-module-to-fasl module-name fasls-dir)))
            (when module-info
              ;; Add to index
              (let ((existing (assoc module-name index :test #'string=)))
                (if existing
                    ;; Update existing entry
                    (let ((versions (getf (cdr existing) :versions)))
                      (push (cons (getf module-info :version) module-info) versions)
                      (setf (getf (cdr existing) :versions) 
                            (sort versions #'string> :key #'car))
                      (setf (getf (cdr existing) :latest) 
                            (car (first versions))))
                  ;; New entry
                  (push (list module-name
                             :latest (getf module-info :version)
                             :versions (list (cons (getf module-info :version) 
                                                  module-info)))
                        index)))))
        (error (e)
          (format t "  Error processing ~A: ~A~%" module-name e))))
    
    ;; Write index file
    (with-open-file (out (merge-pathnames "index.lisp" output-dir)
                        :direction :output
                        :if-exists :supersede)
      (format out ";; Epsilon bundled repository index~%")
      (format out ";; Generated on ~A~%~%" (get-universal-time))
      (write index :stream out :pretty t))
    
    (format t "~%Repository build complete!~%")
    (format t "  Modules processed: ~D~%" (length index))
    (format t "  Index written to: ~A~%" 
            (merge-pathnames "index.lisp" output-dir))))