;;;; Build script to create an EPK package for epsilon.json module
;;;; Demonstrates the package creation process

(require 'epsilon.core)
(require 'epsilon.package)

(defpackage #:build-json-epk
  (:use #:cl)
  (:local-nicknames
   (#:pkg #:epsilon.lib.package)
   (#:repo #:epsilon.lib.package.repository)
   (#:builder #:epsilon.lib.package.builder)
   (#:fs #:epsilon.sys.fs)
   (#:uri #:epsilon.lib.uri)
   (#:map #:epsilon.lib.map)))

(in-package #:build-json-epk)

(defun ensure-output-directory ()
  "Ensure the output directory exists"
  (let ((output-dir (uri:make-uri :scheme "file" :path "target/packages/")))
    (fs:make-dirs output-dir)
    output-dir))

(defun build-json-module ()
  "Build the epsilon.json module and create an EPK package"
  (format t "~&Building epsilon.json module EPK package...~%~%")
  
  ;; Module information
  (let* ((module-dir (uri:make-uri :scheme "file" :path "src/json/"))
         (output-dir (ensure-output-directory))
         (version "1.0.0")
         (platform (pkg:platform-string))
         (epk-filename (pkg:make-package-filename "epsilon.json" version)))
    
    (format t "Module directory: ~A~%" (uri:path module-dir))
    (format t "Output directory: ~A~%" (uri:path output-dir))
    (format t "Platform: ~A~%" platform)
    (format t "EPK filename: ~A~%~%" epk-filename)
    
    ;; Step 1: Compile the module
    (format t "Step 1: Compiling module sources...~%")
    (let ((source-files '("src/lib/json.lisp" "src/lib/api.lisp")))
      (dolist (file source-files)
        (let ((source-path (uri:merge module-dir file)))
          (format t "  Compiling ~A...~%" file)
          (when (fs:exists-p source-path)
            (compile-file (uri:path source-path) :verbose nil :print nil)
            (format t "    ✓ Compiled~%")))))
    
    ;; Step 2: Create combined FASL
    (format t "~%Step 2: Creating combined FASL...~%")
    (let* ((fasl-dir (uri:merge output-dir "fasl/"))
           (combined-fasl (uri:merge fasl-dir "combined.fasl")))
      (fs:make-dirs fasl-dir)
      
      ;; In a real implementation, we would concatenate the individual FASLs
      ;; For now, just create a placeholder
      (with-open-file (out (uri:path combined-fasl) 
                           :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (write-byte 0 out))
      (format t "  Created combined.fasl~%"))
    
    ;; Step 3: Create manifest
    (format t "~%Step 3: Creating manifest...~%")
    (let* ((manifest (map:make-map
                      :name "epsilon.json"
                      :version version
                      :description "JSON parser using epsilon's parser combinator framework"
                      :author "Epsilon Project"
                      :license "MIT"
                      :platform (map:make-map 
                                 :os (pkg::getf pkg:*platform-info* :os)
                                 :arch (pkg::getf pkg:*platform-info* :arch))
                      :provides '("epsilon.lib.json" "epsilon.lib.json.impl")
                      :dependencies '("epsilon.core" "epsilon.parsing")
                      :build (map:make-map
                              :timestamp (get-universal-time)
                              :builder "epsilon-build-example")))
           (meta-dir (uri:merge output-dir "META-INF/"))
           (manifest-file (uri:merge meta-dir "MANIFEST.edn")))
      
      (fs:make-dirs meta-dir)
      (with-open-file (out (uri:path manifest-file)
                           :direction :output
                           :if-exists :supersede)
        (format out "~S~%" manifest))
      (format t "  Created MANIFEST.edn~%"))
    
    ;; Step 4: Create EPK structure
    (format t "~%Step 4: Creating EPK package structure...~%")
    (let ((epk-build-dir (uri:merge output-dir "epk-build/")))
      ;; Create directory structure
      (fs:make-dirs (uri:merge epk-build-dir "META-INF/"))
      (fs:make-dirs (uri:merge epk-build-dir "src/lib/"))
      (fs:make-dirs (uri:merge epk-build-dir "fasl/"))
      (fs:make-dirs (uri:merge epk-build-dir "docs/"))
      
      ;; Copy files
      (format t "  Copying source files...~%")
      (fs:copy-file (uri:merge module-dir "src/lib/json.lisp")
                    (uri:merge epk-build-dir "src/lib/json.lisp"))
      (fs:copy-file (uri:merge module-dir "src/lib/api.lisp")
                    (uri:merge epk-build-dir "src/lib/api.lisp"))
      
      (format t "  Copying manifest...~%")
      (fs:copy-file (uri:merge output-dir "META-INF/MANIFEST.edn")
                    (uri:merge epk-build-dir "META-INF/MANIFEST.edn"))
      
      (format t "  Copying FASL...~%")
      (fs:copy-file (uri:merge output-dir "fasl/combined.fasl")
                    (uri:merge epk-build-dir "fasl/combined.fasl")))
    
    ;; Step 5: Create ZIP archive (EPK file)
    (format t "~%Step 5: Creating EPK archive...~%")
    (let ((epk-file (uri:merge output-dir epk-filename))
          (build-dir (uri:path (uri:merge output-dir "epk-build"))))
      
      ;; Use system zip command
      (let ((zip-cmd (format nil "cd ~A && zip -r ~A ."
                            build-dir
                            (uri:path epk-file))))
        (format t "  Running: ~A~%" zip-cmd)
        (sb-ext:run-program "/bin/sh" (list "-c" zip-cmd)
                            :output t
                            :error t))
      
      (format t "~%✓ Successfully created: ~A~%" (uri:path epk-file))
      
      ;; Step 6: Register in local repository
      (format t "~%Step 6: Registering in local repository...~%")
      (repo:initialize-repository)
      (repo:add-package-to-index
       "epsilon.json" version platform epk-filename
       (with-open-file (in (uri:path epk-file) :element-type '(unsigned-byte 8))
         (file-length in))
       "sha256-placeholder"
       '(("epsilon.core" ">= 1.0.0")
         ("epsilon.parsing" ">= 1.0.0")))
      
      (format t "✓ Package registered in repository index~%")
      
      ;; Show final structure
      (format t "~%Final EPK contents:~%")
      (format t "~A~%" epk-filename)
      (format t "├── META-INF/~%")
      (format t "│   └── MANIFEST.edn~%")
      (format t "├── src/~%")
      (format t "│   └── lib/~%")
      (format t "│       ├── json.lisp~%")
      (format t "│       └── api.lisp~%")
      (format t "├── fasl/~%")
      (format t "│   └── combined.fasl~%")
      (format t "└── docs/~%")
      
      epk-file)))

;; Run the build
(defun main ()
  (handler-case
      (build-json-module)
    (error (e)
      (format t "~%Error during build: ~A~%" e)
      (sb-ext:exit :code 1)))
  (sb-ext:exit :code 0))

(main)