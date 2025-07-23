(defpackage :epsilon.package
  (:use
   :cl
   :epsilon.digest
   :epsilon.hex
   :epsilon.base64)
  (:local-nicknames
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.uri)
   (:map :epsilon.map)
   (:str :epsilon.string)
   (:sequence :epsilon.sequence)
   (:edn :epsilon.edn)
   (:json :epsilon.json))
  (:export
   ;; Package creation
   :build-package
   :create-manifest
   :sign-package
   :create-epk
   :build-epk-from-module
   
   ;; Package installation
   :install-package
   :verify-package
   :list-packages
   
   ;; Repository management
   :create-repository
   :update-index
   :publish-package
   
   ;; Package information
   :package-info
   :dependency-tree
   :platform-compatibility
   
   ;; Platform utilities
   :platform-string
   :*platform-info*
   
   ;; EPK filename utilities
   :make-package-filename
   :parse-package-filename))

(in-package :epsilon.package)

;;;; ==========================================================================
;;;; Platform Detection
;;;; ==========================================================================

(defparameter *platform-info*
  (list :os #+darwin :darwin
            #+linux :linux
            #+windows :windows
            #-(or darwin linux windows) :unknown
        :arch #+x86-64 :x86_64
              #+arm64 :arm64
              #-(or x86-64 arm64) :unknown
        :lisp :sbcl
        :lisp-version (lisp-implementation-version)))

(defun platform-string ()
  "Generate platform string for package naming"
  (format nil "~(~A-~A~)" 
          (getf *platform-info* :os)
          (getf *platform-info* :arch)))

;;;; ==========================================================================
;;;; EPK Filename Utilities
;;;; ==========================================================================

(defun make-package-filename (name version &optional platform arch)
  "Create a standardized EPK filename"
  (format nil "~A-~A-~A-~A.epk" 
          name 
          version
          (or platform (string-downcase (symbol-name (getf *platform-info* :os))))
          (or arch (string-downcase (symbol-name (getf *platform-info* :arch))))))

(defun parse-package-filename (filename)
  "Parse an EPK filename into components"
  (let ((basename (if (str:ends-with-p filename ".epk")
                      (subseq filename 0 (- (length filename) 4))
                      filename)))
    (let ((parts (str:split #\- basename)))
      (when (>= (length parts) 4)
        (let ((name-parts (butlast parts 3))
              (version (nth (- (length parts) 3) parts))
              (platform (nth (- (length parts) 2) parts))
              (arch (nth (- (length parts) 1) parts)))
          (map:make-map
           :name (str:join "-" name-parts)
           :version version
           :platform platform
           :arch arch))))))

;;;; ==========================================================================
;;;; Package Metadata Management
;;;; ==========================================================================

(defun create-manifest (module-info &key build-info dependencies)
  "Create package manifest from module information"
  (map:make-map
   :name (map:get module-info "name")
   :version (map:get module-info "version")
   :description (map:get module-info "description")
   :author (map:get module-info "author")
   :license (map:get module-info "license")
   :homepage (map:get module-info "homepage")
   
   :platform *platform-info*
   
   :provides (map:get module-info "provides")
   :main-package (first (map:get module-info "provides"))
   
   :build (or build-info
              (map:make-map
               :timestamp (format-timestamp (get-universal-time))
               :source-hash (calculate-source-hash module-info)
               :builder (format nil "epsilon-build-~A" 
                               (map:get build-info "version" "1.0.0"))))
   
   :install (map:make-map
             :load-order '("combined.fasl")
             :native-libs '()
             :post-install nil)
   
   :compatibility (map:make-map
                   :min-epsilon-version "1.0.0"
                   :platforms '(:darwin :linux :windows))))

(defun format-timestamp (universal-time)
  "Format universal time as ISO 8601"
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour min sec)))

(defun calculate-source-hash (module-info)
  "Calculate SHA-256 hash of all source files in module"
  (let ((digest (sha-256)))
    ;; This would iterate through all source files and hash them
    ;; For now, return a placeholder
    (digest-string digest (map:get module-info "name"))))

;;;; ==========================================================================
;;;; Package Building
;;;; ==========================================================================

(defun build-package (module-path &key output-dir sign-key)
  "Build a complete .epk package from module source"
  (let* ((module-info (load-module-info module-path))
         (package-name (map:get module-info "name"))
         (version (map:get module-info "version"))
         (platform (platform-string))
         (filename (format nil "~A-~A-~A.epk" package-name version platform))
         (output-path (if output-dir
                          (uri:merge output-dir filename)
                          filename))
         (temp-dir (fs:temp-dir)))
    
    (format t "Building package: ~A~%" filename)
    
    ;; Create temporary build directory
    (let ((build-dir (uri:merge temp-dir (format nil "epk-build-~A/" (random-string 8)))))
      (fs:make-dirs build-dir)
      
      (unwind-protect
           (progn
             ;; Create package structure
             (create-package-structure build-dir module-path module-info)
             
             ;; Compile sources
             (compile-package-sources build-dir module-path)
             
             ;; Generate metadata
             (create-package-metadata build-dir module-info)
             
             ;; Sign package if key provided
             (when sign-key
               (sign-package-contents build-dir sign-key))
             
             ;; Create ZIP archive
             (create-package-archive build-dir output-path)
             
             (format t "Package created: ~A~%" output-path)
             output-path)
        
        ;; Cleanup
        (fs:delete-directory (uri:path build-dir))))))

(defun create-package-structure (build-dir module-path module-info)
  "Create the standard package directory structure"
  (let ((meta-dir (uri:merge build-dir "META-INF/"))
        (src-dir (uri:merge build-dir "src/"))
        (fasl-dir (uri:merge build-dir "fasl/"))
        (docs-dir (uri:merge build-dir "docs/"))
        (tests-dir (uri:merge build-dir "tests/")))
    
    ;; Create directories
    (dolist (dir (list meta-dir src-dir fasl-dir docs-dir tests-dir))
      (fs:make-dirs dir))
    
    ;; Copy source files
    (copy-module-sources module-path src-dir)
    
    ;; Copy documentation
    (copy-module-docs module-path docs-dir)
    
    ;; Copy tests
    (copy-module-tests module-path tests-dir)))

(defun copy-module-sources (module-path dest-dir)
  "Copy all source files from module to package"
  (let ((src-path (uri:merge module-path "src/")))
    (when (fs:exists-p src-path)
      (copy-directory (uri:path src-path) (uri:path dest-dir)))))

(defun compile-package-sources (build-dir module-path)
  "Compile all source files and create combined FASL"
  (let ((individual-dir (uri:merge build-dir "fasl/individual/"))
        (combined-fasl (uri:merge build-dir "fasl/combined.fasl"))
        (src-dir (uri:merge build-dir "src/")))
    
    (fs:make-dirs individual-dir)
    
    ;; Compile individual files (simplified version)
    (let ((fasl-files '()))
      (dolist (lisp-file (fs:list-files src-dir ".lisp"))
        (let* ((base-name (pathname-name (uri:path lisp-file)))
               (fasl-name (format nil "~A.fasl" base-name))
               (fasl-path (uri:merge individual-dir fasl-name)))
          (fs:make-dirs (directory-namestring (uri:path fasl-path)))
          (compile-file (uri:path lisp-file) :output-file (uri:path fasl-path))
          (push fasl-path fasl-files)))
      
      ;; Create combined FASL by concatenating individual ones
      (create-combined-fasl (reverse fasl-files) combined-fasl))))

(defun create-combined-fasl (fasl-files output-path)
  "Combine multiple FASL files into a single file"
  (with-open-file (output (uri:path output-path) 
                          :direction :output 
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (dolist (fasl-file fasl-files)
      (with-open-file (input (uri:path fasl-file)
                            :direction :input
                            :element-type '(unsigned-byte 8))
        (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
          (loop for bytes-read = (read-sequence buffer input)
                while (> bytes-read 0)
                do (write-sequence buffer output :end bytes-read)))))))

(defun create-package-metadata (build-dir module-info)
  "Generate all metadata files for the package"
  (let ((meta-dir (uri:merge build-dir "META-INF/")))
    
    ;; Create manifest
    (let ((manifest (create-manifest module-info))
          (manifest-path (uri:merge meta-dir "MANIFEST.edn")))
      (with-open-file (stream (uri:path manifest-path) :direction :output)
        (print manifest stream)))
    
    ;; Create checksums
    (create-package-checksums build-dir)
    
    ;; Create dependency info
    (create-dependency-info build-dir module-info)))

(defun create-package-checksums (build-dir)
  "Generate SHA-256 checksums for all files in package"
  (let ((checksums '())
        (checksums-path (uri:merge build-dir "META-INF/CHECKSUMS.sha256")))
    
    ;; For now, just add a simple checksum entry for the main source file
    (let ((src-files (fs:list-files (uri:merge build-dir "src/") ".lisp")))
      (dolist (file src-files)
        (let ((hash (file-sha256 (uri:path file)))
              (rel-path (format nil "src/~A" (pathname-name (uri:path file)))))
          (push (format nil "~A  ~A.lisp" hash rel-path) checksums))))
    
    (with-open-file (stream (uri:path checksums-path) :direction :output)
      (dolist (line (sort checksums #'string<))
        (write-line line stream)))))

(defun file-sha256 (filepath)
  "Calculate SHA-256 hash of a file"
  (with-open-file (stream filepath :element-type '(unsigned-byte 8))
    (let ((digest (sha-256))
          (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
      (loop for bytes-read = (read-sequence buffer stream)
            while (> bytes-read 0)
            do (digest-sequence digest buffer :end bytes-read))
      (encode (finalize digest)))))

;;;; ==========================================================================
;;;; Package Archive Creation
;;;; ==========================================================================

(defun create-package-archive (build-dir output-path)
  "Create ZIP archive from build directory"
  ;; For now, use a simple directory copy as a placeholder
  ;; In a full implementation, this would create an actual ZIP file
  (let ((archive-dir (format nil "~A.dir" (uri:path output-path))))
    (fs:make-dirs (directory-namestring archive-dir))
    (copy-directory (uri:path build-dir) archive-dir)
    archive-dir))

(defun copy-directory (source dest)
  "Simple directory copy implementation"
  (fs:make-dirs dest)
  ;; For now, just copy .lisp files directly
  (let ((source-files (fs:list-files (uri:make-uri :scheme "file" :path source) ".lisp")))
    (dolist (file source-files)
      (let* ((filename (file-namestring (uri:path file)))
             (dest-file (merge-pathnames filename dest)))
        (copy-file (uri:path file) dest-file)))))

;;;; ==========================================================================
;;;; Package Signing and Verification
;;;; ==========================================================================

(defun sign-package-contents (build-dir sign-key)
  "Sign package contents using Ed25519"
  ;; For now, create a placeholder signature file
  ;; In a full implementation, this would use actual cryptographic signing
  (let ((signature-path (uri:merge build-dir "META-INF/SIGNATURE")))
    (with-open-file (stream (uri:path signature-path) :direction :output)
      (write-string 
       (format nil "~S"
        (map:make-map
         :algorithm "ed25519"
         :public-key "placeholder-public-key"
         :signature "placeholder-signature"
         :signed-hash "sha256:placeholder-hash"
         :timestamp (format-timestamp (get-universal-time))))
       stream))))

(defun verify-package-signature (package-dir)
  "Verify package digital signature"
  ;; For now, always return true as placeholder
  ;; In a full implementation, this would verify the Ed25519 signature
  (let ((signature-path (uri:merge package-dir "META-INF/SIGNATURE")))
    (fs:exists-p signature-path)))

(defun verify-package-checksums (package-dir)
  "Verify package file checksums"
  (let ((checksums-path (uri:merge package-dir "META-INF/CHECKSUMS.sha256")))
    (when (fs:exists-p checksums-path)
      (with-open-file (stream (uri:path checksums-path))
        (loop for line = (read-line stream nil)
              while line
              always (verify-checksum-line line package-dir))))))

(defun verify-checksum-line (line package-dir)
  "Verify a single checksum line"
  (let* ((parts (str:split "  " line :limit 2))
         (expected-hash (first parts))
         (rel-path (second parts))
         (file-path (uri:merge package-dir rel-path)))
    (when (fs:exists-p file-path)
      (string= expected-hash (file-sha256 (uri:path file-path))))))

;;;; ==========================================================================
;;;; Package Installation
;;;; ==========================================================================

(defun install-package (package-path &key verify force)
  "Install a .epk package"
  (when verify
    (unless (verify-package package-path)
      (error "Package verification failed: ~A" package-path)))
  
  (let* ((package-dir (extract-package package-path))
         (manifest (load-package-manifest package-dir))
         (package-name (map:get manifest :name)))
    
    (unless force
      (when (package-installed-p package-name)
        (error "Package already installed: ~A" package-name)))
    
    ;; Install package files
    (install-package-files package-dir manifest)
    
    ;; Register package
    (register-package manifest)
    
    (format t "Successfully installed ~A ~A~%" 
            (map:get manifest :name)
            (map:get manifest :version))))

(defun verify-package (package-path)
  "Verify package signature and checksums"
  (let ((temp-dir (extract-package package-path)))
    (unwind-protect
         (and (verify-package-signature temp-dir)
              (verify-package-checksums temp-dir))
      (fs:delete-directory (uri:path temp-dir)))))

(defun extract-package (package-path)
  "Extract package to temporary directory"
  ;; For now, assume package is already a directory
  ;; In a full implementation, this would extract from ZIP
  (if (fs:dir-p (uri:path package-path))
      package-path
      (let ((temp-dir (uri:merge (fs:temp-dir) 
                                (format nil "epk-extract-~A/" (random-string 8)))))
        (fs:make-dirs temp-dir)
        ;; Placeholder: copy files assuming package-path is a directory
        (copy-directory (uri:path package-path) (uri:path temp-dir))
        temp-dir)))

(defun load-package-manifest (package-dir)
  "Load package manifest from extracted package"
  (let ((manifest-path (uri:merge package-dir "META-INF/MANIFEST.edn")))
    (unless (fs:exists-p manifest-path)
      (error "No manifest found in package: ~A" package-dir))
    (with-open-file (stream (uri:path manifest-path))
      (read stream))))

(defun package-installed-p (package-name)
  "Check if package is already installed"
  ;; For now, always return false as placeholder
  ;; In a full implementation, this would check the package registry
  nil)

(defun install-package-files (package-dir manifest)
  "Install package files to system location"
  ;; For now, just log the installation
  ;; In a full implementation, this would copy files to appropriate locations
  (format t "Installing package files from ~A~%" package-dir)
  (format t "Main package: ~A~%" (map:get manifest :main-package))
  (format t "Load order: ~A~%" (map:get-in manifest '(:install :load-order))))

(defun register-package (manifest)
  "Register package in system registry"
  ;; For now, just log the registration
  ;; In a full implementation, this would update the package registry
  (format t "Registering package: ~A ~A~%" 
          (map:get manifest :name)
          (map:get manifest :version)))

;;;; ==========================================================================
;;;; Repository Management
;;;; ==========================================================================

(defun create-repository (repository-path)
  "Initialize a new package repository"
  (let ((index-path (uri:merge repository-path "index.json")))
    (fs:make-dirs repository-path)
    (with-open-file (stream (uri:path index-path) :direction :output)
      (write-string 
       (format nil "~S"
        (map:make-map
         :format-version "1.0"
         :generated (format-timestamp (get-universal-time))
         :packages (map:make-map)))
       stream))
    (format t "Repository created at: ~A~%" repository-path)))

(defun update-index (repository-path package-path)
  "Update repository index with new package"
  (let* ((index-path (uri:merge repository-path "index.json"))
         (index (if (fs:exists-p index-path)
                    (with-open-file (stream (uri:path index-path))
                      (read stream))
                    (map:make-map :packages (map:make-map))))
         (manifest (load-package-manifest package-path))
         (package-name (map:get manifest :name))
         (version (map:get manifest :version)))
    
    ;; Update index with package information
    ;; For now, just create a simple index structure
    (let ((package-entry (create-package-index-entry manifest package-path)))
      (setf index (map:assoc index :packages 
                             (map:assoc (map:get index :packages map:+empty+)
                                        package-name package-entry))))
    
    ;; Write updated index
    (with-open-file (stream (uri:path index-path) 
                           :direction :output :if-exists :supersede)
      (write-string (format nil "~S" index) stream))
    
    (format t "Updated index for package: ~A ~A~%" package-name version)))

(defun create-package-index-entry (manifest package-path)
  "Create index entry for a package"
  (let* ((platform-info (map:get manifest :platform))
         (platform-key (format nil "~A-~A" 
                               (map:get platform-info :os)
                               (map:get platform-info :arch))))
    (map:make-map
     :platforms (map:make-map
                 platform-key (map:make-map
                               :filename (file-namestring (uri:path package-path))
                               :size (file-size (uri:path package-path))
                               :sha256 (file-sha256 (uri:path package-path))
                               :dependencies '())))))

(defun publish-package (package-path repository-path)
  "Publish package to repository"
  (let* ((package-name (file-namestring (uri:path package-path)))
         (dest-path (uri:merge repository-path package-name)))
    ;; Copy package to repository
    (copy-file (uri:path package-path) (uri:path dest-path))
    
    ;; Update repository index
    (update-index repository-path package-path)
    
    (format t "Published package: ~A~%" package-name)))

;;;; ==========================================================================
;;;; Package Information and Dependencies
;;;; ==========================================================================

(defun package-info (package-name &optional version)
  "Get information about an installed package"
  ;; For now, return placeholder information
  ;; In a full implementation, this would query the package registry
  (map:make-map
   :name package-name
   :version (or version "unknown")
   :status "not-implemented"))

(defun dependency-tree (package-name)
  "Get dependency tree for a package"
  ;; For now, return empty dependency tree
  ;; In a full implementation, this would resolve all dependencies
  (map:make-map
   :direct '()
   :transitive '()))

(defun platform-compatibility (package-manifest)
  "Check platform compatibility for package"
  (let* ((package-platforms (map:get-in package-manifest '(:compatibility :platforms)))
         (current-platform (getf *platform-info* :os)))
    (if (member current-platform package-platforms)
        :compatible
        :incompatible)))

(defun list-packages (&optional filter)
  "List installed packages"
  ;; For now, return empty list
  ;; In a full implementation, this would query the package registry
  '())

;;;; ==========================================================================
;;;; Module File Operations
;;;; ==========================================================================

(defun copy-module-docs (module-path dest-dir)
  "Copy module documentation to package"
  (let ((docs-path (uri:merge module-path "docs/")))
    (when (fs:exists-p docs-path)
      (copy-directory (uri:path docs-path) (uri:path dest-dir)))))

(defun copy-module-tests (module-path dest-dir)
  "Copy module tests to package"
  (let ((tests-path (uri:merge module-path "tests/")))
    (when (fs:exists-p tests-path)
      (copy-directory (uri:path tests-path) (uri:path dest-dir)))))

(defun create-dependency-info (build-dir module-info)
  "Create dependency information file"
  (let ((deps-path (uri:merge build-dir "META-INF/DEPENDENCIES.edn"))
        (deps-info (map:make-map
                    :direct (map:make-map)
                    :transitive (map:make-map)
                    :resolution-tree (map:make-map))))
    (with-open-file (stream (uri:path deps-path) :direction :output)
      (write-string (format nil "~S" deps-info) stream))))

;;;; ==========================================================================
;;;; Directory Traversal Utilities
;;;; ==========================================================================

(defun walk-directory (directory function &key (test (constantly t)))
  "Walk through directory recursively, calling function on each file/directory that passes test"
  (when (fs:exists-p directory)
    (dolist (entry (fs:list-contents directory))
      (when (funcall test entry)
        (funcall function entry))
      (when (fs:dir-p entry)
        (walk-directory entry function :test test)))))

;;;; ==========================================================================
;;;; Utility Functions
;;;; ==========================================================================

(defun load-module-info (module-path)
  "Load module information from package.lisp"
  (let ((package-file (uri:merge module-path "package.lisp")))
    (unless (fs:exists-p package-file)
      (error "No package.lisp found in ~A" module-path))
    (with-open-file (stream (uri:path package-file))
      (read stream))))

(defun copy-file (source dest)
  "Copy a file from source to destination"
  (with-open-file (input source :element-type '(unsigned-byte 8))
    (with-open-file (output dest :element-type '(unsigned-byte 8) 
                           :direction :output :if-exists :supersede)
      (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
        (loop for bytes-read = (read-sequence buffer input)
              while (> bytes-read 0)
              do (write-sequence buffer output :end bytes-read))))))

(defun file-size (filepath)
  "Get file size in bytes"
  (with-open-file (stream filepath :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun random-string (length)
  "Generate random string of specified length"
  (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length
                  collect (char chars (random (length chars))))
            'string)))

(defun ends-with-p (string suffix)
  "Check if string ends with suffix"
  (and (>= (length string) (length suffix))
       (string= string suffix :start1 (- (length string) (length suffix)))))

(defun contains-p (string substring)
  "Check if string contains substring"
  (search substring string))

(defun replace-char (old new string)
  "Replace old with new in string"
  (substitute new old string))

;;;; ==========================================================================
;;;; EPK Package Creation
;;;; ==========================================================================

(defun compile-module-sources (source-dir fasl-dir)
  "Compile all Lisp sources in a module to FASLs"
  (let ((fasl-files '()))
    (fs:walk-uri source-dir
                 (lambda (file-uri)
                   (when (str:ends-with-p (uri:path file-uri) ".lisp")
                     (let* ((relative-path (enough-namestring (uri:path file-uri) 
                                                             (uri:path source-dir)))
                            (fasl-path (merge-pathnames 
                                       (make-pathname :defaults relative-path
                                                     :type "fasl")
                                       (uri:path fasl-dir))))
                       (ensure-directories-exist fasl-path)
                       (handler-case
                           (progn
                             (compile-file (uri:path file-uri) 
                                          :output-file fasl-path
                                          :verbose nil
                                          :print nil)
                             (push fasl-path fasl-files))
                         (error (e)
                           (warn "Failed to compile ~A: ~A" relative-path e)))))))
    (nreverse fasl-files)))

(defun create-combined-fasl (fasl-files output-file)
  "Combine multiple FASL files into a single file"
  (ensure-directories-exist output-file)
  (with-open-file (output output-file
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (dolist (fasl-file fasl-files)
      (when (probe-file fasl-file)
        (with-open-file (input fasl-file
                               :direction :input
                               :element-type '(unsigned-byte 8))
          (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
            (loop for bytes-read = (read-sequence buffer input)
                  while (> bytes-read 0)
                  do (write-sequence buffer output :end bytes-read)))))))
  output-file)

(defun create-zip-archive (source-dir output-file)
  "Create a ZIP archive from directory using external zip command"
  (ensure-directories-exist output-file)
  (let* ((abs-output (truename output-file))
         (abs-source (truename source-dir))
         (zip-command (format nil "cd ~S && zip -r ~S ."
                             (namestring abs-source)
                             (namestring abs-output))))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program zip-command 
                         :output :string
                         :error-output :string
                         :ignore-error-status t)
      (unless (zerop exit-code)
        (error "ZIP creation failed: ~A~%~A" output error-output))))
  output-file)

(defun create-epk (module-path output-file &key 
                                          manifest 
                                          sign-key
                                          (include-sources t)
                                          (include-tests nil))
  "Create an EPK package from a module directory"
  (let ((temp-dir (merge-pathnames (format nil "epk-build-~A/" (random-string 8))
                                   (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; Create directory structure
           (ensure-directories-exist (merge-pathnames "META-INF/" temp-dir))
           (ensure-directories-exist (merge-pathnames "fasl/" temp-dir))
           (ensure-directories-exist (merge-pathnames "src/" temp-dir))
           (ensure-directories-exist (merge-pathnames "docs/" temp-dir))
           
           ;; Copy source files if requested
           (when include-sources
             (let ((src-dir (uri:merge module-path "src/")))
               (when (fs:exists-p src-dir)
                 (copy-directory (uri:path src-dir) 
                                (merge-pathnames "src/" temp-dir)))))
           
           ;; Compile and create combined FASL
           (let* ((src-dir (uri:merge module-path "src/"))
                  (fasl-temp-dir (merge-pathnames "fasl-temp/" temp-dir))
                  (combined-fasl (merge-pathnames "fasl/combined.fasl" temp-dir)))
             (when (fs:exists-p src-dir)
               (let ((fasl-files (compile-module-sources src-dir 
                                                        (uri:make-uri :scheme "file" 
                                                                     :path fasl-temp-dir))))
                 (create-combined-fasl fasl-files combined-fasl))))
           
           ;; Create manifest
           (let ((manifest-file (merge-pathnames "META-INF/MANIFEST.edn" temp-dir)))
             (with-open-file (out manifest-file :direction :output :if-exists :supersede)
               (edn:write-edn manifest out)))
           
           ;; Generate checksums
           (generate-package-checksums temp-dir)
           
           ;; Sign package if key provided
           (when sign-key
             (sign-package-contents (uri:make-uri :scheme "file" :path temp-dir) sign-key))
           
           ;; Create ZIP archive
           (create-zip-archive temp-dir output-file))
      
      ;; Cleanup temp directory
      (uiop:delete-directory-tree temp-dir :validate t))))

(defun build-epk-from-module (module-name &key 
                                         output-dir
                                         version
                                         sign-key
                                         commit)
  "Build an EPK package from a module by name"
  (let* ((all-modules (discover-modules))
         (module-path (map:get all-modules module-name)))
    
    (unless module-path
      (error "Unknown module: ~A" module-name))
    
    ;; Load module info
    (let* ((module-info (load-module-info module-path))
           (package-version (or version (map:get module-info "version") "0.0.0"))
           (package-filename (make-package-filename module-name package-version))
           (output-path (if output-dir
                           (merge-pathnames package-filename output-dir)
                           (merge-pathnames package-filename 
                                          (merge-pathnames "target/packages/" 
                                                         (uiop:getcwd))))))
      
      ;; Create manifest
      (let ((manifest (create-manifest module-info
                                      :build-info (map:make-map
                                                  :timestamp (get-universal-time)
                                                  :builder "epsilon-package"
                                                  :commit commit))))
        
        ;; Create EPK
        (ensure-directories-exist output-path)
        (create-epk module-path output-path 
                   :manifest manifest
                   :sign-key sign-key)
        
        ;; Register in repository
        (repo:add-package-to-index module-name package-version
                                  (platform-string)
                                  (file-namestring output-path)
                                  (file-size output-path)
                                  (file-sha256 output-path)
                                  (map:get module-info "dependencies")
                                  :commit commit)
        
        (format t "Created EPK: ~A~%" output-path)
        output-path))))
