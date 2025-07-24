;;;; Release tool for building and packaging Epsilon distributions

(in-package :cl-user)

(defpackage :epsilon.tool.release
  (:use :cl)
  (:local-nicknames
   (:fs :epsilon.sys.fs)
   (:path :epsilon.path)
   (:map :epsilon.map)
   (:str :epsilon.string)
   (:seq :epsilon.sequence)
   (:build :epsilon.tool.build)
   (:json :epsilon.json)
   (:digest :epsilon.digest)
   (:hex :epsilon.hex)
   (:log :epsilon.log)
   (:dev :epsilon.tool.dev))
  (:export
   :main
   :build-release
   :build-epk
   :build-distribution
   :generate-index
   :build-all
   :*release-config*
   ;; Configuration structures
   :make-release-config
   :release-config-version
   :release-config-modules  
   :release-config-platforms
   :release-config-output-dir
   ;; Internal functions for testing
   :parse-platforms
   :parse-arguments
   :get-module-info
   :copy-directory-contents
   :generate-checksum))

(in-package :epsilon.tool.release)

(defun copy-directory-contents (source dest)
  "Copy directory contents from source to dest"
  (let ((source-str (if (typep source 'path:path)
                        (path:path-string source)
                        source))
        (dest-str (if (typep dest 'path:path)
                      (path:path-string dest)
                      dest)))
    (fs:make-dirs dest-str)
    (dolist (entry (fs:list-dir source-str))
      (let ((source-path (path:path-string (path:path-join source-str entry)))
            (dest-path (path:path-string (path:path-join dest-str entry))))
        (cond
          ((fs:dir-p source-path)
           (copy-directory-contents source-path dest-path))
          ((fs:file-p source-path)
           (fs:copy-file source-path dest-path)))))))

(defvar *release-config* nil
  "Current release configuration")

(defparameter *default-platforms*
  '((:os :linux :arch :x86-64)
    (:os :darwin :arch :x86-64)
    (:os :darwin :arch :arm64))
  "Default platforms to build for")

(defstruct release-config
  version
  modules
  platforms
  output-dir
  core-version)

(defun discover-modules ()
  "Discover all available modules"
  (map:keys build:*modules*))

(defun get-module-info (module)
  "Get module package.edn info"
  (let* ((module-info (build:get-module module))
         (module-dir (path:path-from-uri (build:module-uri module-info)))
         (package-file (path:path-join module-dir "package.edn")))
    (when (fs:exists-p package-file)
      (with-open-file (stream package-file)
        ;; For now, read as s-expression until EDN is available
        (read stream)))))

(defun get-platform-modules (platform modules)
  "Filter modules compatible with platform"
  (remove-if-not
   (lambda (module)
     (platform-compatible-p module platform))
   modules))

(defun platform-compatible-p (module platform)
  "Check if module is compatible with platform"
  (let ((info (get-module-info module)))
    (let ((platforms (map:get info "platforms")))
      (if platforms
          (member (getf platform :os) platforms :test #'string-equal)
          t)))) ; No platform restriction means compatible with all

(defun build-module-epk (module version output-dir)
  "Build EPK package for a module"
  (let* ((info (get-module-info module))
         (module-name (if (stringp module)
                          (string-downcase module)
                          (string-downcase (symbol-name module))))
         (epk-file (path:path-join output-dir (format nil "~A-~A.epk" module-name version))))
    (format t "Building EPK for ~A~%" module)
    
    ;; First build the module
    (build:build module :force t)
    
    ;; Create EPK structure
    (let ((temp-dir (path:path-string (path:path-join (fs:temp-dir) (format nil "epk-~A" (gensym))))))
      ;; Create the temp directory
      (fs:make-dirs temp-dir)
      (unwind-protect
          (progn
            ;; Copy module files
            (copy-module-files module temp-dir)
            
            ;; Generate metadata
            (generate-epk-metadata module version temp-dir info)
            
            ;; Create EPK archive
            (create-epk-archive temp-dir epk-file))
        (fs:delete-directory temp-dir)))
    
    epk-file))

(defun copy-module-files (module dest-dir)
  "Copy module files to destination"
  (let* ((module-info (build:get-module module))
         (module-dir (path:path-from-uri (build:module-uri module-info)))
         (target-dir (path:path-join module-dir "target"))
         (module-dest (path:path-join dest-dir "module" (if (stringp module)
                                                               (string-downcase module)
                                                               (string-downcase (symbol-name module))))))
    
    ;; Create directory structure
    (fs:make-dirs (path:path-string module-dest))
    
    ;; Copy source files
    (copy-directory-contents (path:path-string (path:path-join module-dir "src")) 
                            (path:path-string (path:path-join module-dest "src")))
    
    ;; Copy compiled files
    (when (fs:dir-p (path:path-string target-dir))
      (copy-directory-contents (path:path-string target-dir) 
                              (path:path-string (path:path-join module-dest "target"))))
    
    ;; Copy package.edn if it exists
    (let ((package-file (path:path-string (path:path-join module-dir "package.edn"))))
      (when (fs:exists-p package-file)
        (fs:copy-file package-file
                      (path:path-string (path:path-join module-dest "package.edn")))))))

(defun generate-epk-metadata (module version dest-dir info)
  "Generate EPK metadata files"
  (let ((metadata map:+empty+))
    (setf metadata (map:assoc metadata "name" (if (stringp module)
                                                       (string-downcase module)
                                                       (string-downcase (symbol-name module)))))
    (setf metadata (map:assoc metadata "version" version))
    (setf metadata (map:assoc metadata "type" "module"))
    (setf metadata (map:assoc metadata "created" (get-universal-time)))
    
    ;; Add dependencies only if info is not nil
    (when info
      (let ((deps (map:get info "dependencies")))
        (when deps
          (setf metadata (map:assoc metadata "dependencies" deps))))
      
      ;; Add platform info
      (let ((platforms (map:get info "platforms")))
        (when platforms
          (setf metadata (map:assoc metadata "platforms" platforms)))))
    
    ;; Write metadata.json
    (with-open-file (stream (path:path-string (path:path-join dest-dir "metadata.json"))
                           :direction :output
                           :if-exists :supersede)
      (json:encode metadata stream))))

(defun create-epk-archive (source-dir output-file)
  "Create EPK archive from directory"
  (let ((parent-path (path:path-parent output-file)))
    (when parent-path
      (fs:make-dirs (path:path-string parent-path))))
  ;; Use tar for now until zip module is available
  (sb-ext:run-program "tar" 
                     (list "czf" (path:path-string output-file) "-C" 
                           (let ((parent (path:path-parent (path:make-path source-dir))))
                             (if parent (path:path-string parent) "."))
                           (let ((p (path:make-path source-dir)))
                             (or (path:path-name p) "")))
                     :search t
                     :output t
                     :error :output))

(defun build-core-image (version output-dir)
  "Build SBCL core image with Epsilon"
  (let ((core-file (path:path-join output-dir "epsilon-core")))
    (log:info "Building core image version ~A" version)
    
    ;; Save current image with epsilon loaded
    (sb-ext:save-lisp-and-die 
     core-file
     :toplevel #'epsilon-toplevel
     :executable nil
     :compression t
     :purify t)))

(defun epsilon-toplevel ()
  "Toplevel function for epsilon core"
  (handler-case
      (progn
        ;; Start REPL or process command line
        (sb-impl::toplevel-init))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (sb-ext:quit :unix-status 1))))

(defun build-distribution (platform version output-dir)
  "Build platform-specific distribution"
  (let* ((os (getf platform :os))
         (arch (getf platform :arch))
         (dist-name (format nil "epsilon-~A-~A-~A" version 
                           (string-downcase (symbol-name os))
                           (string-downcase (symbol-name arch))))
         (dist-dir (path:path-join output-dir dist-name)))
    
    (log:info "Building distribution for ~A ~A" os arch)
    
    ;; Create distribution directory
    (fs:make-dirs dist-dir)
    
    ;; Copy core image
    (fs:copy-file (path:path-join output-dir "epsilon-core")
                  (path:path-join dist-dir "epsilon-core"))
    
    ;; Create wrapper script
    (create-wrapper-script dist-dir os)
    
    ;; Copy documentation
    (fs:copy-file "README.md" (path:path-join dist-dir "README.md"))
    (fs:copy-file "LICENSE" (path:path-join dist-dir "LICENSE"))
    
    ;; Create tarball
    (let ((tarball (format nil "~A.tar.gz" dist-name)))
      (create-distribution-archive dist-dir tarball output-dir)
      
      ;; Generate checksum
      (generate-checksum (path:path-join output-dir tarball))
      
      tarball)))

(defun create-wrapper-script (dist-dir os)
  "Create platform-specific wrapper script"
  (declare (ignore os)) ; TODO: Make OS-specific scripts
  (let ((script-path (path:path-join dist-dir "epsilon")))
    (with-open-file (stream script-path
                           :direction :output
                           :if-exists :supersede)
      (format stream "#!/bin/sh~%")
      (format stream "SCRIPT_DIR=$(dirname \"$0\")~%")
      (format stream "exec sbcl --core \"$SCRIPT_DIR/epsilon-core\" \"$@\"~%"))
    
    ;; Make executable
    #-win32 (sb-posix:chmod script-path #o755)
    #+win32 nil)) ; Windows doesn't need chmod for executables

(defun create-distribution-archive (source-dir archive-name output-dir)
  "Create distribution tarball"
  (let ((cwd (fs:current-directory)))
    (unwind-protect
         (progn
           (fs:change-directory output-dir)
           (sb-ext:run-program "tar" 
                              (list "czf" archive-name 
                                    (let ((p (path:make-path source-dir)))
                                      (or (path:path-name p) "")))
                              :search t
                              :output t
                              :error :output))
      (fs:change-directory cwd))))

(defun generate-checksum (file)
  "Generate SHA256 checksum for file"
  (let* ((file-str (if (typep file 'path:path)
                       (path:path-string file)
                       file))
         (checksum (with-open-file (stream file-str :element-type '(unsigned-byte 8))
                     (let ((digest (digest:make-digest :sha-256)))
                       (digest:digest-stream digest stream)
                       (digest:get-digest digest))))
         (checksum-file (str:concat file-str ".sha256")))
    (with-open-file (stream checksum-file
                           :direction :output
                           :if-exists :supersede)
      (format stream "~A  ~A~%" 
              (hex:u8-to-hex checksum)
              (let ((p (if (typep file 'path:path)
                           file
                           (path:make-path file))))
                (or (path:path-name p) ""))))))

(defun generate-index (modules version output-dir)
  "Generate release index/catalog"
  (let ((index map:+empty+)
        (packages '()))
    
    ;; Add each module to index
    (dolist (module modules)
      (let* ((info (or (get-module-info module) map:+empty+))
             (module-name (if (stringp module)
                              (string-downcase module)
                              (string-downcase (symbol-name module))))
             (entry map:+empty+))
        
        (setf entry (map:assoc entry "name" module-name))
        (setf entry (map:assoc entry "version" version))
        (setf entry (map:assoc entry "description" (map:get info "description" "")))
        (setf entry (map:assoc entry "platforms" (map:get info "platforms" '())))
        (setf entry (map:assoc entry "dependencies" (map:get info "dependencies" '())))
        (setf entry (map:assoc entry "file" (format nil "~A-~A.epk" module-name version)))
        
        (push entry packages)))
    
    (setf index (map:assoc index "version" "1.0"))
    (setf index (map:assoc index "generated" (get-universal-time)))
    (setf index (map:assoc index "packages" packages))
    
    ;; Write index.json
    (format t "DEBUG: About to encode index JSON~%")
    (format t "DEBUG: index type = ~A~%" (type-of index))
    (format t "DEBUG: packages length = ~A~%" (length packages))
    (with-open-file (stream (path:path-string (path:path-join output-dir "index.json"))
                           :direction :output
                           :if-exists :supersede)
      (json:encode index stream :pretty t))))

(defun build-release (&key version modules platforms output-dir)
  "Build complete release with all artifacts"
  (format t "DEBUG: Starting build-release with version=~A modules=~A~%" version modules)
  (handler-case
      (let* ((all-modules (if modules
                              (if (listp modules) modules (list modules))
                              (list "epsilon.core")))  ; Just use core for now
             (release-platforms (or platforms *default-platforms*))
             (out-dir (or output-dir "target/release")))
        (format t "DEBUG: all-modules=~A platforms=~A out-dir=~A~%" all-modules release-platforms out-dir)
        (let ((config (make-release-config 
                       :version version
                       :modules all-modules
                       :platforms release-platforms
                       :output-dir out-dir)))
          (format t "DEBUG: config created successfully~%")
          
          (setf *release-config* config)
          
          (format t "Building release ~A~%" version)
          (format t "Modules: ~A~%" all-modules)
          (format t "DEBUG: all-modules type = ~A~%" (type-of all-modules))
          
          ;; Ensure output directory
          (fs:make-dirs out-dir)
          (fs:make-dirs (path:path-join out-dir "epk"))
          
          ;; Build EPKs for each module
          (dolist (module all-modules)
            (handler-case
                (build-module-epk module version (path:path-join out-dir "epk"))
              (error (e)
                (format t "ERROR: Failed to build EPK for ~A: ~A~%" module e))))
          
          ;; Skip core image build for now - it's complex
          (format t "Skipping core image build for testing~%")
          
          ;; Skip distributions for now - focus on EPK builds
          (format t "Skipping distribution builds for testing~%")
          
          ;; Generate release index
          (format t "Generating release index...~%")
          (generate-index all-modules version out-dir)
          
          (format t "Release build complete in ~A~%" out-dir)
          
          config))
    (error (e)
      (format t "ERROR: build-release failed: ~A~%" e)
      (error e))))

(defun parse-arguments (args)
  "Parse release command arguments"
  (let ((options map:+empty+))
    (loop for arg = (pop args)
          while arg
          do (cond
               ((string= arg "--version")
                (setf options (map:assoc options :version (pop args))))
               ((string= arg "--modules")
                (setf options (map:assoc options :modules 
                                      (seq:realize (str:split #\, (pop args))))))
               ((string= arg "--output")
                (setf options (map:assoc options :output-dir (pop args))))
               ((string= arg "--platforms")
                (setf options (map:assoc options :platforms
                                      (parse-platforms (pop args)))))
               (t
                (error "Unknown option: ~A" arg))))
    options))

(defun parse-platforms (spec)
  "Parse platform specification"
  (mapcar (lambda (p)
            (let* ((dash-pos (position #\- p))
                   (os (if dash-pos (subseq p 0 dash-pos) p))
                   (arch (if dash-pos (subseq p (1+ dash-pos)) "")))
              (list :os (intern (string-upcase os) :keyword)
                    :arch (intern (string-upcase arch) :keyword))))
          (seq:realize (str:split #\, spec))))

(defun build-all (&key 
                   force 
                   (error-behavior :halt) 
                   (warning-behavior :ignore)
                   include-tests
                   include-platform)
  "Build all registered modules.
  
  FORCE - Force compilation of all build steps regardless of timestamps
  ERROR-BEHAVIOR - How to handle compilation errors: :halt (default), :ignore, :print
  WARNING-BEHAVIOR - How to handle compilation warnings: :ignore (default), :halt, :print
  INCLUDE-TESTS - Also build test files (default NIL)
  INCLUDE-PLATFORM - Also build platform-specific modules (default NIL)"
  
  ;; Ensure modules are discovered
  (when (zerop (map:size build:*modules*))
    (build:register-modules))
  
  (let ((modules-to-build (build:list-modules :include-platform include-platform))
        (failed-modules '())
        (succeeded-modules '()))
    
    (format t "~&;;; Building all modules (~D total)~%" (length modules-to-build))
    
    (dolist (module modules-to-build)
      (format t "~&~%;;; ======================================~%")
      (format t ";;; Building module: ~A~%" module)
      (format t ";;; ======================================~%")
      
      (handler-case
          (progn
            (build:build module 
                         :force force
                         :error-behavior error-behavior
                         :warning-behavior warning-behavior
                         :include-tests include-tests)
            (push module succeeded-modules))
        (error (e)
          (format t "~&;;; ERROR building ~A: ~A~%" module e)
          (push module failed-modules)
          ;; If error-behavior is :halt, re-signal the error
          (when (eq error-behavior :halt)
            (error e)))))
    
    ;; Summary
    (format t "~&~%;;; ======================================~%")
    (format t ";;; Build Summary~%")
    (format t ";;; ======================================~%")
    (format t ";;; Total modules: ~D~%" (length modules-to-build))
    (format t ";;; Succeeded: ~D~%" (length succeeded-modules))
    (when failed-modules
      (format t ";;; Failed: ~D (~{~A~^, ~})~%" 
              (length failed-modules) 
              (nreverse failed-modules)))
    (format t ";;; ======================================~%")
    
    ;; Return success status
    (zerop (length failed-modules))))

(defun main (parsed-args)
  "Entry point for release command"
  (handler-case
      (let* ((options (epsilon.tool.dev:parsed-args-options parsed-args))
             (version (map:get options "version")))
        (unless version
          (error "Version required: --version VERSION"))
        
        (build-release 
         :version version
         :modules (let ((modules-str (map:get options "modules")))
                    (when modules-str (seq:realize (str:split #\, modules-str))))
         :platforms (let ((platforms-str (map:get options "platforms")))
                      (when platforms-str (parse-platforms platforms-str)))
         :output-dir (map:get options "output-dir")))
    (error (e)
      (format *error-output* "Release error: ~A~%" e)
      (sb-ext:quit :unix-status 1))))
