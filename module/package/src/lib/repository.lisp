(defpackage :epsilon.lib.package.repository
  (:use
   :cl
   :epsilon.lib.package)
  (:local-nicknames
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.lib.uri)
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)
   (:digest :epsilon.lib.digest)
   (:hex :epsilon.lib.hex))
  (:export
   ;; Repository management
   :*default-repository*
   :initialize-repository
   :repository-path
   :get-package-path
   
   ;; Package cache
   :package-cache-valid-p
   :get-cached-package
   :cache-package
   
   ;; Module registry
   :register-module-package
   :get-module-package
   :list-module-packages))

(in-package :epsilon.lib.package.repository)

;;;; ==========================================================================
;;;; Local Repository Management
;;;; ==========================================================================

(defparameter *default-repository*
  (uri:merge (uri:make-uri :scheme "file" :path (fs:home-dir)) ".epsilon/repository/")
  "Default local repository location")

(defun initialize-repository (&optional (repo-path *default-repository*))
  "Initialize local repository structure"
  (let ((packages-dir (uri:merge repo-path "packages/"))
        (cache-dir (uri:merge repo-path "cache/"))
        (index-file (uri:merge repo-path "index.edn")))
    
    ;; Create directory structure
    (fs:make-dirs packages-dir)
    (fs:make-dirs cache-dir)
    
    ;; Create initial index if doesn't exist
    (unless (fs:exists-p index-file)
      (with-open-file (stream (uri:path index-file) :direction :output)
        (write (map:make-map
                :version "1.0"
                :packages (map:make-map)
                :cache (map:make-map))
               :stream stream)))
    
    repo-path))

(defun repository-path (&optional (repo-path *default-repository*))
  "Get the repository path, initializing if necessary"
  (if (fs:exists-p repo-path)
      repo-path
      (initialize-repository repo-path)))

(defun get-package-path (package-name version &optional (repo-path *default-repository*))
  "Get path to a specific package version in repository"
  (uri:merge (repository-path repo-path)
             (format nil "packages/~A/~A/" package-name version)))

;;;; ==========================================================================
;;;; Package Cache Management
;;;; ==========================================================================

(defstruct cache-entry
  "Cache entry for a built package"
  package-name
  version
  timestamp
  source-hash
  fasl-path
  manifest)

(defun load-cache-index (&optional (repo-path *default-repository*))
  "Load the cache index from repository"
  (let ((index-file (uri:merge (repository-path repo-path) "cache/index.edn")))
    (if (fs:exists-p index-file)
        (with-open-file (stream (uri:path index-file))
          (read stream))
        (map:make-map))))

(defun save-cache-index (index &optional (repo-path *default-repository*))
  "Save the cache index to repository"
  (let ((index-file (uri:merge (repository-path repo-path) "cache/index.edn")))
    (fs:make-dirs (uri:parent index-file))
    (with-open-file (stream (uri:path index-file) 
                           :direction :output
                           :if-exists :supersede)
      (write index :stream stream))))

(defun calculate-source-tree-hash (module-path)
  "Calculate hash of all source files in a module"
  (let ((digest (digest:make-digest :sha-256))
        (source-files '()))
    
    ;; Collect all .lisp files recursively
    (labels ((collect-files (path)
               (let ((files (fs:list-files path ".lisp")))
                 (dolist (file files)
                   (push file source-files))
                 ;; Simple recursive directory traversal
                 (dolist (entry (directory (merge-pathnames "*/" (uri:path path))))
                   (when (pathname-directory entry)
                     (collect-files (uri:make-uri :scheme "file" :path (namestring entry))))))))
      (collect-files module-path))
    
    ;; Sort for consistent hashing
    (setf source-files (sort source-files #'string< :key #'uri:path))
    
    ;; Hash each file
    (dolist (file source-files)
      (with-open-file (stream (uri:path file) :element-type '(unsigned-byte 8))
        (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
          (loop for bytes-read = (read-sequence buffer stream)
                while (> bytes-read 0)
                do (digest:digest-vector digest (subseq buffer 0 bytes-read))))))
    
    (hex:u8-to-hex (digest:get-digest digest))))

(defun package-cache-valid-p (package-name version module-path 
                             &optional (repo-path *default-repository*))
  "Check if cached package is still valid"
  (let* ((cache-index (load-cache-index repo-path))
         (cache-key (format nil "~A-~A" package-name version))
         (entry (map:get cache-index cache-key)))
    
    (when entry
      (let ((current-hash (calculate-source-tree-hash module-path))
            (cached-hash (getf entry :source-hash)))
        (string= current-hash cached-hash)))))

(defun get-cached-package (package-name version &optional (repo-path *default-repository*))
  "Get cached package if valid"
  (let* ((cache-index (load-cache-index repo-path))
         (cache-key (format nil "~A-~A" package-name version))
         (entry (map:get cache-index cache-key)))
    
    (when entry
      (let ((fasl-path (getf entry :fasl-path)))
        (when (fs:exists-p (uri:make-uri :scheme "file" :path fasl-path))
          entry)))))

(defun cache-package (package-name version module-path fasl-path manifest
                     &optional (repo-path *default-repository*))
  "Cache a built package"
  (let* ((cache-index (load-cache-index repo-path))
         (cache-key (format nil "~A-~A" package-name version))
         (source-hash (calculate-source-tree-hash module-path))
         (entry (map:make-map
                 :package-name package-name
                 :version version
                 :timestamp (get-universal-time)
                 :source-hash source-hash
                 :fasl-path (uri:path fasl-path)
                 :manifest manifest)))
    
    (setf cache-index (map:assoc cache-index cache-key entry))
    (save-cache-index cache-index repo-path)
    entry))

;;;; ==========================================================================
;;;; Module Package Registry
;;;; ==========================================================================

(defun load-module-registry (&optional (repo-path *default-repository*))
  "Load module package registry"
  (let ((registry-file (uri:merge (repository-path repo-path) "modules.edn")))
    (if (fs:exists-p registry-file)
        (with-open-file (stream (uri:path registry-file))
          (read stream))
        (map:make-map))))

(defun save-module-registry (registry &optional (repo-path *default-repository*))
  "Save module package registry"
  (let ((registry-file (uri:merge (repository-path repo-path) "modules.edn")))
    (with-open-file (stream (uri:path registry-file)
                           :direction :output
                           :if-exists :supersede)
      (write registry :stream stream))))

(defun register-module-package (module-name package-path manifest
                               &optional (repo-path *default-repository*))
  "Register a module's built package in the repository"
  (let ((registry (load-module-registry repo-path)))
    (setf registry 
          (map:assoc registry module-name
                     (map:make-map
                      :package-path (uri:path package-path)
                      :manifest manifest
                      :timestamp (get-universal-time))))
    (save-module-registry registry repo-path)))

(defun get-module-package (module-name &optional (repo-path *default-repository*))
  "Get registered package for a module"
  (let ((registry (load-module-registry repo-path)))
    (map:get registry module-name)))

(defun list-module-packages (&optional (repo-path *default-repository*))
  "List all registered module packages"
  (map:keys (load-module-registry repo-path)))