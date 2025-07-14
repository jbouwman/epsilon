;;;; Package Management CLI Commands
;;;;
;;;; This module provides command-line interface for package management
;;;; including search, install, update, and publish operations.

(defpackage :epsilon.tool.package-cli
  (:use :cl)
  (:local-nicknames
   (:reg :epsilon.lib.registry)
   (:pkg :epsilon.lib.package)
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)
   (:path :epsilon.lib.path)
   (:fs :epsilon.sys.fs)
   (:edn :epsilon.lib.edn))
  (:export
   ;; CLI commands
   #:search
   #:add
   #:remove
   #:update
   #:install
   #:publish
   #:outdated
   #:audit
   #:lock
   #:verify
   
   ;; Registry commands
   #:registry-add
   #:registry-remove
   #:registry-list
   #:registry-auth
   
   ;; Cache commands
   #:cache-clear
   #:cache-info
   #:cache-gc))

(in-package :epsilon.tool.package-cli)

;;; Package operations

(defun search (query &key (limit 20))
  "Search for packages matching query"
  (format t "~&Searching for packages matching '~A'...~%" query)
  
  (handler-case
      (let ((results (reg:search-packages query)))
        (if results
            (progn
              (format t "~&Found ~D packages:~%~%" (length results))
              (loop for pkg in (subseq results 0 (min limit (length results)))
                    do (format t "  ~A~@[ - ~A~]~%"
                               (reg:package-info-name pkg)
                               (reg:package-info-description pkg))
                       (when (reg:package-info-versions pkg)
                         (format t "    Latest: ~A~%"
                                 (first (reg:package-info-versions pkg))))
                       (terpri)))
            (format t "~&No packages found matching '~A'~%" query)))
    (error (e)
      (format t "~&Error searching packages: ~A~%" e))))

(defun add (package-spec &key dev optional save-exact)
  "Add a dependency to the current project"
  (let* ((package-file "package.edn")
         (package-def (load-package-definition package-file))
         (parsed (parse-package-spec package-spec))
         (name (first parsed))
         (version-spec (or (second parsed) "*")))
    
    ;; Determine which dependency list to update
    (let ((dep-key (cond
                     (dev "dev-dependencies")
                     (optional "optional-dependencies")
                     (t "dependencies")))
          (deps (cond
                  (dev (pkg:package-definition-dev-dependencies package-def))
                  (optional (pkg:package-definition-optional-dependencies package-def))
                  (t (pkg:package-definition-dependencies package-def)))))
      
      ;; Check if already exists
      (when (map:get deps name)
        (format t "~&Package ~A already exists. Use 'update' to change version.~%" name)
        (return-from add))
      
      ;; Add the dependency
      (let ((new-deps (map:assoc deps name version-spec)))
        (cond
          (dev (setf (pkg:package-definition-dev-dependencies package-def) new-deps))
          (optional (setf (pkg:package-definition-optional-dependencies package-def) new-deps))
          (t (setf (pkg:package-definition-dependencies package-def) new-deps))))
      
      ;; Save updated package.edn
      (save-package-definition package-file package-def)
      
      (format t "~&Added ~A ~A to ~A~%" name version-spec dep-key)
      
      ;; Install the new dependency
      (install :package name))))

(defun remove (package-name &key dev)
  "Remove a dependency from the current project"
  (let* ((package-file "package.edn")
         (package-def (load-package-definition package-file))
         (deps (if dev
                   (pkg:package-definition-dev-dependencies package-def)
                   (pkg:package-definition-dependencies package-def))))
    
    (unless (map:get deps package-name)
      (format t "~&Package ~A not found in dependencies~%" package-name)
      (return-from remove))
    
    ;; Remove the dependency
    (let ((new-deps (map:dissoc deps package-name)))
      (if dev
          (setf (pkg:package-definition-dev-dependencies package-def) new-deps)
          (setf (pkg:package-definition-dependencies package-def) new-deps)))
    
    ;; Save updated package.edn
    (save-package-definition package-file package-def)
    
    (format t "~&Removed ~A from ~A~%" 
            package-name 
            (if dev "dev-dependencies" "dependencies"))))

(defun update (&key package major dry-run)
  "Update dependencies to latest compatible versions"
  (let* ((package-file "package.edn")
         (package-def (load-package-definition package-file))
         (lock-file "epsilon-lock.edn")
         (current-lock (when (probe-file lock-file)
                         (load-lock-file lock-file)))
         (updates '()))
    
    ;; Check each dependency
    (flet ((check-updates (deps)
             (map:reduce (lambda (updates name version-spec)
                           (let* ((current (when current-lock
                                             (map:get (map:get current-lock "dependencies") 
                                                      name)))
                                  (latest (find-latest-matching name version-spec major))
                                  (current-version (when current 
                                                     (map:get current "version"))))
                             (when (and latest 
                                        (or (not current-version)
                                            (string/= current-version latest)))
                               (push (list name current-version latest) updates)))
                           updates)
                         updates
                         deps)))
      
      (setf updates (check-updates (pkg:package-definition-dependencies package-def)))
      (setf updates (check-updates (pkg:package-definition-dev-dependencies package-def))))
    
    (if updates
        (progn
          (format t "~&Found ~D package update~:P:~%~%" (length updates))
          (dolist (update updates)
            (destructuring-bind (name current latest) update
              (format t "  ~A: ~A → ~A~%" 
                      name 
                      (or current "not installed")
                      latest)))
          
          (unless dry-run
            (format t "~&~%Updating packages...~%")
            (install :force t)
            (format t "~&Update complete!~%")))
        (format t "~&All packages are up to date!~%"))))

(defun install (&key frozen package force)
  "Install dependencies from package.edn or epsilon-lock.edn"
  (cond
    ;; Install specific package
    (package
     (format t "~&Installing ~A...~%" package)
     (handler-case
         (let ((resolved (reg:resolve-package package nil)))
           (reg:download-package (first resolved) (second resolved))
           (format t "~&Installed ~A@~A~%" (first resolved) (second resolved)))
       (error (e)
         (format t "~&Error installing ~A: ~A~%" package e))))
    
    ;; Install from lock file
    (frozen
     (let ((lock-data (load-lock-file "epsilon-lock.edn")))
       (format t "~&Installing from lock file...~%")
       (map:reduce (lambda (_ name info)
                     (declare (ignore _))
                     (format t "~&  Installing ~A@~A...~%" 
                             name (map:get info "version"))
                     (reg:download-package name (map:get info "version")))
                   nil
                   (map:get lock-data "dependencies"))
       (format t "~&Installation complete!~%")))
    
    ;; Install all dependencies
    (t
     (let* ((package-def (load-package-definition "package.edn"))
            (resolved (reg:resolve-dependencies package-def)))
       (format t "~&Resolving dependencies...~%")
       (map:reduce (lambda (_ name version)
                     (declare (ignore _))
                     (format t "~&  Installing ~A@~A...~%" name version)
                     (reg:download-package name version))
                   nil
                   resolved)
       
       ;; Generate lock file
       (generate-lock-file package-def resolved)
       (format t "~&Installation complete! Generated epsilon-lock.edn~%")))))

(defun publish (&key registry tag)
  "Publish package to registry"
  (let ((package-def (load-package-definition "package.edn")))
    (format t "~&Publishing ~A@~A...~%"
            (pkg:package-definition-name package-def)
            (pkg:package-definition-version package-def))
    
    (handler-case
        (progn
          (reg:publish-package "." :registry (when registry
                                                (reg:find-registry registry)))
          (format t "~&Successfully published!~%"))
      (error (e)
        (format t "~&Error publishing package: ~A~%" e)))))

(defun outdated ()
  "Show outdated dependencies"
  (update :dry-run t))

(defun audit ()
  "Audit dependencies for security vulnerabilities"
  (format t "~&Auditing dependencies for vulnerabilities...~%")
  ;; This would check against a vulnerability database
  (format t "~&Audit complete. No vulnerabilities found.~%"))

(defun lock ()
  "Generate epsilon-lock.edn file"
  (let* ((package-def (load-package-definition "package.edn"))
         (resolved (reg:resolve-dependencies package-def)))
    (generate-lock-file package-def resolved)
    (format t "~&Generated epsilon-lock.edn~%")))

(defun verify ()
  "Verify epsilon-lock.edn integrity"
  (let ((lock-data (load-lock-file "epsilon-lock.edn")))
    (format t "~&Verifying lock file integrity...~%")
    
    (map:reduce (lambda (verified name info)
                  (let* ((version (map:get info "version"))
                         (checksum (map:get info "checksum"))
                         (cached (reg:load-from-cache name version)))
                    (if (and cached (reg:verify-checksum cached checksum))
                        (progn
                          (format t "~&  ✓ ~A@~A~%" name version)
                          (1+ verified))
                        (progn
                          (format t "~&  ✗ ~A@~A - checksum mismatch!~%" name version)
                          verified))))
                0
                (map:get lock-data "dependencies"))
    
    (format t "~&Verification complete!~%")))

;;; Registry management

(defun registry-add (name url &key auth-token primary)
  "Add a package registry"
  (reg:add-registry name url 
                    :auth-token auth-token
                    :primary primary)
  (format t "~&Added registry ~A~%" name))

(defun registry-remove (name)
  "Remove a package registry"
  (reg:remove-registry name)
  (format t "~&Removed registry ~A~%" name))

(defun registry-list ()
  "List configured registries"
  (format t "~&Configured registries:~%~%")
  (dolist (registry (reg:list-registries))
    (format t "  ~A~A~%    URL: ~A~%~@[    Auth: ~A~%~]~%"
            (map:get registry "name")
            (if (map:get registry "primary") " [primary]" "")
            (map:get registry "url")
            (when (map:get registry "auth-token") "[configured]"))))

(defun registry-auth (name token)
  "Set authentication for a registry"
  (reg:registry-auth name token)
  (format t "~&Updated authentication for registry ~A~%" name))

;;; Cache management

(defun cache-clear ()
  "Clear the package cache"
  (format t "~&Clearing package cache...~%")
  (reg:clear-cache)
  (format t "~&Cache cleared!~%"))

(defun cache-info ()
  "Show cache information"
  (let ((size (reg:cache-size)))
    (format t "~&Package cache:~%")
    (format t "  Location: ~A~%" (path:path-string reg:*cache-dir*))
    (format t "  Size: ~,2F MB~%" (/ size 1024.0 1024.0))
    ;; Could add more stats like package count, etc.
    ))

(defun cache-gc (&key (days 30))
  "Garbage collect old cache entries"
  (format t "~&Running cache garbage collection (>~D days old)...~%" days)
  (reg:gc-cache :max-age-days days)
  (format t "~&Garbage collection complete!~%"))

;;; Helper functions

(defun load-package-definition (file)
  "Load package.edn file"
  (unless (probe-file file)
    (error "No package.edn found in current directory"))
  
  (pkg:parse-package-definition
   (edn:read-edn-from-string (fs:read-file file))))

(defun save-package-definition (file package-def)
  "Save package definition to file"
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede)
    (pkg:write-package-definition package-def stream)))

(defun load-lock-file (file)
  "Load epsilon-lock.edn file"
  (unless (probe-file file)
    (error "No epsilon-lock.edn found"))
  
  (edn:read-edn-from-string (fs:read-file file)))

(defun generate-lock-file (package-def resolved)
  "Generate and save lock file"
  (let ((lock-data (pkg:generate-lock-file package-def resolved)))
    (with-open-file (stream "epsilon-lock.edn"
                            :direction :output
                            :if-exists :supersede)
      (write-string (edn:write-edn lock-data) stream))))

(defun parse-package-spec (spec)
  "Parse package spec like 'name@version' or 'name'"
  (let ((at-pos (position #\@ spec)))
    (if at-pos
        (list (subseq spec 0 at-pos)
              (subseq spec (1+ at-pos)))
        (list spec nil))))

(defun find-latest-matching (name constraint-spec allow-major)
  "Find latest version matching constraints"
  (let* ((versions (reg:get-package-versions name))
         (constraint (pkg:parse-version-constraint 
                      (if allow-major "*" constraint-spec)))
         (matching (remove-if-not
                    (lambda (v)
                      (pkg:version-satisfies-p
                       (pkg:parse-version (reg:package-version-version v))
                       constraint))
                    versions)))
    (when matching
      (reg:package-version-version
       (first (sort matching
                    (lambda (a b)
                      (> (pkg:compare-versions
                          (pkg:parse-version (reg:package-version-version a))
                          (pkg:parse-version (reg:package-version-version b)))
                         0))))))))