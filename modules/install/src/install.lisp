;;;; epsilon.install - Main install logic and CLI entry points
;;;;
;;;; Provides the primary interface for installing epsilon modules
;;;; from GitHub releases and other remote sources.

(defpackage epsilon.install
  (:use cl)
  (:local-nicknames
   (fetch epsilon.install.fetch)
   (manifest epsilon.install.manifest)
   (cache epsilon.install.cache)
   (map epsilon.map)
   (str epsilon.string)
   (fs epsilon.sys.fs)
   (path epsilon.path)
   (log epsilon.log))
  (:export
   ;; CLI entry points
   install
   install-manifest
   update
   list-installed
   clear-cache

   ;; Programmatic API
   install-from-url
   install-from-github
   install-dependency
   install-all-dependencies

   ;; Conditions
   install-error))

(in-package epsilon.install)

;;; Conditions

(define-condition install-error (error)
  ((message :initarg :message :reader install-error-message))
  (:report (lambda (c s)
             (format s "Install error: ~A" (install-error-message c)))))

;;; CLI Entry Points

(defun install (&key url github checksum name force)
  "Install a module from a URL or GitHub release.

   CLI entry point for: epsilon --install <spec>

   Options:
     URL      - Direct archive URL (requires CHECKSUM)
     GITHUB   - GitHub shorthand: owner/repo or owner/repo@version
     CHECKSUM - SHA-256 checksum (sha256:hexdigest)
     NAME     - Module name (required for URL installs)
     FORCE    - Re-install even if already present

   Examples:
     epsilon --exec epsilon.install:install -- --github someuser/json-rpc
     epsilon --exec epsilon.install:install -- --github someuser/json-rpc@v1.2.0 --checksum sha256:abc...
     epsilon --exec epsilon.install:install -- --url https://example.com/mod.tar.gz --name my-module --checksum sha256:abc..."

  (when force
    ;; Remove existing installation if force is set
    (let ((module-name (or name
                           (when github
                             (parse-github-repo-name github)))))
      (when (and module-name (cache:module-installed-p module-name))
        (log:info "Removing existing installation of ~A" module-name)
        (let ((install-path (cache:installed-module-path module-name)))
          (fs:delete-directory (path:path-string install-path))))))

  (cond
    ;; Install from direct URL
    (url
     (unless checksum
       (error 'install-error :message "--checksum is required for URL installs"))
     (unless name
       (error 'install-error :message "--name is required for URL installs"))
     (install-from-url url :checksum checksum :name name))

    ;; Install from GitHub
    (github
     (multiple-value-bind (owner repo version)
         (parse-github-spec github)
       (unless checksum
         (error 'install-error
                :message (format nil "--checksum is required for security. ~
                                     Get the checksum from the release page.")))
       (install-from-github owner repo :version version :checksum checksum :name name)))

    (t
     (error 'install-error
            :message "Must specify --url or --github"))))

(defun install-manifest (&key (path "epsilon.manifest") verify-only)
  "Install all dependencies from a manifest file.

   CLI entry point for: epsilon --install-manifest [path]

   Options:
     PATH        - Manifest file path (default: epsilon.manifest)
     VERIFY-ONLY - Only verify checksums, don't install

   Example:
     epsilon --exec epsilon.install:install-manifest
     epsilon --exec epsilon.install:install-manifest -- --path my-manifest.lisp"

  (let ((manifest-path (if (pathnamep path)
                           (namestring path)
                           path)))
    (unless (probe-file manifest-path)
      (error 'install-error
             :message (format nil "Manifest file not found: ~A" manifest-path)))

    (let ((manifest (manifest:read-manifest manifest-path)))
      ;; Validate manifest
      (manifest:validate-manifest manifest)

      (let ((deps (manifest:manifest-dependencies manifest))
            (installed 0)
            (skipped 0)
            (failed 0))

        (format t "~%Installing ~D dependencies from ~A~%~%" (length deps) manifest-path)

        (dolist (dep deps)
          (let ((name (manifest:dependency-name dep))
                (version (manifest:dependency-version dep)))
            (format t "  ~A (~A)... " name version)
            (force-output)

            (handler-case
                (if verify-only
                    (progn
                      (format t "verified~%")
                      (incf skipped))
                    (if (cache:module-installed-p name)
                        (progn
                          (format t "already installed~%")
                          (incf skipped))
                        (progn
                          (fetch:fetch-module dep)
                          (format t "installed~%")
                          (incf installed))))
              (error (e)
                (format t "FAILED: ~A~%" e)
                (incf failed)))))

        (format t "~%Summary: ~D installed, ~D skipped, ~D failed~%"
                installed skipped failed)

        (when (> failed 0)
          (error 'install-error
                 :message (format nil "~D dependencies failed to install" failed)))

        t))))

(defun update (&key (manifest-path "epsilon.manifest") check-only)
  "Check for and apply updates to installed modules.

   Options:
     MANIFEST-PATH - Manifest file path (default: epsilon.manifest)
     CHECK-ONLY    - Only check for updates, don't install

   Note: This checks if modules are installed, not if newer versions exist.
   For version updates, modify the manifest and re-run install-manifest."

  (unless (probe-file manifest-path)
    (error 'install-error
           :message (format nil "Manifest file not found: ~A" manifest-path)))

  (let ((manifest (manifest:read-manifest manifest-path)))
    (manifest:validate-manifest manifest)

    (let ((deps (manifest:manifest-dependencies manifest))
          (missing '()))

      (dolist (dep deps)
        (let ((name (manifest:dependency-name dep)))
          (unless (cache:module-installed-p name)
            (push dep missing))))

      (if (null missing)
          (format t "All dependencies are installed.~%")
          (progn
            (format t "~D dependencies need to be installed:~%" (length missing))
            (dolist (dep missing)
              (format t "  - ~A (~A)~%"
                      (manifest:dependency-name dep)
                      (manifest:dependency-version dep)))

            (unless check-only
              (format t "~%Installing missing dependencies...~%")
              (dolist (dep missing)
                (format t "Installing ~A... " (manifest:dependency-name dep))
                (force-output)
                (handler-case
                    (progn
                      (fetch:fetch-module dep)
                      (format t "done~%"))
                  (error (e)
                    (format t "FAILED: ~A~%" e))))))))))

(defun list-installed (&key verbose)
  "List installed modules.

   Options:
     VERBOSE - Show additional details"

  (let ((modules (cache:list-installed-modules)))
    (if (null modules)
        (format t "No modules installed in ~/.epsilon/modules/~%")
        (progn
          (format t "Installed modules (~D):~%" (length modules))
          (dolist (name modules)
            (if verbose
                (let* ((module-path (cache:installed-module-path name))
                       (module-file (path:path-join module-path "module.lisp")))
                  (format t "  ~A~%" name)
                  (format t "    Path: ~A~%" (path:path-string module-path))
                  (when (probe-file (path:path-string module-file))
                    (handler-case
                        (let ((plist (with-open-file (s (path:path-string module-file))
                                       (read s))))
                          (when (getf plist :version)
                            (format t "    Version: ~A~%" (getf plist :version)))
                          (when (getf plist :description)
                            (format t "    Description: ~A~%" (getf plist :description))))
                      (error () nil))))
                (format t "  ~A~%" name)))))))

(defun clear-cache (&key archives metadata all)
  "Clear the installation cache.

   Options:
     ARCHIVES - Clear downloaded archives
     METADATA - Clear cached metadata
     ALL      - Clear everything (default if no options given)"

  (cond
    (all
     (cache:clear-all-cache)
     (format t "Cleared all caches~%"))
    (archives
     (cache:clear-archive-cache)
     (format t "Cleared archive cache~%"))
    (metadata
     (cache:clear-metadata-cache)
     (format t "Cleared metadata cache~%"))
    (t
     ;; Default: clear all
     (cache:clear-all-cache)
     (format t "Cleared all caches~%"))))

;;; Programmatic API

(defun install-from-url (url &key checksum name)
  "Install a module from a direct URL.
   Returns the path to the installed module."
  (fetch:fetch-from-url url :checksum checksum :name name))

(defun install-from-github (owner repo &key version checksum name)
  "Install a module from a GitHub release.
   Returns the path to the installed module."
  (fetch:fetch-from-github owner repo
                           :version version
                           :checksum checksum
                           :name (or name repo)))

(defun install-dependency (dependency)
  "Install a single dependency specification.
   DEPENDENCY should be a manifest:dependency object."
  (fetch:fetch-module dependency))

(defun install-all-dependencies (manifest)
  "Install all dependencies from a manifest object."
  (dolist (dep (manifest:manifest-dependencies manifest))
    (fetch:fetch-module dep)))

;;; Utility functions

(defun parse-github-spec (spec)
  "Parse a GitHub spec string: owner/repo or owner/repo@version
   Returns (values owner repo version)"
  (let* ((at-pos (position #\@ spec))
         (path (if at-pos (subseq spec 0 at-pos) spec))
         (version (when at-pos (subseq spec (1+ at-pos))))
         (slash-pos (position #\/ path)))
    (unless slash-pos
      (error 'install-error
             :message (format nil "Invalid GitHub spec: ~A (expected owner/repo)" spec)))
    (values (subseq path 0 slash-pos)
            (subseq path (1+ slash-pos))
            version)))

(defun parse-github-repo-name (spec)
  "Extract just the repository name from a GitHub spec."
  (multiple-value-bind (owner repo version)
      (parse-github-spec spec)
    (declare (ignore owner version))
    repo))
