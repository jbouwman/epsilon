;;;; epsilon.install.fetch - HTTP fetching and archive extraction
;;;;
;;;; Provides functionality to fetch modules from GitHub releases and URLs,
;;;; extract archives, and verify checksums.

(defpackage epsilon.install.fetch
  (:use cl)
  (:local-nicknames
   (http epsilon.http.simple)
   (json epsilon.json)
   (map epsilon.map)
   (str epsilon.string)
   (fs epsilon.sys.fs)
   (path epsilon.path)
   (digest epsilon.digest)
   (log epsilon.log)
   (process epsilon.process)
   (cache epsilon.install.cache)
   (manifest epsilon.install.manifest))
  (:export
   ;; Core fetching
   fetch-module
   fetch-from-github
   fetch-from-url

   ;; GitHub API
   resolve-github-release
   get-github-release-info
   get-github-latest-release
   find-archive-asset

   ;; Archive operations
   download-archive
   extract-archive
   detect-archive-format

   ;; Verification
   verify-checksum
   compute-file-checksum

   ;; Conditions
   fetch-error
   download-failed
   checksum-mismatch
   extraction-failed
   github-api-error))

(in-package epsilon.install.fetch)

;;; Conditions

(define-condition fetch-error (error)
  ((message :initarg :message :reader fetch-error-message))
  (:report (lambda (c s)
             (format s "Fetch error: ~A" (fetch-error-message c)))))

(define-condition download-failed (fetch-error)
  ((url :initarg :url :reader download-failed-url))
  (:report (lambda (c s)
             (format s "Download failed for ~A: ~A"
                     (download-failed-url c)
                     (fetch-error-message c)))))

(define-condition checksum-mismatch (fetch-error)
  ((expected :initarg :expected :reader expected-checksum)
   (actual :initarg :actual :reader actual-checksum))
  (:report (lambda (c s)
             (format s "Checksum mismatch: expected ~A but got ~A"
                     (expected-checksum c)
                     (actual-checksum c)))))

(define-condition extraction-failed (fetch-error)
  ((archive :initarg :archive :reader failed-archive))
  (:report (lambda (c s)
             (format s "Failed to extract ~A: ~A"
                     (failed-archive c)
                     (fetch-error-message c)))))

(define-condition github-api-error (fetch-error)
  ((status :initarg :status :reader github-api-error-status))
  (:report (lambda (c s)
             (format s "GitHub API error (status ~A): ~A"
                     (github-api-error-status c)
                     (fetch-error-message c)))))

;;; Configuration

(defparameter *github-api-url* "https://api.github.com"
  "Base URL for GitHub API")

(defparameter *download-timeout* 600
  "Timeout in seconds for downloads")

(defparameter *user-agent* "epsilon-installer/1.0"
  "User-Agent header for HTTP requests")

;;; GitHub API functions

(defun github-api-headers ()
  "Get headers for GitHub API requests."
  (map:make-map
   "Accept" "application/vnd.github.v3+json"
   "User-Agent" *user-agent*))

(defun get-github-release-info (owner repo &key version)
  "Get release information from GitHub.
   If VERSION is nil or 'latest', gets the latest release.
   Otherwise, gets the release for the specified tag."
  (let* ((endpoint (if (or (null version) (string= version "latest"))
                       (format nil "~A/repos/~A/~A/releases/latest"
                               *github-api-url* owner repo)
                       (let ((tag (if (char= (char version 0) #\v)
                                     version
                                     (format nil "v~A" version))))
                         (format nil "~A/repos/~A/~A/releases/tags/~A"
                                 *github-api-url* owner repo tag))))
         (response (http:http-get endpoint :headers (github-api-headers))))
    (unless (http:response-ok-p response)
      (error 'github-api-error
             :status (http:response-status response)
             :message (format nil "Failed to fetch release from ~A/~A" owner repo)))
    (http:response-json response)))

(defun get-github-latest-release (owner repo)
  "Get the latest release from a GitHub repository."
  (get-github-release-info owner repo :version "latest"))

(defun find-archive-asset (release-info &key prefer-format)
  "Find an archive asset from release info.
   Returns the asset map with 'browser_download_url', 'name', 'size'.
   PREFER-FORMAT can be :tar-gz or :zip."
  (let ((assets (map:get release-info "assets"))
        (tar-asset nil)
        (zip-asset nil))
    (dolist (asset assets)
      (let ((name (map:get asset "name")))
        (when name
          (cond
            ((str:ends-with-p ".tar.gz" name)
             (setf tar-asset asset))
            ((str:ends-with-p ".zip" name)
             (setf zip-asset asset))))))
    ;; Return preferred format or first available
    (case prefer-format
      (:tar-gz (or tar-asset zip-asset))
      (:zip (or zip-asset tar-asset))
      (t (or tar-asset zip-asset)))))

(defun resolve-github-release (source-spec &key version)
  "Resolve a source spec to a download URL.
   Returns (values download-url archive-format release-version)."
  (let* ((owner (manifest:source-owner source-spec))
         (repo (manifest:source-repo source-spec))
         (release-info (get-github-release-info owner repo :version version))
         (asset (find-archive-asset release-info))
         (release-version (str:replace-first (map:get release-info "tag_name") "v" "")))
    (unless asset
      (error 'fetch-error
             :message (format nil "No archive asset found in release ~A/~A"
                             owner repo)))
    (let* ((url (map:get asset "browser_download_url"))
           (name (map:get asset "name"))
           (format (detect-archive-format name)))
      (values url format release-version))))

;;; Archive operations

(defun detect-archive-format (filename)
  "Detect archive format from filename. Returns :tar-gz or :zip."
  (cond
    ((str:ends-with-p ".tar.gz" filename) :tar-gz)
    ((str:ends-with-p ".tgz" filename) :tar-gz)
    ((str:ends-with-p ".zip" filename) :zip)
    (t (error 'fetch-error
              :message (format nil "Unknown archive format: ~A" filename)))))

(defun download-archive (url target-path &key expected-checksum)
  "Download an archive from URL to target-path.
   Verifies checksum if expected-checksum is provided.
   Returns the path to the downloaded file."
  (log:info "Downloading ~A..." url)

  ;; Ensure target directory exists
  (fs:make-dirs (path:path-string (path:path-parent (path:make-path target-path))))

  (handler-case
      (progn
        ;; Use http:download-file for the download
        (let ((result (http:download-file url target-path :timeout *download-timeout*)))
          (unless result
            (error 'download-failed
                   :url url
                   :message "Download returned nil")))

        ;; Verify checksum if provided
        (when expected-checksum
          (verify-checksum target-path expected-checksum))

        (log:info "Downloaded to ~A" target-path)
        target-path)
    (error (e)
      (error 'download-failed
             :url url
             :message (format nil "~A" e)))))

(defun extract-archive (archive-path target-dir)
  "Extract archive to target directory.
   Supports tar.gz and zip formats.
   Returns the path to the extracted content (first directory found)."
  (let ((format (detect-archive-format archive-path))
        (target-str (path:path-string target-dir)))
    (log:info "Extracting ~A to ~A..." archive-path target-str)

    ;; Ensure target directory exists
    (fs:make-dirs target-str)

    (handler-case
        (case format
          (:tar-gz (extract-tar-gz archive-path target-str))
          (:zip (extract-zip archive-path target-str)))
      (error (e)
        (error 'extraction-failed
               :archive archive-path
               :message (format nil "~A" e))))

    ;; Find extracted directory
    (let ((entries (directory (merge-pathnames "*/" target-str))))
      (if entries
          (first entries)
          target-dir))))

(defun extract-tar-gz (archive-path target-dir)
  "Extract tar.gz archive using system tar command."
  (let ((result (process:run-sync "tar"
                                  :args (list "xzf" archive-path "-C" target-dir)
                                  :check-executable nil)))
    (unless (zerop (process:process-exit-code result))
      (error "tar extraction failed with exit code ~A" (process:process-exit-code result)))))

(defun extract-zip (archive-path target-dir)
  "Extract zip archive using system unzip command."
  (let ((result (process:run-sync "unzip"
                                  :args (list "-q" archive-path "-d" target-dir)
                                  :check-executable nil)))
    (unless (zerop (process:process-exit-code result))
      (error "unzip extraction failed with exit code ~A" (process:process-exit-code result)))))

;;; Checksum verification

(defun compute-file-checksum (filepath)
  "Compute SHA-256 checksum of a file.
   Returns checksum in format 'sha256:hexdigest'."
  (let ((content (fs:read-file-bytes filepath)))
    (let ((hash (digest:sha256 content)))
      (format nil "sha256:~A" (str:downcase (digest:bytes-to-hex hash))))))

(defun verify-checksum (filepath expected-checksum)
  "Verify file matches expected SHA-256 checksum.
   Raises checksum-mismatch condition on failure."
  (let ((actual (compute-file-checksum filepath)))
    (unless (string= actual expected-checksum)
      (error 'checksum-mismatch
             :expected expected-checksum
             :actual actual))
    (log:info "Checksum verified: ~A" (subseq expected-checksum 0 (min 20 (length expected-checksum))))
    t))

;;; High-level fetch functions

(defun fetch-from-github (owner repo &key version checksum name)
  "Fetch a module from a GitHub release.
   Returns the path to the extracted module directory.

   OWNER - GitHub owner/organization
   REPO - Repository name
   VERSION - Version to fetch (default: latest)
   CHECKSUM - Expected checksum (required)
   NAME - Module name for caching"
  (unless checksum
    (error 'fetch-error :message "Checksum is required for security"))

  (let ((module-name (or name repo)))
    ;; Check if already installed
    (when (cache:module-installed-p module-name)
      (log:info "Module ~A already installed" module-name)
      (return-from fetch-from-github (cache:installed-module-path module-name)))

    ;; Resolve release URL
    (multiple-value-bind (download-url archive-format release-version)
        (let ((source-spec (make-instance 'manifest:source-spec
                                          :type :github
                                          :owner owner
                                          :repo repo)))
          (resolve-github-release source-spec :version version))
      (declare (ignore release-version))

      ;; Check cache for archive
      (let ((cached-archive (cache:get-cached-archive module-name
                                                       (or version "latest")
                                                       :format archive-format)))
        (unless cached-archive
          ;; Download archive
          (let ((temp-path (path:path-string
                            (path:path-join (cache:archives-dir)
                                           (format nil "~A-~A~A"
                                                   module-name
                                                   (or version "latest")
                                                   (case archive-format
                                                     (:tar-gz ".tar.gz")
                                                     (:zip ".zip")))))))
            (download-archive download-url temp-path :expected-checksum checksum)
            (setf cached-archive (path:make-path temp-path))))

        ;; Extract archive
        (let* ((temp-extract-dir (path:path-string
                                   (path:path-join (cache:cache-root)
                                                   "tmp"
                                                   (format nil "extract-~A" (get-universal-time)))))
               (extracted-dir (extract-archive (path:path-string cached-archive) temp-extract-dir)))

          ;; Find module.lisp in extracted content
          (let ((module-dir (find-module-dir extracted-dir)))
            (unless module-dir
              (error 'fetch-error
                     :message "No module.lisp found in extracted archive"))

            ;; Install to modules directory
            (let ((install-path (cache:install-module-dir module-dir module-name)))
              ;; Cleanup temp extraction
              (handler-case
                  (fs:delete-directory temp-extract-dir)
                (error () nil))

              install-path)))))))

(defun fetch-from-url (url &key checksum name)
  "Fetch a module from a direct URL.
   Returns the path to the extracted module directory.

   URL - Direct URL to archive
   CHECKSUM - Expected checksum (required)
   NAME - Module name for caching"
  (unless checksum
    (error 'fetch-error :message "Checksum is required for security"))
  (unless name
    (error 'fetch-error :message "Module name is required for URL installs"))

  ;; Check if already installed
  (when (cache:module-installed-p name)
    (log:info "Module ~A already installed" name)
    (return-from fetch-from-url (cache:installed-module-path name)))

  (let ((archive-format (detect-archive-format url)))
    ;; Check cache
    (let ((cached-archive (cache:get-cached-archive name "url" :format archive-format)))
      (unless cached-archive
        ;; Download archive
        (let ((temp-path (path:path-string
                          (path:path-join (cache:archives-dir)
                                         (format nil "~A-url~A"
                                                 name
                                                 (case archive-format
                                                   (:tar-gz ".tar.gz")
                                                   (:zip ".zip")))))))
          (download-archive url temp-path :expected-checksum checksum)
          (setf cached-archive (path:make-path temp-path))))

      ;; Extract and install
      (let* ((temp-extract-dir (path:path-string
                                 (path:path-join (cache:cache-root)
                                                 "tmp"
                                                 (format nil "extract-~A" (get-universal-time)))))
             (extracted-dir (extract-archive (path:path-string cached-archive) temp-extract-dir)))

        (let ((module-dir (find-module-dir extracted-dir)))
          (unless module-dir
            (error 'fetch-error
                   :message "No module.lisp found in extracted archive"))

          (let ((install-path (cache:install-module-dir module-dir name)))
            (handler-case
                (fs:delete-directory temp-extract-dir)
              (error () nil))
            install-path))))))

(defun find-module-dir (base-dir)
  "Find the directory containing module.lisp within extracted content.
   Handles archives that may have a top-level directory."
  (let ((base-str (if (pathnamep base-dir)
                      (namestring base-dir)
                      base-dir)))
    ;; Check if module.lisp is directly in base-dir
    (when (probe-file (merge-pathnames "module.lisp" base-str))
      (return-from find-module-dir base-str))

    ;; Check subdirectories (common for GitHub releases)
    (dolist (entry (directory (merge-pathnames "*/" base-str)))
      (when (probe-file (merge-pathnames "module.lisp" entry))
        (return-from find-module-dir (namestring entry))))

    nil))

(defun fetch-module (dependency)
  "Fetch a module based on a manifest dependency specification.
   Returns the path to the installed module."
  (let* ((name (manifest:dependency-name dependency))
         (source (manifest:dependency-source dependency))
         (version (manifest:dependency-version dependency))
         (checksum (manifest:dependency-checksum dependency))
         (source-spec (manifest:parse-source-spec source)))

    (case (manifest:source-type source-spec)
      (:github
       (fetch-from-github (manifest:source-owner source-spec)
                          (manifest:source-repo source-spec)
                          :version version
                          :checksum checksum
                          :name name))
      (:url
       (fetch-from-url (manifest:source-url source-spec)
                       :checksum checksum
                       :name name))
      (t
       (error 'fetch-error
              :message (format nil "Unknown source type for ~A" name))))))
