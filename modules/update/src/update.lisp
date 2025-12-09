;;;; Epsilon Self-Update Module
;;;;
;;;; Provides functionality to check for and apply updates to Epsilon.
;;;; Uses the GitHub releases API to check for new versions.

(defpackage epsilon.update
  (:use cl)
  (:local-nicknames
   (http epsilon.http.simple)
   (json epsilon.json)
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence)
   (env epsilon.sys.env)
   (fs epsilon.sys.fs)
   (path epsilon.path)
   (log epsilon.log)
   (process epsilon.process))
  (:export
   ;; CLI entry points
   update
   check

   ;; API functions
   check-for-updates
   get-latest-release
   get-installed-version
   compare-versions
   download-release
   install-release

   ;; Conditions
   update-error
   update-error-message
   no-update-available
   update-download-failed
   update-install-failed))

(in-package epsilon.update)

;;; Configuration

(defparameter *github-repo* "jbouwman/epsilon"
  "GitHub repository for epsilon releases")

(defparameter *github-api-url* "https://api.github.com/repos"
  "Base URL for GitHub API")

(defparameter *update-timeout* 30
  "Timeout in seconds for update operations")

;;; Conditions

(define-condition update-error (error)
  ((message :initarg :message :reader update-error-message))
  (:report (lambda (c s)
             (format s "Update error: ~A" (update-error-message c)))))

(define-condition no-update-available (update-error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "No update available. You are running the latest version."))))

(define-condition update-download-failed (update-error) ()
  (:report (lambda (c s)
             (format s "Failed to download update: ~A" (update-error-message c)))))

(define-condition update-install-failed (update-error) ()
  (:report (lambda (c s)
             (format s "Failed to install update: ~A" (update-error-message c)))))

;;; Version Utilities

(defun parse-version (version-string)
  "Parse a version string like '1.2.3' into a list of integers (1 2 3).
   Also handles versions with 'v' prefix and pre-release suffixes."
  (let* ((stripped (str:trim version-string))
         ;; Remove 'v' prefix if present
         (normalized (if (and (> (length stripped) 0)
                              (char= (char stripped 0) #\v))
                         (subseq stripped 1)
                         stripped))
         ;; Remove pre-release suffix (e.g., -alpha, -beta, -rc.1)
         (dash-pos (position #\- normalized)))
    (when dash-pos
      (setf normalized (subseq normalized 0 dash-pos)))
    (mapcar #'parse-integer
            (seq:realize (str:split #\. normalized)))))

(defun compare-versions (v1 v2)
  "Compare two version strings.
   Returns :greater if v1 > v2, :less if v1 < v2, :equal if same.
   Handles versions like '1.2.3', 'v1.2.3', '1.2.3-alpha'."
  (let ((parts1 (parse-version v1))
        (parts2 (parse-version v2)))
    ;; Pad shorter version with zeros
    (let ((max-len (max (length parts1) (length parts2))))
      (setf parts1 (append parts1 (make-list (- max-len (length parts1)) :initial-element 0)))
      (setf parts2 (append parts2 (make-list (- max-len (length parts2)) :initial-element 0))))
    ;; Compare component by component
    (loop for a in parts1
          for b in parts2
          do (cond
               ((> a b) (return-from compare-versions :greater))
               ((< a b) (return-from compare-versions :less))))
    :equal))

(defun version-newer-p (new-version current-version)
  "Check if new-version is newer than current-version."
  (eq :greater (compare-versions new-version current-version)))

;;; Platform Detection

(defun detect-platform ()
  "Detect the current platform and architecture for release matching."
  (let ((os (env:platform))
        (arch (machine-type)))
    (format nil "~(~A~)-~A"
            (case os
              (:darwin "macos")
              (t os))
            (cond
              ((search "X86-64" arch) "x86_64")
              ((search "ARM64" arch) "arm64")
              (t arch)))))

;;; Installation Directory Detection

(defun get-epsilon-home ()
  "Get the epsilon installation directory."
  (or (sb-ext:posix-getenv "EPSILON_HOME")
      (namestring (user-homedir-pathname))))

(defun get-installed-version ()
  "Get the currently installed epsilon version."
  (env:version))

;;; GitHub API Functions

(defun github-api-url (&rest parts)
  "Construct a GitHub API URL from parts."
  (format nil "~A/~A/~{~A~^/~}" *github-api-url* *github-repo* parts))

(defun fetch-github-api (endpoint)
  "Fetch data from GitHub API endpoint."
  (handler-case
      (let ((response (http:http-get endpoint
                                      :headers (map:make-map
                                               "Accept" "application/vnd.github.v3+json"
                                               "User-Agent" "epsilon-updater"))))
        (if (http:response-ok-p response)
            (http:response-json response)
            (error 'update-error
                   :message (format nil "GitHub API returned ~A"
                                   (http:response-status response)))))
    (error (e)
      (error 'update-error
             :message (format nil "Failed to contact GitHub: ~A" e)))))

(defun get-latest-release (&key include-prereleases)
  "Get information about the latest release from GitHub.
   Returns a map with :version, :tag, :url, :assets, :prerelease, :published-at."
  (let* ((endpoint (if include-prereleases
                       (github-api-url "releases")
                       (github-api-url "releases" "latest")))
         (data (fetch-github-api endpoint)))
    ;; If we got a list (prereleases), take the first one
    (let ((release (if (listp data) (first data) data)))
      (when release
        (map:make-map
         :version (str:replace-first (map:get release "tag_name") "v" "")
         :tag (map:get release "tag_name")
         :url (map:get release "html_url")
         :assets (map:get release "assets")
         :prerelease (map:get release "prerelease")
         :published-at (map:get release "published_at")
         :body (map:get release "body"))))))

(defun get-release-by-version (version)
  "Get a specific release by version number."
  (let* ((tag (if (char= (char version 0) #\v)
                  version
                  (format nil "v~A" version)))
         (endpoint (github-api-url "releases" "tags" tag)))
    (handler-case
        (let ((release (fetch-github-api endpoint)))
          (map:make-map
           :version (str:replace-first (map:get release "tag_name") "v" "")
           :tag (map:get release "tag_name")
           :url (map:get release "html_url")
           :assets (map:get release "assets")
           :prerelease (map:get release "prerelease")
           :published-at (map:get release "published_at")))
      (update-error ()
        (error 'update-error :message (format nil "Version ~A not found" version))))))

(defun find-platform-asset (release)
  "Find the download asset for the current platform in a release."
  (let* ((platform (detect-platform))
         (assets (map:get release :assets))
         (matching-asset nil))
    (when assets
      (dolist (asset assets)
        (let ((name (map:get asset "name")))
          (when (and name
                     (search platform name)
                     (str:ends-with-p ".tar.gz" name))
            (setf matching-asset asset)
            (return)))))
    (when matching-asset
      (map:make-map
       :name (map:get matching-asset "name")
       :url (map:get matching-asset "browser_download_url")
       :size (map:get matching-asset "size")))))

;;; Update Check Functions

(defun check-for-updates (&key include-prereleases)
  "Check if a newer version is available.
   Returns a map with :available, :current-version, :latest-version, :release-info
   or nil if no update is available."
  (let* ((current-version (get-installed-version))
         (release (get-latest-release :include-prereleases include-prereleases))
         (latest-version (map:get release :version)))
    (if (and latest-version (version-newer-p latest-version current-version))
        (map:make-map
         :available t
         :current-version current-version
         :latest-version latest-version
         :release-info release)
        (map:make-map
         :available nil
         :current-version current-version
         :latest-version latest-version))))

;;; Download Functions

(defun download-release (release &optional target-dir)
  "Download a release archive to target directory.
   Returns the path to the downloaded file."
  (let* ((asset (find-platform-asset release))
         (target-dir (or target-dir (format nil "~A/downloads" (get-epsilon-home))))
         (target-file nil))
    (unless asset
      (error 'update-download-failed
             :message (format nil "No release found for platform ~A" (detect-platform))))

    ;; Create target directory
    (fs:make-dirs target-dir)

    ;; Download file
    (let ((url (map:get asset :url))
          (filename (map:get asset :name)))
      (setf target-file (path:path-string (path:path-join target-dir filename)))
      (log:info "Downloading ~A..." filename)

      (handler-case
          (progn
            (http:download-file url target-file :timeout 600)
            (log:info "Downloaded to ~A" target-file)
            target-file)
        (error (e)
          (error 'update-download-failed
                 :message (format nil "Download failed: ~A" e)))))))

;;; Installation Functions

(defun install-release (archive-path &key backup)
  "Install a downloaded release archive.
   If backup is true, backs up the current installation first."
  (let* ((epsilon-home (get-epsilon-home))
         (backup-dir (when backup
                       (format nil "~A.backup.~A"
                               epsilon-home
                               (get-universal-time)))))
    ;; Create backup if requested
    (when backup
      (log:info "Creating backup at ~A" backup-dir)
      (fs:copy-directory epsilon-home backup-dir))

    ;; Extract archive
    (log:info "Extracting ~A..." archive-path)
    (let ((temp-dir (format nil "~A/update-temp-~A"
                            (directory-namestring archive-path)
                            (get-universal-time))))
      (fs:make-dirs temp-dir)

      (handler-case
          (progn
            ;; Extract archive
            (process:run-sync "tar"
                             :args (list "xzf" archive-path "-C" temp-dir)
                             :check-executable nil)

            ;; Find extracted directory
            (let ((extracted-dirs (directory (format nil "~A/*/" temp-dir))))
              (unless extracted-dirs
                (error 'update-install-failed
                       :message "No directory found in archive"))

              ;; Copy new files to installation
              (let ((source-dir (first extracted-dirs)))
                (log:info "Installing from ~A" source-dir)
                ;; Copy modules, bin, etc.
                (dolist (subdir '("modules" "bin" "scripts"))
                  (let ((src (path:path-string (path:path-join (namestring source-dir) subdir)))
                        (dst (path:path-string (path:path-join epsilon-home subdir))))
                    (when (probe-file src)
                      (when (probe-file dst)
                        (fs:delete-directory dst))
                      (fs:copy-directory src dst))))))

            ;; Cleanup
            (fs:delete-directory temp-dir)
            (log:info "Installation complete")
            t)

        (error (e)
          ;; Restore backup if installation failed
          (when (and backup (probe-file backup-dir))
            (log:warn "Installation failed, restoring backup...")
            (fs:delete-directory epsilon-home)
            (rename-file backup-dir epsilon-home))
          (error 'update-install-failed
                 :message (format nil "Installation failed: ~A" e)))))))

;;; CLI Entry Points

(defun check (&key nightly json)
  "Check for available updates.

   Options:
     :nightly - Include pre-release/nightly builds
     :json    - Output result as JSON

   Example: epsilon --exec epsilon.update:check"
  (handler-case
      (let ((result (check-for-updates :include-prereleases nightly)))
        (if json
            ;; JSON output
            (format t "~A~%" (json:encode result))
            ;; Human readable output
            (if (map:get result :available)
                (progn
                  (format t "~%Update available!~%")
                  (format t "  Current version: ~A~%" (map:get result :current-version))
                  (format t "  Latest version:  ~A~%" (map:get result :latest-version))
                  (format t "~%Run 'epsilon update' to install.~%"))
                (progn
                  (format t "~%You are running the latest version (~A)~%"
                          (map:get result :current-version))))))
    (update-error (e)
      (format *error-output* "Error checking for updates: ~A~%" e)
      (sb-ext:exit :code 1))))

(defun update (&key version nightly check-only dry-run force)
  "Update epsilon to the latest or specified version.

   Options:
     :version    - Install specific version (e.g., \"1.0.0\")
     :nightly    - Install latest nightly/pre-release build
     :check-only - Only check for updates, don't install
     :dry-run    - Show what would be done without making changes
     :force      - Skip confirmation prompts

   Examples:
     epsilon update                    ; Update to latest stable
     epsilon update --check-only       ; Check for updates
     epsilon update --version 1.0.0    ; Install specific version
     epsilon update --nightly          ; Install latest nightly"

  ;; Check-only mode
  (when check-only
    (return-from update (check :nightly nightly)))

  (handler-case
      (let* ((current-version (get-installed-version))
             (release (if version
                          (get-release-by-version version)
                          (get-latest-release :include-prereleases nightly)))
             (new-version (map:get release :version)))

        ;; Check if update is needed
        (unless (or force version)
          (unless (version-newer-p new-version current-version)
            (format t "~%You are already running the latest version (~A)~%" current-version)
            (return-from update nil)))

        ;; Show what will be done
        (format t "~%Epsilon Update~%")
        (format t "==============~%")
        (format t "  Current version: ~A~%" current-version)
        (format t "  Target version:  ~A~%" new-version)
        (format t "  Platform:        ~A~%" (detect-platform))

        (when (map:get release :prerelease)
          (format t "  Note: This is a pre-release version~%"))

        ;; Dry run mode
        (when dry-run
          (format t "~%[Dry run] Would download and install version ~A~%" new-version)
          (return-from update nil))

        ;; Confirm unless force mode
        (unless force
          (format t "~%Continue with update? (y/N) ")
          (force-output)
          (let ((response (read-line *standard-input* nil "")))
            (unless (member response '("y" "Y" "yes" "Yes" "YES") :test #'string=)
              (format t "Update cancelled.~%")
              (return-from update nil))))

        ;; Download
        (format t "~%")
        (let ((archive (download-release release)))
          ;; Install
          (install-release archive :backup t)

          ;; Cleanup downloaded archive
          (delete-file archive)

          (format t "~%Successfully updated to version ~A~%" new-version)
          (format t "~%Please restart epsilon to use the new version.~%")
          t))

    (update-error (e)
      (format *error-output* "~%Error: ~A~%" e)
      (sb-ext:exit :code 1))
    (error (e)
      (format *error-output* "~%Unexpected error: ~A~%" e)
      (sb-ext:exit :code 1))))
