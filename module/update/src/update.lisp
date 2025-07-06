(defpackage epsilon.update
  (:use cl)
  (:local-nicknames
   (http epsilon.http.client)
   (json epsilon.lib.json)
   (str epsilon.lib.string)
   (uri epsilon.lib.uri)
   (sys epsilon.sys))
  (:export
   ;; Version checking
   check-for-updates
   get-current-version
   get-latest-version
   version-compare
   
   ;; Update operations
   update-epsilon
   download-update
   install-update
   
   ;; Backup/rollback
   backup-current-version
   rollback-to-previous
   
   ;; Configuration
   get-update-config
   set-update-config
   
   ;; Information
   version-info
   update-available-p))

(in-package epsilon.update)

;;; Version Information

(defvar *current-version* "0.1.0"
  "Current version of epsilon")

(defvar *github-repo* "jbouwman/epsilon"
  "GitHub repository for epsilon")

(defvar *github-api-base* "https://api.github.com"
  "Base URL for GitHub API")

(defvar *install-path* nil
  "Installation path for epsilon")

(defvar *backup-directory* nil
  "Directory for version backups")

;;; Version Comparison

(defun parse-version (version-string)
  "Parse semantic version string into components"
  (let ((parts (str:split version-string #\.)))
    (mapcar (lambda (part)
              (parse-integer part :junk-allowed t))
            parts)))

(defun version-compare (version1 version2)
  "Compare two version strings. Returns :newer, :older, or :equal"
  (let ((v1-parts (parse-version version1))
        (v2-parts (parse-version version2)))
    (loop for v1 in v1-parts
          for v2 in v2-parts
          do (cond
               ((> v1 v2) (return :newer))
               ((< v1 v2) (return :older)))
          finally (return :equal))))

(defun version-newer-p (version1 version2)
  "Check if version1 is newer than version2"
  (eq (version-compare version1 version2) :newer))

;;; GitHub API Integration

(defun get-github-releases-url ()
  "Get GitHub releases API URL"
  (format nil "~A/repos/~A/releases" *github-api-base* *github-repo*))

(defun get-latest-release-url ()
  "Get GitHub latest release API URL"
  (format nil "~A/repos/~A/releases/latest" *github-api-base* *github-repo*))

(defun fetch-latest-release ()
  "Fetch latest release information from GitHub API"
  (handler-case
      (multiple-value-bind (status headers body)
          (http:get (get-latest-release-url))
        (declare (ignore headers))
        (if (= status 200)
            (json:parse body)
            (error "Failed to fetch release info: HTTP ~A" status)))
    (error (e)
      (error "Failed to check for updates: ~A" e))))

(defun extract-version-from-release (release-data)
  "Extract version string from GitHub release data"
  (let ((tag-name (json:get release-data "tag_name")))
    (if (str:starts-with-p tag-name "v")
        (subseq tag-name 1)  ; Remove 'v' prefix
        tag-name)))

(defun get-latest-version ()
  "Get the latest version from GitHub releases"
  (let ((release (fetch-latest-release)))
    (extract-version-from-release release)))

(defun get-current-version ()
  "Get current version of epsilon"
  *current-version*)

;;; Update Checking

(defun check-for-updates ()
  "Check if updates are available"
  (let ((current (get-current-version))
        (latest (get-latest-version)))
    (values (version-newer-p latest current)
            current
            latest)))

(defun update-available-p ()
  "Check if an update is available"
  (multiple-value-bind (available current latest)
      (check-for-updates)
    (declare (ignore current latest))
    available))

;;; Installation Path Detection

(defun find-epsilon-executable ()
  "Find the epsilon executable path"
  (or *install-path*
      (let ((path (sys:which "epsilon")))
        (when path
          (setf *install-path* path)
          path))))

(defun get-install-directory ()
  "Get the directory where epsilon is installed"
  (let ((exe-path (find-epsilon-executable)))
    (when exe-path
      (pathname-directory (pathname exe-path)))))

(defun get-backup-directory ()
  "Get or create backup directory for epsilon versions"
  (or *backup-directory*
      (let* ((install-dir (get-install-directory))
             (backup-dir (when install-dir
                           (merge-pathnames ".epsilon-backups/" 
                                            (make-pathname :directory install-dir)))))
        (when backup-dir
          (ensure-directories-exist backup-dir)
          (setf *backup-directory* backup-dir)
          backup-dir))))

;;; Download and Installation

(defun get-download-url (release-data)
  "Get download URL for current platform from release data"
  (let ((assets (json:get release-data "assets")))
    (when assets
      ;; For now, assume we want the source tarball
      ;; TODO: Add platform-specific binary detection
      (json:get release-data "tarball_url"))))

(defun download-update (version)
  "Download update for specified version"
  (let ((release (fetch-latest-release)))
    (let ((download-url (get-download-url release)))
      (when download-url
        (let ((temp-file (format nil "/tmp/epsilon-~A.tar.gz" version)))
          (multiple-value-bind (status headers body)
              (http:get download-url)
            (declare (ignore headers))
            (if (= status 200)
                (progn
                  (with-open-file (stream temp-file 
                                          :direction :output 
                                          :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)
                    (write-sequence body stream))
                  temp-file)
                (error "Failed to download update: HTTP ~A" status))))))))

(defun backup-current-version ()
  "Backup current epsilon version"
  (let ((exe-path (find-epsilon-executable))
        (backup-dir (get-backup-directory)))
    (when (and exe-path backup-dir)
      (let ((backup-path (merge-pathnames 
                          (format nil "epsilon-~A" (get-current-version))
                          backup-dir)))
        (sys:copy-file exe-path backup-path)
        backup-path))))

(defun install-update (update-file)
  "Install update from downloaded file"
  (declare (ignore update-file))
  ;; TODO: Implement actual installation
  ;; This would involve extracting the tarball, building, and replacing the executable
  (error "Installation not yet implemented"))

;;; High-Level Update Operations

(defun update-epsilon (&key (backup t))
  "Update epsilon to latest version"
  (multiple-value-bind (available current latest)
      (check-for-updates)
    (if available
        (progn
          (format t "Updating epsilon from ~A to ~A~%" current latest)
          (when backup
            (format t "Backing up current version...~%")
            (backup-current-version))
          (format t "Downloading update...~%")
          (let ((update-file (download-update latest)))
            (format t "Installing update...~%")
            (install-update update-file)
            (format t "Update completed successfully~%")))
        (format t "Epsilon is already up to date (~A)~%" current))))

;;; Configuration

(defvar *update-config* (make-hash-table :test 'equal)
  "Configuration for update behavior")

(defun get-update-config (key &optional default)
  "Get update configuration value"
  (gethash key *update-config* default))

(defun set-update-config (key value)
  "Set update configuration value"
  (setf (gethash key *update-config*) value))

;;; Information Display

(defun version-info ()
  "Display version and installation information"
  (format t "Current version: ~A~%" (get-current-version))
  (format t "Installation path: ~A~%" (find-epsilon-executable))
  (format t "Backup directory: ~A~%" (get-backup-directory))
  (format t "Update channel: ~A~%" (get-update-config "channel" "stable"))
  (format t "Auto-check updates: ~A~%" (get-update-config "auto-check" "false"))
  (multiple-value-bind (available current latest)
      (check-for-updates)
    (if available
        (format t "Status: Update available (~A -> ~A)~%" current latest)
        (format t "Status: Up to date~%"))))

;;; Rollback Functionality

(defun rollback-to-previous ()
  "Rollback to previous version"
  (let ((backup-dir (get-backup-directory)))
    (when backup-dir
      ;; TODO: Implement rollback logic
      ;; This would involve finding the most recent backup and restoring it
      (error "Rollback not yet implemented"))))

;;; Initialization

(defun initialize-update-system ()
  "Initialize the update system"
  (find-epsilon-executable)
  (get-backup-directory)
  ;; Set default configuration
  (set-update-config "channel" "stable")
  (set-update-config "auto-check" "false")
  (set-update-config "backup" "true"))