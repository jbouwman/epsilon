;;;; epsilon.install.cache - Local cache management
;;;;
;;;; Manages the local cache for downloaded archives and installed modules.
;;;;
;;;; Cache structure:
;;;;   ~/.epsilon/
;;;;     cache/
;;;;       archives/           ; Downloaded archive files
;;;;         epsilon.json-rpc-1.2.0.tar.gz
;;;;       metadata/           ; Cached release metadata
;;;;         json-rpc.json
;;;;     modules/              ; Extracted and installed modules
;;;;       json-rpc/
;;;;         module.lisp
;;;;         src/

(defpackage epsilon.install.cache
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (fs epsilon.sys.fs)
   (path epsilon.path)
   (log epsilon.log)
   (json epsilon.json))
  (:export
   ;; Cache directories
   cache-root
   archives-dir
   metadata-dir
   modules-dir

   ;; Archive cache operations
   archive-cached-p
   archive-cache-path
   cache-archive
   get-cached-archive

   ;; Metadata cache operations
   metadata-cached-p
   cache-metadata
   get-cached-metadata

   ;; Module installation
   module-installed-p
   installed-module-path
   install-module-dir
   list-installed-modules

   ;; Cache maintenance
   clear-archive-cache
   clear-metadata-cache
   clear-all-cache
   cache-size))

(in-package epsilon.install.cache)

;;; Cache directory paths

(defparameter *cache-root* nil
  "Root directory for the epsilon cache. Defaults to ~/.epsilon/")

(defun cache-root ()
  "Get the cache root directory, creating it if necessary."
  (or *cache-root*
      (let ((root (path:path-join (namestring (user-homedir-pathname)) ".epsilon")))
        (fs:make-dirs (path:path-string root))
        (setf *cache-root* root))))

(defun archives-dir ()
  "Get the archives cache directory."
  (let ((dir (path:path-join (cache-root) "cache" "archives")))
    (fs:make-dirs (path:path-string dir))
    dir))

(defun metadata-dir ()
  "Get the metadata cache directory."
  (let ((dir (path:path-join (cache-root) "cache" "metadata")))
    (fs:make-dirs (path:path-string dir))
    dir))

(defun modules-dir ()
  "Get the installed modules directory."
  (let ((dir (path:path-join (cache-root) "modules")))
    (fs:make-dirs (path:path-string dir))
    dir))

;;; Archive cache operations

(defun archive-filename (name version format)
  "Generate a filename for a cached archive.
   FORMAT should be :tar-gz or :zip"
  (let ((ext (case format
               (:tar-gz ".tar.gz")
               (:zip ".zip")
               (t ".tar.gz"))))
    (format nil "~A-~A~A" name version ext)))

(defun archive-cache-path (name version &key (format :tar-gz))
  "Get the cache path for an archive."
  (path:path-join (archives-dir) (archive-filename name version format)))

(defun archive-cached-p (name version &key (format :tar-gz))
  "Check if an archive is already cached."
  (let ((cache-path (archive-cache-path name version :format format)))
    (probe-file (path:path-string cache-path))))

(defun cache-archive (source-path name version &key (format :tar-gz))
  "Copy an archive to the cache.
   Returns the cache path."
  (let* ((cache-path (archive-cache-path name version :format format))
         (cache-str (path:path-string cache-path))
         (source-str (if (pathnamep source-path)
                         (namestring source-path)
                         source-path)))
    (fs:copy-file source-str cache-str)
    cache-path))

(defun get-cached-archive (name version &key (format :tar-gz))
  "Get the path to a cached archive, or NIL if not cached."
  (let ((cache-path (archive-cache-path name version :format format)))
    (when (probe-file (path:path-string cache-path))
      cache-path)))

;;; Metadata cache operations

(defun metadata-filename (name)
  "Generate a filename for cached metadata."
  (format nil "~A.json" name))

(defun metadata-cache-path (name)
  "Get the cache path for module metadata."
  (path:path-join (metadata-dir) (metadata-filename name)))

(defun metadata-cached-p (name)
  "Check if metadata is cached for a module."
  (probe-file (path:path-string (metadata-cache-path name))))

(defun cache-metadata (name metadata)
  "Cache metadata for a module.
   METADATA should be a map or alist."
  (let ((cache-path (metadata-cache-path name)))
    (fs:write-file-string (path:path-string cache-path)
                          (json:encode metadata))
    cache-path))

(defun get-cached-metadata (name)
  "Get cached metadata for a module, or NIL if not cached."
  (let ((cache-path (metadata-cache-path name)))
    (when (probe-file (path:path-string cache-path))
      (json:decode (fs:read-file-string (path:path-string cache-path))))))

;;; Module installation

(defun normalize-module-name (name)
  "Normalize a module name for use as a directory name.
   Removes 'epsilon.' prefix if present."
  (if (str:starts-with-p "epsilon." name)
      (subseq name 8)
      name))

(defun installed-module-path (name)
  "Get the installation path for a module."
  (path:path-join (modules-dir) (normalize-module-name name)))

(defun module-installed-p (name)
  "Check if a module is installed."
  (let* ((module-dir (installed-module-path name))
         (module-file (path:path-join module-dir "module.lisp")))
    (probe-file (path:path-string module-file))))

(defun install-module-dir (source-dir name)
  "Install a module from an extracted source directory.
   Copies SOURCE-DIR contents to the appropriate modules directory.
   Returns the installation path."
  (let* ((target-dir (installed-module-path name))
         (target-str (path:path-string target-dir))
         (source-str (if (pathnamep source-dir)
                         (namestring source-dir)
                         source-dir)))
    ;; Remove existing installation if present
    (when (probe-file target-str)
      (fs:delete-directory target-str))
    ;; Copy new installation
    (fs:copy-directory source-str target-str)
    (log:info "Installed ~A to ~A" name target-str)
    target-dir))

(defun list-installed-modules ()
  "List all installed modules.
   Returns a list of module names."
  (let ((modules-path (modules-dir))
        (results '()))
    (dolist (entry (path:list-directory modules-path :type :directories))
      (let ((module-file (path:path-join entry "module.lisp")))
        (when (probe-file (path:path-string module-file))
          (push (path:path-name entry) results))))
    (nreverse results)))

;;; Cache maintenance

(defun clear-archive-cache ()
  "Clear the archive cache."
  (let ((dir (path:path-string (archives-dir))))
    (when (probe-file dir)
      (fs:delete-directory dir)
      (fs:make-dirs dir))
    (log:info "Cleared archive cache")))

(defun clear-metadata-cache ()
  "Clear the metadata cache."
  (let ((dir (path:path-string (metadata-dir))))
    (when (probe-file dir)
      (fs:delete-directory dir)
      (fs:make-dirs dir))
    (log:info "Cleared metadata cache")))

(defun clear-all-cache ()
  "Clear all caches (archives and metadata).
   Does NOT remove installed modules."
  (clear-archive-cache)
  (clear-metadata-cache))

(defun cache-size ()
  "Calculate the total size of the cache in bytes.
   Returns (values archive-size metadata-size total-size)."
  (let ((archive-size (directory-size (path:path-string (archives-dir))))
        (metadata-size (directory-size (path:path-string (metadata-dir)))))
    (values archive-size metadata-size (+ archive-size metadata-size))))

(defun directory-size (path)
  "Calculate the total size of files in a directory."
  (let ((total 0))
    (when (probe-file path)
      (dolist (file (directory (merge-pathnames "*.*" path)))
        (handler-case
            (incf total (file-length-safe file))
          (error () 0))))
    total))

(defun file-length-safe (path)
  "Get file length, returning 0 on error."
  (handler-case
      (with-open-file (s path :element-type '(unsigned-byte 8))
        (file-length s))
    (error () 0)))
