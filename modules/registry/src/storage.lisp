;;;; epsilon.registry.storage - Package storage
;;;;
;;;; Provides storage operations for packages and metadata.

(in-package epsilon.registry)

;;; Cache directory management

(defun ensure-cache-directory ()
  "Ensure the cache directory exists."
  (let ((cache-dir (or *cache-directory*
                       (merge-pathnames ".epsilon/cache/"
                                        (user-homedir-pathname)))))
    (ensure-directories-exist cache-dir)
    cache-dir))

(defun package-cache-path (name version)
  "Get the cache path for a package version."
  (merge-pathnames (format nil "packages/~A/~A/" name version)
                   (ensure-cache-directory)))

(defun metadata-cache-path (name)
  "Get the cache path for package metadata."
  (merge-pathnames (format nil "metadata/~A.json" name)
                   (ensure-cache-directory)))

;;; Cache operations

(defun is-cached-p (name version)
  "Check if a package version is cached.
   Returns T if the package exists in cache."
  (probe-file (package-cache-path name version)))

(defun cache-size ()
  "Get total cache size in bytes."
  (let ((cache-dir (ensure-cache-directory)))
    (directory-size cache-dir)))

(defun clear-cache ()
  "Clear the entire package cache."
  (let ((cache-dir (ensure-cache-directory)))
    (delete-directory-recursive cache-dir)
    (ensure-directories-exist cache-dir)
    t))

(defun gc-cache (&key (max-age-days 30))
  "Garbage collect old cache entries.

   MAX-AGE-DAYS - Remove entries older than this many days"
  (let ((cache-dir (ensure-cache-directory))
        (cutoff-time (- (get-universal-time)
                        (* max-age-days 24 60 60)))
        (removed-count 0))
    (dolist (entry (directory (merge-pathnames "packages/*/" cache-dir)))
      (when (< (file-write-date entry) cutoff-time)
        (delete-directory-recursive entry)
        (incf removed-count)))
    removed-count))

;;; Directory utilities

(defun directory-size (path)
  "Calculate total size of all files in a directory."
  (let ((total 0))
    (dolist (file (directory (merge-pathnames "**/*.*" path)))
      (when (probe-file file)
        (incf total (file-size file))))
    total))

(defun file-size (path)
  "Get size of a file in bytes."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (file-length s)))

(defun delete-directory-recursive (path)
  "Recursively delete a directory and all its contents."
  (when (probe-file path)
    (dolist (file (directory (merge-pathnames "*.*" path)))
      (delete-file file))
    (dolist (subdir (directory (merge-pathnames "*/" path)))
      (delete-directory-recursive subdir))
    (delete-file path)))


(defun load-package-metadata (name)
  "Load cached metadata for a package."
  (let ((path (metadata-cache-path name)))
    (when (probe-file path)
      (json:decode (fs:read-file-string path)))))

(defun save-package-metadata (name metadata)
  "Save package metadata to cache."
  (let ((path (metadata-cache-path name)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede)
      (json:encode metadata s))))
