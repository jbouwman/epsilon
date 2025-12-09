;;;; epsilon.registry.search - Package discovery
;;;;
;;;; Provides package search and discovery functionality.

(in-package epsilon.registry)

;;; Client-side search (against remote registries)

(defun search-packages (query &key registry limit)
  "Search for packages matching QUERY across configured registries.

   QUERY - Search string
   REGISTRY - Optional specific registry to search
   LIMIT - Maximum number of results (default 20)

   Returns a list of package info maps."
  (when *offline-mode*
    (return-from search-packages (search-cached-packages query)))
  (let* ((registries (if registry
                         (list (get-registry registry))
                         (list-registries)))
         (limit (or limit 20))
         (results nil))
    (dolist (reg registries)
      (when reg
        (let* ((path (format nil "/search?q=~A&limit=~A"
                             (http:url-encode query)
                             limit))
               (response (make-registry-request reg path)))
          (when (= (http:response-status response) 200)
            (let ((data (json:from-json (http:response-body response))))
              (dolist (pkg (map:get data "packages"))
                (push pkg results)))))))
    (nreverse results)))

(defun search-cached-packages (query)
  "Search packages in local cache only (offline mode)."
  (let ((cache-dir (ensure-cache-directory))
        (results nil))
    (dolist (metadata-file (directory (merge-pathnames "metadata/*.json" cache-dir)))
      (let ((metadata (with-open-file (s metadata-file)
                        (json:read-json s))))
        (when (package-matches-query-p metadata query)
          (push metadata results))))
    results))

(defun package-matches-query-p (metadata query)
  "Check if package metadata matches a search query."
  (let ((query-lower (string-downcase query)))
    (or (str:contains-p query-lower (string-downcase (or (map:get metadata "name") "")))
        (str:contains-p query-lower (string-downcase (or (map:get metadata "description") "")))
        (some (lambda (kw)
                (str:contains-p query-lower (string-downcase kw)))
              (map:get metadata "keywords")))))

;;; Package information retrieval

(defun get-package-info (name &key registry)
  "Get detailed information about a package.

   NAME - Package name
   REGISTRY - Optional specific registry to query

   Returns a package info map or NIL if not found."
  (when *offline-mode*
    (return-from get-package-info (load-package-metadata name)))
  (let* ((registries (if registry
                         (list (get-registry registry))
                         (list-registries))))
    (dolist (reg registries)
      (when reg
        (let* ((path (format nil "/packages/~A" (http:url-encode name)))
               (response (make-registry-request reg path)))
          (when (= (http:response-status response) 200)
            (let ((info (json:from-json (http:response-body response))))
              ;; Cache the metadata
              (save-package-metadata name info)
              (return-from get-package-info info))))))
    nil))

(defun get-package-versions (name &key registry)
  "Get list of available versions for a package.

   NAME - Package name
   REGISTRY - Optional specific registry to query

   Returns a list of version strings."
  (when *offline-mode*
    (return-from get-package-versions (get-cached-versions name)))
  (let* ((registries (if registry
                         (list (get-registry registry))
                         (list-registries))))
    (dolist (reg registries)
      (when reg
        (let* ((path (format nil "/packages/~A/versions" (http:url-encode name)))
               (response (make-registry-request reg path)))
          (when (= (http:response-status response) 200)
            (let ((data (json:from-json (http:response-body response))))
              (return-from get-package-versions
                (mapcar (lambda (v) (map:get v "version"))
                        (map:get data "versions"))))))))
    nil))

(defun get-cached-versions (name)
  "Get cached versions for a package."
  (let* ((cache-dir (ensure-cache-directory))
         (package-dir (merge-pathnames (format nil "packages/~A/" name) cache-dir)))
    (when (probe-file package-dir)
      (mapcar #'pathname-name
              (directory (merge-pathnames "*/" package-dir))))))

;;; Package download

(defun download-package (name version &key registry verify)
  "Download a package from the registry.

   NAME - Package name
   VERSION - Package version
   REGISTRY - Optional specific registry
   VERIFY - If true, verify checksum (default T)

   Returns the local path to the downloaded package."
  ;; Check cache first
  (when (is-cached-p name version)
    (return-from download-package (package-cache-path name version)))
  (when *offline-mode*
    (error "Package ~A@~A not cached and offline mode is enabled" name version))
  (let* ((registries (if registry
                         (list (get-registry registry))
                         (list-registries)))
         (verify (if (null verify) t verify)))
    (dolist (reg registries)
      (when reg
        (let* ((path (format nil "/packages/~A/~A/archive"
                             (http:url-encode name)
                             (http:url-encode version)))
               (response (make-registry-request reg path)))
          (when (= (http:response-status response) 200)
            (let ((content (http:response-body response))
                  (cache-path (package-cache-path name version)))
              ;; Verify checksum if requested
              (when verify
                (let ((expected (get-version-checksum name version)))
                  (when expected
                    (verify-checksum content expected))))
              ;; Save to cache
              (ensure-directories-exist cache-path)
              (let ((archive-path (merge-pathnames "archive.tar.gz" cache-path)))
                (with-open-file (s archive-path :direction :output
                                                :element-type '(unsigned-byte 8)
                                                :if-exists :supersede)
                  (write-sequence content s))
                (return-from download-package cache-path)))))))
    (error 'package-not-found :package name :version version)))

(defun get-version-checksum (name version)
  "Get the checksum for a specific package version."
  (let ((versions-data (get-package-versions-full name)))
    (when versions-data
      (dolist (v versions-data)
        (when (string= (map:get v "version") version)
          (return-from get-version-checksum (map:get v "checksum")))))))

(defun get-package-versions-full (name)
  "Get full version info (with checksums) for a package."
  (let ((reg (first (list-registries))))
    (when reg
      (let* ((path (format nil "/packages/~A/versions" (http:url-encode name)))
             (response (make-registry-request reg path)))
        (when (= (http:response-status response) 200)
          (map:get (json:from-json (http:response-body response)) "versions"))))))

(define-condition package-not-found (error)
  ((package :initarg :package :reader package-not-found-name)
   (version :initarg :version :reader package-not-found-version))
  (:report (lambda (condition stream)
             (format stream "Package not found: ~A@~A"
                     (package-not-found-name condition)
                     (package-not-found-version condition)))))

;;; Server-side search functions

(defun search-packages-internal (query &key limit)
  "Internal search function for server-side package search."
  (declare (ignore limit))
  ;; Implementation depends on storage backend
  ;; For now, return empty list
  (log:warn "search-packages-internal not fully implemented")
  nil)

(defun list-all-packages (&key limit)
  "List all packages in the registry."
  (declare (ignore limit))
  ;; Implementation depends on storage backend
  nil)

(defun get-package-info-internal (name)
  "Get package info from storage (server-side)."
  (let ((path (merge-pathnames (format nil "~A/metadata.json" name)
                               *storage-directory*)))
    (when (and *storage-directory* (probe-file path))
      (with-open-file (s path)
        (json:read-json s)))))

(defun get-package-versions-internal (name)
  "Get package versions from storage (server-side)."
  (when *storage-directory*
    (let ((package-dir (merge-pathnames (format nil "~A/" name) *storage-directory*)))
      (when (probe-file package-dir)
        (mapcar #'pathname-name
                (directory (merge-pathnames "*/" package-dir)))))))

(defun get-version-details-internal (name version)
  "Get version details from storage (server-side)."
  (let ((path (merge-pathnames (format nil "~A/~A/metadata.json" name version)
                               *storage-directory*)))
    (when (and *storage-directory* (probe-file path))
      (with-open-file (s path)
        (json:read-json s)))))
