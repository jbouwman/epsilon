;;;; Package source protocol for epsilon build system
;;;;
;;;; This module defines the protocol for accessing packages from various
;;;; sources (filesystem, network, etc.) in a uniform way.

(defpackage :epsilon.build.protocol
  (:use :cl)
  (:shadow #:find-package)
  (:local-nicknames
   (:protocol :epsilon.protocol)
   (:map :epsilon.map)
   (:path :epsilon.path)
   (fs epsilon.sys.fs))
  (:export
   ;; Protocol name
   #:package-source-protocol
   
   ;; Generic functions
   #:find-package
   #:list-packages
   #:package-exists-p
   #:load-package-metadata
   #:resolve-package-location
   #:resolve-dependencies
   #:retrieve-package
   
   ;; Base class
   #:package-source
   
   ;; Concrete implementations
   #:filesystem-source
   #:directory-source
   #:composite-source
   
   ;; Utilities
   #:make-filesystem-source
   #:make-directory-source
   #:make-composite-source
   
   ;; Package descriptor structure
   #:package-descriptor
   #:make-package-descriptor
   #:name
   #:version
   #:location
   #:metadata
   #:source))

(in-package :epsilon.build.protocol)

;;; Protocol definition

(protocol:define-protocol package-source-protocol
  (:version "1.0")
  (:documentation "Protocol for accessing packages from various sources")
  
  (:method find-package (source package-name &key version)
    "Find a package by name and optional version.
     Returns a package descriptor or NIL if not found.")
  
  (:method list-packages (source &key filter)
    "List all packages available from this source.
     Filter can be a function taking a package name.")
  
  (:method package-exists-p (source package-name &key version)
    "Check if a package exists in this source.")
  
  (:method load-package-metadata (source package-name)
    "Load metadata for a package without loading code.
     Returns a property list with package information.")
  
  (:method resolve-package-location (source package-name)
    "Resolve the physical location of a package.
     Returns a URI or pathname.")
  
  (:method resolve-dependencies (source package-name &key recursive)
    "Resolve all dependencies for a package.
     Returns a list of package names.")
  
  (:method retrieve-package (source package-name target-location)
    "Retrieve a package to a local location.
     Returns the path where the package was retrieved."))

;;; Base class

(defclass package-source ()
  ((name :initarg :name
         :initform nil
         :accessor source-name
         :documentation "Optional name for this source"))
  (:documentation "Base class for all package sources"))

;;; Package descriptor structure

(defstruct (package-descriptor (:conc-name nil))
  "Information about a package"
  name
  version
  location
  metadata
  source)

;;; Default implementations

(defmethod package-exists-p ((source package-source) package-name &key version)
  "Default implementation using find-package"
  (not (null (find-package source package-name :version version))))

(defmethod resolve-dependencies ((source package-source) package-name &key recursive)
  "Default implementation using metadata"
  (let ((metadata (load-package-metadata source package-name)))
    (when metadata
      (let ((deps (getf metadata :dependencies)))
        (if recursive
            (let ((all-deps deps))
              (dolist (dep deps)
                (let ((sub-deps (resolve-dependencies source dep :recursive t)))
                  (setf all-deps (union all-deps sub-deps :test #'string=))))
              all-deps)
            deps)))))

;;; Filesystem source - single package in a directory

(defclass filesystem-source (package-source)
  ((path :initarg :path
         :accessor source-path
         :type path:path
         :documentation "Path to the package directory")
   (package-info :initform nil
                 :accessor source-package-info
                 :documentation "Cached package information"))
  (:documentation "A source representing a single package in a filesystem directory"))

(defun make-filesystem-source (path &key name)
  "Create a filesystem source for a directory containing a single package"
  (make-instance 'filesystem-source 
                 :path (path:ensure-path path)
                 :name name))

(defmethod find-package ((source filesystem-source) package-name &key version)
  (declare (ignore version)) ; Single package, no version selection
  (let ((info (or (source-package-info source)
                  (load-package-info source))))
    (when (and info (string-equal (getf info :name) package-name))
      (make-package-descriptor
       :name package-name
       :version (getf info :version)
       :location (source-path source)
       :metadata info
       :source source))))

(defmethod list-packages ((source filesystem-source) &key filter)
  (let ((info (or (source-package-info source)
                  (load-package-info source))))
    (when info
      (let ((name (getf info :name)))
        (if (or (null filter) (funcall filter name))
            (list name)
            nil)))))

(defmethod load-package-metadata ((source filesystem-source) package-name)
  (let ((info (or (source-package-info source)
                  (load-package-info source))))
    (when (and info (string-equal (getf info :name) package-name))
      info)))

(defmethod resolve-package-location ((source filesystem-source) package-name)
  (let ((info (or (source-package-info source)
                  (load-package-info source))))
    (when (and info (string-equal (getf info :name) package-name))
      (source-path source))))

(defmethod retrieve-package ((source filesystem-source) package-name target-location)
  ;; For filesystem source, we could copy or symlink
  ;; For now, just return the source location
  (resolve-package-location source package-name))

(defun load-package-info (source)
  "Load package.lisp and cache the information"
  (let* ((package-file (path:path-join (source-path source) "package.lisp"))
         (file-string (path:path-string package-file)))
    (when (probe-file file-string)
      (let ((info (with-open-file (stream file-string)
                    (read stream))))
        (setf (source-package-info source) info)
        info))))

;;; Directory source - multiple packages in subdirectories

(defclass directory-source (package-source)
  ((path :initarg :path
         :accessor source-path
         :type path:path
         :documentation "Path to directory containing package subdirectories")
   (cache :initform map:+empty+
          :accessor source-cache
          :documentation "Cache of discovered packages"))
  (:documentation "A source representing multiple packages in subdirectories"))

(defun make-directory-source (path &key name)
  "Create a directory source for a directory containing multiple packages"
  (make-instance 'directory-source 
                 :path (path:ensure-path path)
                 :name name))

(defmethod find-package ((source directory-source) package-name &key version)
  (declare (ignore version)) ; TODO: version support
  (ensure-packages-discovered source)
  (map:get (source-cache source) package-name))

(defmethod list-packages ((source directory-source) &key filter)
  (ensure-packages-discovered source)
  (let ((all-names (map:keys (source-cache source))))
    (if filter
        (remove-if-not filter all-names)
        all-names)))

(defmethod load-package-metadata ((source directory-source) package-name)
  (let ((descriptor (find-package source package-name)))
    (when descriptor
      (metadata descriptor))))

(defmethod resolve-package-location ((source directory-source) package-name)
  (let ((descriptor (find-package source package-name)))
    (when descriptor
      (location descriptor))))

(defmethod retrieve-package ((source directory-source) package-name target-location)
  ;; For directory source, return the subdirectory location
  (resolve-package-location source package-name))

(defun ensure-packages-discovered (source)
  "Discover packages if not already cached"
  (when (zerop (map:size (source-cache source)))
    (discover-packages-in-directory source)))

(defun discover-packages-in-directory (source)
  "Scan directory for package subdirectories"
  (let* ((base-path (source-path source)))
    ;; Use path operations to list subdirectories
    (dolist (entry-path (path:list-directory base-path :type :directories))
      (let* ((package-file (path:path-join entry-path "package.lisp")))
        (when (probe-file (path:path-string package-file))
          (let* ((info (with-open-file (stream (path:path-string package-file) 
                                               :if-does-not-exist nil)
                         (when stream (read stream))))
                 (package-name (getf info :name)))
            (when package-name
              (setf (source-cache source)
                    (map:assoc (source-cache source)
                               package-name
                               (make-package-descriptor
                                :name package-name
                                :version (getf info :version)
                                :location entry-path
                                :metadata info
                                :source source))))))))))

;;; Composite source - combines multiple sources

(defclass composite-source (package-source)
  ((sources :initarg :sources
            :accessor source-list
            :documentation "List of package sources")
   (strategy :initarg :strategy
             :initform :first-found
             :accessor source-strategy
             :documentation "Strategy for combining sources: :first-found or :merge"))
  (:documentation "A source that combines multiple package sources"))

(defun make-composite-source (sources &key name (strategy :first-found))
  "Create a composite source from a list of sources"
  (make-instance 'composite-source :sources sources :name name :strategy strategy))

(defmethod find-package ((source composite-source) package-name &key version)
  (ecase (source-strategy source)
    (:first-found
     ;; Return first matching package
     (dolist (sub-source (source-list source))
       (let ((pkg (find-package sub-source package-name :version version)))
         (when pkg (return pkg)))))
    (:merge
     ;; TODO: Implement merge strategy for multiple versions
     (find-package source package-name :version version))))

(defmethod list-packages ((source composite-source) &key filter)
  (let ((all-packages '()))
    (dolist (sub-source (source-list source))
      (setf all-packages 
            (union all-packages 
                   (list-packages sub-source :filter filter)
                   :test #'string=)))
    all-packages))

(defmethod load-package-metadata ((source composite-source) package-name)
  (dolist (sub-source (source-list source))
    (let ((metadata (load-package-metadata sub-source package-name)))
      (when metadata (return metadata)))))

(defmethod resolve-package-location ((source composite-source) package-name)
  (dolist (sub-source (source-list source))
    (let ((location (resolve-package-location sub-source package-name)))
      (when location (return location)))))

(defmethod retrieve-package ((source composite-source) package-name target-location)
  (dolist (sub-source (source-list source))
    (handler-case
        (let ((result (retrieve-package sub-source package-name target-location)))
          (when result (return result)))
      (error () nil))))

;;; Print methods for better debugging

(defmethod print-object ((source filesystem-source) stream)
  (print-unreadable-object (source stream :type t :identity nil)
    (format stream "~A" (path:path-string (source-path source)))))

(defmethod print-object ((source directory-source) stream)
  (print-unreadable-object (source stream :type t :identity nil)
    (format stream "~A" (path:path-string (source-path source)))))

(defmethod print-object ((source composite-source) stream)
  (print-unreadable-object (source stream :type t :identity nil)
    (format stream "~D sources: ~{~A~^, ~}"
            (length (source-list source))
            (mapcar (lambda (s) 
                      (typecase s
                        (filesystem-source (path:path-string (source-path s)))
                        (directory-source (path:path-string (source-path s)))
                        (t (format nil "~A" (type-of s)))))
                    (source-list source)))))
