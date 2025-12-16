;;;; epsilon.install.manifest - Manifest file parsing
;;;;
;;;; Provides parsing and validation for epsilon.manifest files that
;;;; declare remote module dependencies with checksums.
;;;;
;;;; Manifest format:
;;;; (:version "1.0.0"
;;;;  :dependencies
;;;;  ((:name "epsilon.json-rpc"
;;;;    :source "github:user/json-rpc"
;;;;    :version "1.2.0"
;;;;    :checksum "sha256:abc123...")))

(defpackage epsilon.install.manifest
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (fs epsilon.sys.fs)
   (path epsilon.path)
   (log epsilon.log))
  (:export
   ;; Manifest operations
   read-manifest
   write-manifest
   validate-manifest

   ;; Manifest accessors
   manifest-version
   manifest-dependencies

   ;; Dependency accessors
   dependency-name
   dependency-source
   dependency-version
   dependency-checksum

   ;; Source parsing
   source-spec
   parse-source-spec
   source-type
   source-owner
   source-repo
   source-url

   ;; Conditions
   manifest-error
   manifest-parse-error
   manifest-validation-error))

(in-package epsilon.install.manifest)

;;; Conditions

(define-condition manifest-error (error)
  ((message :initarg :message :reader manifest-error-message))
  (:report (lambda (c s)
             (format s "Manifest error: ~A" (manifest-error-message c)))))

(define-condition manifest-parse-error (manifest-error)
  ((file :initarg :file :reader manifest-parse-error-file))
  (:report (lambda (c s)
             (format s "Failed to parse manifest ~A: ~A"
                     (manifest-parse-error-file c)
                     (manifest-error-message c)))))

(define-condition manifest-validation-error (manifest-error)
  ((errors :initarg :errors :reader manifest-validation-errors))
  (:report (lambda (c s)
             (format s "Manifest validation failed:~%~{  - ~A~%~}"
                     (manifest-validation-errors c)))))

;;; Manifest structure

(defclass manifest ()
  ((version :initarg :version
            :reader manifest-version
            :type (or null string)
            :documentation "Manifest format version")
   (dependencies :initarg :dependencies
                 :reader manifest-dependencies
                 :initform nil
                 :documentation "List of dependency specifications"))
  (:documentation "Represents a parsed epsilon.manifest file"))

(defclass dependency ()
  ((name :initarg :name
         :reader dependency-name
         :type string
         :documentation "Module name (e.g., epsilon.json-rpc)")
   (source :initarg :source
           :reader dependency-source
           :type string
           :documentation "Source specification (github:user/repo or URL)")
   (version :initarg :version
            :reader dependency-version
            :type string
            :documentation "Version string or 'latest'")
   (checksum :initarg :checksum
             :reader dependency-checksum
             :type string
             :documentation "SHA-256 checksum (sha256:hexdigest)"))
  (:documentation "Represents a single dependency specification"))

;;; Source specification parsing

(defclass source-spec ()
  ((type :initarg :type
         :reader source-type
         :type keyword
         :documentation "Source type: :github or :url")
   (owner :initarg :owner
          :reader source-owner
          :initform nil
          :documentation "GitHub owner (for :github type)")
   (repo :initarg :repo
         :reader source-repo
         :initform nil
         :documentation "GitHub repository (for :github type)")
   (url :initarg :url
        :reader source-url
        :initform nil
        :documentation "Direct URL (for :url type)"))
  (:documentation "Parsed source specification"))

(defun parse-source-spec (source-string)
  "Parse a source specification string into a source-spec object.

   Supports:
   - github:owner/repo - GitHub repository
   - github:owner/repo@version - GitHub repository with version
   - https://... - Direct URL"
  (cond
    ;; GitHub shorthand: github:owner/repo[@version]
    ((str:starts-with-p "github:" source-string)
     (let* ((rest (subseq source-string 7))
            (at-pos (position #\@ rest))
            (path (if at-pos (subseq rest 0 at-pos) rest))
            (slash-pos (position #\/ path)))
       (unless slash-pos
         (error 'manifest-error
                :message (format nil "Invalid GitHub source: ~A (expected github:owner/repo)"
                                source-string)))
       (make-instance 'source-spec
                      :type :github
                      :owner (subseq path 0 slash-pos)
                      :repo (subseq path (1+ slash-pos)))))

    ;; Direct URL
    ((or (str:starts-with-p "https://" source-string)
         (str:starts-with-p "http://" source-string))
     (make-instance 'source-spec
                    :type :url
                    :url source-string))

    (t
     (error 'manifest-error
            :message (format nil "Unknown source format: ~A" source-string)))))

;;; Manifest parsing

(defun read-manifest (path)
  "Read and parse an epsilon.manifest file.
   Returns a manifest object."
  (let ((path-str (if (pathnamep path)
                      (namestring path)
                      path)))
    (unless (probe-file path-str)
      (error 'manifest-parse-error
             :file path-str
             :message "File not found"))

    (handler-case
        (let ((plist (with-open-file (stream path-str)
                       (read stream))))
          (parse-manifest-plist plist path-str))
      (end-of-file ()
        (error 'manifest-parse-error
               :file path-str
               :message "Empty or incomplete file"))
      (reader-error (e)
        (error 'manifest-parse-error
               :file path-str
               :message (format nil "Syntax error: ~A" e))))))

(defun parse-manifest-plist (plist path)
  "Parse a manifest property list into a manifest object."
  (unless (and (listp plist) (evenp (length plist)))
    (error 'manifest-parse-error
           :file path
           :message "Manifest must be a property list"))

  (let ((version (getf plist :version))
        (deps-plist (getf plist :dependencies)))
    (make-instance 'manifest
                   :version version
                   :dependencies (mapcar #'parse-dependency-plist deps-plist))))

(defun parse-dependency-plist (plist)
  "Parse a dependency property list into a dependency object."
  (let ((name (getf plist :name))
        (source (getf plist :source))
        (version (getf plist :version))
        (checksum (getf plist :checksum)))
    (make-instance 'dependency
                   :name name
                   :source source
                   :version version
                   :checksum checksum)))

;;; Manifest validation

(defun validate-manifest (manifest)
  "Validate a manifest object.
   Returns T if valid, signals manifest-validation-error otherwise."
  (let ((errors '()))
    ;; Check version
    (unless (manifest-version manifest)
      (push "Missing :version field" errors))

    ;; Check dependencies
    (let ((deps (manifest-dependencies manifest)))
      (when (null deps)
        (push "No dependencies specified" errors))

      (dolist (dep deps)
        (let ((dep-errors (validate-dependency dep)))
          (setf errors (append errors dep-errors)))))

    (when errors
      (error 'manifest-validation-error
             :errors (nreverse errors)))
    t))

(defun validate-dependency (dep)
  "Validate a single dependency. Returns list of error messages."
  (let ((errors '())
        (name (dependency-name dep)))

    ;; Required fields
    (unless (and name (stringp name) (> (length name) 0))
      (push "Dependency missing :name" errors))

    (unless (dependency-source dep)
      (push (format nil "Dependency ~A missing :source" (or name "?")) errors))

    (unless (dependency-version dep)
      (push (format nil "Dependency ~A missing :version" (or name "?")) errors))

    (unless (dependency-checksum dep)
      (push (format nil "Dependency ~A missing :checksum" (or name "?")) errors))

    ;; Validate checksum format
    (let ((checksum (dependency-checksum dep)))
      (when (and checksum (not (str:starts-with-p "sha256:" checksum)))
        (push (format nil "Dependency ~A has invalid checksum format (must start with sha256:)"
                     (or name "?")) errors)))

    ;; Validate source can be parsed
    (when (dependency-source dep)
      (handler-case
          (parse-source-spec (dependency-source dep))
        (manifest-error (e)
          (push (format nil "Dependency ~A: ~A" (or name "?") (manifest-error-message e)) errors))))

    errors))

;;; Manifest writing

(defun write-manifest (manifest path)
  "Write a manifest object to a file."
  (let ((path-str (if (pathnamep path)
                      (namestring path)
                      path)))
    (with-open-file (stream path-str
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream ";;; epsilon.manifest~%")
      (format stream ";;; Module dependencies with checksums~%~%")
      (let ((*print-case* :downcase)
            (*print-pretty* t)
            (*print-right-margin* 80))
        (prin1 (manifest-to-plist manifest) stream))
      (terpri stream))))

(defun manifest-to-plist (manifest)
  "Convert a manifest object to a property list."
  (list :version (manifest-version manifest)
        :dependencies (mapcar #'dependency-to-plist (manifest-dependencies manifest))))

(defun dependency-to-plist (dep)
  "Convert a dependency object to a property list."
  (list :name (dependency-name dep)
        :source (dependency-source dep)
        :version (dependency-version dep)
        :checksum (dependency-checksum dep)))

;;; Utility functions

(defun find-dependency (manifest name)
  "Find a dependency by name in a manifest."
  (find name (manifest-dependencies manifest)
        :key #'dependency-name
        :test #'string=))

(defun update-dependency-checksum (manifest name new-checksum)
  "Return a new manifest with updated checksum for the named dependency."
  (make-instance 'manifest
                 :version (manifest-version manifest)
                 :dependencies (mapcar (lambda (dep)
                                        (if (string= (dependency-name dep) name)
                                            (make-instance 'dependency
                                                           :name (dependency-name dep)
                                                           :source (dependency-source dep)
                                                           :version (dependency-version dep)
                                                           :checksum new-checksum)
                                            dep))
                                      (manifest-dependencies manifest))))
