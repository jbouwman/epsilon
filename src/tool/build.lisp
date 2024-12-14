(defpackage :epsilon.tool.build
  (:use :cl)
  (:local-nicknames
   (#:yaml #:epsilon.lib.yaml))
  (:export #:load-project
           #:compile-project
           #:package-project
           #:make-binary-distribution))

(in-package :epsilon.tool.build)

;; Project configuration structure
(defstruct project
  name
  version
  author
  dependencies
  sources          ; List of source files in compilation order
  hash-version)    ; Computed during compilation

;; YAML configuration parser
(defun parse-project-yaml (path)
  "Parse project YAML file into a project structure"
  (let ((yaml-content (yaml:parse-file path)))
    (make-project
     :name (get-value yaml-content "name")
     :version (get-yaml-value yaml-content "version")
     :author (get-yaml-value yaml-content "author")
     :dependencies (get-yaml-value yaml-content "dependencies")
     :sources (get-yaml-value yaml-content "sources"))))

;; Hash computation for versioning
(defun compute-source-hash (project)
  "Compute SHA-256 hash of all source files"
  (epsilon:compute-files-hash (project-sources project)))

;; Compilation management
(defun compile-source-file (source output-path)
  "Compile a single source file"
  (compile-file source :output-file output-path))

(defun compile-project (project &key (output-dir "build"))
  "Compile all project sources in specified order"
  (let* ((hash (compute-source-hash project))
         (build-dir (format nil "~A/~A" output-dir hash)))
    (ensure-directories-exist build-dir)
    (dolist (source (project-sources project))
      (let ((output-path (compute-output-path build-dir source)))
        (compile-source-file source output-path)))
    (setf (project-hash-version project) hash)
    build-dir))

;; Binary packaging
(defun create-fasl-bundle (project &key (output-dir "build"))
  "Create concatenated FASL bundle"
  (let ((output-path (format nil "~A/~A-bundle.fasl"
                            output-dir
                            (project-name project))))
    (with-open-file (out output-path
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
      (dolist (fasl (collect-project-fasls project output-dir))
        (write-fasl-to-stream fasl out)))
    output-path))

;; Distribution packaging
(defun package-project (project &key (type :source) (output-dir "build"))
  "Create project distribution package"
  (case type
    (:source (create-source-package project output-dir))
    (:binary (create-binary-package project output-dir))
    (:bundle (create-fasl-bundle project :output-dir output-dir))))

;; Utility functions adjusted to take output-dir
(defun create-source-package (project output-dir)
  "Create source distribution package"
  (let ((output-path (format nil "~A/~A-~A-src.tar.gz"
                            output-dir
                            (project-name project)
                            (project-version project))))
    (epsilon:create-tar-gz output-path
                          (project-sources project))
    output-path))

(defun create-binary-package (project output-dir)
  "Create binary distribution package for current architecture"
  (let ((output-path (format nil "~A/~A-~A-~A.tar.gz"
                            output-dir
                            (project-name project)
                            (project-version project)
                            (epsilon:system-architecture))))
    (epsilon:create-tar-gz output-path
                          (collect-project-fasls project output-dir))
    output-path))

(defun collect-project-fasls (project output-dir)
  "Collect all FASL files for the project"
  (let ((pattern (format nil "~A/~A/*.fasl"
                        output-dir
                        (project-hash-version project))))
    (epsilon:glob pattern)))
