;;;; Basic Package Definition Parser
;;;;
;;;; This module provides basic package.edn parsing functionality for the registry.
;;;; This is a simplified version focused on the core functionality needed
;;;; for the registry module.

(defpackage :epsilon.lib.package
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)
   (:path :epsilon.lib.path))
  (:export
   ;; Basic structures
   #:version
   #:make-version
   #:version-major
   #:version-minor
   #:version-patch
   #:version-prerelease
   #:version-build-metadata
   #:parse-version
   #:version-to-string
   #:compare-versions
   
   ;; Version constraints
   #:version-constraint
   #:make-version-constraint
   #:version-constraint-type
   #:version-constraint-version
   #:version-constraint-min-version
   #:version-constraint-max-version
   #:version-constraint-git-url
   #:version-constraint-git-ref
   #:parse-version-constraint
   #:version-satisfies-p
   
   ;; Dependencies
   #:dependency
   #:make-dependency
   #:dependency-name
   #:dependency-constraint
   #:dependency-source
   #:dependency-optional
   #:parse-dependency-spec
   
   ;; Package definitions
   #:package-definition
   #:make-package-definition
   #:package-definition-name
   #:package-definition-version
   #:package-definition-description
   #:package-definition-dependencies
   #:parse-package-definition
   
   ;; Utilities
   #:dependencies-to-edn))

(in-package :epsilon.lib.package)

;;; Version handling

(defstruct version
  "Semantic version representation"
  (major 0 :type integer)
  (minor 0 :type integer)
  (patch 0 :type integer)
  (prerelease nil :type (or null string))
  (build-metadata nil :type (or null string)))

(defun parse-version (version-string)
  "Parse a basic semantic version string like '1.2.3'"
  ;; Simplified version without regex - just basic parsing
  (let ((parts (str:split version-string #\.)))
    (when (>= (length parts) 3)
      (make-version
       :major (parse-integer (first parts) :junk-allowed t)
       :minor (parse-integer (second parts) :junk-allowed t)
       :patch (parse-integer (third parts) :junk-allowed t)))))

(defun version-to-string (version)
  "Convert version struct to string"
  (format nil "~D.~D.~D~@[-~A~]~@[+~A~]"
          (version-major version)
          (version-minor version)
          (version-patch version)
          (version-prerelease version)
          (version-build-metadata version)))

(defun compare-versions (v1 v2)
  "Compare two versions. Returns -1, 0, or 1"
  (cond
    ((< (version-major v1) (version-major v2)) -1)
    ((> (version-major v1) (version-major v2)) 1)
    ((< (version-minor v1) (version-minor v2)) -1)
    ((> (version-minor v1) (version-minor v2)) 1)
    ((< (version-patch v1) (version-patch v2)) -1)
    ((> (version-patch v1) (version-patch v2)) 1)
    (t 0)))

;;; Version constraints

(defstruct version-constraint
  "Version constraint specification"
  (type :exact :type keyword) ; :exact, :caret, :tilde, :range, :git, :any
  (version nil)
  (min-version nil)
  (max-version nil)
  (git-url nil)
  (git-ref nil))

(defun parse-version-constraint (constraint-string)
  "Parse basic version constraint"
  (cond
    ;; Git dependency
    ((str:starts-with-p constraint-string "git+")
     (parse-git-constraint constraint-string))
    
    ;; Latest/any version
    ((or (string= constraint-string "*")
         (string= constraint-string "latest"))
     (make-version-constraint :type :any))
    
    ;; Exact version
    (t
     (let ((version (parse-version constraint-string)))
       (if version
           (make-version-constraint
            :type :exact
            :version version)
           (make-version-constraint :type :any))))))

(defun parse-git-constraint (git-string)
  "Parse git dependency like 'git+https://github.com/user/repo.git@v1.2.3'"
  (let* ((without-prefix (subseq git-string 4)) ; Remove "git+"
         (at-pos (position #\@ without-prefix :from-end t)))
    (if at-pos
        (make-version-constraint
         :type :git
         :git-url (subseq without-prefix 0 at-pos)
         :git-ref (subseq without-prefix (1+ at-pos)))
        (make-version-constraint
         :type :git
         :git-url without-prefix
         :git-ref "main"))))

(defun version-satisfies-p (version constraint)
  "Check if a version satisfies a constraint"
  (ecase (version-constraint-type constraint)
    (:exact
     (= 0 (compare-versions version (version-constraint-version constraint))))
    (:any t)
    (:git nil)))

;;; Dependency specifications

(defstruct dependency
  "Basic dependency specification"
  (name nil :type string)
  (constraint nil :type version-constraint)
  (source :registry :type keyword)
  (optional nil :type boolean))

(defun parse-dependency-spec (name spec)
  "Parse a basic dependency specification"
  (cond
    ;; Simple string version
    ((stringp spec)
     (make-dependency
      :name name
      :constraint (parse-version-constraint spec)))
    
    ;; Map with specification
    ((map:map-p spec)
     (make-dependency
      :name name
      :constraint (parse-version-constraint (or (map:get spec "version") "*"))
      :source (cond
                ((map:get spec "git") :git)
                ((map:get spec "path") :local)
                (t :registry))
      :optional (map:get spec "optional")))
    
    (t (error "Invalid dependency specification: ~A" spec))))

;;; Package definition structure

(defstruct package-definition
  "Basic package definition"
  (name nil :type string)
  (version nil :type string)
  (description nil :type (or null string))
  (dependencies nil :type t))

(defun parse-package-definition (edn-data)
  "Parse a basic package.edn file"
  (let ((def (make-package-definition)))
    ;; Basic metadata
    (setf (package-definition-name def) (map:get edn-data "name"))
    (setf (package-definition-version def) (map:get edn-data "version"))
    (setf (package-definition-description def) (map:get edn-data "description"))
    
    ;; Parse dependencies
    (setf (package-definition-dependencies def)
          (parse-dependencies-map (map:get edn-data "dependencies" map:+empty+)))
    
    def))

(defun parse-dependencies-map (deps-map)
  "Parse a map of dependencies into dependency structures"
  (if (= (map:size deps-map) 0)
      map:+empty+
      (map:from-pairs
       (map:reduce (lambda (acc name spec)
                     (cons (cons name (parse-dependency-spec name spec)) acc))
                   '()
                   deps-map))))

;;; Utilities

(defun dependencies-to-edn (deps-map)
  "Convert dependencies map to basic EDN format"
  (map:map (lambda (dep)
             (if (eq (dependency-source dep) :registry)
                 (version-constraint-to-string (dependency-constraint dep))
                 (map:make-map
                  "version" (version-constraint-to-string (dependency-constraint dep))
                  "source" (string-downcase (symbol-name (dependency-source dep))))))
           deps-map))

(defun version-constraint-to-string (constraint)
  "Convert version constraint to string representation"
  (ecase (version-constraint-type constraint)
    (:exact (version-to-string (version-constraint-version constraint)))
    (:any "*")
    (:git (format nil "git+~A@~A" 
                  (version-constraint-git-url constraint)
                  (version-constraint-git-ref constraint)))))