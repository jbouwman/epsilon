;;;; Epsilon Package-Per-File Loader
;;;;
;;;; This module provides the loader infrastructure for Epsilon's package-per-file
;;;; system using .e files. Unlike traditional Lisp where packages are defined
;;;; explicitly, .e files derive their package name from their file path.
;;;;
;;;; Key features:
;;;; - File path determines package name (e.g., epsilon/http/client.e -> epsilon.http.client)
;;;; - Uses Epsilon's reader for .e files (#{}, [], #B"..." syntax)
;;;; - Clean package rewrite semantics on reload
;;;; - Validation of declared vs. path-derived package names
;;;;
;;;; This module is loaded by the bootstrapped core and used to compile
;;;; subsequent modules that use .e files.

(defpackage :epsilon.eloader
  (:use :cl)
  (:local-nicknames (:fs :epsilon.sys.fs)
                    (:str :epsilon.string)
                    (:path :epsilon.path)
                    (:reader :epsilon.reader))
  (:export
   ;; File discovery
   #:e-file-p
   #:list-e-files

   ;; Path/package utilities
   #:derive-package-from-path
   #:package-to-path
   #:validate-package-path

   ;; Reader integration
   #:read-e-file
   #:read-e-file-first-form

   ;; Package management
   #:redefine-package
   #:public-symbol-p
   #:collect-public-symbols
   #:auto-export-public-symbols
   #:sync-package-exports

   ;; Implicit package support
   #:extract-imports
   #:extract-exports
   #:has-package-form-p
   #:wrap-with-implicit-package

   ;; Compile and load API
   #:compile-e-file
   #:load-e-file
   #:find-package-form
   #:extract-package-name

   ;; Module integration
   #:e-module-p
   #:module-source-extension

   ;; Module-level data structures
   #:e-source-info
   #:make-e-source-info
   #:e-source-info-uri
   #:e-source-info-package-name
   #:e-source-info-declared-name
   #:e-source-info-src-root
   #:e-source-info-requires
   #:e-project
   #:make-e-project
   #:e-project-name
   #:e-project-path
   #:e-project-src-root
   #:e-project-sources
   #:e-project-tests
   #:e-project-requires
   #:e-project-metadata

   ;; Module discovery
   #:find-e-source-info
   #:load-e-project

   ;; Module building
   #:e-build-order
   #:fasl-path-for-e-source
   #:build-e-source
   #:build-e-module

   ;; Module loading
   #:load-e-module))

(in-package :epsilon.eloader)

;;; ---------------------------------------------------------------------------
;;; File Discovery
;;; ---------------------------------------------------------------------------

(defun e-file-p (path)
  "Return T if PATH is an Epsilon .e file."
  (let ((path-str (if (pathnamep path) (namestring path) path)))
    (str:ends-with-p path-str ".e")))

(defun list-e-files (directory)
  "List all .e files in DIRECTORY recursively.
   Returns sorted list of file paths."
  (fs:list-files directory ".e"))

;;; ---------------------------------------------------------------------------
;;; Path/Package Utilities
;;; ---------------------------------------------------------------------------

(defun normalize-path-separators (path)
  "Normalize path separators to forward slash."
  (substitute #\/ #\\ path))

(defun derive-package-from-path (filepath src-root)
  "Derive package name from .e file path relative to src-root.

   The package name is derived by:
   1. Computing the relative path from src-root to filepath
   2. Stripping the .e extension
   3. Converting path separators to dots

   Examples:
     (derive-package-from-path \"src/epsilon/http/client.e\" \"src/\")
     => \"epsilon.http.client\"

     (derive-package-from-path \"/proj/src/epsilon/http.e\" \"/proj/src/\")
     => \"epsilon.http\""
  (let* ((fp (normalize-path-separators (namestring filepath)))
         (sr (normalize-path-separators
              (if (str:ends-with-p src-root "/")
                  src-root
                  (concatenate 'string src-root "/"))))
         ;; Get relative path
         (rel (if (str:starts-with-p fp sr)
                  (subseq fp (length sr))
                  fp))
         ;; Strip .e extension
         (sans-ext (if (str:ends-with-p rel ".e")
                       (subseq rel 0 (- (length rel) 2))
                       rel)))
    ;; Convert path separators to dots
    (substitute #\. #\/ sans-ext)))

(defun package-to-path (package-name)
  "Convert package name to expected .e file path.

   Example:
     (package-to-path \"epsilon.http.client\")
     => \"epsilon/http/client.e\""
  (concatenate 'string
               (substitute #\/ #\. (string-downcase (string package-name)))
               ".e"))

(defun validate-package-path (declared-package filepath src-root)
  "Check if declared package matches path-derived package.

   Returns two values:
   1. T if they match, NIL otherwise
   2. The expected package name (derived from path)

   Example:
     (validate-package-path \"epsilon.http.client\"
                            \"src/epsilon/http/client.e\"
                            \"src/\")
     => T, \"epsilon.http.client\""
  (let ((expected (derive-package-from-path filepath src-root)))
    (values (string-equal (string declared-package) expected)
            expected)))

;;; ---------------------------------------------------------------------------
;;; Reader Integration
;;; ---------------------------------------------------------------------------

(defun read-e-file (filepath &key (setup-package t))
  "Read all forms from .e file using Epsilon reader.
   If SETUP-PACKAGE is T (default) and the first form is a package form with
   imports, sets up the package with local nicknames before reading the rest
   of the file. If SETUP-PACKAGE is NIL, only reads the package form without
   setting up nicknames (useful for dependency extraction).
   Returns list of forms."
  (with-open-file (stream filepath :direction :input)
    (let ((*readtable* reader:*epsilon-readtable*))
      ;; Read first form to check for package declaration
      (let ((first-form (read stream nil :eof)))
        (when (eq first-form :eof)
          (return-from read-e-file nil))
        ;; Check if it's a package form
        (if (and (consp first-form)
                 (member (car first-form) '(package epsilon.main:package)))
            (if setup-package
                ;; Extract imports and set up package before reading rest
                (let* ((pkg-name (extract-package-name first-form))
                       (imports (extract-package-form-imports first-form))
                       (shadows (extract-package-form-shadow first-form))
                       (uses (extract-package-form-use first-form))
                       ;; Only use imports for packages that exist
                       (valid-imports (remove-if-not
                                       (lambda (pair)
                                         (find-package (cdr pair)))
                                       imports))
                       ;; Only use packages that exist, always include :cl
                       (valid-uses (cons :cl (remove-if-not #'find-package uses))))
                  ;; Create/redefine the package with local nicknames
                  (redefine-package pkg-name
                                    :use valid-uses
                                    :local-nicknames valid-imports
                                    :shadow shadows)
                  ;; Read remaining forms with the new package active
                  (let ((*package* (find-package pkg-name)))
                    (cons first-form
                          (loop for form = (read stream nil :eof)
                                until (eq form :eof)
                                collect form))))
                ;; Don't set up package - just return first form for metadata extraction
                (list first-form))
            ;; Not a package form - read rest normally
            (cons first-form
                  (loop for form = (read stream nil :eof)
                        until (eq form :eof)
                        collect form)))))))

(defun read-e-file-first-form (filepath)
  "Read first form from .e file using Epsilon reader.
   Returns the first form, or NIL if file is empty."
  (with-open-file (stream filepath :direction :input)
    (let ((*readtable* reader:*epsilon-readtable*))
      (read stream nil nil))))

;;; ---------------------------------------------------------------------------
;;; Package Management
;;; ---------------------------------------------------------------------------

(defun redefine-package (name &key (use '(:cl)) export local-nicknames shadow)
  "Delete existing package and create fresh.

   This provides clean rewrite semantics: when a .e file is reloaded,
   the package is completely recreated rather than merged. This means:
   - Removed exports disappear
   - Renamed symbols are properly updated
   - No stale bindings remain

   Arguments:
   - NAME: Package name (string or symbol)
   - USE: List of packages to use (default: (:cl))
   - EXPORT: List of symbols to export
   - LOCAL-NICKNAMES: Alist of (nickname . package-name)
   - SHADOW: List of symbols to shadow"
  (let* ((pkg-name (string name))
         (old-pkg (find-package pkg-name)))
    ;; Clean up existing package if present
    (when old-pkg
      ;; First, unuse this package from all packages that use it
      (dolist (user (package-used-by-list old-pkg))
        (unuse-package old-pkg user))
      ;; Delete the old package
      (delete-package old-pkg))
    ;; Create fresh package
    (let ((pkg (make-package pkg-name :use use)))
      ;; Set up shadowed symbols (before local nicknames, so shadowing works correctly)
      (when shadow
        (cl:shadow (mapcar (lambda (sym) (string sym)) shadow) pkg))
      ;; Set up local nicknames
      (loop for (nickname . target) in local-nicknames
            do (sb-ext:add-package-local-nickname nickname target pkg))
      ;; Set up exports
      (when export
        (export (mapcar (lambda (sym)
                          (intern (string sym) pkg))
                        export)
                pkg))
      pkg)))

;;; ---------------------------------------------------------------------------
;;; Symbol Visibility
;;; ---------------------------------------------------------------------------

(defun public-symbol-p (symbol)
  "Return T if SYMBOL is public (not prefixed with % or -).

   Public symbols are those whose names do not start with:
   - % (internal/private convention)
   - - (private convention)

   Examples:
     (public-symbol-p 'foo)      => T
     (public-symbol-p '%internal) => NIL
     (public-symbol-p '-private) => NIL"
  (let ((name (symbol-name symbol)))
    (and (> (length name) 0)
         (not (char= (char name 0) #\%))
         (not (char= (char name 0) #\-)))))

(defun symbol-bound-p (symbol)
  "Return T if SYMBOL has any binding (function, macro, variable, class, etc.)."
  (or (fboundp symbol)
      (boundp symbol)
      (find-class symbol nil)
      (macro-function symbol)
      (special-operator-p symbol)))

(defun collect-public-symbols (package)
  "Collect all public, bound symbols from PACKAGE.

   Returns a list of symbols that:
   1. Are interned in PACKAGE (not inherited)
   2. Have a public name (not prefixed with % or -)
   3. Are bound (have a function, variable, class, or macro binding)"
  (let ((pkg (find-package package))
        (symbols nil))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)  ; Interned here, not inherited
                   (public-symbol-p sym)
                   (symbol-bound-p sym))
          (push sym symbols))))
    symbols))

(defun auto-export-public-symbols (package)
  "Export all public, bound symbols from PACKAGE.

   This implements the convention that symbols not prefixed with % or -
   are automatically exported when they have bindings."
  (let ((pkg (find-package package)))
    (when pkg
      (let ((public-symbols (collect-public-symbols pkg)))
        (when public-symbols
          (export public-symbols pkg))
        public-symbols))))

(defun sync-package-exports (package)
  "Synchronize package exports with currently bound public symbols.

   This:
   1. Unexports symbols that are no longer bound
   2. Exports new public symbols that are now bound

   Use this after reloading a .e file to ensure exports match reality."
  (let ((pkg (find-package package)))
    (when pkg
      ;; First, collect what should be exported
      (let ((should-export (collect-public-symbols pkg))
            (currently-exported nil))
        ;; Collect currently exported symbols
        (do-external-symbols (sym pkg)
          (push sym currently-exported))
        ;; Unexport symbols that shouldn't be exported anymore
        (dolist (sym currently-exported)
          (unless (member sym should-export)
            (unexport sym pkg)))
        ;; Export symbols that should be exported
        (dolist (sym should-export)
          (unless (member sym currently-exported)
            (export sym pkg)))
        should-export))))

;;; ---------------------------------------------------------------------------
;;; Form Analysis
;;; ---------------------------------------------------------------------------

(defun find-package-form (forms)
  "Find package/defpackage form in FORMS.
   Returns the first package definition form found, or NIL."
  (find-if (lambda (form)
             (and (consp form)
                  (member (car form) '(package defpackage cl:defpackage
                                       epsilon.main:package))))
           forms))

(defun extract-package-name (pkg-form)
  "Extract package name from a package definition form.
   Works with (defpackage name ...) and (package name ...) forms."
  (when (and (consp pkg-form) (cdr pkg-form))
    (string (cadr pkg-form))))

(defun extract-package-form-imports (pkg-form)
  "Extract import specs from a (package name (import ...)) form.
   Returns an alist of (nickname . package-name) suitable for local-nicknames."
  (let ((imports nil))
    (dolist (clause (cddr pkg-form))
      (when (and (consp clause)
                 (symbolp (car clause))
                 (string-equal "IMPORT" (symbol-name (car clause))))
        (dolist (spec (cdr clause))
          (when (consp spec)
            (let ((pkg (first spec))
                  (nick (second spec)))
              (when nick
                (push (cons nick pkg) imports)))))))
    (nreverse imports)))

(defun extract-package-form-shadow (pkg-form)
  "Extract shadow specs from a (package name (shadow ...)) form.
   Returns a list of symbols to shadow."
  (let ((shadows nil))
    (dolist (clause (cddr pkg-form))
      (when (and (consp clause)
                 (symbolp (car clause))
                 (string-equal "SHADOW" (symbol-name (car clause))))
        (dolist (sym (cdr clause))
          (push sym shadows))))
    (nreverse shadows)))

(defun extract-package-form-use (pkg-form)
  "Extract use specs from a (package name (use ...)) form.
   Returns a list of package names to use."
  (let ((uses nil))
    (dolist (clause (cddr pkg-form))
      (when (and (consp clause)
                 (symbolp (car clause))
                 (string-equal "USE" (symbol-name (car clause))))
        (dolist (pkg (cdr clause))
          (push (if (stringp pkg) pkg (string pkg)) uses))))
    (nreverse uses)))

(defun has-package-form-p (forms)
  "Return T if FORMS contains a package definition."
  (not (null (find-package-form forms))))

;;; ---------------------------------------------------------------------------
;;; Implicit Package Support
;;; ---------------------------------------------------------------------------

(defun extract-imports (forms)
  "Extract (import ...) forms from FORMS.

   Import forms have the syntax:
     (import (package-name nickname) ...)

   Returns an alist of (nickname . package-name) suitable for local-nicknames."
  (let ((imports nil))
    (dolist (form forms)
      (when (and (consp form) (eq (car form) 'import))
        (dolist (spec (cdr form))
          (when (and (consp spec) (= (length spec) 2))
            (let ((pkg (first spec))
                  (nick (second spec)))
              (push (cons nick pkg) imports))))))
    (nreverse imports)))

(defun extract-exports (forms)
  "Extract (export ...) forms from FORMS.

   Export forms have the syntax:
     (export symbol1 symbol2 ...)

   Returns a list of symbol names to export."
  (let ((exports nil))
    (dolist (form forms)
      (when (and (consp form) (eq (car form) 'export))
        (dolist (sym (cdr form))
          (push sym exports))))
    (nreverse exports)))

(defun remove-import-export-forms (forms)
  "Remove top-level (import ...) and (export ...) forms from FORMS."
  (remove-if (lambda (form)
               (and (consp form)
                    (member (car form) '(import export))))
             forms))

(defun wrap-with-implicit-package (forms package-name &key use)
  "Wrap FORMS with an implicit package declaration.

   This transforms:
     (import (foo f) (bar b))
     (export baz quux)
     (defun baz () ...)

   Into:
     (defpackage package-name
       (:use :cl)
       (:local-nicknames (:f :foo) (:b :bar))
       (:export #:baz #:quux))
     (in-package package-name)
     (defun baz () ...)

   Note: Explicit exports are respected, but auto-export of public symbols
   happens after loading via sync-package-exports."
  (let* ((imports (extract-imports forms))
         (exports (extract-exports forms))
         (body-forms (remove-import-export-forms forms))
         (use-list (or use '(:cl)))
         (pkg-form `(defpackage ,(intern (string-upcase package-name) :keyword)
                      (:use ,@use-list)
                      ,@(when imports
                          `((:local-nicknames
                             ,@(mapcar (lambda (pair)
                                         (list (intern (string (car pair)) :keyword)
                                               (intern (string (cdr pair)) :keyword)))
                                       imports))))
                      ,@(when exports
                          `((:export ,@(mapcar (lambda (s)
                                                 (intern (string s) :keyword))
                                               exports)))))))
    (list* pkg-form
           `(in-package ,(intern (string-upcase package-name) :keyword))
           body-forms)))

;;; ---------------------------------------------------------------------------
;;; Compile and Load API
;;; ---------------------------------------------------------------------------

(defun compile-e-file (filepath src-root &key (warnings t) (verbose nil))
  "Compile a single .e file with package-per-file semantics.

   Process:
   1. Derive expected package from file path
   2. Read forms with epsilon reader
   3. Validate package declaration matches path (warn if not)
   4. Compile with epsilon readtable active

   Arguments:
   - FILEPATH: Path to the .e file
   - SRC-ROOT: Root directory for deriving package names
   - WARNINGS: If T, warn about package/path mismatches (default T)
   - VERBOSE: If T, print compilation progress (default NIL)

   Returns: Result of COMPILE-FILE"
  (let* ((expected-pkg (derive-package-from-path filepath src-root))
         (forms (read-e-file filepath))
         (pkg-form (find-package-form forms)))
    ;; Validate package matches path
    (when (and pkg-form warnings)
      (let ((declared (extract-package-name pkg-form)))
        (unless (string-equal declared expected-pkg)
          (warn "Package ~A declared in ~A but path suggests ~A"
                declared filepath expected-pkg))))
    (when verbose
      (format t "~&; Compiling ~A (package: ~A)~%" filepath expected-pkg))
    ;; Compile with epsilon readtable
    (let ((*readtable* reader:*epsilon-readtable*))
      (compile-file filepath :verbose verbose))))

(defun load-e-file (filepath src-root &key (redefine t) (verbose nil) (auto-export t))
  "Load a .e file with package-per-file semantics.

   Process:
   1. Derive package name from file path
   2. Optionally redefine package for clean slate
   3. Load file with epsilon reader
   4. Auto-export public symbols (not prefixed with % or -)
   5. Sync exports (remove stale, add new)

   Arguments:
   - FILEPATH: Path to the .e file
   - SRC-ROOT: Root directory for deriving package names
   - REDEFINE: If T, completely rewrite the package before loading (default T)
   - VERBOSE: If T, print loading progress (default NIL)
   - AUTO-EXPORT: If T, auto-export public symbols after load (default T)

   Returns: Result of LOAD"
  (let ((expected-pkg (derive-package-from-path filepath src-root)))
    (when verbose
      (format t "~&; Loading ~A (package: ~A)~%" filepath expected-pkg))
    ;; Optionally redefine package for clean slate
    (when redefine
      (redefine-package expected-pkg))
    ;; Load with epsilon readtable
    (let ((result (let ((*readtable* reader:*epsilon-readtable*))
                    (load filepath :verbose verbose))))
      ;; After loading, sync exports based on what's actually bound
      (when auto-export
        (sync-package-exports expected-pkg))
      result)))

;;; ---------------------------------------------------------------------------
;;; Module Integration
;;; ---------------------------------------------------------------------------

(defun e-module-p (module-spec)
  "Return T if MODULE-SPEC indicates an Epsilon (.e) module.

   Checks for :source-type :e in the module specification."
  (eq (getf module-spec :source-type) :e))

(defun module-source-extension (module-spec)
  "Return the source file extension for MODULE-SPEC.

   Returns \".e\" for :source-type :e, otherwise \".lisp\"."
  (if (e-module-p module-spec)
      ".e"
      ".lisp"))

;;; ---------------------------------------------------------------------------
;;; Module-Level Data Structures
;;; ---------------------------------------------------------------------------

(defstruct e-source-info
  "Metadata for a single .e source file.
   Package name is derived from file path. Dependencies are extracted
   from (import ...) forms."
  uri           ; Full path to .e file
  package-name  ; Derived from path (e.g., \"epsilon.http.client\")
  declared-name ; Actual declared name from (package ...) form
  src-root      ; Root directory for derivation
  requires)     ; List of required package names

(defstruct e-project
  "Module/project representation for .e modules.
   Parallels loader.lisp's project class but simplified for .e files."
  name          ; Module name from module.lisp
  path          ; Module directory
  src-root      ; src/ subdirectory
  sources       ; List of e-source-info
  tests         ; List of e-source-info for tests
  requires      ; Dependencies from module.lisp
  metadata)     ; Full module.lisp plist

;;; ---------------------------------------------------------------------------
;;; Module Discovery
;;; ---------------------------------------------------------------------------

(defun extract-e-file-info (filepath)
  "Extract package info from an .e file.
   Returns (values requires declared-name) where:
   - requires: list of required package names from imports
   - declared-name: the declared package name from (package ...) form
   Uses setup-package nil to avoid needing packages to exist yet."
  (let ((forms (read-e-file filepath :setup-package nil))
        (requires nil)
        (declared-name nil))
    ;; Check top-level import forms
    (let ((imports (extract-imports forms)))
      (dolist (pair imports)
        (push (string (cdr pair)) requires)))  ; cdr is package-name
    ;; Check inside package form for import clauses and declared name
    (let ((pkg-form (find-package-form forms)))
      (when pkg-form
        ;; Extract declared package name
        (setf declared-name (extract-package-name pkg-form))
        ;; Extract imports
        (dolist (clause (cddr pkg-form))
          (when (and (consp clause)
                     (symbolp (car clause))
                     (string-equal "IMPORT" (symbol-name (car clause))))
            (dolist (spec (cdr clause))
              (when (and (consp spec) (>= (length spec) 1))
                (push (string (first spec)) requires)))))))
    (values (remove-duplicates (nreverse requires) :test #'string-equal)
            declared-name)))

(defun extract-e-file-requires (filepath)
  "Extract required package names from an .e file.
   Legacy wrapper around extract-e-file-info."
  (extract-e-file-info filepath))

(defun find-e-source-info (src-dir)
  "Find all .e source files in SRC-DIR and create e-source-info for each.
   Extracts dependencies and declared package names from each file."
  (let ((files (list-e-files src-dir)))
    (mapcar (lambda (path)
              (multiple-value-bind (requires declared-name)
                  (extract-e-file-info path)
                (make-e-source-info
                 :uri path
                 :package-name (derive-package-from-path path src-dir)
                 :declared-name declared-name
                 :src-root src-dir
                 :requires requires)))
            files)))

(defun load-e-project (module-path)
  "Load an e-module from MODULE-PATH (directory with module.lisp).
   Returns an e-project structure.

   The module.lisp should have :source-type :e to indicate this is an .e module."
  (let* ((module-file (path:path-string (path:path-join module-path "module.lisp")))
         (metadata (with-open-file (s module-file) (read s)))
         (name (getf metadata :name))
         (source-dirs (or (getf metadata :sources) '("src")))
         (test-dirs (or (getf metadata :tests) '("tests"))))
    (flet ((collect-sources (dirs)
             "Collect all .e files from the given directories"
             (let ((all-sources nil))
               (dolist (dir dirs)
                 (let ((full-dir (path:path-string (path:path-join module-path dir))))
                   (when (and (probe-file full-dir)
                              (fs:dir-p full-dir))
                     (setf all-sources
                           (append all-sources
                                   (find-e-source-info full-dir))))))
               all-sources)))
      (make-e-project
       :name name
       :path module-path
       :src-root (path:path-string (path:path-join module-path (first source-dirs)))
       :sources (collect-sources source-dirs)
       :tests (collect-sources test-dirs)
       :requires (getf metadata :requires)
       :metadata metadata))))

;;; ---------------------------------------------------------------------------
;;; Module Building
;;; ---------------------------------------------------------------------------

(defun e-build-order (sources)
  "Return SOURCES in topological order based on dependencies.
   Files are sorted so that dependencies are loaded before dependents.
   Returns two values: sorted list and any cyclic dependencies found."
  (let* (;; Map from uri -> e-source-info
         (nodes (make-hash-table :test 'equal))
         ;; Map from package-name (uppercase) -> e-source-info
         ;; Supports both path-derived names and declared package names
         (local-packages (make-hash-table :test 'equalp))
         (visiting (make-hash-table :test 'equal))
         (cycles nil)
         (sorted nil))
    ;; Build lookup tables
    ;; Index by both path-derived name and declared name for flexible matching
    (dolist (source sources)
      (setf (gethash (e-source-info-uri source) nodes) source)
      ;; Add entry for path-derived name
      (setf (gethash (e-source-info-package-name source) local-packages) source)
      ;; Also add entry for declared name if different
      (let ((declared (e-source-info-declared-name source)))
        (when (and declared
                   (not (string-equal declared (e-source-info-package-name source))))
          (setf (gethash declared local-packages) source))))
    ;; Topological sort via DFS
    (labels ((dep-uris (source)
               ;; Get URIs of local dependencies
               (loop for pkg in (e-source-info-requires source)
                     for dep-source = (gethash pkg local-packages)
                     when dep-source
                       collect (e-source-info-uri dep-source)))
             (visit (uri path)
               (when (gethash uri visiting)
                 (push (ldiff path (member uri path :test 'equal)) cycles)
                 (return-from visit nil))
               (when (member uri sorted :test 'equal)
                 (return-from visit t))
               (let ((source (gethash uri nodes)))
                 (unless source
                   (return-from visit t))
                 (setf (gethash uri visiting) t)
                 (dolist (dep-uri (dep-uris source))
                   (visit dep-uri (cons uri path)))
                 (remhash uri visiting)
                 (push uri sorted)
                 t)))
      ;; Visit all sources
      (dolist (source sources)
        (visit (e-source-info-uri source) nil))
      ;; Return sorted sources and any cycles
      (values
       (mapcar (lambda (uri) (gethash uri nodes))
               (nreverse sorted))
       cycles))))

(defun fasl-path-for-e-source (source module-path)
  "Compute the FASL output path for an e-source-info.
   Structure: target/package/fasl/lib/relative-path.fasl"
  (let* ((uri (e-source-info-uri source))
         (src-root (e-source-info-src-root source))
         ;; Get relative path from src root
         (rel-path (if (str:starts-with-p uri src-root)
                       (subseq uri (length src-root))
                       uri))
         ;; Strip leading slash if present
         (clean-rel-path (if (and (> (length rel-path) 0)
                                  (char= (char rel-path 0) #\/))
                             (subseq rel-path 1)
                             rel-path))
         ;; Change extension from .e to .fasl
         (fasl-rel-path (if (str:ends-with-p clean-rel-path ".e")
                            (concatenate 'string
                                         (subseq clean-rel-path 0 (- (length clean-rel-path) 2))
                                         ".fasl")
                            (concatenate 'string clean-rel-path ".fasl")))
         ;; Full target path
         (target-path (path:path-string
                       (path:path-join module-path "target" "package" "fasl" "lib" fasl-rel-path))))
    target-path))

(defun prepare-e-source-for-compilation (uri pkg-name)
  "Prepare an .e source file for compilation.
   If the file has no package form, creates a temp file with implicit package.
   Returns (values source-path temp-file-p actual-pkg-name) where:
   - temp-file-p indicates if cleanup needed
   - actual-pkg-name is the package name to use for exports (may differ from path-derived)"
  (let* ((forms (read-e-file uri))
         (pkg-form (find-package-form forms)))
    (if pkg-form
        ;; File has its own package form
        (let ((declared-name (extract-package-name pkg-form))
              (expected-name (string-upcase pkg-name)))
          ;; Warn if declared name doesn't match path-derived name
          (unless (string-equal declared-name expected-name)
            (warn "Package ~A declared in ~A but path suggests ~A"
                  declared-name uri expected-name))
          ;; Return with the declared name (respect what the file says)
          (values uri nil declared-name))
        ;; No package form - create temp file with implicit package
        (let* ((imports (extract-imports forms))
               (pkg-name-upper (string-upcase pkg-name))
               (pkg-sym (intern pkg-name-upper :keyword))
               (temp-path (make-pathname
                           :defaults uri
                           :type "e-tmp")))
          ;; Create the package first (with uppercase name)
          (redefine-package pkg-name-upper
                            :use '(:cl)
                            :local-nicknames (mapcar (lambda (pair)
                                                       (cons (car pair) (cdr pair)))
                                                     imports))
          ;; Re-read the file with *package* bound to target package
          ;; so symbols get interned correctly
          (let* ((target-pkg (find-package pkg-name-upper))
                 (body-forms (let ((*package* target-pkg)
                                   (*readtable* reader:*epsilon-readtable*))
                               (with-open-file (stream uri :direction :input)
                                 (loop for form = (read stream nil :eof)
                                       until (eq form :eof)
                                       ;; Skip import/export forms
                                       unless (and (consp form)
                                                   (member (car form) '(import export)))
                                       collect form)))))
            ;; Write temp file with in-package
            (with-open-file (out temp-path :direction :output
                                           :if-exists :supersede)
              (let ((*package* target-pkg)
                    (*print-case* :downcase))
                (format out ";;; Generated from ~A~%" uri)
                (format out "(in-package ~S)~%~%" pkg-sym)
                (dolist (form body-forms)
                  (write form :stream out :pretty t)
                  (terpri out)
                  (terpri out))))
            (values (namestring temp-path) t pkg-name-upper))))))

(defun build-e-source (source module-path &key verbose force)
  "Compile and load a single e-source-info.
   Returns T on success, signals error on failure."
  (let* ((uri (e-source-info-uri source))
         (pkg-name (e-source-info-package-name source))
         (fasl-path (fasl-path-for-e-source source module-path))
         (actual-pkg-name nil))  ; Will be set during prepare/determine phase
    (when verbose
      (format t "~&; Building ~A~%" pkg-name))
    ;; Ensure target directory exists
    (fs:make-dirs (path:path-parent (path:make-path fasl-path)))
    ;; Check if compilation is needed
    (let ((needs-compile (or force
                             (not (probe-file fasl-path))
                             (and (probe-file uri)
                                  (probe-file fasl-path)
                                  (> (file-write-date uri)
                                     (file-write-date fasl-path))))))
      ;; Determine actual package name and compile if needed
      (if needs-compile
          ;; Compile: prepare source (handle implicit packages)
          (multiple-value-bind (source-path temp-file-p declared-pkg)
              (prepare-e-source-for-compilation uri pkg-name)
            (setf actual-pkg-name declared-pkg)
            (unwind-protect
                 (let ((*readtable* reader:*epsilon-readtable*))
                   (compile-file source-path :output-file fasl-path :verbose nil :print nil))
              (when temp-file-p
                (delete-file source-path))))
          ;; No compile needed: determine package name and ensure it exists
          (let* ((forms (read-e-file uri))
                 (pkg-form (find-package-form forms))
                 (imports (extract-imports forms))
                 (pkg-imports (when pkg-form (extract-package-form-imports pkg-form)))
                 (pkg-shadows (when pkg-form (extract-package-form-shadow pkg-form))))
            (setf actual-pkg-name
                  (if pkg-form
                      (extract-package-name pkg-form)
                      (string-upcase pkg-name)))
            (unless (find-package actual-pkg-name)
              (redefine-package actual-pkg-name
                                :use '(:cl)
                                :local-nicknames (or pkg-imports
                                                     (mapcar (lambda (pair)
                                                               (cons (car pair) (cdr pair)))
                                                             imports))
                                :shadow pkg-shadows))))
      ;; Load the FASL
      (let ((*readtable* reader:*epsilon-readtable*))
        (load fasl-path))
      ;; Auto-export public symbols
      (sync-package-exports (string-upcase actual-pkg-name)))
    t))

(defun build-e-module (project &key verbose force)
  "Build all sources in PROJECT in dependency order.
   Returns T on success, signals error if cyclic dependencies found."
  (multiple-value-bind (ordered cycles)
      (e-build-order (e-project-sources project))
    (when cycles
      (error "Cyclic dependencies detected in ~A: ~A"
             (e-project-name project) cycles))
    (let ((module-path (e-project-path project)))
      (dolist (source ordered)
        (build-e-source source module-path :verbose verbose :force force)))
    t))

;;; ---------------------------------------------------------------------------
;;; Module Loading API
;;; ---------------------------------------------------------------------------

(defun load-e-module (environment module-name &key force verbose)
  "Load an e-module by name.

   This parallels epsilon.loader:load-module but for :source-type :e modules.

   Process:
   1. Get module-info from environment's registry
   2. Load dependencies recursively (via standard loader)
   3. Build and load all .e sources
   4. Mark module as loaded

   ENVIRONMENT - The build environment (from epsilon.loader)
   MODULE-NAME - Name of the module to load
   FORCE - Force recompilation regardless of timestamps
   VERBOSE - Print progress information"
  ;; Import the functions we need from loader - they're in the same package hierarchy
  (let* ((get-module (symbol-function (find-symbol "GET-MODULE" "EPSILON.LOADER")))
         (module-loaded-p-fn (symbol-function (find-symbol "MODULE-LOADED-P" "EPSILON.LOADER")))
         (module-location-fn (symbol-function (find-symbol "MODULE-LOCATION" "EPSILON.LOADER")))
         (mark-module-loaded (symbol-function (find-symbol "MARK-MODULE-LOADED" "EPSILON.LOADER")))
         (find-module-fn (symbol-function (find-symbol "FIND-MODULE" "EPSILON.LOADER")))
         (load-module-fn (symbol-function (find-symbol "LOAD-MODULE" "EPSILON.LOADER")))
         (module-info (funcall get-module environment module-name)))
    ;; Check if already loaded
    (when (and module-info (funcall module-loaded-p-fn module-info) (not force))
      (when verbose
        (format t "~&; Module ~A already loaded, skipping~%" module-name))
      (return-from load-e-module t))
    ;; Get module location
    (unless module-info
      (error "Unknown module: ~A" module-name))
    (let* ((location (funcall module-location-fn module-info))
           (module-path (if (pathnamep location)
                            (namestring location)
                            (path:path-string location)))
           (project (load-e-project module-path)))
      ;; Load dependencies first (using standard loader - they may be .lisp modules)
      (dolist (dep-name (e-project-requires project))
        (let ((resolved-dep (funcall find-module-fn environment :provides dep-name)))
          (unless (funcall module-loaded-p-fn resolved-dep)
            (funcall load-module-fn environment
                     (funcall (symbol-function (find-symbol "MODULE-NAME" "EPSILON.LOADER"))
                              resolved-dep)))))
      ;; Build the .e module
      (build-e-module project :verbose verbose :force force)
      ;; Mark as loaded
      (funcall mark-module-loaded environment module-name)
      t)))
