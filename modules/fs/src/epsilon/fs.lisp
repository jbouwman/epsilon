;;;; epsilon.fs -- Definitive public filesystem interface
;;;;
;;;; This module is the canonical filesystem API for Epsilon.  It is
;;;; ambient -- auto-loaded after bootstrap and always available to
;;;; any module without needing a :requires entry in module.sexp.
;;;;
;;;; Re-exports the stable public surface of epsilon.file and
;;;; epsilon.path, plus the query and watch facilities native to this
;;;; module.
;;;;
;;;; Usage:
;;;;   (defpackage my-thing (:import cl (epsilon.fs fs)))
;;;;   (fs:read-file-string "foo.txt")
;;;;   (fs:query "."
;;;;     '(>> :descendants
;;;;          (filter (and (name? "*.lisp")
;;;;                       (modified-within? 3600)))))

(cl:defpackage :epsilon.fs
  (:use :cl)
  (:local-nicknames
   (:file :epsilon.file)
   (:path :epsilon.path)
   (:str :epsilon.string)
   (:loader :epsilon.loader))
  (:import-from :epsilon.file
                ;; Path manipulation
                #:join-paths #:split-path #:basename #:dirname
                #:extension #:strip-extension #:with-extension
                #:absolute-path #:relative-path #:normalize-path #:clean-path
                ;; Predicates
                #:dir-p #:file-p #:exists-p #:file-exists-p #:directory-exists-p
                #:is-absolute #:is-relative
                ;; Directory operations
                #:home-dir #:list-dir #:list-dirs #:make-dirs
                #:runtime-dir #:temp-dir
                #:with-temp-file #:with-temp-dir
                #:copy-directory #:walk-path #:change-directory
                #:get-cwd
                ;; File operations
                #:copy-file #:rename
                #:read-file #:read-file-string #:read-file-bytes
                #:write-file-string #:write-file-bytes
                #:delete-directory #:delete-file* #:ensure-deleted
                #:list-contents #:list-directories #:list-files
                #:symbolic-link-p #:create-symbolic-link
                ;; Metadata
                #:access-time #:modification-time #:creation-time
                #:file-size #:file-inode
                #:owner #:group #:attributes)
  (:import-from :epsilon.path
                ;; Path object type
                #:path #:make-path #:ensure-path #:path-string
                #:path-absolute-p #:path-segments #:path-drive
                #:path-parent #:path-name #:path-stem #:path-extension
                #:path-join #:path-equal #:path-normalize
                ;; Predicates / traversal on path objects
                #:path-exists-p #:path-file-p #:path-directory-p #:path-type
                #:list-directory #:walk-directory
                ;; Pattern matching
                #:glob-match #:matches-pattern #:find-files
                ;; Temp paths
                #:make-temp-path #:with-temp-path
                ;; URI / string helpers
                #:normalize-separators #:string-path-join
                #:path-from-uri #:path-merge #:uri-merge
                ;; Special variables
                #:*work-directory*)
  (:export
   ;; ==== Re-exported path manipulation (from epsilon.file) ====
   #:join-paths #:split-path #:basename #:dirname
   #:extension #:strip-extension #:with-extension
   #:absolute-path #:relative-path #:normalize-path #:clean-path
   ;; ==== Re-exported predicates ====
   #:dir-p #:file-p #:exists-p #:file-exists-p #:directory-exists-p
   #:is-absolute #:is-relative
   ;; ==== Re-exported directory operations ====
   #:home-dir #:list-dir #:list-dirs #:make-dirs
   #:runtime-dir #:temp-dir
   #:with-temp-file #:with-temp-dir
   #:copy-directory #:walk-path #:change-directory
   #:get-cwd
   ;; ==== Re-exported file operations ====
   #:copy-file #:rename
   #:read-file #:read-file-string #:read-file-bytes
   #:write-file-string #:write-file-bytes
   #:delete-directory #:delete-file* #:ensure-deleted
   #:list-contents #:list-directories #:list-files
   #:symbolic-link-p #:create-symbolic-link
   ;; ==== Re-exported metadata ====
   #:access-time #:modification-time #:creation-time
   #:file-size #:file-inode
   #:owner #:group #:attributes
   ;; ==== Re-exported path object API (from epsilon.path) ====
   #:path #:make-path #:ensure-path #:path-string
   #:path-absolute-p #:path-segments #:path-drive
   #:path-parent #:path-name #:path-stem #:path-extension
   #:path-join #:path-equal #:path-normalize
   #:path-exists-p #:path-file-p #:path-directory-p #:path-type
   #:list-directory #:walk-directory
   #:glob-match #:matches-pattern #:find-files
   #:make-temp-path #:with-temp-path
   #:normalize-separators #:string-path-join
   #:path-from-uri #:path-merge #:uri-merge
   #:*work-directory*
   ;; ==== Native entry type ====
   #:entry
   #:make-entry
   #:entry-path
   #:entry-name
   #:entry-extension
   #:entry-kind
   #:entry-size
   #:entry-mtime
   #:entry-depth
   ;; ==== Native scanning ====
   #:scan
   #:scan-module
   ;; ==== Native query DSL ====
   #:query
   #:eval-fs-query
   #:compile-predicate
   ;; Predicates for programmatic use
   #:name?
   #:path?
   #:extension?))

(in-package :epsilon.fs)

;;; ============================================================================
;;; Entry Type
;;; ============================================================================

(defstruct entry
  "A filesystem entry with path and lazily-populated metadata."
  (path "" :type string)
  (name "" :type string)
  (extension nil :type (or string null))
  (kind :file :type keyword)
  (size nil :type (or integer null))
  (mtime nil :type (or integer null))
  (depth 0 :type fixnum))

(defun path-to-entry (path-string &key (depth 0))
  "Create an entry from a filesystem path string."
  (let* ((base (file:basename path-string))
         (raw-ext (file:extension path-string))
         ;; file:extension returns ".lisp" -- strip the leading dot
         (ext (when (and raw-ext (> (length raw-ext) 1) (char= (char raw-ext 0) #\.))
                (subseq raw-ext 1)))
         (kind (cond
                 ((file:dir-p path-string) :directory)
                 ((file:file-p path-string) :file)
                 (t :other))))
    (make-entry
     :path path-string
     :name base
     :extension ext
     :kind kind
     :depth depth)))

(defun ensure-size (entry)
  "Lazily fetch size for ENTRY if not yet populated."
  (unless (entry-size entry)
    (when (eq (entry-kind entry) :file)
      (handler-case
          (setf (entry-size entry)
                (with-open-file (s (entry-path entry) :direction :input)
                  (file-length s)))
        (error () nil))))
  (entry-size entry))

(defun ensure-mtime (entry)
  "Lazily fetch modification time for ENTRY if not yet populated."
  (unless (entry-mtime entry)
    (handler-case
        (setf (entry-mtime entry) (file:modification-time (entry-path entry)))
      (error () nil)))
  (entry-mtime entry))

;;; ============================================================================
;;; Scanning
;;; ============================================================================

(defun resolve-path (path)
  "Resolve PATH to an absolute path using process cwd."
  (if (file:is-absolute path)
    (file:clean-path path)
    (file:join-paths (file:get-cwd) path)))

(defun scan (root &key (recursive t) (max-depth nil))
  "Scan ROOT directory, returning a list of entry structs.
   With RECURSIVE t (default), descends into subdirectories.
   MAX-DEPTH limits recursion depth (nil = unlimited)."
  (let ((root-path (resolve-path root))
        (entries nil))
    (unless (file:dir-p root-path)
      (if (file:file-p root-path)
        (return-from scan (list (path-to-entry root-path)))
        (return-from scan nil)))
    (labels ((walk (dir depth)
               (when (or (null max-depth) (<= depth max-depth))
                 (handler-case
                     (file:walk-path dir
                                     (lambda (child)
                                       (push (path-to-entry child :depth depth) entries))
                                     :recursive nil
                                     :test (lambda (p)
                                             (or (file:file-p p) (file:dir-p p))))
                   (error () nil))
                 (when recursive
                   (handler-case
                       (file:walk-path dir
                                       (lambda (child)
                                         (walk child (1+ depth)))
                                       :recursive nil
                                       :test #'file:dir-p)
                     (error () nil))))))
      (walk root-path 0))
    (nreverse entries)))

(defun scan-module (module-name)
  "Scan a module's sources, tests, and integration tests.
   Returns a list of entry structs."
  (let ((mod (loader:get-module module-name)))
    (unless mod
      (error "Module not found: ~A" module-name))
    (let* ((mod-dir (loader:module-location mod))
           (project (loader:load-module-project
                     (if (stringp mod-dir)
                       mod-dir
                       (namestring mod-dir))))
           (entries nil))
      (flet ((collect-sources (source-list)
               (dolist (src source-list)
                 (let ((uri (namestring (loader:source-info-uri src))))
                   (when (file:file-p uri)
                     (push (path-to-entry uri) entries))))))
        (collect-sources (loader:module-project-sources project))
        (collect-sources (loader:module-project-tests project))
        (collect-sources (loader:module-project-integration-tests project)))
      (nreverse entries))))

;;; ============================================================================
;;; Predicate Compiler
;;; ============================================================================

(defun pred-head-p (expr name)
  "Check if EXPR is a list whose head symbol has NAME (cross-package safe)."
  (and (consp expr)
       (symbolp (car expr))
       (string= (symbol-name (car expr)) name)))

(defun compile-predicate (expr)
  "Compile a predicate expression into a function (entry -> boolean).

   Predicates:
     (name? pattern)           -- glob match on basename
     (path? pattern)           -- glob match on full path
     (extension? ext)          -- exact extension match
     (file?)                   -- entry is a file
     (dir?)                    -- entry is a directory
     (larger-than? bytes)      -- file size > bytes
     (smaller-than? bytes)     -- file size < bytes
     (modified-after? time)    -- mtime > universal-time
     (modified-within? secs)   -- mtime within last N seconds
     (deeper-than? n)          -- depth > n
     (contains? substring)     -- file content contains substring
     (and pred ...)            -- all predicates match
     (or pred ...)             -- any predicate matches
     (not pred)                -- negate predicate"
  (cond
    ;; Boolean composition
    ((pred-head-p expr "AND")
     (let ((preds (mapcar #'compile-predicate (cdr expr))))
       (lambda (entry)
         (every (lambda (p) (funcall p entry)) preds))))

    ((pred-head-p expr "OR")
     (let ((preds (mapcar #'compile-predicate (cdr expr))))
       (lambda (entry)
         (some (lambda (p) (funcall p entry)) preds))))

    ((pred-head-p expr "NOT")
     (let ((pred (compile-predicate (cadr expr))))
       (lambda (entry)
         (not (funcall pred entry)))))

    ;; Name matching
    ((pred-head-p expr "NAME?")
     (let ((pattern (cadr expr)))
       (lambda (entry)
         (path:glob-match pattern (entry-name entry)))))

    ;; Full path matching
    ((pred-head-p expr "PATH?")
     (let ((pattern (cadr expr)))
       (lambda (entry)
         (path:glob-match pattern (entry-path entry)))))

    ;; Extension matching
    ((pred-head-p expr "EXTENSION?")
     (let ((ext (cadr expr)))
       (lambda (entry)
         (and (entry-extension entry)
              (string-equal ext (entry-extension entry))))))

    ;; Kind predicates
    ((pred-head-p expr "FILE?")
     (lambda (entry)
       (eq (entry-kind entry) :file)))

    ((pred-head-p expr "DIR?")
     (lambda (entry)
       (eq (entry-kind entry) :directory)))

    ;; Size predicates
    ((pred-head-p expr "LARGER-THAN?")
     (let ((threshold (cadr expr)))
       (lambda (entry)
         (let ((size (ensure-size entry)))
           (and size (> size threshold))))))

    ((pred-head-p expr "SMALLER-THAN?")
     (let ((threshold (cadr expr)))
       (lambda (entry)
         (let ((size (ensure-size entry)))
           (and size (< size threshold))))))

    ;; Time predicates
    ((pred-head-p expr "MODIFIED-AFTER?")
     (let ((threshold (cadr expr)))
       (lambda (entry)
         (let ((mtime (ensure-mtime entry)))
           (and mtime (> mtime threshold))))))

    ((pred-head-p expr "MODIFIED-WITHIN?")
     (let ((seconds (cadr expr)))
       (lambda (entry)
         (let ((mtime (ensure-mtime entry)))
           (and mtime (> mtime (- (get-universal-time) seconds)))))))

    ;; Depth predicate
    ((pred-head-p expr "DEEPER-THAN?")
     (let ((n (cadr expr)))
       (lambda (entry)
         (> (entry-depth entry) n))))

    ;; Content search (reads file)
    ((pred-head-p expr "CONTAINS?")
     (let ((substring (cadr expr)))
       (lambda (entry)
         (and (eq (entry-kind entry) :file)
              (handler-case
                  (let ((content (file:read-file (entry-path entry))))
                    (search substring content :test #'char=))
                (error () nil))))))

    (t (error "Unknown predicate expression: ~S" expr))))

;;; ============================================================================
;;; Query Pipeline
;;; ============================================================================

(defun form-head-p (form name)
  "Check if FORM is a list whose first element is a symbol named NAME."
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) name)))

(defun eval-fs-query (query stream)
  "Evaluate QUERY against a stream of entry structs.
   Returns a list of entries.

   Query language:
     :self         -- identity (pass through)
     :children     -- direct children of directory entries
     :descendants  -- recursive descent into all subdirectories
     :files        -- keep only file entries
     :dirs         -- keep only directory entries
     :first        -- first entry only
     :last         -- last entry only
     :paths        -- extract path strings
     :names        -- extract name strings
     (>> s1 s2..)  -- pipeline: chain stages
     (filter p)    -- keep entries matching predicate
     (reject p)    -- remove entries matching predicate
     (sort-by f)   -- sort by :name, :size, :mtime, :path, :depth
     (limit n)     -- take first N entries"
  (cond
    ;; Identity
    ((eq query :self)
     stream)

    ;; Children of directory entries
    ((eq query :children)
     (mapcan (lambda (entry)
               (when (eq (entry-kind entry) :directory)
                 (scan (entry-path entry) :recursive nil)))
             stream))

    ;; Recursive descent
    ((eq query :descendants)
     (mapcan (lambda (entry)
               (when (eq (entry-kind entry) :directory)
                 (scan (entry-path entry) :recursive t)))
             stream))

    ;; Convenience shortcuts
    ((eq query :files)
     (remove-if-not (lambda (e) (eq (entry-kind e) :file)) stream))

    ((eq query :dirs)
     (remove-if-not (lambda (e) (eq (entry-kind e) :directory)) stream))

    ((eq query :first)
     (when stream (list (first stream))))

    ((eq query :last)
     (when stream (list (car (last stream)))))

    ;; Extract values
    ((eq query :paths)
     (mapcar #'entry-path stream))

    ((eq query :names)
     (mapcar #'entry-name stream))

    ;; Pipeline
    ((form-head-p query ">>")
     (let ((result stream))
       (dolist (stage (cdr query))
         (setf result (eval-fs-query stage result)))
       result))

    ;; Filter
    ((form-head-p query "FILTER")
     (let ((pred (compile-predicate (cadr query))))
       (remove-if-not pred stream)))

    ;; Reject
    ((form-head-p query "REJECT")
     (let ((pred (compile-predicate (cadr query))))
       (remove-if pred stream)))

    ;; Sort
    ((form-head-p query "SORT-BY")
     (let ((field (cadr query)))
       (ecase field
         (:name (sort (copy-list stream) #'string< :key #'entry-name))
         (:path (sort (copy-list stream) #'string< :key #'entry-path))
         (:size (sort (copy-list stream) #'<
                      :key (lambda (e) (or (ensure-size e) 0))))
         (:mtime (sort (copy-list stream) #'>
                       :key (lambda (e) (or (ensure-mtime e) 0))))
         (:depth (sort (copy-list stream) #'< :key #'entry-depth)))))

    ;; Limit
    ((form-head-p query "LIMIT")
     (let ((n (cadr query)))
       (subseq stream 0 (min n (length stream)))))

    (t (error "Unknown fs query expression: ~S" query))))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun query (root query-expr)
  "Query the filesystem starting at ROOT with QUERY-EXPR.
   ROOT is a path string. QUERY-EXPR is a pipeline expression.

   Example:
     (fs:query \".\"
       '(>> :descendants
            (filter (and (name? \"*.lisp\")
                         (not (path? \"**/node_modules/**\"))))))"
  (let* ((root-path (resolve-path root))
         (root-entries (list (path-to-entry root-path))))
    (eval-fs-query query-expr root-entries)))

