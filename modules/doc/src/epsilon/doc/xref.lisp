;;;; epsilon.doc.xref -- Bidirectional cross-reference index
;;;;
;;;; Builds an index mapping IMPL references, symbol cross-refs, and
;;;; condition hierarchies between planning docs and code.

(defpackage epsilon.doc.xref
  (:use :cl)
  (:import (epsilon.doc.parse parse)
           (epsilon.annotate ann)
           (epsilon.json json)
           (epsilon.loader loader))
  (:export
   #:build-xref-index
   #:build-full-xref-index
   #:xref-lookup
   #:extract-see-refs
   #:parse-impl-file-refs
   #:generate-xref-json
   #:validate-xref
   #:scan-impl-docs
   #:extract-impl-number))

;;; See-reference extraction

(defun extract-see-refs (symbol)
  "Extract all :see references for SYMBOL from both docstring and annotations.
   Returns a deduplicated list of reference strings (e.g., IMPL numbers, symbol names)."
  (let ((refs nil))
    ;; From docstring
    (let ((docstring (or (documentation symbol 'function)
                         (documentation symbol 'variable)
                         (documentation symbol 'type))))
      (when docstring
        (let* ((parsed (parse:parse-docstring docstring))
               (see (getf parsed :see)))
          (dolist (ref see)
            (pushnew ref refs :test #'string=)))))
    ;; From annotations
    (let ((annotations (ann:annotations symbol)))
      (when annotations
        (let ((see-val (cdr (assoc :see annotations))))
          (when see-val
            (if (listp see-val)
              (dolist (ref see-val)
                (pushnew ref refs :test #'string=))
              (pushnew see-val refs :test #'string=))))))
    (nreverse refs)))

;;; Cross-reference index

(defun build-xref-index (&key symbols)
  "Build a cross-reference index from SYMBOLS.
   Returns an index (alist mapping ref-string -> list of entries).
   Each entry is a plist (:symbol sym :package pkg-name)."
  (let ((index nil))
    (dolist (sym symbols)
      (let ((refs (extract-see-refs sym)))
        (dolist (ref refs)
          (let ((entry (list :symbol (symbol-name sym)
                             :package (when (symbol-package sym)
                                        (package-name (symbol-package sym))))))
            (let ((existing (assoc ref index :test #'string=)))
              (if existing
                (push entry (cdr existing))
                (push (cons ref (list entry)) index)))))))
    (nreverse index)))

(defun xref-lookup (key &key index)
  "Look up KEY in cross-reference INDEX.
   Returns a list of entry plists, or NIL."
  (cdr (assoc key index :test #'string=)))

;;; Implement doc file reference parsing

(defun parse-impl-file-refs (text)
  "Parse implement doc TEXT and extract file paths from **Files:** sections.
   Recognizes lines like:
     - `path/to/file.lisp`
     - `path/to/file.lisp` (description)
   Returns a list of file path strings."
  (let ((refs nil)
        (lines (parse:split-lines text)))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        ;; Match lines like: - `path/to/file.lisp`
        (when (and (>= (length trimmed) 4)
                   (char= (char trimmed 0) #\-)
                   (position #\` trimmed))
          (let* ((bt1 (position #\` trimmed))
                 (bt2 (position #\` trimmed :start (1+ bt1))))
            (when (and bt1 bt2 (> bt2 (1+ bt1)))
              (push (subseq trimmed (1+ bt1) bt2) refs))))))
    (nreverse refs)))

;;; Full index building from loaded packages

(defun collect-all-see-symbols ()
  "Collect all symbols with :see references across all loaded packages.
   Returns a list of symbols."
  (let ((symbols nil))
    (dolist (pkg (list-all-packages))
      (do-external-symbols (sym pkg)
        (when (extract-see-refs sym)
          (push sym symbols))))
    symbols))

(defun build-full-xref-index ()
  "Build a cross-reference index from all loaded packages.
   Returns an index (alist mapping ref-string -> list of entries)."
  (build-xref-index :symbols (collect-all-see-symbols)))

;;; Implement doc scanning

(defun repo-root ()
  "Derive the repository root from EPSILON_HOME (one level up from epsilon/)."
  (let ((home (sb-posix:getenv "EPSILON_HOME")))
    (when home
      (let ((clean (if (char= (char home (1- (length home))) #\/)
                     (subseq home 0 (1- (length home)))
                     home)))
        ;; Go up one directory from epsilon/
        (let ((slash (position #\/ clean :from-end t)))
          (when slash
            (concatenate 'string (subseq clean 0 (1+ slash)))))))))

(defun scan-impl-docs (&key (directory nil directory-p))
  "Scan implement documents in DIRECTORY and extract IMPL numbers and file refs.
   Defaults to manual/implement/ under the repo root.
   Returns an alist mapping IMPL number strings to plists:
     (:file impl-doc-path :refs list-of-file-paths)."
  (let* ((base-dir (if directory-p
                     directory
                     (let ((root (repo-root)))
                       (if root
                         (concatenate 'string root "manual/implement/")
                         "manual/implement/"))))
         (results nil)
         (dir-path (if (char= (char base-dir (1- (length base-dir))) #\/)
                     base-dir
                     (concatenate 'string base-dir "/"))))
    (dolist (file (directory (merge-pathnames "*.md" dir-path)))
      (let* ((filename (file-namestring file))
             (impl-num (extract-impl-number filename)))
        (when impl-num
          (handler-case
              (let* ((content (with-open-file (s file :direction :input)
                                (let ((str (make-string (file-length s))))
                                  (read-sequence str s)
                                  str)))
                     (file-refs (parse-impl-file-refs content)))
                (push (cons impl-num
                            (list :file (namestring file)
                                  :refs file-refs))
                      results))
            (error () nil)))))
    (nreverse results)))

(defun extract-impl-number (filename)
  "Extract IMPL number from a filename like '297_foo.md'.
   Returns a string like 'IMPL-297', or NIL."
  (let ((underscore (position #\_ filename)))
    (when (and underscore (> underscore 0))
      (let ((num-str (subseq filename 0 underscore)))
        (when (every #'digit-char-p num-str)
          (concatenate 'string "IMPL-" num-str))))))

;;; JSON index generation

(defun index-to-json-map (index impl-docs)
  "Convert an xref INDEX and IMPL-DOCS scan into a vector of maps for JSON output."
  (let ((entries nil))
    ;; Code-to-docs direction: symbols tagged with each IMPL
    (dolist (pair index)
      (let* ((ref (car pair))
             (syms (cdr pair))
             (impl-doc (cdr (assoc ref impl-docs :test #'string=)))
             (sym-maps (mapcar (lambda (s)
                                 (epsilon.map:make-map
                                  "symbol" (getf s :symbol)
                                  "package" (or (getf s :package) "")))
                               syms))
             (entry (epsilon.map:make-map
                     "ref" ref
                     "symbols" (coerce sym-maps 'vector))))
        (when impl-doc
          (setf entry (epsilon.map:make-map
                       "ref" ref
                       "symbols" (coerce sym-maps 'vector)
                       "implFile" (getf impl-doc :file)
                       "fileRefs" (coerce (or (getf impl-doc :refs) #()) 'vector))))
        (push entry entries)))
    ;; Add impl docs that have no code references
    (dolist (pair impl-docs)
      (unless (assoc (car pair) index :test #'string=)
        (let ((doc (cdr pair)))
          (push (epsilon.map:make-map
                 "ref" (car pair)
                 "symbols" #()
                 "implFile" (getf doc :file)
                 "fileRefs" (coerce (or (getf doc :refs) #()) 'vector))
                entries))))
    (coerce (nreverse entries) 'vector)))

(defun generate-xref-json (&key output-path impl-directory)
  "Generate manual/xref-index.json from all loaded modules and implement docs.
   Defaults to repo-root-relative paths.  Returns the output path."
  (let* ((root (or (repo-root) ""))
         (out (or output-path (concatenate 'string root "manual/xref-index.json")))
         (index (build-full-xref-index))
         (impl-docs (if impl-directory
                      (scan-impl-docs :directory impl-directory)
                      (scan-impl-docs)))
         (json-data (index-to-json-map index impl-docs))
         (json-str (json:encode-to-string json-data)))
    (with-open-file (s output-path :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string json-str s))
    out))

;;; Validation

(defun validate-xref (&key impl-directory repo-root)
  "Validate cross-references between code annotations and implement docs.
   Defaults to repo-root-relative paths.

   Returns a plist with:
     :errors   -- List of error strings (blocking).
     :warnings -- List of warning strings (non-blocking).
     :valid-p  -- T if no errors."
  (let* ((root (or repo-root (repo-root) "."))
         (index (build-full-xref-index))
         (impl-docs (if impl-directory
                      (scan-impl-docs :directory impl-directory)
                      (scan-impl-docs)))
         (errors nil)
         (warnings nil))
    ;; Rule 1: Every #@(:see "IMPL-NNN") must reference an existing impl doc
    (dolist (pair index)
      (let ((ref (car pair)))
        (when (and (>= (length ref) 5)
                   (string= "IMPL-" ref :end2 5))
          (unless (assoc ref impl-docs :test #'string=)
            (push (format nil "Code references ~A but no implement doc found" ref)
                  errors)))))
    ;; Rule 2: Every **Files:** path must reference an existing file
    (dolist (pair impl-docs)
      (let* ((impl-num (car pair))
             (doc (cdr pair))
             (file-refs (getf doc :refs)))
        (dolist (fref file-refs)
          ;; Skip non-file paths (containing wildcards, etc.)
          (when (and (> (length fref) 0)
                     (not (find #\* fref))
                     (not (find #\? fref)))
            (handler-case
                (let ((full-path (merge-pathnames fref root)))
                  (unless (probe-file full-path)
                    (push (format nil "~A references file ~A which does not exist"
                                  impl-num fref)
                          errors)))
              (error () nil))))))
    ;; Rule 3 (warning): Impl docs that reference files with no reciprocal annotation
    (dolist (pair impl-docs)
      (let ((impl-num (car pair)))
        (unless (assoc impl-num index :test #'string=)
          (push (format nil "~A has no code annotations referencing it" impl-num)
                warnings))))
    (list :errors (nreverse errors)
          :warnings (nreverse warnings)
          :valid-p (null errors))))
