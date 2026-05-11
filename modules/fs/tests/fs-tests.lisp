;;;; Tests for epsilon.fs -- filesystem query facility

(defpackage epsilon.fs.tests
  (:import cl
           epsilon.test
           (epsilon.fs fs)
           (epsilon.file file)))

;;; ============================================================================
;;; Test fixture: temp directory with known contents
;;; ============================================================================

(defvar *test-root* (file:join-paths (file:get-cwd) ".epsilon/test-fs/"))

(defun setup-test-tree ()
  "Create a temp directory tree with known files for testing."
  (let ((root *test-root*))
    (file:make-dirs root)
    (file:make-dirs (file:join-paths root "sub/"))
    (file:write-file-string (file:join-paths root "alpha.lisp")
                            "(defpackage alpha)")
    (file:write-file-string (file:join-paths root "beta.txt")
                            "plain text")
    (file:write-file-string (file:join-paths root "sub/gamma.lisp")
                            "(defpackage gamma)")
    root))

(defun teardown-test-tree ()
  "Remove the temp directory tree."
  (when (file:exists-p *test-root*)
    (file:walk-path *test-root*
                    (lambda (p) (when (file:file-p p) (delete-file p)))
                    :recursive t)
    ;; Remove subdirectories then root
    (handler-case (sb-posix:rmdir (namestring (pathname (file:join-paths *test-root* "sub/"))))
      (error () nil))
    (handler-case (sb-posix:rmdir (namestring (pathname *test-root*)))
      (error () nil))))

;;; ============================================================================
;;; Entry construction
;;; ============================================================================

(deftest test-path-to-entry-file
  "path-to-entry creates correct entry for a file."
  (setup-test-tree)
  (unwind-protect
       (let* ((path (file:join-paths *test-root* "alpha.lisp"))
              (entry (fs::path-to-entry path)))
         (assert-equal (fs:entry-kind entry) :file)
         (assert-not-null (fs:entry-name entry) "Should have a name")
         (assert-equal (fs:entry-extension entry) "lisp")
         (assert-= (fs:entry-depth entry) 0))
    (teardown-test-tree)))

(deftest test-path-to-entry-directory
  "path-to-entry creates correct entry for a directory."
  (let ((entry (fs::path-to-entry (file:get-cwd))))
    (assert-equal (fs:entry-kind entry) :directory)))

;;; ============================================================================
;;; Lazy metadata
;;; ============================================================================

(deftest test-ensure-size
  "ensure-size lazily fetches file size."
  (setup-test-tree)
  (unwind-protect
       (let ((entry (fs::path-to-entry (file:join-paths *test-root* "alpha.lisp"))))
         (assert-not (fs:entry-size entry) "Size should start nil")
         (let ((size (fs::ensure-size entry)))
           (assert-true (and size (> size 0)) "Size should be positive")
           (assert-= (fs:entry-size entry) size "Size should be cached")))
    (teardown-test-tree)))

(deftest test-ensure-mtime
  "ensure-mtime lazily fetches modification time."
  (setup-test-tree)
  (unwind-protect
       (let ((entry (fs::path-to-entry (file:join-paths *test-root* "alpha.lisp"))))
         (assert-not (fs:entry-mtime entry) "Mtime should start nil")
         (let ((mtime (fs::ensure-mtime entry)))
           (assert-true (and mtime (> mtime 0)) "Mtime should be positive")
           (assert-= (fs:entry-mtime entry) mtime "Mtime should be cached")))
    (teardown-test-tree)))

;;; ============================================================================
;;; Scanning
;;; ============================================================================

(deftest test-scan-directory
  "scan returns entries for files in a directory."
  (setup-test-tree)
  (unwind-protect
       (let ((entries (fs:scan *test-root* :recursive nil)))
         (assert-true (> (length entries) 0) "Should find files")
         (assert-true (some (lambda (e) (search "alpha" (fs:entry-name e)))
                            entries)
             "Should find alpha.lisp"))
    (teardown-test-tree)))

(deftest test-scan-recursive
  "scan with recursive finds files in subdirectories."
  (setup-test-tree)
  (unwind-protect
       (let ((entries (fs:scan *test-root* :recursive t)))
         (assert-true (> (length entries) 1)
             "Recursive scan should find multiple files")
         (assert-true (some (lambda (e) (search "gamma" (fs:entry-name e)))
                            entries)
             "Should find file in subdirectory"))
    (teardown-test-tree)))

(deftest test-scan-single-file
  "scan on a single file returns one entry."
  (setup-test-tree)
  (unwind-protect
       (let ((entries (fs:scan (file:join-paths *test-root* "alpha.lisp"))))
         (assert-= (length entries) 1)
         (assert-equal (fs:entry-kind (first entries)) :file))
    (teardown-test-tree)))

(deftest test-scan-max-depth
  "scan respects max-depth."
  (setup-test-tree)
  (unwind-protect
       (let ((shallow (fs:scan *test-root* :recursive t :max-depth 0))
             (deep (fs:scan *test-root* :recursive t :max-depth 10)))
         (assert-true (<= (length shallow) (length deep))
             "Deeper scan should find at least as many entries"))
    (teardown-test-tree)))

;;; ============================================================================
;;; Predicate compilation
;;; ============================================================================

(deftest test-predicate-name-glob
  "name? predicate matches basename with glob."
  (let ((pred (fs:compile-predicate '(name? "*.lisp")))
        (entry (fs:make-entry :path "/foo/bar.lisp" :name "bar.lisp"
                              :extension "lisp" :kind :file)))
    (assert-true (funcall pred entry))))

(deftest test-predicate-name-no-match
  "name? predicate rejects non-matching names."
  (let ((pred (fs:compile-predicate '(name? "*.txt")))
        (entry (fs:make-entry :path "/foo/bar.lisp" :name "bar.lisp"
                              :extension "lisp" :kind :file)))
    (assert-not (funcall pred entry))))

(deftest test-predicate-extension
  "extension? predicate matches file extension."
  (let ((pred (fs:compile-predicate '(extension? "lisp")))
        (lisp (fs:make-entry :path "/a.lisp" :name "a.lisp"
                             :extension "lisp" :kind :file))
        (txt (fs:make-entry :path "/a.txt" :name "a.txt"
                            :extension "txt" :kind :file)))
    (assert-true (funcall pred lisp))
    (assert-not (funcall pred txt))))

(deftest test-predicate-file-and-dir
  "file? and dir? predicates match by kind."
  (let ((file-pred (fs:compile-predicate '(file?)))
        (dir-pred (fs:compile-predicate '(dir?)))
        (f (fs:make-entry :path "/a" :name "a" :kind :file))
        (d (fs:make-entry :path "/b" :name "b" :kind :directory)))
    (assert-true (funcall file-pred f))
    (assert-not (funcall file-pred d))
    (assert-not (funcall dir-pred f))
    (assert-true (funcall dir-pred d))))

(deftest test-predicate-and-composition
  "and composes predicates with logical AND."
  (let ((pred (fs:compile-predicate '(and (file?) (name? "*.lisp"))))
        (match (fs:make-entry :path "/a.lisp" :name "a.lisp"
                              :extension "lisp" :kind :file))
        (wrong-kind (fs:make-entry :path "/a.lisp" :name "a.lisp"
                                   :extension "lisp" :kind :directory))
        (wrong-name (fs:make-entry :path "/a.txt" :name "a.txt"
                                   :extension "txt" :kind :file)))
    (assert-true (funcall pred match))
    (assert-not (funcall pred wrong-kind))
    (assert-not (funcall pred wrong-name))))

(deftest test-predicate-or-composition
  "or composes predicates with logical OR."
  (let ((pred (fs:compile-predicate '(or (name? "*.lisp") (name? "*.txt"))))
        (lisp (fs:make-entry :path "/a.lisp" :name "a.lisp" :kind :file))
        (txt (fs:make-entry :path "/a.txt" :name "a.txt" :kind :file))
        (py (fs:make-entry :path "/a.py" :name "a.py" :kind :file)))
    (assert-true (funcall pred lisp))
    (assert-true (funcall pred txt))
    (assert-not (funcall pred py))))

(deftest test-predicate-not
  "not negates a predicate."
  (let ((pred (fs:compile-predicate '(not (dir?))))
        (f (fs:make-entry :path "/a" :name "a" :kind :file))
        (d (fs:make-entry :path "/b" :name "b" :kind :directory)))
    (assert-true (funcall pred f))
    (assert-not (funcall pred d))))

(deftest test-predicate-deeper-than
  "deeper-than? matches entries by depth."
  (let ((pred (fs:compile-predicate '(deeper-than? 2)))
        (shallow (fs:make-entry :path "/a" :name "a" :kind :file :depth 1))
        (deep (fs:make-entry :path "/a" :name "a" :kind :file :depth 5)))
    (assert-not (funcall pred shallow))
    (assert-true (funcall pred deep))))

;;; ============================================================================
;;; Query pipeline
;;; ============================================================================

(deftest test-query-filter
  "filter stage applies predicate to stream."
  (let* ((entries (list
                   (fs:make-entry :path "/a.lisp" :name "a.lisp" :kind :file)
                   (fs:make-entry :path "/b.txt" :name "b.txt" :kind :file)
                   (fs:make-entry :path "/c.lisp" :name "c.lisp" :kind :file)))
         (result (fs:eval-fs-query '(filter (name? "*.lisp")) entries)))
    (assert-= (length result) 2)))

(deftest test-query-reject
  "reject stage removes matching entries."
  (let* ((entries (list
                   (fs:make-entry :path "/a.lisp" :name "a.lisp" :kind :file)
                   (fs:make-entry :path "/b.txt" :name "b.txt" :kind :file)))
         (result (fs:eval-fs-query '(reject (name? "*.txt")) entries)))
    (assert-= (length result) 1)
    (assert-equal (fs:entry-name (first result)) "a.lisp")))

(deftest test-query-pipeline
  ">> chains multiple stages."
  (let* ((entries (list
                   (fs:make-entry :path "/a.lisp" :name "a.lisp" :kind :file)
                   (fs:make-entry :path "/b.txt" :name "b.txt" :kind :file)
                   (fs:make-entry :path "/c.lisp" :name "c.lisp" :kind :file)
                   (fs:make-entry :path "/d" :name "d" :kind :directory)))
         (result (fs:eval-fs-query
                  '(>> (filter (file?)) (filter (name? "*.lisp")))
                  entries)))
    (assert-= (length result) 2)))

(deftest test-query-sort-by-name
  "sort-by :name sorts entries alphabetically."
  (let* ((entries (list
                   (fs:make-entry :path "/c" :name "c" :kind :file)
                   (fs:make-entry :path "/a" :name "a" :kind :file)
                   (fs:make-entry :path "/b" :name "b" :kind :file)))
         (result (fs:eval-fs-query '(sort-by :name) entries)))
    (assert-equal (fs:entry-name (first result)) "a")
    (assert-equal (fs:entry-name (second result)) "b")
    (assert-equal (fs:entry-name (third result)) "c")))

(deftest test-query-limit
  "limit takes first N entries."
  (let* ((entries (list
                   (fs:make-entry :path "/a" :name "a" :kind :file)
                   (fs:make-entry :path "/b" :name "b" :kind :file)
                   (fs:make-entry :path "/c" :name "c" :kind :file)))
         (result (fs:eval-fs-query '(limit 2) entries)))
    (assert-= (length result) 2)))

(deftest test-query-shortcuts
  ":files and :dirs filter by kind."
  (let* ((entries (list
                   (fs:make-entry :path "/a" :name "a" :kind :file)
                   (fs:make-entry :path "/b" :name "b" :kind :directory)
                   (fs:make-entry :path "/c" :name "c" :kind :file)))
         (files (fs:eval-fs-query :files entries))
         (dirs (fs:eval-fs-query :dirs entries)))
    (assert-= (length files) 2)
    (assert-= (length dirs) 1)))

(deftest test-query-paths-extraction
  ":paths extracts path strings from entries."
  (let* ((entries (list
                   (fs:make-entry :path "/a.lisp" :name "a.lisp" :kind :file)
                   (fs:make-entry :path "/b.lisp" :name "b.lisp" :kind :file)))
         (paths (fs:eval-fs-query :paths entries)))
    (assert-= (length paths) 2)
    (assert-true (stringp (first paths)))))

;;; ============================================================================
;;; Integration: query over real filesystem
;;; ============================================================================

(deftest test-query-real-filesystem
  "query finds .lisp files in a populated directory."
  (setup-test-tree)
  (unwind-protect
       (let ((result (fs:query *test-root*
                       '(>> :descendants
                            (filter (name? "*.lisp"))))))
         (assert-true (> (length result) 0)
             "Should find .lisp files")
         (assert-true (every (lambda (e) (search ".lisp" (fs:entry-name e)))
                             result)
             "All results should be .lisp files"))
    (teardown-test-tree)))

(deftest test-query-contains-predicate
  "contains? reads file content for substring matching."
  (setup-test-tree)
  (unwind-protect
       (let ((result (fs:query *test-root*
                       '(>> :descendants
                            (filter (and (name? "*.lisp")
                                         (contains? "defpackage")))))))
         (assert-true (> (length result) 0)
             "Should find files containing defpackage"))
    (teardown-test-tree)))
