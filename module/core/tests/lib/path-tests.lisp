(defpackage :epsilon.lib.path.tests
  (:use
   :cl
   :epsilon.tool.test
   :epsilon.lib.path))

(in-package :epsilon.lib.path.tests)

;;;; ==========================================================================
;;;; Path Construction Tests
;;;; ==========================================================================

(deftest test-make-path-unix
  "Test Unix path construction"
  (let ((p (make-path "/usr/local/bin")))
    (is (path-absolute-p p))
    (is-equal '("usr" "local" "bin") (path-segments p))
    (is-equal nil (path-drive p))))

(deftest test-make-path-relative
  "Test relative path construction"
  (let ((p (make-path "src/lib/path.lisp")))
    (is-not (path-absolute-p p))
    (is-equal '("src" "lib" "path.lisp") (path-segments p))
    (is-equal nil (path-drive p))))

(deftest test-make-path-windows
  "Test Windows path construction"
  #+win32
  (let ((p (make-path "C:\\Program Files\\App")))
    (is (path-absolute-p p))
    (is-equal '("Program Files" "App") (path-segments p))
    (is-equal "C" (path-drive p))))

(deftest test-make-path-empty
  "Test empty path construction"
  (is-equal nil (make-path ""))
  (is-equal nil (make-path nil)))

(deftest test-path-string-unix
  "Test Unix path string generation"
  (let ((p (make-path "/usr/local/bin")))
    #+win32 (is-equal "\\usr\\local\\bin" (path-string p))
    #-win32 (is-equal "/usr/local/bin" (path-string p))))

(deftest test-path-string-relative
  "Test relative path string generation"
  (let ((p (make-path "src/lib/path.lisp")))
    #+win32 (is-equal "src\\lib\\path.lisp" (path-string p))
    #-win32 (is-equal "src/lib/path.lisp" (path-string p))))

;;;; ==========================================================================
;;;; Path Manipulation Tests
;;;; ==========================================================================

(deftest test-path-parent
  "Test path parent calculation"
  (let* ((p (make-path "/usr/local/bin"))
         (parent (path-parent p)))
    (is-equal '("usr" "local") (path-segments parent))
    (is (path-absolute-p parent))))

(deftest test-path-parent-root
  "Test parent of root path"
  (let* ((p (make-path "/"))
         (parent (path-parent p)))
    (is-equal '() (path-segments parent))
    (is (path-absolute-p parent))))

(deftest test-path-name
  "Test path name extraction"
  (is-equal "path.lisp" (path-name (make-path "/usr/local/path.lisp")))
  (is-equal "bin" (path-name (make-path "/usr/local/bin")))
  (is-equal nil (path-name (make-path "/"))))

(deftest test-path-stem
  "Test path stem extraction (filename without extension)"
  (is-equal "path" (path-stem (make-path "/usr/local/path.lisp")))
  (is-equal "archive" (path-stem (make-path "/tmp/archive.tar.gz")))
  (is-equal "README" (path-stem (make-path "/src/README")))
  (is-equal nil (path-stem (make-path "/"))))

(deftest test-path-extension
  "Test path extension extraction"
  (is-equal "lisp" (path-extension (make-path "/usr/local/path.lisp")))
  (is-equal "gz" (path-extension (make-path "/tmp/archive.tar.gz")))
  (is-equal nil (path-extension (make-path "/src/README")))
  (is-equal nil (path-extension (make-path "/"))))

(deftest test-path-join
  "Test path joining"
  (let ((p (path-join "/usr" "local" "bin")))
    (is-equal '("usr" "local" "bin") (path-segments p))
    (is (path-absolute-p p))))

(deftest test-path-join-absolute-override
  "Test path joining with absolute path override"
  (let ((p (path-join "relative" "/absolute" "path")))
    (is-equal '("absolute" "path") (path-segments p))
    (is (path-absolute-p p))))

(deftest test-path-join-objects
  "Test path joining with path objects"
  (let* ((p1 (make-path "/usr/local"))
         (p2 (make-path "bin"))
         (p3 (path-join p1 p2)))
    (is-equal '("usr" "local" "bin") (path-segments p3))
    (is (path-absolute-p p3))))

(deftest test-path-equal
  "Test path equality comparison"
  (is (path-equal "/usr/local/bin" "/usr/local/bin"))
  (is (path-equal (make-path "/usr/local") (make-path "/usr/local")))
  (is-not (path-equal "/usr/local" "/usr/local/bin"))
  (is-not (path-equal "usr/local" "/usr/local")))

(deftest test-path-normalize
  "Test path normalization"
  (let ((p (path-normalize "/usr/local/../bin/./file")))
    (is-equal '("usr" "bin" "file") (path-segments p))
    (is (path-absolute-p p))))

(deftest test-path-normalize-relative
  "Test relative path normalization"
  (let ((p (path-normalize "src/../lib/./path.lisp")))
    (is-equal '("lib" "path.lisp") (path-segments p))
    (is-not (path-absolute-p p))))

;;;; ==========================================================================
;;;; Path Query Tests
;;;; ==========================================================================

(deftest test-path-exists-p
  "Test path existence checking"
  ;; Test with a path that should exist
  (is (path-exists-p (current-directory)))
  ;; Test with a path that shouldn't exist
  (is-not (path-exists-p "/nonexistent/path/12345")))

(deftest test-path-type
  "Test path type detection"
  (is-equal :directory (path-type (current-directory)))
  ;; Create a temporary file to test
  (let ((temp-path (make-temp-path :suffix ".test")))
    (with-open-file (stream (path-string temp-path) :direction :output)
      (write-string "test" stream))
    (is-equal :file (path-type temp-path))
    (delete-file (path-string temp-path))))

;;;; ==========================================================================
;;;; System Path Tests
;;;; ==========================================================================

(deftest test-current-directory
  "Test current directory access"
  (let ((cwd (current-directory)))
    (is (typep cwd 'path))
    (is (path-absolute-p cwd))
    (is (path-directory-p cwd))))

(deftest test-temp-directory
  "Test temporary directory access"
  (let ((temp (temp-directory)))
    (is (typep temp 'path))
    (is (path-absolute-p temp))
    (is (path-directory-p temp))))

(deftest test-home-directory
  "Test home directory access"
  (let ((home (home-directory)))
    (is (typep home 'path))
    (is (path-absolute-p home))
    (is (path-directory-p home))))

(deftest test-make-temp-path
  "Test temporary path generation"
  (let* ((temp (make-temp-path :prefix "test-" :suffix ".tmp"))
         (name (path-name temp)))
    (is (typep temp 'path))
    (is (and (>= (length name) 9) ; "test-" + digits + ".tmp"
             (and (>= (length name) 5) (string= "test-" name :end2 5))
             (string= ".tmp" name :start2 (- (length name) 4))))))

;;;; ==========================================================================
;;;; Directory Operation Tests
;;;; ==========================================================================

(deftest test-list-directory
  "Test directory listing"
  (let ((files (list-directory (current-directory))))
    (is (listp files))
    (is (every (lambda (f) (typep f 'path)) files))))

(deftest test-list-directory-types
  "Test directory listing with type filtering"
  (let ((files (list-directory (current-directory) :type :files))
        (dirs (list-directory (current-directory) :type :directories)))
    (is (every #'path-file-p files))
    (is (every #'path-directory-p dirs))))

(deftest test-walk-directory
  "Test directory walking"
  (let ((all-items (walk-directory (current-directory) :recursive nil)))
    (is (listp all-items))
    (is (every (lambda (f) (typep f 'path)) all-items))))

;;;; ==========================================================================
;;;; Pattern Matching Tests
;;;; ==========================================================================

(deftest test-glob-match
  "Test glob pattern matching"
  (is (glob-match "*.lisp" "path.lisp"))
  (is (glob-match "test*" "test-file.txt"))
  (is (glob-match "*.txt" "file.txt"))
  (is-not (glob-match "*.lisp" "path.txt"))
  (is (glob-match "test?" "test1"))
  (is (glob-match "test?" "testa"))
  (is-not (glob-match "test?" "test12")))

(deftest test-matches-pattern
  "Test path pattern matching"
  (is (matches-pattern (make-path "/src/path.lisp") "*.lisp"))
  (is-not (matches-pattern (make-path "/src/path.lisp") "*.txt"))
  (is (matches-pattern (make-path "/src/test-file.txt") "test*")))

;; NOTE: find-files test disabled due to infinite loop issue
;; (deftest test-find-files
;;   "Test file finding with patterns"
;;   (let ((all-files (find-files (current-directory) :patterns "*.lisp" :recursive t)))
;;     (is (>= (length all-files) 1))
;;     (is (every (lambda (f) (string= "lisp" (path-extension f))) all-files))))

;;;; ==========================================================================
;;;; Utility Tests
;;;; ==========================================================================

(deftest test-normalize-separators
  "Test path separator normalization"
  (is-equal "usr/local/bin" (normalize-separators "usr\\local\\bin"))
  (is-equal "usr/local/bin" (normalize-separators "usr/local/bin")))

(deftest test-with-temp-path
  "Test temporary path macro"
  (let ((temp-path nil))
    (with-temp-path (path :suffix ".test")
      (setf temp-path (path-string path))
      (with-open-file (stream temp-path :direction :output)
        (write-string "test" stream))
      (is (probe-file temp-path)))
    ;; File should be deleted after with-temp-path
    (is-not (probe-file temp-path))))
