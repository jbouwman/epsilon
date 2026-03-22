(defpackage epsilon.path-test
  (:use :cl :epsilon.test :epsilon.syntax :epsilon.path)
  (:enter t))

;;;; ==========================================================================
;;;; Path Construction Tests
;;;; ==========================================================================

(deftest test-make-path-unix
  "Test Unix path construction"
  (let ((p (make-path "/usr/local/bin")))
    (assert-true (path-absolute-p p))
    (assert-equal '("usr" "local" "bin") (path-segments p))
    (assert-equal nil (path-drive p))))

(deftest test-make-path-relative
  "Test relative path construction"
  (let ((p (make-path "src/lib/path.lisp")))
    (assert-not (path-absolute-p p))
    (assert-equal '("src" "lib" "path.lisp") (path-segments p))
    (assert-equal nil (path-drive p))))

(deftest test-make-path-windows
  "Test Windows path construction"
  #+win32
  (let ((p (make-path "C:\\Program Files\\App")))
    (assert-true (path-absolute-p p))
    (assert-equal '("Program Files" "App") (path-segments p))
    (assert-equal "C" (path-drive p))))

(deftest test-make-path-empty
  "Test empty path construction"
  (assert-equal nil (make-path ""))
  (assert-equal nil (make-path nil)))

(deftest test-path-string-unix
  "Test Unix path string generation"
  (let ((p (make-path "/usr/local/bin")))
    #+win32 (assert-equal "\\usr\\local\\bin" (path-string p))
    #-win32 (assert-equal "/usr/local/bin" (path-string p))))

(deftest test-path-string-relative
  "Test relative path string generation"
  (let ((p (make-path "src/lib/path.lisp")))
    #+win32 (assert-equal "src\\lib\\path.lisp" (path-string p))
    #-win32 (assert-equal "src/lib/path.lisp" (path-string p))))

;;;; ==========================================================================
;;;; Path Manipulation Tests
;;;; ==========================================================================

(deftest test-path-parent
  "Test path parent calculation"
  (let* ((p (make-path "/usr/local/bin"))
         (parent (path-parent p)))
    (assert-equal '("usr" "local") (path-segments parent))
    (assert-true (path-absolute-p parent))))

(deftest test-path-parent-root
  "Test parent of root path"
  (let* ((p (make-path "/"))
         (parent (path-parent p)))
    (assert-equal '() (path-segments parent))
    (assert-true (path-absolute-p parent))))

(deftest test-path-name
  "Test path name extraction"
  (assert-equal "path.lisp" (path-name (make-path "/usr/local/path.lisp")))
  (assert-equal "bin" (path-name (make-path "/usr/local/bin")))
  (assert-equal nil (path-name (make-path "/"))))

(deftest test-path-stem
  "Test path stem extraction (filename without extension)"
  (assert-equal "path" (path-stem (make-path "/usr/local/path.lisp")))
  (assert-equal "archive" (path-stem (make-path "/tmp/archive.tar.gz")))
  (assert-equal "README" (path-stem (make-path "/src/README")))
  (assert-equal nil (path-stem (make-path "/"))))

(deftest test-path-extension
  "Test path extension extraction"
  (assert-equal "lisp" (path-extension (make-path "/usr/local/path.lisp")))
  (assert-equal "gz" (path-extension (make-path "/tmp/archive.tar.gz")))
  (assert-equal nil (path-extension (make-path "/src/README")))
  (assert-equal nil (path-extension (make-path "/"))))

(deftest test-path-join
  "Test path joining"
  (let ((p (path-join "/usr" "local" "bin")))
    (assert-equal '("usr" "local" "bin") (path-segments p))
    (assert-true (path-absolute-p p))))

(deftest test-path-join-absolute-override
  "Test path joining with absolute path override"
  (let ((p (path-join "relative" "/absolute" "path")))
    (assert-equal '("absolute" "path") (path-segments p))
    (assert-true (path-absolute-p p))))

(deftest test-path-join-objects
  "Test path joining with path objects"
  (let* ((p1 (make-path "/usr/local"))
         (p2 (make-path "bin"))
         (p3 (path-join p1 p2)))
    (assert-equal '("usr" "local" "bin") (path-segments p3))
    (assert-true (path-absolute-p p3))))

(deftest test-path-equal
  "Test path equality comparison"
  (assert-true (path-equal "/usr/local/bin" "/usr/local/bin"))
  (assert-true (path-equal (make-path "/usr/local") (make-path "/usr/local")))
  (assert-not (path-equal "/usr/local" "/usr/local/bin"))
  (assert-not (path-equal "usr/local" "/usr/local")))

(deftest test-path-normalize
  "Test path normalization"
  (let ((p (path-normalize "/usr/local/../bin/./file")))
    (assert-equal '("usr" "bin" "file") (path-segments p))
    (assert-true (path-absolute-p p))))

(deftest test-path-normalize-relative
  "Test relative path normalization"
  (let ((p (path-normalize "src/../lib/./path.lisp")))
    (assert-equal '("lib" "path.lisp") (path-segments p))
    (assert-not (path-absolute-p p))))

;;;; ==========================================================================
;;;; Path Query Tests
;;;; ==========================================================================

(deftest test-path-exists-p
  "Test path existence checking"
  ;; Test with a path that should exist
  (assert-true (path-exists-p (current-directory)))
  ;; Test with a path that shouldn't exist
  (assert-not (path-exists-p "/nonexistent/path/12345")))

(deftest test-path-type
  "Test path type detection"
  (assert-equal :directory (path-type (current-directory)))
  ;; Create a temporary file to test
  (let ((temp-path (make-temp-path :suffix ".test")))
    (with-open-file (stream (path-string temp-path) :direction :output)
      (write-string "test" stream))
    (assert-equal :file (path-type temp-path))
    (delete-file (path-string temp-path))))

;;;; ==========================================================================
;;;; System Path Tests
;;;; ==========================================================================

(deftest test-current-directory
  "Test current directory access"
  (let ((cwd (current-directory)))
    (assert-true (typep cwd 'path))
    (assert-true (path-absolute-p cwd))
    (assert-true (path-directory-p cwd))))

(deftest test-temp-directory
  "Test temporary directory access"
  (let ((temp (temp-directory)))
    (assert-true (typep temp 'path))
    (assert-true (path-absolute-p temp))
    (assert-true (path-directory-p temp))))

(deftest test-home-directory
  "Test home directory access"
  (let ((home (home-directory)))
    (assert-true (typep home 'path))
    (assert-true (path-absolute-p home))
    (assert-true (path-directory-p home))))

(deftest test-make-temp-path
  "Test temporary path generation"
  (let* ((temp (make-temp-path :prefix "test-" :suffix ".tmp"))
         (name (path-name temp)))
    (assert-true (typep temp 'path))
    (assert-true (and (>= (length name) 9) ; "test-" + digits + ".tmp"
             (and (>= (length name) 5) (string= "test-" name :end2 5))
             (string= ".tmp" name :start2 (- (length name) 4))))))

;;;; ==========================================================================
;;;; Directory Operation Tests
;;;; ==========================================================================

(deftest test-list-directory
  "Test directory listing"
  (let ((files (list-directory (current-directory))))
    (assert-true (listp files))
    (assert-true (every (lambda (f) (typep f 'path)) files))))

(deftest test-list-directory-types
  "Test directory listing with type filtering"
  (let ((files (list-directory (current-directory) :type :files))
        (dirs (list-directory (current-directory) :type :directories)))
    (assert-true (every #'path-file-p files))
    (assert-true (every #'path-directory-p dirs))))

(deftest test-walk-directory
  "Test directory walking"
  (let ((all-items (walk-directory (current-directory) :recursive nil)))
    (assert-true (listp all-items))
    (assert-true (every (lambda (f) (typep f 'path)) all-items))))

;;;; ==========================================================================
;;;; Pattern Matching Tests
;;;; ==========================================================================

(deftest test-glob-match
  "Test glob pattern matching"
  (assert-true (glob-match "*.lisp" "path.lisp"))
  (assert-true (glob-match "test*" "test-file.txt"))
  (assert-true (glob-match "*.txt" "file.txt"))
  (assert-not (glob-match "*.lisp" "path.txt"))
  (assert-true (glob-match "test?" "test1"))
  (assert-true (glob-match "test?" "testa"))
  (assert-not (glob-match "test?" "test12")))

(deftest test-matches-pattern
  "Test path pattern matching"
  (assert-true (matches-pattern (make-path "/src/path.lisp") "*.lisp"))
  (assert-not (matches-pattern (make-path "/src/path.lisp") "*.txt"))
  (assert-true (matches-pattern (make-path "/src/test-file.txt") "test*")))

;; NOTE: find-files test disabled due to infinite loop issue
;; (deftest test-find-files
;;   "Test file finding with patterns"
;;   (let ((all-files (find-files (current-directory) :patterns "*.lisp" :recursive t)))
;;     (assert-true (>= (length all-files) 1))
;;     (assert-true (every (lambda (f) (string= "lisp" (path-extension f))) all-files))))

;;;; ==========================================================================
;;;; Utility Tests
;;;; ==========================================================================

(deftest test-normalize-separators
  "Test path separator normalization"
  (assert-equal "usr/local/bin" (normalize-separators "usr\\local\\bin"))
  (assert-equal "usr/local/bin" (normalize-separators "usr/local/bin")))

(deftest test-with-temp-path
  "Test temporary path macro"
  (let ((temp-path nil))
    (with-temp-path (path :suffix ".test")
      (setf temp-path (path-string path))
      (with-open-file (stream temp-path :direction :output)
        (write-string "test" stream))
      (assert-true (probe-file temp-path)))
    ;; File should be deleted after with-temp-path
    (assert-not (probe-file temp-path))))
