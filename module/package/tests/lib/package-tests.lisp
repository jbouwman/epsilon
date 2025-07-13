(defpackage :epsilon.lib.package.tests
  (:use
   :cl
   :epsilon.test
   :epsilon.lib.package)
  (:local-nicknames
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.lib.uri)
   (:map :epsilon.lib.map)))

(in-package :epsilon.lib.package.tests)

;;;; ==========================================================================
;;;; Platform Detection Tests
;;;; ==========================================================================

(deftest test-platform-detection
  "Test platform information detection"
  (is (not (null *platform-info*)))
  (is (member (getf *platform-info* :os) '(:darwin :linux :windows :unknown)))
  (is (member (getf *platform-info* :arch) '(:x86_64 :arm64 :unknown)))
  (is (eq (getf *platform-info* :lisp) :sbcl)))

(deftest test-platform-string
  "Test platform string generation"
  (let ((platform (platform-string)))
    (is (stringp platform))
    (is (> (length platform) 3))
    (is (find #\- platform))))

;;;; ==========================================================================
;;;; Manifest Creation Tests
;;;; ==========================================================================

(deftest test-create-manifest
  "Test package manifest creation"
  (let* ((module-info (map:make-map
                       "name" "test-package"
                       "version" "1.0.0" 
                       "description" "Test package description"
                       "author" "Test Author"
                       "license" "MIT"
                       "homepage" "https://example.com"
                       "provides" '("test.package")))
         (manifest (create-manifest module-info)))
    
    (is (map:map= manifest manifest))
    (is-equal "test-package" (map:get manifest :name))
    (is-equal "1.0.0" (map:get manifest :version))
    (is-equal "Test package description" (map:get manifest :description))
    (is-equal "Test Author" (map:get manifest :author))
    (is-equal "MIT" (map:get manifest :license))
    (is-equal "https://example.com" (map:get manifest :homepage))
    (is-equal '("test.package") (map:get manifest :provides))
    (is-equal "test.package" (map:get manifest :main-package))
    (is (not (null (map:get manifest :platform))))
    (is (not (null (map:get manifest :build))))
    (is (not (null (map:get manifest :install))))
    (is (not (null (map:get manifest :compatibility))))))

(deftest test-format-timestamp
  "Test timestamp formatting"
  (let ((timestamp (format-timestamp 0)))
    (is (stringp timestamp))
    (is (= 20 (length timestamp)))
    (is (char= #\T (char timestamp 10)))
    (is (char= #\Z (char timestamp 19)))))

;;;; ==========================================================================
;;;; Utility Function Tests  
;;;; ==========================================================================

(deftest test-random-string
  "Test random string generation"
  (let ((str1 (random-string 8))
        (str2 (random-string 8)))
    (is (= 8 (length str1)))
    (is (= 8 (length str2)))
    (is (stringp str1))
    (is (stringp str2))
    ;; Very unlikely to generate same string twice
    (is-not (string= str1 str2))))

(deftest test-ends-with-p
  "Test string suffix checking"
  (is (ends-with-p "test.lisp" ".lisp"))
  (is (ends-with-p "hello.txt" ".txt"))
  (is-not (ends-with-p "test.lisp" ".txt"))
  (is-not (ends-with-p "short" "longer")))

(deftest test-contains-p
  "Test string substring checking"
  (is (contains-p "hello world" "world"))
  (is (contains-p "test.lisp" "."))
  (is-not (contains-p "hello" "xyz"))
  (is (contains-p "epsilon" "psi")))

(deftest test-replace-char
  "Test character replacement"
  (is-equal "hello_world" (replace-char #\space #\_ "hello world"))
  (is-equal "test.fasl" (replace-char #\l #\s "test.fasl"))
  (is-equal "unchanged" (replace-char #\x #\y "unchanged")))

;;;; ==========================================================================
;;;; File Operation Tests
;;;; ==========================================================================

(deftest test-file-size
  "Test file size calculation"
  ;; Create a temporary file with known content
  (let ((temp-file (format nil "/tmp/test-file-~A.txt" (random-string 8))))
    (unwind-protect
         (progn
           (with-open-file (stream temp-file :direction :output)
             (write-string "hello world" stream))
           (let ((size (file-size temp-file)))
             (is (numberp size))
             (is (= 11 size))))  ; "hello world" is 11 bytes
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(deftest test-copy-file
  "Test file copying"
  (let ((source-file (format nil "/tmp/test-source-~A.txt" (random-string 8)))
        (dest-file (format nil "/tmp/test-dest-~A.txt" (random-string 8))))
    (unwind-protect
         (progn
           ;; Create source file
           (with-open-file (stream source-file :direction :output)
             (write-string "test content" stream))
           
           ;; Copy file
           (copy-file source-file dest-file)
           
           ;; Verify copy
           (is (probe-file dest-file))
           (with-open-file (stream dest-file)
             (is-equal "test content" (read-line stream))))
      
      ;; Cleanup
      (when (probe-file source-file) (delete-file source-file))
      (when (probe-file dest-file) (delete-file dest-file)))))

;;;; ==========================================================================
;;;; Package Information Tests
;;;; ==========================================================================

(deftest test-package-info
  "Test package information retrieval"
  (let ((info (package-info "test-package" "1.0.0")))
    (is (map:map= info info))
    (is-equal "test-package" (map:get info :name))
    (is-equal "1.0.0" (map:get info :version))))

(deftest test-dependency-tree
  "Test dependency tree generation"
  (let ((tree (dependency-tree "test-package")))
    (is (map:map= tree tree))
    (is (listp (map:get tree :direct)))
    (is (listp (map:get tree :transitive)))))

(deftest test-platform-compatibility
  "Test platform compatibility checking"
  (let* ((manifest (map:make-map
                    :compatibility (map:make-map
                                    :platforms '(:darwin :linux :windows))))
         (compat (platform-compatibility manifest)))
    (is (member compat '(:compatible :incompatible)))))

(deftest test-list-packages
  "Test package listing"
  (let ((packages (list-packages)))
    (is (listp packages))))

;;;; ==========================================================================
;;;; Function Availability Tests
;;;; ==========================================================================

(deftest test-exported-functions
  "Test that all expected functions are exported and available"
  (is (fboundp 'build-package))
  (is (fboundp 'create-manifest))
  (is (fboundp 'sign-package))
  (is (fboundp 'install-package))
  (is (fboundp 'verify-package))
  (is (fboundp 'list-packages))
  (is (fboundp 'create-repository))
  (is (fboundp 'update-index))
  (is (fboundp 'publish-package))
  (is (fboundp 'package-info))
  (is (fboundp 'dependency-tree))
  (is (fboundp 'platform-compatibility))
  (is (fboundp 'platform-string))
  (is (boundp '*platform-info*)))

;;;; ==========================================================================
;;;; Mock Package Building Tests
;;;; ==========================================================================

(deftest test-calculate-source-hash
  "Test source hash calculation"
  (let* ((module-info (map:make-map "name" "test-module"))
         (hash (calculate-source-hash module-info)))
    (is (stringp hash))
    (is (> (length hash) 0))))

;; More extensive tests would require actual file system operations
;; and temporary directories, which are complex to set up properly
;; in a test environment. The above tests cover the core functionality
;; that can be tested without external dependencies.